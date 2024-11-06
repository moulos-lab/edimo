userRegistrationWorkflow <- function(req,res){
    username <- tolower(req$body$username)
    name <- req$body$name
    surname <- req$body$surname
    email <- tolower(req$body$email)
    password <- req$body$password
    institution <- req$body$institution # objectId
    role <- req$body$role
    if (is.null(role))
        role <- "user"
    
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect(DB_CREDS,"users")
    on.exit(mongoDisconnect(con))

    # Check if the username already exists since it's our main identifier and
    # also email - system is case insensitive - log messages
    query <- .toMongoJSON(list(
        username=username,
        emails.address=email
    ))
    fields <- .toMongoJSON(list(
        username=1L,
        emails=1L
    ))
    
    log_debug("Checking existence for user ",username," with email ",email,".")
    log_debug(.skipFormatter("MongoDB query is: ",query))
    
    result <- con$find(query,fields=fields)
    if (nrow(result) > 0) {
        erru <- erre <- NULL
        if (username == result$username)
            erru <- paste0("Username ",username," already exists")
        if (email %in% result$emails[[1]]$address) {
            ind <- which(result$emails[[1]]$address == "email")
            if (result$emails[[1]]$verified[ind])
                erre <- paste0("email address ",email,
                    " already exists and is verified")
            else
                erre <- paste0("email address ",email,
                    " already exists and is not verified")
        }
        err <- paste0("Registration failed: ",paste(erru,erre,collapse=", "))
        
        log_error(err)
        
        res$status <- 400
        return(list(error=err))
    }

    # Assuming user not exists, find institution details (if available)
    # to auto-fill user profile
    inst <- .fetchInstitutionDetails(institution)
    
    # If not existing, hash the password
    hashedPassword <- bcrypt::hashpw(password)
    
    
    # Insert the user into the database
    #.newUser(username,name,surname,email,password,role,institution)
    
    query <- paste0(DB_QUERIES$REGISTER_USER,makeInsertString(username,name,
        email,hashedPassword,Sys.time(),NULL,NULL,role))
    
    log_debug("Inserting user ",username," with email ",email," to database.")
    log_debug("SQL query is: ",query)
    
    # Send verification mail
    
    tryCatch({
        nr <- dbExecute(con,query)
        if (nr > 0) { # User inserted
            log_info("User ",username," with email ",email," successfully ",
                "registered!")
            
            res$status <- 201
            return(list(message="User registration successful!"))
        }
        else {
            log_error("User ",username," with email ",email," registration ",
                "failed!")
            
            res$status <- 400
            return(list(error="User registration failed!"))
        }
    },error=function(e) {
        log_error("User registration failed: ",e$message)
        
        res$status <- 500
        return(list(error=paste0("User registration failed: ",e$message)))
    })
    
}

userLogin <- function(req,res) {
    # Parse input from POST request body
    username <- tolower(req$body$username)
    password <- tolower(req$body$password)
    
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect(DB_CREDS,"users")
    on.exit(mongoDisconnect(con))

    # Check if the username already exists since it's our main identifier and
    # also email - system is case insensitive - log messages
    query <- .toMongoJSON(list(
        username=username
    ))
    
    log_debug("Querying database for user ",username,".")
    log_debug(.skipFormatter("MongoDB query is: ",query))
    
    # Check if the user does not exist
    result <- con$find(query,fields='{}')
    if (nrow(result) == 0) {
        log_warn("Failed authentication attempt: invalid username ",username)
        
        res$status <- 401
        return(list(error="Authentication failed: invalid username"))
    }
    
    # If the user exists, check password
    hashedPassword <- result$password$bcrypt
    if (!bcrypt::checkpw(password,hashedPassword)) {
        # Update login attempts
        loginAttempts <- result$metadata$login_attempts
        lockAccount <- ifelse(loginAttempts>=5,TRUE,FALSE)
        
        if (lockAccount)
            log_warn("Many (>=5) failed authentication attempts: for user ",
                username,"! Account locked!")
        else
            log_warn("Failed authentication attempt ",loginAttempts+1,
                ": invalid password for user ",username)
        
        filterQuery <- .toMongoJSON(list(
            `_id`=list(`$oid`=result$`_id`)
        ))
        updateQuery <- .toMongoJSON(list(
            `$set`=list(
                metadata.login_attempts=unbox(loginAttempts + 1),
                metadata.last_login_attempt=unbox(Sys.time()),
                metadata.account_locked=unbox(lockAccount)
            )
        ))
        con$update(ruleQuery,updateQuery)
        
        res$status <- 401
        if (lockedAccount)
            return(list(error=paste0("Authentication failed: invalid ",
                "password, account locked! Please contact an administrator."))
        else
            return(list(error="Authentication failed: invalid password"))
    }
    
    #TODO: if email unverified, discontinue process

    # If all ok, continue the flow
    tryCatch({
        log_debug("Generating authentication token for ",username,".")
        
        authToken <- jose::jwt_claim(
            iss="edimo-app",
            sub=username,
            exp=Sys.time() + 43200, # Token expiry time (12 hours) to be reconsidered
            role=result$metadata$role, # Include user's role in the JWT payload
            uid=result$`_id`, # Include user's id in the JWT payload
            name=result$profile$name,
            surname=result$profile$surname
        )
        token <- jose::jwt_encode_hmac(authToken,THE_SECRET)
        
        # If the token is generated successfully, mark last login time and 
        # reset login attempts if any unsuccessful
        filterQuery <- .toMongoJSON(list(
            `_id`=list(`$oid`=result$`_id`)
        ))
        updateQuery <- .toMongoJSON(list(
            `$set`=list(
                metadata.last_login=unbox(Sys.time()),
                metadata.last_login_attempt=unbox(Sys.time()),
                metadata.login_attempts=unbox(0L)
            )
        ))
    
        log_debug("Updating last login for user ",username,".")
        log_debug(.skipFormatter("Select MongoDB query is: ",filterQuery))
        log_debug(.skipFormatter("Update MongoDB query is: ",updateQuery))
        
        # If date recording failed, not a problem for authentication
        tryCatch({
            nr <- con$update(ruleQuery,updateQuery)
            if (nr$modifiedCount > 0) # Document updated
                log_debug("User ",username," login recorded.")
            else
                log_warn("User ",username," login failed to be recorded.")
        },error=function(e) {
            log_warn("User ",username," login failed to be recorded. ",
                e$message)
        })
        
        # So return
        return(list(message="Authentication successful!",auth_token=token))
    },error=function(e) { # But failure to generate token is a problem
        log_error("Authentication token generation for user ",username,
            "failed: ",e$message)
        
        res$status <- 500
        return(list(error=paste0("Authentication token generation failed! ",
            "Please contact an admin.")))
    })
}
