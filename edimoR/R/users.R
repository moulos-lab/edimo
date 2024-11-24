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
    con <- mongoConnect("users")
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
            ind <- which(result$emails[[1]]$address == email)
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
    # to auto-fill user profile. The institution should exist because the list
    # in front-end is populated from the database - so no further checks.
    inst <- .fetchInstitutionDetails(institution)
    
    # Insert the user into the database
    newUser <- .newUser(username,name,surname,email,password,role,inst)
    
    log_debug("Inserting user ",username," with email ",email," to database.")
    log_debug(.skipFormatter("MongoDB query is: ",.toMongoJSON(newUser)))
    
    tryCatch({
        nr <- con$insert(.toMongoJSON(newUser))
        if (nr$nInserted > 0) { # User inserted
            log_info("User ",username," with email ",email," successfully ",
                "registered!")
                
            # Send verification mail
            log_debug("Sending verification email to ",email,".")
            .userVerificationMail(name,surname,email,.CONFIG$host,
                "auth/verify_email",newUser$emails[[1]]$verification_token)
            
            res$status <- 201
            return(list(message=paste0("User registration successful! Check ",
                "your email.")))
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
    con <- mongoConnect("users")
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
        con$update(filterQuery,updateQuery)
        
        res$status <- 401
        if (lockAccount)
            return(list(error=paste0("Authentication failed: invalid ",
                "password, account locked! Please contact an administrator.")))
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
        token <- jose::jwt_encode_hmac(authToken,.getApiSecret())
        
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
            nr <- con$update(filterQuery,updateQuery)
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

authenticateUser <- function(req,res,token) {
    tryCatch({
        claims <- jose::jwt_decode_hmac(token,.getApiSecret())
        username <- claims$sub

        log_debug("Token based authentication for user ",username,".")

        return(list(message="Authentication successful"))
    },error = function(e) { # Most probably token expired
        res$status <- 401
        if (grepl("expired",e$message)) {
            log_warn("Authentication failed: ",e$message)

            return(list(error=paste0("Authentication failed: ",e$message)))
        }
        else {
            log_warn("Authentication failed: invalid token")

            return(list(error="Authentication failed: Invalid token"))
        }
    })
}

verifyUserEmail <- function(req,res,token) {
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect("users")
    on.exit(mongoDisconnect(con))
    
    # Check if the token is invalid or expired
    claims <- NULL
    claims <- tryCatch({
        jose::jwt_decode_hmac(token,.getApiSecret())
    },error=function(e) {
        log_error("Invalid email verification token: ",e$message)
        
        res$setHeader("Content-Type","text/html")
        res$status <- 500
        return(.CONST$INVALID_TOKEN)
    })
    
    if (!is.null(claims)) {
        # First check if token expired
        if (as.numeric(Sys.time()) > claims$exp) {
            log_warn("Failed mail verification: token expired for ",email)
            
            res$setHeader("Content-Type","text/html")
            res$status <- 401
            return(.CONST$EXPIRED_TOKEN)
        }
        
        # If token is valid, continue...
        username <- claims$sub
        email <- claims$email
        
        # Check if the username already exists and if already verified
        query <- .toMongoJSON(list(
            username=username,
            emails.address=email
        ))
        fields <- .toMongoJSON(list(
            `_id`=1L,
            username=1L,
            emails=1L
        ))
        
        result <- con$find(query,fields=fields)
        
        # User does not exist
        if (nrow(result) == 0) {
            log_warn("Failed mail verification: invalid username ",username)
            
            res$setHeader("Content-Type","text/html")
            res$status <- 401
            return(.CONST$INVALID_USER)
        }
        
        # email already verified
        if (email %in% result$emails[[1]]$address) {
            ind <- which(result$emails[[1]]$address == email)
            if (result$emails[[1]]$verified[ind]) {
                log_warn("Failed mail verification: email already ",
                    "verified for username ",username)
                
                res$setHeader("Content-Type","text/html")
                res$status <- 200
                return(.CONST$EMAIL_VERIFIED)
            }
        }
        
        # Otherwise, mark email as verified
        filterQuery <- .toMongoJSON(list(
            `_id`=list(`$oid`=result$`_id`),
            emails.address=email
        ))
        updateQuery <- .toMongoJSON(list(
            `$set`=list(
                "emails.$.verified"=unbox(TRUE)
            )
        ))
    
        log_debug("Verifying email for user ",username,".")
        log_debug(.skipFormatter("Select MongoDB query is: ",filterQuery))
        log_debug(.skipFormatter("Update MongoDB query is: ",updateQuery))
        
        # If date recording failed, not a problem for authentication
        tryCatch({
            nr <- con$update(filterQuery,updateQuery)
            if (nr$modifiedCount > 0) {
                # Document updated
                log_debug("User ",username," email ",email," verified.")
                return(.CONST$EMAIL_VERIFIED)
            } 
            else {
                log_warn("User ",username," email ",email," failed to ",
                    "be verified.")
                
                res$setHeader("Content-Type","text/html")
                res$status <- 400
                return(.CONST$EMAIL_NOT_VERIFIED)
            }
        },error=function(e) {
            log_warn("User ",username," email ",email," failed to ",
                "be verified. ",e$message)
            
            res$setHeader("Content-Type","text/html")
            res$status <- 400
            return(.CONST$EMAIL_NOT_VERIFIED)
        })
    } 
    else {
        res$setHeader("Content-Type","text/html")
        res$status <- 400
        return(.CONST$INTERNAL_ERROR)
    }
}

userResetPassword <- function(req,res) {
    # Parse input from POST request body
    username <- tolower(req$body$username)
    email <- tolower(req$body$email)
    new_password <- tolower(req$body$new_password)
    
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect("users")
    on.exit(mongoDisconnect(con))

    # Check if the username already exists since it's our main identifier and
    # also email - system is case insensitive - log messages
    query <- .toMongoJSON(list(
        username=username,
        email.address=email
    ))
    
    log_debug("Querying database for user ",username,".")
    log_debug(.skipFormatter("MongoDB query is: ",query))
    
    # Check if the user does not exist
    result <- con$find(query,fields='{}')
    if (nrow(result) == 0) {
        log_warn("Failed password reset: invalid username ",username)
        
        res$status <- 401
        return(list(error="Cannot reset password: invalid username"))
    }

    # Otherwise, hash the new password and update the database
    hashedPassword <- bcrypt::hashpw(new_password)
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=result$`_id`)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            "password.bcrypt"=unbox(hashedPassword)
        )
    ))
    
    log_debug("Resetting password for user ",username,".")
    log_debug(.skipFormatter("Select MongoDB query is: ",filterQuery))
    log_debug(.skipFormatter("Update MongoDB query is: ",updateQuery))
    
    tryCatch({
        nr <- con$update(filterQuery,updateQuery)
        if (nr > 0) { # Password updated
            log_info("Password for user ",username," successfully updated!")
            
            res$status <- 201
            return(list(message="Password update successful!"))
        }
        else {
            log_error("Password update for user ",username," failed!")
            
            res$status <- 400
            return(list(error="Password update failed!"))
        }
    },error=function(e) {
        log_error("Password update failed: ",e$message)
        
        res$status <- 400
        return(list(error=paste0("Password update failed: ",e$message)))
    })
}
