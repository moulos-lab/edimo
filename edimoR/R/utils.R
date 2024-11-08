generateConfigTemplate <- function() {
    # Defile a list to be converted to JSON
    # Dummy names
}

# Utility function to bypass glue and its problem with curly braces in logger
.skipFormatter <- function(...) {
    msg <- unlist(list(...))
    return(logger::skip_formatter(paste(msg,collapse="")))
}

.fetchInstitutionDetails <- function(iid) {
    con <- mongoConnect(DB_CREDS,"institutions")
    on.exit(mongoDisconnect(con))
    
    query <- .toMongoJSON(list(`_id`=list(`$oid`=iid)))
    result <- con$find(query,fields='{}')
    
    return(list(
        name=result$profile$name,
        street=result$profile$street,
        city=result$profile$city,
        state=result$profile$state,
        zip=result$profile$zip,
        country=result$profile$country,
        tel=result$profile$tel
    ))
}

.newUser <- function(username,name,surname,email,password,role,inst) {
    currentTime <- Sys.time()
    
    verificationToken <- jose::jwt_claim(
        iss="edimo-app",
        sub=username,
        exp=Sys.time() + 86400, # Token expiry time (24 hours)
        uid=result$`_id`, # Include user's id in the JWT payload
        name=name,
        surname=surname,
        email=email
    )
    verificationToken <- jose::jwt_encode_hmac(verificationToken,THE_SECRET)
    #claims <- jose::jwt_decode_hmac(verificationToken,THE_SECRET)
    
    newUser <- list(
        username=username,
        password=list(
            bcrypt=bcrypt::hashpw(password)
        ),
        metadata=list(
            date_created=unbox(currentTime),
            date_updated=NULL,
            last_login=NULL,
            last_login_attempt=NULL,
            login_attempts=0,
            role=role,
            account_locked=FALSE
        ),
        emails = list(
            list(
                address=email,
                verified=FALSE,
                main=TRUE,
                verification_token=verificationToken
            )
        ),
        profile=list(
            name=name,
            surname=surname,
            dob=NULL,
            phone=list(
                fix=inst$tel,
                mobile=NULL
            ),
            address=list(
                institution=inst$name,
                street=inst$street,
                city=inst$city,
                state=inst$state,
                zip=as.character(inst$zip),
                country=inst$country
            )
        )
    )
    
    return(newUser)
}

.toMongoJSON <- function(dat,...) {
    return(toJSON(dat,auto_unbox=TRUE,null="null",na="null",POSIXt="mongo",...))
}

# app passwords
# https://support.google.com/accounts/answer/185833?hl=en
# mkyu hkiq hurk lggx
.glueUserVerificationMail <- function(name,surname,email,host,endpoint,token) {
    return(glue(
        'Dear {name} {surname},
        
        Thank you for registering with EDIMO APP.
        
        Please verify your email address ({email}) to activate your account
        using the following link:
        
        {host}/{endpoint}?token={token}
        
        Best regards,
        
        The EDIMO app team'
    ))
}

.userVerificationMail <- function(name,surname,addr,host,endpoint,token) {
    mailConf <- CONFIG$mail
    mailBody<- .glueUserVerificationMail(name,surname,email,host,endpoint,token)
    email <- envelope()
    email <- email %>%
        from(mailConf$from) %>%
        to(addr) %>%
        subject("EDIMO app - Verify your email") %>%
        emayili::text(mailBody)
    smtp <- emayili::server(host=mailConf$host,
       port=mailConf$port,
       username=mailConf$username,
       password=mailConf$password)
    smtp(email)
}

