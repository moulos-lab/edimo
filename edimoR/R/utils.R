generateConfigTemplate <- function() {
    # Define a list to be converted to JSON
    # Dummy names
}

.getDbCreds <- function() {
    if (is.null(.CONFIG$databases$edimoclin)) {
        log_error("FATAL! MongoDB connection details are not defined!")
        stop("FATAL! MongoDB connection details are not defined!")
    }
    return(.CONFIG$databases$edimoclin)
}

.getDbBackCreds <- function() {
    if (is.null(.CONFIG$databases$edimoback)) {
        log_error("FATAL! MongoDB connection details are not defined!")
        stop("FATAL! MongoDB connection details are not defined!")
    }
    return(.CONFIG$databases$edimoback)
}

.getAppWorkspace <- function() {
    if (is.null(.CONFIG$paths$workspace)) {
        log_error("FATAL! Application main workspace is not defined!")
        stop("FATAL! Application main workspace is not defined!")
    }
    return(.CONFIG$paths$workspace)
}

.getApiSecret <- function() {
    if (is.null(.CONFIG$auth$secret)) {
        log_error("FATAL! Main authentication secret token is not defined!")
        stop("FATAL! Main authentication secret token is not defined!")
    }
    return(.CONFIG$auth$secret)
}

.getOncoKbToken <- function() {
    if (is.null(.CONFIG$auth$oncokb)) {
        log_error("FATAL! OncoKB API token is not defined!")
        stop("FATAL! OncoKB API token is not defined!")
    }
    return(.CONFIG$auth$oncokb)
}

.initConfig <- function(conf) {
    if (!file.exists(conf))
        stop("Main configuration file not found!")
    
    # Read main configurtion environment
    conf <- fromJSON(conf)
    
    .CONFIG$databases <- conf$databases
    .CONFIG$paths <- conf$paths
    .CONFIG$auth <- conf$auth
    .CONFIG$tmp_admin <- conf$tmp_admin
    .CONFIG$rest_api <- conf$rest_api
    .CONFIG$host <- conf$host
    .CONFIG$mail <- conf$mail
    .CONFIG$software <- conf$software
    .CONFIG$static_files <- conf$static_files
}

.initAppPaths <- function() {
    .PATHS <- .CONFIG$paths
    .WORKSPACE <- .PATHS$workspace
    
    if (!dir.exists(.WORKSPACE)) {
        dir.create(.WORKSPACE,recursive=TRUE,showWarnings=FALSE)
        dir.create(file.path(.WORKSPACE,"users"),recursive=TRUE,
            showWarnings=FALSE)
        dir.create(logdir,recursive=TRUE,showWarnings=FALSE)
    }
}

# Utility function to bypass glue and its problem with curly braces in logger
.skipFormatter <- function(...) {
    msg <- unlist(list(...))
    return(logger::skip_formatter(paste(msg,collapse="")))
}

.fetchInstitutionDetails <- function(iid) {
    con <- mongoConnect("institutions")
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
    mailConf <- .CONFIG$mail
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

.checkNumArgs <- function(argName,argValue,argType,argBounds,direction) {
    # First generic check so not to continue if fail
    if (!is(argValue,argType))
        stop("\"",argName,"\" parameter must be a(n) ",argType," value!")
    
    # Then, proceed with a lookup table to avoid repetition (suggested by
    # Marcel Ramos during package review)
    lookup <- list(
        both=list(
            fail=function(x) x<argBounds[1] || x>argBounds[2],
            cls=class,
            msg=function(x) paste0("larger than or equal to ",
                argBounds[1]," and smaller than or equal to ",
                argBounds[2])
        ),
        botheq=list(
            fail=function(x) x<=argBounds[1] || x>=argBounds[2],
            cls=class,
            msg=function(x) paste0("larger than ",argBounds[1],
                " and smaller than ",argBounds[2])
        ),
        gt=list(
            fail=function(x) x<=argBounds[1], 
            cls=class,
            msg=function(x) paste0("greater than ",argBounds[1])
        ),
        lt=list(
            fail=function(x) x>=argBounds[1], 
            cls=class,
            msg=function(x) paste0("lower than ",argBounds[1])
        ),
        gte=list(
            fail=function(x) x<argBounds[1], 
            cls=class,
            msg=function(x) paste0("greater than or equal to ",
                argBounds[1])
        ),
        lte=list(
            fail=function(x) x>argBounds[1], 
            cls=class,
            msg=function(x) paste0("lower than or equal to ",
                argBounds[1])
        )
    )
    
    check <- lapply(lookup[[direction]],function(f) f(argValue))
    if (argType == "numeric" && check$cls == "integer")
        argType <- "integer"
    if (check$fail || check$cls != argType)
        stop("\"",argName,"\""," parameter must be a(n) ",argType,
            " value ",check$msg,"!")
}

.checkTextArgs <- function(argName,argValue,argList,multiarg=FALSE) {
    if (!is.character(argValue))
        stop(argValue," must be a character scalar or vector!")
    if (multiarg) {
        argValue <- tolower(argValue)
        if (!all(argValue %in% argList))
            stop("\"",argName,"\""," parameter must be one or more of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
    else {
        argSave <- argValue[1]
        argValue <- tolower(argValue[1])    
        if (!(argValue %in% argList))
            stop("\"",argName,"\""," parameter must be one of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
}

.checkId <- function(i,what) {
    query <- .toMongoJSON(list(`_id`=list(`$oid`=i)))
    switch(what,
        user = {
            con <- mongoConnect("users")
            on.exit(mongoDisconnect(con))
            result <- con$find(query,fields='{"_id": 1}')
        },
        sample = {
            con <- mongoConnect("samples")
            on.exit(mongoDisconnect(con))
            result <- con$find(query,fields='{"_id": 1}')
        }
    )
    return(ifelse(nrow(result)>0,TRUE,FALSE))
}

.getUserName <- function(uid) {
    con <- mongoConnect("users")
    on.exit(mongoDisconnect(con))
            
    query <- .toMongoJSON(list(`_id`=list(`$oid`=uid)))
    fields <- .toMongoJSON(list(
        username=1L,
        profile=1L
    ))
    
    result <- con$find(query,fields=fields)
    return(paste0(result$profile$name," ",result$profile$surname))
}

cmclapply <- function(...,rc) {
    if (suppressWarnings(!requireNamespace("parallel")) 
        || .Platform$OS.type!="unix")
        m <- FALSE
    else {
        m <- TRUE
        ncores <- parallel::detectCores()
        if (ncores==1) 
            m <- FALSE
        else {
            if (!missing(rc) && !is.null(rc))
                ncores <- ceiling(rc*ncores)
            else 
                m <- FALSE
        }
    }
    if (m)
        return(mclapply(...,mc.cores=ncores,mc.set.seed=FALSE))
    else
        return(lapply(...))
}

.openSink <- function(f) {
    fh <- file(f,open="wt")
    sink(fh,type="output")
    sink(fh,type="message")
}

.closeSink <- function() {
    sink(type="message")
    sink(type="output")
}
