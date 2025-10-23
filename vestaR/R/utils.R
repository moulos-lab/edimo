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

.getQueueDb <- function() {
    if (is.null(.CONFIG$paths$workspace)) {
        log_error("FATAL! Application main workspace is not defined!")
        stop("FATAL! Application main workspace is not defined!")
    }
    return(file.path(.CONFIG$paths$workspace,"queue","queue.sqlite"))
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

.getNoAnalysisSteps <- function(type) {
    if (is.null(.CONFIG$helpers$steps)) {
        log_error("FATAL! Helper variables are not defined!")
        stop("FATAL! Helper variables are not defined!")
    }
    return(.CONFIG$helpers$steps[[type]])
}

.getCoresFraction <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn("Helper variable for cores is not defined! Will use default.")
        warning("Helper variable for cores is not defined! Will use default.",
            immediate.=TRUE)
        return(0.25)
    }
    return(.CONFIG$helpers$runtime$cores_fraction)
}

.getJsonifyLimit <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn(paste0("Helper variable for jsonify parallelization not ",
            "defined! Will use default."))
        warning(paste0("Helper variable for jsonify parallelization not ",
            "defined! Will use default."),immediate.=TRUE)
        return(1000)
    }
    return(.CONFIG$helpers$runtime$parallel_jsonify_limit)
}

.getVcfReadChunkSize <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn(paste0("Helper variable for VCF import chunk size not ",
            "defined! Will use default."))
        warning(paste0("Helper variable for VCF import chunk size not ",
            "defined! Will use default."),immediate.=TRUE)
        return(5000)
    }
    return(.CONFIG$helpers$runtime$vcf_read_chunksize)
}

.getJobConcurrency <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn(paste0("Helper variable for job concurrency not defined! ",
            "Will use default."))
        warning(paste0("Helper variable for job concurrency not defined! ",
            "Will use default."),immediate.=TRUE)
        return(4)
    }
    return(.CONFIG$helpers$runtime$job_concurrency)
}

.getQueuePollInterval <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn(paste0("Helper variable for queue poll interval not defined! ",
            "Will use default."))
        warning(paste0("Helper variable for queue poll interval not defined! ",
            "Will use default."),immediate.=TRUE)
        return(10000)
    }
    return(.CONFIG$helpers$runtime$queue_poll_interval)
}

.getJobTimeout <- function() {
    if (is.null(.CONFIG$helpers$runtime)) {
        log_warn(paste0("Helper variable for job timeout not defined! ",
            "Will use default."))
        warning(paste0("Helper variable for job timeout not defined! ",
            "Will use default."),immediate.=TRUE)
        return(8.64e+8)
    }
    return(.CONFIG$helpers$runtime$job_timeout)
}

.getTool <- function(tool) {
    tool <- tolower(tool)
    .checkTextArgs("Tool",tool,names(.CONFIG$software),multiarg=FALSE)
    return(list(
        command=.CONFIG$software[[tool]]$exec,
        version=.CONFIG$software[[tool]]$version
    ))
}

.getStaticFile <- function(g,d) {
    if (is.null(.CONFIG$static_files[[g]])) {
        log_error("FATAL! Static file path for ",g," is not defined!")
        stop("FATAL! Static file path for ",g," is not defined!")
    }
    return(.CONFIG$static_files[[g]][[d]])
}

#~ .getReference <- function(g,d=c("reference","snpeff","so_localizations")) {
#~  if (is.null(.CONFIG$static_files[[g]])) {
#~         log_error("FATAL! Reference genome path for ",g," is not defined!")
#~         stop("FATAL! Reference genome path for ",g," is not defined!")
#~     }
#~     return(.CONFIG$static_files[[g]][[d]])
#~ }

#~ # g: genome, d: database, v: version
#~ # e.g.: dbSNP 155 -> file.path(resourcePath,g,d,v)
#~ .getStaticFile <- function(g,d,v) {
#~     if (is.null(.CONFIG$static_files[[g]])) {
#~         log_error("FATAL! Static file path for ",g," is not defined!")
#~         stop("FATAL! Static file path for ",g," is not defined!")
#~     }
#~     ext <- switch(d,
#~      dbsnp = { return(".vcf.gz") },
#~      dbnsfp = { return(".txt.gz") },
#~      dbnsfp_gene = { return(".txt.gz") },
#~      gnomad_exomes = { return(".vcf.bgz") },
#~      gnomad_genomes = { return(".vcf.bgz") },
#~      clinvar = { return(".vcf.gz") },
#~      civic_variants = { return(".tsv") },
#~      civic_genes = { return(".tsv") }
#~     )
#~     return(file.path(.CONFIG$resource_path,g,d,paste0(v,ext)))
#~ }

.initConfig <- function(conf) {
    if (!file.exists(conf))
        stop("Main configuration file not found!")
    
    # Read main configurtion environment
    conf <- fromJSON(conf)
    
    .CONFIG$databases <- conf$databases
    .CONFIG$paths <- conf$paths
    .CONFIG$auth <- conf$auth
    .CONFIG$tmp_admin <- conf$tmp_admin
    .CONFIG$endpoints <- conf$endpoints
    .CONFIG$host <- conf$host
    .CONFIG$mail <- conf$mail
    .CONFIG$software <- conf$software
    .CONFIG$static_files <- conf$static_files
    .CONFIG$helpers <- conf$helpers
}

.initAppPaths <- function() {
    .PATHS <- .CONFIG$paths
    
    if (!dir.exists(.PATHS$workspace)) {
        dir.create(.PATHS$workspace,recursive=TRUE,showWarnings=FALSE)
        dir.create(file.path(.PATHS$workspace,"users"),recursive=TRUE,
            showWarnings=FALSE)
        dir.create(file.path(.PATHS$workspace,"logs"),recursive=TRUE,
            showWarnings=FALSE)
        dir.create(file.path(.PATHS$workspace,"queue"),recursive=TRUE,
            showWarnings=FALSE)
    }
}

# Utility function to bypass glue and its problem with curly braces in logger
.skipFormatter <- function(...) {
    msg <- unlist(list(...))
    return(logger::skip_formatter(paste(msg,collapse="")))
}

.fetchInstitutionDetails <- function(iid) {
    if (is.null(iid) || is.na(iid) || iid == "") {
        # Probably user auto-registering from external identifier
        return(list(name=NA,street=NA,city=NA,state=NA,zip=NA,
            country=NA,tel=NA))
    }
    
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
        iss="edimo-vesta",
        sub=username,
        exp=Sys.time() + 86400, # Token expiry time (24 hours)
        #uid=result$`_id`, # Include user's id in the JWT payload
        name=name,
        surname=surname,
        email=email
    )
    verificationToken <- jose::jwt_encode_hmac(verificationToken,
        .getApiSecret())
    
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
        
        Thank you for registering with VESTA.
        
        Please verify your email address ({email}) to activate your account
        using the following link:
        
        {host}/{endpoint}?token={token}
        
        Best regards,
        
        The VESTA team'
    ))
}

.userVerificationMail <- function(name,surname,addr,host,endpoint,token) {
    # Temporary hack for firewall...
    host <- sub(":8383","",host)
    
    mailConf <- .CONFIG$mail
    mailBody<- .glueUserVerificationMail(name,surname,addr,host,endpoint,token)
    email <- emayili::envelope()
    email <- email %>%
        emayili::from(mailConf$from) %>%
        emayili::to(addr) %>%
        emayili::subject("EDIMO VESTA - Verify your email") %>%
        emayili::text(mailBody)
    smtp <- emayili::server(host=mailConf$host,
       port=mailConf$port,
       username=mailConf$username,
       password=mailConf$password)
    smtp(email)
}

.checkNumArgs <- function(argName,argValue,argType,argBounds,direction) {
    # First generic check so not to continue if fail
    if (!is(argValue,argType)) {
        log_error("\"",argName,"\" parameter must be a(n) ",argType," value!")
        stop("\"",argName,"\" parameter must be a(n) ",argType," value!")
    }
    
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
    if (check$fail || check$cls != argType) {
        log_error("\"",argName,"\""," parameter must be a(n) ",argType,
            " value ",check$msg,"!")
        stop("\"",argName,"\""," parameter must be a(n) ",argType,
            " value ",check$msg,"!")
    }
}

.checkTextArgs <- function(argName,argValue,argList,multiarg=FALSE) {
    if (!is.character(argValue)) {
        log_error(argValue," must be a character scalar or vector!")
        stop(argValue," must be a character scalar or vector!")
    }
    
    if (multiarg) {
        argValue <- tolower(argValue)
        if (!all(argValue %in% argList)) {
            log_error("\"",argName,"\""," parameter must be one or more of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
            stop("\"",argName,"\""," parameter must be one or more of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
        }
    }
    else {
        argSave <- argValue[1]
        argValue <- tolower(argValue[1])    
        if (!(argValue %in% argList)) {
            log_error("\"",argName,"\""," parameter must be one of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
            stop("\"",argName,"\""," parameter must be one of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
        }            
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
        },
        analysis = {
            con <- mongoConnect("analyses")
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

.updateAnalysisProgress <- function(aid,step,pct,desc) {
    if (is.null(aid))
        return(invisible(aid))
    
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            progress.step=unbox(as.integer(step)),
            progress.pct=unbox(as.integer(pct)),
            progress.desc=unbox(desc)
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.updateAnalysisFailReason <- function(aid,reason) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            metadata.fail_reason=unbox(reason),
            metadata.status=unbox("Failed"),
            metadata.date_updated=unbox(Sys.time()),
            metadata.date_completed=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.updateAnalysisJobId <- function(aid,jobid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            metadata.job_id=unbox(jobid),
            metadata.status=unbox("Running"),
            metadata.date_updated=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.markAnalysisSuccessfull <- function(aid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            metadata.status=unbox("Complete"),
            metadata.date_updated=unbox(Sys.time()),
            metadata.date_completed=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.updateSampleGenomeSize <- function(sid,size) {
    con <- mongoConnect("samples")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=sid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            genome_size=unname(size),
            metadata.date_updated=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.updateSampleAnalyses <- function(sid,aid) {
    con <- mongoConnect("samples")
    on.exit(mongoDisconnect(con))
    
    # Get added analysis name
    aname <- .getAnalysisName(aid)
    
    # Get current analyses so as to add to them
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=sid)
        
    ))
    fields <- .toMongoJSON(list(analyses=1L))
    result <- con$find(filterQuery,fields)
    
    # Check what we have
    if (is.na(result$analyses) 
        || (is.list(result$analyses[[1]]) && length(result$analyses[[1]])==0))
        # The first to be added
        analyses <- list(
            list(
                id=list(`$oid`=aid),
                name=aname,
                date_completed=unbox(Sys.time())
            )
        )
    else {
        # Does it already exist (unlikely) - if yes just return
        if (aid %in% result$analyses[[1]]$id)
            invisible(return(FALSE))
        # Otherwise add the new analysis
        analyses <- unname(apply(result$analyses[[1]],1,function(x) {
            return(list(
                id=list(`$oid`=unname(x["id"])),
                name=unname(x["name"]),
                date_completed=unbox(unname(as.POSIXct(x["date_completed"],
                    format="%Y-%m-%dT%H:%M:%OS")))
            ))
        }))
        analyses <- c(analyses,list(list(
            id=list(`$oid`=aid),
            name=aname,
            date_completed=unbox(Sys.time())
        )))
    }
    
    # Finally, update samples
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            analyses=analyses,
            metadata.date_updated=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.removeAnalysisFromSample <- function(sid,aid) {
    con <- mongoConnect("samples")
    on.exit(mongoDisconnect(con))
    
    # Get current analyses so as to add to them
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=sid)
        
    ))
    fields <- .toMongoJSON(list(analyses=1L))
    result <- con$find(filterQuery,fields)
    
    # Check what we have
    if (is.na(result$analyses))
        # Nothing to be removed
        invisible(return(FALSE))
    else {
        # Does it exist (should be) - if not just return
        if (aid %in% result$analyses[[1]]$id) {
            updateQuery1 <- .toMongoJSON(list(
                `$pull`=list(
                    analyses.id=list(
                        `_id`=list(`$oid`=sid)
                    )
                )
            ))
            invisible(con$update(filterQuery,updateQuery1))
            updateQuery2 <- .toMongoJSON(list(
                `$set`=list(
                    metadata.date_updated=unbox(Sys.time())
                )
            ))
            invisible(con$update(filterQuery,updateQuery2))
        }
        else
            invisible(return(FALSE))
    }
}

.updateAnalysisToolset <- function(aid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            toolset=.getToolset(),
            metadata.date_updated=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.updateAnalysisParams <- function(aid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            parameters=list(
                database=.tmpStaticDbVersions()
            ),
            metadata.date_updated=unbox(Sys.time())
        )
    ))
    invisible(con$update(filterQuery,updateQuery))
}

.getAnalysisName <- function(aid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
        
    ))
    fields <- .toMongoJSON(list(name=1L))
    result <- con$find(filterQuery,fields)
    
    return(result$name)
}

.analysisIdFromVariantId <- function(vid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=vid)
        
    ))
    fields <- .toMongoJSON(list(analysis_id=1L))
    result <- con$find(filterQuery,fields)
    
    return(result$analysis_id)
}

.getToolset <- function() {
    if (is.null(.CONFIG$software)) {
        log_error("FATAL! Software versions are not defined!")
        stop("FATAL! Software versions are not defined!")
    }
    tools <- .CONFIG$software
    return(lapply(names(tools),function(n) {
        return(list(
            name=n,
            version=tools[[n]]$version
        ))
    }))
}

.tmpStaticDbVersions <- function() {
    return(list(
        list(
            name="snpeff",
            fullname="SnpEff",
            version="GRCh37.p13"
        ),
        list(
            name="dbsnp",
            fullname="dbSNP",
            version="155"
        ),
        list(
            name="dbnsfp",
            fullname="dbNSFP",
            version="4.6c"
        ),
        list(
            name="dbnsfp_gene",
            fullname="dbNSFP gene",
            version="4.6c"
        ),
        list(
            name="gnomad_exomes",
            fullname="gnomAD exomes",
            version="4.0"
        ),
        list(
            name="gnomad_genomes",
            fullname="gnomAD genomes",
            version="4.0"
        ),
        list(
            name="clinvar",
            fullname="ClinVar",
            version="202412"
        ),
        list(
            name="cosmic",
            fullname="COSMIC",
            version="100"
        ),
        list(
            name="civic",
            fullname="CIViC",
            version="20241201"
        ),
        list(
            name="oncokb",
            fullname="OncoKB",
            version=gsub("-","",as.character(Sys.Date()))
        ),
        list(
            name="pharmgkb",
            fullname="PharmGKB",
            version="20240905"
        ),
        list(
            name="disgenet",
            fullname="DisGeNET",
            version="7.0"
        ),
        list(
            name="hpo",
            fullname="HPO",
            version="20240813"
        ),
        list(
            name="cgd",
            fullname="Clinical Genomics Database",
            version="20240701"
        ),
        list(
            name="ctd",
            fullname="Comparative Toxicogenomics Database",
            version="20240927"
        )
    ))
}

.isObjectId <- function(x) {
  return(grepl("^[a-fA-F0-9]{24}$",x))
}

.isEmail <- function(x) {
    regex <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    return(grepl(regex,x,ignore.case = TRUE))
}

multigrep <- function(v,x) {
    as.logical(rowSums(sapply(v,function(y) {grepl(as.character(y),x)}, 
        USE.NAMES=FALSE))) 
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

.randomString <- function(n=1,s=5) {
    NUMBERS <- as.character(seq(0,9))
    SPACE <- sample(c(LETTERS,tolower(LETTERS),NUMBERS),
        2*length(LETTERS)+length(NUMBERS))
    return(do.call(paste0,replicate(s,sample(SPACE,n,replace=TRUE),FALSE)))
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
