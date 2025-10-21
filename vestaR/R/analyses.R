runAnalysis <- function(req,res,typ,aid,uid) {
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))

    # First we check if the user exists
    if (!is.null(uid) && !.checkId(uid,"user")) {
        msg <- paste0("User id (uid) ",uid," does not exist in the database!")
        log_error(msg)
        stop(msg)
    }
    # Also if the analysis to run has been recorded in the database
    if (!is.null(aid) && !.checkId(aid,"analysis")) {
        msg <- paste0("Analysis id (aid) ",aid," does not exist in the ",
            "database!")
        log_error(msg)
        stop(msg)
    }
    
    # Get the running user name, existence validated
    uname <- .getUserName(uid)
    
    # As the rest of the contents will be queued with futures, all the rest
    # operations are executed by the wrapper functions.
    log_info("Queueing analysis ",aid," #USER: ",uname)
    
    # Determine analysis type and queue calling the respective wrapper
    # Two main analysis types: VARIANT_ANNOTATION: 1, ENDTOEND: 2. The subtypes 
    # will be determined from aid and database querying.
    scheduled <- FALSE
    if (typ == 1)
        scheduled <- schedule(Q,"annotateAndInsertVariants",args=list(aid=aid),
            type="VARIANT_ANNOTATION")
    #else if (typ == 2)
    #   // Stub
    
    Sys.sleep(1)
    if (scheduled) {
        execute(Q,N=.getJobConcurrency(),ms=.getQueuePollInterval(),
            tmout=.getJobTimeout())
        
        log_info("Analysis ",aid," scheduled for execution.")
                
        res$status <- 201
        return(list(message="Analysis successfully scheduled!"))
    }
        
    else {
        log_error("Failed to schedule job for analysis ",aid,"! Check logs")

        res$status <- 500
        return(list(error="Failed to schedule analysis job! in database"))
    }
}

deleteAnalysisFiles <- function(res,req,aid,uid) {
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect("users")
    on.exit(mongoDisconnect(con))

    # First we check if the user exists
    if (!is.null(uid) && !.checkId(uid,"user")) {
        msg <- paste0("User id (uid) ",uid," does not exist in the database!")
        log_error(msg)
        stop(msg)
    }
    # Also if the sample to associate the VCF file with exists
    if (!is.null(aid) && !.checkId(aid,"analysis")) {
        msg <- paste0("Analysis id (aid) ",aid," does not exist in the ",
            "database!")
        log_error(msg)
        stop(msg)
    }
    
    # Get the running user name, existence validated
    uname <- .getUserName(uid)
    
    # Construct the file path for deletion
    analysisPath <- file.path(.getAppWorkspace(),"users",uid,"analyses",aid)
    if (!dir.exists(analysisPath)) {
        log_error("Analysis ",aid," files not found in server. #USER: ",uname)
        
        res$status <- 400
        return(list(error="Analysis files not found in server."))
    }

    log_debug("Deleting analysis directory ",analysisPath)
    tryCatch({
        unlink(analysisPath,recursive=TRUE)
        
        log_info("Analysis ",aid," files successfully deleted. #USER: ",uname)
            
        res$status <- 200
        return(list(message="Analysis files successfully deleted."))
    },error=function(e) {
        log_error("Failed to delete analysis ",aid," files! ",e$message,
            " #USER: ",uname)
        
        res$status <- 400
        return(list(error="Failed to delete analysis files from server."))
    })
}

downladVariants <- function(res,req) {
    # Incoming data from request body
    aid <- req$body$aid
    vid <- req$body$vid
    que <- req$body$que
    groups <- unlist(req$body$groups)
    fields <- unlist(req$body$fields)
    canonical <- req$body$canonical
    form <- req$body$file_format
    uid <- req$body$uid
    
    # First we check if the user exists
    if (!is.null(uid) && !.checkId(uid,"user")) {
        msg <- paste0("User id (uid) ",uid," does not exist in the database!")
        log_error(msg)
        #stop(msg)
        res$status <- 400
        return(list(error=msg))
    }

    # Also if the analysis exists - probably overkill
    if (!is.null(aid) && !.checkId(aid,"analysis")) {
        msg <- paste0("Analysis id (aid) ",aid," does not exist in the ",
            "database!")
        log_error(msg)
        #stop(msg)
        res$status <- 400
        return(list(error=msg))
    }
    
    # Get the running user name, existence validated
    uname <- .getUserName(uid)

    # Only one of aid, vid or que can be provided - respond early
    checkMain <- c(!is.null(aid),!is.null(vid),!is.null(que))
    if (length(which(checkMain)) > 1) {
        err <- paste0("Only one of analysis id, variant ids vector or MongoDB ",
            "query (que) can be provided!")
        log_error(err)
        
        res$status <- 500
        return(list(error=err))
    }
    

    # If a query has been passed, sanitize it
    oid <- aid  # For later logging as NULL crashes the logger
    if (!is.null(que) && !is.null(que$analysis_id)) {
        oid <- .extractObjectId(que$analysis_id)
        que$analysis_id <- list(`$oid`=oid)
    }
    if (!is.null(groups) && length(groups) == 1 && groups == "all")
        groups <- .mongoVariantFieldGroupNames()
    if (!is.null(fields) && length(fields) == 1 && fields == "all")
        fields <- .mongoVariantFields()

    tryCatch({
        log_info("Preparing download file for analysis ",oid," #USER: ",uname)

        theFile <- exportVariants(aid=aid,vid=vid,que=que,groups=groups,
            fields=fields,outFormat=form,canonical=canonical)
        
        # Add a suffix depnding on what is required
        suf <- ifelse(is.null(aid),"_filtered","_all")
        # ...and decide on prefix (analysis id, oid exists already)
        pre <- ifelse(is.null(aid),oid,aid)
        
        # Proper headers to send as binary attachment to appsmith
        res$setHeader("Access-Control-Expose-Headers","Content-Disposition")
        res$setHeader("Content-Type","application/x-gzip")
        res$setHeader("Content-Disposition",paste0('attachment; filename="',
            pre,suf,'.',form,'.gz"'))
        
        # The only way to return binary, answer from Gemini
        log_info("Sending requested file for analysis ",oid," #USER: ",uname)
        res$body <- readBin(theFile,what="raw",n=file.info(theFile)$size)
        res
    },error=function(e) {
        log_error("File creation failed: ",e$message," #USER: ",uname)

        res$status <- 500
        return(list(error=paste0("File creation failed: ",e$message)))
    })
}

