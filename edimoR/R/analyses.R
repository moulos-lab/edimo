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
    log_info("Queueing analysis ",aid," #USER: ",unname)
    
    # Determine analysis type and queue calling the respective wrapper
    # Two main analysis types: VARIANT_ANNOTATION: 1, ENDTOEND: 2. The subtypes 
    # will be determined from aid and database querying.
    if (typ == 1)
        scheduled <- schedule(Q,"annotateAndInsertVariants",args=list(aid=aid),
            type="VARIANT_ANNOTATION")
    #else if (typ == 2)
    #   // Stub
    
    Sys.sleep(1)
    if (scheduled)
        execute(Q,N=.getJobConcurrency(),ms=.getQueuePollInterval(),
            timeout=.getJobTimeout())
    else {
        log_error("Failed to schedule job for analysis ",aid,"! Check logs")
                
        res$status <- 500
        return(list(error="Failed to schedule analysis job! in database"))
    }
}
