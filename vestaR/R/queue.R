#schedule(Q,command,code)
#Sys.sleep(1)
#execute(Q,email=input$submitterMail,host=runOpts$appaddr)

initq <- function(db) {
    if (missing(db))
        db <- tempfile()
    Q <- ensure_queue("jobs",db=db)
    remove_failed_messages(Q)
    return(Q)
}

waitq <- function(q,N=4,ms=1000) {
    M <- liteq::list_messages(q)
    n <- length(which(M$status=="WORKING"))
    while (n >= N) {
        Sys.sleep(ms/1000)
        M <- liteq::list_messages(q)
        n <- length(which(M$status=="WORKING"))
    }
    return(TRUE)
}

schedule <- function(q,fun,args,type="PROCESS",code=.randomString(1,16),
    title=NULL) {
    if (is.null(title))
        title <- paste0("Process_",format(Sys.time(),"%Y-%m-%d-%H-%M-%S"))
    
    # .queue_logger() must be called every time to ensure writing in the queues
    # log as the framework is not very robust with futures...
    .queue_logger()
    log_debug("Scheduling job of type ",type," with code ",code,
        " for analysis ",args$aid,namespace="queue")
    
    msg <- list(
        timestamp=Sys.time(),
        code=code,
        type=type,
        fun=fun,
        args=args
    )
    
    publish(q,title=title,message=toJSON(msg,null="null",auto_unbox=TRUE))
    return(TRUE)
}

execute <- function(q,N=4,ms=10000,tmout=8.64e+8,...) {
    # Check if queue is empty
    if (is_empty(q)) {
        log_debug("No pending jobs found.",namespace="queue")
        return(invisible(NULL))
    }
    
    # Try the 1st message from the queue
    msg <- consume(q)
    if (is.null(msg)) {
        log_debug("All jobs running.",namespace="queue")
        return(invisible(NULL))
    }
    
    contents <- fromJSON(msg$message)
    fun <- contents$fun
    args <- contents$args
    code <- contents$code
    jobtype <- contents$type
    
    .queue_logger()
    log_debug("Executing job of type ",jobtype," with code ",code,
        " for analysis ",args$aid,namespace="queue")
    .updateAnalysisJobId(args$aid,code)
    
    wait <- future_promise({
        # We define a log for queues
        .queue_logger()
        #print(jobtype)
        #print(code)
        #print(args$aid)
        log_info("Job of type ",jobtype," with code ",code," for analysis ",
            args$aid," entered waiting state",namespace="queue")
        
        withTimeout({
            waitq(q=q,N=N,ms=ms)
        },timeout=tmout/1000,onTimeout="error")},
        seed=TRUE
    )

    promise <- future_promise({
        .queue_logger()
        log_info("Job of type ",jobtype," with code ",code," for analysis ",
            args$aid," entered execution state",namespace="queue")
        
        withTimeout({
            switch(jobtype,
                VARIANT_ANNOTATION = {
                    annotateAndInsertVariants(aid=args$aid)
                }
            )
        },timeout=tmout/1000,onTimeout="error")},
        seed=TRUE
    )
    
    wait %>% 
        then( 
            onFulfilled=function(value) {
                if (value)
                    promise %>%
                        then(onFulfilled=function(value) {
                            # Mark success
                            .queue_logger()
                            log_info("Job of type ",jobtype," with code ",
                                code," completed!",namespace="queue")
                            # Acknowledge to the queue
                            ack(msg)
                        },onRejected=function(err) {
                            .queue_logger()
                            if (is(err,"TimeoutException")) {
                                m <- paste0("Process timeout after ",tmout,
                                    "ms!")
                                log_error("Job of type ",jobtype," with code ",
                                    code," timeout: ",m,namespace="queue")
                            }
                            else
                                log_error("Job of type ",jobtype," with code ",
                                    code," rejected from execution with ",
                                    "error: ",err$message,namespace="queue")
                            # Mark job as failed
                            nack(msg)
                            # Mark analysis as failed
                            .updateAnalysisFailReason(args$aid,err$message)
                        })
            },
            onRejected=function(err) {
                .queue_logger()
                if (is(err,"TimeoutException")) {
                    m <- paste0("Process timeout after ",tmout,"ms!")
                    log_error("Job of type ",jobtype," with code ",code,
                        " rejected with timeout: ",m,namespace="queue")
                }
                else
                    log_error("Job of type ",jobtype," with code ",code,
                        " rejected from waiting list with error: ",err$message,
                        namespace="queue")
                # Mark job as failed
                nack(msg)
                # Mark analysis as failed
                .updateAnalysisFailReason(args$aid,err$message)
            }
        )
}

.queue_logger <- function() {
    logger_debug_queue <- layout_glue_generator(
        format='{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] | {fn} | {msg}')
    log_layout(logger_debug_queue,namespace="queue",index=2)
    log_threshold(DEBUG,namespace="queue",index=2)
    logfile <- file.path(file.path(.getAppWorkspace(),"logs","queue.log"))
    log_appender(appender_file(file=logfile),namespace="queue",index=2)
}
