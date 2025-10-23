# Load R libraries
library(bcrypt)
library(base64enc)
library(emayili)
library(glue)
library(future)
library(future.callr)
library(httr)
library(jose)
library(jsonlite)
library(liteq)
library(logger)
library(mongolite)
library(parallel)
library(plumber)
library(promises)
library(R.utils)
library(wand)

library(VariantAnnotation)

# ACMG related libraries - temporarily here
#~ library(gtexr)
#~ library(tidyr)
#~ library(devtools)
#~ library(dplyr)
#~ library(rvest)
#~ library(stringi)
#~ library(stringr)
#~ library(biomaRt)
#~ library(ensembldb)
#~ library(EnsDb.Hsapiens.v86)

plan(callr)

# Load local libraries
source("aggregations.R")
source("analyses.R")
source("collections.R")
source("apis.R")
source("auth.R")
source("const.R")
source("mdb.R")
source("populate.R")
source("queue.R")
source("samples.R")
source("users.R")
source("utils.R")
source("vcf-parse.R")
source("vcf-snpeff.R")
source("export.R")
source("zzz.R")

conf <- "../config.json"

startVesta <- function(conf) {
    # Otherwise Dates not inserted
    invisible(mongo_options(date_as_char=TRUE))
    
    # Init universe
    initApp(conf)
    
    # Bootstrap main database and test connection (will stop if problems)
    testMongoConnection("edimoclin")
    
    # Initialize or open queue management db - absolute path
    Q <<- initq(.getQueueDb())

    # Logger in database - index = 1 
    logger_db <- suppressWarnings(layout_json(c("time","level","fn","user",
        "msg")))
    log_layout(logger_db,index=1)
    log_appender(function(lines) {
        con <- mongoConnect("logs")
        logmsg <- fromJSON(lines)
        S <- strsplit(logmsg$msg,"#USER:")
        msg <- trimws(S[[1]][1])
        usr <- trimws(S[[1]][2])
        d <- list(
            timestamp=unbox(as.POSIXct(logmsg$time,tz="EET")),
            level=logmsg$level,
            caller=logmsg$fn,
            message=msg,
            sys_uname=logmsg$user,
            user_name=usr
        )
        d <- .toMongoJSON(d)
        con$insert(d)
        mongoDisconnect(con)
    },index=1)

    # More extensive logger in file, we must have some kind of rotation - index = 2
    logger_debug <- layout_glue_generator(
        format='{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] | {fn} | {msg}')
    log_layout(logger_debug,index=2)
    log_threshold(DEBUG,index=2)
    # Init the log with logger simple rotation
    logdir=file.path(.getAppWorkspace(),"logs") # Exists from init
    logfile <- file.path(logdir,"edimo_app.log")
    log_appender(appender_file(file=logfile,max_lines=1e+5,max_files=1000L),
        index=2)

    # Init APIs - the last step as it remains open
    pr_apis <- plumb("apis.R")
    pr_auth <- plumb("auth.R")
    
    message("Starting VESTA server...")
    server <- pr() %>% 
        pr_mount("/auth",pr_auth) %>% 
        pr_mount("/api",pr_apis) %>% 
        pr_set_debug(debug=TRUE) %>% 
        pr_run(host="0.0.0.0",port=8383)
        
    #process <- callr::r_bg(function() {
    #    pr_apis <- plumber::plumb("apis.R")
    #    pr_auth <- plumber::plumb("auth.R")
    #    api <- plumber::pr()
    #    api <- plumber::pr_mount(api,"/auth",pr_auth)
    #    api <- plumber::pr_mount(api,"/api",pr_apis)
    #    plumber::pr_run(api,host="0.0.0.0",port=8383)
    #    #plumber::pr() %>% 
    #    #    plumber::pr_mount("/auth",pr_auth) %>% 
    #    #    plumber::pr_mount("/api",pr_apis) %>% 
    #    #    plumber::pr_run(host="0.0.0.0",port=8383)
    #})
    
    #.SERVER$vesta_server <- server
    #.SERVER$vesta_server <- process
}

stopVesta <- function() {
    if (!is.null(.SERVER$vesta_server)) {
        message("Stopping VESTA server...")
        #try({
        #    .SERVER$vesta_server$stop()
        #},silent=TRUE)
        #.SERVER$vesta_server$kill()
        .SERVER$vesta_server <- NULL
    }
}


# In a package format, zzz.R should initiate the .CONFIG environment
initApp <- function(conf) {
    .initConfig(conf)
    .initAppPaths()
    ## Write the current pid so as to be able to easily terminate
    #pidfile <- file.path(.getAppWorkspace(),".fgf.pid")
    #writeLines(as.character(Sys.getpid()),pidfile)
}

#~ initApp(conf)
#~ # Bootstrap main database and test connection (will stop if problems)
#~ testMongoConnection("edimoclin")

#~ # Initialize or open queue management db - absolute path
#~ Q <- initq(.getQueueDb())

#~ # Logger in database - index = 1 
#~ logger_db <- suppressWarnings(layout_json(c("time","level","fn","user","msg")))
#~ log_layout(logger_db,index=1)
#~ log_appender(function(lines) {
#~     con <- mongoConnect("logs")
#~     logmsg <- fromJSON(lines)
#~     S <- strsplit(logmsg$msg,"#USER:")
#~     msg <- trimws(S[[1]][1])
#~     usr <- trimws(S[[1]][2])
#~     d <- list(
#~         timestamp=unbox(as.POSIXct(logmsg$time,tz="EET")),
#~         level=logmsg$level,
#~         caller=logmsg$fn,
#~         message=msg,
#~         sys_uname=logmsg$user,
#~         user_name=usr
#~     )
#~     d <- .toMongoJSON(d)
#~     con$insert(d)
#~     mongoDisconnect(con)
#~ },index=1)

#~ # More extensive logger in file, we must have some kind of rotation - index = 2
#~ logger_debug <- layout_glue_generator(
#~     format='{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] | {fn} | {msg}')
#~ log_layout(logger_debug,index=2)
#~ log_threshold(DEBUG,index=2)
#~ # Init the log with logger simple rotation
#~ logdir=file.path(.getAppWorkspace(),"logs") # Exists from init
#~ logfile <- file.path(logdir,"edimo_app.log")
#~ log_appender(appender_file(file=logfile,max_lines=1e+5,max_files=1000L),index=2)

# Init APIs - the last step as it remains open
#~ pr_apis <- plumb("apis.R")
#~ pr_auth <- plumb("auth.R")
#~ pr() %>% pr_mount("/auth",pr_auth) %>% pr_mount("/api",pr_apis) %>% 
#~     pr_run(host="0.0.0.0",port=8383)

# Tests needed to be run at some point
# 1. Collections created
# 2. Users registered and other user function
# 3. Sample uploaded
# 4. Analysis run
