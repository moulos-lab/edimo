# Load R libraries
library(bcrypt)
library(base64enc)
library(emayili)
library(glue)
library(future)
library(future.callr)
library(jose)
library(jsonlite)
#library(later)
library(liteq)
library(logger)
library(mongolite)
library(plumber)
library(promises)
library(R.utils)
library(wand)

plan(callr)

# Load local libraries
#source("lib/analyses.R")
source("collections.R")
#source("lib/apis.R")
source("auth.R")
source("const.R")
source("mdb.R")
source("populate.R")
#source("lib/queue.R")
#source("lib/samples.R")
source("users.R")
source("utils.R")
source("zzz.R")

# Otherwise Dates not inserted
mongo_options(date_as_char=TRUE)

# In a package format, zzz.R should initiate the .CONFIG environment
initApp <- function(conf) {
    .initConfig("../config.json")
    .initAppPaths()
    ## Write the current pid so as to be able to easily terminate
    #pidfile <- file.path(.getAppWorkspace(),".fgf.pid")
    #writeLines(as.character(Sys.getpid()),pidfile)
}

initApp("../config.json")
# Bootstrap main database and test connection (will stop if problems)
testMongoConnection("edimoclin")

# Logger in database - index = 1 
logger_db <- suppressWarnings(layout_json(c("time","level","fn","user","msg")))
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
log_appender(appender_file(file=logfile,max_lines=1e+5,max_files=1000L),index=2)

# We need one or several collections for populating selectboxes etc.

# Init APIs - the last step as it remains open
#pr_apis <- plumb("apis.R")
pr_auth <- plumb("auth.R")
pr() %>% pr_mount("/auth",pr_auth) %>% #pr_mount("/api",pr_apis) %>% 
    pr_run(host="0.0.0.0",port=8383)
