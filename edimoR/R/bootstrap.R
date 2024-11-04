# Load R libraries
library(bcrypt)
library(base64enc)
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
#library(wand)

plan(callr)

# Load local libraries
#source("lib/analyses.R")
#source("lib/apis.R")
#source("lib/auth.R")
#source("lib/const.R")
source("mdb.R")
#source("lib/populate.R")
#source("lib/queue.R")
#source("lib/samples.R")
#source("lib/users.R")
source("utils.R")

# Otherwise Dates not inserted
mongo_options(date_as_char=TRUE)

# Globals
CONFIG <- "/media/sevenofnine/raid/tmp/edimo/config.json"
CONFIG <- fromJSON(CONFIG)

# Get once database connection details so as not to read JSON every time
DB_CREDS <- CONFIG$databases$edimoclin # edimoclin should to some evn file
# Bootstrap main database and test connection (will stop if problems)
testMongoConnection(DB_CREDS,"edimoclin")

# Logger in database - index = 1 
logger_db <- layout_json(c("time","level","fn","user","msg"))
log_layout(logger_db,index=1)
log_appender(function(lines) {
    con <- mongoConnect(DB_CREDS,"logs")
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
logdir=file.path(WORKSPACE,"logs")
if (!dir.exists(logdir))
    dir.create(logdir,recursive=TRUE,showWarnings=FALSE)
logfile <- file.path(logdir,"fgf_lims.log")
log_appender(appender_file(file=logfile,max_lines=1e+5,max_files=1000L),index=2)

