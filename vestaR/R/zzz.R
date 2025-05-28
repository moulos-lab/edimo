# Environment
.CONFIG <- new.env()
.CONFIG$databases <- 
.CONFIG$paths <- 
.CONFIG$auth <-
.CONFIG$tmp_admin <- 
.CONFIG$rest_api <- 
.CONFIG$host <- 
.CONFIG$mail <- 
.CONFIG$software <- 
.CONFIG$static_files <- NULL

.onLoad <- function(libname,pkgname) {
    # Could assume a default location for a config file here...
}

