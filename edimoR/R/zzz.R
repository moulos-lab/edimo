.onLoad <- function(libname,pkgname) {
    # Could assume a default location for a config file here...
}

# Environment
.CONFIG <- new.env()

# Variables that will be populated from .CONFIG upon init
.DB_CREDS <- NULL
.PATHS <- NULL
.WORKSPACE <- NULL
.THE_SECRET <- NULL
