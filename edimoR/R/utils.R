generateConfigTemplate <- function() {
    # Defile a list to be converted to JSON
    # Dummy names
}

# Utility function to bypass glue and its problem with curly braces in logger
.skipFormatter <- function(...) {
    msg <- unlist(list(...))
    return(logger::skip_formatter(paste(msg,collapse="")))
}
