#' @filter cors
cors <- function(req,res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    if (req$REQUEST_METHOD == "OPTIONS") {
        res$setHeader("Access-Control-Allow-Methods","*")
        res$setHeader("Access-Control-Allow-Headers",
            req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
        res$status <- 200
        return(list())
    } 
    else
        plumber::forward()
}

#* Upload VCF file
#* @param sid The sample id to associate with
#* @param uid The running user id (can be null)
#* @parser multi
#* @post /vcfupload
function(res,req,sid,uid) {
    return(uploadVcfFile(res,req,sid,uid))
}

#* Delete sample
#* @param sid The sample id to associate with
#* @param uid The running user id (can be null)
#* @delete /delete_sample
function(res,req,sid,uid) {
    return(deleteSampleFiles(res,req,sid,uid))
}
