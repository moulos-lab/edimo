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

#* Run analysis (type is determined within)
#* @param aid The analysis id to perform
#* @param uid The running user id (can be null)
#* @get /run_analysis
function(res,req,typ,aid,uid) {
    return(runAnalysis(res,req,typ,aid,uid))
}

#* Delete analysis (type is determined within)
#* @param aid The analysis id to perform
#* @param uid The running user id (can be null)
#* @delete /delete_analysis
function(res,req,aid,uid) {
    return(deleteAnalysisFiles(res,req,aid,uid))
}
