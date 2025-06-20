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

#* Download VCF file
#* @param sid The sample id to associate with
#* @param uid The running user id (can be null)
#* @get /vcfdownload
function(res,req,sid,uid) {
    return(downloadVcfFile(res,req,sid,uid))
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

#~ #* Download TSV file for analysis
#~ #* @param aid The analysis id to fetch variants
#~ #* @param uid The running user id (can be null)
#~ #* @get /tsvdownload
#~ function(res,req,sid,uid) {
#~     return(downladAnalysisTsv(res,req,aid,uid))
#~ }

#* Download TSV file for specific variants
#* @parser json list(simplifyVector = FALSE)
#* @post /vardownload
function(res,req) {
    return(downladVariants(res,req))
}

#~ #* Test 
#~ #* @parser json list(simplifyVector = FALSE)
#~ #* @post /quetest
#~ function(req, res) {
#~   # Parse JSON from the request body
#~   que <- req$body$que
  
#~   oid <- .extractObjectId(que$analysis_id)
#~   que$analysis_id <- list(`$oid`=oid)
  
#~   #que$analysis_id <- .replaceObjectIds(que$analysis_id)
#~   que <- .toMongoJSON(que,pretty=T)
  
#~   #que <- .replaceObjectIds(que)
#~   #que <- .replaceObjectIds(.toMongoJSON(z,pretty=T))
#~   print(que)
#~ }
