uploadVcfFile <- function(res,req,sid,uid) {
    # Variables come in from API string... Must Rify them
    uid <- fromJSON(uid)
    sid <- fromJSON(sid)
    
    # Initialize connection and make sure it is closed on function end
    con <- mongoConnect("users")
    on.exit(mongoDisconnect(con))

    # First we check if the user exists
    if (!is.null(uid) && !.checkId(uid,"user")) {
        msg <- paste0("User id (uid) ",uid," does not exist in the database!")
        log_error(msg)
        stop(msg)
    }
    # Also if the sample to associate the VCF file with exists
    if (!is.null(sid) && !.checkId(uid,"sample")) {
        msg <- paste0("Sample id (sid) ",sid," does not exist in the database!")
        log_error(msg)
        stop(msg)
    }
    
    # Get the running user name, existence validated
    uname <- .getUserName(uid)
    
    # Get file information
    multipart <- mime::parse_multipart(req)
    dFile <- multipart$file

    # Check if file is uploaded
    if (is.null(dFile)) {
        log_error("No file uploaded! #USER: ",uname)
        
        res$status <- 400
        return(list(error="No file uploaded"))
    }
    
    # Determine the MIME type of the file using wand::get_content_type() if
    # type is not provided with the form data. In this case, Dropjone.js (the
    # appsmith file picker), provides the type but in other cases maybe not.
    mimeType <- ifelse(!is.null(dFile$type),dFile$type,
        wand::get_content_type(dFile$datapath))

    # Define the file path where the file will be saved:
    # WORKSPACE
    # |__user_id
    #    |__samples
    #    |  |__sample_id
    #    |__|__...
    #    |__analyses
    #    |  |__analysis_id
    #    |__|__...
    samplePath <- file.path(.getAppWorkspace(),"users",uid,"samples",sid)
    if (!dir.exists(samplePath))
        dir.create(samplePath,recursive=TRUE,showWarnings=FALSE)
    
    # Handle the file based on its MIME type
    if (mimeType %in% c(
        "application/gzip",
        "application/x-gzip"
    ))
        ext <- ".vcf.gz"
    else if (mimeType %in% c(
        "text/plain",
        "text/x-vcard",
        "text/vcf"
    ))
        ext <- ".vcf"
    else {
        log_error("Unsupported file type: ",mimeType,". #USER: ",uname)
        
        res$status <- 400
        return(list(error="Unsupported file type",mimeType=mimeType))
    }
    destFile <- file.path(samplePath,paste0(sid,ext))
    
    # If all good so far:
    # 1. Copy the file to its proper location
    # 2. Update the original file name in the sample document in the database
    # 3. Update the final file name in the sample document in the database
    
    log_debug("Copying file from ",dFile$datapath," to ",destFile)
    copied <- tryCatch({
        file.copy(from=dFile$datapath,to=destFile,overwrite=TRUE)
        TRUE
    },error=function(e) {
        log_error("Failed to copy design file to proper location! ",e$message,
            " #USER: ",uname)
        return(FALSE)
    })
    
    # Return success message with file path and MIME type
    if (copied) {
        # TODO: Some instant VCF validation is required
        res$status <- 200
        return(list(
            message="File uploaded successfully",
            filePath=basename(destFile)
        ))
    }
    else {
        res$status <- 500
        return(list(
            error="File failed to copy to final location",
            filePath=basename(destFile)
        ))
    }
}

