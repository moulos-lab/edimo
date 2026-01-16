#annotateAndInsertVariants("6780e4e856bbcd63da4958f0")
#annotateAndInsertVariants("6780f37d56bbcd63da4958f3")
annotateAndInsertVariants <- function(aid) {
    # Get VCF file and genome from analysisId
    # Open the VCF file in chunks of X variants
    # Call vcfToList
    # JSON the list, add the analysisId and insert
    # TODO: Update analysis steps etc.
    
    cona <- mongoConnect("analyses")
    conv <- mongoConnect("variants")
    cons <- mongoConnect("samples")
    on.exit({
        mongoDisconnect(cona)
        mongoDisconnect(conv)
        mongoDisconnect(cons)
    })
    
    # We assume that analysisId is checked for existence on API call so we do
    # not do it here - leave for now
    if (missing(aid) || !.checkId(aid,"analysis")) {
        msg <- paste0("Analysis id (aid) ",aid," does not exist in the ",
            "database!")
        #log_error(msg)
        stop(msg)
    }
    
    # 1. Retrieve the analysis document
    query <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    fields <- .toMongoJSON(list(
        name=1L,
        metadata=1L,
        samples=1L,
        ownership=1L
    ))
    
    ## We cannot log at this point within futures...
    #log_debug("Querying database for analysis ",analysisId,".")
    #log_debug(.skipFormatter("MongoDB query is: ",query))
    #log_debug(.skipFormatter("The selected fields are: ",fields))
    
    result <- cona$find(query=query,fields=fields)
    
    # 2. Create the analysis directory structure
    uid <- result$ownership$inserted_by$id
    #uname <- .getUserName(uid)
    analysisPath <- file.path(.getAppWorkspace(),"users",uid,"analyses",aid)
    if (!dir.exists(analysisPath))
        dir.create(analysisPath,recursive=TRUE,showWarnings=FALSE)
    
    # 4. Re-initialize purpose-specific loggers as this will be running within
    #    futures framework, however, not needed when not with futures?
    .analysis_logger(analysisPath)
    
    # 5. Retrieve the sample name/id and copy to analysis directory
    sid <- result$samples[[1]]$id
    pat <- paste0("^",sid,"\\.vcf(?:\\.gz)?$")
    sampleDir <- file.path(.getAppWorkspace(),"users",uid,"samples",sid)
    sampleFile <- dir(sampleDir,pattern=pat,full.names=TRUE)[1]
    if (is.na(sampleFile)) {
        # Is it a shared sample? Try to find the owner
        sharedSampleQuery <- .toMongoJSON(list(
            `_id`=list(`$oid`=sid)
            
        ))
        sharedSampleFields <- .toMongoJSON(list("ownership.inserted_by"=1L))
        sharedResult <- cons$find(sharedSampleQuery,sharedSampleFields)
        
        if (nrow(sharedResult) > 0) {
            msg <- paste0("The required analysis file is shared. Looking to ",
                "respective directory.")
            log_info(msg)
            sampleDir <- file.path(.getAppWorkspace(),"users",
                sharedResult$ownership$inserted_by$id,"samples",sid)
            sampleFile <- dir(sampleDir,pattern=pat,full.names=TRUE)[1]
        }
        
        # If not there either, then fail
        if (is.na(sampleFile)) {
            msg <- paste0("The required analysis file could not be found in ",
            "the respective directory ",sampleDir,"! Consult admin.")
            log_error(msg)
            stop(msg)
        }
    }
    
    destFile <- file.path(analysisPath,basename(sampleFile))
    
    log_debug("Copying file from ",sampleFile," to ",destFile)
    
    copied <- tryCatch({
        file.copy(from=sampleFile,to=destFile,overwrite=TRUE)
    },error=function(e) {
        #log_error("Failed to copy analysis file to proper location! ",e$message,
        #    " #USER: ",uname)
        log_error("Failed to copy analysis file to proper location! ",e$message)
        return(FALSE)
    })
    
    # 6. If properly copied, initiate analysis - individual simple log files are
    # created per analysis step
    nsteps <- .getNoAnalysisSteps("basic")
    if (copied) {
        log_info("Initiating analysis ",aid," of type ",
            result$metadata$tertiary_protocol[1])

        log_info("Executing basic VCF annotation for analysis ",aid)
        #message("Executing basic VCF annotation for analysis ",aid)
        annoFile <- annotateVcf(destFile,gv=result$metadata$genome_version[1],
            aid=aid)
        
        log_info("Retrieving chromosome list and size for analysis ",aid)
        .getGenomeSize(annoFile,sid)
        
        # TODO: In the future. vcfToList will accept one more argument according
        #       to variant annotation type (somatic, germline, etc.)
        aType <- "generic" # Will be analysis-based
        log_info("Executing extended VCF annotation for analysis ",aid)
        #message("Executing extended VCF annotation for analysis ",aid)
        theList <- vcfToList(
            vcfFile=annoFile,
            gv=result$metadata$genome_version[1],
            chunkSize=.getVcfReadChunkSize(),
            aType=aType
        )
        # Analysis step 7 complete - update progress
        .updateAnalysisProgress(aid,7,100*(7/nsteps),"Annotations")
        
        log_info("Adding analysis id and text search helper for analysis ",aid)
        theList <- lapply(theList,function(x) {
            x$analysis_id <- list(`$oid`=aid)
            x$search_text <- .makeSearchText(x)
            return(x)
        })
        
        log_info("Assigning variant stores to new data")
        userVarsWithStores <- .findVariantsWithVarstores(uid)
        if (nrow(userVarsWithStores) > 0) {
            storeMap <- .buildVarstoreMap(userVarsWithStores)
            theList <- lapply(theList,function(x,M) {
                key <- .identityKey(x)
                if (!is.null(M[[key]])) {
                    vs <- unname(apply(M[[key]],1,as.list))
                    vs <- lapply(vs,function(y) {
                        y$id <- list(`$oid`=y$id)
                        return(y)
                    })
                    x$varstores <- vs
                }
                return(x)
            },storeMap)
        }
        
        log_info("Inserting to database for analysis ",aid)
        #message("Inserting to database for analysis ",aid)
        if (length(theList) >= .getJsonifyLimit()) {
            mongoJson <- cmclapply(theList,.toMongoJSON,rc=.getCoresFraction())
            mongoJson <- unlist(mongoJson,recursive=FALSE)
        }
        else
            mongoJson <- sapply(theList,.toMongoJSON)
        
        ins <- tryCatch({
            conv$insert(mongoJson)
        },error=function(e) {
            log_error("Variants for analysis ",aid," failed to insert! ",
                e$messsage) 
            return(NULL)
        })
        
        if (!is.null(ins) && is(ins,"miniprint")) {
            if (ins$nInserted == length(mongoJson))
                #message(length(mongoJson)," variants successfully inserted ",
                #    "for analysis ",aid)
                log_info(length(mongoJson)," variants successfully inserted ",
                    "for analysis ",aid)
            else
                #message("Only ",ins$nInserted," variants successfully ",
                #    "inserted for analysis ",aid,". Contact admins.")
                log_warn("Only ",ins$nInserted," variants successfully ",
                    "inserted for analysis ",aid,". Contact admins.")
            
            # And calculate several stats for graphs and filters
            log_info("Calculating analysis statistics for analysis ",aid)
            upd <- tryCatch({
                updateAnalysisStats(aid)
            },error=function(e) {
                log_error("Statistics calculation for analysis ",aid," failed!",
                    e$messsage)
                return(NULL)
            })
            
            if (!is.null(upd) && is(upd,"miniprint")) {
                if (upd$modifiedCount == 1)
                    log_info("Statistics calculation for analysis ",aid,
                        " successful!")
                else
                    log_error("Statistics insert for analysis ",aid," failed! ",
                        "Contact admins.")
            }
        }
        # Analysis step 8 complete - update progress
        .updateAnalysisProgress(aid,8,100*(8/nsteps),"Complete")
        .markAnalysisSuccessfull(aid)
        # ...and add analysis id to the analyzed sample
        .updateSampleAnalyses(sid,aid)
        # Add toolset and database versions (TODO: db versions dynamically)
        .updateAnalysisToolset(aid)
        .updateAnalysisParams(aid)
    }
    else {
        .updateAnalysisProgress(aid,8,100*(8/nsteps),"Failed")
        .updateAnalysisFailReason(aid,
            "Failed to copy analysis file to proper location! Check logs.")
        log_error("Failed to copy analysis file to proper location!")
        stop("Failed to copy analysis file to proper location! Check logs.")
    }
    
    return(invisible(TRUE))
}

vcfToList <- function(vcfFile,gv=c("hg19","hg38"),chunkSize=5000,
    aType=c("generic","somatic","germline")) {
    if (!requireNamespace("VariantAnnotation")) {
        log_error("Bioconductor package VariantAnnotation is required!")
        stop("Bioconductor package VariantAnnotation is required!")
    }
    
    if (!is.character(vcfFile)) {
        log_error("vcfFile argument must be a proper file name!")
        stop("vcfFile argument must be a proper file name!")
    }
    if (!file.exists(vcfFile)) {
        log_error("The input VCF file ",vcfFile," does not exist!")
        stop(paste0("The input VCF file ",vcfFile," does not exist!"))
    }
    
    aType <- aType[1]
    
    .checkTextArgs("Genome version",gv,c("hg19","hg38"),multiarg=FALSE)
    .checkNumArgs("VCF chunk size (chunkSize)",chunkSize,"numeric",1000,"gt")
    .checkTextArgs("Annotation type",aType,c("generic","somatic","germline"),
        multiarg=FALSE)
    
    # We now decide whether to read the total file or by chunks. If the file is
    # larger than 5000 lines, we read by chunks of 5000
    log_info("Reading VCF file ",vcfFile)
    #message("Reading VCF file ",vcfFile)
    nl <- R.utils::countLines(vcfFile)
    log_info("Input VCF file contains around ",nl," variants")
    #message("Input VCF file contains around ",nl," variants")
    if (nl > .getVcfReadChunkSize()) {
        log_info("I will annotate in chunks of ",chunkSize," variants")
        #message("I will annotate in chunks of ",chunkSize," variants")
        chunkNo <- 0
        varsList <- list()
        vcfCon <- VcfFile(vcfFile,yieldSize=chunkSize)
        open(vcfCon)
        repeat {
            chunkNo <- chunkNo + 1
            vcf <- suppressWarnings(readVcf(vcfCon))
            if (length(vcf) == 0)
                break
            log_info("Annotating chunk ",chunkNo)
            #message("Annotating chunk ",chunkNo)
            #nucleoGenos <- readGT(vcfCon,nucleotides=TRUE)
            nucleoGenos <- tryCatch({
                readGT(vcfCon,nucleotides=TRUE)
            },error=function(e) {
                log_warn("Error parsing genotypes in nucleotide format: ",
                    e$message)
                log_info("Trying to parse potential polyploidies")
                return(readPolyGT(vcfCon))
            })
            varsList[[chunkSize]] <- .vcfToListWorker(vcf,nucleoGenos,gv,aType)
        }
        close(vcfCon)
        return(do.call("c",varsList))
    }
    else {
        log_info("Αnnotating in a single pass")
        # Warnings may be produced during validation - some dNSFP versions 
        # produce problematic VCF headers
        vcf <- suppressWarnings(readVcf(vcfFile))
        # If polyploid variants exist (e.g. Mutect2) readGT fails...
        #nucleoGenos <- readGT(vcfFile,nucleotides=TRUE)
        nucleoGenos <- tryCatch({
            readGT(vcfFile,nucleotides=TRUE)
        },error=function(e) {
            log_warn("Error parsing genotypes in nucleotide format: ",e$message)
            log_info("Trying to parse potential polyploidies")
            return(readPolyGT(vcfFile))
        })
        return(.vcfToListWorker(vcf,nucleoGenos,gv,aType))
    }
}
    
.vcfToListWorker <- function(vcf,nucleoGenos,gv,aType) {    
    # Derive the main building blocks of the list that will represent a document
    # for a single variant in the database. With the building block below, we
    # build the initial document list which will later be enriched from entries
    # from our knowledge base and API calls.
    log_info("Building variant document blocks")
    #message("Building variant document blocks")
    
    # dbSNP id arrays
    rsids <- .parseID(names(vcf))
    
    # Fixed identification fields (coordinates, quality etc.)
    fixed <- data.frame(
        chromosome=as.character(seqnames(vcf)),
        start=as.integer(start(vcf)),
        end=as.integer(end(vcf)),
        ref=as.character(ref(vcf)),
        alt=.makeAlt(vcf),
        qual=qual(vcf),
        type=unlist(info(vcf)[["VARTYPE"]])
    )
    # Genomic HGVS for search in COSMIC and MyVariant API
    fixed$hgvsg <- .makeHgvsg(fixed$chromosome,fixed$start,fixed$ref,fixed$alt)
    rownames(fixed) <- fixed$hgvsg
    
    # INFO metrics, depth etc.
    metrics <- .greedyUncompressDataFrame(info(vcf)[,.getDefaultInfoFields()])
    
    # FORMAT fields including genotype metrics
    # If VCF normalized as per our workflow, this should work, also attach
    # nucleotide version of genotypes
    genos <- as.data.frame(cbind(nucleoGenos,do.call("cbind",geno(vcf))))
    genos[,1] <- as.character(unname(genos[,1]))
    genos[,2] <- as.character(unname(genos[,2]))
    genos[,3] <- as.numeric(unname(genos[,3]))
    genos[,5] <- as.numeric(unname(genos[,5]))
    genos[,6] <- as.numeric(unname(genos[,6]))
    genos[,7] <- as.numeric(unname(genos[,7]))
    colnames(genos) <- c("GTN",.getDefaultGenoFields())
    # Split the AD column
    ads <- do.call("rbind",unname(genos[,4]))
    colnames(ads) <- c("AD_REF","AD_ALT")
    genos <- genos[,-4]
    genos <- cbind(genos,ads)
    rownames(genos) <- NULL
    # Basic SnpEff annotations
    ANN <- info(vcf)["ANN"]
    annList <- cmclapply(ANN[,1],.parseANN,rc=.getCoresFraction())
    
    # dbNSFP annotations - + and - symbols are replaced with _ during import...
    # We split pathogenicity, conservation and population as these will be
    # different section in the variant document
    dbnsfpNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpFields())
    dbnsfp <- .greedyUncompressDataFrame(info(vcf)[,dbnsfpNames])
    dbnsfp[dbnsfp=="."] <- NA
    tonum <- setdiff(seq_along(colnames(dbnsfp)),grep("_pred",colnames(dbnsfp)))
    # Some Aloft probabilities are malformed - it's OK
    dbnsfp[,tonum] <- suppressWarnings(sapply(dbnsfp[,tonum],as.numeric))
    
    # Population annotations
    gnomadExomes <- 
        .greedyUncompressDataFrame(info(vcf)[,.getVcfGnomadExomeFields()])
    gnomadGenomes <- 
        .greedyUncompressDataFrame(info(vcf)[,.getVcfGnomadGenomeFields()])
    alfas <- .greedyUncompressDataFrame(info(vcf)[,.getVcfAlfaFields()])
    alfas <- as.data.frame(do.call("cbind",lapply(alfas,as.numeric)))
    
    # Clinvar annotations
    clinvars <- .greedyUncompressDataFrame(info(vcf)[,.getVcfClinvarFields()])
    clinvars[,2] <- gsub("_"," ",clinvars[,2])
    clinvars[,3] <- gsub("_"," ",clinvars[,3])
    
    # Get all genes present in VCF to construct hashes of hits in backend
    # collections such as disgenet, hpo etc.
    log_info("Retrieving annotation elements from knowledge base")
    #message("Retrieving annotation elements from knowledge base")
    vcfVariants <- unlist(rsids)
    vcfVariants <- vcfVariants[grepl("^rs",vcfVariants)]
    vcfGenes <- unique(unlist(lapply(annList,function(x) x[,"gene_name"])))
        
    geneResource <- .makeGeneResource(aType,vcfGenes)
    variantResource <- .makeVariantResource(aType,vcfVariants,annList,fixed,gv)
    
    log_info("Building variant documents")
    #message("Building variant documents")
    # Ultimately this may end very large... Maybe we could process it in chunks
    # and insert in chunks... Like tabix the VCF and use open and yield with
    # this function... This would also reduce the burden on API calls...
    docs <- lapply(seq_along(vcf),function(i) {
        list(
            identity=.getIdentity(fixed[i,,drop=FALSE],rsids[[i]],
                variantResource$cosmic),
            metrics=.getMetrics(metrics[i,]),
            genotypes=.getGenotype(genos[i,,drop=FALSE]),
            pathogenicity=.getPathogenicity(dbnsfp[i,,drop=FALSE],
                clinvars[i,,drop=FALSE]),
            conservation=.getConservation(dbnsfp[i,,drop=FALSE]),
            population=.getPopulation(dbnsfp[i,,drop=FALSE],
                gnomadExomes[i,,drop=FALSE],gnomadGenomes[i,,drop=FALSE],
                alfas[i,,drop=FALSE]),
            annotation=list(
                genes=.getGenes(annList[[i]],geneResource),
                variant=.getVariant(rsids[[i]],fixed$hgvsg[i],variantResource)
            ),
            classification=.getClassification(),
            varstores=list()
        )
    })
    log_info("Done!")
    #message("Done!")
    return(docs)
}

.identityKey <- function(v) {
    return(paste0(v$identity$chr,":",v$identity$start,"_",v$identity$ref,"/",
        v$identity$alt))
}

.findVariantsWithVarstores <- function(uid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$lookup"=list(
                "from"="analyses",
                "localField"="analysis_id",
                "foreignField"="_id",
                "as"="analysis"
            )
        ),
        list(
            "$unwind"="$analysis"
        ),
        list(
            "$match"=list(
                "analysis.ownership.inserted_by.id"=list(`$oid`=uid),
                "varstores.0"=list("$exists"=TRUE)
            )
        ),
        list(
            "$project"=list(
                "identity.chr"="$identity.chr",
                "identity.start"="$identity.start",
                "identity.ref"="$identity.ref",
                "identity.alt"="$identity.alt",
                "varstores"="$varstores"
            )
        )
    )
    
    return(con$aggregate(.toMongoJSON(pipeline)))
}

.buildVarstoreMap <- function(docs) {
    map <- list()
    for (i in seq_len(nrow(docs))) {
        v <- docs[i,]
        key <- .identityKey(v)
        if (is.null(map[[key]]))
            map[[key]] <- v$varstores[[1]]
        else {
            map[[key]] <- rbind(map[[key]],v$varstores[[1]])
            map[[key]] <- unique(map[[key]])
        }           
    }
    return(map)
}

.makeGeneResource <- function(aType,vcfGenes) {
    switch(aType,
        generic = {
            log_info("DisGeNET for genes")
            #message("  DisGeNET for genes")
            disgenetGeneHits <- .findDisgenetByGene(vcfGenes)
            log_info("Retrieved ",nrow(disgenetGeneHits)," hits")
            #message("    Retrieved ",nrow(disgenetGeneHits)," hits")
            
            log_info("Human Phenotype Ontology")
            #message("  Human Phenotype Ontology")
            hpoHits <- .findHpoByGene(vcfGenes)
            log_info("Retrieved ",length(hpoHits)," hits")
            #message("    Retrieved ",length(hpoHits)," hits")
            
            log_info("Comparative Toxicogenomics Database")
            #message("  Comparative Toxicogenomics Database")
            ctdHits <- .findCtdByGene(vcfGenes)
            log_info("Retrieved ",length(ctdHits)," hits")
            #message("    Retrieved ",length(ctdHits)," hits")
            
            log_info("Clinical Genomics Database")
            #message("  Clinical Genomics Database")
            cgdHits <- .findCgdInheritanceByGene(vcfGenes)
            log_info("Retrieved ",nrow(cgdHits)," hits")
            #message("    Retrieved ",nrow(cgdHits)," hits")
            
            # Silently also add SO localizations
            return(list(
                disgenet=disgenetGeneHits,
                hpo=hpoHits,
                ctd=ctdHits,
                cgd=cgdHits,
                sol=read.delim(.getStaticFile("hg19","so_localizations"))
            ))
        },
        somatic = {},
        germline = {}
    )
}

.makeVariantResource <- function(aType,vcfVariants,annList,fixed,gv) {
    switch(aType,
        generic = {
            log_info("DisGeNET for variants")
            #message("  DisGeNET for variants")
            disgenetVariantHits <- .findDisgenetByVariant(vcfVariants)
            log_info("Retrieved ",nrow(disgenetVariantHits)," hits")
            #message("    Retrieved ",nrow(disgenetVariantHits)," hits")
            
            log_info("COSMIC")
            #message("  COSMIC")
            cosmicHits <- .findCosmicByVariant(fixed$hgvsg,gv)
            log_info("Retrieved ",nrow(cosmicHits)," hits")
            #message("    Retrieved ",nrow(cosmicHits)," hits")

            log_info("PharmGKB")
            #message("  PharmGKB")
            pharmgkbHits <- .findPharmGkbByVariant(vcfVariants)
            log_info("Retrieved ",length(pharmgkbHits)," hits")
            #message("    Retrieved ",length(pharmgkbHits)," hits")
            
            log_info("OncoKB")
            #message(" OncoKB")
            allSoTerms <- unname(sapply(annList,function(x) {
                return(x[1,"detailed_impact_so_term"])
            }))
            rind <- .filterSoTermsForApiCalls(allSoTerms)
            oncokbHits <- .queryOncoKB(fixed$hgvsg[rind],gv)
            names(oncokbHits) <- fixed$hgvsg[rind]
            # Clean the hits from empty genes because of improper hgvs parsing
            oncokbHits = oncokbHits[!sapply(oncokbHits,
                function(x) is.null(x$gene_symbol) || x$oncogenic=="Unknown")]
            log_info("Retrieved ",length(oncokbHits)," hits")
            #message("    Retrieved ",length(oncokbHits)," hits")
            
            log_info("CiVIC")
            #message("  CiVIC")
            civicHits <- .findCivicByOneLetterAA(fixed,annList,gv)
            log_info("Retrieved ",if (is.null(civicHits)) 0 else 
                nrow(civicHits)," hits")
            #message("    Retrieved ",nrow(civicHits)," hits")
            
            return(list(
                disgenet=disgenetVariantHits,
                cosmic=cosmicHits,
                pharmgkb=pharmgkbHits,
                oncokb=oncokbHits,
                civic=civicHits
            ))
        },
        somatic = {},
        germline = {}
    )
}

.getIdentity <- function(fixed,rsids,ch) {
    return(list(
        chr=fixed$chromosome,
        start=fixed$start,
        end=fixed$end,
        ref=fixed$ref,
        alt=fixed$alt,
        qual=fixed$qual,
        type=fixed$type,
        rsid=if (length(rsids)==1) list(rsids) else rsids,
        cosmic=ch[rownames(fixed),"cosmic_id"]
    ))
}

.getMetrics <- function(metrics) {
    return(list(
        ac=ifelse(is.na(metrics$AC),metrics$AO,metrics$AC),
        af=metrics$AF,
        ao=ifelse(is.na(metrics$AO),metrics$AC,metrics$AO),
        dp=metrics$DP,
        sbp=metrics$SBP,
        srf=metrics$SRF,
        srr=metrics$SRR,
        saf=metrics$SAF,
        sar=metrics$SAR
    ))
}

.getGenotype <- function(genos) {
    return(list(
        gtn=genos$GTN,
        gn=genos$GT,
        dp=genos$DP,
        ad_ref=genos$AD_REF,
        ad_alt=genos$AD_ALT,
        af=genos$AF,
        gq=genos$GQ,
        sb=genos$SB
    ))
}

.getPathogenicity <- function(dbnsfp,clinvar) {
    dbnsfpPathoNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpPathogenicityFields())
    patho <- dbnsfp[,dbnsfpPathoNames]
    colnames(patho) <- tolower(gsub("dbNSFP_","",colnames(patho)))
    #patho[patho == "."] <- NA
    colnames(clinvar) <- tolower(colnames(clinvar))
    return(as.list(cbind(patho,clinvar)))
}

.getConservation <- function(dbnsfp) {
    dbnsfpConseNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpConservationFields())
    cons <- dbnsfp[,dbnsfpConseNames]
    #cons[cons == "."] <- NA
    colnames(cons) <- tolower(gsub("dbNSFP_","",colnames(cons)))
    return(as.list(cons))
}

# Arguments will be added when auto ready
.getClassification <- function() {
    return(list(
        acmg=NULL,
        amp=NULL,
        manual_sig=NULL,
        manual_onc=NULL,
        notes=NULL,
        classified_by=NULL,
        edited_by=NULL,
        metadata=list(
            date_updated=NULL,
            notes=NULL
        )
    ))
}

.getPopulation <- function(dbnsfp,gnomadExomes,gnomadGenomes,alfas) {
    dbnsfpPopulNames <- .getVcfDbnsfpPopulationFields()
    dbnsfp <- dbnsfp[,dbnsfpPopulNames]
    
    tgp <- dbnsfp[,grep("1000Gp3",colnames(dbnsfp))]
    colnames(tgp) <- tolower(gsub("dbNSFP_1000Gp3_","",colnames(tgp)))
    #tgp[tgp == "."] <- NA
    exac <- dbnsfp[,grep("ExAC",colnames(dbnsfp))]
    colnames(exac) <- tolower(gsub("dbNSFP_ExAC_","",colnames(exac)))
    #exac[exac == "."] <- NA
    esp <- dbnsfp[,grep("ESP6500",colnames(dbnsfp))]
    colnames(esp) <- tolower(gsub("dbNSFP_ESP6500_","",colnames(esp)))
    #esp[esp== "."] <- NA
    uk10k <- dbnsfp[,grep("UK10K",colnames(dbnsfp))]
    colnames(uk10k) <- tolower(gsub("dbNSFP_UK10K_","",colnames(uk10k)))
    #uk10k[uk10k == "."] <- NA
    #alfa <- dbnsfp[,grep("ALFA",colnames(dbnsfp))]
    #colnames(alfa) <- tolower(gsub("dbNSFP_ALFA_","",colnames(alfa)))
    #alfa[alfa == "."] <- NA
    
    colnames(gnomadExomes) <- tolower(gsub("gnomAD_exomes_","",
        colnames(gnomadExomes)))
    colnames(gnomadGenomes) <- tolower(gsub("gnomAD_genomes_","",
        colnames(gnomadGenomes)))
    colnames(alfas) <- tolower(gsub("ALFA_","",colnames(alfas)))
    
    return(list(
        tgp=as.list(tgp),
        exac=as.list(exac),
        esp=as.list(esp),
        uk10k=as.list(uk10k),
        alfa=as.list(alfas),
        gnomad_exomes=as.list(gnomadExomes),
        gnomad_genomes=as.list(gnomadGenomes)
    ))
}

#.getClinvar <- function(clinvar) {
#    clinvarNames <- gsub("[\\+\\-]","_",.getVcfClinvarFields())
#    clin <- clinvar[,clinvarNames]
#    colnames(clin) <- tolower(gsub("ClinVar_","",colnames(clin)))
#    return(as.list(clin))
#}

.getGenes <- function(annInstance,geneResource) {
    colnames(annInstance)[c(1,2)] <- c("impact_so","impact_snpeff")
    G <- split(as.data.frame(annInstance),annInstance[,"gene_name"])
    return(lapply(unname(G),.getOneGene,geneResource))
}

.getOneGene <- function(g,r) {
    gn <- g$gene_name[1]
    disgenet <- .formatGeneResource(gn,r,"disgenet")
    hpo <- .formatGeneResource(gn,r,"hpo")
    ctd <- .formatGeneResource(gn,r,"ctd")
    return(list(
        name=gn,
        ids=.formatGeneResource(gn,r,"ids"),
        omim=.formatGeneResource(gn,r,"omim"),
        omim_morbid=.formatGeneResource(gn,r,"omim_morbid"),
        cgd=.formatGeneResource(gn,r,"cgd"),
        disgenet=disgenet,
        disgenet_length=length(disgenet),
        hpo=hpo,
        hpo_length=length(hpo),
        ctd=ctd,
        ctd_length=length(ctd),
        dbnsfp_gene=.formatGeneResource(gn,r,"dbnsfp_gene"),
        transcripts=.getTranscripts(g,r)
    ))
}

.formatGeneResource <- function(g,r,w) {
    switch(w,
        ids = {
            return(NULL)
        },
        disgenet = {
            if (g %in% rownames(r$disgenet)) 
                return(unname(apply(r$disgenet[g,2][[1]],1,as.list)))
            else
                return(NULL)
        },
        cgd = {
            if (g %in% rownames(r$cgd))
                return(as.list(r$cgd[g,c(2,3)]))
            else 
                return(NULL)
        },
        hpo = {
            return(r$hpo[[g]][!sapply(r$hpo[[g]],is.null)])
        },
        ctd = {
            return(unname(r$ctd[[g]]))
        },
        dbnsfp_gene = {
            return(NULL)
        },
        omim = {
            return(NULL)
        },
        omim_morbid = {
            return(NULL)
        }
    )
}

.getTranscripts <- function(g,r) {
    g$gene_name <- NULL
    # Add also SO localization
    locInd <- match(g$impact_so,r$sol[,1])
    if (any(is.na(locInd))) {
        # Two cases
        # 1. There is a multi-term separated with "&"
        # 2. Indeed term does not exist
        # Case 1 must be firstly checked and resolved        
        nas <- which(is.na(locInd))
        for (ii in nas) {
            if (grepl("&",g$impact_so[ii])) {
                # Try to find a close match
                tmp <- strsplit(g$impact_so[ii],"&")[[1]]
                m <- match(tmp,r$sol[,1])[1]
                m <- m[!is.na(m)]
                if (length(m) > 0)
                    locInd[ii] <- m[1]
            }
        }
        # For the rest, mention unspecified
        replaceInd <- which(r$sol[,2]=="unspecified")[1]
        locInd[is.na(locInd)] <- replaceInd
    }
    g$location <- r$sol[locInd,2]
    # We may have to split as some variants (in exomes/genomes?) may have 
    # multiple impacts joined with "&"
    g$impact_so <- strsplit(g$impact_so,"&")
    return(unname(apply(g,1,function(x) {
        if (length(x$impact_so) == 1)
            x$impact_so = list(x$impact_so)
        return(as.list(x))
    })))
}

.getVariant <- function(rsid,hgvs,vr) {
    # Check rs-based constructed resources
    disgenet <- pharmgkb <- oncokb <- civic <- NA
    if (length(rsid) > 1) {
        if (any(rsid %in% rownames(vr$disgenet))) {
            rs <- rsid[which(rsid %in% rownames(vr$disgenet))]
            if (length(rs) > 1)
                rs <- rs[1]
            disgenet <- .formatVariantResource(rs,vr,"disgenet")
        }
        if (any(rsid %in% names(vr$pharmgkb))) {
            rs <- rsid[which(rsid %in% names(vr$pharmgkb))]
            if (length(rs) > 1)
                rs <- rs[1]
            pharmgkb <- .formatVariantResource(rs,vr,"pharmgkb")
        }
    }
    if (length(rsid) == 1) {
        if (rsid %in% rownames(vr$disgenet)) {
            rs <- rsid[which(rsid %in% rownames(vr$disgenet))]
            disgenet <- .formatVariantResource(rs,vr,"disgenet")
        }
        if (rsid %in% names(vr$pharmgkb)) {
            rs <- rsid[which(rsid %in% names(vr$pharmgkb))]
            pharmgkb <- .formatVariantResource(rs,vr,"pharmgkb")
        }
    }
    
    # Check HGVSg constructed resources
    if (hgvs %in% names(vr$oncokb))
        oncokb <- .formatVariantResource(hgvs,vr,"oncokb")
    if (hgvs %in% rownames(vr$civic))
        civic <- .formatVariantResource(hgvs,vr,"civic")
    
    return(list(
        disgenet=disgenet,
        disgenet_length=ifelse(is.na(disgenet),0,length(disgenet)[1]),
        pharmgkb=pharmgkb,
        oncokb=oncokb,
        civic=civic
    ))
}

.formatVariantResource <- function(v,r,w) {
    switch(w,
        disgenet = {
            if (v %in% rownames(r$disgenet)) 
                return(unname(apply(r$disgenet[v,2][[1]],1,as.list)))
            else
                return(NULL)
        },
        pharmgkb = {
            # Is a data.frame but in format properly handled by toJSON
            return(r$pharmgkb[[v]])
        },
        oncokb = {
            return(r$oncokb[[v]])
        },
        civic = {
            h <- r$civic[v,c(1,2,4),drop=FALSE]
            rownames(h) <- NULL
            return(list(
                id=h$id,
                gene_id=h$gene_id,
                variant=h$variant
            ))
        }
    )
}

.parseID <- function(n) {
    # Keep only dbSNP ids, rest are auto-generated and should be .
    toDot <- !grepl("^rs",n)
    n[toDot] <- "."
    return(strsplit(n,";"))
}

.makeAlt <- function(v) {
    a <- alt(v)
    names(a) <- 1:length(a)
    A <- as.character(unlist(a))
    S <- split(A,factor(names(A),levels=as.character(unique(names(A)))))
    return(vapply(S,function(x) paste(x,collapse=","),character(1)))
}

.parseANN <- function(a) {
    # a is a character vector or empty (no ref? in case of multiallelic split)
    if (length(a) > 0) {
        # Split the SnpEff fields
        S <- strsplit(a,"\\|")
        # Convert the annotations to matrix
        M <- t(do.call("cbind",lapply(S,function(x) {
            return(c(
                #allele=x[1],
                detailed_impact_so_term=x[2],
                summary_impact=x[3],
                gene_name=x[4],
                #ensembl_id=x[5],
                #ensembl_type=x[6],
                transcript_id=x[7],
                biotype=x[8],
                exon_rank=x[9], 
                hgvs_c=x[10], 
                hgvs_p=x[11],
                cdna_rank=x[12],
                cds_rank=x[13],
                aa_rank=x[14]
            ))
        })))
        # Remove duplicates based on allele and so term, this will retain 
        # only one row per allele most times
        if (nrow(M) > 1) {
            d <- which(duplicated(M[,c(seq_len(5))]))
            if (length(d) > 0)
                M <- M[-d,,drop=FALSE]
        }
        return(M)
    }
    else {
        return(as.matrix(data.frame(
            #allele=NA,
            detailed_impact_so_term=NA,
            summary_impact=NA,
            gene_name=NA,
            #ensembl_id=NA,
            #ensembl_type=NA,
            transcript_id=NA,
            biotype=NA,
            exon_rank=NA, 
            hgvs_c=NA, 
            hgvs_p=NA,
            cdna_rank=NA,
            cds_rank=NA,
            aa_rank=NA
        )))
    }
}

.greedyUncompressDataFrame <- function(DF) {
    L <- vector("list",ncol(DF))
    names(L) <- colnames(DF)
    for (n in colnames(DF)) {
        #message("Uncompressing ",n)
        if (is(DF[,n],"CharacterList")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- .collapseCharacterList(DF[,n])
            else
                res <- unlist(DF[,n])
        }
        else if (is(DF[,n],"IntegerList")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- .collapseCharacterList(DF[,n])
            else
                res <- unlist(DF[,n])
        }
        else if (is(DF[,n],"NumericList")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- .collapseCharacterList(DF[,n])
            else
                res <- unlist(DF[,n])
        }
        else if (is(DF[,n],"list")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- vapply(DF[,n],function(x) paste(x,collapse=","),
                    character(1))
            else
                res <- unlist(DF[,n])
        }
        else
            res <- unlist(DF[,n])
        
        L[[n]] <- res
    }
    return(as.data.frame(L))
}

.collapseCharacterList <- function(a,sep=",") {
    names(a) <- 1:length(a)
    A <- unlist(a)
    S <- split(A,factor(names(A),levels=as.character(unique(names(A)))))
    return(vapply(S,function(x) paste(x,collapse=sep),character(1)))
}

#https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=clinvar&term=rs1801158&retmode=json

.findDisgenetByVariant <- function(variants,ver="7.0") {
    con <- mongoConnect("disgenet_variant","back")
    on.exit(mongoDisconnect(con))

    query <- .toMongoJSON(list(
        variant_id=list(
            "$in"=variants
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        variant_id=1L,
        "network.id"=1L,
        "network.name"=1L
    ))
    result <- con$find(query,fields)
    if (nrow(result) > 0) {
        if (any(duplicated(result[,1]))) { # Duplicate genes may arise
            dd <- which(duplicated(result[,1]))
            remList <- list()
            counter <- 0
            for (j in dd) {
                counter <- counter + 1
                multiIndex <- which(result[,1] == result[j,1])
                networks <- do.call("rbind",result[multiIndex,"network"])
                result[multiIndex[1],"network"][[1]] <- list(networks)
                remList[[counter]] <- multiIndex[2:length(multiIndex)]
            }
            result <- result[-unlist(remList),,drop=FALSE]
        }
        rownames(result) <- result[,1]
        return(result)
    }
    return(NULL)
}

.findCosmicByVariant <- function(hgvs,gv,ver="100") {
    con <- mongoConnect("cosmicid_point","back")
    on.exit(mongoDisconnect(con))
    
    nv <- ifelse(gv=="hg19",37,38)
    query <- list(
        gver=list(
            "$in"=hgvs
        ),
        version=ver
    )
    names(query)[1] <- paste0("hgvsg_",nv)
    query <- .toMongoJSON(query)
    fields <- list(
        `_id`=0L,
        cosmic_id=1L,
        gver=1L
    )
    names(fields)[3] <- paste0("hgvsg_",nv)
    fields <- .toMongoJSON(fields)
    result <- con$find(query,fields)
    
    if (nrow(result) > 0) {
        if (any(duplicated(result))) {
            result <- result[!duplicated(result),,drop=FALSE]
            rownames(result) <- result[,2]
        }
        return(result)
    }
    return(NULL)
}

.findPharmGkbByVariant <- function(ids,ver="20240905") {
    con <- mongoConnect("pharmgkb","back")
    on.exit(mongoDisconnect(con))
    
    query <- .toMongoJSON(list(
        "variant.var_id"=list(
            "$in"=ids
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        level_of_evidence=1L,
        "variant.var_id"=1L,
        "variant.pharmgkb_id"=1L,
        "chemicals.pharmgkb_id"=1L,
        "chemicals.chemical_name"=1L,
        "phenotypes.id"=1L,
        "phenotypes.name"=1L
    ))
    result <- con$find(query,fields)
    
    if (nrow(result) > 0) {
        return(lapply(split(result,result$variant$var_id),function(x) {
            x$pharmgkb_id <- x$variant$pharmgkb_id
            x$variant <- NULL
            return(x)
        }))
    }
    return(NULL)
}

.findCivicByOneLetterAA <- function(fixed,annList,gv) {
    # Prepare map for CiVIC files
    map <- data.frame(
        hgvsg=fixed$hgvsg,
        hgvsp=sapply(annList,function(x) {
            x[1,"hgvs_p"]
        }),
        genes=sapply(annList,function(x) {
            x[1,"gene_name"]
        })
    )
    
    # Remove variants with no protein impacts
    map <- map[map$hgvsp!="",,drop=FALSE]
    # When dealing with all transcripts, we may have fully duplicated entries
    map <- map[!duplicated(map),,drop=FALSE]
    
    # Continue if map still has rows
    if (nrow(map) > 0) {
        # Convert to 1-AA and assign rownames
        map$oneaa <- .makeOneLetterHgvsp(map$hgvsp)
        #rownames(map) <- map$oneaa
        # Read CiVIC files
        cVars <- read.delim(.getStaticFile(gv,"civic_variants"))
        cGens <- read.delim(.getStaticFile(gv,"civic_genes"))
        # Match
        if (any(cVars$variant %in% map$oneaa)) {
            # We need to use our input data as reference as we have cases with 
            # the same hgvsp in same gene - this is probably caused by variant 
            # normalization and multi-allelic split
            # 1. Get variant info, id and gene name, we need to construct a
            #    composite key of oneAA-hgvsp and gene name as obviously, the
            #    same protein alteration may occur more than once in a different
            #    gene
            map_keys <- paste(map$oneaa,map$genes,sep="_")
            civ_keys <- paste(cVars$variant,cVars$feature_name,sep="_")
            varInd <- which(map_keys %in% civ_keys)
            vk <- map_keys[varInd]
            civInd = match(vk,civ_keys)
            hits <- data.frame(
                id=cVars$variant_id[civInd],
                #id=as.integer(cVars$variant_id[civInd]),
                variant=cVars$variant[civInd],
                gene=cVars$feature_name[civInd]
            )
            # 2. Add gene info
            genInd <- match(hits$gene,cGens$name)
            hits$gene_id <- cGens$gene_id[genInd]
            #hits$gene_id <- as.integer(cGens$gene_id[genInd])
            # 3. Add hgvsg rownames
            hits$hgvsg <- map[varInd,"hgvsg"]
            rownames(hits) <- hits$hgvsg

            return(hits)
        }
        else
            return(NULL)
    }
    else
        return(NULL)
}

.findDisgenetByGene <- function(genes,ver="7.0") {
    con <- mongoConnect("disgenet_gene","back")
    on.exit(mongoDisconnect(con))

    query <- .toMongoJSON(list(
        gene_name=list(
            "$in"=genes
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        gene_name=1L,
        "network.id"=1L,
        "network.name"=1L
    ))
    result <- con$find(query,fields)
    if (nrow(result) > 0) {
        if (any(duplicated(result[,1]))) { # Duplicate genes may arise
            dd <- which(duplicated(result[,1]))
            remList <- list()
            counter <- 0
            for (j in dd) {
                counter <- counter + 1
                multiIndex <- which(result[,1] == result[j,1])
                networks <- do.call("rbind",result[multiIndex,"network"])
                result[multiIndex[1],"network"][[1]] <- list(networks)
                remList[[counter]] <- multiIndex[2:length(multiIndex)]
            }
            result <- result[-unlist(remList),,drop=FALSE]
        }
        rownames(result) <- result[,1]
        return(result)
    }
    return(NULL)
}

.findHpoByGene <- function(genes,ver="20240813") {
    con <- mongoConnect("hpo","back")
    on.exit(mongoDisconnect(con))

    query <- .toMongoJSON(list(
        "gene_relation.gene_symbol"=list(
            "$in"=genes
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        hpo_id=1L,
        hpo_name=1L,
        "gene_relation"=1L
    ))
    result <- con$find(query,fields)
    if (nrow(result) > 0) {
        genesLookup <- as.list(rep(0,length(genes)))
        genesResult <- vector("list",length(genes))
        names(genesLookup) <- names(genesResult) <- genes
        
        for (i in seq_len(nrow(result))) {
            for (gs in result[i,"gene_relation"][[1]][,"gene_symbol"]) {
                if (!is.null(genesLookup[[gs]])) {
                    genesLookup[[gs]] <- genesLookup[[gs]] + 1
                    genesResult[[gs]][[genesLookup[[gs]] + 1]] <- list(
                        id=result[i,"hpo_id"],
                        name=result[i,"hpo_name"]
                    )
                }
            }
        }
        # Remove NULLs
        genesResult <- genesResult[which(!sapply(genesResult,is.null))]
        return(genesResult)
    }
    return(NULL)
}

.findCgdInheritanceByGene <- function(genes,ver="20240701") {
    con <- mongoConnect("cgd","back")
    on.exit(mongoDisconnect(con))

    query <- .toMongoJSON(list(
        gene=list(
            "$in"=genes
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        gene=1L,
        inheritance=1L,
        condition=1L
    ))
    result <- con$find(query,fields)
    
    if (nrow(result) > 0) {
        rownames(result) <- result[,1]
        return(result)
    }
    return(NULL)
}

.findCtdByGene <- function(genes,ver="20240927") {
    con <- mongoConnect("ctd_gene","back")
    on.exit(mongoDisconnect(con))

    query <- .toMongoJSON(list(
        gene_symbol=list(
            "$in"=genes
        ),
        version=ver
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        gene_symbol=1L,
        chemical_name=1L,
        "chemical_info.chemical_id"=1L
    ))
    result <- con$find(query,fields)
    
    if (nrow(result) > 0) {
        tmp <- split(result,result$gene_symbol)
        return(lapply(tmp,function(x) {
            x[,3] <- x[,3]$chemical_id
            names(x)[c(2,3)] <- c("name","id")
            return(apply(x[,2:3],1,as.list))
        }))
    }
    return(NULL)
}

.makeHgvsg <- function(chr,start,ref,alt) {
    # Ensure inputs are of equal length
    if (!(length(chr) == length(start) 
        && length(start) == length(ref) 
        && length(ref) == length(alt))) {
        log_error("All input vectors must have the same length.")
        stop("All input vectors must have the same length.")
    }
  
    # Determine lengths of ref and alt alleles
    ref_len <- nchar(ref)
    alt_len <- nchar(alt)
  
    # Initialize HGVS vector
    hgvs <- character(length(chr))
  
    # SNP: Single Nucleotide Polymorphism
    snp_idx <- ref_len == 1 & alt_len == 1
    hgvs[snp_idx] <- paste0(
        chr[snp_idx],":g.",start[snp_idx], 
        ref[snp_idx],">",alt[snp_idx]
    )
  
    # Pure Insertion: Empty ref, non-empty alt
    pure_insertion_idx <- ref_len == 0 & alt_len > 0
    hgvs[pure_insertion_idx] <- paste0(
        chr[pure_insertion_idx],":g.",start[pure_insertion_idx] - 1,"_",
        start[pure_insertion_idx],"ins",alt[pure_insertion_idx]
    )
  
    # Pure Deletion: Non-empty ref, empty alt
    pure_deletion_idx <- ref_len > 0 & alt_len == 0
    pure_deletion_end <- start[pure_deletion_idx] + 
        ref_len[pure_deletion_idx] - 1
    hgvs[pure_deletion_idx] <- paste0(
        chr[pure_deletion_idx],":g.",start[pure_deletion_idx], "_", 
        pure_deletion_end,"del"#,ref[pure_deletion_idx]
    )
  
    # Insertion: Alternative allele is longer, starts with ref
    insertion_idx <- alt_len > ref_len & substr(alt,1,ref_len) == ref
    hgvs[insertion_idx] <- paste0(
        chr[insertion_idx],":g.",start[insertion_idx],"_",
        start[insertion_idx] + ref_len[insertion_idx] - 1,"ins", 
        substr(alt[insertion_idx],ref_len[insertion_idx] + 1,
        alt_len[insertion_idx])
    )
  
    # Deletion: Reference allele is longer, starts with alt
    deletion_idx <- ref_len > alt_len & substr(ref, 1, alt_len) == alt
    deletion_end <- start[deletion_idx] + ref_len[deletion_idx] - 1
    hgvs[deletion_idx] <- paste0(
        chr[deletion_idx],":g.",start[deletion_idx],"_", 
        deletion_end,"del"#, 
        #substr(ref[deletion_idx], alt_len[deletion_idx] + 1, ref_len[deletion_idx])
    )
  
    # MNV: Reference and alt are of equal length (>1), and differ
    mnv_idx <- ref_len > 1 & alt_len > 1 & ref_len == alt_len
    mnv_end <- start[mnv_idx] + ref_len[mnv_idx] - 1
    hgvs[mnv_idx] <- paste0(
        chr[mnv_idx],":g.",start[mnv_idx],"_", 
        mnv_end, ref[mnv_idx],">",alt[mnv_idx]
    )
  
    # Delins: Complex substitution (not covered by the above)
    delins_idx <- !(snp_idx | pure_insertion_idx | pure_deletion_idx |  
        insertion_idx | deletion_idx | mnv_idx)
    delins_end <- start[delins_idx] + ref_len[delins_idx] - 1
    hgvs[delins_idx] <- paste0(
        chr[delins_idx],":g.",start[delins_idx], "_", 
        delins_end,"delins",alt[delins_idx]
    )
  
    return(hgvs)
}

.makeOneLetterHgvsp <- function(hgvs) {
    aaMap <- .AAmap()
    #aaRegex <- paste0("\\b(",paste(names(aaMap),collapse="|"),")\\b")
    hgvs <- sub("^p\\.","",hgvs)
    for (aa in names(aaMap))
        hgvs <- gsub(aa,aaMap[aa],hgvs)
    return(toupper(hgvs))
}

.makeSearchText <- function(doc) {
    # Extract rsids - paste is robust agains length 0 arrays and NULLs
    rsids <- paste(doc$identity$rsid,collapse=" ")
  
    # Construct chr:start
    chrpos <- ""
    if (!is.null(doc$identity$chr) && !is.null(doc$identity$start))
        chrpos <- paste0(doc$identity$chr,":",doc$identity$start)
  
    # Collect gene names
    geneNames <- paste(sapply(doc$annotation$genes,function(x) {
        return(x$name)
    }),collapse=" ")
    
    # Collect transcript information
    txIds <- paste(lapply(doc$annotation$genes,function(x) {
        paste(sapply(x$transcripts,function(y) {
            return(y$transcript_id)
        }),collapse=" ")
    }),collapse=" ")
    
    impactSos <- paste(lapply(doc$annotation$genes,function(x) {
        paste(unique(unlist(lapply(x$transcripts,function(y) {
            return(y$impact_so)
        }))),collapse=" ")
    }),collapse=" ")
    
    hgvs <- paste(lapply(doc$annotation$genes,function(x) {
        paste(sapply(x$transcripts,function(y) {
            return(c(y$hgvs_c,y$hgvs_p))
        }),collapse=" ")
    }),collapse=" ")
    
    # Final concatenated text
    searchText <- paste(c(rsids,chrpos,geneNames,txIds,impactSos,hgvs),
        collapse=" ")
    
    return(trimws(searchText))
}

#.makeOneLetterHgvsp <- function(hgvs) {
#    # Define a mapping of 3-letter amino acid codes to 1-letter codes
#    aamap <- .AAmap()
#  
#    # Helper function to convert amino acid codes from 3-letter to 1-letter
#    ..convertAA <- function(aa) {
#        if (aa %in% names(aamap))
#            return(aamap[aa])
#        else
#            return(aa) # Return unmodified if not an amino acid
#    }
#  
#    # Helper function to split a sequence into 3-letter chunks and convert to 
#    # 1-letter
#    ..convertSeq <- function(sequence) {
#        # Split the input sequence into chunks of 3 characters
#        split_codes <- substring(sequence,seq(1,nchar(sequence),by=3),
#            seq(3,nchar(sequence)+2,by=3))
#        # Convert each 3-letter code to its 1-letter equivalent
#        converted <- sapply(split_codes, ..convertAA)
#        return(paste0(converted,collapse=""))
#    }
#  
#    # Function to process each HGVS string
#    ..convertSingle <- function(hgvs_string) {
#        # Match patterns and apply transformations
#        if (grepl("^p\\.([A-Za-z]{3})([0-9]+)([A-Za-z]{3})$",hgvs_string)) {
#            parts <- regmatches(hgvs_string,
#                regexec("^p\\.([A-Za-z]{3})([0-9]+)([A-Za-z]{3})$",
#                hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],
#                ..convertAA(parts[4])))
#        }
#    
#        if (grepl("^p\\.([A-Za-z]{3})([0-9]+)fs$",hgvs_string)) {
#            parts <- regmatches(hgvs_string,
#                regexec("^p\\.([A-Za-z]{3})([0-9]+)fs$",hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],"fs"))
#        }
#    
#        if (grepl("^p\\.([A-Za-z]{3})([0-9]+)ins([A-Za-z]+)$",hgvs_string)) {
#            parts <- regmatches(hgvs_string, regexec(
#                "^p\\.([A-Za-z]{3})([0-9]+)ins([A-Za-z]+)$",hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],"ins",
#                ..convertSeq(parts[4])))
#        }
#    
#        if (grepl("^p\\.([A-Za-z]{3})([0-9]+)del$",hgvs_string)) {
#            parts <- regmatches(hgvs_string,
#                regexec("^p\\.([A-Za-z]{3})([0-9]+)del$",hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],"del"))
#        }
#    
#        if (grepl("^p\\.([A-Za-z]{3})([0-9]+)_([A-Za-z]{3})([0-9]+)dup$",
#            hgvs_string)) {
#            parts <- regmatches(hgvs_string,
#                regexec("^p\\.([A-Za-z]{3})([0-9]+)_([A-Za-z]{3})([0-9]+)dup$", 
#                hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],"_",
#                ..convertAA(parts[4]),parts[5],"dup"))
#        }
#        
#        if (grepl(paste0("^p\\.([A-Za-z]{3})([0-9]+)_([A-Za-z]{3})([0-9]+)",
#            "delins([A-Za-z]+)$"),hgvs_string)) {
#            parts <- regmatches(hgvs_string,regexec(paste0(     
#                "^p\\.([A-Za-z]{3})([0-9]+)_([A-Za-z]{3})([0-9]+)",
#                "delins([A-Za-z]+)$"),hgvs_string))[[1]]
#            return(paste0(..convertAA(parts[2]),parts[3],"_",
#                ..convertAA(parts[4]),parts[5],"delins",..convertSeq(parts[6])))
#        }
#    
#        # If no match, return the input unchanged
#        return(hgvs_string)
#    }
#  
#    # Apply the conversion to each HGVS string
#    return(sapply(hgvs,..convertSingle,USE.NAMES=FALSE))
#}


#~ .getCivicInfo <- function(map) {
#~     # The usage of CiVIC v2 API (GraphQL) is difficult... It is easier to
#~     # donwload monthly (like with ClinVar) files from CiVIC (genes, variants)
#~     # and provide a link for review if necessary.
#~     # 
#~     # The idea is:
#~     # 1. Create a map data.frame with:
#~     #    - hgvs.p codes extracted from SnpEff annotation
#~     #    - hgvs.g codes to be used for a resource data frame creation to be
#~     #      used for herein internal annotation routines
#~     #    - Gene names
#~     # 2. Deduplicate this data frame so that we can safely assign hgvs.p codes
#~     #    as row names.
#~     # 3. Convert the hgvs.p codes to one-letter AA representation with the help
#~     #    of ChatGPT function (may need further review).
#~     # 4. Check if the one-letter AA exists in CiVIC variants files
#~     # 5. If yes:
#~     #    - Grab the link and add to the list of hits
#~     #    - Grab the gene name and get its link from the genes file and add to 
#~     #      the list of hits
#~     # 6. Return the hits list
#~     #
#~     # CiVIC files should live with the rest of annotation files.
#~ }

.queryOncoKB <- function(hgvs,gv) {
    if (!requireNamespace("httr")) {
        log_error("R package httr is required!")
        stop("R package httr is required!")
    }
    
    # Reference genome
    refg <- ifelse(gv=="hg19","GRCh37","GRCh38")
    
    # OncoKB endpoint URL - hgvsg search
    url <- "https://www.oncokb.org/api/v1/annotate/mutations/byHGVSg"
    
    # Construct query - we have no idea about API limitations...
    payload <- toJSON(data.frame(hgvsg=hgvs,referenceGenome=refg))
    headers <- httr::add_headers(
        `accept`="application/json",
        `Content-Type`="application/json",
        `Authorization`=paste0("Bearer ",.getOncoKbToken())
    )
    
    # POST
    response <- httr::POST(url,body=payload,headers)
    
    # Check if we have response
    if (status_code(response) == 200) {
        # Parse and print the response
        # From the response we need limited fields (for now) that are adequate
        # to display basic info and construct links to OncoKB
        # We need
        # - query$hugoSymbol
        # - query$alteration
        # - oncogenic
        # - mutationEffect$knownEffect
        # - treatments[[x]]$drugs$ncitCode
        # - treatments[[x]]$drugs$drugName
        responseData <- httr::content(response,as="parsed",
            type="application/json")
        shrinked <- lapply(responseData,function(x) {
            list(
                gene_symbol=x$query$hugoSymbol,
                alteration=x$query$alteration,
                oncogenic=x$oncogenic,
                effect=x$mutationEffect$knownEffect,
                treatments=if (length(x$treatments) == 0) NULL else
                    lapply(x$treatments,function(y) {
                        return(lapply(y$drugs,function(z) {
                            return(list(
                                code=z$ncitCode,
                                drug=z$drugName
                            ))
                        }))
                    })
            )
        })
        # Bring treatments up one-level, they are like this because drugs and
        # codes are further burried in treatments$.$drugs
        shrinked <- lapply(shrinked,function(x) {
            if (is.null(x$treatments))
                return(x)
            x$treatments <- lapply(x$treatments,function(y) {
                return(list(
                    code=y[[1]]$code,
                    drug=y[[1]]$drug
                ))
            })
            return(x)
        })
        return(shrinked)
    } 
    else {
        log_error("OncoKB query failed! Status: ",status_code(response))
        # We should not stop the process - rather return an empty list/NULL
        #stop("OncoKB query failed! Status: ",status_code(response))
        return(NULL)
    }
}

# Returns an index to retain
.filterSoTermsForApiCalls <- function(terms) {
    excluded <- c(
        "synonymous_variant",
        "function_uncertain_variant",
        "intergenic_variant",
        "3_prime_UTR_variant",
        "intron_variant",
        "5_prime_UTR_variant",
        "conserved_intron_variant",
        "conserved_intergenic_variant",
        "intragenic_variant",
        "stop_retained_variant",
        "conservative_inframe_insertion",
        "inframe_insertion",
        "conservative_inframe_deletion"
    )
    exprs <- paste(paste("\\b",excluded,"\\b",sep=""),collapse="|")
    return(which(!grepl(exprs,terms)))
}

.AAmap <- function() {
    return(c(
        Ala="A",Arg="R",Asn="N",Asp="D",Cys="C",Gln="Q",Glu="E",Gly="G",His="H",
        Ile="I",Leu="L",Lys="K",Met="M",Phe="F",Pro="P",Ser="S",Thr="T",Trp="W",
        Tyr="Y",Val="V",Sec="U",Pyl="O",Ter="*",Xaa="X",Ter="*"
    ))
}

.getGenomeSize <- function(afile,sid) {
    # Read in VCF header to get genome size
    vcfCon <- VcfFile(afile,yieldSize=1)
    open(vcfCon)            
    vcftmp <- suppressWarnings(readVcf(vcfCon))
    close(vcfCon)
    
    # seqinfo may not be what we want... Get data directly from contigs
    contigs <- meta(header(vcftmp))$contig
    if (!is.null(contigs)) {
        # Derive genome size
        contigs$seq=rownames(contigs)
        # Number of columns not stable, but these two exist
        contigs <- contigs[,c("length","seq"),drop=FALSE]
        genomeSize <- apply(contigs,1,function(x) {
            return(list(
                seq=x[2],
                len=as.integer(x[1])
            ))
        })
        
        # Update sample in database
        .updateSampleGenomeSize(sid,genomeSize)
    }
}

.analysis_logger <- function(analysisPath) {
    # Analysis logger in database
    logger_db_analysis <- 
        suppressWarnings(layout_json(c("time","level","fn","user","msg")))
    log_layout(logger_db_analysis,index=1)
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
    # More extensive analysis logger in file
    logger_debug_analysis <- layout_glue_generator(
        format='{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] | {fn} | {msg}')
    log_layout(logger_debug_analysis,index=2)
    log_threshold(DEBUG,index=2)
    logfile <- file.path(analysisPath,"analysis.log")
    log_appender(appender_file(file=logfile),index=2)
    #log_appender(appender_tee(file=logfile),index=2)
    ## Delete the loggers upon analysis finish - whatever happens
    #on.exit({
    #    delete_logger_index(index=2)
    #    delete_logger_index(index=1)
    #},add=TRUE)
}

readPolyGT <- function(file,param=ScanVcfParam(),row.names=TRUE) {
    lst <- .readLiteGT(file,param,row.names=row.names)
    res <- .geno2genoPoly(lst)
    rowRanges <- lst$rowRanges
    if (row.names)
        dimnames(res)[[1]] <- names(rowRanges)
    return(res)
}

.readLiteGT <- function(file,param,row.names) {
    if (is(param,"ScanVcfParam")) {
        which <- vcfWhich(param)
        samples <- vcfSamples(param)
    } else {
        which <- param 
        samples <- character()
    }
    param <- ScanVcfParam("ALT",NA,"GT",samples,which=which)
    scn <- scanVcf(file,param=param,row.names=row.names)
    return(VariantAnnotation:::.collapseLists(scn,param))
}

.geno2genoPoly <- function(lst,ALT=NULL,REF=NULL,GT=NULL) {
    if (is.null(ALT) && is.null(REF) && is.null(GT)) {
        # Safe as multiallelics come normalized
        ALT <- as.character(lst$ALT,use.names = FALSE)
        REF <- as.character(lst$REF,use.names = FALSE)
        GT  <- lst$GENO$GT
    }

    res <- GT

    ## normalize missing
    missing <- is.na(GT) | GT %in% c(".", "./.", ".|.")
    GT[missing] <- NA_character_

    ## split GT into allele indices (any ploidy)
    GTsplit <- strsplit(as.vector(GT), "[/|]")

    ## allele lookup table (biallelic guaranteed)
    alleles <- cbind(REF,ALT)
    alleles <- as.vector(t(alleles)) # REF, ALT, REF, ALT, ...

    ## cumulative offsets per variant
    REFcs <- cumsum(elementNROWS(REF))
    ALTcs <- cumsum(elementNROWS(ALT))
    cs <- REFcs + c(0,head(ALTcs,-1))

    ## iterate safely over genotypes
    for (i in seq_along(GTsplit)) {
        if (missing[i])
            next
        idx <- suppressWarnings(as.integer(GTsplit[[i]]))
        if (any(is.na(idx)))
            next

        ## map allele indices → bases
        offset <- cs[i]
        bases <- alleles[offset + idx]

        ## summarize dosage
        tab <- table(bases)
        if (length(bases) == 2 && length(tab) == 2) {
            res[i] <- paste(bases,collapse="/")
            next
        }
        res[i] <- paste(paste0(names(tab),"(",as.integer(tab),")"),
            collapse="/")
    }

    res[missing] <- NA_character_
    return(res)
}

#~ ..collapseLists <- function(vcf,param) {
#~     idx <- sapply(vcf,function(elt) length(elt$rowRanges) > 0L)
#~     if (!sum(idx))
#~         return(vcf[[1]])
#~     if (length(vcf) > 1L)
#~         vcf <- vcf[idx]
#~     if (is(param,"ScanVcfParam"))
#~         paramRangeID <- names(unlist(vcfWhich(param), use.names=FALSE))[idx]
#~     else
#~         paramRangeID <- names(param)[idx]
#~     if (is.null(paramRangeID))
#~         paramRangeID <- rep(NA_character_, length(vcf))

#~     ## single range in 'which'
#~     if (1L == length(vcf)) {
#~         lst <- vcf[[1]]
#~         lst$paramRangeID <- as.factor(rep(paramRangeID, length(lst$rowRanges)))
#~     } 
#~     else {
#~      ## multiple ranges in 'which'
#~      lst <- lapply(names(vcf[[1]]), function(elt) {
#~          suppressWarnings(do.call(c, unname(lapply(vcf, "[[", elt))))
#~      })
#~         names(lst) <- names(vcf[[1]])
#~         len <- unlist(lapply(vcf,function(elt) length(elt$rowRanges)),
#~          use.names=FALSE)
#~         paramRangeID <- as.factor(rep(paramRangeID,len))

#~         ## collapse info and geno
#~         info <- lst$INFO
#~         sp <- split(unname(info), unique(names(info)))
#~         sp <- sp[unique(names(info))]
#~         lst$INFO <- lapply(sp,function(elt) {
#~          d <- dim(elt[[1]])
#~          if (is(elt[[1]],"list"))
#~              as.matrix(elt)
#~          else if (is(elt[[1]],"array") && !is.na(d[3])) {
#~              pc <- lapply(seq_len(d[2]),function(i) {
#~                  do.call(rbind,lapply(elt,"[", ,i,))
#~              })
#~              array(do.call(c,pc),c(length(lst$rowRanges),d[2],d[3]))
#~          }
#~          else
#~              do.call(c,elt)
#~      })
#~         geno <- lst$GENO
#~         sp <- split(geno, unique(names(geno)))
#~         lst$GENO <- lapply(sp,function(elt) {
#~          d <- dim(elt[[1]])
#~          if (!is.na(d[3])) {
#~              pc <- lapply(seq_len(d[3]), function(i) {
#~                  do.call(rbind, lapply(elt, "[", ,,i))
#~              })
#~              cmb <- array(do.call(c,pc),c(length(lst$rowRanges),d[2],d[3]))
#~              cmb
#~          } 
#~          else {
#~              trans <- lapply(elt, t)
#~              cmb <- matrix(do.call(c,trans),length(lst$rowRanges),d[2],
#~                  byrow=TRUE)
#~              cmb
#~          }
#~      })
#~         lst$paramRangeID <- paramRangeID
#~     }
#~     lst
#~ }

#~ .geno2genoPoly <- function(lst, ALT=NULL, REF=NULL, GT=NULL)
#~ {
#~     if (is.null(ALT) && is.null(REF) && is.null(GT)) {
#~         ALT <- lst$ALT
#~         REF <- as.character(lst$REF, use.names = FALSE)
#~         GT  <- lst$GENO$GT
#~     }

#~     res <- GT

#~     ## normalize missing
#~     missing <- is.na(GT) | GT %in% c(".", "./.", ".|.")
#~     GT[missing] <- NA_character_

#~     ## split genotypes (variable ploidy)
#~     GTsplit <- strsplit(GT, "[/|]")

#~     ## allele lookup (biallelic guaranteed)
#~     alleles <- as.vector(rbind(REF, ALT))

#~     ## offsets per record
#~     REFcs <- cumsum(elementNROWS(REF))
#~     ALTcs <- cumsum(elementNROWS(ALT))
#~     cs <- REFcs + c(0, head(ALTcs, -1))

#~     ## per-genotype formatter
#~     fmt_one <- function(idx, offset) {
#~         if (is.null(idx))
#~             return(NA_character_)

#~         idx <- suppressWarnings(as.integer(idx))
#~         if (anyNA(idx))
#~             return(NA_character_)

#~         bases <- alleles[offset + idx + 1]
#~         tab <- table(bases)

#~         ## classic diploid display
#~         if (length(bases) == 2L && length(tab) == 2L) {
#~             return(paste(bases, collapse = "/"))
#~         }

#~         paste0(
#~             paste0(names(tab), "\u00D7", as.integer(tab)),
#~             collapse = " / "
#~         )
#~     }

#~     ## apply in C-level loop
#~     res[!missing] <- vapply(
#~         which(!missing),
#~         function(i) fmt_one(GTsplit[[i]], cs[i]),
#~         FUN.VALUE = character(1)
#~     )

#~     res
#~ }

#~ library(VariantAnnotation)

#~ #/media/data/resources/edimo/workspace/users/68f8a60ff9c84538290b48cd/analyses/6964e5ed44d1ff504426529e

#~ vcfFile <- "6964e56444d1ff504426529d.vcf"

#~ vcf <- suppressWarnings(readVcf(vcfFile))



