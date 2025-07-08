#~ groups <- .mongoVariantFieldGroupNames()
#~ fields <- .mongoVariantFields()
#~ aid <- "683427d556bbcd63da495964"
#~ vid <- c(
#~    "68342a7742840e0d74098bf1",
#~    "68342a7742840e0d74098bfb",
#~    "68342a7842840e0d74098cd9",
#~    "68342a7842840e0d74098ce0",
#~    "68342a7842840e0d74098d0a",
#~    "68342a7842840e0d74098d61"
#~ )
#~ que <- '{
#~   "$and": [
#~     {
#~       "$and": [
#~         {
#~           "identity.type": {
#~             "$in": [
#~               "INS",
#~               "SNP",
#~               "DEL"
#~             ]
#~           }
#~         },
#~         {
#~           "annotation.genes.transcripts.location": {
#~             "$in": [
#~               "unspecified",
#~               "upstream",
#~               "3\'utr",
#~               "exonic",
#~               "5\'utr",
#~               "downstream"
#~             ]
#~           }
#~         }
#~       ]
#~     },
#~     {
#~       "$and": [
#~         {
#~           "genotypes.af": {
#~             "$gte": 0.05
#~           }
#~         },
#~         {
#~           "genotypes.af": {
#~             "$lte": 1
#~           }
#~         },
#~         {
#~           "metrics.dp": {
#~             "$gte": 428
#~           }
#~         },
#~         {
#~           "metrics.dp": {
#~             "$lte": 20368
#~           }
#~         }
#~       ]
#~     },
#~     {
#~       "$and": [
#~         {
#~           "annotation.genes.transcripts.impact_so": {
#~             "$in": [
#~               "stop_gained",
#~               "non_coding_transcript_exon_variant",
#~               "splice_region_variant",
#~               "downstream_gene_variant",
#~               "splice_acceptor_variant",
#~               "5_prime_UTR_premature_start_codon_gain_variant",
#~               "frameshift_variant",
#~               "disruptive_inframe_deletion",
#~               "3_prime_UTR_variant",
#~               "upstream_gene_variant",
#~               "splice_donor_variant",
#~               "5_prime_UTR_variant",
#~               "missense_variant"
#~             ]
#~           }
#~         }
#~       ]
#~     }
#~   ],
#~   "analysis_id": {
#~     "$oid": "683427d556bbcd63da495964"
#~   }
#~ }'
#
# out <- exportVariants(aid=aid,canonical=FALSE)

# Either aid or vid or que can be provided, not all, vid can be a vector
# Either groups or fields can be provided, not both
# outFile NULL for a temporary file, this value is returned
# canonical, if TRUE export only the canonical transcript
exportVariants <- function(aid=NULL,vid=NULL,que=NULL,
    groups=.mongoVariantFieldGroupNames(),fields=NULL,outFormat=c("tsv","vcf"),
    outFile=NULL,canonical=FALSE) {
    # Check input arguments
    outFormat <- outFormat[1]
    .checkTextArgs("Output format",outFormat,c("tsv","vcf"),multiarg=FALSE)
    
    # Only one of aid, vid or que can be provided
    checkMain <- c(!is.null(aid),!is.null(vid),!is.null(que))
    if (length(which(checkMain)) > 1) {
        msg <- paste0("Only one of analysis id (aid), variant ids vector ",
            "(vid) or MongoDB query (que) can be provided!")
        log_error(msg)
        stop(msg)
    }
    
    # Only one of groups or fields can be provided
    if (!is.null(groups) && !is.null(fields)) {
        msg <- paste0("Only one of export groups (groups) or export fields ",
            "(fields) can be provided!")
        log_error(msg)
        stop(msg)
    }
    
    # If analysis id given, is it a singleton?
    if (!is.null(aid) && length(aid) > 1) {
        msg <- "Multiple analysis ids provided! Only the first will be used..."
        log_warn(msg)
        warning(msg,immediate.=TRUE)
        aid <- aid[1]
    }
    
    # If analysis id given, is it a proper MongoDB object id?
    if (!is.null(aid) && !.isObjectId(aid)) {
        msg <- paste0("The provided analysis id (aid) is not an Object Id!")
        log_error(msg)
        stop(msg)
    }
    
    # If vid(s) given, are they all object ids?
    if (!is.null(vid) && !all(sapply(vid,.isObjectId))) {
        # TODO: which ones
        msg <- paste0("Some of the provided variant ids are not Object Ids!")
        log_error(msg)
        stop(msg)
    }
    
    # Are the provided groups valid?
    if (!is.null(groups)) {
        groups <- tolower(unique(groups))
        .checkTextArgs("Requested group fields",groups,
            .mongoVariantFieldGroupNames(),multiarg=TRUE)
        # If pass, for proper downstream aggregation we need to enforce the
        # genes group
        if (!("gene" %in% groups)) {
            msg <- paste0("The 'gene' group is necessary for constructing ",
                "proper output! Adding it...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            groups <- c(groups,"gene")
        }
        # Are the necessary groups present if canonical requested?
        if (canonical && !(all(c("location","variant") %in% groups))) {
            msg <- paste0("The 'location' and 'variant' groups are necessary ",
                "for canonical transcripts output! Adding them...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            if (!("location" %in% groups))
                groups <- c("location",groups)
            if (!("variant" %in% groups))
                groups <- c("variant",groups)
        }
        # Are identity and necessary groups for proper VCF export present?
        if (outFormat == "vcf" && !all(.minVcfExportGroups() %in% groups)) {
            msg <- paste0("Some groups necessary for constructing proper VCF ",
                "output are missing! Adding them...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            theGroups <- .minVcfExportGroups()
            for (gi in theGroups) {
                if (!(gi %in% groups))
                    groups <- c(gi,groups)
            }
        }
    }
    
    # Are the provided fields valid?
    if (!is.null(fields)) {
        fields <- tolower(unique(fields))
        .checkTextArgs("Requested fields",fields,.mongoVariantFields(),
            multiarg=TRUE)
        # If pass, for proper downstream aggregation we need to enforce at least
        # one annotation.genes field and one annotation.genes.transcripts field
        if (!any(.geneNestFields() %in% fields)) {
            msg <- paste0("At least one 'annotation_genes_*' field is ",
                "necessary for constructing proper output! Adding it...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            fields <- c(fields,"annotation_genes_name")
        }
        if (!any(.transcriptNestFields() %in% fields)) {
            msg <- paste0("At least one 'annotation_genes_transcripts_*' field",
                " is necessary for constructing proper output! Adding it...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            fields <- c(fields,"annotation_genes_transcripts_transcript_id")
        }
        
        # Are the necessary fields provided for canonical transcript export?
        if (canonical && !(all(.canonicalSplitFields() %in% fields))) {
            msg <- paste0("The 'identity_chr', 'identity_start', ",
                "'identity_ref' and 'identity_alt' groups are necessary for ",
                "canonical transcripts output! Adding them...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            theFields <- rev(.canonicalSplitFields())
            for (fi in theFields) {
                if (!(fi %in% fields))
                    fields <- c(fi,fields)
            }
        }
        
        # Are all necessary fields for proper VCF export present?
        if (outFormat == "vcf" && !all(.minVcfExportFields() %in% fields)) {
            msg <- paste0("Some fields necessary for constructing proper VCF ",
                "output are missing! Adding them...")
            log_warn(msg)
            warning(msg,immediate.=TRUE)
            theFields <- .minVcfExportFields()
            for (fi in theFields) {
                if (!(fi %in% fields))
                    fields <- c(fi,fields)
            }
        }
    }
    
    # OK, checks done... Proceed...
    if (outFormat == "tsv")
        return(.variantsToTxt(aid,vid,que,groups,fields,outFile,canonical))
    else if (outFormat == "vcf")
        return(.variantsToVcf(aid,vid,que,groups,fields,outFile,canonical))
}

.variantsToTxt <- function(aid,vid,que,groups,fields,outFile,canonical) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    # All checks have been done upstream, we start pipeline building by 
    # determining analysis id, variant ids or query (TODO)
    # If yes, the really hard part... The general idea is that we replicate
    # variants for genes and transcripts only, but other fields e.g. disgenet,
    # oncokb treatments etc. are heavily collapsed. So if genes are not included
    # in the output (not likely), export would be simpler. Regarding transcripts
    # the canonical will always be included from the frontend so we cannot have
    # annotation.genes.name but not annotation.genes.transcripts.id.
    #
    # Consider:
    # 1) Enforcing genes and transcripts (at least one element from each, e.g.
    #    annotation.genes.name and annotation.genes.transcripts.id) to be in
    #    the output fields (upstream).
    # 2) Using aggregation instead of simple query to $unwind genes and
    #    transcripts.
    # 3) Using the collapsed field names we introduce here to name $project
    #    within the aggregation and return a clean data frame
    # Pipeline construction will be a pain and we will definitely need different
    # approach for VCF...
    
    # We start by defining the field names as projections, either from groups 
    # or fields. Construct the first projection.
    thePipeline <- .prepareExportPipeline(aid,vid,que,groups,fields)
    
    # Run the aggregation
    result <- con$aggregate(.toMongoJSON(thePipeline))
    
    # Canonical transcript?
    if (canonical && nrow(result) > 0) {
        f <- paste(result$identity_chr,result$identity_start,
            result$identity_ref,result$identity_alt,sep="_")
        splitted <- split(result,f)
        tmp <- lapply(splitted,function(x) {
            return(x[1,,drop=FALSE])
        })
        finalResult <- do.call("rbind",tmp)
    }
    else
        finalResult <- result
    
    # Define output file
    if (is.null(outFile))
        outFile <- tempfile()
    
    # Do we have results? If not, write an almost empty file
    if (nrow(result) == 0) {
        writeLines("No variants found.",outFile)
        return(outFile)
    }
    else {
        outFile <- paste0(outFile,".gz")
        # Exclude id
        write.table(finalResult[,-1,drop=FALSE],file=gzfile(outFile),sep="\t",
            quote=FALSE,row.names=FALSE)
    }
    
    return(outFile)
}

# aid <- "683427d556bbcd63da495964"
# uid <- "672e2004c7b0e6a1170178c5"
# vcf <- readVcf("/media/raid/resources/edimo/workspace/users/672e2004c7b0e6a1170178c5/analyses/683427d556bbcd63da495964/annotated.vcf.gz")
.variantsToVcf <- function(aid,vid,que,groups,fields,outFile,canonical) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    # Similar with TSV up to agregation pipeline preparation
    thePipeline <- .prepareExportPipeline(aid,vid,que,groups,fields)
    
    # Run the aggregation
    result <- con$aggregate(.toMongoJSON(thePipeline))
    # Get also canonical for unique variants and in case of requirement
    f <- paste(result$identity_chr,result$identity_start,result$identity_ref,
        result$identity_alt,sep="_")
    splitted <- split(result,f)
    tmp <- lapply(splitted,function(x) {
        return(x[1,,drop=FALSE])
    })
    canoResult <- do.call("rbind",tmp)
    
    # Final names from projection as we have several merged fields
    exportNames <- names(thePipeline[[4]]$`$project`)
    # Remove identity end, it will cause problems...
    if ("identity_end" %in% exportNames)
        exportNames <- exportNames[-which(exportNames=="identity_end")]
    
    # Construct the output (TODO: add some logs)
    
    # 1. VCF header object
    
    # 1.1. metadata and sample name in header
    if (is.null(aid)) {
        if (is.null(vid)) # que cannot be NULL from upstream
            aid <- que$analysis_id$`$oid`
        else if (is.null(que)) # vid provided, more comples...
            aid <- .analysisIdFromVariantId(vid[1])
    }
    vcfMeta <- .makeVcfMeta(aid)
    sn <- vcfMeta[["_sample"]][1,1]
    vcfMeta[["_sample"]] <- NULL
    
    # 1.2. Separate indices for various VCF components
    # Coordinates - all of them are guaranteed to exist from initial checks
    idNames <- .identityVcfFields()
    idInd <- match(idNames,exportNames)
    
    # Genotype data SHOULD be there because of iniital checks
    genoInd <- grep("genotypes_",exportNames)
    genoNames <- exportNames[genoInd]
    # genotypes_gn MUST be first AND included
    if (genoNames[1] != "genotypes_gn") {
        jj <- which(genoNames=="genotypes_gn")
        if (length(jj) > 0)
            genoNames <- genoNames[-jj]
        genoNames <- c("genotypes_gn",genoNames)
    }
    # Remove special gene and variant fields to be collapsed from infoNames,
    # they will be replaced
    funcGeneInd <- match(.funcGeneFields(),exportNames)
    funcVarInd <- match(.funcVarFields(),exportNames)
    funcGeneInd <- funcGeneInd[!is.na(funcGeneInd)]
    funcVarInd <- funcVarInd[!is.na(funcVarInd)]
    
    # All the rest should be info names
    infoNames <- exportNames[-c(idInd,genoInd,funcGeneInd,funcVarInd)]
    
    # 1.3. INFO (info) in header
    infoHeaders <- do.call("rbind",.availableMongoVcfHeaders()[infoNames])
    
    # 1.4. FORMAT (geno) in header
    genoHeaders <- do.call("rbind",.availableMongoVcfHeaders()[genoNames])
    
    # 1.5. Construct the special INFO headers and merge with currents
    if (length(funcGeneInd) > 0) # Should be
        funcGeneHeader <- .funcGeneAnnHeader(exportNames)
    else
        funcGeneHeader <- DataFrame()
    if (length(funcVarInd) > 0) # Not necessarily
        funcVarHeader <- .funcVarAnnHeader(exportNames)
    else
        funcVarHeader <- DataFrame()
    infoHeaders <- rbind(infoHeaders,funcGeneHeader,funcVarHeader)
    
    # 1.6. Initialize VCFHeader, add also fixed (FILTER)
    vcfHeader <- VCFHeader(reference=rownames(vcfMeta$contig),samples=sn,
        header=vcfMeta)
    fixed(vcfHeader) <- DataFrameList(
        FILTER=DataFrame(
            row.names="PASS",
            Description="All filters passed"
        )
    )
    info(vcfHeader) <- infoHeaders
    geno(vcfHeader) <- genoHeaders
    
    # 2. VCF data
    
    # 2.1. Ranged data - coordinates
    coords <- GRanges(
        seqnames=canoResult$identity_chr,
        ranges=IRanges(
            start=canoResult$identity_start,
            end=canoResult$identity_start
        )
    )
    refs <- DNAStringSet(canoResult$identity_ref)
    alts <- DNAStringSetList(lapply(canoResult$identity_alt,function(x) {
        return(DNAStringSet(x))
    }))
    
    # 2.2. Fixed data - alleles
    mcols(coords) <- fixed <- DataFrame(
        REF=refs,
        ALT=alts,
        QUAL=canoResult$metrics_qual,
        FILTER=rep("PASS",nrow(canoResult))
    )
    names(coords) <- rownames(fixed) <- canoResult$identity_rsid
    
    # 2.3. Info...........
    # 2.3.1. Main variant info fields
    # Original infoNames variable contains all other info-like fields to be
    # added apart from functional annotations
    infoValues <- DataFrame(canoResult[,infoNames],row.names=NULL)
    # infoHeaders is aligned with infoNames, apart from functional annotation
    # fields, so the colnames of infoValues will be the rownames of infoHeaders
    # minus the last two
    namInfo <- rownames(infoHeaders)
    if ("VESTA_FUNC_GENE_ANN" %in% namInfo)
        namInfo <- namInfo[-which(namInfo=="VESTA_FUNC_GENE_ANN")]
    if ("VESTA_FUNC_VAR_ANN" %in% namInfo)
        namInfo <- namInfo[-which(namInfo=="VESTA_FUNC_VAR_ANN")]
    names(infoValues) <- namInfo
    
    # 2.3.2. Functional gene and variant info fields
    # The fields to be collapsed... Genes/transcripts
    geneAnnNames <- intersect(.funcGeneFields(),exportNames)
    varAnnNames <- intersect(.funcVarFields(),exportNames)
    # geneAnnNames cannot be empty, we do not allow it from upstream
    # varAnnNames may be empty

    # Get a shorter version of the data frame to collapse
    geneAnnToBeCollapsed <- result[,geneAnnNames,drop=FALSE]
    # ...and split per variant (f defined above in the beginning)
    splitted <- split(geneAnnToBeCollapsed,f)
    # Each instance of split is a data frame where we must:
    # 1) Collapse the contents of each line separated by " | "
    # 2) Collapse the lines, separated by ^^^
    # 3) Assign the name VESTA_FUNC_GENE_ANN
    # Above extremely verbose, need more compact...
    # Suggestion:
    # gene1 | t1;t2;t3 | so1;so2;s3 | snpeff1;snpeff2;snpeff3 |
    # bio1;bio2;bio3 | ex1;ex2;ex3 | hgc1;hgc2;hgc3 | 
    # hgp1;hgp2;hgp3 | loc1;loc2;loc3 | cdna1;cdna2;cdna3 |
    # cds1;cds2;cds3 | aa1;aa2;aa3 | ids1;ids2 | omim1;omim2 |
    # cgd | disgenet | hpo | ctd
    # gene2 | t1;t2;t3 | so1;so2;s3 | snpeff1;snpeff2;snpeff3 |
    # bio1;bio2;bio3 | ex1;ex2;ex3 | hgc1;hgc2;hgc3 | 
    # hgp1;hgp2;hgp3 | loc1;loc2;loc3 | cdna1;cdna2;cdna3 |
    # cds1;cds2;cds3 | aa1;aa2;aa3 | ids1;ids2 | omim1;omim2 |
    # cgd | disgenet | hpo | ctd
    # For now we stick with vebose...
    tmpGen <- lapply(splitted,function(x) {
        if (canonical)
            x <- x[1,,drop=FALSE]
        x[is.na(x)] <- "."
        x[x==""] <- "."
        y <- apply(x,1,paste,collapse="|")
        y <- paste(y,collapse="^^^")
        # Replace any remaining ;s...
        return(gsub(";","+",y))
    })
    funcAnnGenes <- unlist(tmpGen,use.names=FALSE)
    funcAnnGenes <- DataFrame(VESTA_FUNC_GENE_ANN=funcAnnGenes)
    
    # Now variant functional annotations
    if (length(varAnnNames) > 0) {
        varAnnToBeCollapsed <- canoResult[,varAnnNames,drop=FALSE]
        varAnnToBeCollapsed[is.na(varAnnToBeCollapsed)] <- "."
        varAnnToBeCollapsed[varAnnToBeCollapsed==""] <- "."
        tmpVar <- apply(varAnnToBeCollapsed,1,paste,collapse="|")
        funcAnnVars <- unlist(tmpVar,use.names=FALSE)
        funcAnnVars <- DataFrame(VESTA_FUNC_VAR_ANN=funcAnnVars)
        # Final INFO values
        infoValues <- cbind(infoValues,funcAnnGenes,funcAnnVars)
    }
    else
        infoValues <- cbind(infoValues,funcAnnGenes)
    rownames(infoValues) <- names(coords)
    
    # 2.4. Genotypes
    genos <- lapply(canoResult[,genoNames],function(x) {
        x <- as.matrix(x)
        rownames(x) <- names(coords)
        return(x)
    })
    names(genos) <- rownames(genoHeaders)
    genos <- SimpleList(genos)
    
    # 2.5. Construct?
    vcf <- VCF(
        rowRanges=coords,
        colData=DataFrame(Samples=1L,row.names=sn),
        exptData=list(header=vcfHeader),
        fixed=fixed,
        info=infoValues,
        geno=genos
    )
    # For some reason, we may end up with unsorted VCF, order just in case...
    vcf <- vcf[order(seqnames(vcf),start(vcf))]
    
    # Write!
    #writeVcf(vcf,file="test.vcf",index=TRUE)
    
    # Define output file
    if (is.null(outFile))
        outFile <- tempfile()
    
    # Do we have results? If not, write an almost empty file
    if (nrow(result) == 0) {
        writeLines("No variants found.",outFile)
        return(outFile)
    }
    else {
        # Will be .bgz
        outf <- paste0(outFile,".bgz")
        writeVcf(vcf,file=outFile,index=TRUE)
        return(outf)
    }
}

.prepareExportPipeline <- function(aid,vid,que,groups,fields) {
    if (!is.null(groups)) {
        mongoFields <- .mongoVariantFieldGroups()[groups]
        mongoFields <- unlist(mongoFields,recursive=FALSE)
        names(mongoFields) <- sub("^[^.]+\\.","",names(mongoFields))
        nams <- names(mongoFields)
        mongoFields <- paste("$",mongoFields,sep="")
        names(mongoFields) <- nams
        # We need to be sure that ALL fields will be exported, not just those
        # that are not null in the specific analysis
        firstProjection <- lapply(names(mongoFields),function(n) {
            return(list(`$ifNull`=list(mongoFields[n],NULL)))
        })
        names(firstProjection) <- names(mongoFields)
    }
    else if (!is.null(fields)) {
        mongoFields <- .mongoMap()[fields]
        nams <- names(mongoFields)
        mongoFields <- paste("$",mongoFields,sep="")
        names(mongoFields) <- nams
        firstProjection <- lapply(names(mongoFields),function(n) {
            return(list(`$ifNull`=list(mongoFields[n],NULL)))
        })
        names(firstProjection) <- names(mongoFields)
    }
    
    # Construct the second projection, which will collapse nested documents
    # depending on the fields we have requested.
    if ("identity_rsid" %in% names(firstProjection))
        # Simple array of rs ids
        firstProjection[["identity_rsid"]] <- ..formatOpIdentityRsId()
        
    if ("annotation_genes_transcripts_impact_so" %in% names(firstProjection))
        # Simple array
        firstProjection[["annotation_genes_transcripts_impact_so"]] <- 
            ..formatOpAnnotationGenesTranscriptsImpactSo()
    
    if ("annotation_genes_cgd_condition" %in% names(firstProjection))
        firstProjection[["annotation_genes_cgd_condition"]] <- 
            ..formatOpAnnotationGenesCgdCondition()

    if (any(c("annotation_genes_disgenet_id","annotation_genes_disgenet_name")
        %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_genes_disgenet_id" %in% names(firstProjection))
            firstProjection[["annotation_genes_disgenet_id"]] <- NULL
        if ("annotation_genes_disgenet_name" %in% names(firstProjection))
            firstProjection[["annotation_genes_disgenet_name"]] <- NULL
        # Array of objects
        firstProjection[["annotation_genes_disgenet"]] <- 
            ..formatOpAnnotationGenesDisgenet()
    }
    
    if (any(c("annotation_genes_hpo_id","annotation_genes_hpo_name")
        %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_genes_hpo_id" %in% names(firstProjection))
            firstProjection[["annotation_genes_hpo_id"]] <- NULL
        if ("annotation_genes_hpo_name" %in% names(firstProjection))
            firstProjection[["annotation_genes_hpo_name"]] <- NULL
        # Array of objects
        firstProjection[["annotation_genes_hpo"]] <- 
            ..formatOpAnnotationGenesHpo()
    }
    
    if (any(c("annotation_genes_ctd_id","annotation_genes_ctd_name")
        %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_genes_ctd_id" %in% names(firstProjection))
            firstProjection[["annotation_genes_ctd_id"]] <- NULL
        if ("annotation_genes_ctd_name" %in% names(firstProjection))
            firstProjection[["annotation_genes_ctd_name"]] <- NULL
        # Array of objects
        firstProjection[["annotation_genes_ctd"]] <- 
            ..formatOpAnnotationGenesCtd()
    }
    
    if (any(c("annotation_variant_disgenet_id",
        "annotation_variant_disgenet_name") %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_disgenet_id" %in% names(firstProjection))
            firstProjection[["annotation_variant_disgenet_id"]] <- NULL
        if ("annotation_variant_disgenet_name" %in% names(firstProjection))
            firstProjection[["annotation_variant_disgenet_name"]] <- NULL
        # Array of objects
        firstProjection[["annotation_variant_disgenet"]] <- 
            ..formatOpAnnotationVariantDisgenet()
    }
    
    if (any(c("annotation_variant_pharmgkb_id",
        "annotation_variant_pharmgkb_loe") %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_pharmgkb_id" %in% names(firstProjection))
            firstProjection[["annotation_variant_pharmgkb_id"]] <- NULL
        if ("annotation_variant_pharmgkb_loe" %in% names(firstProjection))
            firstProjection[["annotation_variant_pharmgkb_loe"]] <- NULL
        # Array of objects
        firstProjection[["annotation_variant_pharmgkb_id"]] <- 
            ..formatOpAnnotationVariantPharmgkbId()
    }
    
    if (any(c("annotation_variant_pharmgkb_phenotypes_id",
        "annotation_variant_pharmgkb_phenotypes_name") %in% names(
        firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_pharmgkb_phenotypes_id" 
            %in% names(firstProjection))
            firstProjection[["annotation_variant_pharmgkb_phenotypes_id"]] <- 
                NULL
        if ("annotation_variant_pharmgkb_phenotypes_name" %in% names(
            firstProjection))
            firstProjection[["annotation_variant_pharmgkb_phenotypes_name"]] <- 
                NULL
        # Array of objects
        firstProjection[["annotation_variant_pharmgkb_phenotypes"]] <- 
            ..formatOpAnnotationVariantPharmgkbPhenotypes()
    }
    
    if (any(c("annotation_variant_pharmgkb_chemicals_id",
        "annotation_variant_pharmgkb_chemicals_name") %in% names(
        firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_pharmgkb_chemicals_id" 
            %in% names(firstProjection))
            firstProjection[["annotation_variant_pharmgkb_chemicals_id"]] <- 
                NULL
        if ("annotation_variant_pharmgkb_chemicals_name" %in% names(
            firstProjection))
            firstProjection[["annotation_variant_pharmgkb_chemicals_name"]] <- 
                NULL
        # Array of objects
        firstProjection[["annotation_variant_pharmgkb_chemicals"]] <- 
            ..formatOpAnnotationVariantPharmgkbChemicals()
    }
    
    if (any(c("annotation_variant_oncokb_gene_symbol",
        "annotation_variant_oncokb_alteration",
        "annotation_variant_oncokb_oncogenic",
        "annotation_variant_oncokb_effect") %in% names(firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_oncokb_gene_symbol" %in% names(firstProjection))
            firstProjection[["annotation_variant_oncokb_gene_symbol"]] <- NULL
        if ("annotation_variant_oncokb_alteration" %in% names(firstProjection))
            firstProjection[["annotation_variant_oncokb_alteration"]] <- NULL
        if ("annotation_variant_oncokb_oncogenic" %in% names(firstProjection))
            firstProjection[["annotation_variant_oncokb_oncogenic"]] <- NULL
        if ("annotation_variant_oncokb_effect" %in% names(firstProjection))
            firstProjection[["annotation_variant_oncokb_effect"]] <- NULL 
        # Something.....    
        firstProjection[["annotation_variant_oncokb_entry"]] <- 
            ..formatOpAnnotationVariantOncokbEntry()
    }
    
    if (any(c("annotation_variant_oncokb_treatments_code",
        "annotation_variant_oncokb_treatments_drug") %in% names(
        firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_oncokb_treatments_code
            " %in% names(firstProjection))
            firstProjection[["annotation_variant_oncokb_treatments_code"]] <- 
                NULL
        if ("annotation_variant_oncokb_treatments_drug" %in% names(
            firstProjection))
            firstProjection[["annotation_variant_oncokb_treatments_drug"]] <- 
                NULL

        firstProjection[["annotation_variant_oncokb_treatments"]] <- 
            ..formatOpAnnotationVariantOncokbTreatments()
    }
    
    if (any(c("annotation_variant_civic_id","annotation_variant_civic_gene_id",
        "annotation_variant_civic_variant") %in% names(
        firstProjection))) {
        # Remove the separate fields
        if ("annotation_variant_civic_id" %in% names(firstProjection))
            firstProjection[["annotation_variant_civic_id"]] <- NULL
        if ("annotation_variant_civic_gene_id" %in% names(firstProjection))
            firstProjection[["annotation_variant_civic_gene_id"]] <- NULL
        if ("annotation_variant_civic_variant" %in% names(firstProjection))
            firstProjection[["annotation_variant_civic_variant"]] <- NULL

        firstProjection[["annotation_variant_civic"]] <- 
            ..formatOpAnnotationVariantCivic()
    }
    
    # Then basic aggregation pipeline, unwinds essentially happen only at
    # gene/transcript level
    if (!is.null(aid))
        thePipeline <- list(
            list(
                `$match`=list(
                    `analysis_id`=list(`$oid`=aid)
                )
            ),
            list(
                `$unwind`="$annotation.genes"
            ),
            list(
                `$unwind`="$annotation.genes.transcripts"
            ),
            list(
                `$project`=firstProjection
            )
        )
    else if (!is.null(vid)) {
        oidList <- lapply(vid,function(x) list(`$oid`=x))
        thePipeline <- list(
            list(
                `$match`=list(
                    `_id`=list(`$in`=oidList)
                )
            ),
            list(
                `$unwind`="$annotation.genes"
            ),
            list(
                `$unwind`="$annotation.genes.transcripts"
            ),
            list(
                `$project`=firstProjection
            )
        )
    }
    else if (!is.null(que)) {
        thePipeline <- list(
            list(
                `$match`=que
            ),
            list(
                `$unwind`="$annotation.genes"
            ),
            list(
                `$unwind`="$annotation.genes.transcripts"
            ),
            list(
                `$project`=firstProjection
            )
        )
    }
    
    return(thePipeline)
}


.makeVcfMeta <- function(aid) {
    # Required meta elements
    # fileformat, contig(s), reference, source(=VESTA 1.0)
    # 
    # Steps
    # 1. Get reference (genome_version) from analysis id
    # 2. Get contigs (genome_size) from the first of the associated samples 
    #    in the analysis
    # 3. Construct and output a DataFrame
    
    # 0. Database connections
    cona <- mongoConnect("analyses")
    cons <- mongoConnect("samples")
    on.exit({
        mongoDisconnect(cona)
        mongoDisconnect(cons)
    })
    
    # 1. Reference genome (get also samples in the query)
    query <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        metadata.genome_version=1L,
        samples.id=1L
    ))
    result <- cona$find(query,fields=fields)
    if (nrow(result) == 0) # Unlikely
        stop("Analysis with id ",aid," not found in database!")
    gv <- result[1,1]$genome_version
    gv <- DataFrame(
        row.names="reference",
        Value=gv
    )
    
    # 2. Contigs from sample, also sample name, will be needed later
    sid <- result$samples[[1]]$id
    query <- .toMongoJSON(list(
        `_id`=list(`$oid`=sid)
    ))
    fields <- .toMongoJSON(list(
        `_id`=0L,
        name=1L,
        genome_size=1L
    ))
    result <- cons$find(query,fields=fields)
    if (nrow(result) == 0) # Unlikely
        stop("Sample with id ",sid," not found in database!")
    gs <- result$genome_size[[1]] # data.frame
    rownames(gs) <- gs$seq
    gs$seq <- NULL
    names(gs)[1] <- "length"
    gs$length <- as.character(gs$length)
    gs <- DataFrame(gs)
    sn <- DataFrame(
        row.names="sample",
        Value=result$name
    )
    
    # 3. Construct the meta DataFrameList to return
    ff <- DataFrame(
        row.names="fileformat",
        Value="VCFv4.2"
    )
    so <- DataFrame(
        row.names="source",
        Value="VESTA v1.0"
    )
    M <- DataFrameList(ff,gv,so,gs,sn)
    names(M) <- c("fileformat","reference","source","contig","_sample")
    
    return(M)
}

..formatOpIdentityRsId <- function() {
    return(list(
        `$ifNull`=list(
            list(
                `$reduce`=list(
                    input="$identity.rsid",
                    initialValue="",
                    "in"=list(
                        `$cond`=list(
                            list(`$eq`=list("$$value","")),
                            "$$this",
                            list(`$concat`=list("$$value",", ","$$this"))
                        )
                    )
                )
            ),
            NULL # Fallback
        )
    ))
}

..formatOpAnnotationGenesTranscriptsImpactSo <- function() {
    list(
        `$ifNull`=list(
            list(
                `$reduce`=list(
                    input="$annotation.genes.transcripts.impact_so",
                    initialValue="",
                    "in"=list(
                        `$cond`=list(
                            list(`$eq`=list("$$value","")),
                            "$$this",
                            list(`$concat`=list("$$value",", ","$$this"))
                        )
                    )
                )
            ),
            NULL # Fallback
        )
    )
}

..formatOpAnnotationGenesCgdCondition <- function() {
    # ChatGPT on tsunami and fire...
    return(list(
      `$ifNull`=list(
        list(
          `$let`=list(
            vars=list(
              normArray=list(
                `$cond`=list(
                  list(`$isArray`="$annotation.genes.cgd.condition"),
                    list(
                      `$reduce`=list(
                        input="$annotation.genes.cgd.condition",
                        initialValue=list(),
                        "in"=list(
                          `$concatArrays`=list(
                            "$$value",
                            list(
                              `$cond`=list(
                                list(`$isArray`="$$this"),
                                "$$this",
                                list("$$this")
                              )
                            )
                          )
                        )
                      )
                    ),
                    list("$$ROOT.annotation.genes.cgd.condition")
                  )
                )
              ),
              "in"=list(
                `$reduce`=list(
                  input="$$normArray",
                  initialValue="",
                  "in"=list(
                    `$cond`=list(
                      list(`$eq`=list("$$value", "")),
                      "$$this",
                      list(`$concat`=list("$$value", " + ", "$$this"))
                    )
                  )
                )
              )
            )
          ),
          NULL
       )
    ))

    ## ChatGPT on fire...
    #return(list(
    # `$ifNull`=list(
    #    list(
    #        `$reduce`=list(
    #            input=list(
    #                `$reduce`=list(
    #                    input="$annotation.genes.cgd.condition",
    #                    initialValue=list(),
    #                    "in"=list(`$concatArrays`=list("$$value",
    #                        "$$this"))
    #                )
    #            ),
    #            initialValue="",
    #            "in"=list(
    #                `$cond`=list(
    #                    list(`$eq`=list("$$value","")),
    #                    "$$this",
    #                    list(`$concat`=list("$$value",", ","$$this"))
    #                )
    #            )
    #        )
    #    ),
    #    NULL
    #  )
    #))
}

..formatOpAnnotationGenesDisgenet <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.genes.disgenet"),
                `then`=list(
                    `$reduce`=list(
                        input="$annotation.genes.disgenet",
                        initialValue="",
                        "in"=list(
                            `$cond`=list(
                                `if`=list(`$eq`=list("$$value", "")),
                                `then`=list(
                                    `$concat`=list(
                                        "$$this.id","~","$$this.name"
                                    )
                                ),
                                `else`=list(
                                    `$concat`=list(
                                        "$$value"," + ","$$this.id","~",
                                        "$$this.name"
                                    )
                                )
                            )
                        )
                    )
                ),
                `else`=NULL
        )
    ))
}

..formatOpAnnotationGenesHpo <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.genes.hpo"),
                `then`=list(
                    `$reduce`=list(
                        input="$annotation.genes.hpo",
                        initialValue="",
                        "in"=list(
                            `$cond`=list(
                                `if`=list(`$eq`=list("$$value", "")),
                                `then`=list(
                                    `$concat`=list(
                                        list(
                                            `$replaceAll`=list(
                                                input="$$this.id",find=":",
                                                    replacement=""
                                                )
                                            ),"~","$$this.name"
                                    )
                                ),
                                `else`=list(
                                    `$concat`=list(
                                        "$$value"," + ",
                                        list(
                                            `$replaceAll`=list(
                                                input="$$this.id",
                                                find=":",replacement=""
                                            )
                                        ),"~","$$this.name"
                                    )
                                )
                            )
                        )
                    )
                ),
                `else`=NULL
        )
    ))
}

..formatOpAnnotationGenesCtd <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.genes.ctd"),
                `then`=list(
                    `$reduce`=list(
                        input="$annotation.genes.ctd",
                        initialValue="",
                        "in"=list(
                            `$cond`=list(
                                `if`=list(`$eq`=list("$$value", "")),
                                `then`=list(
                                    `$concat`=list(
                                        "$$this.id","~","$$this.name"
                                    )
                                ),
                                `else`=list(
                                    `$concat`=list(
                                        "$$value"," + ","$$this.id","~",
                                        "$$this.name"
                                    )
                                )
                            )
                        )
                    )
                ),
                `else`=NULL
        )
    ))
}

..formatOpAnnotationVariantDisgenet <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.variant.disgenet"),
            `then`=list(
                `$reduce`=list(
                    input="$annotation.variant.disgenet",
                    initialValue="",
                    "in"=list(
                        `$cond`=list(
                            `if`=list(`$eq`=list("$$value", "")),
                            `then`=list(
                                `$concat`=list(
                                    "$$this.id","~","$$this.name"
                                )
                            ),
                            `else`=list(
                                `$concat`=list(
                                    "$$value"," + ","$$this.id","~",
                                    "$$this.name"
                                )
                            )
                        )
                    )
                )
            ),
            `else`=NULL
        )
    ))
}

..formatOpAnnotationVariantPharmgkbId <- function() {
    return(list(
        `$cond` = list(
            `if`=list(`$isArray`="$annotation.variant.pharmgkb"),
            `then`=list(
                `$reduce`=list(
                    input="$annotation.variant.pharmgkb",
                    initialValue="",
                    `in`=list(
                        `$cond`=list(
                            `if`=list(`$eq`=list("$$value","")),
                            `then`=list(
                                `$concat`=list(
                                    "$$this.pharmgkb_id",
                                    " with LOE ",
                                    "$$this.level_of_evidence"
                                )
                            ),
                            `else`=list(
                                `$concat`=list(
                                    "$$value"," + ",
                                    "$$this.pharmgkb_id",
                                    " with LOE ",
                                    "$$this.level_of_evidence"
                                )
                            )
                        )
                    )
                )
            ),
            `else`=NULL
        )
    ))
}
    
..formatOpAnnotationVariantPharmgkbChemicals <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.variant.pharmgkb"),
            `then`=list(
                `$reduce`=list(
                    input="$annotation.variant.pharmgkb",
                    initialValue="",
                    `in`=list(
                        `$let`=list(
                            vars=list(
                                entries=list(
                                    `$map`=list(
                                        input="$$this.chemicals",
                                        as="c",
                                        `in`=list(
                                            `$concat`=list(
                                                "$$c.pharmgkb_id","~",
                                                "$$c.chemical_name"
                                            )
                                        )
                                    )
                                )
                            ),
                            `in`=list(
                                `$reduce`=list(
                                    input="$$entries",
                                    initialValue="$$value",
                                    `in`=list(
                                        `$cond`=list(
                                            `if`=list(
                                                `$eq`=list("$$value", "")
                                            ),
                                            `then`="$$this",
                                            `else`=list(
                                                `$concat`=list("$$value"," + ",
                                                    "$$this")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            `else`=NULL
        )
    ))
}

..formatOpAnnotationVariantPharmgkbPhenotypes <- function() {
    return(list(
        `$cond`=list(
            `if`=list(`$isArray`="$annotation.variant.pharmgkb"),
            `then`=list(
                `$reduce`=list(
                    input="$annotation.variant.pharmgkb",
                    initialValue="",
                    `in`=list(
                        `$let`=list(
                            vars=list(
                                entries=list(
                                    `$map`=list(
                                        input="$$this.phenotypes",
                                        as="p",
                                        `in`=list(
                                            `$concat`=list("$$p.id","~", 
                                                "$$p.name")
                                        )
                                    )
                                )
                            ),
                            `in`=list(
                                `$reduce`=list(
                                    input="$$entries",
                                    initialValue="$$value",
                                    `in`=list(
                                        `$cond`=list(
                                            `if`=list(`$eq`=list("$$value","")),
                                            `then`="$$this",
                                            `else`=list(
                                                `$concat`=list("$$value"," + ",
                                                    "$$this")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            `else`=NULL
        )
    ))
}

..formatOpAnnotationVariantOncokbEntry <- function() {
    return(list(
        `$ifNull`=list(
            list(
                `$concat`=list(
                    "https://www.oncokb.org/gene/",
                    "$annotation.variant.oncokb.gene_symbol","/",
                    "$annotation.variant.oncokb.alteration"
                )
            ),
            NULL
        )
    ))
}

..formatOpAnnotationVariantOncokbTreatments <- function() {
    return(list(
        `$ifNull`=list(
            list(
                `$reduce`=list(
                    input=list(
                        `$setUnion`=list(
                            list(
                                `$map`=list(
                                    input=
                                        "$annotation.variant.oncokb.treatments",
                                    as="t",
                                    `in`="$$t.drug"
                                )
                            ),
                            list()
                        )
                    ),
                    initialValue="",
                    `in`=list(
                        `$cond`=list(
                            `if`=list(`$eq`=list("$$value", "")),
                            `then`="$$this",
                            `else`=list(`$concat`=list("$$value"," + ",
                                "$$this"))
                        )
                    )
                )
            ),
            NULL
        )
    ))
}

..formatOpAnnotationVariantCivic <- function() {
    return(list(
        `$ifNull`=list(
            list(
                `$concat`=list(
                    "https://civicdb.org/variants/",
                    list(`$toString`="$annotation.variant.civic.id")
                )
            ),
            NULL
        )
    ))
}

.identityVcfFields <- function() {
    return(c("identity_chr","identity_start","identity_ref","identity_alt",
        "metrics_qual","identity_rsid"))
}

.canonicalSplitFields <- function() {
    return(c(
        "identity_chr",
        "identity_start",
        "identity_ref",
        "identity_alt"
    ))
}

.geneNestFields <- function() {
    return(c(
        "annotation_genes_name",
        "annotation_genes_ids",
        "annotation_genes_omim",
        "annotation_genes_omim_morbid"
    ))
}

.transcriptNestFields <- function() {
    return(c(
        "annotation_genes_transcripts_transcript_id",
        "annotation_genes_transcripts_impact_so",
        "annotation_genes_transcripts_impact_snpeff",
        "annotation_genes_transcripts_biotype",
        "annotation_genes_transcripts_exon_rank",
        "annotation_genes_transcripts_hgvs_c",
        "annotation_genes_transcripts_hgvs_p",
        "annotation_genes_transcripts_location",
        "annotation_genes_transcripts_cdna_rank",
        "annotation_genes_transcripts_cds_rank",
        "annotation_genes_transcripts_aa_rank"
    ))
}

.funcGeneFields <- function() {
    return(c(
        "annotation_genes_name",
        "annotation_genes_ids",
        "annotation_genes_transcripts_transcript_id",
        "annotation_genes_transcripts_impact_so",
        "annotation_genes_transcripts_impact_snpeff",
        "annotation_genes_transcripts_biotype",
        "annotation_genes_transcripts_exon_rank",
        "annotation_genes_transcripts_hgvs_c",
        "annotation_genes_transcripts_hgvs_p",
        "annotation_genes_transcripts_location",
        "annotation_genes_transcripts_cdna_rank",
        "annotation_genes_transcripts_cds_rank",
        "annotation_genes_transcripts_aa_rank",
        "annotation_genes_omim",
        "annotation_genes_omim_morbid",
        "annotation_genes_cgd_condition",
        "annotation_genes_cgd_inheritance",
        "annotation_genes_disgenet_id",
        "annotation_genes_disgenet_name",
        "annotation_genes_hpo_id",
        "annotation_genes_hpo_name",
        "annotation_genes_ctd_id",
        "annotation_genes_ctd_name",
        "annotation_genes_disgenet",
        "annotation_genes_hpo",
        "annotation_genes_ctd"
    ))
}

.funcVarFields <- function() {
    return(c(
        "annotation_variant_disgenet_id",
        "annotation_variant_disgenet_name",
        "annotation_variant_pharmgkb_id",
        "annotation_variant_pharmgkb_loe",
        "annotation_variant_pharmgkb_phenotypes_id",
        "annotation_variant_pharmgkb_phenotypes_name",
        "annotation_variant_pharmgkb_chemicals_id",
        "annotation_variant_pharmgkb_chemicals_name",
        "annotation_variant_clinvar_alleleid",
        "annotation_variant_clinvar_clnsig",
        "annotation_variant_clinvar_onc",
        "annotation_variant_oncokb_gene_symbol",
        "annotation_variant_oncokb_alteration",
        "annotation_variant_oncokb_oncogenic",
        "annotation_variant_oncokb_effect",
        "annotation_variant_oncokb_treatments_code",
        "annotation_variant_oncokb_treatments_drug",
        "annotation_variant_civic_id",
        "annotation_variant_civic_gene_id",
        "annotation_variant_civic_variant",
        "annotation_variant_disgenet",
        "annotation_variant_pharmgkb_id",
        "annotation_variant_pharmgkb_phenotypes",
        "annotation_variant_pharmgkb_chemicals",
        "annotation_variant_oncokb_entry",
        "annotation_variant_oncokb_treatments",
        "annotation_variant_civic"
    ))
}

.minVcfExportGroups <- function() {
    return(c("location","variant","qual","dbsnp","depth","vaf"))
}

.minVcfExportFields <- function() {
    return(c(
        "identity_chr","identity_start","identity_ref","identity_alt",
        "identity_rsid","metrics_qual","genotypes_gn","genotypes_dp",
        "genotypes_ad_ref","genotypes_ad_alt","genotypes_af"
    ))
}

.mongoVariantFieldGroupNames <- function() {
    return(names(.mongoVariantFieldGroups()))
}

.mongoVariantFieldGroups <- function() {
    return(list(
        location=c(
            "identity_chr"="identity.chr",
            "identity_start"="identity.start",
            "identity_end"="identity.end"
        ),
        variant=c(
            "identity_ref"="identity.ref",
            "identity_alt"="identity.alt",
            "identity_type"="identity.type",
            "genotypes_gn"="genotypes.gn",
            "genotypes_gtn"="genotypes.gtn"
        ),
        depth=c(
            "genotypes_dp"="genotypes.dp",
            "genotypes_ad_ref"="genotypes.ad_ref",
            "genotypes_ad_alt"="genotypes.ad_alt"
        ),
        vaf=c(
            "genotypes_af"="genotypes.af"
        ),
        dbsnp=c(
            "identity_rsid"="identity.rsid"
        ),
        cosmic=c(
            "identity_cosmic"="identity.cosmic"
        ),
        gene=c(
            "annotation_genes_name"="annotation.genes.name",
            "annotation_genes_ids"="annotation.genes.ids",
            "annotation_genes_omim"="annotation.genes.omim",
            "annotation_genes_omim_morbid"="annotation.genes.omim_morbid",
            "annotation_genes_transcripts_transcript_id"=
                "annotation.genes.transcripts.transcript_id",
            "annotation_genes_transcripts_biotype"=
                "annotation.genes.transcripts.biotype"
        ),
        hgvs=c(
            "annotation_genes_transcripts_hgvs_c"=
                "annotation.genes.transcripts.hgvs_c",
            "annotation_genes_transcripts_hgvs_p"=
                "annotation.genes.transcripts.hgvs_p"
        ),
        impact=c(
            "annotation_genes_transcripts_impact_snpeff"=
                "annotation.genes.transcripts.impact_snpeff",
            "annotation_genes_transcripts_impact_so"=
                "annotation.genes.transcripts.impact_so",
            "annotation_genes_transcripts_location"=
                "annotation.genes.transcripts.location"
        ),
        exon=c(
            "annotation_genes_transcripts_exon_rank"=
                "annotation.genes.transcripts.exon_rank",
            "annotation_genes_transcripts_cdna_rank"=
                "annotation.genes.transcripts.cdna_rank",
            "annotation_genes_transcripts_cds_rank"=
                "annotation.genes.transcripts.cds_rank",
            "annotation_genes_transcripts_aa_rank"=
                "annotation.genes.transcripts.aa_rank"
        ),
        sift=c(
            "patho_sift_score"="pathogenicity.sift_score",
            "patho_sift_rankscore"="pathogenicity.sift_converted_score",
            "patho_sift_pred"="pathogenicity.sift_pred",
            "patho_sift4g_score"="pathogenicity.sift4g_score",
            "patho_sift4g_rankscore"="pathogenicity.sift4g_converted_rankscore",
            "patho_sift4g_pred"="pathogenicity.sift4g_pred"
        ),
        polyphen2=c(
            "patho_polyphen2_hdiv_score"="pathogenicity.polyphen2_hdiv_score",
            "patho_polyphen2_hdiv_rankscore"=
                "pathogenicity.polyphen2_hdiv_rankscore",
            "patho_polyphen2_hdiv_pred"="pathogenicity.polyphen2_hdiv_pred",
            "patho_polyphen2_hvar_score"="pathogenicity.polyphen2_hvar_score",
            "patho_polyphen2_hvar_rankscore"=
                "pathogenicity.polyphen2_hvar_rankscore",
            "patho_polyphen2_hvar_pred"="pathogenicity.polyphen2_hvar_pred"
        ),
        # FATHMM/LRT/PROVEAN
        fathmmgroup=c(
            "patho_fathmm_score"="pathogenicity.fathmm_score",
            "patho_fathmm_rankscore"="pathogenicity.fathmm_converted_rankscore",
            "patho_fathmm_pred"="pathogenicity.fathmm_pred",
            "patho_lrt_score"="pathogenicity.lrt_score",
            "patho_lrt_rankscore"="pathogenicity.lrt_converted_rankscore",
            "patho_lrt_pred"="pathogenicity.lrt_pred",
            "patho_provean_score"="pathogenicity.provean_score",
            "patho_provean_rankscore"=
                "pathogenicity.provean_converted_rankscore",
            "patho_provean_pred"="pathogenicity.provean_pred"
        ),
        # Mutation Assessor/Taster
        mutationgroup=c(
            "patho_mutationtaster_score"="pathogenicity.mutationtaster_score",
            "patho_mutationtaster_rankscore"=
                "pathogenicity.mutationtaster_converted_rankscore",
            "patho_mutationtaster_pred"="pathogenicity.mutationtaster_pred",
            "patho_mutationassessor_score"=
                "pathogenicity.mutationassessor_score",
            "patho_mutationassessor_rankscore"=
                "pathogenicity.mutationassessor_rankscore",
            "patho_mutationassessor_pred"="pathogenicity.mutationassessor_pred"
        ),
        # Meta(SVM/LR/RNN)
        metagroup=c(
            "patho_metasvm_score"="pathogenicity.metasvm_score",
            "patho_metasvm_rankscore"="pathogenicity.metasvm_rankscore",
            "patho_metasvm_pred"="pathogenicity.metasvm_pred",
            "patho_metalr_score"="pathogenicity.metalr_score",
            "patho_metalr_rankscore"="pathogenicity.metalr_rankscore",
            "patho_metalr_pred"="pathogenicity.metalr_pred",
            "patho_metarnn_score"="pathogenicity.metarnn_score",
            "patho_metarnn_rankscore"="pathogenicity.metarnn_rankscore",
            "patho_metarnn_pred"="pathogenicity.metarnn_pred"
        ),
        # M-CAP/PrimateAI/DEOGEN2"
        mcapgroup=c(
            "patho_m_cap_score"="pathogenicity.m_cap_score",
            "patho_m_cap_rankscore"="pathogenicity.m_cap_rankscore",
            "patho_m_cap_pred"="pathogenicity.m_cap_pred",
            "patho_primateai_score"="pathogenicity.primateai_score",
            "patho_primateai_rankscore"="pathogenicity.primateai_rankscore",
            "patho_primateai_pred"="pathogenicity.primateai_pred",
            "patho_deogen2_score"="pathogenicity.deogen2_score",
            "patho_deogen2_rankscore"="pathogenicity.deogen2_rankscore",
            "patho_deogen2_pred"="pathogenicity.deogen2_pred"
        ),
        # ClinPred/AlphaMissense",
        clinpredgroup=c(
            "patho_clinpred_score"="pathogenicity.clinpred_score",
            "patho_clinpred_rankscore"="pathogenicity.clinpred_rankscore",
            "patho_clinpred_pred"="pathogenicity.clinpred_pred",
            "patho_alphamissense_score"="pathogenicity.alphamissense_score",
            "patho_alphamissense_rankscore"=
                "pathogenicity.alphamissense_rankscore",
            "patho_alphamissense_pred"="pathogenicity.alphamissense_pred"
        ),
        # DANN/VEST4/REVEL
        danngroup=c(
            "patho_dann_score"="pathogenicity.dann_score",
            "patho_dann_rankscore"="pathogenicity.dann_rankscore",
            "patho_vest4_score"="pathogenicity.vest4_score",
            "patho_vest4_rankscore"="pathogenicity.vest4_rankscore",
            "patho_revel_score"="pathogenicity.revel_score",
            "patho_revel_rankscore"="pathogenicity.revel_rankscore"
        ),
        clinvar=c(
            "annotation_variant_clinvar_alleleid"=
                "pathogenicity.clinvar_alleleid",
            "annotation_variant_clinvar_clnsig"="pathogenicity.clinvar_clnsig",
            "annotation_variant_clinvar_onc"="pathogenicity.clinvar_onc"
        ),
        gerpgroup=c(
            "cons_gerp_nr"="conservation.gerp___nr",
            "cons_gerp_rs"="conservation.gerp___rs",
            "cons_gerp_rs_ranscore"="conservation.gerp___rs_rankscore"
        ),
        phylopgroup=c(
            "cons_phylop100way_vertebrate"=
                "conservation.phylop100way_vertebrate",
            "cons_phylop100way_vertebrate_rankscore"=
                "conservation.phylop100way_vertebrate_rankscore",
            "cons_phylop470way_mammalian"="conservation.phylop470way_mammalian",
            "cons_phylop470way_mammalian_rankscore"=
                "conservation.phylop470way_mammalian_rankscore",
            "cons_phylop17way_primate"="conservation.phylop17way_primate",
            "cons_phylop17way_primate_rankscore"=
                "conservation.phylop17way_primate_rankscore"
        ),
        phastconsgroup=c(
            "cons_phastcons100way_vertebrate"=
                "conservation.phastcons100way_vertebrate",
            "cons_phastcons100way_vertebrate_rankscore"=
                "conservation.phastcons100way_vertebrate_rankscore",
            "cons_phastcons470way_mammalian"=
                "conservation.phastcons470way_mammalian",
            "cons_phastcons470way_mammalian_rankscore"=
                "conservation.phastcons470way_mammalian_rankscore",
            "cons_phastcons17way_primate"="conservation.phastcons17way_primate",
            "cons_phastcons17way_primate_rankscore"=
                "conservation.phastcons17way_primate_rankscore"
        ),
        cgd=c(
            "annotation_genes_cgd_condition"="annotation.genes.cgd.condition",
            "annotation_genes_cgd_inheritance"=
                "annotation.genes.cgd.inheritance"
        ),
        disgenet_genes=c(
            "annotation_genes_disgenet_id"="annotation.genes.disgenet.id",
            "annotation_genes_disgenet_name"="annotation.genes.disgenet.name"
        ),
        hpo=c(
            "annotation_genes_hpo_id"="annotation.genes.hpo.id",
            "annotation_genes_hpo_name"="annotation.genes.hpo.name"
        ),
        ctd=c(
            "annotation_genes_ctd_id"="annotation.genes.ctd.id",
            "annotation_genes_ctd_name"="annotation.genes.ctd.name"
        ),
        tgp=c(
            "population_tgp_af"="population.tgp.af",
            "population_tgp_ac"="population.tgp_ac",
            "population_tgp_eur_af"="population.tgp.eur_af",
            "population_tgp_eur_ac"="population.tgp.eur_ac",
            "population_tgp_amr_af"="population.tgp.amr_af",
            "population_tgp_amr_ac"="population.tgp.amr_ac",
            "population_tgp_eas_af"="population.tgp.eas_af",
            "population_tgp_eas_ac"="population.tgp.eas_ac",
            "population_tgp_sas_af"="population.tgp.sas_af",
            "population_tgp_sas_ac"="population.tgp.sas_ac",
            "population_tgp_afr_af"="population.tgp.afr_af",
            "population_tgp_afr_ac"="population.tgp.afr_ac"
        ),
        exac=c(
            "population_exac_af"="population.exac.af",
            "population_exac_ac"="population.exac.ac",
            "population_exac_adj_af"="population.exac.adj_af",
            "population_exac_adj_ac"="population.exac.adj_ac",
            "population_exac_nfe_af"="population.exac.nfe_af",
            "population_exac_nfe_ac"="population.exac.nfe_ac",
            "population_exac_fin_af"="population.exac.fin_af",
            "population_exac_fin_ac"="population.exac.fin_ac",
            "population_exac_amr_af"="population.exac.amr_af",
            "population_exac_amr_ac"="population.exac.amr_ac",
            "population_exac_eas_af"="population.exac.eas_af",
            "population_exac_eas_ac"="population.exac.eas_ac",
            "population_exac_sas_af"="population.exac.sas_af",
            "population_exac_sas_ac"="population.exac.sas_ac",
            "population_exac_afr_af"="population.exac.afr_af",
            "population_exac_afr_ac"="population.exac.afr_ac"
        ),
        esp=c(
            "population_esp_aa_af"="population.esp.aa_af",
            "population_esp_aa_ac"="population.esp.aa_ac",
            "population_esp_ea_af"="population.esp.ea_af",
            "population_esp_ea_ac"="population.esp.ea_ac"
        ),
        uk10k=c(
            "population_uk10k_af"="population.uk10k.af",
            "population_uk10k_ac"="population.uk10k.ac"
        ),
        alfa=c(
            "population_alfa_european_af"="population.alfa.european_af",
            "population_alfa_european_ac"="population.alfa.european_ac",
            "population_alfa_african_others_af"=
                "population.alfa.african_others_af",
            "population_alfa_african_others_ac"=
                "population.alfa.african_others_ac",
            "population_alfa_east_asian_af"="population.alfa.east_asian_af",
            "population_alfa_east_asian_ac"="population.alfa.east_asian_ac",
            "population_alfa_african_american_af"=
                "population.alfa.african_american_af",
            "population_alfa_african_american_ac"=
                "population.alfa.african_american_ac",
            "population_alfa_latin_american_1_af"=
                "population.alfa.latin_american_1_af",
            "population_alfa_latin_american_1_ac"=
                "population.alfa.latin_american_1_ac",
            "population_alfa_latin_american_2_af"=
                "population.alfa.latin_american_2_af",
            "population_alfa_latin_american_2_ac"=
                "population.alfa.latin_american_2_ac",
            "population_alfa_other_asian_af"="population.alfa.other_asian_af",
            "population_alfa_other_asian_ac"="population.alfa.other_asian_ac",
            "population_alfa_south_asian_af"="population.alfa.south_asian_af",
            "population_alfa_south_asian_ac"="population.alfa.south_asian_ac",
            "population_alfa_other_af"="population.alfa.other_af",
            "population_alfa_other_ac"="population.alfa.other_ac",
            "population_alfa_african_af"="population.alfa.african_af",
            "population_alfa_african_ac"="population.alfa.african_ac",
            "population_alfa_asian_af"="population.alfa.asian_af",
            "population_alfa_asian_ac"="population.alfa.asian_ac",
            "population_alfa_total_af"="population.alfa.total_af",
            "population_alfa_total_ac"="population.alfa.total_ac"
        ),
        gnomad_exomes=c(
            "population_gnomad_exomes_af"="population.gnomad_exomes.af",
            "population_gnomad_exomes_ac"="population.gnomad_exomes.ac",
            "population_gnomad_exomes_afr_af"="population.gnomad_exomes.af_afr",
            "population_gnomad_exomes_afr_ac"="population.gnomad_exomes.ac_afr",
            "population_gnomad_exomes_amr_af"="population.gnomad_exomes.af_amr",
            "population_gnomad_exomes_amr_ac"="population.gnomad_exomes.ac_amr",
            "population_gnomad_exomes_asj_af"="population.gnomad_exomes.af_asj",
            "population_gnomad_exomes_asj_ac"="population.gnomad_exomes.ac_asj",
            "population_gnomad_exomes_eas_af"="population.gnomad_exomes.af_eas",
            "population_gnomad_exomes_eas_ac"="population.gnomad_exomes.ac_eas",
            "population_gnomad_exomes_fin_af"="population.gnomad_exomes.af_fin",
            "population_gnomad_exomes_fin_ac"="population.gnomad_exomes.ac_fin",
            "population_gnomad_exomes_mid_af"="population.gnomad_exomes.af_mid",
            "population_gnomad_exomes_mid_ac"="population.gnomad_exomes.ac_mid",
            "population_gnomad_exomes_nfe_af"="population.gnomad_exomes.af_nfe",
            "population_gnomad_exomes_nfe_ac"="population.gnomad_exomes.ac_nfe",
            "population_gnomad_exomes_sas_af"="population.gnomad_exomes.af_sas",
            "population_gnomad_exomes_sas_ac"="population.gnomad_exomes.ac_sas"
        ),
        gnomad_genomes=c(
            "population_gnomad_genomes_af"="population.gnomad_genomes.af",
            "population_gnomad_genomes_ac"="population.gnomad_genomes.ac",
            "population_gnomad_genomes_afr_af"=
                "population.gnomad_genomes.af_afr",
            "population_gnomad_genomes_afr_ac"=
                "population.gnomad_genomes.ac_afr",
            "population_gnomad_genomes_amr_af"=
                "population.gnomad_genomes.af_amr",
            "population_gnomad_genomes_amr_ac"=
                "population.gnomad_genomes.ac_amr",
            "population_gnomad_genomes_asj_af"=
                "population.gnomad_genomes.af_asj",
            "population_gnomad_genomes_asj_ac"=
                "population.gnomad_genomes.ac_asj",
            "population_gnomad_genomes_eas_af"=
                "population.gnomad_genomes.af_eas",
            "population_gnomad_genomes_eas_ac"=
                "population.gnomad_genomes.ac_eas",
            "population_gnomad_genomes_fin_af"=
                "population.gnomad_genomes.af_fin",
            "population_gnomad_genomes_fin_ac"=
                "population.gnomad_genomes.ac_fin",
            "population_gnomad_genomes_nfe_af"=
                "population.gnomad_genomes.af_nfe",
            "population_gnomad_genomes_nfe_ac"=
                "population.gnomad_genomes.ac_nfe"
        ),
        disgenet_variants=c(
            "annotation_variant_disgenet_id"="annotation.variant.disgenet.id",
            "annotation_variant_disgenet_name"=
                "annotation.variant.disgenet.name"
        ),
        pharmgkb=c(
            "annotation_variant_pharmgkb_id"=
                "annotation.variant.pharmgkb.pharmgkb_id",
            "annotation_variant_pharmgkb_loe"=
                "annotation.variant.pharmgkb.level_of_evidence",
            "annotation_variant_pharmgkb_phenotypes_id"=
                "annotation.variant.pharmgkb.phenotypes.id",
            "annotation_variant_pharmgkb_phenotypes_name"=
                "annotation.variant.pharmgkb.phenotypes.name",
            "annotation_variant_pharmgkb_chemicals_id"=
                "annotation.variant.pharmgkb.chemicals.pharmgkb_id",
            "annotation_variant_pharmgkb_chemicals_name"=
                "annotation.variant.pharmgkb.chemicals.chemical_name"
        ),
        oncokb=c(
            "annotation_variant_oncokb_gene_symbol"=
                "annotation.variant.oncokb.gene_symbol",
            "annotation_variant_oncokb_alteration"=
                "annotation.variant.oncokb.alteration",
            "annotation_variant_oncokb_oncogenic"=
                "annotation.variant.oncokb.oncogenic",
            "annotation_variant_oncokb_effect"=
                "annotation.variant.oncokb.effect",
            "annotation_variant_oncokb_treatments_code"=
                "annotation.variant.oncokb.treatments.code",
            "annotation_variant_oncokb_treatments_drug"=
                "annotation.variant.oncokb.treatments.drug"
        ),
        civic=c(
            "annotation_variant_civic_id"="annotation.variant.civic.id",
            "annotation_variant_civic_gene_id"=
                "annotation.variant.civic.gene_id",
            "annotation_variant_civic_variant"=
                "annotation.variant.civic.variant"
        ),
        qual=c(
            "metrics_qual"="identity.qual"
        ),
        sb=c(
            "metrics_sbp"="metrics.sbp",
            "metrics_srf"="metrics.srf",
            "metrics_srr"="metrics.srr",
            "metrics_saf"="metrics.saf",
            "metrics_sar"="metrics.sar",
            "genotypes_sb"="genotypes.sb",
            "genotypes_gq"="genotypes.gq"
        ),
        class=c(
            "annotation_classification_acmg"="annotation.classification.acmg",
            "annotation_classification_amp"="annotation.classification.amp",
            "annotation_classification_manual_sig"=
                "annotation.classification.manual_sig",
            "annotation_classification_manual_onc"=
                "annotation.classification.manual_onc",
            "annotation_classification_notes"="annotation.classification.notes"
        )        
    ))
}

.mongoMap <- function() {
    return(c(
        "identity_chr"="identity.chr",
        "identity_start"="identity.start",
        "identity_end"="identity.end",
        "identity_ref"="identity.ref",
        "identity_alt"="identity.alt",
        "identity_type"="identity.type",
        "identity_rsid"="identity.rsid",
        "identity_cosmic"="identity.cosmic",
        "metrics_qual"="identity.qual",
        "metrics_sbp"="metrics.sbp",
        "metrics_srf"="metrics.srf",
        "metrics_srr"="metrics.srr",
        "metrics_saf"="metrics.saf",
        "metrics_sar"="metrics.sar",
        "genotypes_gn"="genotypes.gn",
        "genotypes_gtn"="genotypes.gtn",
        "genotypes_dp"="genotypes.dp",
        "genotypes_ad_ref"="genotypes.ad_ref",
        "genotypes_ad_alt"="genotypes.ad_alt",
        "genotypes_af"="genotypes.af",
        "genotypes_gq"="genotypes.gq",
        "genotypes_sb"="genotypes.sb",
        "patho_sift_score"="pathogenicity.sift_score",
        "patho_sift_rankscore"="pathogenicity.sift_converted_score",
        "patho_sift_pred"="pathogenicity.sift_pred",
        "patho_sift4g_score"="pathogenicity.sift4g_score",
        "patho_sift4g_rankscore"="pathogenicity.sift4g_converted_rankscore",
        "patho_sift4g_pred"="pathogenicity.sift4g_pred",
        "patho_polyphen2_hdiv_score"="pathogenicity.polyphen2_hdiv_score",
        "patho_polyphen2_hdiv_rankscore"=
            "pathogenicity.polyphen2_hdiv_rankscore",
        "patho_polyphen2_hdiv_pred"="pathogenicity.polyphen2_hdiv_pred",
        "patho_polyphen2_hvar_score"="pathogenicity.polyphen2_hvar_score",
        "patho_polyphen2_hvar_rankscore"=
            "pathogenicity.polyphen2_hvar_rankscore",
        "patho_polyphen2_hvar_pred"="pathogenicity.polyphen2_hvar_pred",
        "patho_lrt_score"="pathogenicity.lrt_score",
        "patho_lrt_rankscore"="pathogenicity.lrt_converted_rankscore",
        "patho_lrt_pred"="pathogenicity.lrt_pred",
        "patho_mutationtaster_score"="pathogenicity.mutationtaster_score",
        "patho_mutationtaster_rankscore"=
            "pathogenicity.mutationtaster_converted_rankscore",
        "patho_mutationtaster_pred"="pathogenicity.mutationtaster_pred",
        "patho_mutationassessor_score"="pathogenicity.mutationassessor_score",
        "patho_mutationassessor_rankscore"=
            "pathogenicity.mutationassessor_rankscore",
        "patho_mutationassessor_pred"="pathogenicity.mutationassessor_pred",
        "patho_fathmm_score"="pathogenicity.fathmm_score",
        "patho_fathmm_rankscore"="pathogenicity.fathmm_converted_rankscore",
        "patho_fathmm_pred"="pathogenicity.fathmm_pred",
        "patho_provean_score"="pathogenicity.provean_score",
        "patho_provean_rankscore"="pathogenicity.provean_converted_rankscore",
        "patho_provean_pred"="pathogenicity.provean_pred",
        "patho_metasvm_score"="pathogenicity.metasvm_score",
        "patho_metasvm_rankscore"="pathogenicity.metasvm_rankscore",
        "patho_metasvm_pred"="pathogenicity.metasvm_pred",
        "patho_metalr_score"="pathogenicity.metalr_score",
        "patho_metalr_rankscore"="pathogenicity.metalr_rankscore",
        "patho_metalr_pred"="pathogenicity.metalr_pred",
        "patho_metarnn_score"="pathogenicity.metarnn_score",
        "patho_metarnn_rankscore"="pathogenicity.metarnn_rankscore",
        "patho_metarnn_pred"="pathogenicity.metarnn_pred",
        "patho_m_cap_score"="pathogenicity.m_cap_score",
        "patho_m_cap_rankscore"="pathogenicity.m_cap_rankscore",
        "patho_m_cap_pred"="pathogenicity.m_cap_pred",
        "patho_primateai_score"="pathogenicity.primateai_score",
        "patho_primateai_rankscore"="pathogenicity.primateai_rankscore",
        "patho_primateai_pred"="pathogenicity.primateai_pred",
        "patho_deogen2_score"="pathogenicity.deogen2_score",
        "patho_deogen2_rankscore"="pathogenicity.deogen2_rankscore",
        "patho_deogen2_pred"="pathogenicity.deogen2_pred",
        "patho_clinpred_score"="pathogenicity.clinpred_score",
        "patho_clinpred_rankscore"="pathogenicity.clinpred_rankscore",
        "patho_clinpred_pred"="pathogenicity.clinpred_pred",
        "patho_alphamissense_score"="pathogenicity.alphamissense_score",
        "patho_alphamissense_rankscore"="pathogenicity.alphamissense_rankscore",
        "patho_alphamissense_pred"="pathogenicity.alphamissense_pred",
        "patho_dann_score"="pathogenicity.dann_score",
        "patho_dann_rankscore"="pathogenicity.dann_rankscore",
        "patho_vest4_score"="pathogenicity.vest4_score",
        "patho_vest4_rankscore"="pathogenicity.vest4_rankscore",
        "patho_revel_score"="pathogenicity.revel_score",
        "patho_revel_rankscore"="pathogenicity.revel_rankscore",
        "cons_gerp_nr"="conservation.gerp___nr",
        "cons_gerp_rs"="conservation.gerp___rs",
        "cons_gerp_rs_ranscore"="conservation.gerp___rs_rankscore",
        "cons_phylop100way_vertebrate"="conservation.phylop100way_vertebrate",
        "cons_phylop100way_vertebrate_rankscore"=
            "conservation.phylop100way_vertebrate_rankscore",
        "cons_phylop470way_mammalian"="conservation.phylop470way_mammalian",
        "cons_phylop470way_mammalian_rankscore"=
            "conservation.phylop470way_mammalian_rankscore",
        "cons_phylop17way_primate"="conservation.phylop17way_primate",
        "cons_phylop17way_primate_rankscore"=
            "conservation.phylop17way_primate_rankscore",
        "cons_phastcons100way_vertebrate"="conservation.phastcons100way_vertebrate",
        "cons_phastcons100way_vertebrate_rankscore"=
            "conservation.phastcons100way_vertebrate_rankscore",
        "cons_phastcons470way_mammalian"="conservation.phastcons470way_mammalian",
        "cons_phastcons470way_mammalian_rankscore"=
            "conservation.phastcons470way_mammalian_rankscore",
        "cons_phastcons17way_primate"="conservation.phastcons17way_primate",
        "cons_phastcons17way_primate_rankscore"=
            "conservation.phastcons17way_primate_rankscore",
        "population_tgp_af"="population.tgp.af",
        "population_tgp_ac"="population.tgp_ac",
        "population_tgp_eur_af"="population.tgp.eur_af",
        "population_tgp_eur_ac"="population.tgp.eur_ac",
        "population_tgp_amr_af"="population.tgp.amr_af",
        "population_tgp_amr_ac"="population.tgp.amr_ac",
        "population_tgp_eas_af"="population.tgp.eas_af",
        "population_tgp_eas_ac"="population.tgp.eas_ac",
        "population_tgp_sas_af"="population.tgp.sas_af",
        "population_tgp_sas_ac"="population.tgp.sas_ac",
        "population_tgp_afr_af"="population.tgp.afr_af",
        "population_tgp_afr_ac"="population.tgp.afr_ac",
        "population_exac_af"="population.exac.af",
        "population_exac_ac"="population.exac.ac",
        "population_exac_adj_af"="population.exac.adj_af",
        "population_exac_adj_ac"="population.exac.adj_ac",
        "population_exac_nfe_af"="population.exac.nfe_af",
        "population_exac_nfe_ac"="population.exac.nfe_ac",
        "population_exac_fin_af"="population.exac.fin_af",
        "population_exac_fin_ac"="population.exac.fin_ac",
        "population_exac_amr_af"="population.exac.amr_af",
        "population_exac_amr_ac"="population.exac.amr_ac",
        "population_exac_eas_af"="population.exac.eas_af",
        "population_exac_eas_ac"="population.exac.eas_ac",
        "population_exac_sas_af"="population.exac.sas_af",
        "population_exac_sas_ac"="population.exac.sas_ac",
        "population_exac_afr_af"="population.exac.afr_af",
        "population_exac_afr_ac"="population.exac.afr_ac",
        "population_esp_aa_af"="population.esp.aa_af",
        "population_esp_aa_ac"="population.esp.aa_ac",
        "population_esp_ea_af"="population.esp.ea_af",
        "population_esp_ea_ac"="population.esp.ea_ac",
        "population_uk10k_af"="population.uk10k.af",
        "population_uk10k_ac"="population.uk10k.ac",
        "population_alfa_european_af"="population.alfa.european_af",
        "population_alfa_european_ac"="population.alfa.european_ac",
        "population_alfa_african_others_af"="population.alfa.african_others_af",
        "population_alfa_african_others_ac"="population.alfa.african_others_ac",
        "population_alfa_east_asian_af"="population.alfa.east_asian_af",
        "population_alfa_east_asian_ac"="population.alfa.east_asian_ac",
        "population_alfa_african_american_af"=
            "population.alfa.african_american_af",
        "population_alfa_african_american_ac"=
            "population.alfa.african_american_ac",
        "population_alfa_latin_american_1_af"=
            "population.alfa.latin_american_1_af",
        "population_alfa_latin_american_1_ac"=
            "population.alfa.latin_american_1_ac",
        "population_alfa_latin_american_2_af"=
            "population.alfa.latin_american_2_af",
        "population_alfa_latin_american_2_ac"=
            "population.alfa.latin_american_2_ac",
        "population_alfa_other_asian_af"="population.alfa.other_asian_af",
        "population_alfa_other_asian_ac"="population.alfa.other_asian_ac",
        "population_alfa_south_asian_af"="population.alfa.south_asian_af",
        "population_alfa_south_asian_ac"="population.alfa.south_asian_ac",
        "population_alfa_other_af"="population.alfa.other_af",
        "population_alfa_other_ac"="population.alfa.other_ac",
        "population_alfa_african_af"="population.alfa.african_af",
        "population_alfa_african_ac"="population.alfa.african_ac",
        "population_alfa_asian_af"="population.alfa.asian_af",
        "population_alfa_asian_ac"="population.alfa.asian_ac",
        "population_alfa_total_af"="population.alfa.total_af",
        "population_alfa_total_ac"="population.alfa.total_ac",
        "population_gnomad_exomes_af"="population.gnomad_exomes.af",
        "population_gnomad_exomes_ac"="population.gnomad_exomes.ac",
        "population_gnomad_exomes_afr_af"="population.gnomad_exomes.af_afr",
        "population_gnomad_exomes_afr_ac"="population.gnomad_exomes.ac_afr",
        "population_gnomad_exomes_amr_af"="population.gnomad_exomes.af_amr",
        "population_gnomad_exomes_amr_ac"="population.gnomad_exomes.ac_amr",
        "population_gnomad_exomes_asj_af"="population.gnomad_exomes.af_asj",
        "population_gnomad_exomes_asj_ac"="population.gnomad_exomes.ac_asj",
        "population_gnomad_exomes_eas_af"="population.gnomad_exomes.af_eas",
        "population_gnomad_exomes_eas_ac"="population.gnomad_exomes.ac_eas",
        "population_gnomad_exomes_fin_af"="population.gnomad_exomes.af_fin",
        "population_gnomad_exomes_fin_ac"="population.gnomad_exomes.ac_fin",
        "population_gnomad_exomes_mid_af"="population.gnomad_exomes.af_mid",
        "population_gnomad_exomes_mid_ac"="population.gnomad_exomes.ac_mid",
        "population_gnomad_exomes_nfe_af"="population.gnomad_exomes.af_nfe",
        "population_gnomad_exomes_nfe_ac"="population.gnomad_exomes.ac_nfe",
        "population_gnomad_exomes_sas_af"="population.gnomad_exomes.af_sas",
        "population_gnomad_exomes_sas_ac"="population.gnomad_exomes.ac_sas",
        "population_gnomad_genomes_af"="population.gnomad_genomes.af",
        "population_gnomad_genomes_ac"="population.gnomad_genomes.ac",
        "population_gnomad_genomes_afr_af"="population.gnomad_genomes.af_afr",
        "population_gnomad_genomes_afr_ac"="population.gnomad_genomes.ac_afr",
        "population_gnomad_genomes_amr_af"="population.gnomad_genomes.af_amr",
        "population_gnomad_genomes_amr_ac"="population.gnomad_genomes.ac_amr",
        "population_gnomad_genomes_asj_af"="population.gnomad_genomes.af_asj",
        "population_gnomad_genomes_asj_ac"="population.gnomad_genomes.ac_asj",
        "population_gnomad_genomes_eas_af"="population.gnomad_genomes.af_eas",
        "population_gnomad_genomes_eas_ac"="population.gnomad_genomes.ac_eas",
        "population_gnomad_genomes_fin_af"="population.gnomad_genomes.af_fin",
        "population_gnomad_genomes_fin_ac"="population.gnomad_genomes.ac_fin",
        "population_gnomad_genomes_nfe_af"="population.gnomad_genomes.af_nfe",
        "population_gnomad_genomes_nfe_ac"="population.gnomad_genomes.ac_nfe",
        "annotation_genes_name"="annotation.genes.name",
        "annotation_genes_ids"="annotation.genes.ids",
        "annotation_genes_omim"="annotation.genes.omim",
        "annotation_genes_omim_morbid"="annotation.genes.omim_morbid",
        "annotation_genes_cgd_condition"="annotation.genes.cgd.condition",
        "annotation_genes_cgd_inheritance"="annotation.genes.cgd.inheritance",
        "annotation_genes_disgenet_id"="annotation.genes.disgenet.id",
        "annotation_genes_disgenet_name"="annotation.genes.disgenet.name",
        "annotation_genes_hpo_id"="annotation.genes.hpo.id",
        "annotation_genes_hpo_name"="annotation.genes.hpo.name",
        "annotation_genes_ctd_id"="annotation.genes.ctd.id",
        "annotation_genes_ctd_name"="annotation.genes.ctd.name",
        "annotation_genes_transcripts_transcript_id"=
            "annotation.genes.transcripts.transcript_id",
        "annotation_genes_transcripts_impact_so"=   
            "annotation.genes.transcripts.impact_so",
        "annotation_genes_transcripts_impact_snpeff"=
            "annotation.genes.transcripts.impact_snpeff",
        "annotation_genes_transcripts_biotype"=
            "annotation.genes.transcripts.biotype",
        "annotation_genes_transcripts_exon_rank"=
            "annotation.genes.transcripts.exon_rank",
        "annotation_genes_transcripts_hgvs_c"=
            "annotation.genes.transcripts.hgvs_c",
        "annotation_genes_transcripts_hgvs_p"=
            "annotation.genes.transcripts.hgvs_p",
        "annotation_genes_transcripts_location"=
            "annotation.genes.transcripts.location",
        "annotation_genes_transcripts_cdna_rank"=
            "annotation.genes.transcripts.cdna_rank",
        "annotation_genes_transcripts_cds_rank"=
            "annotation.genes.transcripts.cds_rank",
        "annotation_genes_transcripts_aa_rank"=
            "annotation.genes.transcripts.aa_rank",
        "annotation_variant_disgenet_id"="annotation.variant.disgenet.id",
            "annotation_variant_disgenet_name"=
                "annotation.variant.disgenet.name",
        "annotation_variant_pharmgkb_id"=
            "annotation.variant.pharmgkb.pharmgkb_id",
        "annotation_variant_pharmgkb_loe"=
            "annotation.variant.pharmgkb.level_of_evidence",
        "annotation_variant_pharmgkb_phenotypes_id"=
            "annotation.variant.pharmgkb.phenotypes.id",
        "annotation_variant_pharmgkb_phenotypes_name"=
            "annotation.variant.pharmgkb.phenotypes.name",
        "annotation_variant_pharmgkb_chemicals_id"=
            "annotation.variant.pharmgkb.chemicals.pharmgkb_id",
        "annotation_variant_pharmgkb_chemicals_name"=
            "annotation.variant.pharmgkb.chemicals.chemical_name",
        "annotation_variant_clinvar_alleleid"="pathogenicity.clinvar_alleleid",
        "annotation_variant_clinvar_clnsig"="pathogenicity.clinvar_clnsig",
        "annotation_variant_clinvar_onc"="pathogenicity.clinvar_onc",
        "annotation_variant_oncokb_gene_symbol"=
            "annotation.variant.oncokb.gene_symbol",
        "annotation_variant_oncokb_alteration"=
            "annotation.variant.oncokb.alteration",
        "annotation_variant_oncokb_oncogenic"=
            "annotation.variant.oncokb.oncogenic",
        "annotation_variant_oncokb_effect"=
            "annotation.variant.oncokb.effect",
        "annotation_variant_oncokb_treatments_code"=
            "annotation.variant.oncokb.treatments.code",
        "annotation_variant_oncokb_treatments_drug"=
            "annotation.variant.oncokb.treatments.drug",
        "annotation_variant_civic_id"="annotation.variant.civic.id",
        "annotation_variant_civic_gene_id"="annotation.variant.civic.gene_id",
        "annotation_variant_civic_variant"="annotation.variant.civic.variant",
        "annotation_classification_acmg"="annotation.classification.acmg",
        "annotation_classification_amp"="annotation.classification.amp",
        "annotation_classification_manual_sig"=
            "annotation.classification.manual_sig",
        "annotation_classification_manual_onc"=
            "annotation.classification.manual_onc",
        "annotation_classification_notes"="annotation.classification.notes"
    ))
}

.mongoVariantFields <- function() {
    return(c(
        "identity_chr",
        "identity_start",
        "identity_end",
        "identity_ref",
        "identity_alt",
        "identity_rsid",
        "identity_cosmic",
        "identity_type",
        "metrics_qual",
        "metrics_sbp", # strand bias
        "metrics_srf", # #reference allele - forward strand
        "metrics_srr", # #reference allele - reverse strand
        "metrics_saf", # #alternative allele - forward strand
        "metrics_sar", # #alternative allele - reverse strand
        "genotypes_gn",
        "genotypes_gtn",
        "genotypes_dp",
        "genotypes_ad_ref",
        "genotypes_ad_alt",
        "genotypes_af",
        "genotypes_gq",
        "genotypes_sb",
        "patho_sift_score",
        "patho_sift_rankscore",
        "patho_sift_pred",
        "patho_sift4g_score",
        "patho_sift4g_rankscore",
        "patho_sift4g_pred",
        "patho_polyphen2_hdiv_score",
        "patho_polyphen2_hdiv_rankscore",
        "patho_polyphen2_hdiv_pred",
        "patho_polyphen2_hvar_score",
        "patho_polyphen2_hvar_rankscore",
        "patho_polyphen2_hvar_pred",
        "patho_lrt_score",
        "patho_lrt_rankscore",
        "patho_lrt_pred",
        "patho_mutationtaster_score",
        "patho_mutationtaster_rankscore",
        "patho_mutationtaster_pred",
        "patho_mutationassessor_score",
        "patho_mutationassessor_rankscore",
        "patho_mutationassessor_pred",
        "patho_fathmm_score",
        "patho_fathmm_rankscore",
        "patho_fathmm_pred",
        "patho_provean_score",
        "patho_provean_rankscore",
        "patho_provean_pred",
        "patho_metasvm_score",
        "patho_metasvm_rankscore",
        "patho_metasvm_pred",
        "patho_metalr_score",
        "patho_metalr_rankscore",
        "patho_metalr_pred",
        "patho_metarnn_score",
        "patho_metarnn_rankscore",
        "patho_metarnn_pred",
        "patho_m_cap_score",
        "patho_m_cap_rankscore",
        "patho_m_cap_pred",
        "patho_primateai_score",
        "patho_primateai_rankscore",
        "patho_primateai_pred",
        "patho_deogen2_score",
        "patho_deogen2_rankscore",
        "patho_deogen2_pred",
        "patho_clinpred_score",
        "patho_clinpred_rankscore",
        "patho_clinpred_pred",
        "patho_alphamissense_score",
        "patho_alphamissense_rankscore",
        "patho_alphamissense_pred",
        "patho_dann_score",
        "patho_dann_rankscore",
        "patho_vest4_score",
        "patho_vest4_rankscore",
        "patho_revel_score",
        "patho_revel_rankscore",
        "cons_gerp_nr",
        "cons_gerp_rs",
        "cons_gerp_rs_ranscore",
        "cons_phylop100way_vertebrate",
        "cons_phylop100way_vertebrate_rankscore",
        "cons_phylop470way_mammalian",
        "cons_phylop470way_mammalian_rankscore",
        "cons_phylop17way_primate",
        "cons_phylop17way_primate_rankscore",
        "cons_phastcons100way_vertebrate",
        "cons_phastcons100way_vertebrate_rankscore",
        "cons_phastcons470way_mammalian",
        "cons_phastcons470way_mammalian_rankscore",
        "cons_phastcons17way_primate",
        "cons_phastcons17way_primate_rankscore",
        "population_tgp_af",
        "population_tgp_ac",
        "population_tgp_eur_af",
        "population_tgp_eur_ac",
        "population_tgp_amr_af",
        "population_tgp_amr_ac",
        "population_tgp_eas_af",
        "population_tgp_eas_ac",
        "population_tgp_sas_af",
        "population_tgp_sas_ac",
        "population_tgp_afr_af",
        "population_tgp_afr_ac",
        "population_exac_af",
        "population_exac_ac",
        "population_exac_adj_af",
        "population_exac_adj_ac",
        "population_exac_nfe_af",
        "population_exac_nfe_ac",
        "population_exac_fin_af",
        "population_exac_fin_ac",
        "population_exac_amr_af",
        "population_exac_amr_ac",
        "population_exac_eas_af",
        "population_exac_eas_ac",
        "population_exac_sas_af",
        "population_exac_sas_ac",
        "population_exac_afr_af",
        "population_exac_afr_ac",
        "population_esp_aa_af",
        "population_esp_aa_ac",
        "population_esp_ea_af",
        "population_esp_ea_ac",
        "population_uk10k_af",
        "population_uk10k_ac",
        "population_alfa_european_af",
        "population_alfa_european_ac",
        "population_alfa_african_others_af",
        "population_alfa_african_others_ac",
        "population_alfa_east_asian_af",
        "population_alfa_east_asian_ac",
        "population_alfa_african_american_af",
        "population_alfa_african_american_ac",
        "population_alfa_latin_american_1_af",
        "population_alfa_latin_american_1_ac",
        "population_alfa_latin_american_2_af",
        "population_alfa_latin_american_2_ac",
        "population_alfa_other_asian_af",
        "population_alfa_other_asian_ac",
        "population_alfa_south_asian_af",
        "population_alfa_south_asian_ac",
        "population_alfa_other_af",
        "population_alfa_other_ac",
        "population_alfa_african_af",
        "population_alfa_african_ac",
        "population_alfa_asian_af",
        "population_alfa_asian_ac",
        "population_alfa_total_af",
        "population_alfa_total_ac",
        "population_gnomad_exomes_af",
        "population_gnomad_exomes_ac",
        "population_gnomad_exomes_afr_af", # be careful to replace afr_af with af_afr!
        "population_gnomad_exomes_afr_ac",
        "population_gnomad_exomes_amr_af",
        "population_gnomad_exomes_amr_ac",
        "population_gnomad_exomes_asj_af",
        "population_gnomad_exomes_asj_ac",
        "population_gnomad_exomes_eas_af",
        "population_gnomad_exomes_eas_ac",
        "population_gnomad_exomes_fin_af",
        "population_gnomad_exomes_fin_ac",
        "population_gnomad_exomes_mid_af",
        "population_gnomad_exomes_mid_ac",
        "population_gnomad_exomes_nfe_af",
        "population_gnomad_exomes_nfe_ac",
        "population_gnomad_exomes_sas_af",
        "population_gnomad_exomes_sas_ac",
        "population_gnomad_genomes_af",
        "population_gnomad_genomes_ac",
        "population_gnomad_genomes_afr_af", # be careful to replace afr_af with af_afr!
        "population_gnomad_genomes_afr_ac",
        "population_gnomad_genomes_amr_af",
        "population_gnomad_genomes_amr_ac",
        "population_gnomad_genomes_asj_af",
        "population_gnomad_genomes_asj_ac",
        "population_gnomad_genomes_eas_af",
        "population_gnomad_genomes_eas_ac",
        "population_gnomad_genomes_fin_af",
        "population_gnomad_genomes_fin_ac",
        "population_gnomad_genomes_nfe_af",
        "population_gnomad_genomes_nfe_ac",
        "annotation_genes_name",
        "annotation_genes_ids",
        "annotation_genes_omim",
        "annotation_genes_omim_morbid",
        "annotation_genes_cgd_condition",
        "annotation_genes_cgd_inheritance",
        "annotation_genes_disgenet_id",
        "annotation_genes_disgenet_name",
        "annotation_genes_hpo_id",
        "annotation_genes_hpo_name",
        "annotation_genes_ctd_id",
        "annotation_genes_ctd_name",
        "annotation_genes_transcripts_transcript_id",
        "annotation_genes_transcripts_impact_so",
        "annotation_genes_transcripts_impact_snpeff",
        "annotation_genes_transcripts_biotype",
        "annotation_genes_transcripts_exon_rank",
        "annotation_genes_transcripts_hgvs_c",
        "annotation_genes_transcripts_hgvs_p",
        "annotation_genes_transcripts_location",
        "annotation_genes_transcripts_cdna_rank",
        "annotation_genes_transcripts_cds_rank",
        "annotation_genes_transcripts_aa_rank",
        "annotation_variant_disgenet_id",
        "annotation_variant_disgenet_name",
        "annotation_variant_pharmgkb_id",
        "annotation_variant_pharmgkb_loe",
        "annotation_variant_pharmgkb_phenotypes_id",
        "annotation_variant_pharmgkb_phenotypes_name",
        "annotation_variant_pharmgkb_chemicals_id",
        "annotation_variant_pharmgkb_chemicals_name",
        "annotation_variant_clinvar_alleleid",
        "annotation_variant_clinvar_clnsig",
        "annotation_variant_clinvar_onc",
        "annotation_variant_oncokb_gene_symbol",
        "annotation_variant_oncokb_alteration",
        "annotation_variant_oncokb_oncogenic",
        "annotation_variant_oncokb_effect",
        "annotation_variant_oncokb_treatments_code",
        "annotation_variant_oncokb_treatments_drug",
        "annotation_variant_civic_id",
        "annotation_variant_civic_gene_id",
        "annotation_variant_civic_variant",
        "annotation_classification_acmg",
        "annotation_classification_amp",
        "annotation_classification_manual_sig",
        "annotation_classification_manual_onc",
        "annotation_classification_notes"
    ))
}

.availableMongoVcfHeaders <- function() {
    return(list(
        identity_type=DataFrame(
            Number="1",
            Type="String",
            Description="Variant type",
            row.names="TYPE"
        ),
        identity_cosmic=DataFrame(
            Number="1",
            Type="String",
            Description="COSMIC accession",
            row.names="COSMIC"
        ),
        metrics_sbp=DataFrame(
            Number="A",
            Type="Float",
            Description="Phred-scaled p-value of Strand bias",
            row.names="SBP"
        ),
        metrics_srf=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("Number of reference observations on ",
                "the forward strand"),
            row.names="SRF"
        ),
        metrics_srr=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("Number of reference observations on ",
                "the reverse strand"),
            row.names="SRR"
        ),
        metrics_saf=DataFrame(
            Number="A",
            Type="Integer",
            Description=paste0("Alternate allele observations on the ",
                "forward strand"),
            row.names="SAF"
        ),
        metrics_sar=DataFrame(
            Number="A",
            Type="Integer",
            Description=paste0("Alternate allele observations on the ",
                "reverse strand"),
            row.names="SAR"
        ),
        genotypes_gn=DataFrame(
            Number="1",
            Type="String",
            Description="Genotype - VCF notation",
            row.names="GT"
        ),
        genotypes_gtn=DataFrame(
            Number="1",
            Type="String",
            Description="Genotype - alleles",
            row.names="GTN"
        ),
        genotypes_dp=DataFrame(
            Number="1",
            Type="Integer",
            Description="Read depth",
            row.names="DP"
        ),
        genotypes_ad_ref=DataFrame(
            Number="1",
            Type="Integer",
            Description="Reference allele depth",
            row.names="ADREF"
        ),
        genotypes_ad_alt=DataFrame(
            Number="1",
            Type="Integer",
            Description="Alternative allele depth",
            row.names="ADALT"
        ),
        genotypes_af=DataFrame(
            Number="A",
            Type="Float",
            Description="Reference allele depth",
            row.names="AF"
        ),
        genotypes_gq=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("Genotype Quality, the Phred-scaled ",
                "marginal (or unconditional) probability of the called ",
                "genotype"),
            row.names="GQ"
        ),
        genotypes_sb=DataFrame(
            Number="1",
            Type="Float",
            Description="Strand bias",
            row.names="SB"
        ),
        patho_sift_score=DataFrame(
            Number="1",
            Type="Float",
            Description="SIFT pathogenicity score from dbNSFP",
            row.names="SIFT_SCORE"
        ),
        patho_sift_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="SIFT pathogenicity rankscore from dbNSFP",
            row.names="SIFT_RANKSCORE"
        ),
        patho_sift_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="SIFT pathogenicity prediction from dbNSFP",
            row.names="SIFT_PRED"
        ),
        patho_sift4g_score=DataFrame(
            Number="1",
            Type="Float",
            Description="SIFT4G pathogenicity score from dbNSFP",
            row.names="SIFT4G_SCORE"
        ),
        patho_sift4g_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="SIFT4G pathogenicity rankscore from dbNSFP",
            row.names="SIFT4G_RANKSCORE"
        ),
        patho_sift4g_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="SIFT4G pathogenicity prediction from dbNSFP",
            row.names="SIFT4G_PRED"
        ),
        patho_polyphen2_hdiv_score=DataFrame(
            Number="1",
            Type="Float",
            Description="Polyphen2 Hdiv pathogenicity score from dbNSFP",
            row.names="POLYPHEN2_HDIV_SCORE"
        ),
        patho_polyphen2_hdiv_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="Polyphen2 Hdiv pathogenicity rankscore from dbNSFP",
            row.names="POLYPHEN2_HDIV_RANKSCORE"
        ),
        patho_polyphen2_hdiv_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="Polyphen2 Hdiv pathogenicity prediction from dbNSFP",
            row.names="POLYPHEN2_HDIV_PRED"
        ),
        patho_polyphen2_hvar_score=DataFrame(
            Number="1",
            Type="Float",
            Description="Polyphen2 Hvar pathogenicity score from dbNSFP",
            row.names="POLYPHEN2_HVAR_SCORE"
        ),
        patho_polyphen2_hvar_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="Polyphen2 Hvar pathogenicity rankscore from dbNSFP",
            row.names="POLYPHEN2_HVAR_RANKSCORE"
        ),
        patho_polyphen2_hvar_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="Polyphen2 Hvar pathogenicity prediction from dbNSFP",
            row.names="POLYPHEN2_HVAR_PRED"
        ),
        patho_lrt_score=DataFrame(
            Number="1",
            Type="Float",
            Description="LRT pathogenicity score from dbNSFP",
            row.names="LRT_SCORE"
        ),
        patho_lrt_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="LRT pathogenicity rankscore from dbNSFP",
            row.names="LRT_RANKSCORE"
        ),
        patho_lrt_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="LRT pathogenicity prediction from dbNSFP",
            row.names="LRT_PRED"
        ),
        patho_mutationtaster_score=DataFrame(
            Number="1",
            Type="Float",
            Description="MutationTaster pathogenicity score from dbNSFP",
            row.names="MUTATIONTASTER_SCORE"
        ),
        patho_mutationtaster_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="MutationTaster pathogenicity rankscore from dbNSFP",
            row.names="MUTATIONTASTER_RANKSCORE"
        ),
        patho_mutationtaster_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="MutationTaster pathogenicity prediction from dbNSFP",
            row.names="MUTATIONTASTER_PRED"
        ),
        patho_mutationassessor_score=DataFrame(
            Number="1",
            Type="Float",
            Description="MutationAssessor pathogenicity score from dbNSFP",
            row.names="MUTATIONASSESSOR_SCORE"
        ),
        patho_mutationassessor_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="MutationAssessor pathogenicity rankscore from dbNSFP",
            row.names="MUTATIONASSESSOR_RANKSCORE"
        ),
        patho_mutationassessor_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="MutationAssessor prediction score from dbNSFP",
            row.names="MUTATIONASSESSOR_PRED"
        ),
        patho_fathmm_score=DataFrame(
            Number="1",
            Type="Float",
            Description="FATHMM pathogenicity score from dbNSFP",
            row.names="FATHMM_SCORE"
        ),
        patho_fathmm_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="FATHMM pathogenicity rankscore from dbNSFP",
            row.names="FATHMM_RANKSCORE"
        ),
        patho_fathmm_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="FATHMM pathogenicity prediction from dbNSFP",
            row.names="FATHMM_PRED"
        ),
        patho_provean_score=DataFrame(
            Number="1",
            Type="Float",
            Description="PROVEAN pathogenicity score from dbNSFP",
            row.names="PROVEAN_SCORE"
        ),
        patho_provean_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="PROVEAN pathogenicity rankscore from dbNSFP",
            row.names="PROVEAN_RANKSCORE"
        ),
        patho_provean_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="PROVEAN pathogenicity prediction from dbNSFP",
            row.names="PROVEAN_PRED"
        ),
        patho_metasvm_score=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaSVM pathogenicity score from dbNSFP",
            row.names="METASVM_SCORE"
        ),
        patho_metasvm_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaSVM pathogenicity rankscore from dbNSFP",
            row.names="METASVM_RANKSCORE"
        ),
        patho_metasvm_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="MetaSVM pathogenicity prediction from dbNSFP",
            row.names="METASVM_PRED"
        ),
        patho_metalr_score=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaLR pathogenicity score from dbNSFP",
            row.names="METALR_SCORE"
        ),
        patho_metalr_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaLR pathogenicity rankscore from dbNSFP",
            row.names="METALR_RANKSCORE"
        ),
        patho_metalr_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="MetaLR pathogenicity prediction from dbNSFP",
            row.names="METALR_PRED"
        ),
        patho_metarnn_score=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaRNN pathogenicity score from dbNSFP",
            row.names="METARNN_SCORE"
        ),
        patho_metarnn_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaRNN pathogenicity score from dbNSFP",
            row.names="METARNN_SCORE"
        ),
        patho_metarnn_pred=DataFrame(
            Number="1",
            Type="Float",
            Description="MetaLR pathogenicity score from dbNSFP",
            row.names="METALR_SCORE"
        ),
        patho_m_cap_score=DataFrame(
            Number="1",
            Type="Float",
            Description="M-CAP pathogenicity score from dbNSFP",
            row.names="MCAP_SCORE"
        ),
        patho_m_cap_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="M-CAP pathogenicity rankscore from dbNSFP",
            row.names="MCAP_RANKSCORE"
        ),
        patho_m_cap_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="M-CAP pathogenicity prediction from dbNSFP",
            row.names="MCAP_PRED"
        ),
        patho_primateai_score=DataFrame(
            Number="1",
            Type="Float",
            Description="PrimateAI pathogenicity score from dbNSFP",
            row.names="PRIMATEAI_SCORE"
        ),
        patho_primateai_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="PrimateAI pathogenicity rankscore from dbNSFP",
            row.names="PRIMATEAI_RANKSCORE"
        ),
        patho_primateai_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="PrimateAI pathogenicity prediction from dbNSFP",
            row.names="PRIMATEAI_PRED"
        ),
        patho_deogen2_score=DataFrame(
            Number="1",
            Type="Float",
            Description="DEOGEN2 pathogenicity score from dbNSFP",
            row.names="DEOGEN2_SCORE"
        ),
        patho_deogen2_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="DEOGEN2 pathogenicity rankscore from dbNSFP",
            row.names="DEOGEN2_RANKSCORE"
        ),
        patho_deogen2_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="DEOGEN2 pathogenicity prediction from dbNSFP",
            row.names="DEOGEN2_PRED"
        ),
        patho_clinpred_score=DataFrame(
            Number="1",
            Type="Float",
            Description="ClinPred pathogenicity score from dbNSFP",
            row.names="CLINPRED_SCORE"
        ),
        patho_clinpred_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="ClinPred pathogenicity rankscore from dbNSFP",
            row.names="CLINPRED_SCORE"
        ),
        patho_clinpred_pred=DataFrame(
            Number="1",
            Type="Character",
            Description="ClinPred pathogenicity prediction from dbNSFP",
            row.names="CLINPRED_SCORE"
        ),
        patho_alphamissense_score=DataFrame(
            Number="1",
            Type="Float",
            Description="AlphaMissense pathogenicity score from dbNSFP",
            row.names="ALPHAMISSENSE_SCORE"
        ),
        patho_alphamissense_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="AlphaMissense pathogenicity rankscore from dbNSFP",
            row.names="ALPHAMISSENSE_RANKSCORE"
        ),
        patho_alphamissense_pred=DataFrame(
            Number="1",
            Type="Float",
            Description="AlphaMissense pathogenicity prediction from dbNSFP",
            row.names="ALPHAMISSENSE_PRED"
        ),
        patho_dann_score=DataFrame(
            Number="1",
            Type="Float",
            Description="DANN pathogenicity score from dbNSFP",
            row.names="DANN_SCORE"
        ),
        patho_dann_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="DANN pathogenicity rankscore from dbNSFP",
            row.names="DANN_RANKSCORE"
        ),
        patho_vest4_score=DataFrame(
            Number="1",
            Type="Float",
            Description="VEST4 pathogenicity score from dbNSFP",
            row.names="VEST4_SCORE"
        ),
        patho_vest4_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="VEST4 pathogenicity rankscore from dbNSFP",
            row.names="VEST4_RANKSCORE"
        ),
        patho_revel_score=DataFrame(
            Number="1",
            Type="Float",
            Description="REVEL pathogenicity score from dbNSFP",
            row.names="REVEL_SCORE"
        ),
        patho_revel_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description="REVEL pathogenicity rankscore from dbNSFP",
            row.names="REVEL_RANKSCORE"
        ),
        cons_gerp_nr=DataFrame(
            Number="1",
            Type="Float",
            Description="GERP++ NR conservation score from dbNSFP",
            row.names="GERPNR_SCORE"
        ),
        cons_gerp_rs=DataFrame(
            Number="1",
            Type="Float",
            Description="GERP++ RS conservation score from dbNSFP",
            row.names="GERPRS_SCORE"
        ),
        cons_gerp_rs_ranscore=DataFrame(
            Number="1",
            Type="Float",
            Description="GERP++ RS conservation rankscore from dbNSFP",
            row.names="GERPRS_RANKSCORE"
        ),
        cons_phylop100way_vertebrate=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 100way vertebrate conservation score ",
                "from dbNSFP"),
            row.names="PHYLOP100_SCORE"
        ),
        cons_phylop100way_vertebrate_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 100way vertebrate conservation ",
                "rankscore from dbNSFP"),
            row.names="PHYLOP100_RANKSCORE"
        ),
        cons_phylop470way_mammalian=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 470way mammalian conservation score ",
                "from dbNSFP"),
            row.names="PHYLOP470_SCORE"
        ),
        cons_phylop470way_mammalian_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 470way mammalian conservation ",
                "rankscore from dbNSFP"),
            row.names="PHYLOP470_RANKSCORE"
        ),
        cons_phylop17way_primate=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 17way primate conservation score ",
                "from dbNSFP"),
            row.names="PHYLOP17_SCORE"
        ),
        cons_phylop17way_primate_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhyloP 17way primate conservation rankscore ",
                "from dbNSFP"),
            row.names="PHYLOP17_RANKSCORE"
        ),
        cons_phastcons100way_vertebrate=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 100way vertebrate conservation ",
                "score from dbNSFP"),
            row.names="PHASTCONS100_SCORE"
        ),
        cons_phastcons100way_vertebrate_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 100way vertebrate conservation ",
                "rankscore from dbNSFP"),
            row.names="PHASTCONS100_RANKSCORE"
        ),
        cons_phastcons470way_mammalian=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 470way mammalian conservation score ",
                "from dbNSFP"),
            row.names="PHASTCONS470_SCORE"
        ),
        cons_phastcons470way_mammalian_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 470way vertebrate conservation ",
                "rankscore from dbNSFP"),
            row.names="PHASTCONS470_RANKSCORE"
        ),
        cons_phastcons17way_primate=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 17way primate conservation score ",
                "from dbNSFP"),
            row.names="PHASTCONS17_SCORE"
        ),
        cons_phastcons17way_primate_rankscore=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("PhastCons 17way vertebrate conservation ",
                "rankscore from dbNSFP"),
            row.names="PHASTCONS17_RANKSCORE"
        ),
        population_tgp_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes total alternative allele ",
                "frequency from dbNSFP"),
            row.names="TGP_AF"
        ),
        population_tgp_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes total alternative allele counts ",
                "from dbNSFP"),
            row.names="TGP_AC"
        ),
        population_tgp_eur_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes alternative allele frequency for ",
                "for Europan ancestry from dbNSFP"),
            row.names="TGP_EUR_AF"
        ),
        population_tgp_eur_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes alternative allele counts for ",
                "Europan ancestry from dbNSFP"),
            row.names="TGP_EUR_AC"
        ),
        population_tgp_amr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes alternative allele frequency for ",
                "American ancestry from dbNSFP"),
            row.names="TGP_AMR_AF"
        ),
        population_tgp_amr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes alternative allele counts for ",
                "American ancestry from dbNSFP"),
            row.names="TGP_AMR_AC"
        ),
        population_tgp_eas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes alternative allele frequency for ",
                "East Asian ancestry from dbNSFP"),
            row.names="TGP_EAS_AF"
        ),
        population_tgp_eas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes alternative allele counts for ",
                "East Asian ancestry from dbNSFP"),
            row.names="TGP_EAS_AC"
        ),
        population_tgp_sas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes alternative allele frequency for ",
                "South Asian ancestry from dbNSFP"),
            row.names="TGP_SAS_AF"
        ),
        population_tgp_sas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes alternative allele counts for ",
                "South Asian ancestry from dbNSFP"),
            row.names="TGP_SAS_AC"
        ),
        population_tgp_afr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("1000 genomes alternative allele frequency for ",
                "African ancestry from dbNSFP"),
            row.names="TGP_AFR_AF"
        ),
        population_tgp_afr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("1000 genomes alternative allele counts for ",
                "African ancestry from dbNSFP"),
            row.names="TGP_AFR_AC"
        ),
        population_exac_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC total alternative allele frequency from ",
                "dbNSFP"),
            row.names="EXAC_AF"
        ),
        population_exac_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC total alternative allele counts from ",
                "dbNSFP"),
            row.names="EXAC_AF"
        ),
        population_exac_adj_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC adjusted total alternative allele ",
                "frequency from dbNSFP"),
            row.names="EXAC_ADJAF"
        ),
        population_exac_adj_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC adjusted total alternative allele ",
                "counts from dbNSFP"),
            row.names="EXAC_ADJAC"
        ),
        population_exac_nfe_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for ",
                "Non-Finnish European ancestry from dbNSFP"),
            row.names="EXAC_NFE_AF"
        ),
        population_exac_nfe_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for ",
                "Non-Finnish European ancestry from dbNSFP"),
            row.names="EXAC_NFE_AC"
        ),
        population_exac_fin_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for Finnish ",
                "ancestry from dbNSFP"),
            row.names="EXAC_FIN_AF"
        ),
        population_exac_fin_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for Finnish ",
                "ancestry from dbNSFP"),
            row.names="EXAC_FIN_AC"
        ),
        population_exac_amr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for ",
                "American ancestry from dbNSFP"),
            row.names="EXAC_AMR_AF"
        ),
        population_exac_amr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for American ",
                "ancestry from dbNSFP"),
            row.names="EXAC_AMR_AC"
        ),
        population_exac_eas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for East ",
                "Asian ancestry from dbNSFP"),
            row.names="EXAC_EAS_AF"
        ),
        population_exac_eas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for East Asian ",
                "ancestry from dbNSFP"),
            row.names="EXAC_EAS_AC"
        ),
        population_exac_sas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for South ",
                "Asian ancestry from dbNSFP"),
            row.names="EXAC_SAS_AF"
        ),
        population_exac_sas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for South Asian",
                "ancestry from dbNSFP"),
            row.names="EXAC_SAS_AC"
        ),
        population_exac_afr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ExAC alternative allele frequency for African ",
                "ancestry from dbNSFP"),
            row.names="EXAC_AFR_AF"
        ),
        population_exac_afr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ExAC alternative allele counts for African ",
                "ancestry from dbNSFP"),
            row.names="EXAC_AFR_AC"
        ),
        population_esp_aa_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ESP6500 alternative allele frequency for ",
                "African American ancestry from dbNSFP"),
            row.names="ESP_AA_AF"
        ),
        population_esp_aa_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ESP6500 alternative allele counts for African ",
                "American ancestry from dbNSFP"),
            row.names="ESP_AA_AC"
        ),
        population_esp_ea_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ESP6500 alternative allele frequency for ",
                "European American ancestry from dbNSFP"),
            row.names="ESP_EA_AF"
        ),
        population_esp_ea_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ESP6500 alternative allele counts for ",
                "European American ancestry from dbNSFP"),
            row.names="ESP_EA_AC"
        ),
        population_uk10k_af=DataFrame(
            Number="1",
            Type="Float",
            Description="UK10k alternative allele frequency from dbNSFP",
            row.names="UK10K_AF"
        ),
        population_uk10k_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description="UK10k alternative allele counts from dbNSFP",
            row.names="UK10K_AC"
        ),
        population_alfa_european_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for ",
                "European ancestry"),
            row.names="ALFA_EUROPEAN_AF"
        ),
        population_alfa_european_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for European ",
                "ancestry"),
            row.names="ALFA_EUROPEAN_AC"
        ),
        population_alfa_african_others_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for ",
                "African Others ancestry"),
            row.names="ALFA_AFRICAN_OTHERS_AF"
        ),
        population_alfa_african_others_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for African ",
                "Others ancestry"),
            row.names="ALFA_AFRICAN_OTHERS_AC"
        ),
        population_alfa_east_asian_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for East ",
                "Asian ancestry"),
            row.names="ALFA_EAST_ASIAN_AF"
        ),
        population_alfa_east_asian_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for East Asian ",
                "ancestry"),
            row.names="ALFA_EAST_ASIAN_AC"
        ),
        population_alfa_african_american_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for ",
                "African American ancestry"),
            row.names="ALFA_AFRICAN_AMERICAN_AF"
        ),
        population_alfa_african_american_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for African ",
                "American ancestry"),
            row.names="ALFA_AFRICAN_AMERICAN_AC"
        ),
        population_alfa_latin_american_1_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for Latin ",
                "American 1 ancestry"),
            row.names="ALFA_LATIN_AMERICAN_1_AF"
        ),
        population_alfa_latin_american_1_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for Latin ",
                "American 1 ancestry"),
            row.names="ALFA_LATIN_AMERICAN_1_AC"
        ),
        population_alfa_latin_american_2_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for Latin ",
                "American 2 ancestry"),
            row.names="ALFA_LATIN_AMERICAN_2_AF"
        ),
        population_alfa_latin_american_2_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for Latin ",
                "American 2 ancestry"),
            row.names="ALFA_LATIN_AMERICAN_2_AC"
        ),
        population_alfa_other_asian_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for Other ",
                "Asian ancestry"),
            row.names="ALFA_OTHER_ASIAN_AF"
        ),
        population_alfa_other_asian_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for Other ",
                "Asian ancestry"),
            row.names="ALFA_OTHER_ASIAN_AC"
        ),
        population_alfa_south_asian_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for South ",
                "Asian ancestry"),
            row.names="ALFA_SOUTH_ASIAN_AF"
        ),
        population_alfa_south_asian_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for South ",
                "Asian ancestry"),
            row.names="ALFA_SOUTH_ASIAN_AC"
        ),
        population_alfa_other_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for Other ",
                "ancestry"),
            row.names="ALFA_OTHER_AF"
        ),
        population_alfa_other_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for Other ",
                "ancestry"),
            row.names="ALFA_OTHER_AC"
        ),
        population_alfa_african_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for African ",
                "ancestry"),
            row.names="ALFA_AFRICAN_AF"
        ),
        population_alfa_african_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("ALFA alternative allele counts for African ",
                "ancestry"),
            row.names="ALFA_AFRICAN_AC"
        ),
        population_alfa_asian_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele frequency for Asian ",
                "ancestry"),
            row.names="ALFA_ASIAN_AF"
        ),
        population_alfa_asian_ac=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("ALFA alternative allele counts for Asian ",
                "ancestry"),
            row.names="ALFA_ASIAN_AC"
        ),
        population_alfa_total_af=DataFrame(
            Number="1",
            Type="Float",
            Description="ALFA total alternative allele frequency",
            row.names="ALFA_TOTAL_AF"
        ),
        population_alfa_total_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description="ALFA total alternative allele counts",
            row.names="ALFA_TOTAL_AC"
        ),
        population_gnomad_exomes_af=DataFrame(
            Number="1",
            Type="Float",
            Description="gnomAD exomes total alternative allele frequency",
            row.names="GNOMAD_EXOMES_TOTAL_AF"
        ),
        population_gnomad_exomes_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description="gnomAD exomes total alternative allele counts",
            row.names="GNOMAD_EXOMES_TOTAL_AC"
        ),
        population_gnomad_exomes_afr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for African ancestry"),
            row.names="GNOMAD_EXOMES_AFR_AF"
        ),
        population_gnomad_exomes_afr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for African ancestry"),
            row.names="GNOMAD_EXOMES_AFR_AC"
        ),
        population_gnomad_exomes_amr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for American ancestry"),
            row.names="GNOMAD_EXOMES_AMR_AF"
        ),
        population_gnomad_exomes_amr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for American ancestry"),
            row.names="GNOMAD_EXOMES_AMR_AC"
        ),
        population_gnomad_exomes_asj_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for Askenazim ancestry"),
            row.names="GNOMAD_EXOMES_ASJ_AF"
        ),
        population_gnomad_exomes_asj_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for Askenazim ancestry"),
            row.names="GNOMAD_EXOMES_ASJ_AC"
        ),
        population_gnomad_exomes_eas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for East Asian ancestry"),
            row.names="GNOMAD_EXOMES_EAS_AF"
        ),
        population_gnomad_exomes_eas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for East Asian ancestry"),
            row.names="GNOMAD_EXOMES_EAS_AC"
        ),
        population_gnomad_exomes_fin_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for Finnish ancestry"),
            row.names="GNOMAD_EXOMES_FIN_AF"
        ),
        population_gnomad_exomes_fin_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for Finnish ancestry"),
            row.names="GNOMAD_EXOMES_FIN_AC"
        ),
        population_gnomad_exomes_mid_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for Middle East ancestry"),
            row.names="GNOMAD_EXOMES_MID_AF"
        ),
        population_gnomad_exomes_mid_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for Middle East ancestry"),
            row.names="GNOMAD_EXOMES_MID_AC"
        ),
        population_gnomad_exomes_nfe_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for Non-Finnish European ancestry"),
            row.names="GNOMAD_EXOMES_NFE_AF"
        ),
        population_gnomad_exomes_nfe_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for Non-Finnish European ancestry"),
            row.names="GNOMAD_EXOMES_NFE_AC"
        ),
        population_gnomad_exomes_sas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD exomes total alternative allele ",
                "frequency for South Asian ancestry"),
            row.names="GNOMAD_EXOMES_SAS_AF"
        ),
        population_gnomad_exomes_sas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD exomes total alternative allele counts ",
                "for South Asian ancestry"),
            row.names="GNOMAD_EXOMES_SAS_AC"
        ),
        population_gnomad_genomes_af=DataFrame(
            Number="1",
            Type="Float",
            Description="gnomAD genomes total alternative allele frequency",
            row.names="GNOMAD_GENOMES_TOTAL_AF"
        ),
        population_gnomad_genomes_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description="gnomAD genomes total alternative allele counts",
            row.names="GNOMAD_GENOMES_TOTAL_AC"
        ),
        population_gnomad_genomes_afr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for African ancestry"),
            row.names="GNOMAD_GENOMES_AFR_AF"
        ),
        population_gnomad_genomes_afr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for African ancestry"),
            row.names="GNOMAD_GENOMES_AFR_AC"
        ),
        population_gnomad_genomes_amr_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for American ancestry"),
            row.names="GNOMAD_GENOMES_AMR_AF"
        ),
        population_gnomad_genomes_amr_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for American ancestry"),
            row.names="GNOMAD_GENOMES_AMR_AC"
        ),
        population_gnomad_genomes_asj_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for Askenazim ancestry"),
            row.names="GNOMAD_GENOMES_ASJ_AF"
        ),
        population_gnomad_genomes_asj_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for Askenazim ancestry"),
            row.names="GNOMAD_GENOMES_ASJ_AC"
        ),
        population_gnomad_genomes_eas_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for East Asian ancestry"),
            row.names="GNOMAD_GENOMES_EAS_AF"
        ),
        population_gnomad_genomes_eas_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for East Asian ancestry"),
            row.names="GNOMAD_GENOMES_EAS_AC"
        ),
        population_gnomad_genomes_fin_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for Finnish ancestry"),
            row.names="GNOMAD_GENOMES_FIN_AF"
        ),
        population_gnomad_genomes_fin_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for Finnish ancestry"),
            row.names="GNOMAD_GENOMES_FIN_AC"
        ),
        population_gnomad_genomes_nfe_af=DataFrame(
            Number="1",
            Type="Float",
            Description=paste0("gnomAD genomes total alternative allele ",
                "frequency for Non-Finnish European ancestry"),
            row.names="GNOMAD_GENOMES_NFE_AF"
        ),
        population_gnomad_genomes_nfe_ac=DataFrame(
            Number="1",
            Type="Integer",
            Description=paste0("gnomAD genomes total alternative allele counts ",
                "for Non-Finnish European ancestry"),
            row.names="GNOMAD_GENOMES_NFE_AC"
        ),
        # This is a special case handled outside here - VESTA_FUNC_GENE_ANN
        "annotation_genes_name"=NA,
        "annotation_genes_ids"=NA,
        "annotation_genes_omim"=NA,
        "annotation_genes_omim_morbid"=NA,
        "annotation_genes_cgd_condition"=NA,
        "annotation_genes_cgd_inheritance"=NA,
        "annotation_genes_disgenet_id"=NA,
        "annotation_genes_disgenet_name"=NA,
        "annotation_genes_hpo_id"=NA,
        "annotation_genes_hpo_name"=NA,
        "annotation_genes_ctd_id"=NA,
        "annotation_genes_ctd_name"=NA,
        "annotation_genes_transcripts_transcript_id"=NA,
        "annotation_genes_transcripts_impact_so"=NA,
        "annotation_genes_transcripts_impact_snpeff"=NA,
        "annotation_genes_transcripts_biotype"=NA,
        "annotation_genes_transcripts_exon_rank"=NA,
        "annotation_genes_transcripts_hgvs_c"=NA,
        "annotation_genes_transcripts_hgvs_p"=NA,
        "annotation_genes_transcripts_location"=NA,
        "annotation_genes_transcripts_cdna_rank"=NA,
        "annotation_genes_transcripts_cds_rank"=NA,
        "annotation_genes_transcripts_aa_rank"=NA,
        # This is a special case handled outside here - VESTA_FUNC_VAR_ANN
        "annotation_variant_disgenet_id"=NA,
        "annotation_variant_disgenet_name"=NA,
        "annotation_variant_pharmgkb_id"=NA,
        "annotation_variant_pharmgkb_loe"=NA,
        "annotation_variant_pharmgkb_phenotypes_id"=NA,
        "annotation_variant_pharmgkb_phenotypes_name"=NA,
        "annotation_variant_pharmgkb_chemicals_id"=NA,
        "annotation_variant_pharmgkb_chemicals_name"=NA,
        "annotation_variant_clinvar_alleleid"=NA,
        "annotation_variant_clinvar_clnsig"=NA,
        "annotation_variant_clinvar_onc"=NA,
        "annotation_variant_oncokb_gene_symbol"=NA,
        "annotation_variant_oncokb_alteration"=NA,
        "annotation_variant_oncokb_oncogenic"=NA,
        "annotation_variant_oncokb_effect"=NA,
        "annotation_variant_oncokb_treatments_code"=NA,
        "annotation_variant_oncokb_treatments_drug"=NA,
        "annotation_variant_civic_id"=NA,
        "annotation_variant_civic_gene_id"=NA,
        "annotation_variant_civic_variant"=NA,
        # Continue
        annotation_classification_acmg=DataFrame(
            Number="A",
            Type="String",
            Description="ACMG classification",
            row.names="ACMG"
        ),
        annotation_classification_amp=DataFrame(
            Number="A",
            Type="String",
            Description="AMP classification",
            row.names="AMP"
        ),
        annotation_classification_manual_sig=DataFrame(
            Number="A",
            Type="String",
            Description="Manual germline pathogenicity classification",
            row.names="MAN_SIG"
        ),
        annotation_classification_manual_onc=DataFrame(
            Number="A",
            Type="String",
            Description="Manual somatic pathogenicity classification",
            row.names="MAN_ONC"
        ),
        annotation_classification_notes=DataFrame(
            Number=".",
            Type="String",
            Description="Classigication notes",
            row.names="CLASS_NOTES"
        )
    ))
}

.funcGeneAnnHeader <- function(fields) {
    desc <- "VESTA functional annotations: "
    
    # Standard fields (we may neved have to use some of them)
    if ("annotation_genes_name" %in% fields)
        desc <- c(desc,"gene name")
    if ("annotation_genes_ids" %in% fields)
        desc <- c(desc,"comma separated gene ids")
    if ("annotation_genes_transcripts_transcript_id" %in% fields)
        desc <- c(desc,"transcript id")
    if ("annotation_genes_transcripts_impact_so" %in% fields)
        desc <- c(desc,"SO impact")
    if ("annotation_genes_transcripts_impact_snpeff" %in% fields)
        desc <- c(desc,"SnpEff impact")
    if ("annotation_genes_transcripts_biotype" %in% fields)
        desc <- c(desc,"transcript biotype")
    if ("annotation_genes_transcripts_exon_rank" %in% fields)
        desc <- c(desc,"exon number/total exons")
    if ("annotation_genes_transcripts_hgvs_c" %in% fields)
        desc <- c(desc,"HGVS genomic change")
    if ("annotation_genes_transcripts_hgvs_p" %in% fields)
        desc <- c(desc,"HGVS amino acid change")
    if ("annotation_genes_transcripts_location" %in% fields)
        desc <- c(desc,"variant location")
    if ("annotation_genes_transcripts_cdna_rank" %in% fields)
        desc <- c(desc,"cDNA position/cDNA length")
    if ("annotation_genes_transcripts_cds_rank" %in% fields)
        desc <- c(desc,"CDS position/CDS length")
    if ("annotation_genes_omim" %in% fields)
        desc <- c(desc,"omim ids")
    if ("annotation_genes_omim_morbid" %in% fields)
        desc <- c(desc,"omim morbid ids")
    if ("annotation_genes_cgd_condition" %in% fields)
        desc <- c(desc,"CGD conditions")
    if ("annotation_genes_cgd_inheritance" %in% fields)
        desc <- c(desc,"CGD inheritance scenarios")
    if ("annotation_genes_disgenet_id" %in% fields)
        desc <- c(desc,"DisGeNET UMLS id")
    if ("annotation_genes_disgenet_name" %in% fields)
        desc <- c(desc,"DisGeNET disease name")
    if ("annotation_genes_hpo_id" %in% fields)
        desc <- c(desc,"HPO accession")
    if ("annotation_genes_hpo_name" %in% fields)
        desc <- c(desc,"HPO name")
    if ("annotation_genes_ctd_id" %in% fields)
        desc <- c(desc,"CTD accession")
    if ("annotation_genes_ctd_name" %in% fields)
        desc <- c(desc,"CTD chemical name")
    
    # Extended fields (from merging some of them to single string through the
    # aggregation pipeline)
    if ("annotation_genes_disgenet" %in% fields)
        desc <- c(desc,"DisGeNET UMLS id and disease")
    if ("annotation_genes_hpo" %in% fields)
        desc <- c(desc,"HPO id and name")
    if ("annotation_genes_ctd" %in% fields)
        desc <- c(desc,"CTD id and name")
    
    return(DataFrame(
        Number=".",
        Type="String",
        Description=paste0(desc[1],paste(desc[2:length(desc)],collapse=" | ")),
        row.names="VESTA_FUNC_GENE_ANN"
    ))
}

.funcVarAnnHeader <- function(fields) {
    desc <- "VESTA functional annotations:"
        
    # Standard fields (we may neved have to use some of them)
    if ("annotation_variant_disgenet_id" %in% fields)
        desc <- c(desc,"DisGeNET UMLS id")
    if ("annotation_variant_disgenet_name" %in% fields)
        desc <- c(desc,"DisGeNET disease name")
    if ("annotation_variant_pharmgkb_id" %in% fields)
        desc <- c(desc,"PharmGKB id")
    if ("annotation_variant_pharmgkb_loe" %in% fields)
        desc <- c(desc,"PharmGKB level of evidence")
    if ("annotation_variant_pharmgkb_phenotypes_id" %in% fields)
        desc <- c(desc,"PharmGKB phenotypes id")
    if ("annotation_variant_pharmgkb_phenotypes_name" %in% fields)
        desc <- c(desc,"PharmGKB phenotypes name")
    if ("annotation_variant_pharmgkb_chemicals_id" %in% fields)
        desc <- c(desc,"PharmGKB chemicals id")
    if ("annotation_variant_pharmgkb_chemicals_name" %in% fields)
        desc <- c(desc,"PharmGKB chemicals name")
    if ("annotation_variant_clinvar_alleleid" %in% fields)
        desc <- c(desc,"ClinVar allele id")
    if ("annotation_variant_clinvar_clnsig" %in% fields)
        desc <- c(desc,"ClinVar germline significance")
    if ("annotation_variant_clinvar_onc" %in% fields)
        desc <- c(desc,"ClinVar somatic significance")
    if ("annotation_variant_oncokb_gene_symbol" %in% fields)
        desc <- c(desc,"OncoKB gene symbol")
    if ("annotation_variant_oncokb_alteration" %in% fields)
        desc <- c(desc,"OncoKB alteration")
    if ("annotation_variant_oncokb_oncogenic" %in% fields)
        desc <- c(desc,"OncoKB oncogenic potential")
    if ("annotation_variant_oncokb_effect" %in% fields)
        desc <- c(desc,"OncoKB effect")
    if ("annotation_variant_oncokb_treatments_code" %in% fields)
        desc <- c(desc,"OncoKB treatments code")
    if ("annotation_variant_oncokb_treatments_drug" %in% fields)
        desc <- c(desc,"OncoKB treatments drug")
    if ("annotation_variant_civic_id" %in% fields)
        desc <- c(desc,"CiVIC id")
    if ("annotation_variant_civic_gene_id" %in% fields)
        desc <- c(desc,"CiVIC gene id")
    if ("annotation_variant_civic_variant" %in% fields)
        desc <- c(desc,"CiVIC variant")
    
    # Extended fields (from merging some of them to single string through the
    # aggregation pipeline)
    if ("annotation_variant_disgenet" %in% fields)
        desc <- c(desc,"DisGeNET UMLS id and disease for variant")
    if ("annotation_variant_pharmgkb_id" %in% fields)
        desc <- c(desc,"PharmGKB id and level of evidence")
    if ("annotation_variant_pharmgkb_phenotypes" %in% fields)
        desc <- c(desc,"PharmGKB phenotypes")
    if ("annotation_variant_pharmgkb_chemicals" %in% fields)
        desc <- c(desc,"PharmGKB chemicals")
    if ("annotation_variant_oncokb_entry" %in% fields)
        desc <- c(desc,"OncoKB entry")
    if ("annotation_variant_oncokb_treatments" %in% fields)
        desc <- c(desc,"OncoKB treatments")
    if ("annotation_variant_civic" %in% fields)
        desc <- c(desc,"CiVIC entry")
    
    return(DataFrame(
        Number=".",
        Type="String",
        Description=paste0(desc[1],paste(desc[2:length(desc)],collapse=" | ")),
        row.names="VESTA_FUNC_VAR_ANN"
    ))
}

.replaceObjectIds <- function(query) {
  # Replace ObjectId("...") with { "$oid": "..." }
  gsub("ObjectId\\(['\"]([a-fA-F0-9]{24})['\"]\\)",
       '{ "$oid": "\\1" }',query,perl=TRUE)
}

.extractObjectId <- function(x) {
    mts <- regmatches(x,regexec("ObjectId\\(['\"]([a-fA-F0-9]{24})['\"]\\)",x))
    if (length(mts[[1]]) >= 2)
        return(mts[[1]][2])
    else
        return(NA_character_)
}
