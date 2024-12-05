annotateAndInsertVariants <- function(analysisId) {
    # Get VCF file and genome from analysisId
    # Open the VCF file in chunks of X variants
    # Call vcfToList
    # JSON the list, add the analysisId and insert
}

vcfToList <- function(vcfFile,gv=c("hg19","hg38")) {
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
    
    .checkTextArgs("Genome version",gv,c("hg19","hg38"),multiarg=FALSE)
    
    # Warnings may be produced during validation - some dNSFP versions produce
    # problematic VCF headers
    message("Reading VCF file ",vcfFile)
    vcf <- suppressWarnings(readVcf(vcfFile))
    nucleoGenos <- readGT(vcfFile,nucleotides=TRUE)
    
    # Derive the main building blocks of the list that will represent a document
    # for a single variant in the database. With the building block below, we
    # build the initial document list which will later be enriched from entries
    # from our knowledge base and API calls.
    message("Building variant document blocks")
    
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
        type=unlist(info(vcf)[["VARTYPE"]]),
        # Genomic HGVS for search in COSMIC and MyVariant API
        hgvsg=.makeHgvsg(fixed$chromosome,fixed$start,fixed$ref,fixed$alt)
    )
    rownames(fixed) <- fixed$hgvsg
    
    # INFO metrics, depth etc.
    metrics <- .greedyUncompressDataFrame(info(vcf)[,.getDefaultInfoFields()])
    
    # FORMAT fields including genotype metrics
    # If VCF normalized as per our workflow, this should work, also attach
    # nucleotide version of genotypes
    genos <- as.data.frame(cbind(nucleoGenos,do.call("cbind",geno(vcf))))
    genos[,3] <- as.numeric(genos[,3])
    genos[,4] <- as.numeric(genos[,4])
    colnames(genos) <- c("GTN",.getDefaultGenoFields())
    
    # Basic SnpEff annotations
    ANN <- info(vcf)["ANN"]
    annList <- cmclapply(ANN[,1],.parseANN,rc=0.25)
    
    # dbNSFP annotations - + and - symbols are replaced with _ during import...
    # We split pathogenicity, conservation and population as these will be
    # different section in the variant document
    dbnsfpNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpFields())
    dbnsfp <- .greedyUncompressDataFrame(info(vcf)[,dbnsfpNames])
    
    # Population annotations
    gnomadExomes <- 
        .greedyUncompressDataFrame(info(vcf)[,.getVcfGnomadExomeFields()])
    gnomadGenomes <- 
        .greedyUncompressDataFrame(info(vcf)[,.getVcfGnomadGenomeFields()])
    
    # Clinvar annotations
    clinvars <- .greedyUncompressDataFrame(info(vcf)[,.getVcfClinvarFields()])
    clinvars[,2] <- gsub("_"," ",clinvars[,2])
    clinvars[,3] <- gsub("_"," ",clinvars[,3])
    
    # Get all genes present in VCF to construct hashes of hits in backend
    # collections such as disgenet, hpo etc.
    message("Retrieving annotation elements from knowledge base")
    vcfVariants <- unlist(rsids)
    vcfVariants <- vcfVariants[grepl("^rs",vcfVariants)]
    vcfGenes <- unique(unlist(lapply(annList,function(x) x[,"gene_name"])))
    
    message("  DisGeNET for genes")
    disgenetGeneHits <- .findDisgenetByGene(vcfGenes)
    message("    Retrieved ",nrow(disgenetGeneHits)," hits")
    
    message("  DisGeNET for variants")
    disgenetVariantHits <- .findDisgenetByVariant(vcfVariants)
    message("    Retrieved ",nrow(disgenetVariantHits)," hits")
    
    message("  Human Phenotype Ontology")
    hpoHits <- .findHpoByGene(vcfGenes)
    message("    Retrieved ",length(hpoHits)," hits")
    
    message("  Comparative Toxicogenomics Database")
    ctdHits <- .findCtdByGene(vcfGenes)
    message("    Retrieved ",length(ctdHits)," hits")
    
    message("  Clinical Genomics Database")
    cgdHits <- .findCgdInheritanceByGene(vcfGenes)
    message("    Retrieved ",nrow(cgdHits)," hits")
    
    message("  COSMIC")
    cosmicHits <- .findCosmicByVariant(fixed$hgvsg,gv)
    message("    Retrieved ",nrow(cosmicHits)," hits")

    message("  PharmGKB")
    pharmgkbHits <- .findPharmGkbByVariant(vcfVariants)
    message("    Retrieved ",nrow(pharmgkbHits)," hits")
    
    message(" OncoKB")
    allSoTerms <- unname(sapply(annList,function(x) {
        return(x[1,"detailed_impact_so_term"])
    }))
    rind <- .filterSoTermsForApiCalls(allSoTerms)
    oncokbHits <- .queryOncoKB(fixed$hgvsg[rind],gv)
    names(oncokbHits) <- fixed$hgvsg[rind]
    # Clean the hits from empty genes because of improper hgvs parsing
    oncokbHits = oncokbHits[!sapply(oncokbHits,
        function(x) is.null(x$gene_symbol) || x$oncogenic=="Unknown")]
    message("    Retrieved ",length(oncokbHits)," hits")
    
    geneResource <- list(
        disgenet=disgenetGeneHits,
        hpo=hpoHits,
        ctd=ctdHits,
        cgd=cgdHits
    )
    variantResource <- list(
        disgenet=disgenetVariantHits,
        pharmgkb=pharmgkbHits,
        oncokb=oncokbHits
    )
    
    message("Building variant documents")
    # Ultimately this may end very large... Maybe we could process it in chunks
    # and insert in chunks... Like tabix the VCF and use open and yield with
    # this function... This would also reduce the burden on API calls...
    docs <- lapply(seq_along(vcf),function(i) {
        list(
            identity=.getIdentity(fixed[i,,drop=FALSE],rsids[[i]],cosmicHits),
            metrics=.getMetrics(metrics[i,]),
            genotypes=.getGenotype(genos[i,,drop=FALSE]),
            pathogenicity=.getPathogenicity(dbnsfp[i,,drop=FALSE],
                clinvars[i,,drop=FALSE]),
            conservation=.getConservation(dbnsfp[i,,drop=FALSE]),
            population=.getPopulation(dbnsfp[i,,drop=FALSE],
                gnomadExomes[i,,drop=FALSE],gnomadGenomes[i,,drop=FALSE]),
            annotation=list(
                genes=.getGenes(annList[[i]],geneResource),
                variant=.getVariant(rsids[[i]],fixed$hgvsg[i],variantResource)
            )
        )
    })
    message("Done!")
    return(docs)
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
        rsid=rsids,
        cosmic=ch[rownames(fixed),"cosmic_id"]
    ))
}

.getMetrics <- function(metrics) {
    return(list(
        ac=ifelse(is.na(metrics$AC),metrics$AO,metrics$AC),
        af=metrics$AF,
        ac=ifelse(is.na(metrics$AO),metrics$AC,metrics$AO),
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
        gq=genos$GQ,
        sb=genos$SB
    ))
}

.getPathogenicity <- function(dbnsfp,clinvar) {
    dbnsfpPathoNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpPathogenicityFields())
    patho <- dbnsfp[,dbnsfpPathoNames]
    colnames(patho) <- tolower(gsub("dbNSFP_","",colnames(patho)))
    colnames(clinvar) <- tolower(colnames(clinvar))
    return(as.list(cbind(patho,clinvar)))
}

.getConservation <- function(dbnsfp) {
    dbnsfpConseNames <- gsub("[\\+\\-]","_",.getVcfDbnsfpConservationFields())
    cons <- dbnsfp[,dbnsfpConseNames]
    colnames(cons) <- tolower(gsub("dbNSFP_","",colnames(cons)))
    return(as.list(cons))
}

.getPopulation <- function(dbnsfp,gnomadExomes,gnomadGenomes) {
    dbnsfpPopulNames <- .getVcfDbnsfpPopulationFields()
    dbnsfp <- dbnsfp[,dbnsfpPopulNames]
    
    tgp <- dbnsfp[,grep("1000Gp3",colnames(dbnsfp))]
    colnames(tgp) <- tolower(gsub("dbNSFP_1000Gp3_","",colnames(tgp)))
    exac <- dbnsfp[,grep("ExAC",colnames(dbnsfp))]
    colnames(exac) <- tolower(gsub("dbNSFP_ExAC_","",colnames(exac)))
    esp <- dbnsfp[,grep("ESP6500",colnames(dbnsfp))]
    colnames(esp) <- tolower(gsub("dbNSFP_ESP6500_","",colnames(esp)))
    uk10k <- dbnsfp[,grep("UK10K",colnames(dbnsfp))]
    colnames(uk10k) <- tolower(gsub("dbNSFP_UK10K_","",colnames(uk10k)))
    alfa <- dbnsfp[,grep("ALFA",colnames(dbnsfp))]
    colnames(alfa) <- tolower(gsub("dbNSFP_ALFA_","",colnames(alfa)))
    
    colnames(gnomadExomes) <- tolower(gsub("gnomAD_exomes_","",
        colnames(gnomadExomes)))
    colnames(gnomadGenomes) <- tolower(gsub("gnomAD_genomes_","",
        colnames(gnomadGenomes)))
    
    return(list(
        tgp=tgp,
        exac=exac,
        esp=esp,
        uk10k=uk10k,
        alfa=alfa,
        gnomad_exomes=gnomadExomes,
        gnomad_genomes=gnomadGenomes
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
    return(list(
        name=gn,
        ids=.formatGeneResource(gn,r,"ids"),
        omim=.formatGeneResource(gn,r,"omim"),
        omim_morbid=.formatGeneResource(gn,r,"omim_morbid"),
        cgd=.formatGeneResource(gn,r,"cgd"),
        disgenet=.formatGeneResource(gn,r,"disgenet"),
        hpo=.formatGeneResource(gn,r,"hpo"),
        ctd=.formatGeneResource(gn,r,"ctd"),
        dbnsfp_gene=.formatGeneResource(gn,r,"dbnsfp_gene"),
        transcripts=.getTranscripts(g)
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

.getTranscripts <- function(g) {
    g$gene_name <- NULL
    # We may have to split as some variants (in exomes/genomes?) may have 
    # multiple impacts joined with "&"
    #g$impact_so <- strsplit(g$impact_so,"&")
    return(unname(apply(g,1,as.list)))
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
    
    return(list(
        disgenet=disgenet,
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
            return(NULL)
        }
    )
}

.parseID <- function(n) {
    # Keep only dbSNP ids, rest are auto-generated and should be .
    toDot <- !grepl("^rs",n)
    n[toDot] <- "."
    return(strsplit(n,";"))
}

#"Functional annotations: 'Allele | Annotation | Annotation_Impact | Gene_Name | Gene_ID | Feature_Type | Feature_ID | Transcript_BioType | Rank | HGVS.c | HGVS.p | cDNA.pos / cDNA.length | CDS.pos / CDS.length | AA.pos / AA.length | Distance | ERRORS / WARNINGS / INFO'"


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
                hgvs_p=x[11]
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
            #biotype=NA,
            exon_rank=NA, 
            hgvs_c=NA, 
            hgvs_p=NA
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
                res <- collapseCharacterList(DF[,n])
            else
                res <- unlist(DF[,n])
        }
        else if (is(DF[,n],"IntegerList")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- collapseCharacterList(DF[,n])
            else
                res <- unlist(DF[,n])
        }
        else if (is(DF[,n],"NumericList")) {
            em <- which(lengths(DF[,n]) == 0)
            if (length(em) > 0)
                DF[em,n] <- NA
            if (any(lengths(DF[,n]) > 1))
                res <- collapseCharacterList(DF[,n])
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
    if (nrow(result) > 0)
        rownames(result) <- result[,1]
    
    return(result)
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

.makeHgvsg <- function(chromosome, start, reference, alternative) {
  # Ensure inputs are of equal length
  if (!(length(chromosome) == length(start) &&
        length(start) == length(reference) &&
        length(reference) == length(alternative))) {
    stop("All input vectors must have the same length.")
  }
  
  # Determine lengths of reference and alternative alleles
  ref_len <- nchar(reference)
  alt_len <- nchar(alternative)
  
  # Initialize HGVS vector
  hgvs <- character(length(chromosome))
  
  # SNP: Single Nucleotide Polymorphism
  snp_idx <- ref_len == 1 & alt_len == 1
  hgvs[snp_idx] <- paste0(
    chromosome[snp_idx], ":g.", start[snp_idx], 
    reference[snp_idx], ">", alternative[snp_idx]
  )
  
  # Pure Insertion: Empty reference, non-empty alternative
  pure_insertion_idx <- ref_len == 0 & alt_len > 0
  hgvs[pure_insertion_idx] <- paste0(
    chromosome[pure_insertion_idx], ":g.", start[pure_insertion_idx] - 1, "_", 
    start[pure_insertion_idx], "ins", alternative[pure_insertion_idx]
  )
  
  # Pure Deletion: Non-empty reference, empty alternative
  pure_deletion_idx <- ref_len > 0 & alt_len == 0
  pure_deletion_end <- start[pure_deletion_idx] + ref_len[pure_deletion_idx] - 1
  hgvs[pure_deletion_idx] <- paste0(
    chromosome[pure_deletion_idx], ":g.", start[pure_deletion_idx], "_", 
    pure_deletion_end, "del"#, reference[pure_deletion_idx]
  )
  
  # Insertion: Alternative allele is longer, starts with reference
  insertion_idx <- alt_len > ref_len & 
                   substr(alternative, 1, ref_len) == reference
  hgvs[insertion_idx] <- paste0(
    chromosome[insertion_idx], ":g.", start[insertion_idx], "_", 
    start[insertion_idx] + ref_len[insertion_idx] - 1, "ins", 
    substr(alternative[insertion_idx], ref_len[insertion_idx] + 1, alt_len[insertion_idx])
  )
  
  # Deletion: Reference allele is longer, starts with alternative
  deletion_idx <- ref_len > alt_len & 
                  substr(reference, 1, alt_len) == alternative
  deletion_end <- start[deletion_idx] + ref_len[deletion_idx] - 1
  hgvs[deletion_idx] <- paste0(
    chromosome[deletion_idx], ":g.", start[deletion_idx], "_", 
    deletion_end, "del"#, 
    #substr(reference[deletion_idx], alt_len[deletion_idx] + 1, ref_len[deletion_idx])
  )
  
  # MNV: Reference and alternative are of equal length (>1), and differ
  mnv_idx <- ref_len > 1 & alt_len > 1 & ref_len == alt_len
  mnv_end <- start[mnv_idx] + ref_len[mnv_idx] - 1
  hgvs[mnv_idx] <- paste0(
    chromosome[mnv_idx], ":g.", start[mnv_idx], "_", 
    mnv_end, reference[mnv_idx], ">", alternative[mnv_idx]
  )
  
  # Delins: Complex substitution (not covered by the above)
  delins_idx <- !(snp_idx | pure_insertion_idx | pure_deletion_idx | insertion_idx | deletion_idx | mnv_idx)
  delins_end <- start[delins_idx] + ref_len[delins_idx] - 1
  hgvs[delins_idx] <- paste0(
    chromosome[delins_idx], ":g.", start[delins_idx], "_", 
    delins_end, "delins", alternative[delins_idx]
  )
  
  return(hgvs)
}

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
        responseData <- httr::content(response,as="parsed",type="application/json")
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

