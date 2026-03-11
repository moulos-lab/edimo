# Questions: which genome versions? like the old one

# Where is biotype from?

# What exactly is the uniprot field? Swiss-Prot? Ok

# Regarding versioning, what should the structure be? Simply keep the versioned
# id for gene, transcript, peptide and the rest as previously?

# MANE does not work well, and does not exist for hg19. Figure out why, also, use
# RefSeq mRNA ID? Yes. Include predicted??? No. Include ncRNA?

# Architecture for genome versions? What should be where?

# Where is exon start/end from? Found

# UTR data? No

# Format strand info from +/-1 to +/-

# For hg19, refseq are versioned
# Regarding versioning, hg38 only versioned
# No UTRs just exons

# Should I keep External gene name or HGNC?

library(biomaRt)

# This is code from the SiTaDeLa package, modified to our needs.
.bypassTimeoutByFilters <- function(org,type,ver,tv,mart) {
    message(paste("Processing", org, "-", type, "- version:", ver, "- tv:", tv))
    chrom_splits <- list("1,2,3,4", 
                         "5,6,7,8", 
                         "9,10,11,12,13,14,15,16,17,18", 
                         "19,20,21,22,23,X,Y,MT")
    filter <- .getFilter(org,type,ver,tv)
    filterValues <- do.call(rbind, lapply(chrom_splits, function(split) {
        getBM(attributes=filter,
              filters = "chromosome_name",
              values = split,
              mart=mart)
    }))
    data <- getBM(attributes = .getAttributes(org, type, ver, tv),
                  filters = filter,
                  values = filterValues,
                  mart = mart)
    return(data)
}

.getFilter <- function(org,type,ver=NULL,tv=FALSE) {
    ver <- .checkEnsVer(ver,org)
    if (type == "gene") {
        if (tv)
            return(.getVersionedFilter(org,"gene",ver))
        else
            return("ensembl_gene_id")
    }
    else if (type == "exon")
        return("ensembl_exon_id")
    else if (type == "transcript") {
        if (tv)
            return(.getVersionedFilter(org,"transcript",ver))
        else
            return("ensembl_transcript_id")
    }
    else if (type == "peptide") {
        if (tv)
            return(.getVersionedFilter(org,"peptide",ver))
        else
            return("ensembl_peptide_id")
    }    
}

.getVersionedFilter <- function(org,type,ver) {
    if (org %in% .orgsWithNoVersion())
        return(paste0("ensembl_",type,"_id"))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(paste0("ensembl_",type,"_id"))
        else
            return(paste0("ensembl_",type,"_id_version"))
    }
    else if (org %in% .orgsWithVersion())
        return(paste0("ensembl_",type,"_id_version"))
}

.getAttributes <- function(org,type,ver=NULL,tv=FALSE) {
    ver <- .checkEnsVer(ver,org)
    switch(type,
        gene = {
            if (tv)
                return(.getVersionedGeneAttributes(org,ver))
            else
                return(.getGeneAttributes(org))
        },
        transcript = {
            if (tv)
                return(.getVersionedTranscriptAttributes(org,ver))
            else
                return(.getTranscriptAttributes(org))
        },
        exon = {
            if (tv)
                return(.getVersionedExonAttributes(org,ver))
            else
                return(.getExonAttributes(org))
        },
        peptide = {
            if (tv)
                return(.getVersionedPeptideAttributes(org,ver))
            else
                return(.getPeptideAttributes(org))
        }
    )
}

.getGeneAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9"))
        return(c(
            "ensembl_gene_id",
            "external_gene_id",
            "chromosome_name",
            "start_position",
            "end_position",
            "strand",
            "band",
            "description",
            "gene_biotype",
            "mim_gene_accession",
            "mim_morbid_accession",
            "entrezgene"
        ))
    else
        return(c(
            "ensembl_gene_id",
            "external_gene_name",
            "chromosome_name",
            "start_position",
            "end_position",
            "strand",
            "band",
            "description",
            "gene_biotype",
            "mim_gene_accession",
            "mim_morbid_accession"
        ))
}

.getVersionedGeneAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getGeneAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getGeneAttributes(org))
        else
            return(c(
                "ensembl_gene_id_version",
                "external_gene_name",
                "chromosome_name",
                "start_position",
                "end_position",
                "strand",
                "band",
                "description",
                "gene_biotype",
                "mim_gene_accession",
                "mim_morbid_accession"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
                "ensembl_gene_id_version",
                "external_gene_name",
                "chromosome_name",
                "start_position",
                "end_position",
                "strand",
                "band",
                "description",
                "gene_biotype",
                "mim_gene_accession",
                "mim_morbid_accession"
            ))
}

.getTranscriptAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9"))
        return(c(
            "ensembl_gene_id",
            "ensembl_transcript_id",
            "transcript_start",
            "transcript_end",
            "transcript_biotype",
            "refseq_mrna",
            "refseq_ncrna"
        ))
    else
        return(c(
            "ensembl_gene_id",
            "ensembl_transcript_id",
            "transcript_start",
            "transcript_end",
            "transcript_biotype",
            "refseq_mrna",
            "refseq_ncrna"
        ))
}

.getVersionedTranscriptAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getTranscriptAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getTranscriptAttributes(org))
        else
            return(c(
                "ensembl_gene_id_version",
                "ensembl_transcript_id_version",
                "transcript_start",
                "transcript_end",
                "transcript_biotype",
                "refseq_mrna",
                "refseq_ncrna",
                "transcript_mane_select",
                "transcript_mane_plus_clinical"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "ensembl_gene_id_version",
            "ensembl_transcript_id_version",
            "transcript_start",
            "transcript_end",
            "transcript_biotype",
            "refseq_mrna",
            "refseq_ncrna"
        ))
}

.getExonAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9"))
        return(c(
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "ensembl_transcript_id"
        ))
    else
        return(c(
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "ensembl_transcript_id"
        ))
}

.getVersionedExonAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getExonAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getExonAttributes(org))
        else
            return(c(
                "exon_chrom_start",
                "exon_chrom_end",
                "ensembl_exon_id", # May need to be changed to _version
                "ensembl_transcript_id_version"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id", # May need to be changed to _version
            "ensembl_transcript_id_version"
        ))
}

.getPeptideAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9"))
        return(c(
            "ensembl_transcript_id",
            "ensembl_peptide_id",
            "uniprot_swissprot",
            "uniprot_sptrembl"
        ))
    else
        return(c(
            "ensembl_transcript_id",
            "ensembl_peptide_id",
            "uniprotswissprot",
            "uniprotsptrembl"
        ))
}

.getVersionedPeptideAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getTranscriptUtrAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getTranscriptUtrAttributes(org))
        else
            return(c(
                "ensembl_transcript_id_version",
                "ensembl_peptide_id_version",
                "uniprotswissprot",
                "uniprotsptrembl"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "ensembl_transcript_id_version",
            "ensembl_peptide_id_version",
            "uniprotswissprot",
            "uniprotsptrembl"
        ))
}

.ucscToEnsembl <- function() {
    return(list(
        hg19=75,
        hg38=95:cur_ver
    ))
}

.getHost <- function(org,ver=NULL) {
    if (!requireNamespace("biomaRt"))
        stop("The Bioconductor package biomaRt is required to proceed!")
    
    org <- tolower(org[1])
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    if (!is.null(ver) 
        && (!is.numeric(ver) || is.na(suppressWarnings(as.numeric(ver)))))
        stop("ver must be numeric or coercible to numeric if not NULL!")
        
    if (org == "tair10")
        return("plants.ensembl.org")
    
    aver <- .getUcscToEnsembl(org)
    if (!is.null(ver) && !(ver %in% aver)) {
        warning("Version ",ver," not available/existing for ",org,"! Will ",
            "use the latest available version...",immediate.=TRUE)
        ver <- NULL
    }
    
    if (is.null(ver)) {
        u2e <- .ucscToEnsembl()
        vers <- u2e[[org]]
        ver <- vers[length(vers)]
    }
    
    ea <- biomaRt::listEnsemblArchives()
    i <- grep(as.character(ver),ea[,"version"])
    if (length(i) > 0) {
        if (ea[i,"current_release"] == "*")
            return("https://www.ensembl.org")
        else
            return(ea[i,"url"])
    }
    else {
        warning("Version ",ver," not found in biomaRt archives for ",org,"! ",
            "Will use the latest available version for ",org,"...",
            immediate.=TRUE)
        u2e <- .ucscToEnsembl()
        vers <- u2e[[org]]
        for (v in rev(vers)) {
            newi <- grep(as.character(v),ea[,"version"])
            if (length(newi) > 0)
                return(ea[newi,"url"])
        }
    }
    # If everything fails...
    return(NULL)
}

.getDataset <- function(org) {
    switch(org,
        hg19 = { return("hsapiens_gene_ensembl") },
        hg38 = { return("hsapiens_gene_ensembl") }
    )
}

.getUcscToEnsembl <- function(org) {
    u2e <- .ucscToEnsembl()
    return(u2e[[org]])
}

.checkUcscToEnsembl <- function(org,ver) {
    u2e <- .getUcscToEnsembl()
    return(ver %in% u2e[[org]])
}

.orgsWithNoVersion <- function() {
    return(c("hg18","hg19","mm9","rn5","dm3","dm6","pantro4","danrer7",
        "danrer10","susscr3","equcab2","tair10"))
}

.orgsWithVersionAfter90 <- function() {
    return(c("hg38","mm10","rn6","pantro5","susscr11"))
}

.orgsWithVersion <- function() {
    return(c("danrer11","equcab3"))
}

.checkEnsVer <- function(ver,org) {
    if (is.null(ver)) {
        u2e <- .ucscToEnsembl()
        vers <- u2e[[org]]
        ver <- vers[length(vers)]
    }
    return(ver)
}

.checkTextArgs <- function(argName,argValue,argList,multiarg=FALSE) {
    if (multiarg) {
        argValue <- tolower(argValue)
        if (!all(argValue %in% argList))
            stop("\"",argName,"\""," parameter must be one or more of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
    else {
        argSave <- argValue[1]
        argValue <- tolower(argValue[1])    
        if (!(argValue %in% argList))
            stop("\"",argName,"\""," parameter must be one of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
}

getSupportedOrganisms <- function() {
    return(c("hg18","hg19","hg38","mm9","mm10","rn5","rn6","dm3","dm6",
        "danrer7","danrer10","danrer11","pantro4","pantro5",#"pantro6",
        "susscr3","susscr11","equcab2","equcab3","tair10"))
}

# Function to get ver and tv values based on org
.getVerAndTV <- function(org) {
  if (org == "hg19") {
    return(list(ver = 75, tv = FALSE))
  } else if (org == "hg38") {
    return(list(ver = cur_ver, tv = TRUE))
  } else {
    stop("Unrecognized organism specified.")
  }
}

# Function to fetch Ensembl data
fetchEnsembl <- function(orgs) {
    types <- list("gene", "transcript", "exon", "peptide")
    
    ensembl <- list()
    for (org in orgs) {
        ver_tv <- .getVerAndTV(org)
        
        for (type in types) {

            if (org == "hg19" && type != "exon") next
            if (org == "hg38") next

            # Get the host and dataset based on org and type
            host <- .getHost(org, ver_tv[["ver"]])
            dataset <- .getDataset(org)

            # Use biomaRt to get the mart
            mart <- useEnsembl(biomart = "genes", host = host, dataset = dataset)

            # Call the function to bypass timeout by filters
            data <- .bypassTimeoutByFilters(org, type, ver_tv[["ver"]], ver_tv[["tv"]], mart)

            write.table(data,file=paste0("/home/stelios/references/ensembl/ensembl_",org,"_",type,"_v",ver_tv[["ver"]],".csv"), sep="\t", row.names = FALSE, quote = FALSE)
        }
    }
    # return(ensembl)
}

# Define orguments and types
orgs <- list("hg19", "hg38")
cur_ver <- 115

# Fetch Ensembl data
#fetchEnsembl(orgs)
