annotateVcf <- function(vcfFile) {
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
    
    # 0. Check chromosome names and rename if necessary
    seqStyle <- .guessGenomeStyle(vcfFile)
    if (seqStyle == "UCSC") {
        log_info("Renaming chromosomes from UCSC style to NCBI style in file ",
            vcfFile)
        
        bcftools <- .getTool("bcftools")
        #baseCommand <- paste(
        #   paste0(bcftools," annotate \\"),
        #   paste0("  --rename ",plinkBase," \\"),
        #   paste0("  --out ",plinkBase),
        #   sep="\n"
        #)
        command <- glue("
            {bcftools} annotate \
              --rename-chrs {chrRenameFile} \
              --output {outVcf0} \
              {vcfFile}
        ")
        
        message("Executing:\n",command)
#~      out <- tryCatch({
#~          system(command,ignore.stdout=TRUE,ignore.stderr=TRUE)
#~          if (prismaVerbosity() == "full") {
#~              logfile <- paste0(plinkBase,".log")
#~              log <- .formatSystemOutputForDisp(readLines(logfile))
#~              disp("\nPLINK output is:\n")
#~              disp(paste(log,collapse="\n"),"\n")
#~          }
#~          0L
#~      },error=function(e) {
#~          disp("Caught error: ",e$message)
#~          return(1L)
#~      },finally="")
        
#~      if (out != 1L) {
#~          # Read-in the resulting non-pruned SNP names
#~          snpsetIbd <- readLines(paste0(plinkBase,".prune.in"))
#~          disp("LD pruning finished!")
#~          disp(length(snpsetIbd)," SNPs will be used ","for IBD analysis.")
#~      }
#~      else
#~          stop("LD pruning with PLINK has failed! Please use another ",
#~              "backend for LD pruning...")
    }
    
    # 1. Strip incoming VCF from unecessary fields for this app
}

checkVcf <- function(vcfFile) {
    if (!requireNamespace("VariantAnnotation"))
        stop("Bioconductor package VariantAnnotation is required!")
    
    param <- ScanVcfParam(fixed=NA,info=NA,samples=NA)
    isVcf <- tryCatch({
        readVcf(vcfFile,param=param)
        TRUE
    },error=function(e) {
        message("VCF file check failed! ",e$message)
        return(FALSE)
    })
    
    return(isVcf)
}

stripVcf <- function(vcfFile,keepExtraFields=TRUE,cnvs=TRUE) {
    if (!requireNamespace("VariantAnnotation"))
        stop("Bioconductor package VariantAnnotation is required!")
    
    if (!is.character(vcfFile))
        stop("vcfFile argument must be a proper file name!")
    if (!file.exists(vcfFile))
        stop(paste0("The input VCF file ",vcfFile," does not exist!"))
    if (is.na(as.logical(keepExtraFields))) {
        warning(paste0("The keepExtraFields argument must be TRUE or FALSE! ",
            "Assuming TRUE..."),immediate.=TRUE)
        keepExtraFields=TRUE
    }
    if (is.na(as.logical(cnvs))) {
        warning(paste0("The cnvs argument must be TRUE or FALSE! Assuming ",
            "TRUE..."),immediate.=TRUE)
        cnvs=TRUE
    }
    
    # Define basename for the output(s)
    compression <- grepl("[.](gz|bz2|xz)$",vcfFile)
    vcfFile <- ifelse(compression,sub("[.](gz|bz2|xz)$","",vcfFile),vcfFile)
    outBase <- sub("([^.]+)\\.[[:alnum:]]+$","\\1",basename(vcfFile))
    outDir <- dirname(vcfFile)

    # Define common fields after our analysis
    infoFields <- .getDefaultInfoFields()
    genoFields <- .getDefaultGenoFields()
    infoHeaders <- .getDefaultInfoHeaders()
    genoHeaders <- .getDefaultGenoHeaders()
    
    message("Reading VCF file ",vcfFile)
    vcf <- readVcf(vcfFile)
    
    message("Stripping extra fields and harmonizing")
    # Harmonize strand bias - add a common header
    info(header(vcf))["SBP", ] <- infoHeaders[["SBP"]]
    # Possible strand bias fields are FS, SBP and STBP
    if ("FS" %in% rownames(info(header(vcf))))
        info(vcf)[["SBP"]] <- info(vcf)[["FS"]]
    else if ("STBP" %in% rownames(info(header(vcf))))
        info(vcf)[["SBP"]] <- -10*log10(info(vcf)[["STBP"]])
    else {
        warning("Strand bias field unavailable!",immediate.=TRUE)
        info(vcf)[["SBP"]] <- rep(".",length(vcf))
    }
    
    # Harmonize INFO fields
    for (field in infoFields) {
        if (!(field %in% rownames(info(header(vcf))))) {
            info(header(vcf))[field, ] <- infoHeaders[[field]]
            info(vcf)[[field]] <- rep(".",length(vcf))
        }
    }
    # Harmonize FORMAT fields
    for (field in genoFields) {
        if (!(field %in% rownames(geno(header(vcf))))) {
            geno(header(vcf))[field, ] <- genoHeaders[[field]]
            geno(vcf)[[field]] <- as.matrix(rep(".", length(vcf)))
        }
    }
    
    # Makes a list of the ALT field. Works incorrectly with multiallelic files
    altList <- lapply(as.list(alt(vcf)),as.character)

    vcfCnv <- vcf[which(altList=="<CNV>")]
    vcfIndel <- vcf[which(altList!="<CNV>")]

    info(vcfIndel) <- info(vcfIndel)[infoFields]
    geno(vcfIndel) <- geno(vcfIndel)[genoFields]
    info(header(vcfIndel)) <- info(header(vcfIndel))[infoFields, ]
    geno(header(vcfIndel)) <- geno(header(vcfIndel))[genoFields, ]

    # Writes CNVs in separate vcf if they exist
    if (length(rownames(vcfCnv)) > 0)
    # Write the filtered VCF
        writeVcf(vcfCnv,file.path(outDir,paste0(outBase,"_stripped_CNV.vcf")))
    writeVcf(vcfIndel,file.path(outDir,paste0(outBase,"_stripped.vcf")))
}

.guessGenomeStyle <- function(vcfFile) {
    param <- ScanVcfParam(fixed=NA,info=NA,samples=NA)
    vcf <- readVcf(vcfFile,param=param)
    return(seqlevelsStyle(vcf))
}

.getDefaultInfoFields <- function() {
    return(c("AC","AF","AO","DP","SBP","SRF","SRR","SAF","SAR"))
}

.getDefaultGenoFields <- function() {
    return(c("GT","DP","GQ","SB"))
}

.getDefaultInfoHeaders <- function() {
    return(list(
        AC=list("A","Integer","Allele count in genotypes"),
        AF=list("A","Float","Allele Frequency"),
        AO=list("A","Integer","Alternate allele observations"),
        DP=list("1","Integer","Total read depth at the locus"),
        SBP=list("A","Float","Phred-scaled p-value of Strand bias"),
        SAF=list("A","Integer",paste0("Alternate allele observations on the ",
            "forward strand")),
        SAR=list("A","Integer",paste0("Alternate allele observations on the ",
            "reverse strand")),
        SRF=list("1","Integer",paste0("Number of reference observations on ",
            "the forward strand")),
        SRR=list("1","Integer",paste0("Number of reference observations on ",
            "the reverse strand"))
    ))
}

.getDefaultGenoHeaders <- function() {
    return(list(
    GT=list("1","String","Genotype"),
    DP=list("1","Integer","Read Depth"),
    GQ=list("1","Integer",paste0("Genotype Quality, the Phred-scaled ",
        "marginal (or unconditional) probability of the called genotype")),
    SB=list("1","Float","Strand Bias")
  ))
}

.getTool <- function(tool) {
    tool <- tolower(tool)
    .checkTextArgs("Tool",tool,names(.CONFIG$software),multiarg=FALSE)
    return(list(
        command=.CONFIG$software[[tool]]$exec,
        version=.CONFIG$software[[tool]]$version
    ))
}

.dbnsfpFieldsString <- function() {
    return(paste(.getVanillaDbnsfpFields(),collapse=","))
}

.getVcfDbnsfpFields <- function() {
    return(paste("dbNSFP",.getVanillaDbnsfpFields(),sep="_"))
}

.getVanillaDbnsfpFields <- function() {
    return(c(
        .getVanillaDbnsfpPathogenicityFields(),
        .getVanillaDbnsfpConservationFields(),
        .getVanillaDbnsfpPopulationFields()
    ))
}

.getVanillaDbnsfpPathogenicityFields <- function() {
    return(c(
        "SIFT_score",
        "SIFT_converted_rankscore",
        "SIFT_pred",
        "SIFT4G_score",
        "SIFT4G_converted_rankscore",
        "SIFT4G_pred",
        "Polyphen2_HDIV_score",
        "Polyphen2_HDIV_rankscore",
        "Polyphen2_HDIV_pred",
        "Polyphen2_HVAR_score",
        "Polyphen2_HVAR_rankscore",
        "Polyphen2_HVAR_pred",
        "LRT_score",
        "LRT_converted_rankscore",
        "LRT_pred",
        "MutationTaster_score",
        "MutationTaster_converted_rankscore",
        "MutationTaster_pred",
        "MutationAssessor_score",
        "MutationAssessor_rankscore",
        "MutationAssessor_pred",
        "FATHMM_score",
        "FATHMM_converted_rankscore",
        "FATHMM_pred",
        "PROVEAN_score",
        "PROVEAN_converted_rankscore",
        "PROVEAN_pred",
        "MetaSVM_score",
        "MetaSVM_rankscore",
        "MetaSVM_pred",
        "MetaLR_score",
        "MetaLR_rankscore",
        "MetaLR_pred",
        "MetaRNN_score",
        "MetaRNN_rankscore",
        "MetaRNN_pred",
        "M_CAP_score",
        "M_CAP_rankscore",
        "M_CAP_pred",
        "PrimateAI_score",
        "PrimateAI_rankscore",
        "PrimateAI_pred",
        "DEOGEN2_score",
        "DEOGEN2_rankscore",
        "DEOGEN2_pred",
        "ClinPred_score",
        "ClinPred_rankscore",
        "ClinPred_pred",
        "AlphaMissense_score",
        "AlphaMissense_rankscore",
        "AlphaMissense_pred",
        "DANN_score",
        "DANN_rankscore",
        "VEST4_score",
        "VEST4_rankscore",
        "REVEL_score",
        "REVEL_rankscore",
        "Aloft_prob_Tolerant",
        "Aloft_prob_Recessive",
        "Aloft_prob_Dominant",
        "Aloft_pred"
    ))
}

.getVanillaDbnsfpConservationFields <- function() {
    return(c(
        "GERP++_NR",
        "GERP++_RS",
        "GERP++_RS_rankscore",
        "phyloP100way_vertebrate",
        "phyloP100way_vertebrate_rankscore",
        "phyloP470way_mammalian",
        "phyloP470way_mammalian_rankscore",
        "phyloP17way_primate",
        "phyloP17way_primate_rankscore",
        "phastCons100way_vertebrate",
        "phastCons100way_vertebrate_rankscore",
        "phastCons470way_mammalian",
        "phastCons470way_mammalian_rankscore",
        "phastCons17way_primate",
        "phastCons17way_primate_rankscore"
    ))
}

.getVanillaDbnsfpPopulationFields <- function() {
    return(c(
        "1000Gp3_AF",
        "1000Gp3_AC",
        "1000Gp3_EUR_AF",
        "1000Gp3_EUR_AC",
        "1000Gp3_AMR_AF",
        "1000Gp3_AMR_AC",
        "1000Gp3_EAS_AF",
        "1000Gp3_EAS_AC",
        "1000Gp3_SAS_AF",
        "1000Gp3_SAS_AC",
        "1000Gp3_AFR_AF",
        "1000Gp3_AFR_AC",
        "ExAC_AF",
        "ExAC_AC",
        "ExAC_Adj_AF",
        "ExAC_Adj_AC",
        "ExAC_NFE_AF",
        "ExAC_NFE_AC",
        "ExAC_FIN_AF",
        "ExAC_FIN_AC",
        "ExAC_AMR_AF",
        "ExAC_AMR_AC",
        "ExAC_EAS_AF",
        "ExAC_EAS_AC",
        "ExAC_SAS_AF",
        "ExAC_SAS_AC",
        "ExAC_AFR_AF",
        "ExAC_AFR_AC",
        "ESP6500_AA_AF",
        "ESP6500_AA_AC",
        "ESP6500_EA_AF",
        "ESP6500_EA_AC",
        "UK10K_AF",
        "UK10K_AC",
        "ALFA_European_AF",
        "ALFA_European_AC",
        "ALFA_African_Others_AF",
        "ALFA_African_Others_AC",
        "ALFA_East_Asian_AF",
        "ALFA_East_Asian_AC",
        "ALFA_African_American_AF",
        "ALFA_African_American_AC",
        "ALFA_Latin_American_1_AF",
        "ALFA_Latin_American_1_AC",
        "ALFA_Latin_American_2_AF",
        "ALFA_Latin_American_2_AC",
        "ALFA_Other_Asian_AF",
        "ALFA_Other_Asian_AC",
        "ALFA_South_Asian_AF",
        "ALFA_South_Asian_AC",
        "ALFA_Other_AF",
        "ALFA_Other_AC",
        "ALFA_African_AF",
        "ALFA_African_AC",
        "ALFA_Asian_AF",
        "ALFA_Asian_AC",
        "ALFA_Total_AF",
        "ALFA_Total_AC"
    ))
}

.gnomadExomeFieldsString <- function() {
    return(paste(.getGnomadExomeFields(),collapse=","))
}

.getGnomadExomeFields <- function() {
    return(c(   
        "AF",
        "AC",
        "AF_afr",
        "AC_afr",
        "AF_amr",
        "AC_amr",
        "AF_asj",
        "AC_asj",
        "AF_eas",
        "AC_eas",
        "AF_fin",
        "AC_fin",
        "AF_mid",
        "AC_mid",
        "AF_nfe",
        "AC_nfe",
        "AF_sas",
        "AC_sas",
        "AF_ESP",
        "AF_EXAC",
        "AF_TGP"
    ))
}

.gnomadGenomeFieldsString <- function() {
    return(paste(.getGnomadGenomeFields(),collapse=","))
}

.getGnomadGenomeFields <- function() {
    return(c(   
        "AF",
        "AC",
        "AF_afr",
        "AC_afr",
        "AF_amr",
        "AC_amr",
        "AF_asj",
        "AC_asj",
        "AF_eas",
        "AC_eas",
        "AF_fin",
        "AC_fin",
        "AF_nfe",
        "AC_nfe",
        "AF_oth",
        "AC_oth"
    ))
}
