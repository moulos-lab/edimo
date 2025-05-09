# It is assumed that the path where the vcfFile lives, is the path where all
# analyses will run and files will be produced
# An argument dv for database version will be added
annotateVcf <- function(vcfFile,gv=c("hg19","hg38"),aid=NULL) {
    log_info("Entering main VCF annotator")
    
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
    
    gv <- gv[1]
    .checkTextArgs("Genome version",gv,c("hg19","hg38"),multiarg=FALSE)
    nsteps <- .getNoAnalysisSteps("basic")
    
    # Working directory and base for outputs
    vcfDir <- dirname(vcfFile)
    
    #-1. Uncompress the vcf file as we nay do extensive modifications sometimes
    vcfFile <- .bgunzip(vcfFile)
    
    # 0. Check chromosome names and rename if necessary
    seqStyle <- .guessGenomeStyle(vcfFile)
    if (seqStyle == "UCSC")
        .chrRename(vcfFile,gv)
    else # Simply file copy to continue the flow
        cp <- file.copy(from=vcfFile,to=file.path(vcfDir,"00_chrom_rename.vcf"))
    # After each step, we will have to update analysis status in database so as
    # to display some progress    
    
    # 1. Normalize VCF (left alignement, break multi-allelics)
    .normalize(vcfDir,gv)
    
    # 2. Strip incoming VCF from unecessary fields for this app
    .strip(vcfDir)
    # Analysis step 1 complete - update progress
    .updateAnalysisProgress(aid,1,100*(1/nsteps),"VCF cleaned")
    
    # 3. Effect annotation with SnpEff
    .snpEff(vcfDir,gv)
    # Analysis step 2 complete - update progress
    .updateAnalysisProgress(aid,2,100*(2/nsteps),"Effects")
    
    # 4. SnpSift - Annotation with dbSNP
    .snpSiftDbSnp(vcfDir,gv)
    # Analysis step 3 complete - update progress
    .updateAnalysisProgress(aid,3,100*(3/nsteps),"dbSNP ids")
    
    # 5. SnpSift - Annotation with dbNSFP
    .snpSiftDbNsfp(vcfDir,gv)
    # Analysis step 4 complete - update progress
    .updateAnalysisProgress(aid,4,100*(4/nsteps),"Pathogenicities")
    
    # 6. SnpSift - Annotation with gnomAD exomes
    .snpSiftGnomadExomes(vcfDir,gv)
    
    # 7. SnpSift - Annotation with gnomAD genomes
    .snpSiftGnomadGenomes(vcfDir,gv)
    
    # 8. SnpSift - Annotation with ALFA
    .snpSiftAlfa(vcfDir,gv)
    
    # 9. SnpSift - Annotation with ClinVar and variant type
    .snpSiftClinvarVt(vcfDir,gv)
    # Analysis step 5 complete - update progress
    .updateAnalysisProgress(aid,5,100*(5/nsteps),"Populations")
    
    # Finally, create a copy of the last annotated file with a fixed name. This
    # allows adding further annotation steps later, e.g. ALFA.
    cp <- file.copy(
        #from=file.path(vcfDir,"08_clinvar.vcf"),
        from=file.path(vcfDir,"09_clinvar.vcf"),
        to=file.path(vcfDir,"annotated.vcf"),
        overwrite=TRUE
    )
    # ... and bgzip/tabix
    .bgzip(vcfDir)
    .tabix(vcfDir)
    # Analysis step 6 complete - update progress
    .updateAnalysisProgress(aid,6,100*(6/nsteps),"Indexing")
    
    log_info("Exiting main VCF annotator")
    return(file.path(vcfDir,"annotated.vcf.gz"))
}

.chrRename <- function(vcfFile,gv) {
    log_info("Entering chromosome name harmonizer")
    log_info("Renaming chromosomes from UCSC style to NCBI style in file ",
        vcfFile," for genome version ",gv)
    
    vcfDir <- dirname(vcfFile)
    out <- FALSE
    # Necessary tools
    bcftoolsData <- .getTool("bcftools")
    bcftools <- bcftoolsData$command
    # Necessary files
    chrRenameFile <- .getStaticFile(gv,"rename_map")
    outVcf <- file.path(vcfDir,"00_chrom_rename.vcf")
    
    # Construct human readable command to display - we need sed for (*&(*&@#)
    # older versions of IonReporter...
    sed <- Sys.which("sed")
    #humanCommand <- glue("
    #sed 's/\\\\[{\\\\(.*\\\\)}\\\\]/./g' {vcfFile} | \\\
    #{bcftools} annotate \\\ 
    #  --rename-chrs {chrRenameFile} \\\ 
    #  --output {outVcf}
    #")
    humanCommand <- paste(
       paste0(sed," 's/\\[{\\(.*\\)}\\]/./g' \\"),
       paste0("  ",vcfFile," | \\"),
       paste0(bcftools," annotate \\"),
       paste0("  --rename-chrs ",chrRenameFile," \\"),
       paste0("  --output ",outVcf),
       sep="\n"
    )
    # The same args for system2 - quite verbose but we need logs
    args <- c("'s/\\[{\\(.*\\)}\\]/./g'",vcfFile,"|",bcftools,
        "annotate","--rename-chrs",chrRenameFile,"--output",outVcf)
    # Define log file
    logfile <- file.path(vcfDir,"00_chrom_rename.log")
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2(sed,args=args,stdout=logfile,
            stderr=logfile))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    
    log_info("Exiting chromosome name harmonizer")
    if (!out) {
        cat(paste0("\nVCF chromosome renaming failed!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during VCF chromosome renaming.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF chromosome renaming finished!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
}

# TODO: Attach sed -i s/\#\#FORMAT\=\<ID\=AD\,Number\=\./\#\#FORMAT\=\<ID\=AD\,Number\=\R/ 000_chrom_rename.vcf
# to ensure that multi-allelic AD will be split
# In the end of the process we need to keep only canonical chrs on header
# grep -P "^##contig\=\<ID\=(1?[1-9]|1[0-9]|2[0-2]|X|Y|MT)" 67hdg.hard-filtered_stripped.vcf
.normalize <- function(vcfDir,gv) {
    log_info("Entering VCF variant normalizer")
    log_info("Normalizing file ",file.path(vcfDir,"00_chrom_rename.vcf"),
        ", refefence genome version is ",gv)

    out <- FALSE
    # Necessary tools
    bcftoolsData <- .getTool("bcftools")
    bcftools <- bcftoolsData$command
    # Necessary files
    genomeReference <- .getStaticFile(gv,"reference")
    inVcf <- file.path(vcfDir,"00_chrom_rename.vcf")
    outVcf <- file.path(vcfDir,"01_normalize.vcf")
    
    # Construct human readbale command to display
    sed <- Sys.which("sed")
    #humanCommand <- glue("
    #{bcftools} norm \\\ 
    #  --multiallelics -any \\\ 
    #  --fasta-ref {genomeReference} \\\ 
    #  --output-type v \\\ 
    #  --output {outVcf} \\\ 
    #  {inVcf}
    #")
    humanCommand <- paste(
        paste0(sed," s/\\#\\#FORMAT\\=\\<ID\\=AD\\,Number\\=\\.",
            "/\\#\\#FORMAT\\=\\<ID\\=AD\\,Number\\=\\R/ \\"),
        paste0("  ",inVcf," | \\"),
        paste0(bcftools," norm \\"),
        "  --multiallelics -any \\",
        paste0("  --fasta-ref ",genomeReference," \\"),
        "  --check-ref w \\",
        "  --output-type v \\",
        paste0("  --output ",outVcf),
        sep="\n"
    )
    # The same args for system2 - quite verbose but we need logs
    args <- c(paste0(" s/\\#\\#FORMAT\\=\\<ID\\=AD\\,Number\\=\\.",
        "/\\#\\#FORMAT\\=\\<ID\\=AD\\,Number\\=\\R/"),inVcf,"|",bcftools,
        "norm","--multiallelics","-any","--fasta-ref",genomeReference,
        "--check-ref","w","--output-type","v","--output",outVcf)
    # Define log file
    logfile <- file.path(vcfDir,"01_normalize.log")
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2(sed,args=args,stdout=logfile,
            stderr=logfile))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF variant normalizer")
    if (!out) {
        cat(paste0("\nVCF normalization failed!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during VCF normalization.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF normalization finished!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
}

.strip <- function(vcfDir) {
    log_info("Entering VCF file stripper and harmonizer")
    log_info("Harmonizing file ",file.path(vcfDir,"01_normalize.vcf"))

    out <- tryCatch({
        stripVcf(file.path(vcfDir,"01_normalize.vcf"))
        if (file.exists(file.path(vcfDir,"01_normalize_stripped.vcf"))) {
            file.rename(
                from=file.path(vcfDir,"01_normalize_stripped.vcf"),
                to=file.path(vcfDir,"02_stripped.vcf")
            )
        }
        else FALSE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file stripper and harmonizer")
    if (!out) {
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during VCF harmonization.")
        log_error(msg1)
        stop(msg1)
    }
}

.snpEff <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - genomic elements and effects")
    log_info("Annotating effect in file ",file.path(vcfDir,"02_stripped.vcf"),
        ", genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpeffData <- .getTool("snpeff")
    snpeff <- snpeffData$command
    bcftoolsData <- .getTool("bcftools")
    bcftools <- bcftoolsData$command
    # Necessary files
    snpeffDatabase <- .getStaticFile(gv,"snpeff")
    inVcf <- file.path(vcfDir,"02_stripped.vcf")
    outVcf <- file.path(vcfDir,"03_snpeff.vcf")
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpeff} ann \\\ 
      -v -noLog -noStats -noLof \\\ 
      -ud 2000 {snpeffDatabase} \\\ 
      {inVcf} | \\\ 
      {bcftools} annotate \\\ 
        --remove ID > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpEff STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"03_snpeff.log")
    # The same args for system2 - quite verbose but we need logs
    snpeff <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpeff,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpeff,"ann","-v","-noLog","-noStats","-noLof",
        "-ud","2000",#"-canon",
        snpeffDatabase,inVcf,"2>",logfile,"|",bcftools,"annotate","--remove",
        "ID",">",outVcf)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - genomic elements and effects")
    if (!out) {
        cat(paste0("\nVCF SnpEff annotation failed!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    cat(paste0("\nSnpEff annotation complete!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
}

.snpSiftDbSnp <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - dbSNP")
    log_info("Annotating file ",file.path(vcfDir,"03_snpeff.vcf"),
        " with dbSNP, genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    dbsnp <- .getStaticFile(gv,"dbsnp")
    inVcf <- file.path(vcfDir,"03_snpeff.vcf")
    outVcf <- file.path(vcfDir,"04_dbsnp.vcf")
    
    # Construct human readable command to display
    humanCommand <- glue("
    {snpsift} annotate \\\ 
      -v -noInfo \\\ 
      -id {dbsnp} \\\ 
      {inVcf} > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"04_dbsnp.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"annotate","-v","-noInfo","-id",
        dbsnp,inVcf,">",outVcf,"2>",logfile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - dbSNP")
    if (!out) {
        cat(paste0("\nVCF SnpSift dbSNP annotation failed!\nThe command was:\n",
            humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff dbSNP annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift dbSNP annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.snpSiftDbNsfp <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - dbNSFP")
    log_info("Annotating file ",file.path(vcfDir,"04_dbsnp.vcf"),
        " with dbNSFP, genome version is ",gv)
    
    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    dbnsfp <- .getStaticFile(gv,"dbnsfp")
    inVcf <- file.path(vcfDir,"04_dbsnp.vcf")
    outVcf <- file.path(vcfDir,"05_dbnsfp.vcf")
    # dbSNFP fields
    dbnsfpFields <- .dbnsfpFieldsString()
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpsift} dbnsfp \\\ 
      -v -m -collapse -f {dbnsfpFields} \\\ 
      -db {dbnsfp} \\\ 
      {inVcf} > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"05_dbnsfp.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"dbnsfp","-v","-m","-collapse","-f",
        dbnsfpFields,"-db",dbnsfp,inVcf,">",outVcf,"2>",logfile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - dbNSFP")
    if (!out) {
        cat(paste0("\nVCF SnpSift dbNSFP annotation failed!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff dbNSFP annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift dbNSFP annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.snpSiftGnomadExomes <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - gnomAD exomes")
    log_info("Annotating file ",file.path(vcfDir,"05_dbnsfp.vcf"),
        " with gnomAD exomes, genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    gnomadExomes <- .getStaticFile(gv,"gnomad_exomes")
    inVcf <- file.path(vcfDir,"05_dbnsfp.vcf")
    outVcf <- file.path(vcfDir,"06_gnomad_exomes.vcf")
    # dbSNFP fields
    gnomadExomesFields <- .gnomadExomeFieldsString()
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpsift} annotate \\\ 
      -v {gnomadExomes} \\\ 
      -info {gnomadExomesFields} \\\ 
      -name gnomAD_exomes_ \\\ 
      {inVcf} > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"06_gnomad_exomes.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"annotate","-v",gnomadExomes,"-info",
        gnomadExomesFields,"-name","gnomAD_exomes_",inVcf,">",outVcf,
        "2>",logfile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - gnomAD exomes")
    if (!out) {
        cat(paste0("\nVCF SnpSift gnomAD exomes annotation failed!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff gnomAD exomes annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift gnomAD exomes annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.snpSiftGnomadGenomes <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - gnomAD genomes")
    log_info("Annotating file ",file.path(vcfDir,"06_gnomad_exomes.vcf"),
        " with gnomAD genomes, genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    gnomadGenomes <- .getStaticFile(gv,"gnomad_genomes")
    inVcf <- file.path(vcfDir,"06_gnomad_exomes.vcf")
    outVcf <- file.path(vcfDir,"07_gnomad_genomes.vcf")
    # dbSNFP fields
    gnomadGenomesFields <- .gnomadGenomeFieldsString()
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpsift} annotate \\\ 
      -v {gnomadGenomes} \\\ 
      -info {gnomadGenomesFields} \\\ 
      -name gnomAD_genomes_ \\\ 
      {inVcf} > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"07_gnomad_genomes.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"annotate","-v",gnomadGenomes,"-info",
        gnomadGenomesFields,"-name","gnomAD_genomes_",inVcf,">",outVcf,"2>",
        logfile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - gnomAD genomes")
    if (!out) {
        cat(paste0("\nVCF SnpSift gnomAD genomes annotation failed!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff gnomAD exomes annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift gnomAD genomes annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.snpSiftAlfa <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - ALFA")
    log_info("Annotating file ",file.path(vcfDir,"07_gnomad_genomes.vcf"),
        " with ALFA frequenices, genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    alfa <- .getStaticFile(gv,"alfa")
    inVcf <- file.path(vcfDir,"07_gnomad_genomes.vcf")
    outVcf <- file.path(vcfDir,"08_alfa.vcf")
    # dbSNFP fields
    alfaFields <- .alfaFieldsString()
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpsift} annotate \\\ 
      -v {alfa} \\\ 
      -info {alfaFields} \\\ 
      -name ALFA_ \\\ 
      {inVcf} > \\\ 
      {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"08_alfa.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"annotate","-v",alfa,"-info",
        alfaFields,"-name","ALFA_",inVcf,">",outVcf,"2>",logfile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - ALFA")
    if (!out) {
        cat(paste0("\nVCF SnpSift ALFA annotation failed!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff ALFA annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift ALFA annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.snpSiftClinvarVt <- function(vcfDir,gv) {
    log_info("Entering VCF file annotator - ClinVar and variant type")
    log_info("Annotating file ",file.path(vcfDir,"06_gnomad_genomes.vcf"),
        " with ClinVar, genome version is ",gv)

    out <- FALSE
    # Necessary tools
    snpsiftData <- .getTool("snpsift")
    snpsift <- snpsiftData$command
    # Necessary files
    clinvar <- .getStaticFile(gv,"clinvar")
    #inVcf <- file.path(vcfDir,"07_gnomad_genomes.vcf")
    #outVcf <- file.path(vcfDir,"08_clinvar.vcf")
    inVcf <- file.path(vcfDir,"08_alfa.vcf")
    outVcf <- file.path(vcfDir,"09_clinvar.vcf")
    # dbSNFP fields
    clinvarFields <- .clinvarFieldsString()
    
    # Construct human readbale command to display
    humanCommand <- glue("
    {snpsift} annotate \\\ 
      -noId -v {clinvar} \\\ 
      -info {clinvarFields} \\\ 
      -name ClinVar_ \\\ 
      {inVcf} | \\\ 
     {snpsift} varType - > {outVcf}
    ")
    # Define log file - needed in args to explicitly redirect SnpSift STDERR
    # (system2 argument does not work with Java)
    logfile <- file.path(vcfDir,"08_clinvar.log")
    #logfile <- file.path(vcfDir,"09_clinvar.log")
    # The same args for system2 - quite verbose but we need logs
    snpsift <- sub("^java(?: -\\S+)?(?: -jar)?\\s*","",snpsift,perl=TRUE)
    args <- c("-Xmx4096m","-jar",snpsift,"annotate","-noId","-v",clinvar,
        "-info",clinvarFields,"-name","ClinVar_",inVcf,"2>",logfile,"|",
        "java","-Xmx4096m","-jar",snpsift,"varType","-",">",outVcf)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2("java",args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file annotator - ClinVar and variant type")
    if (!out) {
        cat(paste0("\nVCF SnpSift ClinVar annotation failed!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during SnpEff ClinVar annotation.")
        msg2 <- paste0("The log is at ",logfile)
        log_error(msg1)
        log_error(msg2)
        stop(paste(msg1,msg2,collapse="\n"))
    }
    else
        cat(paste0("\nVCF SnpSift ClinVar annotation finished!",
            "\nThe command was:\n",humanCommand),file=logfile,append=TRUE)
}

.bgzip <- function(vcfDir) {
    log_info("Entering VCF file compressor")
    log_info("Compressing file ",file.path(vcfDir,"annotated.vcf"),
        " with bgzip")

    out <- FALSE
    # Necessary tools
    bgzipData <- .getTool("bgzip")
    bgzip <- bgzipData$command
    # Necessary files
    inVcf <- file.path(vcfDir,"annotated.vcf")
    
    # Construct human readbale command to display
    humanCommand <- glue("{bgzip} --force {inVcf}")
    args <- c("--force",inVcf)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2(bgzip,args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file compressor")
    if (!out) {
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during compressing the final VCF.")
        log_error(msg1)
        stop(msg1)
    }
}

.tabix <- function(vcfDir) {
    log_info("Entering VCF file indexer")
    log_info("Indexing file ",file.path(vcfDir,"annotated.vcf.gz"),
        " with tabix")

    out <- FALSE
    # Necessary tools
    tabixData <- .getTool("tabix")
    tabix <- tabixData$command
    # Necessary files
    inVcf <- file.path(vcfDir,"annotated.vcf.gz")
    
    # Construct human readbale command to display
    humanCommand <- glue("{tabix} --force {inVcf}")
    args <- c("--force",inVcf)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2(tabix,args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file indexer")
    if (!out) {
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during indexing the final VCF.")
        log_error(msg1)
        stop(msg1)
    }
}

.bgunzip <- function(vcfFile) {
    log_info("Entering VCF file decompressor")
    
    compression <- grepl("[.](gz|bz2|xz)$",vcfFile)
    if (!compression) {
        log_info("VCF file ",vcfFile," is not compressed, nothing to do")
        log_info("Exiting VCF file compressor")
        return(vcfFile)
    }
    
    log_info("Decompressing file ",vcfFile," with bgzip")
    
    out <- FALSE
    # Necessary tools
    bgzipData <- .getTool("bgzip")
    bgzip <- bgzipData$command
    
    # Construct human readbale command to display
    humanCommand <- glue("{bgzip} --force --decompress {vcfFile}")
    args <- c("--force","--decompress",vcfFile)
    
    message("Executing:\n",humanCommand)
    out <- tryCatch({
        suppressWarnings(system2(bgzip,args=args))
        TRUE
    },error=function(e) {
        message("Caught error: ",e$message)
        return(FALSE)
    },finally="")
    
    log_info("Exiting VCF file compressor")
    if (!out) {
        msg1 <- paste0("VCF file annotation failed! The error most likely ",
            "occured during decompressing the original VCF.")
        log_error(msg1)
        stop(msg1)
    }
    
    return(sub("[.](gz|bz2|xz)$","",vcfFile))
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
    
    #!# header is masked from other loaded libraries... This should be fixed
    #!# later when we have full package and namespaces controlled by imports.
    #..header <- VariantAnnotation::header
    
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
    
    log_info("Reading VCF file ",vcfFile)
    vcf <- readVcf(vcfFile)
    
    log_info("Stripping extra fields and harmonizing")
    # Firstly keep standard chromomosomes, as problems may arise later
    # Remove from seqlevels/seqinfo
    cind <- grep("^(1?[1-9]|1[0-9]|2[0-2]|X|Y|MT)$",seqlevels(vcf),perl=TRUE)
    seqlevels(vcf) <- seqlevels(vcf)[cind]
    # Remove from vcf object header metadata, otherwise written in output
    mind <- grep("^(1?[1-9]|1[0-9]|2[0-2]|X|Y|MT)$",
        rownames(meta(header(vcf))$contig),perl=TRUE)
    meta(header(vcf))$contig <- meta(header(vcf))$contig[mind,,drop=FALSE]
    # Remove from actual variants - if any
    citf <- grepl("^(1?[1-9]|1[0-9]|2[0-2]|X|Y|MT)$",seqnames(vcf),perl=TRUE)
    vcf <- vcf[as.logical(citf)]
    
    # Harmonize strand bias - add a common header
    info(VariantAnnotation::header(vcf))["SBP", ] <- infoHeaders[["SBP"]]
    # Possible strand bias fields are FS, SBP and STBP
    if ("FS" %in% rownames(info(VariantAnnotation::header(vcf))))
        info(vcf)[["SBP"]] <- info(vcf)[["FS"]]
    else if ("STBP" %in% rownames(info(VariantAnnotation::header(vcf))))
        info(vcf)[["SBP"]] <- -10*log10(info(vcf)[["STBP"]])
    else {
        warning("Strand bias field unavailable!",immediate.=TRUE)
        info(vcf)[["SBP"]] <- rep(".",length(vcf))
    }
    
    # Harmonize INFO fields
    for (field in infoFields) {
        if (!(field %in% rownames(info(VariantAnnotation::header(vcf))))) {
            info(VariantAnnotation::header(vcf))[field, ] <- 
                infoHeaders[[field]]
            info(vcf)[[field]] <- rep(".",length(vcf))
        }
    }
    
    # Harmonize FORMAT fields - add to checkVcf some of the fields below
    for (field in genoFields) {
        if (!(field %in% rownames(geno(VariantAnnotation::header(vcf))))) {
            geno(VariantAnnotation::header(vcf))[field,] <- 
                genoHeaders[[field]]
            
            # Examine different cases, more may be added
            if (field == "AF") {
                # Case 1 - Illumina files w/o AF
                DP <- geno(vcf)[["DP"]][,1]
                if ("AD" %in% names(geno(vcf))) { # Illumina/GATK flavor
                    AD <- do.call("rbind",geno(vcf)[["AD"]][,1])
                    AF <- round(AD[,2]/DP,5)
                }
                # Case 2 - Thermo/FreeBayes files w/o AF    
                else if ("AO" %in% names(geno(vcf))) { # Thermo/FreeBayes flavor
                    AO <- geno(vcf)[["AO"]][,1]
                    AF <- round(AO/DP,5)
                }
                geno(vcf)[["AF"]] <- as.matrix(AF)
            }
            else if (field == "AD") { # It should be having AO
                DP <- geno(vcf)[["DP"]][,1]
                if ("AO" %in% names(geno(vcf))) {
                    AO <- geno(vcf)[["AO"]][,1]
                    # Need to create a character list of 2 as AD
                    AD <- lapply(seq_along(AO),function(i,ao,dp) {
                        y <- numeric(2)
                        y[1] <- dp[i] - ao[[i]][1] # Ref
                        y[2] <- ao[[i]][1]         # Alt
                        return(y)
                    },AO,DP)
                    names(AD) <- names(AO)
                    geno(vcf)[["AD"]] <- as.matrix(AD)
                }
                else {
                    log_error("Cannot extract harmonization information!")
                    stop("Cannot extract harmonization information!")
                }
            }
            else
                geno(vcf)[[field]] <- as.matrix(rep(".", length(vcf)))
        }
    }
    
    # Makes a list of the ALT field. Works incorrectly with multiallelic files
    altList <- lapply(as.list(alt(vcf)),as.character)

    vcfCnv <- vcf[which(altList=="<CNV>")]
    vcfIndel <- vcf[which(altList!="<CNV>")]

    info(vcfIndel) <- info(vcfIndel)[infoFields]
    geno(vcfIndel) <- geno(vcfIndel)[genoFields]
    info(VariantAnnotation::header(vcfIndel)) <- 
        info(VariantAnnotation::header(vcfIndel))[infoFields, ]
    geno(VariantAnnotation::header(vcfIndel)) <- 
        geno(VariantAnnotation::header(vcfIndel))[genoFields, ]

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
    return(c("GT","DP","AD","AF","GQ","SB"))
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
        AD=list("R","Integer",paste0("Allelic depths for the ref and alt ",
            "alleles in the order listed")),
        AF=list("A","Float","Allele frequency for sample"),
        GQ=list("1","Integer",paste0("Genotype Quality, the Phred-scaled ",
            "marginal (or unconditional) probability of the called genotype")),
        SB=list("1","Float","Strand Bias")
    ))
}

.dbnsfpFieldsString <- function() {
    return(paste(.getVanillaDbnsfpFields(),collapse=","))
}

.getVcfDbnsfpFields <- function() {
    return(paste("dbNSFP",.getVanillaDbnsfpFields(),sep="_"))
}

.getVcfDbnsfpPathogenicityFields <- function() {
    return(paste("dbNSFP",.getVanillaDbnsfpPathogenicityFields(),sep="_"))
}

.getVcfDbnsfpConservationFields <- function() {
    return(paste("dbNSFP",.getVanillaDbnsfpConservationFields(),sep="_"))
}

.getVcfDbnsfpPopulationFields <- function() {
    return(paste("dbNSFP",.getVanillaDbnsfpPopulationFields(),sep="_"))
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
        "M-CAP_score",
        "M-CAP_rankscore",
        "M-CAP_pred",
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
    return(paste(.getVanillaGnomadExomeFields(),collapse=","))
}

.getVcfGnomadExomeFields <- function() {
    return(paste("gnomAD_exomes",.getVanillaGnomadExomeFields(),sep="_"))
}

.getVanillaGnomadExomeFields <- function() {
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
        "AC_sas"#,
        #"AF_ESP",
        #"AF_EXAC",
        #"AF_TGP"
    ))
}

.gnomadGenomeFieldsString <- function() {
    return(paste(.getVanillaGnomadGenomeFields(),collapse=","))
}

.getVcfGnomadGenomeFields <- function() {
    return(paste("gnomAD_genomes",.getVanillaGnomadGenomeFields(),sep="_"))
}

.getVanillaGnomadGenomeFields <- function() {
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
        "AC_nfe"#,
        #"AF_oth",
        #"AC_oth"
    ))
}

.clinvarFieldsString <- function() {
    return(paste(.getVanillaClinvarFields(),collapse=","))
}

.getVcfClinvarFields <- function() {
    return(paste("ClinVar",.getVanillaClinvarFields(),sep="_"))
}

.getVanillaClinvarFields <- function() {
    return(c(
        "ALLELEID",
        "CLNSIG",
        "ONC"
    ))
}

.alfaFieldsString <- function() {
    return(paste(.getVanillaAlfaFields(),collapse=","))
}

.getVcfAlfaFields <- function() {
    return(paste("ALFA",.getVanillaAlfaFields(),sep="_"))
}

.getVanillaAlfaFields <- function() {
    return(c(   
        "European_AF",
        "European_AC",
        "African_Others_AF",
        "African_Others_AC",
        "East_Asian_AF",
        "East_Asian_AC",
        "African_American_AF",
        "African_American_AC",
        "Latin_American_1_AF",
        "Latin_American_1_AC",
        "Latin_American_2_AF",
        "Latin_American_2_AC",
        "Other_Asian_AF",
        "Other_Asian_AC",
        "South_Asian_AF",
        "South_Asian_AC",
        "Other_AF",
        "Other_AC",
        "African_AF",
        "African_AC",
        "Asian_AF",
        "Asian_AC",
        "Total_AF",
        "Total_AC"
        #"EUROPEAN_AF",
        #"EUROPEAN_AC",
        #"AFRICAN_OTHERS_AF",
        #"AFRICAN_OTHERS_AC",
        #"EAST_ASIAN_AF",
        #"EAST_ASIAN_AC",
        #"AFRICAN_AMERICAN_AF",
        #"AFRICAN_AMERICAN_AC",
        #"LATIN_AMERICAN_1_AF",
        #"LATIN_AMERICAN_1_AC",
        #"LATIN_AMERICAN_2_AF",
        #"LATIN_AMERICAN_2_AC",
        #"OTHER_ASIAN_AF",
        #"OTHER_ASIAN_AC",
        #"SOUTH_ASIAN_AF",
        #"SOUTH_ASIAN_AC",
        #"OTHER_AF",
        #"OTHER_AC",
        #"AFRICAN_AF",
        #"AFRICAN_AC",
        #"ASIAN_AF",
        #"ASIAN_AC",
        #"TOTAL_AF",
        #"TOTAL_AC"
    ))
}
