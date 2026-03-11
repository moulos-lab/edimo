# Takes a non-multiallelic vcf file, standardizes fields, and splits vcf into 
# separate cnv and indel files.
processVCF <- function(vcf_path,
                       out_name = NULL) {

  # Check if user provided output basename
  # If NULL use original basename
  if (is.null(out_name)) {
    # Get the basename without extension
    name <- basename(vcf_path)
    name <- tools::file_path_sans_ext(name)    
  } else {
    # Check if user provided path is valid char object
    if (is.character(out_name)) {
      name <- out_name
    } else {
      # Throw error if non char is provided and STOP
      stop("Please provide a valid output path.")
    }
  }
  
  file_dir <- dirname(vcf_path)
  
  info_fields <- c("AC","AF","AO","DP","SBP","SRF","SRR","SAF","SAR")
  geno_fields <- c("GT","DP","GQ","SB")
  
  default_info_headers <- list(
    AC = list("A", "Integer", "Allele count in genotypes"),
    AF = list("A", "Float", "Allele Frequency"),
    AO = list("A", "Integer", "Alternate allele observations"),
    DP = list("1", "Integer", "Total read depth at the locus"),
    SBP = list("A","Float","Phred-scaled p-value of Strand bias"),
    SAF = list("A", "Integer", "Alternate allele observations on the forward strand"),
    SAR = list("A", "Integer", "Alternate allele observations on the reverse strand"),
    SRF = list("1", "Integer", "Number of reference observations on the forward strand"),
    SRR = list("1", "Integer", "Number of reference observations on the reverse strand")
  )
  default_geno_headers <- list(
    GT = list("1", "String", "Genotype"),
    DP = list("1", "Integer", "Read Depth"),
    GQ = list("1", "Integer", "Genotype Quality, the Phred-scaled marginal (or unconditional) probability of the called genotype"),
    SB = list("1", "Float", "Strand Bias")
  )
  
  vcf_object <- readVcf(vcf_path)
  
  if ("FS" %in% rownames(info(header(vcf_object)))) {
    info(vcf_object)[["SBP"]] <- info(vcf_object)[["FS"]]
  } else if ("STBP" %in% rownames(info(header(vcf_object)))) {
    info(vcf_object)[["SBP"]] <- -10 * log10(info(vcf_object)[["STBP"]])
  } else {
    stop(paste("Strand bias field not recognized."))
  }
  info(header(vcf_object))["SBP", ] <- default_info_headers[["SBP"]]
  
  for (field in info_fields) {
    if (!(field %in% rownames(info(header(vcf_object))))) {
      info(vcf_object)[[field]] <- rep(".", length(vcf_object))
      info(header(vcf_object))[field, ] <- default_info_headers[[field]]
    }
  }
  for (field in geno_fields) {
    if (!(field %in% rownames(geno(header(vcf_object))))) {
      geno(vcf_object)[[field]] <- rep(".", length(vcf_object))
      geno(header(vcf_object))[field, ] <- default_geno_headers[[field]]
    }
  }
  
  # Makes a list of the ALT field. Works incorrectly with multiallelic files
  alt_list <- lapply(as.list(alt(vcf_object)), as.character)
  
  vcf_cnv <- vcf_object[which(alt_list == "<CNV>")]
  vcf_indel <- vcf_object[which(alt_list != "<CNV>")]
  
  info(vcf_indel) <- info(vcf_indel)[info_fields]
  geno(vcf_indel) <- geno(vcf_indel)[geno_fields]
  info(header(vcf_indel)) <- info(header(vcf_indel))[info_fields, ]
  geno(header(vcf_indel)) <- geno(header(vcf_indel))[geno_fields, ]
  
  # Writes CNVs in separate vcf if they exist
  if (length(rownames(vcf_cnv)) > 0) {
    # Write the filtered VCF
    writeVcf(vcf_cnv, paste0(file_dir, "/", name, "_processed_cnv.vcf"))
  }
  writeVcf(vcf_indel, paste0(file_dir, "/", name, "_processed_indel.vcf"))
}
