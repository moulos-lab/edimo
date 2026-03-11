# Checks if dependencies are loaded, and installs and loads if needed
if (!require(VariantAnnotation, quietly = TRUE)) {
  install.packages("VariantAnnotation")
  library(VariantAnnotation)
}

# Function definitions ---------------------------------------------------------

# Getter function that returns default INFO, GENO fields to be retained
.getInfoGeno <- function() {
  info_fields <- c("AF","AO","DP","FR","FWDB","HS","RO","SAF","SAR","SRF","SRR",
                   "TYPE")
  geno_fields <- c("GT","DP","GQ")
  list(info_fields, geno_fields)
}

# Function that takes as input the path to a VCF file and the INFO, GENO fields
# to keep, and returns a filtered VCF file
filterInfoGeno <- function(vcf_path,
                           out_name = NULL,
                           info_fields = .getInfoGeno()[[1]],
                           geno_fields = .getInfoGeno()[[2]]) {
  
  # Check if user provided output path
  # If NULL use default /original/path/filename_base.vcf
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
  
  # Create an instance of ScanVcfParam that holds the desired fields
  params <- ScanVcfParam(info = info_fields, geno = geno_fields)

  # Read the VCF file keeping only desired fields and manually update header
  vcf_object <- readVcf(vcf_path, param = params)
  info(header(vcf_object)) <- info(header(vcf_object))[info_fields, ]
  geno(header(vcf_object)) <- geno(header(vcf_object))[geno_fields, ]

  # Write the filtered VCF
  writeVcf(vcf_object,
           paste0("C://Users/User/Desktop/", name, "_base.vcf"))
}

