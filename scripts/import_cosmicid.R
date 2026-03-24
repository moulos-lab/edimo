library(data.table)
library(mongolite)

# Function to process and prepare coding or targeted CosmicID data
.processCodingOrTargetedData <- function(file, genome) {
  cosmic_data <- fread(file, header = TRUE, sep = "\t", showProgress = FALSE, select = c(
    "GENOMIC_MUTATION_ID", "TRANSCRIPT_ACCESSION",
    "MUTATION_CDS", "MUTATION_AA", "HGVSG",
    "GENE_SYMBOL", "MUTATION_DESCRIPTION",
    "CHROMOSOME", "GENOME_START", "GENOME_STOP",
    "STRAND", "GENOMIC_WT_ALLELE", "GENOMIC_MUT_ALLELE"
  ))

  if (genome == "grch37") {
    colnames(cosmic_data) <- c("cosmic_id", "trans_id_37", "mut_cds", "mut_aa",
                               "hgvsg_37", "gene", "descr", "chr",
                               "start_pos_37", "stop_pos_37",
                               "strand", "ref", "alt")
  } else if (genome == "grch38") {
    colnames(cosmic_data) <- c("cosmic_id", "trans_id_38", "mut_cds", "mut_aa",
                               "hgvsg_38", "gene", "descr", "chr",
                               "start_pos_38", "stop_pos_38", 
                               "strand", "ref", "alt")
  } else {
    stop("Unrecognized genome specified.")
  }

  # Replace missing values with NA so they display correctly in mongoDB
  cosmic_data[cosmic_data == ''] <- NA

  cosmic_data <- na.omit(cosmic_data, cols = "cosmic_id")

  cosmic_data <- unique(cosmic_data)

  return(cosmic_data)
}

# Function to process and prepare non-coding CosmicID data
.processNoncodingData <- function(file, genome) {
  cosmic_data <- fread(file, header = TRUE, sep = "\t", showProgress = FALSE, select = c(
    "GENOMIC_MUTATION_ID", "HGVSG", "CHROMOSOME",
    "GENOME_START", "GENOME_STOP",
    "GENOMIC_WT_ALLELE", "GENOMIC_MUT_ALLELE"
  ))

  if (genome == "grch37") {
    colnames(cosmic_data) <- c("cosmic_id", "hgvsg_37", "chr", 
                               "start_pos_37", "stop_pos_37", "ref", "alt")
  } else if (genome == "grch38") {
    colnames(cosmic_data) <- c("cosmic_id", "hgvsg_38", "chr",
                               "start_pos_38", "stop_pos_38", "ref", "alt")
  } else {
    stop("Unrecognized genome specified.")
  }

  # Replace missing values with NA so they display correctly in mongoDB
  cosmic_data[cosmic_data == ''] <- NA

  cosmic_data <- na.omit(cosmic_data, cols = "cosmic_id")

  cosmic_data <- unique(cosmic_data)

  return(cosmic_data)
}

# Function to perform liftOver
performLiftover <- function(data, chain_file, from_version, to_version) {
  # Create GRanges object from data
  gr <- GRanges(
    seqnames = data$chr,
    ranges = IRanges(start = data$start_pos, end = data$stop_pos),
    CosmicID = data$cosmic_id
  )
  
  # Load the chain file
  chain <- import.chain(chain_file)
  
  # Perform liftOver
  lifted <- liftOver(gr, chain)
  
  # Flatten the result (if multiple mappings, keep only the first for simplicity)
  lifted <- unlist(lifted)
  
  # Convert back to data.table
  lifted_dt <- data.table(
    cosmic_id = mcols(lifted)$CosmicID,
    chr = as.character(seqnames(lifted)),
    start_pos = start(lifted),
    stop_pos = end(lifted),
    genome_version = to_version
  )
  
  return(lifted_dt)
}

# Function to import CosmicID Point mutations
importCosmicPoint <- function(file_coding_grch37, 
                              file_coding_grch38, 
                              file_noncoding_grch37, 
                              file_noncoding_grch38, 
                              file_targeted_grch37, 
                              file_targeted_grch38, 
                              # ver, db_connection) {
                              ver, db_connection, hg19_to_hg38_chain, hg38_to_hg19_chain) {  
  # Read and process coding CosmicID Point Mutations for GRCh37
  message("Loading Coding CosmicID Point Mutations for GRCh37...")
  cosmic_coding_grch37 <-
    .processCodingOrTargetedData(file_coding_grch37, "grch37")
  # Read and process coding CosmicID Point Mutations for GRCh38
  message("Loading Coding CosmicID Point Mutations for GRCh38...")
  cosmic_coding_grch38 <-
    .processCodingOrTargetedData(file_coding_grch38, "grch38")

  cosmic_coding_grch37 <- 
    cosmic_coding_grch37[, .(trans_ids_37 = unlist(.SD)), 
                         by = .(cosmic_id, mut_cds, mut_aa, descr, strand, chr,
                                ref, alt, hgvsg_37, start_pos_37, stop_pos_37, gene)]
  cosmic_coding_grch38 <- 
    cosmic_coding_grch38[, .(trans_ids_38 = unlist(.SD)), 
                         by = .(cosmic_id, mut_cds, mut_aa, descr, strand, chr,
                                ref, alt, hgvsg_38, start_pos_38, stop_pos_38, gene)]
  
  # Merging coding data into complete database
  message("Merging coding data into complete database...")
  cosmic_coding_data <- merge(cosmic_coding_grch37, cosmic_coding_grch38,
                       by = c("cosmic_id", "mut_cds", "mut_aa",
                              "gene", "descr", "chr", "strand", "ref", "alt"),
                       all = TRUE, allow.cartesian=TRUE)

  # Create the trans_id column
  cosmic_coding_data [, trans_id := ifelse(!is.na(trans_ids_38), trans_ids_38, trans_ids_37)]
  
  # Remove the trans_id_37 and trans_id_38 columns
  cosmic_coding_data [, c("trans_ids_37", "trans_ids_38") := NULL]


  # Read and process non-coding CosmicID Point Mutations for GRCh37
  message("Loading Non-coding CosmicID Point Mutations for GRCh37...")
  cosmic_noncoding_grch37 <-
    .processNoncodingData(file_noncoding_grch37, "grch37")
  # Read and process non-coding CosmicID Point Mutations for GRCh38
  message("Loading Non-coding CosmicID Point Mutations for GRCh38...")
  cosmic_noncoding_grch38 <-
    .processNoncodingData(file_noncoding_grch38, "grch38")

#  # Add empty columns so it can be merged with coding data
#  cosmic_noncoding_grch37[, c("trans_id", "mut_cds", "mut_aa", "gene", "descr", "strand") := 
#                            .(NA, NA, NA, NA, NA, NA)]
  
#  cosmic_noncoding_grch38[, c("trans_id", "mut_cds", "mut_aa", "gene", "descr", "strand") := 
#                            .(NA, NA, NA, NA, NA, NA)]
  
  # # Merging noncoding data into the complete database
  # message("Merging noncoding data into the complete database...")
  # cosmic_data <- rbindlist(list(
  #   cosmic_coding_data,
  #   merge(cosmic_noncoding_grch37, cosmic_noncoding_grch38,
  #         by = c("cosmic_id", "trans_id", "mut_cds", "mut_aa",
  #                "gene", "descr", "chr", "strand", "ref", "alt"),
  #         all = TRUE)
  # ), use.names=TRUE, fill=TRUE)

  # Merging noncoding data into the complete database
  message("Merging noncoding data into the complete database...")
  cosmic_data <- rbindlist(list(
    cosmic_coding_data,
    merge(cosmic_noncoding_grch37, cosmic_noncoding_grch38,
          by = c("cosmic_id", "chr", "ref", "alt"),
          all = TRUE)
  ), use.names=TRUE, fill=TRUE)
  
  
  # Read and process targeted CosmicID Point Mutations for GRCh37
  message("Loading Targeted CosmicID Point Mutations for GRCh37...")
  cosmic_targeted_grch37 <-
    .processCodingOrTargetedData(file_targeted_grch37, "grch37")
  # Read and process targeted CosmicID Point Mutations for GRCh38
  message("Loading Targeted CosmicID Point Mutations for GRCh38...")
  cosmic_targeted_grch38 <-
    .processCodingOrTargetedData(file_targeted_grch38, "grch38")
  
  cosmic_targeted_grch37 <- 
    cosmic_targeted_grch37[, .(trans_ids_37 = unlist(.SD)), 
                           by = .(cosmic_id, mut_cds, mut_aa, descr, strand, chr,
                                  ref, alt, hgvsg_37, start_pos_37, stop_pos_37, gene)]
  cosmic_targeted_grch38 <- 
    cosmic_targeted_grch38[, .(trans_ids_38 = unlist(.SD)), 
                           by = .(cosmic_id, mut_cds, mut_aa, descr, strand, chr,
                                  ref, alt, hgvsg_38, start_pos_38, stop_pos_38, gene)]
  
  # Merging targeted data from genome versions
  message("Merging coding data into complete database...")
  cosmic_targeted_data <- merge(cosmic_targeted_grch37, cosmic_targeted_grch38,
                       by = c("cosmic_id", "mut_cds", "mut_aa",
                              "gene", "descr", "chr", "strand", "ref", "alt"),
                       all = TRUE, allow.cartesian=TRUE)

  # Create the trans_id column
  cosmic_targeted_data [, trans_id := ifelse(!is.na(trans_ids_38), trans_ids_38, trans_ids_37)]
  
  # Remove the trans_id_37 and trans_id_38 columns
  cosmic_targeted_data [, c("trans_ids_37", "trans_ids_38") := NULL]
  
  # Merging targeted data into the complete database
  cosmic_data <- rbindlist(list(cosmic_data, cosmic_targeted_data))

  # Remove duplicate entries stemming from targeted and (non-)coding overlap
  message("Removing duplicates...")
  cosmic_data <- unique(cosmic_data)


  # Identify missing entries in each version
  missing_in_grch37 <- cosmic_data[is.na(start_pos_37)]
  missing_in_grch38 <- cosmic_data[is.na(start_pos_38)]

  # Perform liftOver
  lifted_to_grch37 <- performLiftover(
    data = missing_in_grch37[, .(cosmic_id, chr, start_pos_38, stop_pos_38)],
    chain_file = hg38_to_hg19_chain,
    from_version = "GRCh38",
    to_version = "GRCh37"
  )
  lifted_to_grch38 <- performLiftover(
    data = missing_in_grch38[, .(cosmic_id, chr, start_pos_37, stop_pos_37)],
    chain_file = hg19_to_hg38_chain,
    from_version = "GRCh37",
    to_version = "GRCh38"
  )

  # Combine original and lifted data
  cosmic_data <- rbindlist(list(cosmic_data, lifted_to_grch37, lifted_to_grch38),
                           use.names = TRUE, fill = TRUE)


  message("Grouping transcripts...")
  # Nesting by cosmic_id, chr, ref, alt, hgvsg_37, hgvsg_38, start_pos_37, stop_pos_37, start_pos_38, stop_pos_38, gene
  cosmic_data <- cosmic_data[, .(transcripts = list(.SD)), 
                             by = .(cosmic_id, chr, ref, alt, hgvsg_37, hgvsg_38, 
                                    start_pos_37, stop_pos_37, start_pos_38, stop_pos_38, gene)]
  message("Grouping genes...")
  # Nesting by cosmic_id, chr, ref, alt, hgvsg_37, hgvsg_38, start_pos_37, stop_pos_37, start_pos_38, stop_pos_38
  cosmic_data <- cosmic_data[, .(genes = list(.SD)), 
                             by = .(cosmic_id, chr, ref, alt, hgvsg_37, hgvsg_38, 
                                    start_pos_37, stop_pos_37, start_pos_38, stop_pos_38)]

  message("Adding version column...")
  # Add version column
  cosmic_data <- cosmic_data[, version := ver]

  # Rearrange columns
  cosmic_data <- cosmic_data[, c("cosmic_id", "version", "genes", "hgvsg_37", "hgvsg_38", 
                                 "ref", "alt", "chr", "start_pos_37", 
                                 "stop_pos_37", "start_pos_38", "stop_pos_38")]

  # Directly filter nested data.tables in the 'genes' column
  cosmic_data[, genes := lapply(genes, function(dt) {
    dt[!is.na(gene)]
  })]
  
#  # A function that returns an empty 'genes' list, if no genes are identified
#  replace_na_gene <- function(tbl) {
#    if (is.na(tbl[["gene"]][[1]])) {
#      tbl <- list()
#    }
#    return(tbl)
#  }

#  message("Removing empty genes entries...")
#  # Apply the function to each tibble in cosmic_data$genes
#  cosmic_data[, genes := lapply(genes, replace_na_gene)]

  # Insert into database
  message("Inserting CosmicID Point Mutations DB...\n")
  db_connection$insert(cosmic_data, auto_unbox = TRUE, na = 'null')
}

# Function to import CosmicID structural mutations
# For now GRCh37 and GRCh38 versions have to imported into separate entries in the same collection
# In R, extract_pos() returns lists, but in mongoDB they are int32.
importCosmicStructural <- function(file, gen, ver, db_connection) {
  # Display message
  message("Loading CosmicID Structural Mutations for genome ", gen, "...")
  
  # Read data from file
  cosmic_data <- fread(file, header = TRUE, sep = "\t", showProgress = FALSE, select = c(
    "COSMIC_STRUCTURAL_ID", "MUTATION_TYPE",
    "DESCRIPTION", "CHROMOSOME_FROM", "CHROMOSOME_TO",
    "LOCATION_FROM_MIN", "LOCATION_FROM_MAX",
    "LOCATION_TO_MIN", "LOCATION_TO_MAX",
    "STRAND_FROM", "STRAND_TO"
  ))
  
  colnames(cosmic_data) <- c("cosmic_id", "mut_type", "descr", "chr_from", "chr_to", 
                             "pos_from_min", "pos_from_max",
                             "pos_to_min", "pos_to_max", 
                             "strand_from", "strand_to")
  
  # Remove duplicate entries
  cosmic_data <- unique(cosmic_data)

  # Add genome and version columns
  cosmic_data[, c("genome", "version") := .(gen, ver)]

  # Define a function to extract the correct position values 
  .extract_pos <- function(x) {
    if (is.na(x)) {
      return(NA)  # Return NA if the value is NA
    }
    
    if (grepl(":", x)) {
      # If the value is of the form "NW_923984:53945503", extract the second part
      return(as.integer(strsplit(x, ":")[[1]][2]))
    } else {
      # Otherwise, assume it's already an integer
      return(as.integer(x))
    }
  }

  # Apply the function to relevant columns
  cosmic_data[, c("chr_from", "chr_to") := lapply(.SD, as.character), .SDcols = c("chr_from", "chr_to")]
  cosmic_data[["pos_from_min"]] <- lapply(cosmic_data[["pos_from_min"]], .extract_pos)
  cosmic_data[["pos_from_max"]] <- lapply(cosmic_data[["pos_from_max"]], .extract_pos)
  cosmic_data[["pos_to_min"]] <- lapply(cosmic_data[["pos_to_min"]], .extract_pos)
  cosmic_data[["pos_to_max"]] <- lapply(cosmic_data[["pos_to_max"]], .extract_pos)
  
  # Rearrange columns
  setcolorder(cosmic_data, c("cosmic_id", "version", "mut_type",
                              "descr", "chr_from", "chr_to",
                              "pos_from_min", "pos_from_max",
                              "pos_to_min", "pos_to_max",
                              "strand_from", "strand_to"))
  
  # Insert into database
  db_connection$insert(cosmic_data, auto_unbox = TRUE, na = 'null')
}

# Function to import CosmicID CNA mutations
# For now GRCh37 and GRCh38 versions have to imported into separate entries in the same collection
importCosmicCNA <- function(file, gen, ver, db_connection) {
  # Display message
  message("Loading CosmicID Copy Number Alterations for genome ", gen, "...")
  
  # Read data from file
  cosmic_data <- fread(file, header = TRUE, sep = "\t", showProgress = FALSE, select = c(
    "COSMIC_CNV_ID", "GENE_SYMBOL", "MUT_TYPE",
    "CHROMOSOME", "GENOME_START", "GENOME_STOP"
  ))
  
  colnames(cosmic_data) <- c("cosmic_id", "gene", "mut_type", "chr",
                              "start_pos", "stop_pos")
  
  # Remove duplicate entries
  cosmic_data <- unique(cosmic_data)
  
  # Group entries for the same CNA, and keep affected genes as a list
  cosmic_data <- cosmic_data[, .(genes = list(unlist(.SD))), 
                             by = .(cosmic_id, mut_type, chr, start_pos, stop_pos)]

  # Change data types
  cosmic_data[, chr := as.character(chr)]
  cosmic_data[, c("start_pos", "stop_pos") := lapply(.SD, as.integer), .SDcols = c("start_pos", "stop_pos")]
  
  # Add genome and version columns
  cosmic_data[, c("genome", "version") := .(gen, ver)]
  
  # Insert into database
  db_connection$insert(cosmic_data, auto_unbox = TRUE, na = 'null')
}
