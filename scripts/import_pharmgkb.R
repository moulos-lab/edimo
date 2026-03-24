# Helper functions

# Function to load a string value
loadString <- function(value) {
  value <- gsub('^"|"$', '', value)
  value <- gsub('^\\s+|\\s+$', '', value)
  if (value %in% c(".", ".\r", "")) {
    return(NULL)
  } else {
    return(value)
  }
}

# Function to load an array of strings
loadStringArray <- function(value) {
  string_array <- strsplit(value, ",")[[1]]
  string_array <- lapply(string_array, loadString)
  # string_array <- unique(sort(Filter(Negate(is.null), string_array)))
  return(string_array)
}

# Function to load an integer value
loadInt <- function(var_int) {
  var_int <- loadString(var_int)
  if (!is.null(var_int)) {
    var_int <- as.integer(var_int)
  }
  return(var_int)
}

# Function to load an array of integers
loadIntArray <- function(value) {
  int_array <- strsplit(value, ",")[[1]]
  int_array <- lapply(int_array, loadInt)
  # int_array <- unique(sort(Filter(Negate(is.null), int_array)))
  return(int_array)
}

################################################################################
# Define a function to get PharmGKB genotype annotation entries
getPharmgkbClinAnnAllelesEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load clinical alleles annotation file .........")
  
  # Determine file path
  clin_ann_alleles_file <- file
  
  # Initialize list
  clin_ann_alleles_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", clin_ann_alleles_file)) {
    con <- gzfile(clin_ann_alleles_file, "r")
  } else {
    con <- file(clin_ann_alleles_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]
    pharmgkb_id <- parts[[1]]
    gen_all <- parts[[2]]
    annot <- parts[[3]]
    clin_ann_alleles_list[[pharmgkb_id]] <- append(
      clin_ann_alleles_list[[pharmgkb_id]],
      list(c("genotype" = loadString(gen_all), "annotation" = loadString(annot)))
    )
  }
  close(con)
  
  return(clin_ann_alleles_list)
}

# Define a function to get PharmGKB variant annotations entries
getPharmgkbVarannEntries <- function(pheno_file, drug_file, fun_file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load variant annotation file .........")
  
  # Determine file paths
  varann_files_list <- list(
    varann_pheno_file = pheno_file,
    varann_drug_file = drug_file,
    varann_fun_file = fun_file
  )
  
  # Initialize list
  varann_list <- list()
  
  for (varann_file in varann_files_list) {
    # Open file connection
    if (grepl("\\.gz$", varann_file)) {
      con <- gzfile(varann_file, "r")
    } else {
      con <- file(varann_file, "r")
    }
    
    # Read header indices
    header <- strsplit(readLines(con, n = 1), "\t")[[1]]
    header_indices <- setNames(seq_along(header), header)
    
    # Read data lines
    while (length(line <- readLines(con, n = 1)) > 0) {
      parts <- strsplit(line, "\t")[[1]]
      id <- parts[[1]]
      allele <- parts[[10]]
      sentence <- parts[[9]]
      notes <- parts[[8]]
      signif <- parts[[7]]
      pmid <- loadInt(parts[[5]])
      
      varann_list[[id]] <- list("allele" = loadString(allele),
                                "sentence" = loadString(sentence),
                                "notes" = loadString(notes),
                                "significance" = loadString(signif),
                                "pmid" = pmid)
    }
    close(con)
    
  }
  return(varann_list)
}

# Define a function to get PharmGKB evidence entries
getPharmgkbEvidenceEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load clinical annotation evidence file .........")
  
  # Determine file path
  evidence_file <- file
  
  # Initialize list
  evidence_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", evidence_file)) {
    con <- gzfile(evidence_file, "r")
  } else {
    con <- file(evidence_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]
    
    # Extract PharmGKB Accession Id
    id <- parts[[1]]
    
    # Store evidence data in list
    evidence_list[[id]][["ev_ids"]] <- 
      append(evidence_list[[id]][["ev_ids"]], loadString(parts[[2]]))
    evidence_list[[id]][["pmids"]] <- 
      append(evidence_list[[id]][["pmids"]], loadInt(parts[[5]]))
  }
  close(con)
  
  return(evidence_list)
}

# Define a function to get PharmGKB gene entries
getPharmgkbGeneEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load gene vocabulary file .........")
  
  # Determine file path
  gene_file <- file
  
  # Initialize list
  gene_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", gene_file)) {
    con <- gzfile(gene_file, "r")
  } else {
    con <- file(gene_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]

    # Extract gene symbol
    gene_symbol <- parts[[6]]
    
    # Extract list according to delimitation style
    if (grepl("\"", parts[[7]])) {
      alt_names <- strsplit(parts[[7]], "\"\", \"\"")[[1]]
      alt_names <- gsub("\"", "", alt_names)
    } else {
      if (!parts[[7]] == "") {
        alt_names <- strsplit(parts[[7]], ", ")[[1]]
      }
      else {
        alt_names <- NULL
      }
    }
    
    # Load gene data into a list
    gene_data <- list(
      "hgnc_symbol" = loadString(parts[[6]]),
      "hgnc_name" = loadString(parts[[5]]),
      "hgnc_id" = strsplit(parts[[3]], ":")[[1]][2],
      "pharmgkb_id" = loadString(parts[[1]]),
      "ncbi_id" = loadString(parts[[2]]),
      "ensembl_id" = loadStringArray(parts[[4]]),
      "alternative_names" = alt_names,
      "alternative_symbols" = loadStringArray(parts[[8]]),
      "is_vip" = loadString(parts[[9]]),
      "has_variant_annotation" = loadString(parts[[10]]),
      "has_cpic" = loadString(parts[[12]]),
      "chr" = loadString(parts[[13]]),
      "chr_start_hg19" = loadInt(parts[[14]]),
      "chr_stop_hg19" = loadInt(parts[[15]]),
      "chr_start_hg38" = loadInt(parts[[16]]),
      # There is one mistake where an entry w/o coordinates has length 16, not 17
      "chr_stop_hg38" = if (length(parts) == 17) loadInt(parts[[17]]) else NULL
    )
    
    # Store gene data in list
    gene_list[[gene_symbol]] <- gene_data
  }
  close(con)

  return(gene_list)
}

# Define a function to get PharmGKB variant entries
getPharmgkbVariantEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load variant vocabulary file .........")
  
  # Determine file path
  variant_file <- file
  
  # Initialize list
  variant_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", variant_file)) {
    con <- gzfile(variant_file, "r")
  } else {
    con <- file(variant_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]
    variant_id <- parts[[2]]
    
    # Load variant data into a list
    variant_data <- list(
      "var_id" = loadString(parts[[2]]),
      "pharmgkb_id" = loadString(parts[[1]]),
      "gene_ids" = loadStringArray(parts[[3]]),
      "gene_symbols" = loadStringArray(parts[[4]]),
      "va_count" = loadInt(parts[[6]]),
      "ca_count" = loadInt(parts[[7]]),
      "level_12" = loadInt(parts[[8]]),
      "dg_count" = loadInt(parts[[9]]),
      # "haplotype" = loadString(parts[[11]]),
      "synonyms" = loadStringArray(parts[[11]])
    )
    
    # Store variant data in list
    variant_list[[variant_id]] <- variant_data
  }
  close(con)
  
  return(variant_list)
}

# Define a function to get PharmGKB chemical entries
getPharmgkbChemicalEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load chemical vocabulary file .........")
  
  # Determine file path
  chemical_file <- file
  
  # Initialize list
  chemical_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", chemical_file)) {
    con <- gzfile(chemical_file, "r")
  } else {
    con <- file(chemical_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]

    # Extract chemical name
    name <- parts[[2]]

    # Extract lists according to delimitation style
    if (grepl("\"", parts[[3]])) {
      generic_names <- strsplit(parts[[3]], "\"\", \"\"")[[1]]
      generic_names <- gsub("\"", "", generic_names)
    } else {
      if (!parts[[3]] == "") {
        generic_names <- strsplit(parts[[3]], ", ")[[1]]
      }
      else {
        generic_names <- NULL
      }
    }
    
    if (grepl("\"", parts[[4]])) {
      trade_names <- strsplit(parts[[4]], "\"\", \"\"")[[1]]
      trade_names <- gsub("\"", "", trade_names)
    } else {
      if (!parts[[4]] == "") {
        trade_names <- strsplit(parts[[4]], ", ")[[1]]
      }
      else {
        trade_names <- NULL
      }
    }
    
    # Load chemical data into a list
    chemical_data <- list(
      "chemical_name" = loadString(parts[[2]]),
      "pharmgkb_id" = loadString(parts[[1]]),
      "generic_names" = as.list(generic_names),
      "trade_names" = as.list(trade_names),
      "brand_mixtures" = loadStringArray(parts[[5]]),
      "type" = loadStringArray(parts[[6]]),
      "cross_references" = loadStringArray(parts[[7]]),
      "smiles" = loadString(parts[[8]]),
      "inchi" = loadString(parts[[9]]),
      "dosing_guideline" = loadString(parts[[10]]),
      "external_vocabulary" = loadStringArray(parts[[11]]),
      "ca_count" = loadInt(parts[[12]]),
      "va_count" = loadInt(parts[[13]]),
      "pa_count" = loadInt(parts[[14]]),
      "vip_count" = loadInt(parts[[15]]),
      "dosing_guideline_sources" = loadStringArray(parts[[16]]),
      "tca_level" = loadString(parts[[17]]),
      "tfda_level" = loadString(parts[[18]]),
      "tad_level" = loadString(parts[[19]])
    )
    
    # Store chemical data in list
    chemical_list[[name]] <- chemical_data
  }
  close(con)
  
  return(chemical_list)
}

# Define a function to get PharmGKB phenotype entries
getPharmgkbPhenotypeEntries <- function(file, gen, dbver) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load phenotype vocabulary file .........")
  
  # Determine file path
  phenotype_file <- file
  
  # Initialize list
  phenotype_list <- list()
  
  # Open file connection
  if (grepl("\\.gz$", phenotype_file)) {
    con <- gzfile(phenotype_file, "r")
  } else {
    con <- file(phenotype_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]

    # Extract phenotype name
    name <- parts[[2]]
    id <- parts[[1]]
    
    # Extract list according to delimitation style
    if (grepl("\"", parts[[3]])) {
      alt_names <- strsplit(parts[[3]], "\"\", \"\"")[[1]]
      alt_names <- gsub("\"", "", alt_names)
    } else {
      if (!parts[[3]] == "") {
        alt_names <- strsplit(parts[[3]], ", ")[[1]]
      }
      else {
        alt_names <- NULL
      }
    }

    # Load phenotype data into a list
    phenotype_data <- list(
      "name" = loadString(parts[[2]]),
      "id" = loadString(id),
      "alt_names" = as.list(alt_names)
    )
    
    # Store phenotype data in list
    phenotype_list[[name]] <- phenotype_data
  }
  close(con)
  
  return(phenotype_list)
}

# Define a function to get PharmGKB clinical entries
getPharmgkbClinicalEntries <- function(db_connection, gen, dbver, 
                                          clinical_file, 
                                          varann_list, 
                                          clin_ann_alleles_list, 
                                          evidence_list,
                                          gene_list, 
                                          variant_list, 
                                          chemical_list, 
                                          phenotype_list) {
  # Extract configuration parameters
  gen <- gen
  dbver <- dbver
  
  # Display progress
  message("Load clinicals file .........")
  
  # Open file connection
  if (grepl("\\.gz$", clinical_file)) {
    con <- gzfile(clinical_file, "r")
  } else {
    con <- file(clinical_file, "r")
  }

  # Read header indices
  header <- strsplit(readLines(con, n = 1), "\t")[[1]]
  header_indices <- setNames(seq_along(header), header)
  
  # Read data lines
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, "\t")[[1]]

    ann_id <- loadInt(parts[[1]])

    location <- parts[[2]]
    # Replace specific patterns
    location <- gsub("G6PD Mediterranean, Dallas, Panama' Sassari, Cagliari, Birmingham", 
                     "G6PD Mediterranean,Dallas,Panama' Sassari,Cagliari,Birmingham", location)
    location <- gsub("G6PD Canton, Taiwan-Hakka, Gifu-like, Agrigento-like", 
                     "G6PD Canton,Taiwan-Hakka,Gifu-like,Agrigento-like", location)
    var_id <- as.list(strsplit(location, ",\\s")[[1]])

    gene_symbols <- as.list(strsplit(parts[[3]], ";")[[1]])
    # Convert gene symbols to gene information
    genes <- lapply(gene_symbols, function(symbol) gene_list[[symbol]])

    loe <- loadString(parts[[4]])

    cat <- loadStringArray(parts[[8]])

    # ann_id should probably be a string here, as this is how it is loaded in clin_ann_alleles_list
    clin_ann_alleles <- if (!is.null(clin_ann_alleles_list[[as.character(ann_id)]])) {
      clin_ann_alleles_list[[as.character(ann_id)]]
    # clin_ann_alleles <- if (!is.null(clin_ann_alleles_list[[ann_id]])) {
      # clin_ann_alleles_list[[ann_id]]
    } else NULL

    evs <- evidence_list[[as.character(ann_id)]]
    # Convert EvidenceIDs to variant annotations 
    varann <- lapply(evs[[1]], 
      function(id) if (!is.null(varann_list[[id]])) varann_list[[id]] else NULL)
    varann <- varann[!vapply(varann, is.null, logical(1))]
    # Retrieve PMIDs
    pmids <- evs[[2]]

    evidence_count <- loadInt(parts[[10]])

    chem_names <- as.list(strsplit(parts[[11]], ";")[[1]])
    # Convert chemical names to chemical information 
    chems <- lapply(chem_names, function(name) chemical_list[[name]])

    phen_names <- as.list(strsplit(parts[[12]], ";")[[1]])
    # Convert phenotype names to phenotype information 
    phens <- lapply(phen_names, function(name) phenotype_list[[name]])

    # race <- loadStringArray(parts[["Race"]])
    
    # Prepare data for insertion
    for (var in var_id) {
      variant <- list(
        "synonyms" = list(),
        "ca_count" = NULL,
        "gene_ids" = list(),
        "gene_symbols" = gene_symbols,
        "level_12" = NULL,
        "dg_count" = NULL,
        "haplotype" = NULL,
        "va_count" = NULL,
        "pharmgkb_id" = NULL,
        "var_id" = var
      )
      if (var %in% names(variant_list)) {
        variant <- variant_list[[var]]
      }
      
      input_list <- list(
        "genome_version" = gen,
        "version" = dbver,
        "type" = cat,
        "level_of_evidence" = loe,
        "variant" = variant,
        "phenotypes" = phens,
        "chemicals" = chems,
        "genes" = genes,
        "ann_id" = ann_id,
        "genotypes" = clin_ann_alleles,
        "annotations" = varann,
        "pmids" = pmids,
        "evidence_count" = evidence_count
        # "race" = race
      )
      
      # Insert data into collection_pharmgkb
      db_connection$insert(input_list, auto_unbox = TRUE, null = 'null')
    }
  }
  close(con)
}


# # Get current date and time
# date_time <- Sys.time()

# # Display start message
# message(paste0(date_time, "- Started..."))

# # Load clinical allele, variant annotations, evidence
# # and gene, variant, drugs, chemicals, phenotypes vocabularies
# clin_ann_alleles_list <- getPharmgkbClinAnnAllelesEntries()
# varann_list <- getPharmgkbVarannEntries()
# evidence_list <- getPharmgkbEvidenceEntries()
# gene_list <- getPharmgkbGeneEntries()
# variant_list <- getPharmgkbVariantEntries()
# chemical_list <- getPharmgkbChemicalEntries()
# phenotype_list <- getPharmgkbPhenotypeEntries()


# # Display progress message
# message("Communicating with MongoDB...")

# # Store pharmgkb data to Clingon MongoDB
# message("Storing PharmKGB data to MongoDB.....")
# getPharmgkbClinicalEntries(db_connection, gen, dbver, 
#                             clinical_file, 
#                             varann_list, 
#                             clin_ann_alleles_list, 
#                             evidence_list, 
#                             gene_list, 
#                             variant_list, 
#                             chemical_list, 
#                             phenotype_list)

# # Display finish message
# message(paste0(date_time, "- Finished..."))
