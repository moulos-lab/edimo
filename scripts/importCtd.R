importCtd <- function(db_connection, dbver, chem_file_path, chem_gene_file_path, dis_file_path, disease_chem_file_path) {
  
  # Initialize an empty list to store CTD entries
  ctd_entry <- list()
  
  ################# Chemical Vocabulary ##########################
  
  message("Load chemical vocabulary file .........")

  # Determine if the file is gzipped
  is_gzipped <- grepl("\\.gz$", chem_file_path)
  
  # Open file connection
  if (is_gzipped) {
    con <- gzfile(chem_file_path, "r")
  } else {
    con <- file(chem_file_path, "r")
  }
  
  # Initialize an empty list to store chemical vocabulary
  chem_voc <- list()

  # Read chemical vocabulary file
  while (length(chem_line <- readLines(con, n = 1)) > 0) {
    if (substr(chem_line[1], 1, 1) == "#") next
    chem_line <- strsplit(chem_line, "\t")[[1]]
    chem_voc[[chem_line[1]]] <- list(
      chemical_id = chem_line[2],
      casrn = NULL,
      definition = NULL,
      synonyms = strsplit(chem_line[8], "|", fixed = TRUE)[[1]]
      # drugbank_ids = strsplit(chem_line[9], "|", fixed = TRUE)[[1]]
    )
    if (chem_line[3] != "") {
      chem_voc[[chem_line[1]]][["casrn"]] <- chem_line[3]
    }
    if (chem_line[4] != "") {
      chem_voc[[chem_line[1]]][["definition"]] <- chem_line[4]
    }
  }
  close(con)
  
  ################# Chem-gene-interaction Association ##########################
  
  message("Load gene-chemical association file...")
  
  # Determine if the file is gzipped
  is_gzipped <- grepl("\\.gz$", chem_gene_file_path)
  
  # Open file connection
  if (is_gzipped) {
    con <- gzfile(chem_gene_file_path, "r")
  } else {
    con <- file(chem_gene_file_path, "r")
  }

  # Read chemical-gene-interaction association file
  while (length(chem_gene_line <- readLines(con, n = 1)) > 0) {
    if (substr(chem_gene_line[1], 1, 1) == "#") next
    chem_gene_line <- strsplit(chem_gene_line, "\t")[[1]]
    if (chem_gene_line[7] != "Homo sapiens") next
    gene_symbol <- chem_gene_line[4]
    # if (!gene_symbol %in% names(ctd_entry)) {
    if (is.null(ctd_entry[[gene_symbol]])) {
      # Initialize gene entry
      gene_list <- list(
        organism = chem_gene_line[7],
        chemicals = NULL
      )
      ctd_entry[[gene_symbol]] <- gene_list
    }
    chemical_name <- chem_gene_line[1]
    # if (!chemical_name %in% names(ctd_entry[[gene_symbol]][["chemicals"]])) {
    if (is.null(ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]])) {
      chem_list <- list(
        chemical_info = chem_voc[[chemical_name]],
        interactions = NULL
      )
      ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]] <- chem_list          
    }
    interaction <- chem_gene_line[9]
    # if (!interaction %in% names(ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]][["interactions"]])) {
    if (is.null(ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]][["interactions"]][[interaction]])) {
      inter_list <- list(
        gene_forms = strsplit(chem_gene_line[6], "|", fixed = TRUE)[[1]],
        interaction_actions = strsplit(chem_gene_line[10], "|", fixed = TRUE)[[1]],
        pubmed_ids = strsplit(chem_gene_line[11], "|", fixed = TRUE)[[1]]
      )
      ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]][["interactions"]][[interaction]] <- inter_list
    }
  }
  close(con)
  
  ################# Disease Vocabulary ##########################
  
  message("Load disease vocabulary file...")
  
  # Determine if the file is gzipped
  is_gzipped <- grepl("\\.gz$", dis_file_path)
  
  # Open file connection
  if (is_gzipped) {
    con <- gzfile(dis_file_path, "r")
  } else {
    con <- file(dis_file_path, "r")
  }

  # Initialize an empty list to store disease vocabulary
  disease_voc <- list()

  # Read disease vocabulary file
  while (length(disease_line <- readLines(con, n = 1)) > 0) {
    if (substr(disease_line[1], 1, 1) == "#") next
    disease_line <- strsplit(disease_line, "\t")[[1]]
    disease_voc[[disease_line[1]]] <- list(
      definition = disease_line[4],
      alt_disease_ids = strsplit(disease_line[3], "|", fixed = TRUE)[[1]],
      synonyms = strsplit(disease_line[8], "|", fixed = TRUE)[[1]]
    )
  }
  close(con)
  
  ################# Chem-gene-disease Association ##########################
  
  message("Load gene-disease-chemical association file...")
  
  # Determine if the file is gzipped
  is_gzipped <- grepl("\\.gz$", disease_chem_file_path)
  
  # Open file connection
  if (is_gzipped) {
    con <- gzfile(disease_chem_file_path, "r")
  } else {
    con <- file(disease_chem_file_path, "r")
  }  

  # Read chemical-gene-disease association file
  while (length(disease_chem_line <- readLines(con, n = 1)) > 0) {
    if (substr(disease_chem_line[1], 1, 1) == "#") next
    disease_chem_line <- strsplit(disease_chem_line, "\t")[[1]]
    gene_symbol <- disease_chem_line[7]
    chemical_name <- disease_chem_line[1]
    disease_name <- disease_chem_line[4]
    if (gene_symbol == "") next
    if (!is.null(ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]])) {
      ctd_entry[[gene_symbol]][["chemicals"]][[chemical_name]][["diseases"]][[disease_name]] <- list(
        disease_id = disease_chem_line[5],
        definition = disease_voc[[disease_name]][["definition"]],
        alt_disease_ids = disease_voc[[disease_name]][["alt_disease_ids"]],
        synonyms = disease_voc[[disease_name]][["synonyms"]],
        direct_evidence = strsplit(disease_chem_line[6], "|", fixed = TRUE)[[1]],
        inference_score = as.double(disease_chem_line[8]),
        omim_ids = strsplit(disease_chem_line[9], "|", fixed = TRUE)[[1]],
        pubmed_ids = strsplit(disease_chem_line[10], "|", fixed = TRUE)[[1]]
      )
    }
  }
  close(con)
  
  # Set to MongoDB
  setToMongo(db_connection, ctd_entry, dbver)
}

################################################################################

setToMongo <- function(db_connection, ctd_entry, dbver) {
  
  # Display message
  message("Store CTD gene collection to Mongo...")
  
  # Iterate through each gene in ctd_entry
  for (key_gene in names(ctd_entry)) {
    
    # Initialize temporary list to store chemical entries
    temp_chem <- list()
    
    # Iterate through each chemical for the current gene
    for (key_chem in names(ctd_entry[[key_gene]][["chemicals"]])) {
      
      # Initialize gene entry
      gene_entry <- list()
      
      # Initialize temporary lists for interactions and diseases
      temp_inter <- list()
      temp_dis <- list()
      
      # Iterate through each disease associated with the chemical
      for (key_dis in names(ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]])) {
        
        # Create disease hash
        disease_hash <- list(
          disease_name = key_dis,
          disease_id = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["disease_id"]],
          definition = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["definition"]],
          alt_disease_ids = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["alt_disease_ids"]],
          synonyms = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["synonyms"]],
          direct_evidence = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["direct_evidence"]],
          inference_score = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["inference_score"]],
          omim_ids = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["omim_ids"]],
          pubmed_ids = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["diseases"]][[key_dis]][["pubmed_ids"]]
        )
        
        # Append disease hash to temporary disease list
        temp_dis <- c(temp_dis, list(disease_hash))
      }
      
      # Initialize inner interactions counter
      inner_inter <- 0L
      
      # Iterate through each interaction associated with the chemical
      for (key_inter in names(ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["interactions"]])) {
        
        # Create interaction hash
        inter_hash <- list(
          interaction = key_inter,
          interaction_actions = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["interactions"]][[key_inter]][["interaction_actions"]],
          gene_forms = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["interactions"]][[key_inter]][["gene_forms"]],
          pubmed_ids = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["interactions"]][[key_inter]][["pubmed_ids"]]
        )
        
        # Append interaction hash to temporary interaction list
        temp_inter <- c(temp_inter, list(inter_hash))
        
        # Increment inner interactions counter
        inner_inter <- inner_inter + length(inter_hash[["interaction_actions"]])
      }
      
      # Create gene entry
      gene_entry <- list(
        gene_symbol = key_gene,
        organism = ctd_entry[[key_gene]][["organism"]],
        chemical_name = key_chem,
        outer_interactions = length(temp_inter),
        inner_interactions = inner_inter,
        chemical_info = ctd_entry[[key_gene]][["chemicals"]][[key_chem]][["chemical_info"]],
        interactions = temp_inter,
        diseases = temp_dis,
        version = dbver
      )
      
      # Insert gene entry into MongoDB collection
      db_connection$insert(gene_entry, auto_unbox = TRUE, null = 'null')
    }
  }
}
