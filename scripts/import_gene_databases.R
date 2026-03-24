# Is DBI required?

library(RSQLite)
library(DBI)
library(mongolite)

importDisgenetGene <- function(file_path, version, organism, db_connection) {  
  # Open a connection to the SQLite DisGeNET database
  con <- dbConnect(RSQLite::SQLite(), file_path)
  
  # Display progress message
  message("Parsing...")

  # Prepare and execute the SQL query to retrieve gene attributes
  gene_query <- "SELECT geneNID, geneName FROM geneAttributes"
  gene_results <- dbGetQuery(con, gene_query)

  # Initialize disgenet as an empty list
  disgenet <- list()

  # Iterate over each row of gene results
  for (i in seq_len(nrow(gene_results))) {
    gene_nid <- gene_results[i, "geneNID"]
    gene_name <- gene_results[i, "geneName"]
    
    # Initialize a list for current gene record
    record <- list(
      "version" = version,
      "organism" = organism,
      "gene_name" = gene_name,
      "network" = list())

    disgenet_instance <- list()
    
    # Fetch disease NIDs for the current gene
    disease_query <- paste0("SELECT DISTINCT diseaseNID ",
                            "FROM geneDiseaseNetwork ",
                            "WHERE geneNID = ", gene_nid)
    disease_results <- dbGetQuery(con, disease_query)

    # Iterate over each disease NID
    for (j in seq_len(nrow(disease_results))) {
      disease_nid <- disease_results[j, "diseaseNID"]
      
      # Fetch associations for the current gene-disease pair
      association_query <- paste0("SELECT * ",
                                  "FROM geneDiseaseNetwork ",
                                  "WHERE geneNID = ", gene_nid,
                                  " AND diseaseNID = ", disease_nid)
      association_results <- dbGetQuery(con, association_query)
      
      # Process association results
      for (k in seq_len(nrow(association_results))) {
        # Extract association data from the current row of association results
        
        # Fill main data
        if (!disease_nid %in% names(disgenet_instance)) {
          disgenet_instance[[as.character(disease_nid)]] <- list()
          # Construct the SQL query to retrieve disease attributes
          disease_query <- paste0(
            "SELECT diseaseId, diseaseName, type, ",
            "diseaseClass, diseaseClassName ",
            "FROM diseaseAttributes INNER JOIN disease2class ",
            "ON diseaseAttributes.diseaseNID = disease2class.diseaseNID ",
            "INNER JOIN diseaseClass ",
            "ON disease2class.diseaseClassNID = diseaseClass.diseaseClassNID ",
            "WHERE diseaseAttributes.diseaseNID = ", disease_nid)
          
          # Prepare and execute the SQL query
          disease_data <- dbGetQuery(con, disease_query)

          # Checks if results with class are returned
          has_disease_class_flag <- FALSE
          for (l in seq_len(nrow(disease_data))) {
            has_disease_class_flag <- TRUE
            disgenet_instance[[as.character(disease_nid)]]$id <- 
              disease_data$diseaseId[l]
            disgenet_instance[[as.character(disease_nid)]]$name <- 
              disease_data$diseaseName[l]
            disgenet_instance[[as.character(disease_nid)]]$type <- 
              disease_data$type[l]
            disgenet_instance[[as.character(disease_nid)]]$disease <- 
              append(disgenet_instance[[as.character(disease_nid)]]$disease,
                     list(list(class = disease_data$diseaseClass[l], 
                               name = trimws(disease_data$diseaseClassName[l],
                                             which = "left"))))
          }
          
          # Runs a modified query for phenotypes and diseases without class
          if (!has_disease_class_flag) {
            # Construct the SQL query to retrieve disease/phenotype attributes
            disease_wo_class_query <- paste0(
              "SELECT diseaseId, diseaseName, type ",
              "FROM diseaseAttributes ",
              "WHERE diseaseAttributes.diseaseNID = ", disease_nid)
            
            # Prepare and execute the SQL query
            disease_wo_class_data <- dbGetQuery(con, disease_wo_class_query)

            for (l in seq_len(nrow(disease_wo_class_data))) {
              disgenet_instance[[as.character(disease_nid)]]$id <- 
                disease_wo_class_data$diseaseId[l]
              disgenet_instance[[as.character(disease_nid)]]$name <- 
                disease_wo_class_data$diseaseName[l]
              disgenet_instance[[as.character(disease_nid)]]$type <- 
                disease_wo_class_data$type[l]
              disgenet_instance[[as.character(disease_nid)]]$disease <- 
                append(disgenet_instance[[as.character(disease_nid)]]$disease,
                       list(list(class = NULL, name = NULL)))
            }
          }
        }

        # Initialize with null so missing values can be caught
        association_instance <- list(
          pmid = NULL,
          type = NULL,
          source = NULL,
          score = NULL,
          sentence = NULL,
          year = NULL
        )

        # Extract association attributes
        pmid <- association_results[k, "pmid"]
        type <- association_results[k, "associationType"]
        source <- association_results[k, "source"]
        score <- association_results[k, "score"]
        sentence <- association_results[k, "sentence"]
        year <- association_results[k, "year"]

        # Excessive because:
        # For 'year' missing values are 'NA'
        # For the rest they are NULL
        if (!is.na(pmid)) {
          association_instance[["pmid"]] <-as.integer(pmid)
        }
        if (!is.na(type)) {
          association_instance[["type"]] <- as.character(type)
        }
        if (!is.na(source)) {
          association_instance[["source"]] <- as.character(source)
        }
        if (!is.na(score)) {
          association_instance[["score"]] <- as.double(score)
        }
        if (!is.na(sentence)) {
          association_instance[["sentence"]] <- as.character(sentence)
        }
        if (!is.na(year) && !year==0) {
          association_instance[["year"]] <- year
        }

        # Add association to the list for the current gene-disease pair
        disgenet_instance[[as.character(disease_nid)]]$association <- 
          append(disgenet_instance[[as.character(disease_nid)]]$association,
                 list(association_instance))
      }
      
      # Add associations to the list for the current gene
      record[["network"]] <- append(record[["network"]],
        list(disgenet_instance[[as.character(disease_nid)]]))
    }

    # Add gene and associations to the disgenet data
    disgenet <- rbind(disgenet, record)
  }
  
  # Close the database connection
  dbDisconnect(con)
  
  # Display progress message
  message("Importing data...")
  
  # Converts disgenet to work well with mongolite
  disgenet <- as.data.frame(disgenet)
  row.names(disgenet) <- NULL
  
  # Insert disgenet data into database
  db_connection$insert(disgenet, auto_unbox = TRUE, null = 'null')
}

importDisgenetVariant <- function(file_path, version, organism, db_connection) {
  # Open a connection to the SQLite DisGeNET database
  con <- dbConnect(RSQLite::SQLite(), file_path)
  
  # Display progress message
  message("Parsing...")

  # Prepare and execute the SQL query to retrieve variant attributes
  variant_query <- "SELECT variantNID, variantId FROM variantAttributes"
  variant_results <- dbGetQuery(con, variant_query)

  # Initialize disgenet as an empty list
  disgenet <- list()

  # Iterate over each row of variant results
  for (i in seq_len(nrow(variant_results))) {
    variant_nid <- variant_results[i, "variantNID"]
    variant_id <- variant_results[i, "variantId"]
    
    # Initialize a list for current variant record
    record <- list(
      "version" = version,
      "organism" = organism,
      "variant_id" = variant_id,
      "network" = list())

    disgenet_instance <- list()
    
    # Fetch disease NIDs for the current variant
    disease_query <- paste0("SELECT DISTINCT diseaseNID ",
                            "FROM variantDiseaseNetwork ",
                            "WHERE variantNID = ", variant_nid)
    disease_results <- dbGetQuery(con, disease_query)

    # Iterate over each disease NID
    for (j in seq_len(nrow(disease_results))) {
      disease_nid <- disease_results[j, "diseaseNID"]
      
      # Fetch associations for the current variant-disease pair
      association_query <- paste0("SELECT * ",
                                  "FROM variantDiseaseNetwork ",
                                  "WHERE variantNID = ", variant_nid,
                                  " AND diseaseNID = ", disease_nid)
      association_results <- dbGetQuery(con, association_query)
      
      # Process association results
      for (k in seq_len(nrow(association_results))) {
        # Extract association data from the current row of association results
        
        # Fill main data
        if (!disease_nid %in% names(disgenet_instance)) {
          disgenet_instance[[as.character(disease_nid)]] <- list()
          # Construct the SQL query to retrieve disease attributes
          disease_query <- paste0(
            "SELECT diseaseId, diseaseName, type, ",
            "diseaseClass, diseaseClassName ",
            "FROM diseaseAttributes INNER JOIN disease2class ",
            "ON diseaseAttributes.diseaseNID = disease2class.diseaseNID ",
            "INNER JOIN diseaseClass ",
            "ON disease2class.diseaseClassNID = diseaseClass.diseaseClassNID ",
            "WHERE diseaseAttributes.diseaseNID = ", disease_nid)
          
          # Prepare and execute the SQL query
          disease_data <- dbGetQuery(con, disease_query)

          # Checks if results with class are returned
          has_disease_class_flag <- FALSE
          for (l in seq_len(nrow(disease_data))) {
            has_disease_class_flag <- TRUE
            disgenet_instance[[as.character(disease_nid)]]$id <- 
              disease_data$diseaseId[l]
            disgenet_instance[[as.character(disease_nid)]]$name <- 
              disease_data$diseaseName[l]
            disgenet_instance[[as.character(disease_nid)]]$type <- 
              disease_data$type[l]
            disgenet_instance[[as.character(disease_nid)]]$disease <- 
              append(disgenet_instance[[as.character(disease_nid)]]$disease,
                     list(list(class = disease_data$diseaseClass[l], 
                               name = trimws(disease_data$diseaseClassName[l],
                                             which = "left"))))
          }
          
          # Runs a modified query for phenotypes and diseases without class
          if (!has_disease_class_flag) {
            # Construct the SQL query to retrieve disease/phenotype attributes
            disease_wo_class_query <- paste0(
              "SELECT diseaseId, diseaseName, type ",
              "FROM diseaseAttributes ",
              "WHERE diseaseAttributes.diseaseNID = ", disease_nid)
            
            # Prepare and execute the SQL query
            disease_wo_class_data <- dbGetQuery(con, disease_wo_class_query)

            for (l in seq_len(nrow(disease_wo_class_data))) {
              disgenet_instance[[as.character(disease_nid)]]$id <- 
                disease_wo_class_data$diseaseId[l]
              disgenet_instance[[as.character(disease_nid)]]$name <- 
                disease_wo_class_data$diseaseName[l]
              disgenet_instance[[as.character(disease_nid)]]$type <- 
                disease_wo_class_data$type[l]
              disgenet_instance[[as.character(disease_nid)]]$disease <- 
                append(disgenet_instance[[as.character(disease_nid)]]$disease,
                       list(list(class = NULL, name = NULL)))
            }
          }
        }

        # Initialize with null so missing values can be caught
        association_instance <- list(
          pmid = NULL,
          type = NULL,
          source = NULL,
          score = NULL,
          sentence = NULL,
          year = NULL
        )

        # Extract association attributes
        pmid <- association_results[k, "pmid"]
        type <- association_results[k, "associationType"]
        source <- association_results[k, "source"]
        score <- association_results[k, "score"]
        sentence <- association_results[k, "sentence"]
        year <- association_results[k, "year"]

        # Excessive because:
        # For 'year' missing values are 'NA'
        # For the rest they are NULL
        if (!is.na(pmid)) {
          association_instance[["pmid"]] <-as.integer(pmid)
        }
        if (!is.na(type)) {
          association_instance[["type"]] <- as.character(type)
        }
        if (!is.na(source)) {
          association_instance[["source"]] <- as.character(source)
        }
        if (!is.na(score)) {
          association_instance[["score"]] <- as.double(score)
        }
        if (!is.na(sentence)) {
          association_instance[["sentence"]] <- as.character(sentence)
        }
        if (!is.na(year) && !year==0) {
          association_instance[["year"]] <- year
        }

        # Add association to the list for the current variant-disease pair
        disgenet_instance[[as.character(disease_nid)]]$association <- 
          append(disgenet_instance[[as.character(disease_nid)]]$association,
                 list(association_instance))
      }
      
      # Add associations to the list for the current variant
      record[["network"]] <- append(
        record[["network"]],
        list(disgenet_instance[[as.character(disease_nid)]]))
    }

    # Add variant and associations to the disgenet data
    disgenet <- rbind(disgenet, record)
  }
  
  # Close the database connection
  dbDisconnect(con)
  
  # Display progress message
  message("Importing data...")
  
  # Converts disgenet to work well with mongolite
  disgenet <- as.data.frame(disgenet)
  row.names(disgenet) <- NULL
  
  # Insert disgenet data into database
  db_connection$insert(disgenet, auto_unbox = TRUE, null = 'null')
}

# Function to import CGD data into a MongoDB database
importCgd <- function(file_path, version, organism, db_connection) {
  # Open file connection
  if (grepl("\\.gz$", file_path)) {
    con <- gzfile(file_path, "r")
  } else {
    con <- file(file_path, "r")
  }
  
  # Skip header
  header <- readLines(con, n = 1)
  
  # Read data
  data <- readLines(con)
  
  # Close file connection
  close(con)

  # Display progress message
  message("Parsing...")

  # Define a function to process each line
  process_line <- function(line, version, organism) {
    fields <- strsplit(line, "\t")[[1]]
    
    # Process age group information
    # Because multiple can exist, keeps the original and also creates counters
    age_group <- list(
      definition = fields[6],
      pediatric = as.integer(grepl("Pediatric", fields[6], ignore.case = TRUE)),
      adult = as.integer(grepl("Adult", fields[6], ignore.case = TRUE)),
      non_specific = as.integer(fields[6] == "N/A")
    )

    # Process manifestation categories
    manifestation <- strsplit(fields[8], ";")
    
    # Process intervention categories
    intervention <- strsplit(fields[9], ";")

    # Process references data to separate correctly
    references <- lapply(
      lapply(
        strsplit(trimws(fields[12]), ";  | ; |; |;| ")[[1]],  # Order is important
        function(ref) gsub(",", "", ref)  # Trim ","
      ),
      as.integer
    )

    # Initialize record as NULL to retain missing values
    record <- list(
      version = NULL,
      organism = NULL,
      gene = NULL,
      condition = NULL,
      inheritance = NULL,
      age_group = NULL,
      allelic_conditions = NULL,
      manifestation_categories = NULL,
      intervention_categories = NULL,
      comments = NULL,
      intervention_rationale = NULL,
      references = NULL
    )

    # Populate record
    record[["version"]] <- version
    record[["organism"]] <- organism
    record[["gene"]] <- fields[1]
    record[["condition"]] <- strsplit(fields[4], "; ")[[1]]
    record[["inheritance"]] <- trimws(fields[5])
    record[["age_group"]] <- age_group
    if (fields[7] != "") {
      record[["allelic_conditions"]] <- unlist(strsplit(fields[7], ";"))
    }
    record[["manifestation_categories"]] <- manifestation
    record[["intervention_categories"]] <- intervention
    record[["comments"]] <- if(fields[10] != "") fields[10]
    record[["intervention_rationale"]] <- if(fields[11] != "") fields[11]
    record[["references"]] <- references
    
    return(record)
  }

  # Process data
  records <- lapply(data, process_line, version = version, organism = organism)
  
  # Convert records to a data frame
  records <- as.data.frame(do.call(rbind, records))

  # Print status
  message("Importing...")
  
  # Insert data into database
  db_connection$insert(records, auto_unbox = TRUE, null = 'null')
}

# This version only keeps HPO terms that have genes associated with them,
# and does not include synonyms at all. Frequency is reported as is.
importHpo <- function(version,
                      phenotype_to_genes,
                      annotations,
                      db_connection) {
  
  # Display progress message
  message("Parsing...")

  # Initialize HPO list
  hpo_list <- list()
  
  # HPO file processing
  hpo_file <- phenotype_to_genes

  # Open connection to HPO file
  message("Load HPO file .........")
  con <- file(hpo_file, "r")

  # Skip comments and header
  while (TRUE) {
    hpo_line <- readLines(con, n = 1)
    if (length(hpo_line) == 0) break  # End of file
    if (!grepl("^#", hpo_line)) break  # Not a comment
  }

  # Process each line in the HPO file
  while (length(hpo_line <- readLines(con, n = 1)) > 0) {
    hpo_array <- strsplit(hpo_line, "\t")[[1]]

    hpo_id <- hpo_array[1]

    # if (!hpo_id %in% names(hpo_list)) {
    if (is.null(hpo_list[[hpo_id]])) {
      # Initialize phenotype entry
      phe_list <- list(
        hpo_id = hpo_id,
        hpo_name = hpo_array[2],
        version = version,
        gene_relation = NULL,
        phenotype_annotation = NULL
      )
      hpo_list[[hpo_id]] <- phe_list
    }

    hpo_list[[hpo_id]][["gene_relation"]][[hpo_array[3]]][["gene_id"]] <- hpo_array[3]
    hpo_list[[hpo_id]][["gene_relation"]][[hpo_array[3]]][["gene_symbol"]] <- hpo_array[4]
    
    hpo_list[[hpo_id]][["gene_relation"]][[hpo_array[3]]][["diseases"]] <- 
      append(hpo_list[[hpo_id]][["gene_relation"]][[hpo_array[3]]][["diseases"]],
             list(hpo_array[5]))
  }
  close(con)

  for (hpo_id in names(hpo_list)) {
    # Remove names of all elements in gene_relation under hpo_list[hpo_id]
    # so it works correctly as an array in MongoDB
    names(hpo_list[[hpo_id]][["gene_relation"]]) <- NULL
  }

  # Phenotype annotations file processing
  phe_file <- annotations

  # Open connection to phenotype annotations file
  message("Load Phenotype file .........")
  con <- file(phe_file, "r")

  # Skip comments and header
  while (TRUE) {
    phe_line <- readLines(con, n = 1)
    if (length(phe_line) == 0) break  # End of file
    if (!grepl("^#", phe_line)) break  # Not a comment
  }

  # Process each line in the phenotype annotations file
  while (length(phe_line <- readLines(con, n = 1)) > 0) {    
    phe_array <- strsplit(phe_line, "\t")[[1]]
    
    hpo_id <- phe_array[4]
    # Skips entries that have no gene relation in HPO
    # if (!hpo_id %in% names(hpo_list)) next
    if (is.null(hpo_list[[hpo_id]])) next
    
    # Initialize annotation instance
    annot_list <- list(
      database_id = NULL,
      disease_name = NULL,
      database_reference = NULL,
      evidence_code = NULL,
      onset_modifier = NULL,
      frequency = NULL,
      aspect = NULL
    )

    # Assign variable names
    database_id <- phe_array[1]
    disease_name <- phe_array[2]
    qualifier <- phe_array[3]
    database_reference <- strsplit(phe_array[5], ";")[[1]]
    evidence_code <- phe_array[6]
    onset_modifier <- phe_array[7]
    frequency <- phe_array[8]
    aspect <- phe_array[11]

    # Populate annotation instance with checks
    annot_list[["database_id"]] <- as.character(database_id)
    annot_list[["disease_name"]] <- as.character(disease_name)
    if (qualifier != "") {
      annot_list[["qualifier"]] <- as.character(qualifier)
    }
    annot_list[["database_reference"]] <- database_reference
    annot_list[["evidence_code"]] <- as.character(evidence_code)
    if (onset_modifier != "") {
      annot_list[["onset_modifier"]] <- as.character(onset_modifier)
    }
    if (frequency != "") {
      annot_list[["frequency"]] <- as.character(frequency)
    }
    annot_list[["aspect"]] <- as.character(aspect)

    # Add annotation instance to HPO list
    hpo_list[[hpo_id]][["phenotype_annotation"]] <-
      append(hpo_list[[hpo_id]][["phenotype_annotation"]], list(annot_list))
  }
  close(con)

  # Make HPO list a dataframe so it works with MongoDB driver
  hpo_list<-do.call(rbind, hpo_list)
  hpo_list<-as.data.frame(hpo_list)
  row.names(hpo_list) <- NULL
  
  # Display progress message
  message("Importing data...")
  
  # Insert HPO data into database
  db_connection$insert(hpo_list, auto_unbox = TRUE, null = 'null')  
}
