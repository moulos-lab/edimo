library(RSQLite)
library(mongolite)

importDisgenetDiseases <- function(disgenet_file, db_connection) {
  # Open a connection to the SQLite DisGeNET database
  con <- dbConnect(RSQLite::SQLite(), disgenet_file)

  # Construct the SQL query to retrieve disease attributes
  disease_query <- paste0(
    "SELECT diseaseId, diseaseName, type ",
    "FROM diseaseAttributes ",
    "WHERE type = 'disease'")

  # Prepare and execute the SQL query
  disease_data <- dbGetQuery(con, disease_query)
  colnames(disease_data) <- c("id", "name", "type")
  
  # Display progress message
  message("Importing data...")

  # Insert disgenet data into database
  db_connection$insert(disease_data, auto_unbox = TRUE, null = 'null')
}

importHpoPhenotypes <- function(phenotype_to_genes, db_connection) {
  # Display progress message
  message("Parsing HPO...")

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
        id = hpo_id,
        name = hpo_array[2],
        type = "phenotype"
      )
      hpo_list[[hpo_id]] <- phe_list
    }
  }
# }
# importHpoPhenotypes <- function(phe_file, db_connection) {
  # # Open connection to phenotype annotations file
  # message("Load Phenotype file .........")
  # con <- file(phe_file, "r")

  # # Skip comments and header
  # while (TRUE) {
    # phe_line <- readLines(con, n = 1)
    # if (length(phe_line) == 0) break  # End of file
    # if (!grepl("^#", phe_line)) break  # Not a comment
  # }

  # # Initialize HPO list
  # hpo_list <- list()

  # # Process each line in the phenotype annotations file
  # while (length(phe_line <- readLines(con, n = 1)) > 0) {    
    # phe_array <- strsplit(phe_line, "\t")[[1]]
    
    # hpo_id <- as.character(phe_array[4])
    # disease_name <- as.character(phe_array[2])

    # annot_list <- list(
      # id = NULL,
      # name = NULL,
      # type = "phenotype"
    # )

    # annot_list[["id"]] <- hpo_id
    # annot_list[["name"]] <- disease_name

    # hpo_list[[hpo_id]] <- annot_list
  # }
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