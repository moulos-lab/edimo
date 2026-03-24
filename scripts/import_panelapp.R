### Inheritance standardized strings are messy!

# Ensure required packages are installed and loaded
library(httr2)
library(data.table)

# Define the base URL for the PanelApp API
base_url <- "https://panelapp.genomicsengland.co.uk/api/v1/"

# Function to query the PanelApp API and fetch all panels
query_panelapp <- function(endpoint) {
  url <- paste0(base_url, endpoint)
  all_results <- list()
  
  repeat {
    # Initialize the request
    req <- request(url)
    
    # Perform the request
    response <- req_perform(req)
    
    # Parse the JSON response
    response_json <- resp_body_json(response)
    
    # Append the results to the list
    all_results <- c(all_results, response_json$results)
    
    # Check if there is a next page
    if (is.null(response_json[["next"]])) {
      break
    }
    
    # Update the URL for the next page
    url <- response_json[["next"]]
  }
  
  return(all_results)
}

# Helper function to extract chr and coords from gene entries
.extract_coordinates <- function(coordinate_string) {
  # Split the string at the ':' delimiter
  chr_coords <- strsplit(coordinate_string, ":")[[1]]
  
  # Extract chr and the start-stop string
  chr <- chr_coords[1]
  start_end <- chr_coords[2]
  
  # Split the start-stop string at the '-' delimiter
  start_end_split <- strsplit(start_end, "-")[[1]]
  
  # Extract start and stop values
  start <- as.integer(start_end_split[1])
  end <- as.integer(start_end_split[2])
  
  # Return the values as a list
  return(list(chr = chr, start = start, end = end))
}

# Function to get genes for each panel
get_genes_for_panel <- function(panel_id) {
  message(paste("Retrieving genes for panel with ID:", panel_id))
  endpoint <- paste0("panels/", panel_id, "/genes")
  panel_genes <- query_panelapp(endpoint)
  gene_list <- lapply(panel_genes, function(gene) {
    gene_data <- gene$gene_data

    gene_list_inst <- list(
      gene_symbol = gene_data$gene_symbol,
      chr = NULL,
      grch37_coords = NULL,
      grch38_coords = NULL,
      ensembl_ids = NULL,
      inheritance = NULL,
      penetrance = NULL,
      phenotypes = gene$phenotypes
    )
    
    if (length(gene_data$ensembl_genes) != 0) {
      # There are a few entries like this, so need to check
      if (!identical(gene_data$ensembl_genes, "{}")) {
        # Look only into the most recent ensembl version for each genome
        grch37_data <- gene_data$ensembl_genes$GRch37[[length(gene_data$ensembl_genes$GRch37)]]
        grch38_data <- gene_data$ensembl_genes$GRch38[[length(gene_data$ensembl_genes$GRch38)]]
        
        # Check if location info exists and get it
        if (!is.null(grch37_data$location)) {
          grch37_loc <- .extract_coordinates(grch37_data$location)
          grch37_coords <- list(start = grch37_loc$start, end = grch37_loc$end)
          # Populate on if they exist
          gene_list_inst$grch37_coords <- grch37_coords
        } else {
          grch37_loc <- NULL   # So that chr is correct
        }
        if (!is.null(grch38_data$location)) {
          grch38_loc <- .extract_coordinates(grch38_data$location)
          grch38_coords <- list(start = grch38_loc$start, end = grch38_loc$end)
          # Populate on if they exist
          gene_list_inst$grch38_coords <- grch38_coords
        } else {
          grch38_loc <- NULL   # So that chr is correct
        }
        # There should only be one chr, but just to check
        chr <- unlist(unique(c(grch37_loc$chr, grch38_loc$chr)))
        ensembl_ids <- list(unique(unlist(c(grch37_data$ensembl_id, grch38_data$ensembl_id))))
      
        # Populate values only if they exist
        gene_list_inst$chr <- chr
        gene_list_inst$ensembl_ids <- ensembl_ids
      }
    }
    if (gene$mode_of_inheritance != "") {
      gene_list_inst$inheritance <- gene$mode_of_inheritance
    }
    if (!is.null(gene$penetrance)) {
      gene_list_inst$penetrance <- gene$penetrance
    }
    
    return(gene_list_inst)
  })
  return(gene_list)
}

importPanelapp <- function(panelapp_version) {
  # Fetch the complete list of gene panels
  panels_endpoint <- "panels/"
  panels <- query_panelapp(panels_endpoint)

  # Initialize a list to store the structured data
  structured_data <- list()
  # Iterate over each panel to fetch its details and genes
  for (panel in panels) {
    panel_id <- panel$id
    panel_name <- panel$name
    panel_version <- panel$version
    panel_descriptions <- lapply(panel$type, function(x) x$description)
    panel_disease_group <- panel$disease_group
    panel_disease_subgroup <- panel$disease_sub_group
    
    # Fetch genes for the panel
    genes_info <- get_genes_for_panel(panel_id)

    # Structure the data for the current panel
    panel_data <- list(
      panelapp_version = panelapp_version,
      panel_id = panel_id,
      version = panel_version,
      name = panel_name,
      description = panel_descriptions,
      disease_group = NULL,
      disease_subgroup = NULL,
      genes = genes_info,
      share_level = NULL,
      share_with = NULL
    )
    
    # Populate info only if present
    if (panel_disease_group != "") {
      panel_data$disease_group <- panel_disease_group
    }
    if (panel_disease_subgroup != "") {
      panel_data$disease_subgroup <- panel_disease_subgroup
    }
    
    # Append the structured panel data to the list
    structured_data <- append(structured_data, list(panel_data))
  }

  structured_data <- do.call(rbind, structured_data)
  structured_data <- as.data.frame(structured_data)

  db_connection$insert(structured_data, auto_unbox = TRUE, na = 'null', null = 'null')
}