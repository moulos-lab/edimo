importDbnsfpGene <- function(file_path, ver, org, db_connection) {
  message("Parsing...")

  # Determine if the file is gzipped
  is_gzipped <- grepl("\\.gz$", file_path)
  
  # Open file connection
  if (is_gzipped) {
    con <- gzfile(file_path, "r")
  } else {
    con <- file(file_path, "r")
  }

  # Skip header
  readLines(con, n = 1)
  
  # Initialize empty list to store parsed records
  dbgene <- list()

  # Read file line by line
  while (length(line <- readLines(con, n = 1)) > 0) {
    # Split line by tab
    parts <- strsplit(line, "\t")[[1]]
    
    # Extract fields
    gene_name <- parts[[1]]
    # ensembl_id <- parts[[2]]
    # chr <- parts[[3]]
    # gene_old <- parts[[4]]
    gene_other <- parts[[5]]
    # uniprot_acc <- parts[[6]]
    # uniprot_id <- parts[[7]]
    entrez_id <- parts[[8]]
    ccds_id <- parts[[9]]
    refseq_id <- parts[[10]]
    # ucsc_id <- parts[[11]]
    # mim_id <- parts[[12]]
    # omim_id <- parts[[13]]
    gene_full <- parts[[14]]
    # uniprot <- parts[[15]]
    biocarta_short <- parts[[16]]
    biocarta_full <- parts[[17]]
    cons_path_db <- parts[[18]]
    kegg_id <- parts[[19]]
    kegg_full <- parts[[20]]
    func <- parts[[21]]
    # disease <- parts[[22]]
    # mim_pheno_id <- parts[[23]]
    # mim_disease <- parts[[24]]
    # trait_gwas <- parts[[28]]
    # goslim_bp <- parts[[31]]
    # goslim_cc <- parts[[32]]
    # goslim_mf <- parts[[33]]
    # expr_egen <- parts[[35]]
    # expr_gnf <- parts[[36]]
    inter_intact <- parts[[37]]
    inter_biogrid <- parts[[38]]
    inter_cons <- parts[[39]]
    phi <- parts[[40]]
    prec <- parts[[44]]
    known_rec <- parts[[45]]
    essential_gene <- parts[[88]]
    # mgi_gene <- parts[[94]]
    # mgi_pheno <- parts[[95]]
    # zfin_gene <- parts[[96]]
    # zfin_stru <- parts[[97]]
    # zfin_phenoq <- parts[[98]]
    # zfin_phenot <- parts[[99]]
    
    # Create a record as a named list
    record <- list(
      version = ver,
      organism = org,
      gene_name = gene_name,
      synonyms = list(),
      entrez = list(),
      ccds = list(),
      refseq = list(),
      full_name = gene_full,
      biocarta = list(),
      cons_path_db = list(),
      kegg = list(),
      func = NULL,
      interaction_intact = list(),
      interaction_biogrid = list(),
      interaction_cons_path_db = list(),
      phi = NULL,
      prec = NULL,
      known_rec_info = NULL,
      essential_gene = NULL
    )

    if (gene_other != ".") record[["synonyms"]] <- strsplit(gene_other, ";")[[1]]
    if (entrez_id != ".") record[["entrez"]] <- strsplit(entrez_id, ";")[[1]]
    if (ccds_id != ".") record[["ccds"]] <- strsplit(ccds_id, ";")[[1]]
    if (refseq_id != ".") record[["refseq"]] <- strsplit(refseq_id, ";")[[1]]
    if (biocarta_short != ".") {
      record[["biocarta"]] <- Map(function(id, name) list(id = id, name = name),
        strsplit(biocarta_short, ";")[[1]], 
        strsplit(biocarta_full, ";")[[1]])
      names(record[["biocarta"]]) <- NULL
    }
    if (cons_path_db != ".") record[["cons_path_db"]] <- strsplit(cons_path_db, ";")[[1]]
    if (kegg_id != ".") {
      record[["kegg"]] <- Map(function(id, name) list(id = id, name = name),
        strsplit(kegg_id, ";")[[1]],
        strsplit(kegg_full, ";")[[1]])
      names(record[["kegg"]]) <- NULL
    }
    if (func != ".") record[["func"]] <- func
    if (inter_intact != ".") {
      record[["interaction_intact"]] <- Map(
        function(x) {
          parts <- strsplit(x, "\\[")[[1]]
          list(gene = parts[[1]], pid = as.list(strsplit(parts[[2]], ";")[[1]]))
        },
        strsplit(inter_intact, "\\];")[[1]]
      )
      names(record[["interaction_intact"]]) <- NULL
    }
    if (inter_biogrid != ".") {
      record[["interaction_biogrid"]] <- Map(
        function(x) {
          parts <- strsplit(x, "\\[")[[1]]
          list(gene = parts[[1]], pid = as.list(strsplit(parts[[2]], ";")[[1]]))
        },
        strsplit(inter_biogrid, "\\];")[[1]]
      )
      names(record[["interaction_biogrid"]]) <- NULL
    }
    if (inter_cons != ".") {
      record[["interaction_cons_path_db"]] <- Map(
        function(x) {
          parts <- strsplit(x, "\\[")[[1]]
          parts[[2]] <- gsub("\\]", "", parts[[2]])
          if (parts[[2]] != "NA") {
            list(gene = parts[[1]], score = as.double(parts[[2]]))
          } else {
            list(gene = parts[[1]], score = NULL)
          }
        },
        strsplit(inter_cons, ";")[[1]]
      )      
      names(record[["interaction_cons_path_db"]]) <- NULL
    }
    if (phi != ".") record[["phi"]] <- as.double(phi)
    if (prec != ".") record[["prec"]] <- as.double(prec)
    if (known_rec != ".") record[["known_rec_info"]] <- known_rec
    if (essential_gene != ".") record[["essential_gene"]] <- essential_gene
    
    # Append record to list
    dbgene <- c(dbgene, list(record))
  }
  # Close the file
  close(con)
  
  dbgene <- as.data.frame(do.call(rbind, dbgene))

  message("Importing...")
  # Insert records into database collection
  db_connection$insert(dbgene, auto_unbox = TRUE, null = 'null')
}
