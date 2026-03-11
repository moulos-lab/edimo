### RefSeq field contains MANE info if present, otherwise standard mRNA info. Also contains ncRNA info.

### OK   Check refseq field missing for entry 3 ENST00000371582.8 in hg19 version transcript.0

library(data.table)

# Define a function to clean the grouped list columns
.clean_list_cols <- function(x) {
  # Flatten the list
  x <- unlist(x)
  # Remove empty strings
  x <- x[x != ""]
  # Return as a list
  return(as.list(x))
}

# Custom function to replace NULL with list() after merge()
.replace_null_with_list <- function(x) {
  if (is.null(x)) return(list())
  return(x)
}


wrapImportEnsembl <- function(file_gene, file_trans, file_ex, file_pept, gen, ver, org, db) {

  # Display status messages
  message("Parsing and importing Ensembl data for genome ", gen, ", ensembl version ", ver)
  
  # Load genes
  message("Genes...")
  genes <- parseGenes(file_gene)
  
  # Load transcripts
  message("Transcripts...")
  transcripts <- parseTranscripts(file_trans)

  # Load exons
  message("Exons...")
  exons <- parseExons(file_ex)
  
  # Load peptides
  message("Peptides...")
  peptides <- parsePeptides(file_pept)
  
  # Import Ensembl data
  message("Combining data...")
  importEnsembl(genes, transcripts, exons, peptides, gen, ver, org, db)
}

parseGenes <- function(file_gene) {
  # Load genes from file
  genes <- fread(file_gene, header = TRUE, sep = "\t", showProgress = FALSE)

  # For hg38 data Entrez data are missing, so NA column is added
  if (length(genes) == 11) {
    genes[, entrezgene := .(NA_integer_)]
  }

  colnames(genes) <- c("ensembl", "symbol", "chromosome", "start", "end", 
                       "strand", "band", "description", "biotype", "omim", 
                       "omim_morbid", "entrez")
  
  # Replace "" values in band with NAs and join multiple bands into one 
  genes <- genes[, .(band = paste(sort(unique(band)), collapse = "-")), 
                 by = .(ensembl, symbol, chromosome, start, end, strand, 
                        description, biotype, omim, omim_morbid, entrez)]
  genes[band == "", band := NA_character_]
  
  # Format strand info from -1/1 to -/+
  genes[, strand := ifelse(strand == -1, "-", "+")]
  
  # Collapse multiple values of omim, omim_morbid, entrez into lists
  genes <- genes[, .(
    omim = list(unique(na.omit(omim))),
    omim_morbid = list(unique(na.omit(omim_morbid))),
    entrez = list(unique(na.omit(entrez)))
  ), by = .(ensembl, symbol, chromosome, start, end, strand, band, description, biotype)]

  # Nest id and coordinates sections by unique ensembl values
  genes <- genes[, .(
    id = list(list(ensembl = ensembl, symbol = symbol, omim = omim, omim_morbid = omim_morbid, entrez = entrez)),
    coordinates = list(list(chromosome = chromosome, start = start, end = end, strand = strand, band = band))
  ), by = .(ensembl, description, biotype)]

  # Change omim, omim_morbid and entrez from vector in list of one, to correct list
  genes[, id := lapply(id, function(x) {
    x[["omim"]] <- .clean_list_cols(x[["omim"]])
    return(x)
  })]
  genes[, id := lapply(id, function(x) {
    x[["omim_morbid"]] <- as.list(unlist(x[["omim_morbid"]]))
    return(x)
  })]
  genes[, id := lapply(id, function(x) {
    x[["entrez"]] <- as.list(unlist(x[["entrez"]]))
    return(x)
  })]
  
  return(genes)
}

parseTranscripts <- function(file_trans) {
  # Load transcripts from file
  transcripts <- fread(file_trans, header = TRUE, sep = "\t", showProgress = FALSE)
  
  # For hg19 data refseq_mane(_clinical) data are missing, so NA columns are added
  if (length(transcripts) == 7) {
    transcripts[, c("refseq_mane_select", "refseq_mane_plus_clinical") := .("")]
  }

  colnames(transcripts) <- c("gene_id", "transcript_id", "start", "end", "biotype",
                             "refseq_mrna", "refseq_ncrna", "refseq_mane",
                             "refseq_mane_clinical")  

  # Make refseq IDs into lists instead of having multiple entries
  transcripts <- transcripts[, .(
    refseq_mrna = list(unique(na.omit(refseq_mrna))),
    refseq_ncrna = list(unique(na.omit(refseq_ncrna))),
    refseq_mane = list(unique(na.omit(refseq_mane))),
    refseq_mane_clinical = list(unique(na.omit(refseq_mane_clinical)))
  ), by = .(gene_id, transcript_id, start, end, biotype)]

  transcripts[, refseq := lapply(1:.N, function(i) {
      if (refseq_mane[i] != "" | refseq_mane_clinical[i] != "") {
          unique(na.omit(c(refseq_mane[i], refseq_mane_clinical[i], refseq_ncrna[i])))
      } else {
          unique(na.omit(c(refseq_mrna[i], refseq_ncrna[i])))
      }
  })]

  # Apply the function to the refseq columns
  transcripts[, refseq := lapply(refseq, .clean_list_cols)]
  transcripts[, refseq_mane := lapply(refseq_mane, .clean_list_cols)]
  transcripts[, refseq_mane_clinical := lapply(refseq_mane_clinical, .clean_list_cols)]

  # # Drop individual refseq columns
  # transcripts <- transcripts[,-c(6:9)]  
  # Drop individual refseq_mrna, refseq_ncrna column
  transcripts <- transcripts[,-c(6,7)]  

  return(transcripts)
}

parseExons <- function(file_ex) {
  # Load exons from file
  exons <- fread(file_ex, header = TRUE, sep = "\t", showProgress = FALSE)

  colnames(exons) <- c("start", "end", "exon_id", "transcript_id")

  return(exons)
}

parsePeptides <- function(file_pept) {
  # Load peptides from file
  peptides <- fread(file_pept, header = TRUE, sep = "\t", showProgress = FALSE)
  
  colnames(peptides) <- c("transcript_id", "peptide_id", "swissprot", "trembl")
  
  # Make uniprot IDs into lists instead of having multiple entries
  peptides <- peptides[, .(
    swissprot = list(unique(na.omit(swissprot))),
    trembl = list(unique(na.omit(trembl)))
  ), by = .(transcript_id, peptide_id)]
  
  # Apply the function to the swissprot, trembl columns
  peptides[, swissprot := lapply(swissprot, .clean_list_cols)]
  peptides[, trembl := lapply(trembl, .clean_list_cols)]

  return(peptides)
}

importEnsembl <- function(genes, transcripts, exons, peptides, gen, ver, org, db) {
  # Transform to group by transcript_id and nest other fields
  exons <- exons[, .(exons = list(.SD)), by = transcript_id] 
  
  # Transform to group by transcript_id and nest other fields
  peptides <- peptides[, .(peptides = list(.SD)), by = transcript_id]  
  
  # Merge the exons data.table on the transcript_id column
  transcripts <- merge(transcripts, exons,
                       by.x = "transcript_id", by.y = "transcript_id",
                       all.x = TRUE, sort = FALSE)
  
  # Merge the peptides data.table on the transcript_id column
  transcripts <- merge(transcripts, peptides,
                       by.x = "transcript_id", by.y = "transcript_id",
                       all.x = TRUE, sort = FALSE)
  
  # Apply the custom function to ensure no NULL values
  transcripts[, exons := lapply(exons, .replace_null_with_list)]
  transcripts[, peptides := lapply(peptides, .replace_null_with_list)]
  
  # Transform to group by gene_id and nest other fields
  transcripts <- transcripts[, .(transcripts = list(.SD)), by = gene_id]  

  # Merge the transcripts data.table with genes on the gene_id column
  genes <- merge(genes, transcripts, by.x = "ensembl", by.y = "gene_id",
                 all.x = TRUE, sort = FALSE)

  # Drop gene_id_no_version, gene_id, ensembl columns
  genes <- genes[, ensembl := NULL]
  
  genes[, metadata := list(list(
    version = as.character(ver),
    organism = org,
    genome_version = gen
  ))]
  
  # Display import status message
  message("Importing...")
  
  # Insert genes into database collection
  db$insert(genes, auto_unbox = TRUE, na = 'null')
}
