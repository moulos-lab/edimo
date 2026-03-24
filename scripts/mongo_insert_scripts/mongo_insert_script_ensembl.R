source("/home/stelios/scripts/functions/fetch_ensembl.R")
# source("/home/stelios/scripts/functions/import_ensembl.R")
source("/home/stelios/scripts/functions/import_ensembl_wMANE.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
#collection_name <- "ensembl_test"
collection_name <- "ensembl"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()
hg19_ver <- 75
hg38_ver <- 115

file_gene_19 <- paste0("/home/stelios/references/ensembl/ensembl_hg19_gene_v",hg19_ver,".csv")
file_transcipt_19 <- paste0("/home/stelios/references/ensembl/ensembl_hg19_transcript_v",hg19_ver,".csv")
file_exon_19 <- paste0("/home/stelios/references/ensembl/ensembl_hg19_exon_v",hg19_ver,".csv")
file_peptide_19 <- paste0("/home/stelios/references/ensembl/ensembl_hg19_peptide_v",hg19_ver,".csv")
file_gene_38 <- paste0("/home/stelios/references/ensembl/ensembl_hg38_gene_v",hg38_ver,".csv")
file_transcipt_38 <- paste0("/home/stelios/references/ensembl/ensembl_hg38_transcript_v",hg38_ver,".csv")
file_exon_38 <- paste0("/home/stelios/references/ensembl/ensembl_hg38_exon_v",hg38_ver,".csv")
file_peptide_38 <- paste0("/home/stelios/references/ensembl/ensembl_hg38_peptide_v",hg38_ver,".csv")
genomes <- list("hg19", "hg38")
org <- "Homo_sapiens"

start_time <- Sys.time()
# Function calls
#fetchEnsembl(genomes)
#wrapImportEnsembl(file_gene_19, file_transcipt_19, file_exon_19, file_peptide_19, "hg19", as.character(hg19_ver), org, db_connection)
wrapImportEnsembl(file_gene_38, file_transcipt_38, file_exon_38, file_peptide_38, "hg38", as.character(hg38_ver), org, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))