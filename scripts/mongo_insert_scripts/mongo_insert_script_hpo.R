source("/home/stelios/scripts/functions/import_gene_databases.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "hpo"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

ver <- "20250901"
phenotype_to_genes <- paste0("/home/stelios/references/HPO/",ver,"/phenotype_to_genes.txt")
annotations <- paste0("/home/stelios/references/HPO/",ver,"/phenotype.hpoa")

start_time <- Sys.time()
# Function calls
importHpo(ver, phenotype_to_genes,
               annotations,
               db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))