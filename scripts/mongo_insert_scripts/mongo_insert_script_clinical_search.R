source("/home/stelios/scripts/functions/import_clinical_search.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

collection_name <- "clinical_search"

db_connection <- mongolite::mongo(
  url = edimoclin_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()


disgenet_file <- "/home/stelios/references/disgenet_2020.db"
phenotype_to_genes <- "/home/stelios/references/HPO/phenotype_to_genes.txt"

start_time <- Sys.time()
# Function calls
importDisgenetDiseases(disgenet_file, db_connection)
importHpoPhenotypes(phenotype_to_genes, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))
