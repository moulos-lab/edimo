source("/home/stelios/scripts/functions/import_gene_databases.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "cgd"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

file_path <- "/home/stelios/references/CGD/CGD_20250224.txt"
version <- "20250224"
organism <- "Homo_sapiens"

start_time <- Sys.time()
# Function calls
importCgd(file_path, version, organism, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))