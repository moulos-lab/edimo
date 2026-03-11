source("/home/stelios/scripts/acmg_scripts/parse_clinvar_chunks.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "clinvar"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

chunks_path <- "/media/raid/users/stelios/references/clinvar_chunks"
#ver <- "1.3"
org <- "Homo_sapiens"

start_time <- Sys.time()
# Function calls
importClinvar(chunks_path, org, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))