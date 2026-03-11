source("/home/stelios/scripts/functions/importRevel.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "revel"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

file_path <- "/home/stelios/references/revel/v1.3/revel_with_transcript_ids"
ver <- "1.3"

start_time <- Sys.time()
# Function calls
importRevel(file_path, ver, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))