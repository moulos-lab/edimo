source("/home/stelios/scripts/functions/importDbnsfpGene.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "dbnsfp_gene"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

file_path <- "/home/stelios/references/dbNSFP/dbNSFP4.7/dbNSFP4.7_gene.complete.gz"
ver <- "4.7"
org <- "Homo_sapiens"

start_time <- Sys.time()
# Function calls
importDbnsfpGene(file_path, ver, org, db_connection)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))