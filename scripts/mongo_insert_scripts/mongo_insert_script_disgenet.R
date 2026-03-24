source("/home/stelios/scripts/functions/import_gene_databases.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name_gene <- "disgenet_gene"
collection_name_variant <- "disgenet_variant"

db_connection_gene <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name_gene
)
db_connection_variant <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name_variant
)

#db_connection_gene$drop()
#db_connection_variant$drop()

file_path <- "/home/stelios/references/disgenet_2020.db"
version <- "7.0"
organism <- "Homo_sapiens"


start_time <- Sys.time()
# Function calls
importDisgenetGene(file_path, version, organism, db_connection_gene)
importDisgenetVariant(file_path, version, organism, db_connection_variant)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))