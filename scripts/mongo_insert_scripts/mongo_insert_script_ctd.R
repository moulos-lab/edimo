source("/home/stelios/scripts/functions/importCtd.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "ctd_gene"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

dbver<-"20250827"
chem_file_path <- paste0("/home/stelios/references/CTD/",dbver,"/CTD_chemicals.tsv")
chem_gene_file_path <- paste0("/home/stelios/references/CTD/",dbver,"/CTD_chem_gene_ixns.tsv")
dis_file_path <- paste0("/home/stelios/references/CTD/",dbver,"/CTD_diseases.tsv")
disease_chem_file_path <- paste0("/home/stelios/references/CTD/",dbver,"/CTD_chemicals_diseases.tsv")


start_time <- Sys.time()
# Function calls
importCtd(db_connection, dbver, chem_file_path, chem_gene_file_path, dis_file_path, disease_chem_file_path)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))