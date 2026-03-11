source("/home/stelios/scripts/functions/import_cosmicid.R")
# source("/home/stelios/scripts/functions/import_cosmicid_w_liftover.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name_point <- "cosmicid_point"
# collection_name_struct <- "cosmicid_structural"
# collection_name_cna <- "cosmicid_cna"

db_connection_point <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name_point
)
# db_connection_struct <- mongolite::mongo(
#   url = clingontest_url,
#   db = db_name,
#   collection = collection_name_struct
# )
# db_connection_cna <- mongolite::mongo(
#   url = clingontest_url,
#   db = db_name,
#   collection = collection_name_cna
# )

#db_connection_point$drop()
# db_connection_struct$drop()
# db_connection_cna$drop()

file_point_coding_grch37_path <- "/home/stelios/references/CosmicID/Cosmic_GenomeScreensMutant_v100_GRCh37.tsv"
file_point_coding_grch38_path <- "/home/stelios/references/CosmicID/Cosmic_GenomeScreensMutant_v100_GRCh38.tsv"
file_point_noncoding_grch37_path <- "/home/stelios/references/CosmicID/Cosmic_NonCodingVariants_v100_GRCh37.tsv"
file_point_noncoding_grch38_path <- "/home/stelios/references/CosmicID/Cosmic_NonCodingVariants_v100_GRCh38.tsv"
file_point_targeted_grch37_path <- "/home/stelios/references/CosmicID/Cosmic_CompleteTargetedScreensMutant_v100_GRCh37.tsv"
file_point_targeted_grch38_path <- "/home/stelios/references/CosmicID/Cosmic_CompleteTargetedScreensMutant_v100_GRCh38.tsv"
file_struct_grch37_path <- "/home/stelios/references/CosmicID/Cosmic_StructuralVariants_v100_GRCh37.tsv"
file_struct_grch38_path <- "/home/stelios/references/CosmicID/Cosmic_StructuralVariants_v100_GRCh38.tsv"
file_cna_grch37_path <- "/home/stelios/references/CosmicID/Cosmic_CompleteCNA_v100_GRCh37.tsv"
file_cna_grch38_path <- "/home/stelios/references/CosmicID/Cosmic_CompleteCNA_v100_GRCh38.tsv"
hg19_to_hg38_chain <- "/home/stelios/references/hg19ToHg38.over.chain"
hg38_to_hg19_chain <- "/home/stelios/references/hg38ToHg19.over.chain"
ver<-"100"


start_time <- Sys.time()
# Function calls
importCosmicPoint(file_point_coding_grch37_path, 
                  file_point_coding_grch38_path, 
                  file_point_noncoding_grch37_path, 
                  file_point_noncoding_grch38_path, 
                  file_point_targeted_grch37_path, 
                  file_point_targeted_grch38_path, 
                  ver, db_connection_point)
#importCosmicPoint(file_point_coding_grch37_path, 
#                  file_point_coding_grch38_path, 
#                  file_point_noncoding_grch37_path, 
#                  file_point_noncoding_grch38_path, 
#                  file_point_targeted_grch37_path, 
#                  file_point_targeted_grch38_path, 
#                  ver, db_connection_point, hg19_to_hg38_chain, hg38_to_hg19_chain)
# importCosmicStructural(file_struct_grch37_path, "hg19", ver, db_connection_struct)
# importCosmicStructural(file_struct_grch38_path, "hg38", ver, db_connection_struct)
# importCosmicCNA(file_cna_grch37_path, "hg19", ver, db_connection_cna)
# importCosmicCNA(file_cna_grch38_path, "hg38", ver, db_connection_cna)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Time taken:", time_taken, units(time_taken))