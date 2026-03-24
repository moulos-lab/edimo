source("/home/stelios/scripts/functions/import_pharmgkb.R")
#source("/home/stelios/daily/import_pharmgkb.R")

# Path to the file that contains clingontest_url and db_name
source("/home/stelios/scripts/mongo_insert_scripts/db_credentials.R")

# Specify DBNAME
collection_name <- "pharmgkb"

db_connection <- mongolite::mongo(
  url = clingontest_url,
  db = db_name,
  collection = collection_name
)

#db_connection$drop()

gen <- "hg19"
dbver <- "20250905"

# clinical_file <- "/home/stelios/references/PharmGKB/clinical_annotations.tsv"
# clin_ann_alleles_file <- "/home/stelios/references/PharmGKB/clinical_ann_alleles.tsv"
# evidence_file <- "/home/stelios/references/PharmGKB/clinical_ann_evidence.tsv"
# gene_file <- "/home/stelios/references/PharmGKB/genes.tsv"
# variant_file <- "/home/stelios/references/PharmGKB/variants.tsv"
# chemical_file <- "/home/stelios/references/PharmGKB/chemicals.tsv"
# phenotype_file <- "/home/stelios/references/PharmGKB/phenotypes.tsv"
# fun_file <- "/home/stelios/references/PharmGKB/var_fa_ann.tsv"
# pheno_file <- "/home/stelios/references/PharmGKB/var_pheno_ann.tsv"
# drug_file <- "/home/stelios/references/PharmGKB/var_drug_ann.tsv"
clinical_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/summary_annotations.tsv")
clin_ann_alleles_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/summary_ann_alleles.tsv")
evidence_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/summary_ann_evidence.tsv")
gene_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/genes.tsv")
variant_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/variants.tsv")
chemical_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/chemicals.tsv")
phenotype_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/phenotypes.tsv")
fun_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/var_fa_ann.tsv")
pheno_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/var_pheno_ann.tsv")
drug_file <- paste0("/home/stelios/references/ClinPGx/",dbver,"/var_drug_ann.tsv")

start_time <- Sys.time()
# Display progress message
message("Started...")

# Function calls

# Load clinical allele, variant annotations, evidence
# and gene, variant, drugs, chemicals, phenotypes vocabularies
clin_ann_alleles_list <- getPharmgkbClinAnnAllelesEntries(clin_ann_alleles_file, gen, dbver)
varann_list <- getPharmgkbVarannEntries(pheno_file, drug_file, fun_file, gen, dbver)
evidence_list <- getPharmgkbEvidenceEntries(evidence_file, gen, dbver)
gene_list <- getPharmgkbGeneEntries(gene_file, gen, dbver)
variant_list <- getPharmgkbVariantEntries(variant_file, gen, dbver)
chemical_list <- getPharmgkbChemicalEntries(chemical_file, gen, dbver)
phenotype_list <- getPharmgkbPhenotypeEntries(phenotype_file, gen, dbver)

# Display progress message
message("Communicating with MongoDB...")

getPharmgkbClinicalEntries(db_connection, gen, dbver, 
                            clinical_file, 
                            varann_list, 
                            clin_ann_alleles_list, 
                            evidence_list, 
                            gene_list, 
                            variant_list, 
                            chemical_list, 
                            phenotype_list)
end_time <- Sys.time()
time_taken <- end_time - start_time

message("Finished. Time taken:", time_taken, units(time_taken))