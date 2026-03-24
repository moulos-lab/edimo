# Only values that are not INT in pos_hg38 are missing "." values.

library(data.table)

importRevel <- function(file, ver, db_connection) {
  revel <- fread(file, header = TRUE, sep = ",", showProgress = FALSE)

  colnames(revel) <- c("chr", "pos_hg19", "pos_hg38", "ref", "alt", 
                       "ref_aa", "alt_aa", "revel_score", "ensembl_transcript_id")

  revel[, "version" := .(ver)]

  revel[, pos_hg38 := lapply(pos_hg38, as.integer)]
  
  db_connection$insert(revel, auto_unbox = TRUE, na = 'null')

}
