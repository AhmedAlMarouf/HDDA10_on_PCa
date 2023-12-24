# Author: Konstantinos Liosis
# import libraries
library(readxl)
library(dplyr)
library(stringi)

# load UCalgary clinical data
uofc <- read.csv("UCalgary_clinical.csv")
old_bx_uids <- uofc[,1]
# rename biopsy and rp uid columns
colnames(uofc)[1:2] <- c("bx_uid", "Prostatectomy Surgical #")
# load IHC 2005 data
ihc2005 <- read_excel("IHC_data_2005.xlsx")
# discard useless entries
ihc2005 <- ihc2005[1:30, ]
# rename patient id and biopsy uid columns
colnames(ihc2005)[1:2] <- c("patient_id", "bx_uid")

# load IHC 2006-2013 data
ihc2006 <- read_excel("IHC_2006-2013.xlsx")
# discard useless entries
ihc2006 <- ihc2006[1:33, ]
# rename patient id and biopsy uid columns
colnames(ihc2006)[1:2] <- c("patient_id", "bx_uid")

# load controls
controls <- read_excel("controls.xlsx")
# discard useless entries
controls <- controls[, 2:ncol(controls)]
colnames(controls)[1:2] <- c("patient_id", "bx_uid")
colnames(controls)[5] <- c("IHC")
controls <- controls %>% filter(grepl("SR", bx_uid))


# UofC

new_ids <- c()
for (entry in uofc$bx_uid){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
uofc$bx_uid <- new_ids

new_ids <- c()
for (entry in uofc$"Prostatectomy Surgical #"){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
uofc$"Prostatectomy Surgical #" <- new_ids


# IHC 2006

new_ids <- c()
for (entry in ihc2006$bx_uid){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
ihc2006$bx_uid <- new_ids

new_ids <- c()
for (entry in ihc2006$"Prostatectomy Surgical #"){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
ihc2006$"Prostatectomy Surgical #" <- new_ids

# IHC 2005

new_ids <- c()
for (entry in ihc2005$bx_uid){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
ihc2005$bx_uid <- new_ids

new_ids <- c()
for (entry in ihc2005$"Prostatectomy Surgical #"){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
ihc2005$"Prostatectomy Surgical #" <- new_ids

# Controls

new_ids <- c()
for (entry in controls$bx_uid){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
controls$bx_uid <- new_ids

new_ids <- c()
for (entry in controls$"Prostatectomy Surgical #"){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
controls$"Prostatectomy Surgical #" <- new_ids


cat_ihc <- na.omit(rbind(select(ihc2005, c("patient_id", "bx_uid", "Prostatectomy Surgical #")),
                         select(ihc2006, c("patient_id", "bx_uid", "Prostatectomy Surgical #")),
                         select(controls, c("patient_id", "bx_uid", "Prostatectomy Surgical #")),
                         select(uofc, c("patient_id", "bx_uid", "Prostatectomy Surgical #"))))




# Read in the new-complete file
complete_uofc <- read_excel("210318_UCalgary_AllInfo.xlsx")

# Fix the complete file
extra_cols <- data.frame(surgical_biopsy_number = NA,
                         surgical_rp_number = NA,
                         block_id = NA,
                         stringsAsFactors = F)
colnames(extra_cols) <- c("surgical_biopsy_number", "surgical_rp_number", "block_id")
for (i in 1:nrow(complete_uofc)){
  entry <- complete_uofc$specimen_id[i]
  slash_pos <- unlist(stri_locate_all(pattern = '/', entry, fixed = TRUE))[1]
  if (!is.na(slash_pos)){
    block_id <- substr(entry, slash_pos+1, nchar(entry))
    temp <- substr(entry, 1, slash_pos-1)
  }
  else {
    block_id <- NA
    temp <- entry
  }
  last_dash <- stri_locate_last(temp, regex = "-")[1]
  if (!is.na(last_dash) && last_dash > 9){
    temp <- substr(temp, 1, last_dash-1)
  }
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_id <- paste0(substr(temp, 1, 6), as.character(uid))
  if (complete_uofc$sample_type[i] == "Biopsy (Y)"){
    rp <- cat_ihc[which(cat_ihc$bx_uid == new_id), "Prostatectomy Surgical #"]$"Prostatectomy Surgical #"[1]
    extra_cols <- rbind(extra_cols, c(new_id, rp, block_id))
  }
  else{
    bx <- cat_ihc[which(cat_ihc$"Prostatectomy Surgical #" == new_id), "bx_uid"]$"bx_uid"[1]
    extra_cols <- rbind(extra_cols, c(bx, new_id, block_id))
  }
}
extra_cols <- extra_cols[2:nrow(extra_cols), ]


uofc_ge <- read.csv("gdxExonV1-expressionData-TBL-Heterogeneity-Analysis-V2-2.csv", sep=',', header = FALSE, stringsAsFactors = F)
cnames <- uofc_ge[, 1]
uofc_ge <- uofc_ge[, -1]
uofc_ge <- as.data.frame(t(uofc_ge), stringsAsFactors = F)
colnames(uofc_ge) <- cnames

new_ids <- c()
for (entry in uofc_ge$surgical_biopsy_number){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
uofc_ge$surgical_biopsy_number <- new_ids

new_ids <- c()
for (entry in uofc_ge$surgical_rp_number){
  temp <- entry
  if (!startsWith(temp, 'S')){
    temp <- substr(temp, unlist(gregexpr("SR", temp)), nchar(temp))
  }
  if (!grepl('-', temp, fixed = TRUE)){
    temp <- paste(substr(temp, 1, 2),
                  substr(temp, 3, 4),
                  substr(temp, 5, nchar(temp)),
                  sep = '-')
  }
  uid <- as.numeric(substr(temp, 7, nchar(temp)))
  new_ids <- c(new_ids, paste0(substr(temp, 1, 6), as.character(uid)))
}
uofc_ge$surgical_rp_number <- new_ids

write.csv(ihc2006, "IHC2006_final.csv")
write.csv(ihc2005, "IHC2005_final.csv")
write.csv(controls, "controls_final.csv")
write.csv(uofc, "uofc_clinical_final.csv")
write.csv(cbind(extra_cols, complete_uofc), "uofc_complete_decipher_file_final.csv")
write.csv(uofc_ge, "uofc_gene_exprs_final.csv")


