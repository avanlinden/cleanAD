## MSBB Whole Genome Sequencing data

# 2020-09-10

library("synapser")
library("readxl")
library("tidyverse")
library("dccvalidator")
synLogin()

# Load files
template <- synGet("syn21018358", version = 7)
template <- read_excel(template$path)

wgs_cov <- synGet("syn11384608", version = 6)
wgs_cov <- read_delim(wgs_cov$path, delim = "\t")

wgs_annots <- synGetAnnotations("syn11384608")

biosp <- synGet("syn21893059", version = 8)
biosp <- read_csv(biosp$path, guess_max = 10000)

# Are sample IDs all in biospecimen file?
all(wgs_cov$sampleIdentifier %in% biosp$specimenID) # yes -- good

# Any duplicates?
any(duplicated(wgs_cov$sampleIdentifier)) # no -- good

# Fix column names. None of the columns in the wgs covariates file correspond
# to things in the wgs assay metadata template -- they're all individual or
# biospecimen information. We only retain the specimen ID for mapping.
wgs_assay <- wgs_cov %>%
  transmute(specimenID = sampleIdentifier) %>%
  left_join(template, by = "specimenID") %>%
  mutate(platform = "HiSeqX", assay = "wholeGenomeSeq") # taken from assay description

# Save file and annotations
path <- "MSBB_assay_wholeGenomeSeq_metadata.csv"
write.csv(wgs_assay, path, row.names = FALSE)
new_file <- File(path = path, parent = "syn22344122")
new_file_stored <- synStore(new_file, forceVersion = FALSE)

annots <- unlist(wgs_annots, recursive = FALSE)
synSetAnnotations(new_file_stored$properties$id, annots)
