##################################
# Generate specimen table for AD #
##################################

# Import package -----
suppressPackageStartupMessages(library("synapser"))

# Use synapser and log in -----
synLogin()

# Variables -----
# Top directories
id_model <- "syn5550383"
id_human <- "syn5550382"
# Specimen Table
id_table <- "syn21578908"
# All data file view to get annotations from
file_view <- "syn11346063"

RELEVANT_METADATA_COLUMNS <- c("individualID", "specimenID", "assay")
METADATA_TYPES <- c("biospecimen", "assay", "individual")

# Get study metadata file IDs -----
dir_ids <- c(id_human, id_model)
all_files <- purrr::map_dfr(dir_ids, ~ gather_metadata_synIDs_all(.))

# Remove files that are probably not metadata -----
# Only keep csv files that don't have 'dictionary' in name
all_files <- all_files[grepl(".csv$", all_files$name, ignore.case = TRUE), ]
all_files <- all_files[!grepl("dictionary", all_files$name, ignore.case = TRUE), ] # nolint
# Fix id column to be character
all_files$id <- as.character(all_files$id)

# Grab file view for annotations -----
view_query <- synapser::synTableQuery(
  glue::glue("SELECT * FROM {file_view}")
)$asDataFrame()

# Grab metadata-related annotations from file view and join -----
view_meta <- view_query[view_query$id %in% all_files$id, ]
all_files <- dplyr::left_join(
  all_files,
  view_meta[, c("id", "dataType", "metadataType", "assay")]
)
# Remove any with metadataType dictionary or protocol
all_files <- all_files[!grepl("dictionary|protocol", all_files$metadataType), ]

# Open files and gather IDs
all_meta_ids <- gather_ids_all_studies(all_files)

# Grab all file annotations
all_annots <- simple_table_query(table_id = file_view)

# Remove json from annotated study names
all_annots[, "study"] <- unlist(purrr::map(
  all_annots$study,
  ~ clean_json_string(., remove_spaces = TRUE)
))
# Separate into multiple rows for IDs that have multiple study annotations
all_annots <- tidyr::separate_rows(all_annots, study, sep = ",")

# Bind together with metadata to get full set
all_ids <- rbind(all_meta_ids, all_annots)

# Clean up NAs and duplicates
all_ids <- dplyr::distinct(all_ids)
# Don't need rows where BOTH the individualID and specimenID are NA
all_ids <- all_ids[!(is.na(all_ids$specimenID) & is.na(all_ids$individualID)), ]

# Grab samples table, delete old data, add new data
#! COMMENTED OUT BECAUSE THIS IS DESTRUCTIVE -- MAKE SURE THE SCRIPT
#! IS CORRECT BEFORE DEPLOYING
# update_samples_table(table_id = id_table, new_data = all_ids)
