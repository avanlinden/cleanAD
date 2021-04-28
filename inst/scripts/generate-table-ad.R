##################################
# Generate specimen table for AD #
##################################

# Import package -----


# Use synapser and log in -----
synapser$login()

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

# Functions --------------------------------------------------------------------

## Finding metadata files -----

#' @title Gather metadata synIDs
#'
#' @description Gather synIDs for metadata files
#'
#' @param dir_id synID for top level directory.
#' @return table with file synID, filename, and study name.
gather_metadata_synIDs <- function(dir_id) {
  children <- synapser::synGetChildren(dir_id)$asList()
  study_metadata <- purrr::map(children, function(study_folder) {
    gather_metadata_synIDs_helper(
      dir_id = study_folder$id,
      study = study_folder$name
    )
  })
  study_metadata <- Reduce(rbind, study_metadata)
  study_metadata <- na.omit(study_metadata)
  return(study_metadata)
}

#' @title Gather metadata synIDs helper
#'
#' @description Gather synIDs for metadata files
#'
#' @param dir_id synID for directory.
#' @param study name of study.
gather_metadata_synIDs_helper <- function(dir_id, study) {
  children <- synapser::synGetChildren(dir_id)$asList()
  has_metadata <- does_child_exist(children, c("Metadata", "metadata"))
  if (any(has_metadata)) {
    metadata <- gather_metadata_synIDs_files(
      dir_id = purrr::flatten(children[has_metadata])$id,
      study = study
    )
    return(metadata)
  }
  has_data <- does_child_exist(children, c("Data", "data"))
  if (any(has_data)) {
    metadata <- gather_metadata_synIDs_helper(
      dir_id = purrr::flatten(children[has_data])$id,
      study = study
    )
    return(metadata)
  } else {
    return(NA)
  }
}

#' @title See if child folder exists
#'
#' @description See if child folder exists
#'
#' @param children list of children.
#' @param dir_name vector of names to search for.
#' @return boolean vector indicating whether the child matches one
#' of the dir_name(s) in same order as children.
does_child_exist <- function(children, dir_name) {
  purrr::map_lgl(children, function(child) {
    child$name %in% dir_name
  })
}

#' @title Gather metadata file synIDs
#'
#' @description Gather the file synIDs
#'
#' @param dir_id synID for metadata directory.
#' @param study name of study.
gather_metadata_synIDs_files <- function(dir_id, study) {
  children <- synapser::synGetChildren(dir_id)$asList()
  if (length(children) > 0) {
    df <- as.data.frame(Reduce(rbind, children))
    df <- dplyr::select(df, name, id)
    df["study"] <- study
    # Remove weird rownames
    rownames(df) <- c()
    return(df)
  }
  return(NA)
}

# Pull specimen and individual IDs from files

#' @title Gather IDs from metadata files
#'
#' @description Assumes all metadata files are csv with some combination of
#' columns: `individualID`, `specimenID`, `assay`. Gathers by study grouping.
#'
#' @param all_files dataframe with columns `id` (synID of file), `study` (name
#' of related study), `metadataType` (`assay`, `individual`, or `biospecimen`),
#' `assay` (type of assay), `dataType` (type of data).
gather_ids <- function(all_files) {
  all_studies <- unique(all_files$study)
  all_metadata <- purrr::map(all_studies, function(study) {
    study_metadata <- add_missing_meta_rows(
      all_files[all_files$study %in% study, ]
    )
    study_metadata <- get_all_study_data(study_metadata)
    study_metadata <- gather_ids_helper(study_metadata)
    if (!inherits(study_metadata, "data.frame")) {
      return(as.character(NA))
    }
    study_metadata[, "study"] <- unique(meta_files$study)
    return(study_metadata)
  })
  Reduce(rbind, all_metadata)
}

gather_ids_helper <- function(meta_files) {
  # Should have none that are integer(0), but data will be NA
  file_indices <- get_file_indices(meta_files)
  # Join in order of assay, biospecimen, individual
  # Reorder
  ordering <- c(
    file_indices$assay,
    file_indices$biospecimen,
    file_indices$individual
  )
  meta_files <- meta_files[ordering, ]
  # Remove any rows that have `NA` for data
  meta_files <- meta_files[!is.na(meta_files$data), ]
  if (nrow(meta_files) == 0) {
    return(as.character(NA))
  }
  metadata <- Reduce(
    dplyr::full_join,
    meta_files$data
  )
  return(metadata)
}

get_all_study_data <- function(meta_files) {
  study_data <- purrr::map2(meta_files$id, meta_files$assay, function(id, assay) {
    if (is.na(id)) {
      return(as.character(NA))
    }
    metadata <- get_file_data(id)
    if (!inherits(metadata, "data.frame")) {
      return(as.character(NA))
    }
    metadata <- metadata[, colnames(metadata) %in% RELEVANT_METADATA_COLUMNS]
    if (length(metadata) == 0) {
      return(as.character(NA))
    }
    # If not empty and an assay, make sure has assay column with type
    if (!is.na(assay) & !"assay" %in% colnames(metadata)) {
      metadata[, "assay"] <- assay
    }
    # Make sure all columns are type `character`
    for (column in colnames(metadata)) {
      if (!inherits(metadata[, column], "character")) {
        metadata[, column] <- as.character(metadata[, column])
      }
    }
    return(metadata)
  })
  meta_files[, "data"] <- list(study_data)
  return(meta_files)
}

add_missing_meta_rows <- function(meta_files) {
  file_indices <- get_file_indices(meta_files)
  # If any type is `integer(0)`, then add NA row to meta_files
  # Makes it easier to join in correct order later
  if (length(file_indices$individual) == 0) {
    meta_files <- add_empty_row(meta_files, type = "individual")
  }
  if (length(file_indices$biospecimen) == 0) {
    meta_files <- add_empty_row(meta_files, type = "biospecimen")
  }
  if (length(file_indices$assay) == 0) {
    meta_files <- add_empty_row(meta_files, type = "assay")
  }
  return(meta_files)
}

get_file_indices <- function(meta_files, meta_types = METADATA_TYPES) {
  file_indices <- purrr::map(
    meta_types,
    function(meta_type) {
      which(meta_files$metadataType %in% meta_type)
    }
  )
  names(file_indices) <- meta_types
  return(file_indices)
}

add_empty_row <- function(meta_files, type) {
  df_row <- data.frame(id = NA, metadataType = type, study = unique(meta_files$study))
  for (name in setdiff(colnames(meta_files), c("id", "metadataType", "study"))) {
    df_row[, name] <- as.character(NA)
  }
  # Reorder columns
  df_row <- df_row[, colnames(meta_files)]
  rbind(meta_files, df_row)
}

get_file_data <- function(syn_id) {
  # Be explicit in col_types to stop the messages about parsing
  # Also stop it from making NA into strings
  tryCatch(
    {
      readr::read_csv(
        synapser::synGet(syn_id)$path,
        na = character(),
        col_types = readr::cols()
      )
    },
    error = function(e) {
      return(as.character(NA))
    }
  )
}

# End functions ----------------------------------------------------------------

# Get study metadata file IDs -----
files_human <- gather_metadata_synIDs(id_human)
files_model <- gather_metadata_synIDs(id_model)
all_files <- rbind(files_human, files_model)

# Remove files that are probably not metadata -----
# Only keep csv files that don't have 'dictionary' in name
all_files <- all_files[grepl(".csv$", all_files$name, ignore.case = TRUE), ]
all_files <- all_files[!grepl("dictionary", all_files$name, ignore.case = TRUE), ]
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
all_meta_ids <- gather_ids(all_files)

