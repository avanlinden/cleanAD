##################################
# Generate specimen table for AD #
##################################

# Import package -----
suppressPackageStartupMessages(library("log4r"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("synapser"))

## CLI Args --------------------------------------------------------------------

option_list <- list(
  optparse::make_option(
    "--authToken",
    type = "character",
    default = NA,
    help = "Synapse personal access token to log in with [default = %default].
            If no token given, assumes a local .synapseConfig file exists with
            credentials."
  ),
  optparse::make_option(
    "--directories",
    type = "character",
    help = "Synapse synIDs for top level directories to search for metadata as
            comma-separated list (e.g. --directories syn123,syn789). Folders
            within these directories should be organized as follows. The first
            level within the directories should be study folders, named with the
            study name. There are two locations allowed for metadata folders to
            exist within the study folder: either the first level of the study
            folder or the second level of the study folder, within a folder
            called Data. In either case, the folder should be called Metadata."
  ),
  optparse::make_option(
    "--consortia_dir",
    type = "character",
    default = NA,
    help = "Synapse synID for top level directory associated with
            consortium studies [default: NA]. The first level of
            folders within this directory should be named after their
            study. These study names will be removed from the specimen
            table."
  ),
  optparse::make_option(
    "--id_table",
    type = "character",
    help = "Synapse synID for the specimen table. Table should exist with the
            columns: study, individualID, specimenID, assay."
  ),
  optparse::make_option(
    "--file_view",
    type = "character",
    help = "Synapse synID for file view with all annotated data
            [default = %default]. This is used to gather specimenID and
            individualID for studies that do not have metadata or are missing
            this information in their metadata. It is also used to gather the
            annotations on the metadata files."
  ),
  optparse::make_option(
    "--log_dir",
    type = "character",
    default = "./log",
    help = "Directory to output log files to [default = %default]."
  ),
  optparse::make_option(
    "--task_id",
    type = "character",
    default = NA,
    help = "Synapse synID to update annotations on for tracking success of the
            task [default = %default]."
  ),
  optparse::make_option(
    "--task_view",
    type = "character",
    default = NA,
    help = "Synapse synID of file view that includes task_id
            [default = %default]."
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opts <- optparse::parse_args(opt_parser)

## Functions -------------------------------------------------------------------

#' Update task annotation
#'
#' Update the 'completed_successfully' task annotation on a Synapse folder.
#' Expected that the annotation is a text type.
#'
#' @param annots Synapse annotation object for the task folder
#' @param success TRUE if the task was completed successfully, else FALSE
#' @param task_view synID for the task file view. If provided, will update the
#' view after annotating the task folder.
update_task_annotation <- function(annots, success, task_view = NA) {
  annots['completed_successfully'][[1]] <- success
  synapser::synSetAnnotations(annots)
  ## Force file view update, if given task_view
  if (!is.na(task_view)) {
    synTableQuery(glue::glue("SELECT * FROM {task_view}"))
  }
}

## Setup -----------------------------------------------------------------------

## Constants
FILE_VIEW_COLUMNS_BIND <- c("study", "individualID", "specimenID", "assay")
FILE_VIEW_COLUMNS_JOIN <- c("id", "dataType", "metadataType", "assay")
RELEVANT_METADATA_COLUMNS <- c("individualID", "specimenID", "assay")
METADATA_TYPES <- c("biospecimen", "assay", "individual")

# # For local testing -- comment out
# opts <- list(
#   directories = c("syn5550383", "syn5550382"),
#   consortia_dir = "syn5550378",
#   id_table = "syn21578908",
#   file_view = "syn11346063",
#   log_dir = NA,
#   task_id = NA,
#   task_view = NA,
#   authToken = NA
# )

## Create logger
## Make sure a directory exists; create if doesn't
if (!is.na(opts$log_dir)) {
  if (!dir.exists(opts$log_dir)) {
    dir.create(opts$log_dir)
  }
}
logfile_name <- glue::glue("{year(today())}-{month(today())}")
logpath <- glue::glue("{opts$log_dir}/{logfile_name}.log")
logger <- create.logger(logfile = logpath, level = "INFO")

## Use synapser and log in
tryCatch(
  {
    if(is.na(opts$authToken)) {
      synLogin()
    } else {
      synLogin(authToken = opts$authToken)
    }
  },
  error = function(e) {
    failure_message <- glue::glue(
      "Log in error:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Grab annotations on task, if provided with task_id
## If not provided with task_id, don't update
update_task <- FALSE
annots <- NA
if (!is.na(opts$task_id)) {
  update_task <- TRUE
  annots <- tryCatch(
    {
      synapser::synGetAnnotations(opts$task_id)
    },
    error = function(e) {
      if (update_task) {
        update_task_annotation(
          annots = annots,
          success = "false",
          task_view = opts$task_view
        )
      }
      failure_message <- glue::glue(
        "Could not gather task annotations:\n  {e$message}"
      )
      error(logger, failure_message)
      quit(status = 1)
    }
  )
}

## Setup done ------------------------------------------------------------------

## Get study metadata file IDs -----
all_files <- tryCatch(
  {
    purrr::map_dfr(opts$directories, ~ gather_metadata_synIDs_all(.))
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        annots = annots,
        success = "false",
        task_view = opts$task_view
      )
    }
    failure_message <- glue::glue(
      "There was a problem getting synIDs for metadata files:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Remove files that are probably not metadata -----

## Only keep csv files that don't have 'dictionary' in name
all_files <- all_files[grepl(".csv$", all_files$name, ignore.case = TRUE), ]
all_files <- all_files[!grepl("dictionary", all_files$name, ignore.case = TRUE), ] # nolint
## Fix id column to be character
all_files$id <- as.character(all_files$id)

## Grab file view for annotations -----

view_query <- tryCatch(
  {
    synapser::synTableQuery(
      glue::glue("SELECT * FROM {opts$file_view}")
    )$asDataFrame()
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        annots = annots,
        success = "false",
        task_view = opts$task_view
      )
    }
    failure_message <- glue::glue(
      "There was a problem getting the file view:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Check that view has needed columns

missing_cols <- setdiff(
  unique(c(FILE_VIEW_COLUMNS_BIND, FILE_VIEW_COLUMNS_JOIN)),
  colnames(view_query)
)
if (length(missing_cols) > 0) {
  if (update_task) {
    update_task_annotation(
      annots = annots,
      success = "false",
      task_view = opts$task_view
    )
  }
  missing <- glue::glue_collapse(missing_cols, sep = ", ")
  failure_message <- glue::glue(
    "The file view is missing these columns:\n  {missing}"
  )
  error(logger, failure_message)
  quit(status = 1)
}

## Grab metadata-related annotations from file view and join -----

view_meta <- view_query[view_query$id %in% all_files$id, ]
all_files <- dplyr::left_join(
  all_files,
  view_meta[, FILE_VIEW_COLUMNS_JOIN]
)
## Remove any with metadataType dictionary or protocol
all_files <- all_files[!grepl("dictionary|protocol", all_files$metadataType), ]

## Join metadata -----

## Open files and gather IDs
all_meta_ids <- tryCatch({
    gather_ids_all_studies(all_files) -> all_meta_id
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        annots = annots,
        success = "false",
        task_view = opts$task_view
      )
    }
    failure_message <- glue::glue(
      "There was a problem gathering metadata from the files:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Grab file annotations to add missing individuals/specimens -----
all_annots <- view_query[, FILE_VIEW_COLUMNS_BIND]

# Remove json from annotated study names
all_annots[, "study"] <- unlist(purrr::map(
  all_annots$study,
  ~ clean_json_string(., remove_spaces = TRUE)
))
# Separate into multiple rows for IDs that have multiple study annotations
all_annots <- tidyr::separate_rows(all_annots, study, sep = ",")
# Remove any with consortia study name
if (!is.na(opts$consortia_dir)) {
  all_annots <- tryCatch(
    {
      consortia_studies <- child_names(opts$consortia_dir)
      all_annots[!all_annots$study %in% consortia_studies, ]
    },
    error = function(e) {
      if (update_task) {
        update_task_annotation(
          annots = annots,
          success = "false",
          task_view = opts$task_view
        )
      }
      failure_message <- glue::glue(
        "There was a problem gathering consortia study names:\n  {e$message}"
      )
      error(logger, failure_message)
      quit(status = 1)
    }
  )
}

## Remove rows where both specimenID and individualID are NA
all_annots <- all_annots[
  !(is.na(all_annots$specimenID) & is.na(all_annots$individualID)),
]

## Bind together with metadata to get full set -----
all_ids <- add_missing_specimens(meta_df = all_meta_ids, annot_df = all_annots)

## Grab samples table, delete old data, add new data -----
#! COMMENTED OUT BECAUSE THIS IS DESTRUCTIVE -- MAKE SURE THE SCRIPT
#! IS CORRECT BEFORE DEPLOYING
tryCatch(
  {
    update_samples_table(table_id = opts$id_table, new_data = all_ids)
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        annots = annots,
        success = "false",
        task_view = opts$task_view
      )
    }
    failure_message <- glue::glue(
      "There was a problem updating the specimen table:\n  {e$message}"
    )
    error(logger, failure_message)
    quit(status = 1)
  }
)

## Should have finished successfully -----

if (update_task) {
  update_task_annotation(
    annots = annots,
    success = "true",
    task_view = opts$task_view
  )
}

quit(status = 0)
