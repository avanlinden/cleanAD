#' @title Gather Metadata synIDs
#'
#' @description Gather synIDs for metadata files. Assumes all children of
#' `dir_id` are named study folders.
#'
#' @param dir_id Synapse synID for top level directory.
#' @return Dataframe with file synID, filename, and study name.
#' @export
gather_metadata_synIDs_all <- function(dir_id) {
  children <- wrapper_synGetChildren(dir_id)
  study_metadata <- purrr::map(children, function(study_folder) {
    gather_study_metadata_synIDs(
      dir_id = study_folder$id,
      study = study_folder$name
    )
  })
  study_metadata <- Reduce(rbind, study_metadata)
  study_metadata <- na.omit(study_metadata)
  return(study_metadata)
}

#' @title Gather Metadata synIDs By Study
#'
#' @description Gather synIDs for metadata files. Assumes metadata files will
#' be in a folder called `Metadata` or `metadata`. This folder will be either a
#' child of `dir_id` or in a folder called `Data` or `data` that is within
#' `dir_id`.
#'
#' @param dir_id Synapse synID for study directory.
#' @param study Name of study.
#' @export
gather_study_metadata_synIDs <- function(dir_id, study) {
  children <- wrapper_synGetChildren(dir_id)
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
    metadata <- gather_study_metadata_synIDs(
      dir_id = purrr::flatten(children[has_data])$id,
      study = study
    )
    return(metadata)
  } else {
    return(NA)
  }
}

#' @title Gather Metadata File synIDs
#'
#' @description Gather the file synIDs
#'
#' @param dir_id Synapse synID for metadata directory.
#' @param study Name of study.
#' @noRd
gather_metadata_synIDs_files <- function(dir_id, study) {
  children <- wrapper_synGetChildren(dir_id)
  if (!all(is.na(children))) {
    df <- dplyr::bind_rows(children)
    df <- df[, c("name", "id")]
    df["study"] <- study
    # Remove weird rownames
    rownames(df) <- c()
    return(df)
  }
  return(NA)
}

#' @title Check Child Existence
#'
#' @description Check if a specifically named child folder exists within a
#' list of child folders.
#'
#' @param children List of children.
#' @param dir_name Vector of names to search for.
#' @return Boolean vector indicating whether the child matches one
#' of the dir_name(s) in same order as children.
#' @noRd
#' @examples
#' dat <- list(
#'   list(
#'     name = "wrong",
#'     stuff = "foo"
#'   ),
#'   list(
#'     name = "right",
#'     other = "bar"
#'   )
#' )
#' does_child_exist(dat, "right")
does_child_exist <- function(children, dir_name) {
  purrr::map_lgl(children, function(child) {
    child$name %in% dir_name
  })
}

#' @title Pull out names of children
#'
#' @description Get the names of the child folders.
#'
#' @export
#' @param dir_id Synapse folder synID
child_names <- function(dir_id) {
  children <- wrapper_synGetChildren(dir_id)
  unlist(purrr::map(children, function(x) {
    x$name
  }))
}

#' @title synapser::synGetChildren Wrapper
#'
#' @description Wrapper around synapser::synGetChildren for mocking
#' ease.
#'
#' @noRd
#' @param dir_id Synapse folder synID
#' @return List of children in dir_id
wrapper_synGetChildren <- function(dir_id) {
  synapser::synGetChildren(dir_id)$asList()
}
