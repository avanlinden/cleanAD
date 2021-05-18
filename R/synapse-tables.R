#' @title Update sample ID table
#'
#' @description **Warning** This is a destructive function!
#' Update the sample ID table with new data. This function will delete the rows
#' in the old data and upload the new data in its place. The existing table
#' schema must match the columns in `new_data`.
#'
#' @param table_id synID of the table to overwrite.
#' @param new_data the new data to overwrite the table with.
#' @export
update_samples_table <- function(table_id, new_data) {
  if (!inherits(new_data, "data.frame")) {
    stop("No new_data was provided.")
  }
  # Write new data to a file and create Table object
  temp_path <- tempfile(fileext = ".csv")
  write.csv(new_data, temp_path, row.names = FALSE)
  new_table <- tryCatch(
    {
      synapser::Table(table_id, new_data)
    },
    error = function(e) {
      stop(
        glue::glue("There's a problem with making the table:\n  {e$message}")
      )
    }
  )

  # Grab table rows; delete them
  samples <- synapser::synTableQuery(
    glue::glue("SELECT * FROM {table_id}")
  )
  synapser::synDelete(samples)

  # Store new table
  synapser::synStore(new_table)
  # Query table to force update
  synapser::synTableQuery(glue::glue("SELECT * FROM {table_id} LIMIT 1"))
}

#' @title Get Simple Table Query
#'
#' @description Query a Synapse table for specific columns and only receive
#' those columns back in dataframe.
#'
#' @param table_id Synapse synID of table to query.
#' @param columns Vector of desired columns from the table. Default is
#' `c("study", "individualID", "specimenID", "assay")`.
#' @return Dataframe from query with only the requested columns.
#' @export
simple_table_query <- function(table_id,
                               columns = c("study", "individualID", "specimenID", "assay")) { # nolint
  column_string <- glue::glue_collapse(columns, ", ")
  all_annots <- synapser::synTableQuery(
    glue::glue("SELECT {column_string} FROM {file_view}")
  )$asDataFrame()
  # Keeps row info; remove this
  all_annots[, columns]
}
