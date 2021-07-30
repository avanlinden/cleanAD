#' @title Remove JSON From Multi-annotations
#'
#' @description Remove the JSON-specific elements from a string. For example,
#' a JSON string could look like `"[\"foo\", \"bar\"]"`. This function would
#' return the string `"foo, bar"`.
#'
#' @param json_string Character string with JSON elements.
#' @param remove_spaces `TRUE` to also remove spaces, else `FALSE`. Note that
#' this will remove *all* spaces. Default is `FALSE`.
#' @return Character string without brackets, escaped quotes, and (optionally)
#' spaces.
#' @export
clean_json_string <- function(json_string, remove_spaces = FALSE) {
  if (remove_spaces) {
    return(gsub("\\[|\"|\\]| ", "", json_string))
  } else {
    return(gsub("\\[|\"|\\]|", "", json_string))
  }
}
