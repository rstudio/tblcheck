#' Declare a problem
#' 
#' Useful for constructing a small list to communicate the problem that was
#' discovered during checking.
#' 
#' @param type A character string, e.g. `column_values` or `table_rows`, that
#'   describes the problem that was discovered.
#' @param expected,actual The expected and actual values. These should be
#'   included when the value is a summary, e.g. `nrow(expected)` or 
#'   `length(actual)`. Be careful not to include large amounts of data.
#' @param ... Additional elements to be included in the `problem` object.
#'   
#' @keywords internal
#' @noRd
problem <- function(
  type, expected = NULL, actual = NULL, object_label = "result", ...
) {
  checkmate::assert_string(type, min.chars = 1)
  
  problem <- list(
    type = type,
    expected = expected,
    actual = actual,
    object_label = object_label,
    ...
  )
  
  structure(
    purrr::compact(problem),
    class = c("tblcheck_problem", "gradethis_problem", "list")
  )
}
