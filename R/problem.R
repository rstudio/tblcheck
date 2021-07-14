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
#'   
#' @keywords internal
#' @noRd
problem <- function(type, expected = NULL, actual = NULL, ...) {
  stopifnot(
    "`type` must be string" = is.character(type),
    "`type` must be length 1" = length(type) == 1,
    "`type` must have a character value" = nzchar(type)
  )
  
  list(
    type = type,
    expected = expected,
    actual = actual,
    ...
  )
}