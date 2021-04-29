
#' Checks if a table has the expected number of rows.
#'
#' @param x tibble or data.frame
#' @param reference tibble or data.frame
#' @param n_rows numeric
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_n_rows(mtcars, mtcars)
#' has_n_rows(mtcars, n_rows = 32)
has_n_rows <- function(
  x, 
  reference = NULL,
  n_rows = NULL
) {
  if (is.null(reference) && is.null(n_rows)) {
    stop("Grading error: `has_n_rows()` requires a value for either the `reference` or the `n_rows` argument.")
  }
  if (!is.null(reference)) {
    n_rows <- n_rows %||% nrow(reference)
  }
  return(nrow(x) == n_rows)
}

#' Checks if a table has the expected number of columns.
#'
#' @param x tibble or data.frame
#' @param reference tibble or data.frame
#' @param n_cols numeric
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_n_cols(mtcars, mtcars)
#' has_n_cols(mtcars, n_cols = 11)
has_n_cols <- function(
  x, 
  reference = NULL,
  n_cols = NULL
) {
  if (is.null(reference) && is.null(n_cols)) {
    stop("Grading error: `has_n_cols()` requires a value for either the `reference` or the `n_cols` argument.")
  }
  if (!is.null(reference)) {
    n_cols <- n_cols %||% ncol(reference)
  }
  return(ncol(x) == n_cols)
}

