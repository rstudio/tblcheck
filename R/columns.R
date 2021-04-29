
#' Checks if a table has extra columns.
#'
#' @param x data.frame or tibble
#' @param reference data.frame or tibble
#' @param col_names character
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_extra_columns(mtcars, mtcars)
#' has_extra_columns(mtcars, col_names = "disp")
has_extra_columns <- function(
  x,
  reference = NULL,
  col_names = NULL
) {
  if (is.null(reference) && is.null(col_names)) {
    stop("Grading error: `has_extra_columns()` requires a value for either the `reference` or the `col_names` argument.")
  }
  if (!is.null(reference)) {
    col_names <- col_names %||% names(reference)
  }
  return(length(setdiff(names(x), col_names)) > 0)
}

#' Checks if a table has missing columns.
#'
#' @param x data.frame or tibble
#' @param reference data.frame or tibble
#' @param col_names character
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_missing_columns(mtcars[, 1:2], mtcars)
#' has_missing_columns(mtcars[, 1:2], col_names = names(mtcars))
has_missing_columns <- function(
  x,
  reference = NULL,
  col_names = NULL
) {
  if (is.null(reference) && is.null(col_names)) {
    stop("Grading error: `has_missing_columns()` requires a value for either the `reference` or the `col_names` argument.")
  }
  if (!is.null(reference)) {
    col_names <- col_names %||% names(reference)
  }
  return(length(setdiff(col_names, names(x))) > 0)
}

#' Checks if a table has the right column types.
#'
#' @param x data.frame or tibble
#' @param reference data.frame or tibble
#' @param col_types character
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_column_types(mtcars, reference = mtcars)
has_column_types <- function(
  x,
  reference = NULL,
  col_types = NULL
) {
  if (is.null(reference) && is.null(col_types)) {
    stop("Grading error: `has_column_types()` requires a value for either the `reference` or the `col_types` argument.")
  }
  if (!is.null(reference)) {
    col_types <- vapply(reference, class, character(1))
  }
  x_types <- vapply(x, class, character(1))
  not_same <- col_types[x_types != col_types]
  names(not_same) <- names(x_types)[x_types != col_types]
  
  return(length(not_same) == 0)
}


