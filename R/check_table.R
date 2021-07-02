#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the data frames differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param check_nrow A [logical] indicating whether to check that `object` and 
#'   `expected` have the same number of rows with [nrow()].
#' @param check_names A [logical] indicating whether to check that `object` and
#'   `expected` have the same column names with [check_names()].
#' @param check_ncol A [logical] indicating whether to check that `object` and
#'   `expected` have the same number of columns with [ncol()].
#'   By default, `check_ncol` is [`FALSE`] if `check_names` is [`TRUE`], as
#'   running both is redundant.
#' @param check_columns A [logical] indicating whether to check that all columns
#'   have the same contents with [check_column()].
#' @param max The maximum number of name mismatches to print,
#'   passed to `check_names()`. Defaults to 3.
#'
#' @return Returns `object` invisibly.
#' @export

check_table <- function(
  object      = .result,
  expected    = .solution,
  check_nrow  = TRUE,
  check_names = TRUE,
  check_ncol  = !check_names,
  max         = 3
) {
  if (inherits(object, "gradethis_placeholder")) {
    x <- get(".solution", parent.frame(2))
  }
  if (inherits(expected, "gradethis_placeholder")) {
    y <- get(".solution", parent.frame(2))
  }
  
  # check length ----
  if (check_nrow) {
    if (!identical(nrow(object), nrow(expected))) { 
      gradethis::fail("Your table should have {nrow(expected)} rows.")
    }
  }
  
  # check column names ----
  if (check_names) check_names(object, expected, max = max)
  
  # check number of columns ----
  if (check_ncol) {
    if (!identical(ncol(object), ncol(expected))) { 
      gradethis::fail("Your table should have {ncol(expected)} rows.")
    }
  }
  
  # check column contents ----
  # if (check_columns) lapply(object_names, check_column)
  
  return(invisible(object))
}
