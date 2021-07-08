#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the data frames differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param max The maximum number of mismatched values to display in an
#'   informative failure message.
#'   Passed to [check_names()] to determine the number of mismatched column
#'   names to display and the `n_values` argument of [check_column()] to
#'   determine the number of mismatched column values to display.
#'   Defaults to 3.
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
#' @inheritParams check_column
#'
#' @return Returns `object` invisibly.
#' @export

check_table <- function(
  object        = .result,
  expected      = .solution,
  max_print     = 3,
  check_nrow    = TRUE,
  check_names   = TRUE,
  check_ncol    = !check_names,
  check_columns = TRUE,
  check_class   = check_columns,
  check_values  = check_columns
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  # check number of rows ----
  if (check_nrow && !identical(nrow(object), nrow(expected))) {
    gradethis::fail(
      "Your table should have {plu::ral('n row', n = nrow(expected))}."
    )
  }
  
  # check column names ----
  if (check_names) check_names(object, expected, max = max)
  
  # check number of columns ----
  if (check_ncol && !identical(ncol(object), ncol(expected))) {
    gradethis::fail(
      "Your table should have {plu::ral('n column', n = ncol(expected))}."
    )
  }
  
  # check column contents ----
  if (check_columns) {
    lapply(
      object, check_column,
      check_class = check_class, check_values = check_values, n_values = max
    )
  }
  
  return(invisible(object))
}
