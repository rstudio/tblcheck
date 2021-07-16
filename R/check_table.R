#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the data frames differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `table_nrow`: The table doesn't have the expected number of rows
#' 2. `table_ncol`: The table doesn't have the expected number of columns
#' 
#' Additional problems may be produced by [check_names()] and [check_column()]
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param max_diffs The maximum number of mismatched values to display in an
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
#' @return If there are any issues, generate a [gradethis::fail()] message.
#'   Otherwise, invisibly return [`NULL`].
#' @export

check_table <- function(
  object        = .result,
  expected      = .solution,
  max_diffs     = 3,
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
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    
    assert_map(
      checkmate::assert_logical,
      check_nrow, check_names, check_ncol, check_columns,
      check_class, check_values,
      any.missing = FALSE, len = 1
    )
    
    assert_map(checkmate::assert_data_frame, object, expected)
  })
  
  # check number of rows ----
  if (check_nrow && !identical(nrow(object), nrow(expected))) {
    exp_rows <- plu::ral("n row", n = nrow(expected))
    obj_rows <- plu::ral("n row", n = nrow(object))
    gradethis::fail(
      "Your table should have {exp_rows}, but it has {obj_rows}.",
      problem = problem("table_nrow", nrow(expected), nrow(object))
    )
  }
  
  # check column names ----
  if (check_names) check_names(object, expected, max = max_diffs)
  
  # check number of columns ----
  if (check_ncol && !identical(ncol(object), ncol(expected))) {
    exp_cols <- plu::ral('n column', n = ncol(expected))
    obj_cols <- plu::ral('n column', n = ncol(object))
    gradethis::fail(
      "Your table should have {exp_cols}, but it has {obj_cols}.",
      problem = problem("table_ncol", ncol(expected), ncol(object))
    )
  }
  
  # check column contents ----
  if (check_columns) {
    lapply(
      names(object),
      check_column,
      object       = object,
      expected     = expected,
      check_class  = check_class,
      check_values = check_values,
      check_length = FALSE,
      max_diffs    = max_diffs
    )
  }
  
  invisible()
}

