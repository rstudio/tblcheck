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
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   display in an informative failure message.
#'   Passed to [check_names()] to determine the number of mismatched column
#'   names to display and the `n_values` argument of [check_column()] to
#'   determine the number of mismatched column values to display.
#'   Defaults to 3.
#' @param check_nrow `[logical(1)]`\cr Whether to check that `object` and 
#'   `expected` have the same number of rows with [nrow()].
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same column names with [check_names()].
#' @param check_ncol `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same number of columns with [ncol()].
#'   By default, `check_ncol` is [`FALSE`] if `check_names` is [`TRUE`], as
#'   running both is redundant.
#' @param check_columns `[logical(1)]`\cr Whether to check that all columns
#'   have the same contents with [check_column()].
#' @param final The final grade returned if all of the table checks pass.
#'   Defaults to [gradethis::pass()], set to `NULL` to only return a failing
#'   grade.
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
  check_values  = check_columns,
  final = pass()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_nrow,    any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_names,   any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_ncol,    any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_columns, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_class,   any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values,  any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  # check number of rows ----
  if (check_nrow) {
    nrow_object   <- nrow(object)
    nrow_expected <- nrow(expected)
    
    if (!identical(nrow_object, nrow_expected)) {
      exp_rows <- plu::ral("n row", n = nrow_expected)
      obj_rows <- plu::ral("n row", n = nrow_object)
      gradethis::fail(
        "Your table should have {exp_rows}, but it has {obj_rows}.",
        problem = problem("table_nrow", nrow_expected, nrow_object)
      )
    }
  }
  
  # check column names ----
  if (check_names) {
    check_names(object, expected, max_diffs = max_diffs)
  }
  
  # check number of columns ----
  if (check_ncol) {
    ncol_object <- ncol(object)
    ncol_expected <- ncol(expected)
  
    if(!identical(ncol_object, ncol_expected)) {
      exp_cols <- plu::ral('n column', n = ncol_expected)
      obj_cols <- plu::ral('n column', n = ncol_object)
      gradethis::fail(
        "Your table should have {exp_cols}, but it has {obj_cols}.",
        problem = problem("table_ncol", ncol_expected, ncol_object)
      )
    }
  }
  
  # check column contents ----
  if (check_columns) {
    purrr::walk(
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
  
  force(final)
  
  invisible()
}
