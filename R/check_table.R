#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the data frames differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `table_class`: The table does not have the expected classes
#' 2. `table_nrow`: The table doesn't have the expected number of rows
#' 3. `table_ncol`: The table doesn't have the expected number of columns
#' 4. `table_names`: The table has names that are not expected,
#'   or is missing names that are expected.
#' 
#' Additional problems may be produced by [check_column()]
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   display in an informative failure message.
#'   Passed to [check_names()] to determine the number of mismatched column
#'   names to display and the `n_values` argument of [check_column()] to
#'   determine the number of mismatched column values to display.
#'   Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and 
#'   `expected` have the same classes with [class()].
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
#' @param check_column_class `[logical(1)]`\cr Whether to check that each
#'   columns has the same class in `object` and `expected`.
#' @param check_column_values `[logical(1)]`\cr Whether to check that each
#'   column has the same values in `object` and `expected`.
#'
#' @return If there are any issues, generate a [gradethis::fail()] message.
#'   Otherwise, invisibly return [`NULL`].
#' @export

check_table <- function(
  object              = .result,
  expected            = .solution,
  max_diffs           = 3,
  check_class         = TRUE,
  check_nrow          = TRUE,
  check_names         = TRUE,
  check_ncol          = !check_names,
  check_columns       = TRUE,
  check_column_class  = check_columns,
  check_column_values = check_columns
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_class,         any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_nrow,          any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_names,         any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_ncol,          any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_columns,       any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_column_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_column_values, any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  # check table class ----
  if (check_class) {
    return_if_problem(
      tbl_check_class(
        object, expected,
        object_label = "table",
        problem_prefix = "table_"
      )
    )
  }
  
  # check number of rows ----
  if (check_nrow) {
    return_if_problem(
      tbl_check_length(
        object, expected,
        dimension = "nrow",
        object_label = "table",
        problem_prefix = "table_"
      )
    )
  }
  
  # check column names ----
  if (check_names) {
    return_if_problem(
      tbl_check_names(
        object, expected,
        object_label = "table",
        problem_prefix = "table_"
      )
    )
  }
  
  # check number of columns ----
  if (check_ncol) {
    return_if_problem(
      tbl_check_length(
        object, expected,
        dimension = "ncol",
        object_label = "table",
        problem_prefix = "table_"
      )
    )
  }
  
  # check column contents ----
  if (check_columns) {
    return_if_graded(
      purrr::walk(
        names(object),
        check_column,
        object       = object,
        expected     = expected,
        check_class  = check_column_class,
        check_values = check_column_values,
        check_length = FALSE,
        max_diffs    = max_diffs
      )
    )
  }
  
  invisible()
}
