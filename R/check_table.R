#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the tables differ
#' - `tbl_check_names()` returns a list describing the problem
#' - `tbl_grade_names()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `table_class`: The table does not have the expected classes
#' 2. `table_dimensions`: The table doesn't have the expected number of rows
#'   and columns
#' 4. `table_names`: The table has names that are not expected,
#'   or is missing names that are expected.
#' 
#' Additional problems may be produced by [tbl_check_column()]
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   display in an informative failure message.
#'   Passed to [tbl_check_names()] to determine the number of mismatched column
#'   names to display and the `n_values` argument of [tbl_check_column()] to
#'   determine the number of mismatched column values to display.
#'   Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and 
#'   `expected` have the same classes with [class()].
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same column names with [tbl_check_names()].
#' @param check_dimensions `[logical(1)]`\cr Whether to check that `object` and 
#'   `expected` have the same number of rows and columns with [dim()].
#' @param check_columns `[logical(1)]`\cr Whether to check that all columns
#'   have the same contents with [tbl_check_column()].
#' @param check_column_class `[logical(1)]`\cr Whether to check that each
#'   columns has the same class in `object` and `expected`.
#' @param check_column_values `[logical(1)]`\cr Whether to check that each
#'   column has the same values in `object` and `expected`.
#' @param envir The environment in which to find `.result` and `.solution`.
#'
#' @return If there are any issues, a [list] from `tbl_check_table()` or a
#'   [gradethis::fail()] message from `tbl_grade_table()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples 
#' .result <- data.frame(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:10, b = 11:20)
#' tbl_check_table()
#' tbl_grade_table()
#' 
#' .result <- tibble::tibble(a = 1:10, b = a, c = a, d = a, e = a, f = a)
#' .solution <- tibble::tibble(z = 1:10, y = z, x = z, w = z, v = z, u = z)
#' tbl_check_table()
#' tbl_grade_table()
#' tbl_grade_table(max_diffs = 5)
#' tbl_grade_table(max_diffs = Inf)
#' 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:11, b = 12:22)
#' tbl_check_table()
#' tbl_grade_table()
#'
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = letters[1:10], b = letters[11:20])
#' tbl_check_table()
#' tbl_grade_table()
#' 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 11:20, b = 1:10)
#' tbl_check_table()
#' tbl_grade_table()
#' tbl_grade_table(max_diffs = 5)
#' tbl_grade_table(max_diffs = Inf)
tbl_check_table <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_names = TRUE,
  check_dimensions = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_values = check_columns,
  envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_class,         any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_names,         any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_dimensions,    any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_columns,       any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_column_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_column_values, any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  # check table class ----
  if (check_class) {
    return_if_problem(
      tbl_check_class(object, expected),
      prefix = "table"
    )
  }
  
  # check column names ----
  if (check_names) {
    return_if_problem(
      tbl_check_names(object, expected),
      prefix = "table"
    )
  }
  
  # check dimensions ----
  if (check_dimensions) {
    return_if_problem(
      tbl_check_dimensions(object, expected),
      prefix = "table"
    )
  }
  
  # check column contents ----
  if (check_columns) {
    for (column in names(expected)) {
      return_if_problem(
        tbl_check_column(
          name = column,
          object = object,
          expected = expected,
          check_class = check_column_class,
          check_values = check_column_values,
          check_length = FALSE,
          max_diffs = max_diffs
        )
      )
    }
  }
}

#' @rdname tbl_check_table
#' @export
tbl_grade_table <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_names = TRUE,
  check_dimensions = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_values = check_columns,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_table(
        object = object,
        expected = expected,
        max_diffs = max_diffs,
        check_class = check_class,
        check_names = check_names,
        check_dimensions = check_dimensions,
        check_columns = check_columns,
        check_column_class = check_column_class,
        check_column_values = check_column_values,
        envir = envir
      ),
      max_diffs = max_diffs
    )
  )
}
