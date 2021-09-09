#' Checks that a column is identical across two tables
#'
#' Checks if `column` has the same class and values in `object` and `expected`.
#' If the columns differ
#' - `tbl_check_column()` returns a list describing the problem
#' - `tbl_grade_column()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `table_names`: `object` doesn't contain a column named column
#' 1. `column_class`: Any mismatch in the classes of the `column`
#' 1. `column_length`: The `column` doesn't have the expected length
#' 1. `column_n_levels`, `column_levels`, `column_reverse_levels`, 
#'   `column_level_order_diffs`, `column_level_order`: See [vec_check_levels()]
#' 1. `column_value_diffs`: The first `max_diffs` elements of the `column`
#'   don't have the expected values
#' 1. `column_values`: The `column` doesn't have the expected values
#'
#' @param column `[character(1)]`\cr The name of the column to check.
#' @inheritParams tbl_check_table
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `column` has the
#'   same class in `object` and `expected`.
#' @param check_length `[logical(1)]`\cr Whether to check that `column` has the
#'   same length in `object` and `expected`.
#' @param check_values `[logical(1)]`\cr Whether to check that `column` has the
#'   same values in `object` and `expected`.
#'
#' @return If there are any issues, a [list] from `tbl_check_column()` or a
#'   [gradethis::fail()] message from `tbl_grade_column()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = letters[1:10], b = letters[11:20])
#' tbl_check_column("a")
#' tbl_grade_column("a")
#' 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:11, b = 12:22)
#' tbl_check_column("a")
#' tbl_grade_column("a")
#' 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 11:20, b = 1:10)
#' tbl_check_column("a")
#' tbl_grade_column("a")
#' tbl_grade_column("a", max_diffs = 5)
#' tbl_grade_column("a", max_diffs = Inf)
tbl_check_column <- function(
  column,
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  return_if_problem(
    assert_internally({
      checkmate::assert_character(column, len = 1, any.missing = FALSE)
      checkmate::assert_number(max_diffs, lower = 1)
      checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
      checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
      checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
      checkmate::assert_data_frame(object)
      checkmate::assert_data_frame(expected)
    })
  )
  
  if (!column %in% names(expected)) {
    warning("`", column, "` is not a column in `expected`.")
    return()
  }
  
  names_problem <- tbl_check_names(object, expected)
  if (column %in% names_problem$missing) {
    names_problem$missing <- column
    names_problem$unexpected <- NULL
    return_if_problem(names_problem, prefix = "table")
  }
  
  return_if_problem(
    vec_check_vector(
      object[[column]],
      expected[[column]],
      max_diffs = max_diffs,
      check_class = check_class,
      check_length = check_length,
      check_values = check_values,
      check_names = FALSE
    ),
    prefix = "column",
    column = column
  )
}

#' @rdname tbl_check_column
#' @export
tbl_grade_column <- function(
  column,
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  env = parent.frame()
) {
  tbl_grade(
    tbl_check_column(
      column = column,
      object = object,
      expected = expected,
      max_diffs = max_diffs,
      check_class = check_class,
      check_length = check_length,
      check_values = check_values,
      env = env
    ), 
    env = env
  )
}
