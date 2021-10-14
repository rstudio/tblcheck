#' Checks that a column is identical across two tables
#'
#' @description
#' Checks for differences between the `name` column in `object` and in
#' `expected` in the following order:
#' 1. Check that the `name` column exists in `object`
#' 1. Check class with [vec_check_class()]
#' 1. Check length with [vec_check_dimensions()]
#' 1. If the column is a factor, check factor levels with [vec_check_levels()]
#' 1. Check column values with [vec_check_values()]
#' 
#' If the columns differ
#' - `tbl_check_column()` returns a list describing the problem
#' - `tbl_grade_column()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `names` (`table_problem`): `object` doesn't contain a column named column.
#' 1. `class`: Any mismatch in the classes of the `column`.
#' 1. `length`: The `column` doesn't have the expected length.
#' 1. `levels_n`, `levels`, `levels_reversed`, `levels_order`:
#'   See [vec_check_levels()].
#' 1. `values`: The `column` doesn't have the expected values.
#' 1. `names` (`column_problem`): The `column` has different [names][names()]
#'   than expected.
#' 1. `names_order`: The `column` has the same [names][names()] as expected,
#'   but in a different order.
#'
#' @param column `[character(1)]`\cr The name of the column to check.
#' @inheritParams tbl_check
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `column` has the
#'   same class in `object` and `expected`.
#' @param check_length `[logical(1)]`\cr Whether to check that `column` has the
#'   same length in `object` and `expected`.
#' @param check_values `[logical(1)]`\cr Whether to check that `column` has the
#'   same values in `object` and `expected`.
#' @param check_values `[logical(1)]`\cr Whether to check that `column` has the
#'   same values in `object` and `expected`.
#' @param check_names `[logical(1)]`\cr Whether to check that `column` has the
#'   same [names][names()] in `object` and `expected`.
#'   Defaults to `FALSE`.
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
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = FALSE,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  return_if_internal_problem({
    checkmate::assert_character(column, len = 1, any.missing = FALSE)
    checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
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
    vec_check(
      object[[column]],
      expected[[column]],
      check_class = check_class,
      check_length = check_length,
      check_values = check_values,
      check_names = check_names
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
  check_names = FALSE,
  env = parent.frame()
) {
  tblcheck_grade(
    tbl_check_column(
      column = column,
      object = object,
      expected = expected,
      check_class = check_class,
      check_length = check_length,
      check_values = check_values,
      check_names = check_names,
      env = env
    ),
    max_diffs = max_diffs,
    env = env
  )
}
