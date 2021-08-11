#' Checks that a column is identical across two tables
#'
#' Checks if the `name` column has the same class and values in `object` and
#' `expected`.
#' #' If the columns differ
#' - `tbl_check_column()` returns a list describing the problem
#' - `tbl_grade_column()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `column_class`: Any mismatch in the classes of the `name` column
#' 2. `column_length`: The `name` column doesn't have the expected length
#' 3. `column_values`: The `name` column doesn't have the expected values
#' 4. `column`: The `name` column doesn't appear in the `object`
#'
#' @param name `[character(1)]`\cr The name of the column to check.
#' @inheritParams tbl_check_table
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `name` has the
#'   same class in `object` and `expected`.
#' @param check_length `[logical(1)]`\cr Whether to check that `name` has the
#'   same length in `object` and `expected`.
#' @param check_values `[logical(1)]`\cr Whether to check that `name` has the
#'   same values in `object` and `expected`.
#'
#' @return If there are any issues, a [list] from `tbl_check_column()` or a
#'   [gradethis::fail()] message from `tbl_grade_column()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export

tbl_check_column <- function(
  name,
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  assert_internally({
    checkmate::assert_character(name, len = 1, any.missing = FALSE)
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  if (!name %in% names(expected)) {
    warning("`", name, "` is not a column in `expected`.")
    return()
  }
  
  if (!name %in% names(object)) {
    return_if_problem(problem("missing", name), column = name)
  }
  
  return_if_problem(
    tbl_check_vector(
      object[[name]],
      expected[[name]],
      max_diffs = max_diffs,
      check_class = check_class,
      check_length = check_length,
      check_values = check_values,
      check_names = FALSE
    ),
    column = name
  )
}

#' @rdname tbl_check_column
#' @export

tbl_grade_column <- function(
  name,
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_column(
        name = name,
        object = object,
        expected = expected,
        max_diffs = max_diffs,
        check_class = check_class,
        check_length = check_length,
        check_values = check_values,
        envir = envir
      )
    )
  )
}

tbl_message_missing <- function(problem, ...) {
  exp_column <- problem$expected
  
  message <- glue::glue("Your table should have a column named `{exp_column}`.")
  
  return_fail(message, problem = problem)
}
