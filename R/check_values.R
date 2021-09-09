#' Checks that two vectors are contain the same values
#'
#' Check if two vectors contain the same values.
#' If the values differ
#' - `vec_check_values()` returns a list describing the problem
#' - `vec_grade_values()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `value_diffs`: The first `max_diffs` elements of `object` don't
#'   contain the same values as `expected`
#' 1. `values`: `object` doesn't contain the same values as `expected`
#'
#' @param object A vector to be compared to `expected`.
#' @param expected A vector containing the expected result.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same classes.
#' @param check_length `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same length.
#' @param check_levels `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same [factor levels][levels()].
#' @param check_values `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` contain the same values.
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same names.
#' @inheritParams tbl_check_table
#'
#' @return If there are any issues, a [list] from `vec_check_vector()` or a
#'   [gradethis::fail()] message from `vec_grade_vector()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples 
#' .result <- 1:10
#' .solution <- letters[1:10]
#' vec_check_vector()
#' vec_grade_vector()
#' 
#' .result <- 1:10
#' .solution <- 1:11
#' vec_check_vector()
#' vec_grade_vector()
#' 
#' .result <- 1:10
#' .solution <- rlang::set_names(1:10, letters[1:10])
#' vec_check_vector()
#' vec_grade_vector()
#' vec_grade_vector(max_diffs = 5)
#' vec_grade_vector(max_diffs = Inf)
#' 
#' .result <- 1:10
#' .solution <- 11:20
#' vec_check_vector()
#' vec_grade_vector()
#' vec_grade_vector(max_diffs = 5)
#' vec_grade_vector(max_diffs = Inf)
vec_check_values <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  assert_internally({
    checkmate::assert_vector(object)
    checkmate::assert_vector(expected)
    checkmate::assert_number(max_diffs, lower = 1)
  })
  
  exp_values <- unname(expected)
  obj_values <- unname(object)
  
  n_values <- min(length(expected), max_diffs)
  first_n_values <- exp_values[seq_len(n_values)]
  
  if (!identical(obj_values[seq_len(n_values)], first_n_values)) {
    return(problem("value_diffs", first_n_values))
  }
  
  if (!identical(obj_values, exp_values)) {
    return(problem("values"))
  }
}

#' @rdname vec_check_values
#' @export
vec_grade_values <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      vec_check_vector(
        object = object,
        expected = expected,
        max_diffs = max_diffs,
        env = env
      ),
      max_diffs = max_diffs,
      env = env
    )
  )
}

tbl_message.value_diffs_problem <- function(problem, ...) {
  problem$n_values <- length(problem$expected)
  problem$expected <- knitr::combine_words(md_code(problem$expected))
  
  problem$msg <- problem$msg %||%
    ngettext(
      problem$n_values,
      "The first value of your result should be {expected}.",
      "The first {n_values} values of your result should be {expected}."
    )
  
  glue::glue_data(problem, problem$msg)
}

tbl_message.column_value_diffs_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    ngettext(
      length(problem$expected),
      "The first value of your `{column}` column should be {expected}.",
      "The first {n_values} values of your `{column}` column should be {expected}."
    )
  
  NextMethod()
}

tbl_message.values_problem <- function(problem, ...) {
  "Your result contains unexpected values."
}

tbl_message.column_values_problem <- function(problem, ...) {
  glue::glue_data(problem, "Your `{column}` column contains unexpected values.")
}
