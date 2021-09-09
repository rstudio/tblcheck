#' Checks that two vectors are the same
#'
#' Check if two vectors have the same class, length, and values.
#' If the vectors differ
#' - `vec_check_vector()` returns a list describing the problem
#' - `vec_grade_vector()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `vector_class`: `object` doesn't have the same classes as `expected`
#' 1. `vector_length`: `object` doesn't have the same length as `expected`
#' 1. `vector_n_levels`, `vector_levels`, `vector_reverse_levels`,
#'   `vector_level_order_diffs`, `vector_level_order`: See [vec_check_levels()]
#' 1. `column_value_diffs`: The first `max_diffs` elements of `object` don't
#'   contain the same values as `expected`
#' 1. `vector_values`: `object` doesn't contain the same values as `expected`
#' 1. `vector_names`: `object` has different [names][names()] than `expected`
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
vec_check_vector <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_levels = TRUE,
  check_values = TRUE,
  check_names = TRUE,
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
      checkmate::assert_vector(object)
      checkmate::assert_vector(expected)
      checkmate::assert_number(max_diffs, lower = 1)
      checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
      checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
      checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
    })
  )
  
  if (check_class) {
    return_if_problem(
      vec_check_class(object, expected),
      prefix = "vector"
    )
  }
  
  if (check_length) {
    return_if_problem(
      vec_check_length(object, expected),
      prefix = "vector"
    )
  }
  
  if (check_levels) {
    return_if_problem(
      vec_check_levels(object, expected),
      prefix = "vector"
    )
  }
  
  if (check_values) {
    return_if_problem(
      vec_check_values(object, expected, max_diffs),
      prefix = "vector"
    )
  }
  
  if (check_names) {
    return_if_problem(
      vec_check_names(object, expected),
      prefix = "vector"
    )
  }
}

#' @rdname vec_check_vector
#' @export
vec_grade_vector <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  env = parent.frame()
) {
  tbl_grade(
    vec_check_vector(
      object = object,
      expected = expected,
      max_diffs = max_diffs,
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
