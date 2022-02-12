#' Checks that two vectors are the same
#'
#' @description
#' Checks for differences between `object` and `expected` in the following order:
#' 1. Check class with [vec_check_class()]
#' 1. Check length with [vec_check_dimensions()]
#' 1. If the vector is a factor, check factor levels are the same with [vec_check_levels()]
#' 1. Check vector values are the same with [vec_check_values()]
#' 1. Check names with [vec_check_names()]
#'
#' If the vectors differ
#' - `vec_check()` returns a list describing the problem
#' - `vec_grade()` returns a failing grade and informative message
#' with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `class`: `object` doesn't have the same classes as `expected`.
#' 1. `length`: `object` doesn't have the same length as `expected`.
#' 1. `levels_n`, `levels`, `levels_reversed`, `levels_order`:
#'   See [vec_check_levels()].
#' 1. `values`: `object` doesn't contain the same values as `expected`.
#' 1. `names`: `object` has different [names][names()] than `expected`.
#' 1. `names_order`: `object` has the same [names][names()] as `expected`,
#'   but in a different order.
#'
#' @param object A vector to be compared to `expected`.
#' @param expected A vector containing the expected result.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same classes.
#' @inheritParams tbl_check_class
#' @param check_length `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same length.
#' @param check_levels `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same [factor levels][levels()].
#' @param check_values `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` contain the same values.
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same names.
#' @inheritParams tbl_check
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `vec_check()` or a
#'   [gradethis::fail()] message from `vec_grade()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#'
#' @examples
#' .result <- 1:10
#' .solution <- letters[1:10]
#' vec_check()
#' vec_grade()
#'
#' .result <- 1:10
#' .solution <- 1:11
#' vec_check()
#' vec_grade()
#'
#' .result <- 1:10
#' .solution <- rlang::set_names(1:10, letters[1:10])
#' vec_check()
#' vec_grade()
#' vec_grade(max_diffs = 5)
#' vec_grade(max_diffs = Inf)
#'
#' .result <- 1:10
#' .solution <- 11:20
#' vec_check()
#' vec_grade()
#' vec_grade(max_diffs = 5)
#' vec_grade(max_diffs = Inf)
vec_check <- function(
  object = .result,
  expected = .solution,
  check_class = TRUE,
  ignore_class = NULL,
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

  return_if_internal_problem({
    checkmate::assert_vector(expected)
    checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
  })

  if (check_class) {
    return_if_problem(
      vec_check_class(object, expected, ignore_class),
      prefix = "vector"
    )
  }

  if (check_length) {
    return_if_problem(
      vec_check_dimensions(object, expected),
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
      vec_check_values(object, expected),
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

#' @rdname vec_check
#' @export
vec_grade <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  env = parent.frame(),
  ...
) {
  tblcheck_grade(
    vec_check(
      object = object,
      expected = expected,
      check_class = check_class,
      ignore_class = ignore_class,
      check_length = check_length,
      check_values = check_values,
      check_names = check_names,
      env = env
    ),
    max_diffs = max_diffs,
    env = env,
    ...
  )
}
