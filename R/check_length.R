#' Check that the length of two object are the same
#'
#' Checks if `object` and `expected` have the same [length][length()].
#' If the lengths differ
#' - `vec_check_lengths()` returns a list describing the problem
#' - `vec_grade_lengths()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `length`: `object` and `expected` are one-dimensional vectors of
#'   different lengths
#'
#' @inheritParams tbl_check_dimensions
#'
#' @return If there are any issues, a [list] from `vec_check_length()` or a
#'   [gradethis::fail()] message from `vec_grade_length()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples
#' .result <- 1:10
#' .solution <- 1:5
#' vec_check_length()
#' vec_grade_length()
vec_check_length <- function(
  object = .result,
  expected = .solution,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  obj_length <- length(object)
  exp_length <- length(expected)
  
  if (!identical(obj_length, exp_length)) {
    return(problem("length", exp_length, obj_length))
  }
}

#' @rdname vec_check_length
#' @export
vec_grade_length <- function(
  object = .result, 
  expected = .solution,
  env = parent.frame()
) {
  return_if_graded(
    tbl_grade(vec_check_length(object, expected, env = env))
  )
}

tbl_message.length_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should contain {expected} value, ",
      "Your result should contain {expected} values, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} value.",
      "but it has {actual} values."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.column_length_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your `{column}` column should contain {expected} value, ",
      "Your `{column}` column should contain {expected} values, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} value.",
      "but it has {actual} values."
    )
  
  NextMethod()
}
