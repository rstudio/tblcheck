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
#' 1. `values`: `object` doesn't contain the same values as `expected`
#'
#' @inheritParams vec_check_vector
#' @inheritParams tbl_grade
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
  
  return_if_internal_problem({
    checkmate::assert_vector(object)
    checkmate::assert_vector(expected)
    checkmate::assert_number(max_diffs, lower = 1)
  })
  
  exp_values <- unname(expected)
  obj_values <- unname(object)
  
  if (!identical(obj_values, exp_values)) {
    return(problem("values", exp_values, obj_values))
  }
}

#' @rdname vec_check_values
#' @export
vec_grade_values <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame(),
  ...
) {
  tbl_grade(
    vec_check_values(
      object = object,
      expected = expected,
      env = env
    ),
    max_diffs = max_diffs,
    env = env,
    ...
  )
}

tbl_message.values_problem <- function(problem, max_diffs = 3, ...) {
  problem$n_values <- min(
    max(length(problem$expected), length(problem$actual)),
    max_diffs
  )
  
  if (
    identical(
      problem$expected[seq_len(problem$n_values)],
      problem$actual[seq_len(problem$n_values)]
    )
  ) {
    if (is_problem(problem, "column")) {
      problem$msg <- problem$msg %||%
        "Your `{column}` column contains unexpected values."
    }
    
    problem$msg <- problem$msg %||%
      "Your result contains unexpected values."
    
    return(glue::glue_data(problem, problem$msg))
  }
  
  problem$expected <- knitr::combine_words(
    md_code(problem$expected[seq_len(problem$n_values)])
  )
  problem$actual <- knitr::combine_words(
    md_code(problem$actual[seq_len(problem$n_values)])
  )
  
  if (is_problem(problem, "column")) {
    problem$msg <- problem$msg %||%
      ngettext(
        problem$n_values,
        "The first value of your `{column}` column should be {expected}.",
        "The first {n_values} values of your `{column}` column should be {expected}."
      )
  }
  
  problem$msg <- problem$msg %||%
    ngettext(
      problem$n_values,
      "The first value of your result should be {expected}.",
      "The first {n_values} values of your result should be {expected}."
    )
  
  glue::glue_data(problem, problem$msg)
}
