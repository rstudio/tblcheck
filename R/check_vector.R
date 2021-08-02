#' Checks that two vectors are the same
#'
#' Check if two vectors have the same class, length, and values.
#' If the vectors differ, returns a failure state and an informative
#' message with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `vector_class`: `object` doesn't have the same classes as `expected`
#' 2. `vector_length`: `object` doesn't have the same length as `expected`
#' 3. `vector_values`: `object` doesn't contain the same values as `expected`
#'
#' @param object A vector to be compared to `expected`.
#' @param expected A vector containing the expected result.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   print. Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same classes.
#' @param check_length `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same length.
#' @param check_values `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` contain the same values.
#' @param unit `[character(1)]`\cr The label used to describe the vector in
#'   feedback messages. Defaults to `"result"`.
#' @param prefix `[character(1)]`\cr The prefix appended to the `problem` label 
#'   in [gradethis::fail()] objects. Defaults to `"vector_"`.
#'
#' @inherit check_table return
#' @export

check_vector <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  unit   = "result",
  prefix = "vector_"
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  assert_internally({
    checkmate::assert_vector(object)
    checkmate::assert_vector(expected)
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
    checkmate::assert_string(unit)
    checkmate::assert_string(prefix)
  })
  
  if (check_class) {
    return_if_graded(
      check_class(object, expected, unit = unit, prefix = prefix)
    )
  }
  
  if (check_length) {
    obj_length <- length(object)
    exp_length <- length(expected)
    
    if (!identical(obj_length, exp_length)) {
      length_problem <- problem(
        paste0(prefix, "length"), exp_length, obj_length
      )
      exp_length <- plu::ral("n value", n = exp_length)
      obj_length <- plu::ral("n value", n = obj_length)
      
      return_fail(
        "Your {unit} should contain {exp_length}, but it has {obj_length}.",
        problem = length_problem
      )
    }
  }
  
  if (check_values) {
    n_values <- min(length(expected), max_diffs)
    
    if (!identical(object[seq_len(n_values)], expected[seq_len(n_values)])) {
      t_values <- plu::ral("n value", n = n_values)
      first_n_values <- knitr::combine_words(expected[seq_len(n_values)], before = "`")
      
      return_fail(
        "The first {t_values} of your {unit} should be {first_n_values}.",
        problem = problem(paste0(prefix, "values"), first_n_values)
      )
    }
    
    if (!identical(object, expected)) {
      return_fail(
        "Your {unit} contains unexpected values.",
        problem = problem(paste0(prefix, "values"), NULL)
      )
    }
  }
}
