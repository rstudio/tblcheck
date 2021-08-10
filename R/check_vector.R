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
#' 4. `vector_names`: `object` has different `names` than `expected`
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
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same names.
#' @param object_label `[character(1)]`\cr The label used to describe the vector
#'   in feedback messages. Defaults to `"result"`.
#' @param problem_prefix `[character(1)]`\cr The prefix appended to the
#'   `problem` label in [gradethis::fail()] objects. Defaults to `"vector_"`.
#' @inheritParams tbl_check_table
#'
#' @return If there are any issues, a [list] from `tbl_check_vector()` or a
#'   [gradethis::fail()] message from `tbl_grade_vector()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export

tbl_check_vector <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  object_label = NULL,
  problem_prefix = "vector_",
  envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  object_label <- object_label %||% "result"
  
  assert_internally({
    checkmate::assert_vector(object)
    checkmate::assert_vector(expected)
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_logical(check_class,  any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_values, any.missing = FALSE, len = 1)
    checkmate::assert_logical(check_length, any.missing = FALSE, len = 1)
    checkmate::assert_string(object_label)
    checkmate::assert_string(problem_prefix)
  })
  
  if (check_class) {
    return_if_problem(
      tbl_check_class(
        object, expected,
        object_label = object_label,
        problem_prefix = problem_prefix
      )
    )
  }
  
  if (check_length) {
    return_if_problem(
      tbl_check_length(
        object, expected,
        object_label = object_label,
        problem_prefix = problem_prefix
      )
    )
  }
  
  if (check_values) {
    exp_values <- unname(expected)
    obj_values <- unname(object)
    
    n_values <- min(length(expected), max_diffs)
    first_n_values <- exp_values[seq_len(n_values)]
    
    if (!identical(obj_values[seq_len(n_values)], first_n_values)) {
      return(
        problem(
          paste0(problem_prefix, "values"),
          first_n_values,
          object_label = object_label
        )
      )
    }
    
    if (!identical(obj_values, exp_values)) {
      return(
        problem(
          paste0(problem_prefix, "values"),
          object_label = object_label
        )
      )
    }
  }
  
  if (check_names) {
    return_if_problem(
      tbl_check_names(
        object, expected,
        object_label = object_label,
        problem_prefix = problem_prefix
      )
    )
  }
}

tbl_grade_vector <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  object_label = NULL,
  problem_prefix = "vector_",
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_vector(
        object = object,
        expected = expected,
        max_diffs = max_diffs,
        check_class = check_class,
        check_length = check_length,
        check_values = check_values,
        check_names = check_names,
        object_label = object_label,
        problem_prefix = problem_prefix,
        envir = envir
      ),
      max_diffs = max_diffs
    )
  )
}

tbl_message_values <- function(problem, ...) {
  object_label <- problem$object_label
  n_values <- length(problem$expected)
  exp_values <- knitr::combine_words(md_code(problem$expected))
  
  message <- if (n_values != 0) {
    ngettext(
      n_values,
      "The first value of your {object_label} should be {exp_values}.",
      "The first {n_values} values of your {object_label} should be {exp_values}."
    )
  } else {
    "Your {object_label} contains unexpected values."
  }
  
  return_fail(glue::glue(message), problem = problem)
}
