#' Create a message from a problem object
#'
#' `problem_message()` is an S3 generic that powers the conversion of problems
#' detected by [tbl_check()] and [vec_check()] (and their related helper
#' functions) into a human-readable message.
#'
#' @examples
#' problem <- problem(
#'   type = "class",
#'   expected = "character",
#'   actual = "numeric",
#'   expected_length = 1,
#'   actual_length = 2
#' )
#'
#' problem_message(problem)
#'
#' @param problem An object with base class `gradethis_problem`. Problems
#'   identified by \pkg{tblcheck} also include `tblcheck_problem`, plus
#'   additional classes that more specifically identify the problem type.
#' @param ... Additional arguments passed to the underlying methods.
#'
#' @return A length-1 character string with a message describing the problem.
#'
#' @family Problem functions
#' @export
problem_message <- function(problem, ...) {
  UseMethod("problem_message")
}

#' @export
problem_message.default <- function(problem, ...) {
  invisible()
}

#' @export
problem_message.gradethis_problem <- function(problem, ...) {
  type_msg <- if (!is.null(problem$type)) {
    gettext("Your code resulted in a `{type}` problem. ")
  } else {
    ""
  }

  exp_msg <- if (!is.null(problem$expected)) {
    expected <- paste(md_code(problem$expected), collapse = ", ")
    gettext("I was expecting a value of {expected}. ")
  } else {
    ""
  }

  obj_msg <- if (!is.null(problem$actual)) {
    actual <- paste(md_code(problem$actual), collapse = ", ")
    gettext("Your result gave a value of `{actual}`. ")
  } else {
    ""
  }

  glue::glue_data(problem, type_msg, exp_msg, obj_msg)
}

#' @export
problem_message.tblcheck_problem <- function(problem, ...) {
  NextMethod()
}
