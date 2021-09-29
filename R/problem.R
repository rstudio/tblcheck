#' Declare a problem
#' 
#' Useful for constructing a small list to communicate the problem that was
#' discovered during checking.
#' 
#' @param type A character string, e.g. `column_values` or `table_rows`, that
#'   describes the problem that was discovered.
#' @param expected,actual The expected and actual values. These should be
#'   included when the value is a summary, e.g. `nrow(expected)` or 
#'   `length(actual)`. Be careful not to include large amounts of data.
#' @param ... Additional elements to be included in the `problem` object.
#'   
#' @keywords internal
#' @noRd
problem <- function(
  type, expected = NULL, actual = NULL, ...
) {
  checkmate::assert_string(type, min.chars = 1)
  
  problem <- list(
    type = type,
    expected = expected,
    actual = actual,
    ...
  )
  
  structure(
    purrr::compact(problem),
    class = c(paste0(type, "_problem"), "tblcheck_problem", "gradethis_problem")
  )
}

return_if_problem <- function(
  problem, prefix = NULL, ..., env = parent.frame()
) {
  if (inherits(problem, "tblcheck_problem")) {
    if (!is.null(prefix)) {
      problem$location <- prefix
      
      problem_class <- append(
        class(problem), paste0(prefix, "_problem"), after = 1
      )
    } else {
      problem_class <- class(problem)
    }
    
    # Attributes are dropped by `c()`, so the class must be reintroduced
    problem <- c(problem, ...)
    class(problem) <- unique(problem_class)
    
    rlang::return_from(env, problem)
  }
}

#' Problem helper functions
#' 
#' - `problem_type()` returns a problem's type, or [`NULL`] if the input is
#'   not a problem.
#' - `is_problem()` tests whether an object is a `gradethis` problem.
#' - `is_tblcheck_problem()` tests whether an object is a problem created
#'   by `tblcheck`.
#' - `as_problem()` converts a list to a `tblcheck_problem`.
#'   
#' If `type` is specified, `is_problem()` and `is_tblcheck_problem()` test
#' whether an object is a problem of the specified type.
#'
#' @param x An object 
#' @param type `[character(1)]`\cr A `problem` type
#'
#' @return `is_problem()` and `is_tblcheck_problem()` return a [logical]
#'   of length 1.
#'   `problem_type()` returns a [character] of length 1.
#'   `as_problem()` returns a `tblcheck_problem`.
#' @export
#'
#' @examples
#' problem_type(vec_check_vector(1, "1"))
#' is_problem(vec_check_vector(1, "1"), "vector_class")
#' is_tblcheck_problem(vec_check_vector(1, "1"), "class")
problem_type <- function(x) {
  if (is_problem(x)) {
    return(x$type)
  }
  
  NULL
}

#' @rdname problem_type
#' @export
is_problem <- function(x, type = NULL) {
  inherits(x, "gradethis_problem") && (
    is.null(type) || inherits(x, paste0(type, "_problem"))
  )
}

#' @rdname problem_type
#' @export
is_tblcheck_problem <- function(x, type = NULL) {
  inherits(x, "tblcheck_problem") && (
    is.null(type) || inherits(x, paste0(type, "_problem"))
  )
}

#' @rdname problem_type
#' @export
as_problem <- function(x) {
  checkmate::assert_list(x)
  class(x) <- c("tblcheck_problem", "gradethis_problem")
  
  if (!is.null(x$location)) {
    class(x) <- c(paste0(x$location, "_problem"), class(x))
  }
  
  if (!is.null(problem_type(x))) {
    class(x) <- c(paste0(problem_type(x), "_problem"), class(x))
  }
  
  x
}

#' @export
print.tblcheck_problem <- function(x, ...) {
  cat("<tblcheck problem>", format(x, ...) %||% "<no message>", sep = "\n")
  str <- utils::capture.output(utils::str(unclass(x)))[-1]
  cat(sub("^ ", "", str), sep = "\n")
}

#' @export
format.tblcheck_problem <- function(x, ...) {
  tbl_message(x, ...)
}
