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
  problem, prefix = NULL, ..., envir = parent.frame()
) {
  if (inherits(problem, "tblcheck_problem")) {
    problem_class <- class(problem)
    problem <- c(problem, ...)
    class(problem) <- problem_class
    
    if (!is.null(prefix)) {
      # Add trailing underscore to prefix if it doesn't already have one
      prefix <- gsub("_?$", "_", prefix)
      
      custom_classes <- setdiff(
        problem_class, c("tblcheck_problem", "gradethis_problem", "list")
      )
      base_class <- custom_classes[length(custom_classes)]
      class(problem) <- unique(c(paste0(prefix, base_class), problem_class))
      
      problem$type <- gsub("^(.*_)?", prefix, problem$type)
    }
    
    rlang::return_from(envir, problem)
  }
}

#' Problem helper functions
#' 
#' - `problem_type()` returns a problem's type, or [`NULL`] if the input is
#'   not a problem.
#' - `is_problem()` tests whether an object is a `gradethis` problem.
#' - `is_tblcheck_problem()` tests whether an object is a problem created
#'   by `tblcheck`.
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
#' @export
#'
#' @examples
#' problem_type(tbl_check_vector(1, "1"))
#' is_problem(tbl_check_vector(1, "1"), "vector_class")
#' is_tblcheck_problem(tbl_check_vector(1, "1"), "class")
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

#' @export
print.tblcheck_problem <- function(x, ...) {
  cat("<tblcheck problem>", format(x, ...) %||% "<no message>", sep = "\n")
  str(unclass(x))
}

#' @export
format.tblcheck_problem <- function(x, ...) {
  tbl_message(x, ...)
}
