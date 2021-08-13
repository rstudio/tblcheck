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
    class = c(
      paste0(type, "_problem"), "tblcheck_problem", "gradethis_problem", "list"
    )
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
      class(problem) <- c(paste0(prefix, base_class), problem_class)
      
      problem$type <- gsub("^(.*_)?", prefix, problem$type)
    }
    
    rlang::return_from(envir, problem)
  }
}
