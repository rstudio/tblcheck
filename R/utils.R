#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

"%||%" <- function(x, y) if (is.null(x)) y else x

md_code <- function(x) {
  if (!length(x)) return(x)
  paste0("`", x, "`")
}

assert_internally <- function(expr, ..., error = internal_error) {
  return_if_graded(tryCatch(expr, error = error, ...), parent.frame())
}

internal_error <- function(err) {
  message("An error occurred in the grading code: ", err$message)
  gradethis::graded(
    message = paste(
      "Uh-oh! We can't provide feedback at this time. Don't worry, it's not", 
      "your fault! There's an issue behind-the-scenes with this exercise."
    ),
    correct = logical(0),
    type = "warning",
    location = "replace",
    problem = problem("internal_feedback_error", error = err$message)
  )
}
  
# Wrap any expression that may return a grade in `return_if_graded()` to return
# the graded condition from the calling function if we don't have another
# calling handler watching for the `gradethis_graded` condition.
return_if_graded <- function(expr, envir = parent.frame()) {
  withCallingHandlers(
    expr,
    gradethis_graded = function(grade) {
      signalCondition(grade)
      if (getOption("tblcheck.return_first_grade", TRUE)) {
        rlang::return_from(envir, grade)
      }
    }
  )
}

return_fail <- function(..., env = parent.frame()) {
  grade <- gradethis::fail(..., env = env)
  if (getOption("tblcheck.return_first_grade", TRUE)) {
    rlang::return_from(env, grade)
  }
}

return_if_problem <- function(problem, ..., envir = parent.frame()) {
  if (inherits(problem, "tblcheck_problem")) {
    dots <- list(...)
    
    if (length(dots)) {
      problem_prefix <- paste0(names(dots)[[length(dots)]], "_")
      assert_internally(checkmate::assert_string(problem_prefix))
      problem$type <- gsub("^(.*_)?", problem_prefix, problem$type)
      
      dots    <- dots[!names(dots) %in% names(problem)]
      problem <- as.problem(c(problem, dots))
    }
    
    rlang::return_from(envir, problem)
  }
}
