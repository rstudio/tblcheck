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


combine_words_with_more <- function(
  x, max_length = Inf, transform = md_code, ...
) {
  if (!length(x)) {
    return(NULL)
  }
  
  x_length <- length(x)
  
  x_max <- x[seq_len(min(max_length, x_length))]
  
  more <- if (x_length > max_length) {
    paste(x_length - max_length, "more")
  }
  
  knitr::combine_words(c(transform(x_max), more), ...)
}
