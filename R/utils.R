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

md_code <- function(x) paste0("`", x, "`")

check_table <- function(x) {
  name <- deparse(substitute(x))
  if (inherits(x, "data.frame")) return(TRUE)
  sprintf("%s is not a data frame", name)
}

assert_internally <- function(expr, ..., error = internal_error) {
  tryCatch(expr, error = error, ...)
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

str_subset <- function(string, pattern, negate = FALSE) {
  matches <- grepl(pattern, string)
  if (negate) matches <- !matches
  string[matches]
}
