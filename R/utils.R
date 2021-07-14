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

assert_map <- function(fn, ...) {
  call <- as.list(match.call()[-1])
  call <- call[names(call) != "fn"]
  
  dots <- list(...)
  args <- dots[!names(call) == ""]
  vars <- dots[names(call) == ""]
  names(vars) <- call[names(call) == ""]
  
  mapply(fn, vars, .var.name = names(vars), MoreArgs = args)
}
