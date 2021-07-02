#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object with the expected names.
#' @param max The maximum number of missing and/or unexpected names to include
#'   in an informative failure message. Defaults to 3.
#'
#' @return Returns `object` invisibly.
#' @export

check_names <- function(object, expected, max = 3) {
  missing_msg <- unexpected_msg <- NULL
  
  expected_names <- names(expected)
  object_names   <- names(object)
  
  unit <- if (inherits(object, "data.frame")) "a column named" else "the name"
  
  missing_names <- list_setdiff(
    expected_names, object_names, max = max, before = "`"
  )
  missing_msg <- glue::glue("Your result should have {unit} {names}. ")
  
  unexpected_names <- list_setdiff(
    object_names, expected_names, max = max, and = " or ", before = "`"
  )
  unexpected_msg <- glue::glue("Your result should not have {unit} {names}.")
  
  if (!is.null(missing_msg) || !is.null(unexpected_msg)) {
    gradethis::fail(paste0(missing_msg, unexpected_msg))
  }
  
  return(invisble(object))
}

#' List the difference between two vectors
#' 
#' Calls [setdiff()], optionally limits the result to `max` items, and then
#' combines the result with [knitr::combine_words()].
#'
#' @param x,y Vectors. Elements that appear in `x` but not `y` will be listed.
#' @param max The maximum number of elements to list.
#'   Defaults to [Inf], which includes all elements.
#' @param ... Additional arguments passed to [knitr::combine_words()]
#'
#' @return A single [character] string.

list_setdiff <- function(x, y, max = Inf, ...) {
  diff <- setdiff(x, y)
  diff <- diff[seq_len(min(max, length(diff)))]
  knitr::combine_words(diff, ...)
}
