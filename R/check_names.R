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
  expected_names <- names(expected)
  object_names   <- names(object)
  
  unit <- if (inherits(object, "data.frame")) "a column {named}" else "the name"
  
  missing     <- max_setdiff(expected_names, object_names, max = max)
  missing_str <- knitr::combine_words(missing, before = "`")
  missing_msg <- glue::glue(
    "Your result should have {plu::ral(unit, missing)} {missing_str}. "
  )
  
  unexpected     <- max_setdiff(object_names, expected_names, max = max)
  unexpected_str <- knitr::combine_words(unexpected, and = " or ", before = "`")
  unexpected_msg <- glue::glue(
    "Your result should not have {plu::ral(unit, unexpected)} {unexpected_str}."
  )
  
  if (length(missing_msg) || length(unexpected_msg)) {
    gradethis::fail(paste0(missing_msg, unexpected_msg))
  }
  
  return(invisble(object))
}

#' List the difference between two vectors in a user-friendly string
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
  max_setdiff(x, y, max = max)
  knitr::combine_words(diff, ...)
}

#' Find up to a certain number of differences between two vectors
#'
#' @inheritParams list_setdiff
#' @return A vector with no more than `max` elements.

max_setdiff <- function(x, y, max = Inf) {
  diff <- setdiff(x, y)
  diff[seq_len(min(max, length(diff)))]
}
