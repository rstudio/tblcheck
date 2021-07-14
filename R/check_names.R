#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object with the expected names.
#' @param max_print The maximum number of missing and/or unexpected names to include
#'   in an informative failure message. Defaults to 3.
#'
#' @return Returns `object` invisibly.
#' @export

check_names <- function(object, expected, max_print = 3) {
  unit <- if (inherits(object, "data.frame")) "a column {named}" else "the name"
  
  assert_internally({
    checkmate::assert_number(max_print, lower = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  missing_msg <- check_names_message(
    names(expected), names(object),
    "Your result should have {unit} {str}. ",
    unit = unit, max_print = max_print
  )

  unexpected_msg <- check_names_message(
    names(object), names(expected),
    "Your result should not have {unit} {str}.",
    unit = unit, max_print = max_print, and = " or "
  )
  
  if (length(missing_msg) || length(unexpected_msg)) {
    gradethis::fail(
      paste0(missing_msg, unexpected_msg),
      problem = problem("table_names")
    )
  }
  
  return(invisible(object))
}

check_names_message <- function(
  x, y, glue_string, unit, max_print, and = " and "
) {
  names <- max_setdiff(x, y, max = max_print)
  str   <- knitr::combine_words(names, and = and, before = "`")
  unit  <- plu::ral(unit, names)
  glue::glue(glue_string)
}

#' Find up to a certain number of differences between two vectors
#'
#' @param x,y Vectors. Elements that appear in `x` but not `y` will be listed.
#' @param max The maximum number of elements to list.
#'   Defaults to [Inf], which includes all elements.
#' @return A vector with no more than `max` elements.
#' @keywords internal

max_setdiff <- function(x, y, max = Inf) {
  diff <- setdiff(x, y)
  diff[seq_len(min(max, length(diff)))]
}
