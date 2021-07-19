#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `names`: The table has names that are not expected,
#'   or is missing names that are expected.
#'
#' @inheritParams check_table
#' @param max_diffs `[numeric(1)]`\cr The maximum number of missing and/or
#'   unexpected names to include in an informative failure message.
#'   Defaults to 3.
#'
#' @inherit check_table return
#' @export

check_names <- function(object, expected, max_diffs = 3) {
  unit <- if (inherits(object, "data.frame")) "a column {named}" else "the name"
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  missing <- max_setdiff(
    md_code(names(expected)), md_code(names(object)), max_diffs = max_diffs
  )
  missing_unit <- plu::ral(unit, missing)
  missing      <- knitr::combine_words(missing, and = " and ")
  missing_msg  <- glue::glue(
    "Your result should have {missing_unit} {missing}. "
  )

  unexpected <- max_setdiff(
    md_code(names(object)), md_code(names(expected)), max_diffs = max_diffs
  )
  unexpected_unit <- plu::ral(unit, unexpected)
  unexpected      <- knitr::combine_words(unexpected, and = " or ")
  unexpected_msg  <- glue::glue(
    "Your result should not have {unexpected_unit} {unexpected}. "
  )
  
  if (length(missing_msg) || length(unexpected_msg)) {
    gradethis::fail(
      paste0(missing_msg, unexpected_msg),
      problem = problem(
        "names",
        missing = setdiff(names(expected), names(object)),
        unexpected = setdiff(names(object), names(expected))
      )
    )
  }
  
  invisible()
}

#' Find up to a certain number of differences between two vectors
#'
#' @param x,y Vectors. Elements that appear in `x` but not `y` will be listed.
#' @param max_diffs The maximum number of elements to list.
#'   Defaults to [Inf], which includes all elements.
#' @return A vector with no more than `max_diffs` elements.
#' @keywords internal

max_setdiff <- function(x, y, max_diffs = Inf) {
  diffs     <- setdiff(x, y)
  max_diffs <- diffs[seq_len(min(max_diffs, length(diffs)))]
  
  more <- if (length(diffs) > length(max_diffs)) {
    glue::glue(length(diffs) - length(max_diffs), " more")
  } else {
    NULL
  }
  
  c(max_diffs, more)
}
