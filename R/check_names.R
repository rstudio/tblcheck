#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `names`: The object has names that are not expected,
#'   or is missing names that are expected.
#'
#' @inheritParams tbl_check_class
#' @param max_diffs `[numeric(1)]`\cr The maximum number of missing and/or
#'   unexpected names to include in an informative failure message.
#'   Defaults to 3.
#'
#' @inherit check_table return
#' @export

check_names <- function(
  object = .result, expected = .solution,
  max_diffs = 3, unit = "result", problem_prefix = ""
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  assert_internally({
    checkmate::assert_number(max_diffs, lower = 1)
    checkmate::assert_string(unit)
    checkmate::assert_string(problem_prefix)
  })
  
  names_exp <- names(expected)
  names_obj <- names(object)
  
  missing <- max_setdiff(
    names_exp, names_obj,
    max_diffs = max_diffs,
    column = inherits(object, "data.frame")
  )
  
  missing_msg  <- glue::glue(
    "Your {unit} should have {missing$unit} {missing$str}. "
  )

  unexpected <- max_setdiff(
    names_obj, names_exp,
    max_diffs = max_diffs,
    column = inherits(object, "data.frame"),
    and = " or "
  )
  
  unexpected_msg  <- glue::glue(
    "Your {unit} should not have {unexpected$unit} {unexpected$str}. "
  )
  
  if (length(missing$diffs) || length(unexpected$diffs)) {
    return_fail(
      paste0(missing_msg, unexpected_msg),
      problem = problem(
        paste0(problem_prefix, "names"),
        missing = missing$diffs,
        unexpected = unexpected$diffs
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
#' @param unit An optional string describing the elements in `x` and `y`. Passed
#'   to [plu::ral()]. Should be written in the singular form, e.g. `"n column"`.
#' @param transform A function applied to each element of the items appearing in
#'   the string, prior to combining into a string. Use `identity` to keep the
#'   items without modification.
#' @inheritParams knitr::combine_words
#' @return A list with three items: 
#' 
#'   - `diffs` contains the full `setdiff()` between `x` and `y`. 
#'   - `str` contains a string describing the differences up to `max_diffs`.
#'   - `unit` contains the pluralized `unit`, relative to the number of `diffs`.
#' 
#' @keywords internal

max_setdiff <- function(
  x,
  y,
  max_diffs = Inf,
  and = " and ",
  column = FALSE,
  transform = md_code
) {
  diffs <- setdiff(x, y)
  diffs_max <- diffs[seq_len(min(max_diffs, length(diffs)))]

  more <- if (length(diffs) > length(diffs_max)) {
    glue::glue(length(diffs) - length(diffs_max), " more")
  } else {
    NULL
  }

  diffs_max <- c(transform(diffs_max), more)
  
  unit <- if (column) {
    ngettext(length(diffs), "a column named", "columns named")
  } else {
    ngettext(length(diffs), "the name", "the names")
  }

  list(
    diffs = diffs,
    str = knitr::combine_words(diffs_max, and = and),
    unit = unit
  )
}
