#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ
#' - `tbl_check_names()` returns a list describing the problem
#' - `tbl_grade_names()` returns a failing grade and informative message
#' with [gradethis::fail()]
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
#' @return If there are any issues, a [list] from `tbl_check_names()` or a
#'   [gradethis::fail()] message from `tbl_grade_names()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export

tbl_check_names <- function(
  object = .result, expected = .solution,
  object_label = NULL, problem_prefix = "", envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  if (is.null(object_label)) {
    object_label <- if (is.data.frame(object)) {
      "table"
    } else {
      "result"
    }
  }
  
  assert_internally({
    checkmate::assert_string(object_label)
    checkmate::assert_string(problem_prefix)
  })
  
  names_exp <- names(expected)
  names_obj <- names(object)
  
  if (!identical(names_exp, names_obj)) {
    return(
      problem(
        paste0(problem_prefix, "names"), 
        missing = setdiff(names_exp, names_obj),
        unexpected = setdiff(names_obj, names_exp),
        object_label = object_label
      )
    )
  }
}

#' @rdname tbl_check_names
#' @export

tbl_grade_names <- function(
  object = .result, expected = .solution, max_diffs = 3, envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_names(object, expected, envir = envir),
      max_diffs = max_diffs
    )
  )
}

tbl_message_names <- function(problem, max_diffs = 3) {
  object_label <- problem$object_label
  
  missing_names <- combine_words_with_more(
    problem$missing, max_diffs
  )
  missing_msg <- if (!is.null(missing_names)) {
    if (object_label == "table") {
      ngettext(
        length(problem$missing),
        "Your table should have a column named {missing_names}. ",
        "Your table should have columns named {missing_names}. "
      )
    } else {
      ngettext(
        length(problem$missing),
        "Your {object_label} should have the name {missing_names}. ",
        "Your {object_label} should have the names {missing_names}. "
      )
    }
  } else {
    ""
  }
  
  unexpected_names <- combine_words_with_more(
    problem$unexpected, max_diffs, and = " or "
  )
  unexpected_msg <- if (!is.null(unexpected_names)) {
    if (object_label == "table") {
      ngettext(
        length(problem$unexpected),
        "Your table should not have a column named {unexpected_names}.",
        "Your table should not have columns named {unexpected_names}."
      )
    } else {
      ngettext(
        length(problem$unexpected),
        "Your {unit} should not have the name {unexpected_names}.",
        "Your {unit} should not have the names {unexpected_names}."
      )
    }
  } else {
    ""
  }
  
  return_fail(
    glue::glue(missing_msg, unexpected_msg),
    problem = problem
  )
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
