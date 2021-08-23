#' Check that the levels of two factors are the same
#'
#' Checks if `object` and `expected` have the same [levels][levels()].
#' If the levels differ
#' - `tbl_check_levels()` returns a list describing the problem
#' - `tbl_grade_levels()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `n_levels`: `object` and `expected` have a different number of levels.
#' 1. `levels`: The object has levels that are not expected,
#'   or is missing names that are expected.
#'
#' @inheritParams tbl_check_class
#' @param max_diffs `[numeric(1)]`\cr The maximum number of missing and/or
#'   unexpected names to include in an informative failure message.
#'   Defaults to 3.
#'
#' @return If there are any issues, a [list] from `tbl_check_levels()` or a
#'   [gradethis::fail()] message from `tbl_grade_levels()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples
#' .result <- as.factor(rep_len(letters[1:3], 6))
#' .solution <- as.factor(rep_len(letters[1:2], 6))
#' tbl_check_levels()
#' tbl_grade_levels()
#' 
#' .result <- as.factor(letters[1:6])
#' .solution <- as.factor(letters[21:26])
#' tbl_check_levels()
#' tbl_grade_levels()
#' tbl_grade_levels(max_diffs = 5)
#' tbl_grade_levels(max_diffs = Inf)
tbl_check_levels <- function(
  object = .result,
  expected = .solution,
  envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  levels_exp <- levels(expected)
  levels_obj <- levels(object)
  
  if (!identical(levels_exp, levels_obj)) {
    if (!identical(length(levels_exp), length(levels_obj))) {
      return(problem("n_levels", length(levels_exp), length(levels_obj)))
    }
    
    problem(
      "levels", 
      missing = setdiff(levels_exp, levels_obj),
      unexpected = setdiff(levels_obj, levels_exp)
    )
  }
}

#' @rdname tbl_check_levels
#' @export
tbl_grade_levels <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_levels(object, expected, envir = envir),
      max_diffs = max_diffs
    )
  )
}

tbl_message.levels_problem <- function(problem, max_diffs = 3, ...) {
  problem$missing_msg <- problem$missing_msg %||% 
    ngettext(
      length(problem$missing),
      "Your result should have the level {missing}. ",
      "Your result should have the levels {missing}. "
    )
  
  problem$unexpected_msg  <- problem$unexpected_msg %||% 
    ngettext(
      length(problem$unexpected),
      "Your result should not have the level {unexpected}.",
      "Your result should not have the levels {unexpected}."
    )
  
  if (!is.null(problem[["missing"]])) {
    problem$missing <- combine_words_with_more(problem$missing, max_diffs)
  } else {
    problem$missing_msg <- ""
  }
  
  if (!is.null(problem[["unexpected"]])) {
    problem$unexpected <- combine_words_with_more(problem$unexpected, max_diffs, and = " or ")
  } else {
    problem$unexpected_msg <- ""
  }
  
  glue::glue_data(problem, paste0(problem$missing_msg, problem$unexpected_msg))
}

tbl_message.column_levels_problem <- function(problem, max_diffs = 3, ...) {
  problem$missing_msg <- problem$missing_msg %||% 
    ngettext(
      length(problem$missing),
      "Your `{column}` column should have the level {missing}. ",
      "Your `{column}` column should have the levels {missing}. "
    )
  
  problem$unexpected_msg <- problem$missing_msg %||% 
    ngettext(
      length(problem$unexpected),
      "Your `{column}` column should not have the level {unexpected}.",
      "Your `{column}` column should not have the levels {unexpected}."
    )
  
  NextMethod()
}

tbl_message.n_levels_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should have {expected} level, ",
      "Your result should have {expected} levels, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} level.",
      "but it has {actual} levels."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.column_length_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your `{column}` column should have {expected} lavel, ",
      "Your `{column}` column should have {expected} levels, "
    )
  
  NextMethod()
}
