#' Check that the levels of two factors are the same
#'
#' Checks if `object` and `expected` have the same [levels][levels()].
#' If the levels differ
#' - `vec_check_levels()` returns a list describing the problem
#' - `vec_grade_levels()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `n_levels`: `object` and `expected` have a different number of levels.
#' 1. `levels`: The object has levels that are not expected,
#'   or is missing levels that are expected.
#' 1. `reverse_levels`: The `levels` of `object` are in the opposite order
#'   of `expected`
#' 1. `level_order_diffs`: The first `max_diffs` levels of `object` are not in 
#'   the same order as `expected`
#' 1. `level_order`: The levels of `object` are not in the same order
#'   as `expected`
#'
#' @inheritParams tbl_check_class
#' @param max_diffs `[numeric(1)]`\cr The maximum number of missing and/or
#'   unexpected names to include in an informative failure message.
#'   Defaults to 3.
#'
#' @return If there are any issues, a [list] from `vec_check_levels()` or a
#'   [gradethis::fail()] message from `vec_grade_levels()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples
#' .result <- as.factor(rep_len(letters[1:3], 6))
#' .solution <- as.factor(rep_len(letters[1:2], 6))
#' vec_check_levels()
#' vec_grade_levels()
#' 
#' .result <- as.factor(letters[1:6])
#' .solution <- as.factor(letters[21:26])
#' vec_check_levels()
#' vec_grade_levels()
#' vec_grade_levels(max_diffs = 5)
#' vec_grade_levels(max_diffs = Inf)
vec_check_levels <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  levels_exp <- levels(expected)
  levels_obj <- levels(object)
  
  if (!identical(levels_exp, levels_obj)) {
    if (!identical(length(levels_exp), length(levels_obj))) {
      return(problem("n_levels", length(levels_exp), length(levels_obj)))
    }
    
    missing_levels    <- setdiff(levels_exp, levels_obj)
    unexpected_levels <- setdiff(levels_obj, levels_exp)
    
    if (length(missing_levels) || length(unexpected_levels)) {
      return(
        problem(
          "levels", missing = missing_levels, unexpected = unexpected_levels
        )
      )
    }
    
    if (identical(levels_obj, rev(levels_exp))) {
      return(problem("reverse_levels"))
    }
    
    n_levels <- min(length(levels_exp), max_diffs)
    n_levels_exp <- levels_exp[seq_len(n_levels)]
    n_levels_obj <- levels_obj[seq_len(n_levels)]
    
    if (!identical(n_levels_obj, n_levels_exp)) {
      return(problem("level_order_diffs", n_levels_exp, n_levels_obj))
    }
    
    problem("level_order")
  }
}

#' @rdname vec_check_levels
#' @export
vec_grade_levels <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame()
) {
  tbl_grade(
    vec_check_levels(object, expected, env = env),
    max_diffs = max_diffs,
    env = env
  )
}

tbl_message.levels_problem <- function(problem, max_diffs = 3, ...) {
  problem$missing_msg <- problem$missing_msg %||% 
    ngettext(
      length(problem$missing),
      "Your result should have a level named {missing}. ",
      "Your result should have levels named {missing}. "
    )
  
  problem$unexpected_msg  <- problem$unexpected_msg %||% 
    ngettext(
      length(problem$unexpected),
      "Your result should not have a level named {unexpected}.",
      "Your result should not have levels named {unexpected}."
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
      "Your `{column}` column should have a level named {missing}. ",
      "Your `{column}` column should have levels named {missing}. "
    )
  
  problem$unexpected_msg <- problem$unexpected_msg %||% 
    ngettext(
      length(problem$unexpected),
      "Your `{column}` column should not have a level named {unexpected}.",
      "Your `{column}` column should not have levels named {unexpected}."
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

tbl_message.column_n_levels_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your `{column}` column should have {expected} level, ",
      "Your `{column}` column should have {expected} levels, "
    )
  
  NextMethod()
}

tbl_message.reverse_levels_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    gettext("The order of the levels in your result are the reverse of the expected order.")
  
  glue::glue_data(problem, problem$msg, problem$exp_msg %||% "")
}

tbl_message.column_reverse_levels_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    gettext("The order of the levels in your `{column}` column are the reverse of the expected order.")
  
  NextMethod()
}

tbl_message.level_order_diffs_problem <- function(problem, ...) {
  problem$n_levels <- length(problem$expected)
  problem$expected <- knitr::combine_words(md_code(problem$expected))
  problem$actual   <- knitr::combine_words(md_code(problem$actual))
  
  problem$msg <- problem$msg %||%
    gettext("Your result's levels were not in the expected order. ")
  
  problem$exp_msg <- problem$exp_msg %||%
    ngettext(
      problem$n_levels,
      "The first level of your result should be {expected}, but it was {actual}.",
      "The first {n_levels} levels of your result should be {expected}, but they were {actual}."
    )
  
  glue::glue_data(problem, problem$msg, problem$exp_msg)
}

tbl_message.column_level_order_diffs_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    gettext("Your `{column}` column's levels were not in the expected order. ")
  
  problem$exp_msg <- problem$exp_msg %||%
    ngettext(
      length(problem$expected),
      "The first level of your `{column}` column should be {expected}, but it was {actual}.",
      "The first {n_levels} levels of your `{column}` column should be {expected}, but they were {actual}."
    )
  
  NextMethod()
}

tbl_message.level_order_problem <- function(problem, ...) {
  "Your result's levels were not in the expected order."
}

tbl_message.column_level_order_problem <- function(problem, ...) {
  glue::glue_data(
    problem, "Your `{column}` column's levels were not in the expected order."
  )
}
