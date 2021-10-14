#' Check that the dimensions of two object are the same
#'
#' Checks if `object` and `expected` have the same [dimenisons][dim()].
#' If the dimensions differ
#' - `tbl_check_dimensions()` returns a list describing the problem
#' - `tbl_grade_dimensions()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `dimensions_n`: `object` and `expected` have a different number
#'   of dimensions
#' 1. `length`: `object` and `expected` are one-dimensional vectors of
#'   different lengths
#' 1. `ncol`: `object` and `expected` are two-dimensional objects with a
#'   different number of columns
#' 1. `nrow`: `object` and `expected` are two-dimensional objects with a
#'   different number of rows
#' 1. `dimensions`: `object` and `expected` are multi-dimensional arrays with
#'   different dimensions
#'
#' @inheritParams tbl_check_class
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check_dimensions()` or a
#'   [gradethis::fail()] message from `tbl_grade_dimensions()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples
#' .result <- 1:10
#' .solution <- 1:5
#' tbl_check_dimensions()
#' tbl_grade_dimensions()
#' 
#' .result <- tibble::tibble(a = 1:10, b = 1:10, c = 1:10)
#' .solution <- tibble::tibble(a = 1:10, b = 1:10)
#' tbl_check_dimensions()
#' tbl_grade_dimensions()
#' 
#' .result <- tibble::tibble(a = 1:10, b = 1:10)
#' .solution <- tibble::tibble(a = 1:5, b = 1:5)
#' tbl_check_dimensions()
#' tbl_grade_dimensions()
#' 
#' .result <- 1:12
#' .solution <- matrix(1:12, 3)
#' tbl_check_dimensions()
#' tbl_grade_dimensions()
tbl_check_dimensions <- function(
  object = .result,
  expected = .solution,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  obj_dim <- dim(object) %||% length(object)
  exp_dim <- dim(expected) %||% length(expected)
  
  if (identical(obj_dim, exp_dim)) {
    return(invisible())
  }
  
  if (!identical(length(obj_dim), length(exp_dim))) {
    return(problem("dimensions_n", length(exp_dim), length(obj_dim)))
  }
  
  if (length(exp_dim) == 1) {
    return(problem("length", exp_dim, obj_dim))
  }
  
  if (length(exp_dim) > 2) {
    return(problem("dimensions", exp_dim, obj_dim))
  }
  
  if (!identical(obj_dim[[2]], exp_dim[[2]])) {
    return(problem("ncol", exp_dim[[2]], obj_dim[[2]]))
  }
  
  if (!identical(obj_dim[[1]], exp_dim[[1]])) {
    return(problem("nrow", exp_dim[[1]], obj_dim[[1]]))
  }
}

#' @rdname tbl_check_dimensions
#' @export
vec_check_dimensions <- tbl_check_dimensions

#' @rdname tbl_check_dimensions
#' @export
tbl_grade_dimensions <- function(
  object = .result, 
  expected = .solution,
  env = parent.frame(),
  ...
) {
  tbl_grade(
    tbl_check_dimensions(object, expected, env = env),
    env = env,
    ...
  )
}

#' @rdname tbl_check_dimensions
#' @export
vec_grade_dimensions <- tbl_grade_dimensions

tbl_message.dimensions_n_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your `{column}` column should have {expected} dimension, ",
        "Your `{column}` column should have {expected} dimensions, "
      )
  } else if (is_problem(problem, "table")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your table should have {expected} dimension, ",
        "Your table should have {expected} dimensions, "
      )
  }
  
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should have {expected} dimension, ",
      "Your result should have {expected} dimensions, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} dimension.",
      "but it has {actual} dimensions."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.length_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your `{column}` column should contain {expected} value, ",
        "Your `{column}` column should contain {expected} values, "
      )
  }
  
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should contain {expected} value, ",
      "Your result should contain {expected} values, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} value.",
      "but it has {actual} values."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.ncol_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your `{column}` column should have {expected} column, ",
        "Your `{column}` column should have {expected} columns, "
      )
  } else if (is_problem(problem, "table")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your table should have {expected} column, ",
        "Your table should have {expected} columns, "
      )
  }
  
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should have {expected} column, ",
      "Your result should have {expected} columns, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} column.",
      "but it has {actual} columns."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.nrow_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your `{column}` column should have {expected} row, ",
        "Your `{column}` column should have {expected} rows, "
      )
  } else if (is_problem(problem, "table")) {
    problem$exp_msg <- problem$exp_msg %||% 
      ngettext(
        problem$expected,
        "Your table should have {expected} row, ",
        "Your table should have {expected} rows, "
      )
  }
  
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your result should have {expected} row, ",
      "Your result should have {expected} rows, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it has {actual} row.",
      "but it has {actual} rows."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.dimensions_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$msg <- problem$exp_msg %||% 
      gettext("Your `{column}` column should be an array with dimensions {expected}, but it has dimensions {actual}.")
  } else if (is_problem(problem, "table")) {
    problem$msg <- problem$exp_msg %||% 
      gettext("Your table should be an array with dimensions {expected}, but it has dimensions {actual}.")
  }
  
  problem$msg <- problem$msg %||%
    gettext("Your result should be an array with dimensions {expected}, but it has dimensions {actual}.")
  
  problem$actual   <- paste(problem$actual, collapse = " x ")
  problem$expected <- paste(problem$expected, collapse = " x ")
    
  glue::glue_data(problem, problem$msg)
}
