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
#' 1. `n_dimensions`: `object` and `expected` have a different number
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
    return(problem("n_dimensions", length(exp_dim), length(obj_dim)))
  }
  
  if (length(exp_dim) == 1) {
    return_if_problem(vec_check_length(object, expected, env))
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
tbl_grade_dimensions <- function(
  object = .result, 
  expected = .solution,
  env = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_dimensions(object, expected, env = env),
      env = env
    )
  )
}

tbl_message.n_dimensions_problem <- function(problem, ...) {
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

tbl_message.ncol_problem <- function(problem, ...) {
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

tbl_message.table_ncol_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your table should have {expected} column, ",
      "Your table should have {expected} columns, "
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

tbl_message.table_nrow_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Your table should have {expected} row, ",
      "Your table should have {expected} rows, "
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
  problem$msg <- problem$msg %||%
    gettext(
      "Your result should be an array with dimensions {expected}, but it has dimensions {actual}."
    )
  
  problem$actual   <- paste(problem$actual, collapse = " x ")
  problem$expected <- paste(problem$expected, collapse = " x ")
    
  glue::glue_data(problem, problem$msg)
}
