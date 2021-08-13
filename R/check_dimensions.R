#' Check that the length of two object are the same
#'
#' Checks if `object` and `expected` have the same [dimenisons][dim()].
#' If the names differ
#' - `tbl_check_dimensions()` returns a list describing the problem
#' - `tbl_grade_dimensions()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `dimensions`: `object` and `expected` have different dimensions
#'
#' @inheritParams tbl_check_class
#'
#' @return If there are any issues, a [list] from `tbl_check_dimensions()` or a
#'   [gradethis::fail()] message from `tbl_grade_dimensions()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
tbl_check_dimensions <- function(
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
  
  obj_dim <- dim(object) %||% length(object)
  exp_dim <- dim(expected) %||% length(expected)
  
  if (!identical(obj_dim, exp_dim)) {
    return(problem("dimensions", exp_dim, obj_dim))
  }
}

#' @rdname tbl_check_dimensions
#' @export
tbl_grade_dimensions <- function(
  object = .result, 
  expected = .solution,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(tbl_check_dimensions(object, expected, envir = envir))
  )
}

tbl_message_dimensions <- function(problem, ...) {
  exp_dim     <- problem$expected
  exp_n_dim   <- length(exp_dim)
  obj_dim     <- problem$actual
  obj_n_dim   <- length(obj_dim)
  column_name <- problem$column
  
  message <- if (!identical(obj_n_dim, exp_n_dim)) {
    glue::glue(
      ngettext(
        exp_n_dim,
        "Your result should have {exp_n_dim} dimension, ",
        "Your result should have {exp_n_dim} dimensions, "
      ),
      ngettext(
        obj_n_dim,
        "but it has {obj_n_dim} dimension.",
        "but it has {obj_n_dim} dimensions."
      )
    )
  } else if (length(exp_dim) == 1) {
    if (!is.null(column_name)) {
      glue::glue(
        ngettext(
          exp_dim,
          "Your `{column_name}` column should contain {exp_dim} value, ",
          "Your `{column_name}` column should contain {exp_dim} values, "
        ),
        ngettext(
          obj_dim,
          "but it has {obj_dim} value.",
          "but it has {obj_dim} values."
        )
      )
    } else {
      glue::glue(
        ngettext(
          exp_dim,
          "Your result should contain {exp_dim} value, ",
          "Your result should contain {exp_dim} values, "
        ),
        ngettext(
          obj_dim,
          "but it has {obj_dim} value.",
          "but it has {obj_dim} values."
        )
      )
    }
  } else if (length(exp_dim) == 2) {
    obj_rows <- obj_dim[[1]]
    exp_rows <- exp_dim[[1]]
    obj_cols <- obj_dim[[2]]
    exp_cols <- exp_dim[[2]]
    
    if (!identical(obj_cols, exp_cols)) {
      glue::glue(
        ngettext(
          exp_cols,
          "Your table should have {exp_cols} column, ",
          "Your table should have {exp_cols} columns, "
        ),
        ngettext(
          obj_cols,
          "but it has {obj_cols} column.",
          "but it has {obj_cols} columns."
        )
      )
    } else if (!identical(obj_rows, exp_rows)) {
      glue::glue(
        ngettext(
          exp_rows,
          "Your table should have {exp_rows} row, ",
          "Your table should have {exp_rows} rows, "
        ),
        ngettext(
          obj_rows,
          "but it has {obj_rows} row.",
          "but it has {obj_rows} rows."
        )
      )
    } 
  } else {
    obj_dim_str <- paste(obj_dim, collapse = " x ")
    exp_dim_str <- paste(exp_dim, collapse = " x ")
    
    glue::glue(
      "Your table should be an arry with dimensions {exp_dim_str}, ",
      "but it has dimensions {obj_dim_str}."
    )
  }
  
  return_fail(message, problem = problem)
}
