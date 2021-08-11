#' Check that the length of two object are the same
#'
#' Checks if `object` and `expected` have the same [length][length()].
#' If the names differ
#' - `tbl_check_names()` returns a list describing the problem
#' - `tbl_grade_names()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `length`: `object` and `expected` have different lengths.
#' 2. `ncol`: `object` and `expected` have a different number of columns.
#' 3. `nrow`: `object` and `expected` have a different number of rows
#'
#' @inheritParams tbl_check_class
#' @param dimension `[character(1)]`\cr The dimension on which to compare the
#'   lengths of `object` and `expected`.
#'   One of `"length"`, `"ncol"`, or "`nrow`".
#'   Defaults to `"length"`.
#'
#' @return If there are any issues, a [list] from `tbl_check_length()` or a
#'   [gradethis::fail()] message from `tbl_grade_length()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export

tbl_check_length <- function(
  object = .result,
  expected = .solution,
  dimension = c("length", "ncol", "nrow"), 
  envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  dimension <- match.arg(dimension)
  dimension_fn <- methods::getFunction(dimension)
  
  assert_internally({
    checkmate::assert_string(dimension)
    checkmate::assert_function(dimension_fn)
  })
  
  obj_length <- dimension_fn(object)
  exp_length <- dimension_fn(expected)
  
  if (!identical(obj_length, exp_length)) {
    return(problem(dimension, exp_length, obj_length))
  }
}

#' @rdname tbl_check_length
#' @export

tbl_grade_length <- function(
  object = .result, 
  expected = .solution,
  dimension = c("length", "ncol", "nrow"),
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_length(
        object, expected, dimension = match.arg(dimension), envir = envir
      )
    )
  )
}

tbl_message_length <- function(problem, ...) {
  exp_length  <- problem$expected
  obj_length  <- problem$actual
  column_name <- problem$column
  
  message <- if (!is.null(column_name)) {
    glue::glue(
      ngettext(
        exp_length,
        "Your `{column_name}` column should contain {exp_length} value, ",
        "Your `{column_name}` column should contain {exp_length} values, "
      ),
      ngettext(
        obj_length,
        "but it has {obj_length} value.",
        "but it has {obj_length} values."
      )
    )
  } else {
    glue::glue(
      ngettext(
        exp_length,
        "Your result should contain {exp_length} value, ",
        "Your result should contain {exp_length} values, "
      ),
      ngettext(
        obj_length,
        "but it has {obj_length} value.",
        "but it has {obj_length} values."
      )
    )
  }
  
  return_fail(message, problem = problem)
}

tbl_message_ncol <- function(problem, ...) {
  exp_length <- problem$expected
  obj_length <- problem$actual
  
  message <- glue::glue(
    ngettext(
      exp_length,
      "Your table should have {exp_length} column, ",
      "Your table should have {exp_length} columns, "
    ),
    ngettext(
      obj_length,
      "but it has {obj_length} column.",
      "but it has {obj_length} columns."
    )
  )
  
  return_fail(message, problem = problem)
}

tbl_message_nrow <- function(problem, ...) {
  exp_length <- problem$expected
  obj_length <- problem$actual
  
  message <- glue::glue(
    ngettext(
      exp_length,
      "Your table should have {exp_length} row, ",
      "Your table should have {exp_length} rows, "
    ),
    ngettext(
      obj_length,
      "but it has {obj_length} row.",
      "but it has {obj_length} rows."
    )
  )
  
  return_fail(message, problem = problem)
}
