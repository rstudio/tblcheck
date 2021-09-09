#' Checks that a column is identical across two tables
#'
#' Checks if `column` exists in `object`.
#' If the column does not exist
#' - `tbl_check_missing()` returns a list describing the problem
#' - `tbl_grade_missing()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `missing`: The `column` doesn't appear in the `object`
#'
#' @param column `[character(1)]`\cr The name of the column to check.
#' @inheritParams tbl_check_column
#'
#' @return If there are any issues, a [list] from `tbl_check_missing()` or a
#'   [gradethis::fail()] message from `tbl_grade_missing()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples 
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:10, b = 11:20)
#' tbl_check_missing("b")
#' tbl_grade_missing("b")
tbl_check_missing <- function(column, object = .result, env = parent.frame()) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  
  assert_internally({
    checkmate::assert_character(column, len = 1, any.missing = FALSE)
    checkmate::assert_data_frame(object)
  })
  
  if (!column %in% names(object)) {
    return(problem("missing", column))
  }
}

#' @rdname tbl_check_missing
#' @export
tbl_grade_missing <- function(column, object = .result, env = parent.frame()) {
  return_if_graded(
    tbl_grade(
      tbl_check_column(column = column, object = object, env = env), 
      env = env
    )
  )
}

tbl_message.missing_problem <- function(problem, ...) {
  glue::glue_data(problem, "Your table should have a column named `{expected}`.")
}
