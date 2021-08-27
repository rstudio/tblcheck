#' Check that the two vectors have all the same number of characters
#'
#' @description
#'
#' Checks if every element in `object` and `expected` have the same
#' [number of characters][nchar()].
#' If the number of characters differ
#' - `tbl_check_nchar()` returns a list describing the problem
#' - `tbl_grade_nchar()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' Every element of `expected` must have the same number of characters.
#' If `expected` does not have a consistent number of characters, no grade will
#' be produced.
#' 
#' If `object` and `expected` are not both [character] vectors, they will be
#' passed to [tbl_check_class()] for grading.
#' 
#' @section Problems:
#' 
#' 1. `inconsistent_nchar`: every element of `object` does not have the same
#'   number of characters
#' 1. `nchar`: every element of `object` does not have the same number of
#'   characters as `expected`
#'   
#' Additional problems may be produced by [tbl_check_class()]
#'
#' @inheritParams tbl_check_class
#'
#' @return If there are any issues, a [list] from `tbl_check_nchar()` or a
#'   [gradethis::fail()] message from `tbl_grade_nchar()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples
#' .result <- letters
#' .solution <- strrep(letters, 2)
#' tbl_check_nchar()
#' tbl_grade_nchar()
#' 
#' .result <- strrep(letters, 1:26)
#' .solution <- strrep(letters, 2)
#' tbl_check_nchar()
#' tbl_grade_nchar()
tbl_check_nchar <- function(
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
  
  return_if_problem(tbl_check_class(object, expected, envir = envir))
  
  obj_nchar <- unique(nchar(object))
  exp_nchar <- unique(nchar(expected))
  
  if (identical(obj_nchar, exp_nchar)) {
    return(invisible())
  }
  
  if (!length(exp_nchar) == 1) {
    warning("`expected` does not have a consistent number of characters.")
    return()
  }
  
  if (length(obj_nchar) > 1) {
    return(problem("inconsistent_nchar", exp_nchar))
  }
  
  return(problem("nchar", exp_nchar, obj_nchar))
}

#' @rdname tbl_check_nchar
#' @export
tbl_grade_nchar <- function(
  object = .result, 
  expected = .solution,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(tbl_check_nchar(object, expected, envir = envir))
  )
}

tbl_message.nchar_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Every element of your result should be {expected} character long, ",
      "Every element of your result should be {expected} characters long, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    ngettext(
      problem$actual,
      "but it is all {actual} character long.",
      "but it is all {actual} characters long."
    )
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}

tbl_message.inconsistent_nchar_problem <- function(problem, ...) {
  problem$exp_msg <- problem$exp_msg %||% 
    ngettext(
      problem$expected,
      "Every element of your result should be {expected} character long, ",
      "Every element of your result should be {expected} characters long, "
    )
  
  problem$obj_msg <- problem$obj_msg %||%
    gettext("but it does not have a consistent character length.")
  
  glue::glue_data(problem, problem$exp_msg, problem$obj_msg)
}
