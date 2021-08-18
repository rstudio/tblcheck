#' Check that the groups of two object are the same
#'
#' Checks if `object` and `expected` have the same [groups][dplyr::group_by()].
#' If the groups differ
#' - `tbl_check_groups()` returns a list describing the problem
#' - `tbl_grade_groups()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `groups`: The object has groups that are not expected,
#'   or is missing groups that are expected.
#'
#' @inheritParams tbl_check_names
#'
#' @return If there are any issues, a [list] from `tbl_check_groups()` or a
#'   [gradethis::fail()] message from `tbl_grade_groups()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' 
#' @examples 
#' .result <- dplyr::group_by(tibble::tibble(a = 1:10, b = 11:20), a)
#' .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = 11:20), b)
#' tbl_check_groups()
#' tbl_grade_groups()
tbl_check_groups <- function(
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
  
  assert_internally({
    checkmate::assert_data_frame(object)
    checkmate::assert_data_frame(expected)
  })
  
  groups_exp <- group_vars(expected)
  groups_obj <- group_vars(object)
  
  if (!identical(groups_exp, groups_obj)) {
    return_if_problem(
      problem(
        "groups", 
        missing = setdiff(groups_exp, groups_obj),
        unexpected = setdiff(groups_obj, groups_exp)
      ),
      prefix = "table"
    )
  }
}

group_vars <- function(x) {
  setdiff(names(attr(x, "groups")), ".rows")
}

#' @rdname tbl_check_groups
#' @export
tbl_grade_groups <- function(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(
      tbl_check_groups(object, expected, envir = envir),
      max_diffs = max_diffs
    )
  )
}

tbl_message.groups_problem <- function(problem, max_diffs = 3, ...) {
  missing_msg <- if (!is.null(problem$missing)) {
    missing_groups <- combine_words_with_more(
      problem$missing, max_diffs
    )
    
    "Your table should be grouped by {missing_groups}. "
  } else {
    ""
  }
  
  unexpected_msg <- if (!is.null(problem$unexpected)) {
    unexpected_groups <- combine_words_with_more(
      problem$unexpected, max_diffs, and = " or "
    )
    
    "Your table should not be grouped by {unexpected_groups}. "
  } else {
    ""
  }
  
  return_fail(glue::glue(missing_msg, unexpected_msg), problem = problem)
}
