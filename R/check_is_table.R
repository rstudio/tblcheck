#' Checks that an object is a table
#'
#' Checks if `object` inherits the [data.frame] class.
#' If the not
#' - `tbl_check_is_table()` returns a list describing the problem
#' - `tbl_grade_is_table()` returns a failing grade and
#'   informative message with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `not_table`: The object is not a table
#'
#' @examples
#' .result <- data.frame(a = 1:10)
#' tbl_check_is_table()
#' tbl_grade_is_table()
#'
#' .result <- tibble::tibble(a = 1:10)
#' tbl_check_is_table()
#' tbl_grade_is_table()
#'
#' .result <- list(a = 1:10)
#' tbl_check_is_table()
#' tbl_grade_is_table()
#' @param object An object to be compared to `expected`.
#' @inheritParams tbl_check
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check_is_table()` or a
#'   [gradethis::fail()] message from `tbl_grade_is_table()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
tbl_check_is_table <- function(
	object = .result,
	env = parent.frame()
) {
	if (inherits(object, ".result")) {
		object <- get(".result", env)
	}

	if (!inherits(object, "data.frame")) {
		problem("not_table", actual = object, actual_length = length(object))
	}
}

#' @rdname tbl_check_is_table
#' @export
tbl_grade_is_table <- function(
	object = .result,
	env = parent.frame(),
	...
) {
	problem_grade(
		tbl_check_is_table(object, env),
		env = env,
		...
	)
}

#' @export
problem_message.not_table_problem <- function(problem, ...) {
	problem$msg <- problem$msg %||%
		"Your result should be a table, but it is {actual}."

	problem$actual <- friendly_class(problem$actual)

	glue::glue_data(problem, problem$msg)
}
