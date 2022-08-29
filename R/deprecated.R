#' Check that the rows and columns of two tables are the same
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tbl_check_table()` and `tbl_grade_table()` were renamed to [tbl_check()]
#' and [tbl_grade()].
#'
#' @keywords internal
#' @export
tbl_check_table <- function(
	object = .result,
	expected = .solution,
	check_class = TRUE,
	check_names = TRUE,
	check_column_order = FALSE,
	check_dimensions = TRUE,
	check_groups = TRUE,
	check_columns = TRUE,
	check_column_class = check_columns,
	check_column_values = check_columns,
	env = parent.frame()
) {
	lifecycle::deprecate_warn("0.1.0", "tbl_check_table()", "tbl_check()")

	tbl_check(
		object = object,
		expected = expected,
		check_class = check_class,
		check_names = check_names,
		check_column_order = check_column_order,
		check_dimensions = check_dimensions,
		check_groups = check_groups,
		check_columns = check_columns,
		check_column_class = check_column_class,
		check_column_values = check_column_values,
		env = env
	)
}

#' @rdname tbl_check_table
#' @export
tbl_grade_table <- function(
	object = .result,
	expected = .solution,
	max_diffs = 3,
	check_class = TRUE,
	check_names = TRUE,
	check_column_order = FALSE,
	check_dimensions = TRUE,
	check_groups = TRUE,
	check_columns = TRUE,
	check_column_class = check_columns,
	check_column_values = check_columns,
	env = parent.frame(),
	...
) {
	lifecycle::deprecate_warn("0.1.0", "tbl_grade_table()", "tbl_grade()")

	tbl_grade(
		object = object,
		expected = expected,
		max_diffs = max_diffs,
		check_class = check_class,
		check_names = check_names,
		check_column_order = check_column_order,
		check_dimensions = check_dimensions,
		check_groups = check_groups,
		check_columns = check_columns,
		check_column_class = check_column_class,
		check_column_values = check_column_values,
		env = env,
		...
	)
}

#' Check that the rows and columns of two tables are the same
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `vec_check_vector()` and `vec_grade_vector()` were renamed to [vec_check()]
#' and [vec_grade()].
#'
#' @keywords internal
#' @export
vec_check_vector <- function(
	object = .result,
	expected = .solution,
	check_class = TRUE,
	check_length = TRUE,
	check_levels = TRUE,
	check_values = TRUE,
	check_names = TRUE,
	env = parent.frame()
) {
	lifecycle::deprecate_warn("0.1.0", "vec_check_vector()", "vec_check()")

	vec_check(
		object = object,
		expected = expected,
		check_class = check_class,
		check_length = check_length,
		check_values = check_values,
		check_names = check_names,
		env = env
	)
}

#' @rdname vec_check_vector
#' @export
vec_grade_vector <- function(
	object = .result,
	expected = .solution,
	max_diffs = 3,
	check_class = TRUE,
	check_length = TRUE,
	check_values = TRUE,
	check_names = TRUE,
	env = parent.frame(),
	...
) {
	lifecycle::deprecate_warn("0.1.0", "vec_check_vector()", "vec_check()")

	vec_grade(
		object = object,
		expected = expected,
		max_diffs = max_diffs,
		check_class = check_class,
		check_length = check_length,
		check_values = check_values,
		check_names = check_names,
		env = env,
		...
	)
}

#' Deprecated Generics
#'
#' These generics are now deprecated.
#'
#' @keywords internal
#' @name deprecated-methods
NULL

#' @describeIn deprecated-methods is now [problem_grade()].
#' @export
tblcheck_grade <- function(problem, ...) {
	lifecycle::deprecate_soft("0.2.0", "tblcheck_grade()", "problem_grade()")
	problem_grade(problem, ...)
}

#' @describeIn deprecated-methods is now [problem_message()].
#' @export
tblcheck_message <- function(problem, ...) {
	lifecycle::deprecate_soft("0.2.0", "tblcheck_message()", "problem_message()")
	problem_message(problem, ...)
}
