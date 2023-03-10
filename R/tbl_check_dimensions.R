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
#' @param check_ncol `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same number of columns.
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
	check_ncol = TRUE,
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
		return(
			problem(
				"length", expected, object,
				expected_length = exp_dim,
				actual_length = obj_dim
			)
		)
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
vec_check_length <- tbl_check_dimensions

#' @rdname tbl_check_dimensions
#' @export
tbl_grade_dimensions <- function(
	object = .result,
	expected = .solution,
	check_ncol = TRUE,
	env = parent.frame(),
	...
) {
	problem_grade(
		tbl_check_dimensions(object, expected, check_ncol = check_ncol, env = env),
		env = env,
		...
	)
}

#' @rdname tbl_check_dimensions
#' @export
vec_grade_dimensions <- tbl_grade_dimensions

#' @rdname tbl_check_dimensions
#' @export
vec_grade_length <- tbl_grade_dimensions

#' @export
problem_message.dimensions_n_problem <- function(problem, ...) {
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

#' @export
problem_message.length_problem <- function(problem, ...) {
	problem$value_msg <- ""

	if (is_problem(problem, "column")) {
		problem$exp_msg <- problem$exp_msg %||%
			ngettext(
				problem$expected_length,
				"Your `{column}` column should contain {expected_length} value, ",
				"Your `{column}` column should contain {expected_length} values, "
			)
	}

	problem$exp_msg <- problem$exp_msg %||%
		ngettext(
			problem$expected_length,
			"Your result should contain {expected_length} value, ",
			"Your result should contain {expected_length} values, "
		)

	problem$obj_msg <- problem$obj_msg %||%
		ngettext(
			problem$actual_length,
			"but it has {actual_length} value.",
			"but it has {actual_length} values."
		)

	if ((problem$actual_length - problem$expected_length) %in% 1:3) {
		problem$value <- setdiff(problem$actual, problem$expected)

		if (length(problem$value) <= 3) {
			problem$value_msg <- ngettext(
				length(problem$value),
				" I didn't expect your result to include the value {value}.",
				" I didn't expect your result to include the values {value}."
			)

			problem$value <- knitr::combine_words(md_code(problem$value))
		}
	} else if ((problem$expected_length - problem$actual_length) %in% 1:3) {
		problem$value <- setdiff(problem$expected, problem$actual)

		if (length(problem$value) <= 3) {
			problem$value_msg <- ngettext(
				length(problem$value),
				" I expected your result to include the value {value}.",
				" I expected your result to include the values {value}."
			)

			problem$value <- knitr::combine_words(md_code(problem$value))
		}
	}

	if (!length(problem$value)) {
		# remove values message: obj and exp have the same values but different length
		problem$value_msg <- ""
	}

	glue::glue_data(problem, problem$exp_msg, problem$obj_msg, problem$value_msg)
}

#' @export
problem_message.ncol_problem <- function(problem, ...) {
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

#' @export
problem_message.nrow_problem <- function(problem, ...) {
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

#' @export
problem_message.dimensions_problem <- function(problem, ...) {
	if (is_problem(problem, "column")) {
		problem$msg <- problem$exp_msg %||%
			gettext("Your `{column}` column should be an array with dimensions {expected}, but it has dimensions {actual}.")
	} else if (is_problem(problem, "table")) {
		problem$msg <- problem$exp_msg %||%
			gettext("Your table should be an array with dimensions {expected}, but it has dimensions {actual}.")
	}

	problem$msg <- problem$msg %||%
		gettext("Your result should be an array with dimensions {expected}, but it has dimensions {actual}.")

	problem$actual <- paste(problem$actual, collapse = " x ")
	problem$expected <- paste(problem$expected, collapse = " x ")

	glue::glue_data(problem, problem$msg)
}
