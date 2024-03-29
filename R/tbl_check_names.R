#' Check that the names of two object are the same
#'
#' Checks if `object` and `expected` have the same [names][names()].
#' If the names differ
#' - `tbl_check_names()` and `vec_check_names()` returns a list describing
#'   the problem
#' - `tbl_grade_names()` and `vec_grade_names()` returns a failing grade and
#'   informative message with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `names`: The object has names that are not expected,
#'   or is missing names that are expected.
#' 1. `names_order`: The object has the same names as expected,
#'   but in a different order.
#'
#' @inheritParams tbl_check_class
#' @param check_order `[logical(1)]`\cr Whether to check that the names of
#'   `object` and `expected` are in the same order.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of missing and/or
#'   unexpected names to include in an informative failure message.
#'   Defaults to 3.
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check_names()` and
#'   `vec_check_names()` or a [gradethis::fail()] message from
#'   `tbl_grade_names()` and `vec_grade_names()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#'
#' @examples
#' .result <- c(1, 2, 3, 4, 5, 6, 7)
#' .solution <- c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7)
#' vec_check_names()
#' vec_grade_names()
#' vec_grade_names(max_diffs = 5)
#' vec_grade_names(max_diffs = Inf)
#'
#' .result <- tibble::tibble(a = 1:5, b = 6:10, c = 11:15)
#' .solution <- tibble::tibble(a = 1:5, x = 6:10, y = 11:15)
#' tbl_check_names()
#' tbl_grade_names()
tbl_check_names <- function(
	object = .result,
	expected = .solution,
	check_order = TRUE,
	env = parent.frame()
) {
	if (inherits(object, ".result")) {
		object <- get(".result", env)
	}
	if (inherits(expected, ".solution")) {
		expected <- get(".solution", env)
	}

	# We use `as.character()` to ensure the result is always a character vector.
	# For an unnamed object, this will return `character(0)`.
	# Otherwise, it would return `NULL`.
	names_exp <- as.character(names(expected))
	names_obj <- as.character(names(object))

	same_when_sorted <- identical(sort(names_exp), sort(names_obj))

	if (!check_order && same_when_sorted) {
		return(invisible())
	}

	if (identical(names_exp, names_obj)) {
		return(invisible())
	}

	problem <-
		if (same_when_sorted) {
			problem("names_order", names_exp, names_obj)
		} else {
			problem(
				"names",
				missing = setdiff(names_exp, names_obj),
				unexpected = setdiff(names_obj, names_exp)
			)
		}

	if (is.data.frame(object) && is.data.frame(expected)) {
		return_if_problem(problem, prefix = "table")
	}

	return(problem)
}

#' @rdname tbl_check_names
#' @export
vec_check_names <- tbl_check_names

#' @rdname tbl_check_names
#' @export
tbl_grade_names <- function(
	object = .result,
	expected = .solution,
	max_diffs = 3,
	check_order = TRUE,
	env = parent.frame(),
	...
) {
	problem_grade(
		tbl_check_names(object, expected, check_order = check_order, env = env),
		max_diffs = max_diffs,
		env = env,
		...
	)
}

#' @rdname tbl_check_names
#' @export
vec_grade_names <- tbl_grade_names

#' @export
problem_message.names_problem <- function(problem, max_diffs = 3, ...) {
	if (is_problem(problem, "column")) {
		problem$missing_msg <- problem$missing_msg %||%
			ngettext(
				length(problem$missing),
				"Your `{column}` column should have the name {missing}. ",
				"Your `{column}` column should have the names {missing}. "
			)

		problem$unexpected_msg <- problem$unexpected_msg %||%
			ngettext(
				length(problem$unexpected),
				"Your `{column}` column should not have the name {unexpected}.",
				"Your `{column}` column should not have the names {unexpected}."
			)
	} else if (is_problem(problem, "table")) {
		problem$missing_msg <- problem$missing_msg %||%
			ngettext(
				length(problem$missing),
				"Your table should have a column named {missing}. ",
				"Your table should have columns named {missing}. "
			)

		problem$unexpected_msg <- problem$unexpected_msg %||%
			ngettext(
				length(problem$unexpected),
				"Your table should not have a column named {unexpected}.",
				"Your table should not have columns named {unexpected}."
			)
	}

	problem$missing_msg <- problem$missing_msg %||%
		ngettext(
			length(problem$missing),
			"Your result should have the name {missing}. ",
			"Your result should have the names {missing}. "
		)

	problem$unexpected_msg <- problem$unexpected_msg %||%
		ngettext(
			length(problem$unexpected),
			"Your result should not have the name {unexpected}.",
			"Your result should not have the names {unexpected}."
		)

	if (length(problem[["missing"]]) > 0) {
		problem$missing <- combine_words_with_more(problem$missing, max_diffs)
	} else {
		problem$missing_msg <- ""
	}

	if (length(problem[["unexpected"]]) > 0) {
		problem$unexpected <- combine_words_with_more(problem$unexpected, max_diffs, and = " or ")
	} else {
		problem$unexpected_msg <- ""
	}

	glue::glue_data(problem, paste0(problem$missing_msg, problem$unexpected_msg))
}

#' @export
problem_message.names_order_problem <- function(problem, max_diffs = 3, ...) {
	problem$n_values <- min(
		max(length(problem$expected), length(problem$actual)),
		max_diffs
	)

	if (is_problem(problem, "column")) {
		problem$msg <- problem$msg %||%
			"Your `{column}` column's names were not in the expected order. "
	} else if (is_problem(problem, "table")) {
		problem$msg <- problem$msg %||%
			"Your table's columns were not in the expected order. "
	}

	problem$msg <- problem$msg %||%
		"Your result's names were not in the expected order. "

	if (
		identical(
			problem$expected[seq_len(problem$n_values)],
			problem$actual[seq_len(problem$n_values)]
		)
	) {
		return(glue::glue_data(problem, problem$msg))
	}

	problem$expected <- knitr::combine_words(
		md_code(problem$expected[seq_len(problem$n_values)])
	)
	problem$actual <- knitr::combine_words(
		md_code(problem$actual[seq_len(problem$n_values)])
	)

	if (is_problem(problem, "column")) {
		problem$exp_msg <- problem$exp_msg %||%
			ngettext(
				problem$n_values,
				"The first name of your `{column}` column should be {expected}.",
				"The first {n_values} names of your `{column}` column should be {expected}."
			)
	} else if (is_problem(problem, "table")) {
		problem$exp_msg <- problem$exp_msg %||%
			ngettext(
				problem$n_values,
				"The first column of your table should be {expected}.",
				"The first {n_values} columns of your table should be {expected}."
			)
	}

	problem$exp_msg <- problem$exp_msg %||%
		ngettext(
			problem$n_values,
			"The first name of your result should be {expected}.",
			"The first {n_values} names of your result should be {expected}."
		)

	glue::glue_data(problem, problem$msg, problem$exp_msg)
}
