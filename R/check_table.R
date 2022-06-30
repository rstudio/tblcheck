#' Check that the rows and columns of two tables are the same
#'
#' @description
#' Checks for differences between `object` and `expected` in the following order:
#' 1. Check table class with [tbl_check_class()]
#' 1. Check column names with [tbl_check_names()]
#' 1. Check number of rows and columns with [tbl_check_dimensions()]
#' 1. Check [groups][dplyr::group_by()] with [tbl_check_groups()]
#' 1. Check that each column is the same with [tbl_check_column()]
#'
#' If the tables differ
#' - `tbl_check()` returns a list describing the problem
#' - `tbl_grade()` returns a failing grade and informative message
#' with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `class`: The table does not have the expected classes.
#' 1. `not_table`: `object` does not inherit the [data.frame] class.
#' 1. `names`: The table has column names that are not expected,
#'   or is missing names that are expected.
#' 1. `names_order`: The table has the same column names as expected,
#'   but in a different order.
#' 1. `ncol`: The table doesn't have the expected number of columns.
#' 1. `nrow`: The table doesn't have the expected number of rows.
#' 1. `groups`: The table has [groups][dplyr::group_by()] that are
#'   not expected, or is missing groups that are expected.
#'
#' Additional problems may be produced by [tbl_check_column()].
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param cols [[`tidy-select`][tidyselect::language]]\cr A selection of columns
#'   to compare between `object` and `expected`. Differences in other columns
#'   will be ignored. If [`NULL`], the default, all columns will be checked.
#' @param max_diffs `[numeric(1)]`\cr The maximum number of mismatched values to
#'   display in an informative failure message.
#'   Passed to [tbl_check_names()] to determine the number of mismatched column
#'   names to display and the `n_values` argument of [tbl_check_column()] to
#'   determine the number of mismatched column values to display.
#'   Defaults to 3.
#' @param check_class `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same classes with [tbl_check_class()].
#' @inheritParams tbl_check_class
#' @param check_names `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same column names with [tbl_check_names()].
#' @param check_column_order `[logical(1)]`\cr Whether to check that the columns
#'   of  `object` are in the same order as `expected` with [tbl_check_names()].
#'   Defaults to `FALSE`.
#' @param check_dimensions `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same number of rows and columns
#'   with [tbl_check_dimensions()].
#' @param check_groups `[logical(1)]`\cr Whether to check that `object` and
#'   `expected` have the same [groups][dplyr::group_by()]
#'   with [dplyr::group_vars()].
#' @param check_columns `[logical(1)]`\cr Whether to check that all columns
#'   have the same contents with [tbl_check_column()].
#' @param check_column_class `[logical(1)]`\cr Whether to check that each
#'   columns has the same class in `object` and `expected`.
#' @param check_column_values `[logical(1)]`\cr Whether to check that each
#'   column has the same values in `object` and `expected`.
#' @inheritParams vec_check_values
#' @param env The environment in which to find `.result` and `.solution`.
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check()` or a
#'   [gradethis::fail()] message from `tbl_grade()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#'
#' @examples
#' .result <- data.frame(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:10, b = 11:20)
#' tbl_check()
#' tbl_grade()
#'
#' .result <- tibble::tibble(a = 1:10, b = a, c = a, d = a, e = a, f = a)
#' .solution <- tibble::tibble(z = 1:10, y = z, x = z, w = z, v = z, u = z)
#' tbl_check()
#' tbl_grade()
#' tbl_grade(max_diffs = 5)
#' tbl_grade(max_diffs = Inf)
#'
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 1:11, b = 12:22)
#' tbl_check()
#' tbl_grade()
#'
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = letters[1:10], b = letters[11:20])
#' tbl_check()
#' tbl_grade()
#'
#' .result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
#' .solution <- tibble::tibble(a = 1:10, b = 11:20)
#' tbl_check(cols = any_of(names(.solution)))
#' tbl_grade(cols = any_of(names(.solution)))
#'
#' .result <- tibble::tibble(a = 1:10, b = 11:20)
#' .solution <- tibble::tibble(a = 11:20, b = 1:10)
#' tbl_check()
#' tbl_grade()
#' tbl_grade(max_diffs = 5)
#' tbl_grade(max_diffs = Inf)
#'
#' .result <- tibble::tibble(a = 1:10, b = rep(1:2, 5))
#' .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = rep(1:2, 5)), b)
#' tbl_check()
#' tbl_grade()
#' tbl_grade(check_groups = FALSE)
tbl_check <- function(
	object = .result,
	expected = .solution,
	cols = NULL,
	check_class = TRUE,
	ignore_class = NULL,
	check_names = TRUE,
	check_column_order = FALSE,
	check_dimensions = TRUE,
	check_groups = TRUE,
	check_columns = TRUE,
	check_column_class = check_columns,
	check_column_values = check_columns,
	tolerance = sqrt(.Machine$double.eps),
	env = parent.frame()
) {
	if (inherits(object, ".result")) {
		object <- get(".result", env)
	}
	if (inherits(expected, ".solution")) {
		expected <- get(".solution", env)
	}

	return_if_internal_problem({
		checkmate::assert_logical(check_class,         any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_names,         any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_dimensions,    any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_groups,        any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_columns,       any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_column_class,  any.missing = FALSE, len = 1)
		checkmate::assert_logical(check_column_values, any.missing = FALSE, len = 1)
		checkmate::assert_data_frame(expected)
	})

	# check table class ----
	if (check_class) {
		if (!check_groups) {
			ignore_class <- c(ignore_class, "grouped_df")
		}

		return_if_problem(
			tbl_check_class(object, expected, ignore_class),
			prefix = "table"
		)
	} else (
		return_if_problem(
			tbl_check_is_table(object),
			prefix = "table"
		)
	)

	# filter columns in object and expected ----
	cols <- rlang::enexpr(cols)

	if (!is.null(cols)) {
		object <- object[tidyselect::eval_select(cols, object)]
		expected <- expected[tidyselect::eval_select(cols, expected)]
	}

	# check column names ----
	if (check_names) {
		return_if_problem(
			tbl_check_names(object, expected, check_order = check_column_order),
			prefix = "table"
		)
	}

	# check dimensions ----
	if (check_dimensions) {
		return_if_problem(
			tbl_check_dimensions(
				object, expected,
				# Don't check number of columns if a subset of columns was specified
				# or if names were already checked
				check_ncol = is.null(cols) && !check_names
			),
			prefix = "table"
		)
	}

	# check groups ----
	if (check_groups) {
		return_if_problem(
			tbl_check_groups(object, expected),
			prefix = "table"
		)
	}

	# check column contents ----
	if (check_columns) {
		for (column in names(expected)) {
			return_if_problem(
				tbl_check_column(
					column = column,
					object = object,
					expected = expected,
					check_class = check_column_class,
					ignore_class = ignore_class,
					check_values = check_column_values,
					tolerance = tolerance,
					check_length = FALSE
				)
			)
		}
	}
}

#' @rdname tbl_check
#' @export
tbl_grade <- function(
	object = .result,
	expected = .solution,
	cols = NULL,
	max_diffs = 3,
	check_class = TRUE,
	ignore_class = NULL,
	check_names = TRUE,
	check_column_order = FALSE,
	check_dimensions = TRUE,
	check_groups = TRUE,
	check_columns = TRUE,
	check_column_class = check_columns,
	check_column_values = check_columns,
	tolerance = sqrt(.Machine$double.eps),
	env = parent.frame(),
	...
) {
	tblcheck_grade(
		tbl_check(
			object = object,
			expected = expected,
			cols = !!rlang::enexpr(cols),
			check_class = check_class,
			ignore_class = ignore_class,
			check_names = check_names,
			check_column_order = check_column_order,
			check_dimensions = check_dimensions,
			check_groups = check_groups,
			check_columns = check_columns,
			check_column_class = check_column_class,
			check_column_values = check_column_values,
			tolerance = tolerance,
			env = env
		),
		max_diffs = max_diffs,
		env = env,
		...
	)
}
