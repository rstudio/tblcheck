#' Check that the rows and columns of two tables are the same
#'
#' Test if two tables are equivalent using the same process as [tbl_check()].
#' Unlike [tbl_check()], which returns either a [problem] object or [`NULL`],
#' `tbl_equal()` returns either [`TRUE`] or [`FALSE`].
#'
#' @inheritParams tbl_check
#' @return A [`TRUE`] or [`FALSE`] value.
#' @export
#'
#' @examples
#' tbl_equal(
#'   data.frame(a = 1:10, b = 11:20),
#'   data.frame(b = 11:20, a = 1:10)
#' )
tbl_equal <- function(
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
	check_column_levels = check_columns,
	check_column_values = check_columns,
	tolerance = sqrt(.Machine$double.eps),
	env = parent.frame()
) {
	is.null(
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
			check_column_levels = check_column_levels,
			check_column_values = check_column_values,
			tolerance = tolerance,
			env = env
		)
	)
}
