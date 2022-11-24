#' Checks that two objects have the same classes
#'
#' Checks if `object` and `expected` have the same [class][class()].
#' If the classes differ
#' - `tbl_check_class()` and `vec_check_class()` return a list describing
#'   the problem
#' - `tbl_grade_class()` and `vec_grade_class()` return a failing grade and
#'   informative message with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `class`: The object does not have the expected classes
#'
#' @examples
#' .result <- 1:10
#' .solution <- as.character(1:10)
#' vec_check_class()
#' vec_grade_class()
#'
#' .result <- data.frame(a = 1:10)
#' .solution <- tibble::tibble(a = 1:10)
#' tbl_check_class()
#' tbl_grade_class()
#'
#' .result <- tibble::tibble(a = 1:10, b = a %% 2 == 0)
#' .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a %% 2 == 0), b)
#' tbl_check_class()
#' tbl_grade_class()
#'
#' # Ignore the difference between tibble and data frame
#' .result <- data.frame(a = 1:10)
#' .solution <- tibble::tibble(a = 1:10)
#' tbl_check_class(ignore_class = c("tbl_df", "tbl"))
#' tbl_grade_class(ignore_class = c("tbl_df", "tbl"))
#'
#' # Ignore the difference between integer and double
#' .result <- 1L
#' .solution <- 1
#' vec_check_class(ignore_class = c("integer" = "numeric"))
#' vec_grade_class(ignore_class = c("integer" = "numeric"))
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @param ignore_class `[character()]`\cr A vector of classes to ignore when
#'   finding differences between `object` and `expected`.
#'
#'   If an element is named, differences will only be ignored between the pair
#'   of the element and its name.
#'   For example, `ignore_class = c("integer" = "numeric")` will ignore class
#'   differences only if `object` has class [integer] and `expected` has class
#'   [numeric], or vice versa.
#'
#'   If all the classes of `expected` are included in `ignore_class`,
#'   a `class` problem will never be returned.
#' @inheritParams tbl_check
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check_class()` and
#'   `vec_check_class()` or a [gradethis::fail()] message from
#'   `tbl_grade_class()` and `vec_grade_class()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
tbl_check_class <- function(
	object = .result,
	expected = .solution,
	ignore_class = NULL,
	env = parent.frame()
) {
	if (inherits(object, ".result")) {
		object <- get(".result", env)
	}
	if (inherits(expected, ".solution")) {
		expected <- get(".solution", env)
	}

	obj_class <- class(object)
	exp_class <- class(expected)

	paired <- rlang::names2(ignore_class) != ""

	obj_class_ignored <- setdiff(obj_class, ignore_class[!paired])
	exp_class_ignored <- setdiff(exp_class, ignore_class[!paired])

	if (length(exp_class_ignored) == 0) {
		return(invisible())
	}

	# Replace classes that match named elements of `ignore_class` with the
	# element's name. This allows us to ignore differences between the element
	# class and the name class.
	for (i in seq_along(ignore_class[paired])) {
		obj_class_ignored[obj_class_ignored == ignore_class[paired][[i]]] <-
			names(ignore_class[paired])[[i]]
		exp_class_ignored[exp_class_ignored == ignore_class[paired][[i]]] <-
			names(ignore_class[paired])[[i]]
	}

	if (!setequal(obj_class_ignored, exp_class_ignored)) {
		problem(
			"class",
			expected,
			object,
			# Object lengths are stored so the correct pluralization
			# can be applied in problem_message.class_problem()
			expected_length = length(expected),
			actual_length = length(object)
		)
	}
}

#' @rdname tbl_check_class
#' @export
vec_check_class <- tbl_check_class

#' @rdname tbl_check_class
#' @export
tbl_grade_class <- function(
	object = .result,
	expected = .solution,
	ignore_class = NULL,
	env = parent.frame(),
	...
) {
	problem_grade(
		tbl_check_class(object, expected, ignore_class, env),
		env = env,
		...
	)
}

#' @rdname tbl_check_class
#' @export
vec_grade_class <- tbl_grade_class

#' @export
problem_message.class_problem <- function(problem, ...) {
	if (is_problem(problem, "column")) {
		problem$msg <- problem$msg %||%
			"Your `{column}` column should be {expected}, but it is {actual}."
	} else if (is_problem(problem, "table")) {
		problem$msg <- problem$msg %||%
			"Your table should be {expected}, but it is {actual}."
	}

	problem$msg <- problem$msg %||%
		"Your result should be {expected}, but it is {actual}."

	hinted_class_message <- hinted_class_message(problem$actual, problem$expected)
	if (!is.null(hinted_class_message)) return(hinted_class_message)

	problem$expected <- friendly_class(problem$expected)
	problem$actual <- friendly_class(problem$actual)

	glue::glue_data(problem, problem$msg)
}
