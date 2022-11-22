#' Declare a problem
#'
#' Useful for constructing a small list to communicate the problem that was
#' discovered during checking.
#'
#' @examples
#' problem(
#' 	type = "class",
#' 	expected = "character",
#' 	actual = "numeric",
#' 	expected_length = 1,
#' 	actual_length = 2
#' )
#'
#' @param type A character string, e.g. `column_values` or `table_rows`, that
#'   describes the problem that was discovered.
#' @param expected,actual The expected and actual values. These should be
#'   included when the value is a summary, e.g. `nrow(expected)` or
#'   `length(actual)`. Be careful not to include large amounts of data.
#' @param ... Additional elements to be included in the `problem` object.
#' @param .class The class of the problem. Typically, we expect the problem
#'   class to be `<type>_problem`, but if you are building custom classes you
#'   may set these classes as desired.
#'
#' @return Returns a problem with class `<type>_problem` and the base classes
#'   `tblcheck_problem` and `gradethis_problem`.
#'
#' @family Problem functions
#' @export
problem <- function(
	type,
	expected,
	actual,
	...,
	.class = c(paste0(type, "_problem"), "tblcheck_problem")
) {
	checkmate::assert_string(type, min.chars = 1)
	if (!checkmate::test_character(.class, pattern = "^[[:alpha:]][[:alnum:]_.]*$")) {
		rlang::abort(
			"`.class` must be a character vector of valid R class names",
			class = "error_problem_class"
		)
	}

	problem <- list(type = type)

	if (!missing(expected)) problem$expected <- expected
	if (!missing(actual)) problem$actual <- actual

	problem <- c(problem, list(...))

	if (any(!nzchar(names(problem)))) {
		rlang::abort("Arguments in `...` must be named.")
	}

	structure(
		problem,
		class = unique(c(.class, "gradethis_problem"))
	)
}

return_if_problem <- function(
	problem, prefix = NULL, ..., env = parent.frame()
) {
	if (inherits(problem, "tblcheck_problem")) {
		if (!is.null(prefix)) {
			problem$location <- prefix

			problem_class <- append(
				class(problem), paste0(prefix, "_problem"), after = 1
			)
		} else {
			problem_class <- class(problem)
		}

		# Attributes are dropped by `c()`, so the class must be reintroduced
		problem <- c(problem, ...)
		class(problem) <- unique(problem_class)

		rlang::return_from(env, problem)
	}
}

#' Problem helper functions
#'
#' - `problem_type()` returns a problem's type, or [`NULL`] if the input is
#'   not a problem.
#' - `is_problem()` tests whether an object is a `gradethis` problem.
#' - `is_tblcheck_problem()` tests whether an object is a problem created
#'   by `tblcheck`.
#' - `as_problem()` converts a list to a `tblcheck_problem`.
#'
#' If `type` is specified, `is_problem()` and `is_tblcheck_problem()` test
#' whether an object is a problem of the specified type.
#'
#' @examples
#' problem_type(vec_check(1, "1"))
#' is_problem(vec_check(1, "1"), "vector_class")
#' is_tblcheck_problem(vec_check(1, "1"), "class")
#'
#' @param x An object
#' @param type `[character(1)]`\cr A `problem` type
#'
#' @return `is_problem()` and `is_tblcheck_problem()` return a [logical]
#'   of length 1.
#'   `problem_type()` returns a [character] of length 1.
#'   `as_problem()` returns a `tblcheck_problem`.
#'
#' @family Problem functions
#' @export
problem_type <- function(x) {
	if (is_problem(x)) {
		return(x$type)
	}

	NULL
}

#' @rdname problem_type
#' @export
is_problem <- function(x, type = NULL) {
	if (!inherits(x, "gradethis_problem")) return(FALSE)
	if (is.null(type)) return(TRUE)
	inherits(x, c(type, paste0(type, "_problem")))
}

#' @rdname problem_type
#' @export
is_tblcheck_problem <- function(x, type = NULL) {
	if (!inherits(x, "tblcheck_problem")) return(FALSE)
	if (is.null(type)) return(TRUE)
	# tblcheck problem classes always are "<type>_problem"
	inherits(x, paste0(type, "_problem"))
}

#' @rdname problem_type
#' @export
as_problem <- function(x) {
	checkmate::assert_list(x)

	if (!is.null(x$location) && !is.null(x$type) && is.null(x$.class)) {
		# this is probably a tblcheck problem as a list
		x$.class <- c(
			paste0(c(x$type, x$location), "_problem"),
			"tblcheck_problem",
			"gradethis_problem"
		)
	}

	tryCatch(
		rlang::eval_bare(rlang::call2("problem", !!!x)),
		error_problem_class = function(err) {
			rlang::abort(
				"Please set `.class` for your list, see `?problem()` for details",
				parent = err
			)
		}
	)
}

#' @export
print.tblcheck_problem <- function(x, ...) {
	cat("<tblcheck problem>", format(x, ...) %||% "<no message>", sep = "\n")
	str <- utils::capture.output(utils::str(unclass(x)))[-1]
	cat(sub("^ ", "", str), sep = "\n")
}

#' @export
format.tblcheck_problem <- function(x, ...) {
	problem_message(x, ...)
}
