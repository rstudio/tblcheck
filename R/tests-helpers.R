expect_internal_problem <- function(grade, message) {
	suppressMessages({
		testthat::expect_message(grade)
		testthat::expect_length(grade$correct, 0)
		if (is_problem(grade)) {
			testthat::expect_s3_class(grade, "tblcheck_internal_problem")
			testthat::expect_equal(grade$type, "tblcheck_internal")
			testthat::expect_match(as.character(grade$error$message), message)
		} else {
			testthat::expect_match(grade$message, "can't provide feedback")
			testthat::expect_equal(grade$problem$type, "tblcheck_internal")
			testthat::expect_match(as.character(grade$error$message), message)
		}
	})
}

expect_warning <- function(...) {
	suppressWarnings(testthat::expect_warning(...))
}

expect_problem <- function(object, type, expected, actual, ...) {
	expect_s3_class(object, "tblcheck_problem")

	if (!rlang::is_missing(type)) expect_equal(object$type, type)
	if (!rlang::is_missing(expected)) expect_equal(object$expected, expected)
	if (!rlang::is_missing(actual)) expect_equal(object$actual, actual)

	purrr::iwalk(
		list(...),
		function(value, name) expect_equal(object[[name]], value)
	)
}

tblcheck_test_grade <- function(expr, return_all = FALSE) {
	expr <- rlang::enexpr(expr)

	if (identical(expr[[1]], rlang::sym("{"))) {
		expr_setup <- expr[-length(expr)]
		expr_check <- expr[[length(expr)]]
		final_call <- paste(extract_first(expr[[length(expr)]]))
	} else {
		expr_setup <- NULL
		expr_check <- expr
		final_call <- paste(expr[[1]])
	}


	if (
		!grepl(
			"^(tbl|vec|tblcheck)_(check|grade)|^grade_this_(table|column|vector)",
			final_call
		)
	) {
		stop("tblcheck_test_grade() expected a {tblcheck} function as the final expression")
	}

	# Grade returned by check_*(), without calling handlers
	grade_ret <- rlang::eval_bare(expr)

	# Grade returned by check_*(), when using calling handlers
	grade_captured <-
		tryCatch(
			rlang::eval_bare(expr),
			gradethis_graded = identity
		)

	# Grade collected inside grade_this(), but doesn't check extras like hint/encouragement
	ex <- gradethis::mock_this_exercise(.user_code = "NA", .solution_code = "NA")
	# eval the setup expressions into the exercise envir
	# to let tests create .result, .solution, etc. objects directly
	if (!is.null(expr_setup)) {
		rlang::eval_bare(expr_setup, ex)
	}
	grader <- gradethis::grade_this(!!expr_check)
	grade_gradethis <- grader(ex)

	# expect all grades are equal
	testthat::expect_equal(grade_ret, grade_captured)
	testthat::expect_equal(grade_ret, grade_gradethis)

	if (!isTRUE(return_all)) {
		return(grade_ret)
	}

	rlang::dots_list(grade_ret, grade_captured, grade_gradethis, .named = TRUE)
}

extract_first <- function(x) {
	x <- x[[1]]
	if (length(x) > 1) {x <- extract_first(x)}
	x
}
