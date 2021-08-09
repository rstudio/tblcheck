expect_internal_problem <- function(grade, message) {
  testthat::expect_message(grade)
  testthat::expect_equal(grade$correct, logical())
  testthat::expect_match(grade$message, "can't provide feedback")
  testthat::expect_equal(grade$problem$type, "internal_feedback_error")
  testthat::expect_match(as.character(grade$problem$error), message)
}

expect_result_message <- function(result, expected, ...) {
  testthat::expect_match(as.character(result$feedback$message), expected, ...)
}

expect_grade <- function(grade, message, correct = FALSE, problem = NULL, ...) {
  testthat::expect_s3_class(grade, "gradethis_graded")
  testthat::expect_equal(grade$correct, correct)
  if (!is.null(message)) {
    testthat::expect_match(grade$message, message, ...)
  }
  if (!is.null(problem)) {
    testthat::expect_equal(grade$problem, problem)
  }
}

tblcheck_test_grade <- function(expr, return_all = FALSE) {
  expr <- rlang::enexpr(expr)
  
  if (identical(expr[[1]], rlang::sym("{"))) {
    expr_setup <- expr[-length(expr)]
    expr_check <- expr[[length(expr)]]
    final_call <- paste(expr[[length(expr)]][[1]])
  } else {
    expr_setup <- NULL
    expr_check <- expr
    final_call <- paste(expr[[1]])
  }
  
  check_fns <- grep(
    "^tbl_", paste(utils::lsf.str("package:tblcheck")), value = TRUE
  )
  if (!final_call %in% check_fns) {
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
