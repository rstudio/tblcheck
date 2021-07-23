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
  testthat::expect_match(grade$message, message, ...)
  if (!is.null(problem)) {
    testthat::expect_equal(grade$problem, problem)
  }
}
