expect_internal_problem <- function(grade, message) {
  expect_message(grade)
  expect_equal(grade$correct, logical())
  expect_match(grade$message, "can't provide feedback")
  expect_equal(grade$problem$type, "internal_feedback_error")
  expect_match(as.character(grade$problem$error), message)
}

expect_result_message <- function(result, expected, ...) {
  expect_match(as.character(result$feedback$message), expected, ...)
}
