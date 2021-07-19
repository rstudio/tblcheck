expect_result_message <- function(result, expected, ...) {
  expect_match(as.character(result$feedback$message), expected, ...)
}
