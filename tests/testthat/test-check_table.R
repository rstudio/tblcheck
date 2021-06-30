test_that("check_table", {
  ex <- learnr:::mock_exercise(
    user_code     = "tibble(a = letters, b = letters, c = letters)",
    solution_code = "tibble(x = letters, y = letters, z = letters)",
    check         = "
    grade_this({
      check_table()
    })
  ",
    global_setup = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker",
    exercise.error.check.code = NULL
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should have a column named .*x.*, .*y.*, and .*z"
  )
  expect_match(
    result$feedback$message,
    "should not have a column named .*a.*, .*b.*, or .*c"
  )
})
