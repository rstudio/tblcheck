test_that("check_table names", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters, b = a, c = a, d = a)",
    solution_code    = "tibble(x = letters, y = x, z = x, w = x)",
    check            = "grade_this({check_table()})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should have columns named .*x.*, .*y.*, and .*z"
  )
  expect_match(
    result$feedback$message,
    "should not have columns named .*a.*, .*b.*, or .*c"
  )
  
  ex$check <- "grade_this({check_table(max = Inf)})"
  result   <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should have columns named .*x.*, .*y.*, .*z.*, and .*w"
  )
  expect_match(
    result$feedback$message,
    "should not have columns named .*a.*, .*b.*, .*c.*, or .*d"
  )
  
  ex$check <- "grade_this({check_table(max = 1)})"
  result   <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(result$feedback$message, "should have a column named .*x")
  expect_match(result$feedback$message, "should not have a column named .*a")
})

test_that("check_table rows", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters)",
    solution_code    = "tibble(a = letters[1:25])",
    check            = "grade_this({check_table()})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_match(result$feedback$message, "should have 25 rows")
  
  ex$solution_code <- "tibble(a = letters[1])"
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_match(result$feedback$message, "should have 1 row")
})

test_that("check_table columns", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters, b = letters, c = letters)",
    solution_code    = "tibble(a = letters, b = letters)",
    check            = "grade_this({check_table(check_names = FALSE)})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_match(result$feedback$message, "should have 2 columns")
  
  ex$solution_code <- "tibble(a = letters)"
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_match(result$feedback$message, "should have 1 column")
})
