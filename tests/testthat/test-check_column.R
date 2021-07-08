test_that("check_column() checks classes", {
  ex <- learnr:::mock_exercise(
      user_code        = "tibble(a = letters)",
      solution_code    = "tibble(a = 1:3)",
      check            = "grade_this({check_column('a')})",
      global_setup     = "library(gradethis); library(tibble)",
      exercise.checker = "gradethis::gradethis_exercise_checker"
    )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should have class integer, but it has class character"
  )

})

test_that("check_column() checks the first three values", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = rev(letters))",
    solution_code    = "tibble(a = letters)",
    check            = "grade_this({check_column('a')})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should be <code>a</code>, <code>b</code> and <code>c</code>."
  )
  
})

test_that("check_column() checks multiple classes", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = data.frame(x = 1))",
    solution_code    = "tibble(a = tibble(x = 1))",
    check            = "grade_this({check_column('a')})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "should have classes tbl_df, tbl and data.frame, but it has class data.frame."
  )
  
})

test_that("check_column() checks for value differences beyond the first 3", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = c(rep(1, 3), 5:10))",
    solution_code    = "tibble(a = c(rep(1, 3), 10:15))",
    check            = "grade_this({check_column('a')})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "column contains unexpected values."
  )
  
})

test_that("n_values modifies the number of values to print", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters)",
    solution_code    = "tibble(a = rev(letters))",
    check            = "grade_this({check_column('a', n_values = 5)})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "The first 5 values of your <code>a</code> column should be <code>z</code>, <code>y</code>, <code>x</code>, <code>w</code> and <code>v</code>."
  )
  
})
