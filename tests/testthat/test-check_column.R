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
    "should have class .*integer.*, but it has class .*character.*"
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
    "should be <code>a</code>, <code>b</code>, and <code>c</code>."
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
    "should have classes .*tbl_df.*, .*tbl.*, and .*data.frame.*, but it has class .*data.frame"
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

test_that("max_print modifies the number of values to print", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters)",
    solution_code    = "tibble(a = rev(letters))",
    check            = "grade_this({check_column('a', max_print = 5)})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis::gradethis_exercise_checker"
  )
  
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_match(
    result$feedback$message,
    "The first 5 values of your <code>a</code> column should be <code>z</code>, <code>y</code>, <code>x</code>, <code>w</code>, and <code>v</code>."
  )
  
})

test_that("max_print doesn't overflow", {
  result <- tibble(a = letters[1:2])
  solution <- tibble(a = letters[2:1])
  
  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution, max_print = 3)
  )
  
  expect_equal(grade$problem, problem("column_values"))
  expect_false(grade$correct)
  expect_no_match(grade$message, "`NA`")
  expect_match(grade$message, "`b` and `a`.")
})

test_that("checks that columns have the same length", {
  result <- tibble(a = letters[1:3])
  solution <- tibble(a = letters[1:4])
  
  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution)
  )
  
  expect_equal(grade$problem, problem("column_length", 4, 3))
  expect_false(grade$correct)
  expect_match(grade$message, "should contain 4 values")
})

test_that("checks that the column is present in object", {
  result <- tibble(b = letters[1:3])
  solution <- tibble(a = letters[1:3])
  
  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution)
  )
  
  expect_equal(grade$problem, problem("column_name", "a"))
  expect_false(grade$correct)
  expect_match(grade$message, "should have a column named `a`")
})

test_that("check_column() handles bad user input", {
  result <- tibble(b = letters[1:3])
  solution <- tibble(a = letters[1:3])
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column(3, object = result, expected = solution)
    ),
    "name"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column(c("a", "b"), object = result, expected = solution)
    ),
    "name"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column("a", object = result, expected = solution, check_class = "yes")
    ),
    "check_class"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column("a", object = result, expected = solution, check_values = "yes")
    ),
    "check_values"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column("b", object = 12, expected = solution)
    ),
    "object"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_column("a", object = result, expected = list(a = 1))
    ),
    "expected"
  )
})
