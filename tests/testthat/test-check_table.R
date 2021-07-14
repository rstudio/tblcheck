library(gradethis)
library(tibble)

test_that("check_table names", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters, b = a, c = a, d = a)",
    solution_code    = "tibble(x = letters, y = x, z = x, w = x)",
    check            = "grade_this({check_table()})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_result_message(
    result, "should have columns named .*x.*, .*y.*, and .*z"
  )
  expect_result_message(
    result, "should not have columns named .*a.*, .*b.*, or .*c"
  )
  
  ex$check <- "grade_this({check_table(max = Inf)})"
  result   <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_result_message(
    result, "should have columns named .*x.*, .*y.*, .*z.*, and .*w"
  )
  expect_result_message(
    result, "should not have columns named .*a.*, .*b.*, .*c.*, or .*d"
  )
  
  ex$check <- "grade_this({check_table(max = 1)})"
  result   <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  
  expect_result_message(result, "should have a column named .*x")
  expect_result_message(result, "should not have a column named .*a")
})

test_that("check_table rows", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters)",
    solution_code    = "tibble(a = letters[1:25])",
    check            = "grade_this({check_table()})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_result_message(result, "should have 25 rows")
  
  ex$solution <- "tibble(a = letters[1])"
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_result_message(result, "should have 1 row")
})

test_that("check_table columns", {
  ex <- learnr:::mock_exercise(
    user_code        = "tibble(a = letters, b = letters, c = letters)",
    solution_code    = "tibble(a = letters, b = letters)",
    check            = "grade_this({check_table(check_names = FALSE)})",
    global_setup     = "library(gradethis); library(tibble)",
    exercise.checker = "gradethis_exercise_checker"
  )
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_result_message(result, "should have 2 columns")
  
  ex$solution <- "tibble(a = letters)"
  result <- learnr:::evaluate_exercise(
    ex, new.env(), evaluate_global_setup = TRUE
  )
  expect_result_message(result, "should have 1 column")
})

test_that("check_table() with no problems returns invisible()", {
  result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  expect_invisible(
    grade <- gradethis:::capture_graded(
      check_table(object = result, expected = solution)
    )
  )
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_table() handles bad user input", {
  result <- tibble::tibble(b = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])
  
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
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = solution, max_print = "a")
    ),
    "max_print"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = solution, max_print = -1)
    ),
    "max_print"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = solution, max_print = 1:2)
    ),
    "max_print"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_nrow = "yes")
    ),
    "check_nrow"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_names = 5)
    ),
    "check_names"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_ncol = list())
    ),
    "check_ncol"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_columns = NULL)
    ),
    "check_columns"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_class = NA)
    ),
    "check_class"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_table(object = result, expected = solution, check_values = c(TRUE, TRUE))
    ),
    "check_values"
  )
})
