
test_that("check_table() rows", {
  result   <- tibble::tibble(a = letters, b = a)
  solution <- tibble::tibble(x = letters[-1], y = x)
  
  grade <- gradethis:::capture_graded(
    check_table(object = result, expected = solution)
  )
  
  expect_equal(grade$problem, problem("table_nrow", 25, 26))
  expect_false(grade$correct)
  expect_match(grade$message, "should have 25 rows")
  
  solution <- tibble::tibble(x = letters[1], y = x)
  grade    <- gradethis:::capture_graded(
    check_table(object = result, expected = solution)
  )
  
  expect_equal(grade$problem, problem("table_nrow", 1, 26))
  expect_false(grade$correct)
  expect_match(grade$message, "should have 1 row")
})

test_that("check_table() ncol", {
  result   <- tibble::tibble(a = letters, b = a, c = a)
  solution <- tibble::tibble(a = letters, b = a)
  
  grade <- gradethis:::capture_graded(
    check_table(object = result, expected = solution, check_names = FALSE)
  )
  
  expect_equal(grade$problem, problem("table_ncol", 2, 3))
  expect_false(grade$correct)
  expect_match(grade$message, "should have 2 columns")
  
  solution <- tibble::tibble(a = letters)
  grade   <- gradethis:::capture_graded(
    check_table(object = result, expected = solution, check_names = FALSE)
  )
  
  expect_equal(grade$problem, problem("table_ncol", 1, 3))
  expect_false(grade$correct)
  expect_match(grade$message, "should have 1 column")
})

test_that("check_table() names", {
  result   <- tibble::tibble(a = letters[1:3], b = a)
  solution <- tibble::tibble(x = letters[1:3], y = x)
  
  grade <- gradethis:::capture_graded(
    check_table(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = c("x", "y"), unexpected = c("a", "b"))
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should have columns named `x` and `y`")
  expect_match(grade$message, "should not have columns named `a` or `b`")
})

test_that("check_table() columns", {
  result   <- tibble::tibble(a = letters[1:3])
  solution <- tibble::tibble(a = letters[24:26])
  
  grade <- gradethis:::capture_graded(
    check_table(object = result, expected = solution)
  )
  
  expect_equal(grade$problem, problem("column_values"))
  expect_false(grade$correct)
  expect_match(
    grade$message,
    "first 3 values of your `a` column should be `x`, `y`, and `z`"
  )
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
      check_names(object = result, expected = solution, max_diffs = "a")
    ),
    "max_diffs"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = solution, max_diffs = -1)
    ),
    "max_diffs"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = solution, max_diffs = 1:2)
    ),
    "max_diffs"
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

test_that("check_table() returns grades with row problems", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters),
    .solution_code = tibble::tibble(a = letters[1:25])
  )
  
  grade <- gradethis::grade_this(check_table())(ex)
  
  expect_grade(
    grade,
    "should have 25 rows",
    problem = problem(type = "table_nrow", expected = 25L, actual = 26L)
  )
  
  ex_single <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters),
    .solution_code = tibble::tibble(a = letters[1])
  )
  
  grade_single <- gradethis::grade_this(check_table())(ex_single)
  
  expect_grade(
    grade_single,
    "should have 1 row",
    problem = problem(type = "table_nrow", expected = 1L, actual = 26L)
  )
})

test_that("check_table() returns ncol feedback to learnr", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters, b = letters, c = letters),
    .solution_code = tibble::tibble(a = letters, b = letters)
  )
  
  grade <- gradethis::grade_this(check_table(check_names = FALSE))(ex)
  
  expect_grade(
    grade,
    "should have 2 columns",
    problem = problem("table_ncol", 2, 3)
  )
  
  
  ex_one <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters, b = letters, c = letters),
    .solution_code = tibble::tibble(a = letters)
  )
  
  grade_one <- gradethis::grade_this(check_table(check_names = FALSE))(ex_one)
  
  expect_grade(
    grade_one,
    "should have 1 column",
    problem = problem("table_ncol", 1, 3)
  )
})

test_that("check_table() returns names feedback to learnr", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters, b = a, c = a, d = a),
    .solution_code = tibble::tibble(x = letters, y = x, z = x, w = x)
  )
  
  grade <- gradethis::grade_this(check_table())(ex)
  
  expect_grade(
    grade, 
    "should have columns named .*x.*, .*y.*, .*z.*, and 1 more",
    problem = problem("names", missing = c("x", "y", "z", "w"), unexpected = c("a", "b", "c", "d"))
  )
  
  expect_grade(
    grade,
    "should not have columns named .*a.*, .*b.*, .*c.*, or 1 more"
  )
  
  # ---- with all diffs ---
  grade_inf <- gradethis::grade_this(check_table(max_diffs = Inf))(ex)
  
  expect_grade(
    grade_inf,
    "should have columns named .*x.*, .*y.*, .*z.*, and .*w",
    problem = problem("names", missing = c("x", "y", "z", "w"), unexpected = c("a", "b", "c", "d"))
  )
  
  expect_grade(
    grade_inf,
    "should not have columns named .*a.*, .*b.*, .*c.*, or .*d"
  )
  
  # ---- with one diff ---
  grade_one <- gradethis::grade_this(check_table(max_diffs = 1))(ex)
  
  expect_grade(
    grade_one,
    "should have columns named .*x.* and 3 more",
    problem = problem("names", missing = c("x", "y", "z", "w"), unexpected = c("a", "b", "c", "d"))
  )
  
  expect_grade(
    grade_one,
    "should not have columns named .*a.* or 3 more"
  )
})
