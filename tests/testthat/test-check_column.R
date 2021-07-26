
test_that("check_column() checks classes", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters),
    .solution_code = tibble::tibble(a = 1:3)
  )
  
  grade <- gradethis::grade_this(check_column("a"))(ex)
  
  expect_grade(
    grade,
    "should have class .*integer.*, but it has class .*character.*",
    problem = problem("column_class", "integer", "character")
  )
})

test_that("check_column() checks the first three values", {
  ex <- gradethis::mock_this_exercise(
    .user_code     = tibble::tibble(a = rev(letters)),
    .solution_code = tibble::tibble(a = letters)
  )
  
  grade <- gradethis::grade_this(check_column("a"))(ex)
  
  expect_grade(
    grade,
    "should be .*a.*, .*b.*, and .*c",
    problem = problem("column_values")
  )
})

test_that("check_column() checks multiple classes", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = data.frame(x = 1)),
    .solution_code = tibble::tibble(a = tibble::tibble(x = 1))
  )
  
  grade <- gradethis::grade_this(check_column('a'))(ex)
  
  expect_grade(
    grade,
    "should have classes .*tbl_df.*, .*tbl.*, and .*data.frame.*, but it has class .*data.frame",
    problem = problem(
      type = "column_class", 
      expected = c("tbl_df", "tbl", "data.frame"), 
      actual = "data.frame"
    )
  )
  
  
})

test_that("check_column() checks for value differences beyond the first 3", {
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = c(rep(1, 3), 5:10)),
    .solution_code = tibble::tibble(a = c(rep(1, 3), 10:15))
  )
  
  grade <- gradethis::grade_this(check_column("a"))(ex)
  
  expect_grade(
    grade,
    "column contains unexpected values.",
    problem = problem("column_values")
  )
})

test_that("max_diffs modifies the number of values to print", {
  
  ex <- gradethis::mock_this_exercise(
    .user_code = tibble::tibble(a = letters),
    .solution_code = tibble::tibble(a = rev(letters))
  )
  
  grade <- gradethis::grade_this(check_column("a", max_diffs = 5))(ex)
  
  expect_grade(
    grade,
    "The first 5 values of your .*a.* column should be .*z.*, .*y.*, .*x.*, .*w.*, and .*v",
    problem = problem("column_values")
  )
})

test_that("max_diffs doesn't overflow", {
  result <- tibble::tibble(a = letters[1:2])
  solution <- tibble::tibble(a = letters[2:1])

  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution, max_diffs = 3)
  )

  expect_grade(
    grade,
    message = "`b` and `a`.",
    problem = problem("column_values")
  )
  expect_no_match(grade$message, "`NA`")
})

test_that("checks that columns have the same length", {
  result <- tibble::tibble(a = letters[1:3])
  solution <- tibble::tibble(a = letters[1:4])

  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution)
  )
  
  expect_grade(
    grade,
    message = "should contain 4 values",
    problem = problem("column_length", 4, 3)
  )
})

test_that("checks that the column is present in object", {
  result <- tibble::tibble(b = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])

  grade <- gradethis:::capture_graded(
    check_column("a", object = result, expected = solution)
  )
  
  expect_grade(
    grade,
    message = "should have a column named `a`",
    problem = problem("column_name", "a")
  )
})

test_that("checks that the column is present in expected", {
  result <- tibble::tibble(b = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])

  expect_warning(
    grade <- gradethis:::capture_graded(
      check_column("b", object = result, expected = solution)
    ),
    "`b` is not a column in `expected`"
  )
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_column() with no problems returns invisible()", {
  result   <- tibble::tibble(a = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])

  expect_invisible(
    grade <- gradethis:::capture_graded(
      check_column("a", object = result, expected = solution)
    )
  )
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_column() handles bad user input", {
  result <- tibble::tibble(b = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])

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
