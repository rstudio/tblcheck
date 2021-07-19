test_that("check missing names", {
  result   <- tibble::tibble(a = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3], b = a)
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = "b", unexpected = character(0))
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should have a column named `b`")
  
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = c("b", "c"), unexpected = character(0))
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should have columns named `b` and `c`")
})

test_that("check unexpected names", {
  result   <- tibble::tibble(a = letters[1:3], b = a)
  solution <- tibble::tibble(a = letters[1:3])
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = character(0), unexpected = "b")
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should not have a column named `b`")
  
  result <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = character(0), unexpected = c("b", "c"))
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should not have columns named `b` or `c`")
})

test_that("check missing and unexpected names", {
  result   <- tibble::tibble(a = letters[1:3], b = a)
  solution <- tibble::tibble(x = letters[1:3], y = x)
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = c("x", "y"), unexpected = c("a", "b"))
  )
  expect_false(grade$correct)
  expect_match(grade$message, "should have columns named `x` and `y`")
  expect_match(grade$message, "should not have columns named `a` or `b`")
})

test_that("check names max_diffs()", {
  result   <- tibble::tibble()
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution)
  )
  
  expect_equal(
    grade$problem,
    problem("names", missing = letters[1:4], unexpected = character(0))
  )
  expect_false(grade$correct)
  expect_match(
    grade$message, "should have columns named `a`, `b`, `c`, and 1 more"
  )
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution, max_diffs = Inf)
  )
  expect_match(
    grade$message, "should have columns named `a`, `b`, `c`, and `d`"
  )
  
  grade <- gradethis:::capture_graded(
    check_names(object = result, expected = solution, max_diffs = 1)
  )
  expect_match(grade$message, "should have columns named `a` and 3 more")
})

test_that("check_names() with no problems returns invisible()", {
  result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  expect_invisible(
    grade <- gradethis:::capture_graded(
      check_names(object = result, expected = solution)
    )
  )
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_names() handles bad user input", {
  result <- tibble::tibble(b = letters[1:3])
  solution <- tibble::tibble(a = letters[1:3])
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = 12, expected = solution)
    ),
    "object"
  )
  
  expect_internal_problem(
    gradethis:::capture_graded(
      check_names(object = result, expected = list(a = 1))
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
})
