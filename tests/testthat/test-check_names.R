test_that("check missing names", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3])
    solution <- tibble::tibble(a = letters[1:3], b = a)
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have a column named `b`",
    problem = problem("names", missing = "b", unexpected = character(0))
  )
  
  
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3])
    solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have columns named `b` and `c`",
    problem = problem("names", missing = c("b", "c"), unexpected = character(0))
  )
})

test_that("check unexpected names", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3], b = a)
    solution <- tibble::tibble(a = letters[1:3])
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should not have a column named `b`",
    problem = problem("names", missing = character(0), unexpected = "b")
  )
  
  grade <- tblcheck_test_grade({
    result <- tibble::tibble(a = letters[1:3], b = a, c = a)
    solution <- tibble::tibble(a = letters[1:3])
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should not have columns named `b` or `c`",
    problem = problem("names", missing = character(0), unexpected = c("b", "c"))
  )
})

test_that("check missing and unexpected names", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3], b = a)
    solution <- tibble::tibble(x = letters[1:3], y = x)
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have columns named `x` and `y`",
    problem = problem("names", missing = c("x", "y"), unexpected = c("a", "b"))
  )
  expect_match(grade$message, "should not have columns named `a` or `b`")
})

test_that("check names max_diffs()", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble()
    solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    check_names(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have columns named `a`, `b`, `c`, and 1 more",
    problem = problem("names", missing = letters[1:4], unexpected = character(0))
  )
  
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble()
    solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    check_names(object = result, expected = solution, max_diffs = Inf)
  })
  
  expect_grade(
    grade, 
    message = "should have columns named `a`, `b`, `c`, and `d`",
    problem = problem("names", missing = letters[1:4], unexpected = character(0))
  )
  
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble()
    solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    check_names(object = result, expected = solution, max_diffs = 1)
  })
  
  expect_grade(
    grade, 
    message = "should have columns named `a` and 3 more",
    problem = problem("names", missing = letters[1:4], unexpected = character(0))
  )
})

test_that("check_names() with no problems returns invisible()", {
  result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- expect_invisible(check_names(object = result, expected = solution))
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_names() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      check_names(object = result, expected = solution, max_diffs = "a")
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      check_names(object = result, expected = solution, max_diffs = -1)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      check_names(object = result, expected = solution, max_diffs = 1:2)
    }),
    "max_diffs"
  )
})
