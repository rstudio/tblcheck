test_that("grade missing names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_equal(
    problem,
    problem("table_names", missing = "b", unexpected = character(0)),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should have a column named `b`.",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_equal(
    problem,
    problem("table_names", missing = c("b", "c"), unexpected = character(0)),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should have columns named `b` and `c`.",
    problem = problem
  )
})

test_that("grade unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(a = letters[1:3])
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_equal(
    problem,
    problem("table_names", missing = character(0), unexpected = "b"),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should not have a column named `b`.",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
    .solution <- tibble::tibble(a = letters[1:3])
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_equal(
    problem,
    problem("table_names", missing = character(0), unexpected = c("b", "c")),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should not have columns named `b` or `c`.",
    problem = problem
  )
})

test_that("grade missing and unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_equal(
    problem,
    problem("table_names", missing = c("x", "y"), unexpected = c("a", "b")),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should have columns named `x` and `y`. Your table should not have columns named `a` or `b`.",
    problem = tbl_check_names(
      tibble::tibble(a = letters[1:3], b = a),
      tibble::tibble(x = letters[1:3], y = x)
    )
  )
})

test_that("grade names max_diffs()", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    problem   <- tbl_check_names()
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `a`, `b`, `c`, and 1 more.",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    problem   <- tbl_check_names()
    tbl_grade_names(max_diffs = Inf)
  })
  
  expect_grade(
    grade, 
    message = "Your table should have columns named `a`, `b`, `c`, and `d`.",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    problem   <- tbl_check_names()
    tbl_grade_names(max_diffs = 1)
  })
  
  expect_grade(
    grade, 
    message = "Your table should have columns named `a` and 3 more.",
    problem = problem
  )
})

test_that("tbl_grade_names() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  problem <- expect_invisible(tbl_check_names())
  expect_null(problem)
  
  grade <- expect_invisible(tbl_grade_names())
  expect_null(grade)
})

test_that("tbl_grade_names() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble()
      .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
      tbl_grade_names(max_diffs = "a")
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble()
      .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
      tbl_grade_names(max_diffs = -1)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble()
      .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
      tbl_grade_names(max_diffs = 1:2)
    }),
    "max_diffs"
  )
})
