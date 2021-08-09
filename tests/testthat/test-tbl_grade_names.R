test_that("grade missing names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have a column named `b`.",
    problem = problem(
      "names", missing = "b", unexpected = character(0), object_label = "table"
    )
  )
  
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `b` and `c`.",
    problem = problem(
      "names", missing = c("b", "c"), unexpected = character(0),
      object_label = "table"
    )
  )
})

test_that("grade unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should not have a column named `b`.",
    problem = problem(
      "names", missing = character(0), unexpected = "b", object_label = "table"
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should not have columns named `b` or `c`.",
    problem = problem(
      "names", missing = character(0), unexpected = c("b", "c"),
      object_label = "table"
    )
  )
})

test_that("grade missing and unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `x` and `y`. Your table should not have columns named `a` or `b`.",
    problem = problem(
      "names", missing = c("x", "y"), unexpected = c("a", "b"),
      object_label = "table"
    )
  )
})

test_that("grade names max_diffs()", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `a`, `b`, `c`, and 1 more.",
    problem = problem(
      "names", missing = letters[1:4], unexpected = character(0),
      object_label = "table"
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = Inf)
  })
  
  expect_grade(
    grade, 
    message = "Your table should have columns named `a`, `b`, `c`, and `d`.",
    problem = problem(
      "names", missing = letters[1:4], unexpected = character(0),
      object_label = "table"
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = 1)
  })
  
  expect_grade(
    grade, 
    message = "Your table should have columns named `a` and 3 more.",
    problem = problem(
      "names", missing = letters[1:4], unexpected = character(0),
      object_label = "table"
    )
  )
})

test_that("tbl_grade_names() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- expect_invisible(tbl_grade_names())
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
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
