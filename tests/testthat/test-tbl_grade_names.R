test_that("grade missing names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have a column named `b`.",
    problem = tbl_check_names(
      tibble::tibble(a = letters[1:3]),
      tibble::tibble(a = letters[1:3], b = a)
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
    problem = tbl_check_names(
      tibble::tibble(a = letters[1:3]),
      tibble::tibble(a = letters[1:3], b = a, c = a)
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
    problem = tbl_check_names(
      tibble::tibble(a = letters[1:3], b = a),
      tibble::tibble(a = letters[1:3])
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
    problem = tbl_check_names(
      tibble::tibble(a = letters[1:3], b = a, c = a),
      tibble::tibble(a = letters[1:3])
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
    tbl_grade_names()
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `a`, `b`, `c`, and 1 more.",
    problem = tbl_check_names(
      tibble::tibble(),
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
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
    problem = tbl_check_names(
      tibble::tibble(),
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
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
    problem = tbl_check_names(
      tibble::tibble(),
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
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
