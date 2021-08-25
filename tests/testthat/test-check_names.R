test_that("grade missing names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
})

test_that("grade unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
})

test_that("grade missing and unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
})

test_that("grade names max_diffs()", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = Inf)
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = 1)
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
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
