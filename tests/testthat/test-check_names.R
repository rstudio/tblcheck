test_that("grade missing names", {
  grade_missing_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_names()
  })
  
  expect_snapshot(grade_missing_1)
  
  expect_equal(
    grade_missing_1$problem,
    problem("table_names", missing = "b", unexpected = character(0)),
    ignore_attr = "class"
  )
  
  grade_missing_2 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
    tbl_grade_names()
  })
  
  expect_snapshot(grade_missing_2)
  
  expect_equal(
    grade_missing_2$problem,
    problem("table_names", missing = c("b", "c"), unexpected = character(0)),
    ignore_attr = "class"
  )
})

test_that("grade unexpected names", {
  grade_unexpected_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_snapshot(grade_unexpected_1)
  
  expect_equal(
    grade_unexpected_1$problem,
    problem("table_names", missing = character(0), unexpected = "b"),
    ignore_attr = "class"
  )
  
  grade_unexpected_2 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_names()
  })
  
  expect_snapshot(grade_unexpected_2)
  
  expect_equal(
    grade_unexpected_2$problem,
    problem("table_names", missing = character(0), unexpected = c("b", "c")),
    ignore_attr = "class"
  )
})

test_that("grade missing and unexpected names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    tbl_grade_names()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("table_names", missing = c("x", "y"), unexpected = c("a", "b")),
    ignore_attr = "class"
  )
})

test_that("grade names max_diffs()", {
  grade_max_diffs_3 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = 3)
  })
  
  expect_snapshot(grade_max_diffs_3)
  
  grade_max_diffs_inf <-
    tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = Inf)
  })
  
  expect_snapshot(grade_max_diffs_inf)
  
  grade_max_diffs_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble()
    .solution <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    tbl_grade_names(max_diffs = 1)
  })
  
  expect_snapshot(grade_max_diffs_1)
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
