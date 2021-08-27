test_that("grade consistent lengths", {
  grade <- tblcheck_test_grade({
    .result <- letters
    .solution <- strrep(letters, 2)
    tbl_grade_nchar()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("nchar", 2, 1),
    ignore_attr = "class"
  )
})

test_that("grade inconsistent lengths", {
  grade <- tblcheck_test_grade({
    .result <- strrep(letters, 1:26)
    .solution <- strrep(letters, 2)
    tbl_grade_nchar()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("inconsistent_nchar", 2),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_nchar() fails if expected has inconsistent lengths", {
  expect_warning(
    grade <- tblcheck_test_grade({
      .result <- letters
      .solution <- strrep(letters, 1:26)
      tbl_grade_nchar()
    }),
    "`expected` does not have a consistent number of characters."
  )
  
  expect_null(grade)
})

test_that("tbl_grade_nchar() passes non-character to tbl_check_class()", {
  grade_nchar <- tblcheck_test_grade({
    .result <- 1:26
    .solution <- strrep(letters, 2)
    tbl_grade_nchar()
  })
  
  grade_class <- tblcheck_test_grade({
    .result <- 1:26
    .solution <- strrep(letters, 2)
    tbl_grade_class()
  })
  
  expect_equal(grade_nchar, grade_class)
})
