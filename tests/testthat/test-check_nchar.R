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
  
  grade_col <- tblcheck_test_grade({
    .result <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_nchar(column = "a")
  })
  
  expect_snapshot(grade_col)
  
  expect_equal(
    grade$problem,
    problem("column_nchar", 2, 1, column = "a"),
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
  
  grade_col <- tblcheck_test_grade({
    .result <- tibble::tibble(a = strrep(letters, 1:26))
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_nchar(column = "a")
  })
  
  expect_snapshot(grade_col)
  
  expect_equal(
    grade_col$problem,
    problem("column_inconsistent_nchar", 2, column = "a"),
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
  
  expect_warning(
    grade_col <- tblcheck_test_grade({
      .result <- tibble::tibble(a = letters)
      .solution <- tibble::tibble(a = strrep(letters, 1:26))
      tbl_grade_nchar(column = "a")
    }),
    "`expected` does not have a consistent number of characters."
  )
  
  expect_null(grade_col)
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
  
  expect_equal(grade_nchar, grade_class, ignore_attr = "class")
  
  grade_nchar_col <- tblcheck_test_grade({
    .result <- tibble::tibble(a = 1:26)
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_nchar(column = "a")
  })
  
  grade_column <- tblcheck_test_grade({
    .result <- tibble::tibble(a = 1:26)
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_column("a")
  })
  
  expect_equal(grade_nchar_col, grade_column, ignore_attr = "class")
})

test_that("tbl_grade_nchar(column = ) checks names", {
  grade_nchar <- tblcheck_test_grade({
    .result <- tibble::tibble(b = 1:26)
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_nchar(column = "a")
  })
  
  grade_names <- tblcheck_test_grade({
    .result <- tibble::tibble(b = 1:26)
    .solution <- tibble::tibble(a = strrep(letters, 2))
    tbl_grade_names()
  })
  
  expect_equal(grade_nchar, grade_names, ignore_attr = "class")
  
  expect_warning(
    grade_missing <- tblcheck_test_grade({
      .result <- tibble::tibble(a = letters)
      .solution <- tibble::tibble(b = strrep(letters, 1:26))
      tbl_grade_nchar(column = "a")
    }),
    "`b` is not a column in expected."
  )
  
  expect_null(grade_missing)
})
