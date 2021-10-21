test_that("vector length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:6]
    vec_grade_dimensions()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "length",
      letters[1:6], letters[1:3], expected_length = 6, actual_length = 3
    ),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:5]
    vec_grade_dimensions()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "length",
      letters[1:5], letters[1:3], expected_length = 5, actual_length = 3
    ),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    vec_grade_dimensions()
  })

  expect_snapshot(grade)

  expect_equal(
    grade$problem,
    problem(
      "length",
      letters[1:4], letters[1:3], expected_length = 4, actual_length = 3
    ),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[4:7]
    vec_grade_dimensions()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "length",
      letters[4:7], letters[1:3], expected_length = 4, actual_length = 3
    ),
    ignore_attr = "class"
  )
})

test_that("table rows", {
  grade_tbl_rows_missing_1 <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters, b = a)
      .solution <- tibble::tibble(a = letters[-1], b = a)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_tbl_rows_missing_1)

  expect_equal(
    grade_tbl_rows_missing_1$problem,
    problem("nrow", 25, 26),
    ignore_attr = "class"
  )

  grade_tbl_rows_extra_1 <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters, b = a)
      .solution <- tibble::tibble(a = letters[1], b = a)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_tbl_rows_extra_1)

  expect_equal(
    grade_tbl_rows_extra_1$problem,
    problem("nrow", 1, 26),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_dimensions() ncol", {
  grade_tbl_cols_extra_1 <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters, b = a, c = a)
      .solution <- tibble::tibble(a = letters, b = a)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_tbl_cols_extra_1)

  expect_equal(
    grade_tbl_cols_extra_1$problem,
    problem("ncol", 2, 3),
    ignore_attr = "class"
  )

  grade_tbl_cols_extra_2 <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters, b = a, c = a)
      .solution <- tibble::tibble(a = letters)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_tbl_cols_extra_2)

  expect_equal(
    grade_tbl_cols_extra_2$problem,
    problem("ncol", 1, 3),
    ignore_attr = "class"
  )

  grade_tbl_cols_missing_1 <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters)
      .solution <- tibble::tibble(a = letters, b = a)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_tbl_cols_missing_1)

  expect_equal(
    grade_tbl_cols_missing_1$problem,
    problem("ncol", 2, 1),
    ignore_attr = "class"
  )
})

test_that("mismatched dimensions", {
  grade <-
    tblcheck_test_grade({
      .result   <- 1:12
      .solution <- matrix(1:12, 3)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade)

  expect_equal(
    grade$problem,
    problem("dimensions_n", 2, 1),
    ignore_attr = "class"
  )
})

test_that("multidimensional array", {
  grade <-
    tblcheck_test_grade({
      .result   <- array(1:12, c(1, 3, 4))
      .solution <- array(1:12, c(2, 2, 3))
      tbl_grade_dimensions()
    })

  expect_snapshot(grade)

  expect_equal(
    grade$problem,
    problem("dimensions", c(2, 2, 3), c(1, 3, 4)),
    ignore_attr = "class"
  )
})

test_that("matrices", {
  grade <-
    tblcheck_test_grade({
      .result   <- matrix(1:12, 3)
      .solution <- matrix(1:12, 4)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade)

  expect_equal(
    grade$problem,
    problem("ncol", 3, 4),
    ignore_attr = "class"
  )

  grade_n <-
    tblcheck_test_grade({
      .result   <- 1:12
      .solution <- matrix(1:12, 4)
      tbl_grade_dimensions()
    })

  expect_snapshot(grade_n)

  expect_equal(
    grade_n$problem,
    problem("dimensions_n", 2, 1),
    ignore_attr = "class"
  )
})
