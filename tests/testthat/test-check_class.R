test_that("tbl_grade_class()", {
  grade <- tblcheck_test_grade({
    .result   <- "1"
    .solution <- 1
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("1", "2")
    .solution <- c(1, 2)
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- "2021-07-29 10:59:59"
    .solution <- as.POSIXct("2021-07-29 10:59:59")
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = c("POSIXct", "POSIXt"),
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
})

test_that("tbl_grade_class() ignores inconsequential mismatches", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = "character",
      actual   = c("glue", "character"),
      expected_length = 1,
      actual_length = 1
    )
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = c("POSIXct", "POSIXt"),
      expected_length = 2,
      actual_length = 2
    )
  )
})

test_that("tbl_grade_class() with multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1L
    class(.solution) <- c("test", "class", "integer")
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = c("test", "class", "integer"),
      actual   = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  grade <- tblcheck_test_grade({
    .result <- 1L
    class(.result) <- c("test", "class", "integer")
    .solution <- 1L
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type     = "class",
      expected = "integer",
      actual   = c("test", "class", "integer"),
      expected_length = 1,
      actual_length = 1
    )
  )
})
