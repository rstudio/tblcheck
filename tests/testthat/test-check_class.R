test_that("tbl_grade_class()", {
  grade <- tblcheck_test_grade({
    .result   <- "1"
    .solution <- 1
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- c("1", "2")
    .solution <- c(1, 2)
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- "2021-07-29 10:59:59"
    .solution <- as.POSIXct("2021-07-29 10:59:59")
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
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
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
})

test_that("tbl_grade_class() with multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1L
    class(.solution) <- c("test", "class", "integer")
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
  
  grade <- tblcheck_test_grade({
    .result <- 1L
    class(.result) <- c("test", "class", "integer")
    .solution <- 1L
    tbl_grade_class()
  })
  
  expect_snapshot(grade)
  expect_snapshot_value(grade$problem, style = "deparse")
})
