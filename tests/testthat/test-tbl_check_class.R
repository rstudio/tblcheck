test_that("tbl_check_class()", {
  .result   <- "1"
  .solution <- 1
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )

  .result   <- c("1", "2")
  .solution <- c(1, 2)
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )

  .result   <- "2021-07-29 10:59:59"
  .solution <- as.POSIXct("2021-07-29 10:59:59")
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXct", "POSIXt"),
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )

  .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
  .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
})

test_that("tbl_check_class() returns inconsequential mismatches", {
  .result   <- 1L
  .solution <- 1
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "integer",
      expected_length = 1,
      actual_length = 1,
      message = FALSE
    )
  )

  .result   <- glue::glue("x")
  .solution <- "x"
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "character",
      actual   = c("glue", "character"),
      expected_length = 1,
      actual_length = 1,
      message = FALSE
    )
  )

  .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = c("POSIXct", "POSIXt"),
      expected_length = 2,
      actual_length = 2,
      message = FALSE
    )
  )
})

test_that("tbl_check_class() with multiple classes", {
  .result          <- 1L
  .solution        <- 1L
  class(.solution) <- c("test", "class", "integer")
  problem          <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("test", "class", "integer"),
      actual   = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )

  .result        <- 1L
  class(.result) <- c("test", "class", "integer")
  .solution      <- 1L
  problem        <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "integer",
      actual   = c("test", "class", "integer"),
      expected_length = 1,
      actual_length = 1
    )
  )
})
