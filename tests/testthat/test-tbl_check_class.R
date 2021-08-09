test_that("tbl_check_class()", {
  .result   <- "1"
  .solution <- 1
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = "numeric", length = 1),
      actual   = list(class = "character", length = 1)
    )
  )

  .result   <- c("1", "2")
  .solution <- c(1, 2)
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = "numeric", length = 2),
      actual   = list(class = "character", length = 2)
    )
  )

  .result   <- "2021-07-29 10:59:59"
  .solution <- as.POSIXct("2021-07-29 10:59:59")
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = c("POSIXct", "POSIXt"), length = 1),
      actual   = list(class = "character", length = 1)
    )
  )

  .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
  .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = c("POSIXlt", "POSIXt"), length = 2),
      actual   = list(class = "character", length = 2)
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
      expected = list(class = "numeric", length = 1),
      actual   = list(class = "integer", length = 1)
    )
  )

  .result   <- glue::glue("x")
  .solution <- "x"
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = "character", length = 1),
      actual   = list(class = c("glue", "character"), length = 1)
    )
  )

  .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
  problem   <- tbl_check_class()
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = list(class = c("POSIXlt", "POSIXt"), length = 2),
      actual   = list(class = c("POSIXct", "POSIXt"), length = 2)
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
      expected = list(class = c("test", "class", "integer"), length = 1),
      actual   = list(class = "integer", length = 1)
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
      expected = list(class = "integer", length = 1),
      actual   = list(class = c("test", "class", "integer"), length = 1)
    )
  )
})
