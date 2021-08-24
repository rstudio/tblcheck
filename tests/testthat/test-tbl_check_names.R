test_that("check missing names", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem("table_names", missing = "b", unexpected = character(0)),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem("table_names", missing = c("b", "c"), unexpected = character(0)),
    ignore_attr = "class"
  )
})

test_that("check unexpected names", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem("table_names", missing = character(0), unexpected = "b"),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem("table_names", missing = character(0), unexpected = c("b", "c")),
    ignore_attr = "class"
  )
})

test_that("check missing and unexpected names", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(x = letters[1:3], y = x)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem("table_names", missing = c("x", "y"), unexpected = c("a", "b")),
    ignore_attr = "class"
  )
})

test_that("tbl_check_names() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  problem <- expect_invisible(tbl_check_names())
  expect_null(problem)
})
