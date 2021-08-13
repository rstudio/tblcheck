test_that("check missing names", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = "b", unexpected = character(0), table = TRUE
    )
  )
  
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = c("b", "c"), unexpected = character(0), table = TRUE
    )
  )
})

test_that("check unexpected names", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = character(0), unexpected = "b", table = TRUE
    )
  )
  
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = character(0), unexpected = c("b", "c"), table = TRUE
    )
  )
})

test_that("check missing and unexpected names", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(x = letters[1:3], y = x)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = c("x", "y"), unexpected = c("a", "b"), table = TRUE
    )
  )
})

test_that("tbl_check_names() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  problem <- expect_invisible(tbl_check_names())
  expect_null(problem)
})
