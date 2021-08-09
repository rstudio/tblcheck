test_that("check missing names", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = "b", unexpected = character(0), object_label = "table"
    )
  )
  
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = c("b", "c"), unexpected = character(0),
      object_label = "table"
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
      "names", missing = character(0), unexpected = "b",
      object_label = "table"
    )
  )
  
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_names()
  
  expect_equal(
    problem,
    problem(
      "names", missing = character(0), unexpected = c("b", "c"),
      object_label = "table"
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
      "names", missing = c("x", "y"), unexpected = c("a", "b"),
      object_label = "table"
    )
  )
})

test_that("tbl_check_names() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a, c = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- expect_invisible(tbl_check_names())
  expect_null(problem)
})

test_that("tbl_check_names() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      .result <- .solution <- tibble::tibble(b = letters[1:3])
      tbl_check_names(object_label = 1)
    }),
    "object_label"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      .result <- .solution <- tibble::tibble(b = letters[1:3])
      tbl_check_names(problem_prefix = NULL)
    }),
    "problem_prefix"
  )
})
