test_that("tbl_grade_column() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = 1:3)
    tbl_grade_column("a", .result, .solution)
  })
  
  expect_grade(
    grade,
    "Your `a` column should be a vector of integers (class `integer`), but it is a vector of text (class `character`).",
    problem = tbl_check_column(
      "a", tibble::tibble(a = letters), tibble::tibble(a = 1:3)
    ),
    fixed = TRUE
  )
})

test_that("tbl_grade_column() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = rev(letters))
    .solution <- tibble::tibble(a = letters)
    tbl_grade_column("a")
  })
  
  expect_grade(
    grade,
    "The first 3 values of your `a` column should be `a`, `b`, and `c",
    problem = tbl_check_column(
      "a", tibble::tibble(a = rev(letters)), tibble::tibble(a = letters)
    )
  )
})

test_that("tbl_grade_column() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = data.frame(x = 1))
    .solution <- tibble::tibble(a = tibble::tibble(x = 1))
    tbl_grade_column("a")
  })
  
  expect_grade(
    grade,
    "Your `a` column should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`).",
    problem = tbl_check_column(
      "a",
      tibble::tibble(a = data.frame(x = 1)),
      tibble::tibble(a = tibble::tibble(x = 1))
    ),
    fixed = TRUE
  )
})

test_that("tbl_grade_column() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = c(rep(1, 3), 5:10))
    .solution <- tibble::tibble(a = c(rep(1, 3), 10:15))
    tbl_grade_column("a")
  })
  
  expect_grade(
    grade,
    "Your `a` column contains unexpected values.",
    problem = tbl_check_column(
      "a",
      tibble::tibble(a = c(rep(1, 3), 5:10)),
      tibble::tibble(a = c(rep(1, 3), 10:15))
    )
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = rev(letters))
    tbl_grade_column("a", max_diffs = 5)
  })
  
  expect_grade(
    grade,
    "The first 5 values of your `a` column should be `z`, `y`, `x`, `w`, and `v",
    problem = tbl_check_column(
      "a",
      tibble::tibble(a = letters),
      tibble::tibble(a = rev(letters)),
      max_diffs = 5
    )
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:2])
    .solution <- tibble::tibble(a = letters[2:1])
    tbl_grade_column("a", max_diffs = 3)
  })

  expect_grade(
    grade,
    message = "`b` and `a`.",
    problem = tbl_check_column(
      "a",
      tibble::tibble(a = letters[1:2]),
      tibble::tibble(a = letters[2:1])
    )
  )
  expect_no_match(grade$message, "`NA`")
})

test_that("checks that columns have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:4])
    tbl_grade_column("a")
  })
  
  expect_grade(
    grade,
    message = "should contain 4 values",
    problem = tbl_check_column(
      "a",
      tibble::tibble(a = letters[1:3]),
      tibble::tibble(a = letters[1:4])
    )
  )
})

test_that("checks that the column is present in object", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(b = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_column("a")
  })
  
  expect_grade(
    grade,
    message = "should have a column named `a`",
    problem = tbl_check_column(
      "a",
      tibble::tibble(b = letters[1:3]),
      tibble::tibble(a = letters[1:3])
    )
  )
})

test_that("checks that the column is present in expected", {
  grade <- expect_warning(
    tblcheck_test_grade({
      .result   <- tibble::tibble(b = letters[1:3])
      .solution <- tibble::tibble(a = letters[1:3])
      tbl_grade_column("b")
    }),
    "`b` is not a column in `expected`"
  )

  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("tbl_grade_column() with no problems returns invisible()", {
  grade <- expect_null(
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters[1:3])
      .solution <- tibble::tibble(a = letters[1:3])
      tbl_grade_column("a")
    })
  )
  
  expect_invisible(
    tbl_grade_column("a", tibble::tibble(a = 1), tibble::tibble(a = 1))
  )

  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("tbl_grade_column() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column(3, object = result, expected = solution)
    }),
    "name"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column(c("a", "b"), object = result, expected = solution)
    }),
    "name"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_values = "yes")
    }),
    "check_values"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, max_diffs = 1:3)
    }),
    "max_diffs"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = 12, expected = solution)
    }),
    "object"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = list(a = 1))
    }),
    "expected"
  )
})
