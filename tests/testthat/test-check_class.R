test_that("tbl_grade_class()", {
  grade_num_chr_1 <-
    tblcheck_test_grade({
      .result <- "1"
      .solution <- 1
      tbl_grade_class()
    })

  expect_snapshot(grade_num_chr_1)

  expect_equal(
    grade_num_chr_1$problem,
    problem(
      type = "class",
      expected = "numeric",
      actual = "character",
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_num_chr_2 <-
    tblcheck_test_grade({
      .result <- c("1", "2")
      .solution <- c(1, 2)
      tbl_grade_class()
    })

  expect_snapshot(grade_num_chr_2)

  expect_equal(
    grade_num_chr_2$problem,
    problem(
      type = "class",
      expected = "numeric",
      actual = "character",
      expected_length = 2,
      actual_length = 2
    )
  )

  grade_posixct_1 <-
    tblcheck_test_grade({
      .result <- "2021-07-29 10:59:59"
      .solution <- as.POSIXct("2021-07-29 10:59:59")
      tbl_grade_class()
    })

  expect_snapshot(grade_posixct_1)

  expect_equal(
    grade_posixct_1$problem,
    problem(
      type = "class",
      expected = c("POSIXct", "POSIXt"),
      actual = "character",
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_posixct_2 <-
    tblcheck_test_grade({
      .result <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
      .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
      tbl_grade_class()
    })

  expect_snapshot(grade_posixct_2)

  expect_equal(
    grade_posixct_2$problem,
    problem(
      type = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
})

test_that("tbl_grade_class() ignore classes", {
  grade_glue_chr <-
    tblcheck_test_grade({
      .result <- glue::glue("x")
      .solution <- "x"
      tbl_grade_class()
    })

  expect_snapshot(grade_glue_chr)

  expect_equal(
    grade_glue_chr$problem,
    problem(
      type = "class",
      expected = "character",
      actual = c("glue", "character"),
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_glue_chr_ignore <-
    tblcheck_test_grade({
      .result <- glue::glue("x")
      .solution <- "x"
      tbl_grade_class(ignore_class = "glue")
    })

  expect_null(grade_glue_chr_ignore)

  grade_tbl_df <-
    tblcheck_test_grade({
      .result <- data.frame(a = 1, b = 2)
      .solution <- tibble::tibble(a = 1, b = 2)
      tbl_grade_class()
    })

  expect_snapshot(grade_tbl_df)

  expect_equal(
    grade_tbl_df$problem,
    problem(
      type = "class",
      expected = c("tbl_df", "tbl", "data.frame"),
      actual = c("data.frame"),
      expected_length = 2,
      actual_length = 2
    )
  )

  grade_py_tbl_df <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = c(1, 2), b = c(3, 4))
      class(.result) <- c("py_tbl_df", class(.result))
      .solution   <- tibble::tibble(a = c(1, 2), b = c(3, 4)) %>%
        dplyr::group_by(a)
      class(.solution) <- c("py_grouped_df", "py_tbl_df", class(.solution))
      tbl_grade_class()
    })

  expect_snapshot(grade_py_tbl_df)

  expect_equal(
    grade_py_tbl_df$problem,
    problem(
      type     = "class",
      expected = c(
        "py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"
      ),
      actual   = c("py_tbl_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    )
  )

  grade_py_tbl_df_grouped <-
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = c(1, 2), b = c(3, 4)) %>%
        dplyr::group_by(a)
      class(.result) <- c("py_grouped_df", "py_tbl_df", class(.result))
      .solution   <- tibble::tibble(a = c(1, 2), b = c(3, 4))
      class(.solution) <- c("py_tbl_df", class(.solution))
      tbl_grade_class()
    })

  expect_snapshot(grade_py_tbl_df_grouped)

  expect_equal(
    grade_py_tbl_df_grouped$problem,
    problem(
      type     = "class",
      expected   = c("py_tbl_df", "tbl_df", "tbl", "data.frame"),
      actual = c(
        "py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"
      ),
      expected_length = 2,
      actual_length = 2
    )
  )

  grade_tbl_df_ignore <-
    tblcheck_test_grade({
      .result <- data.frame(a = 1, b = 2)
      .solution <- tibble::tibble(a = 1, b = 2)
      tbl_grade_class(ignore_class = c("tbl_df", "tbl"))
    })

  expect_null(grade_tbl_df_ignore)
})

test_that("tbl_grade_class() with paired ignore_class", {
  grade_int_dbl <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1
      tbl_grade_class()
    })

  expect_snapshot(grade_int_dbl)

  expect_equal(
    grade_int_dbl$problem,
    problem(
      type = "class",
      expected = "numeric",
      actual = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_int_dbl_ignore <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1
      tbl_grade_class(ignore_class = c("integer" = "numeric"))
    })

  expect_null(grade_int_dbl_ignore)

  grade_int_chr_wrong_ignore <-
    tblcheck_test_grade({
      .result <- "1"
      .solution <- 1
      tbl_grade_class(ignore_class = c("integer" = "numeric"))
    })

  expect_snapshot(grade_int_chr_wrong_ignore)

  expect_equal(
    grade_int_chr_wrong_ignore$problem,
    problem(
      type = "class",
      expected = "numeric",
      actual = "character",
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_posix_ct_lt <-
    tblcheck_test_grade({
      .result <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
      .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
      tbl_grade_class()
    })

  expect_snapshot(grade_posix_ct_lt)

  expect_equal(
    grade_posix_ct_lt$problem,
    problem(
      type = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual = c("POSIXct", "POSIXt"),
      expected_length = 2,
      actual_length = 2
    )
  )

  grade_posix_ct_lt_ignore <-
    tblcheck_test_grade({
      .result <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
      .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
      tbl_grade_class(ignore_class = c("POSIXct" = "POSIXlt"))
    })

  expect_null(grade_posix_ct_lt_ignore)
})

test_that("tbl_grade_class() with multiple paired ignore_class", {
  grade_int_dbl_ignore <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1
      tbl_grade_class(
        ignore_class = c("numeric" = "integer", "character" = "numeric")
      )
    })

  expect_null(grade_int_dbl_ignore)
})

test_that("tbl_grade_class() with ignore_class leaving NULL solution class", {
  grade_int_dbl_ignore <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1
      tbl_grade_class(ignore_class = c("numeric"))
    })

  expect_null(grade_int_dbl_ignore)

  grade_chr_dbl_ignore <-
    tblcheck_test_grade({
      .result <- "1"
      .solution <- 1
      tbl_grade_class(ignore_class = c("numeric"))
    })

  expect_null(grade_int_dbl_ignore)

  grade_tbl_dbl_ignore <-
    tblcheck_test_grade({
      .result <- tibble::tibble(a = 1)
      .solution <- 1
      tbl_grade_class(ignore_class = c("numeric"))
    })

  expect_null(grade_int_dbl_ignore)
})

test_that("tbl_grade_class() with multiple classes", {
  grade_class_solution <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1L
      class(.solution) <- c("test", "class", "integer")
      tbl_grade_class()
    })

  expect_snapshot(grade_class_solution)

  expect_equal(
    grade_class_solution$problem,
    problem(
      type = "class",
      expected = c("test", "class", "integer"),
      actual = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )

  grade_class_result <-
    tblcheck_test_grade({
      .result <- 1L
      class(.result) <- c("test", "class", "integer")
      .solution <- 1L
      tbl_grade_class()
    })

  expect_snapshot(grade_class_result)

  expect_equal(
    grade_class_result$problem,
    problem(
      type = "class",
      expected = "integer",
      actual = c("test", "class", "integer"),
      expected_length = 1,
      actual_length = 1
    )
  )
})

test_that("tbl_grade_class() with classes in different orders", {
  grade <-
    tblcheck_test_grade({
      .result <- 1L
      .solution <- 1L
      class(.result) <- c("test", "class", "integer")
      class(.solution) <- c("class", "test", "integer")
      tbl_grade_class()
    })

  expect_null(grade)
})

test_that("tbl_grade_class() with hinted messages", {
  grade_ungrouped <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    tbl_grade_class()
  })

  expect_snapshot(grade_ungrouped)

  expect_equal(
    grade_ungrouped$problem,
    problem(
      "class",
      expected = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      actual = c("tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )

  grade_grouped <- tblcheck_test_grade({
    .result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_class()
  })

  expect_snapshot(grade_grouped)

  expect_equal(
    grade_grouped$problem,
    problem(
      "class",
      expected = c("tbl_df", "tbl", "data.frame"),
      actual = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )

  grade_ungrouped_int <- tblcheck_test_grade({
    .result   <- 1:2
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    tbl_grade_class()
  })

  expect_snapshot(grade_ungrouped_int)

  expect_equal(
    grade_ungrouped_int$problem,
    problem(
      "class",
      expected = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      actual = "integer",
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )

  grade_unrowwise_int <- tblcheck_test_grade({
    .result   <- 1:2
    .solution <- dplyr::rowwise(tibble::tibble(a = letters[1:3], b = a))
    tbl_grade_class()
  })

  expect_snapshot(grade_unrowwise_int)

  expect_equal(
    grade_unrowwise_int$problem,
    problem(
      "class",
      expected = c("rowwise_df", "tbl_df", "tbl", "data.frame"),
      actual = "integer",
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )

  grade_pyungrouped_int <- tblcheck_test_grade({
    .result <- 1
    class(.result) <- "int"
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    class(.solution) <- c("py_tbl_df", class(.solution))
    tbl_grade_class()
  })
  expect_snapshot(grade_pyungrouped_int)

  expect_equal(
    grade_pyungrouped_int$problem,
    problem(
      type     = "class",
      expected = c("py_tbl_df", "tbl_df", "tbl", "data.frame"),
      actual   = "int",
      expected_length = 2,
      actual_length = 1
    )
  )

  grade_pygrouped_int <- tblcheck_test_grade({
    .result <- 1
    class(.result) <- "int"
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    class(.solution) <- c("py_grouped_df", "py_tbl_df", class(.solution))
    tbl_grade_class()
  })
  expect_snapshot(grade_pygrouped_int)

  expect_equal(
    grade_pygrouped_int$problem,
    problem(
      type     = "class",
      expected = c(
        "py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"
      ),
      actual   = "int",
      expected_length = 2,
      actual_length = 1
    )
  )
})
