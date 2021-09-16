test_that("grade missing groups", {
  grade_ungrouped <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    tbl_grade_groups()
  })
  
  expect_snapshot(grade_ungrouped)
  
  expect_equal(
    grade_ungrouped$problem,
    problem(
      "groups", missing = "b", unexpected = character(0), location = "table"
    ),
    ignore_attr = "class"
  )
  
  grade_grouped <- tblcheck_test_grade({
    .result <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a), a
    )
    .solution <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a), a, b, c
    )
    tbl_grade_groups()
  })
  
  expect_snapshot(grade_grouped)
  
  expect_equal(
    grade_grouped$problem,
    problem(
      "groups",
      missing = c("b", "c"),
      unexpected = character(0),
      location = "table"
    ),
    ignore_attr = "class"
  )
})

test_that("grade unexpected groups", {
  grade_single <- tblcheck_test_grade({
    .result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    .solution <- tibble::tibble(a = letters[1:3], b = a)
    tbl_grade_groups()
  })
  
  expect_snapshot(grade_single)
  
  expect_equal(
    grade_single$problem,
    problem(
      "groups", missing = character(0), unexpected = "b", location = "table"
    ),
    ignore_attr = "class"
  )
  
  grade_multiple <- tblcheck_test_grade({
    .result <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a), a, b, c
    )
    .solution <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a), a
    )
    tbl_grade_groups()
  })
  
  expect_snapshot(grade_multiple)
  
  expect_equal(
    grade_multiple$problem,
    problem(
      "groups",
      missing = character(0),
      unexpected = c("b", "c"),
      location = "table"
    ),
    ignore_attr = "class"
  )
})

test_that("grade missing and unexpected groups", {
  grade <- tblcheck_test_grade({
    .result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), a)
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    tbl_grade_groups()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("groups", missing = "b", unexpected = "a", location = "table"),
    ignore_attr = "class"
  )
})

test_that("grade groups max_diffs()", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    .solution <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
    )
    tbl_grade_groups()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    tbl_check_groups(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a),
      dplyr::group_by(
        tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
      )
    ),
    ignore_attr = "class"
  )
  
  grade_inf <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    .solution <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
    )
    tbl_grade_groups(max_diffs = Inf)
  })
  
  expect_snapshot(grade_inf)
  
  expect_equal(
    grade_inf$problem,
    tbl_check_groups(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a),
      dplyr::group_by(
        tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
      )
    ),
    ignore_attr = "class"
  )
  
  grade_one <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
    .solution <- dplyr::group_by(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
    )
    tbl_grade_groups(max_diffs = 1)
  })
  
  expect_snapshot(grade_one)
  
  expect_equal(
    grade_one$problem,
    tbl_check_groups(
      tibble::tibble(a = letters[1:3], b = a, c = a, d = a),
      dplyr::group_by(
        tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
      )
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_groups() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(a = letters[1:3], b = a)
  
  grade <- expect_invisible(tbl_grade_groups())
  expect_null(grade)
  
  problem <- expect_invisible(tbl_check_groups())
  expect_null(problem)
  
  .result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
  .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
  
  grade <- expect_invisible(tbl_grade_groups())
  expect_null(grade)
  
  problem <- expect_invisible(tbl_check_groups())
  expect_null(problem)
})

test_that("tbl_grade_groups() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters[1:3], b = a)
      .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
      tbl_grade_groups(max_diffs = "a")
    }),
    "max_diffs"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters[1:3], b = a)
      .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
      tbl_grade_groups(max_diffs = -1)
    }),
    "max_diffs"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      .result   <- tibble::tibble(a = letters[1:3], b = a)
      .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
      tbl_grade_groups(max_diffs = 1:2)
    }),
    "max_diffs"
  )
})
