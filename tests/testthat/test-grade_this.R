test_that("rlang_call_match() with {rlang} > 0.4.12", {
  use_call_match <- function(a = 1, b = 3) {
    rlang_call_match(n = 1)
  }

  expect_equal(
    rlang::expr_text(use_call_match(a = 12)),
    "use_call_match(a = 12, b = 3)"
  )
  
  expect_equal(
    rlang::expr_text(use_call_match(b = 4)),
    "use_call_match(a = 1, b = 4)"
  )
  
  expect_equal(
    rlang::expr_text(use_call_match()),
    "use_call_match(a = 1, b = 3)"
  )
})

test_that("rlang_call_match() with {rlang} <= 0.4.12", {
  use_call_match <- function(a = 1, b = 3) {
    rlang_call_match(n = 1)
  }
  
  # mock usage of older version of rlang
  mockery::stub(rlang_call_match, "has_rlang_version", FALSE)
  
  expect_equal(
    rlang::expr_text(use_call_match(a = 12)),
    "use_call_match(a = 12, b = 3)"
  )
  
  expect_equal(
    rlang::expr_text(use_call_match(b = 4)),
    "use_call_match(a = 1, b = 4)"
  )
  
  expect_equal(
    rlang::expr_text(use_call_match()),
    "use_call_match(a = 1, b = 3)"
  )
})
