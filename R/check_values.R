#' Checks that two vectors are contain the same values
#'
#' Check if two vectors contain the same values.
#' If the values differ
#' - `vec_check_values()` returns a list describing the problem
#' - `vec_grade_values()` returns a failing grade and informative message
#' with [gradethis::fail()]
#'
#' @section Problems:
#'
#' 1. `values`: `object` doesn't contain the same values as `expected`
#'
#' @examples
#' .result <- 1:10
#' .solution <- letters[1:10]
#' vec_check_values()
#' vec_grade_values()
#'
#' .result <- 1:10
#' .solution <- 1:11
#' vec_check_values()
#' vec_grade_values()
#'
#' .result <- 1:10
#' .solution <- rlang::set_names(1:10, letters[1:10])
#' vec_check_values()
#' vec_grade_values()
#' vec_grade_values(max_diffs = 5)
#' vec_grade_values(max_diffs = Inf)
#'
#' .result <- 1:10
#' .solution <- 11:20
#' vec_check_values()
#' vec_grade_values()
#' vec_grade_values(max_diffs = 5)
#' vec_grade_values(max_diffs = Inf)
#' @inheritParams vec_check
#' @inheritDotParams gradethis::fail -message
#' @param tolerance `[numeric(1) ≥ 0]`\cr `values` differences smaller than
#'   `tolerance` are ignored. The default value is close to `1.5e-8`.
#'
#' @return If there are any issues, a [list] from `vec_check_values()` or a
#'   [gradethis::fail()] message from `vec_grade_values()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
vec_check_values <- function(
  object = .result,
  expected = .solution,
  tolerance = sqrt(.Machine$double.eps),
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }

  return_if_internal_problem({
    checkmate::assert_vector(object)
    checkmate::assert_vector(expected)
  })

  # Check if values are comparable types
  if (!has_common_ptype(object, expected)) {
    return_if_problem(vec_check_class(object, expected))

    return(problem("values"))
  }

  # Check if values are the same length
  return_if_problem(vec_check_dimensions(object, expected))

  # Check if values are the same
  if (!is_all_equal(object, expected, tolerance = tolerance)) {
    return(problem("values", expected, object))
  }
}

#' @rdname vec_check_values
#' @export
vec_grade_values <- function(
  object = .result,
  expected = .solution,
  tolerance = sqrt(.Machine$double.eps),
  max_diffs = 3,
  env = parent.frame(),
  ...
) {
  problem_grade(
    vec_check_values(
      object = object,
      expected = expected,
      tolerance = tolerance,
      env = env
    ),
    max_diffs = max_diffs,
    env = env,
    ...
  )
}

#' @export
problem_message.values_problem <- function(problem, max_diffs = 3, ...) {
  # If values problem is empty, return vague message
  if (is.null(problem$actual) && is.null(problem$expected)) {
    if (is_problem(problem, "column")) {
      problem$msg <- problem$msg %||%
        "Your `{column}` column contains unexpected values."
    }

    problem$msg <- problem$msg %||%
      "Your result contains unexpected values."

    return(glue::glue_data(problem, problem$msg))
  }

  # First, alert the user if the first `n` values do not match
  problem$n_values <- min(
    max(length(problem$expected), length(problem$actual)),
    max_diffs
  )

  first_n_values_are_equal <-
    vctrs::vec_equal(
      problem$expected[seq_len(problem$n_values)],
      problem$actual[seq_len(problem$n_values)],
      na_equal = TRUE
    )

  if (!all(first_n_values_are_equal)) {
    problem$expected <- knitr::combine_words(
      md_code(problem$expected[seq_len(problem$n_values)])
    )
    problem$actual <- knitr::combine_words(
      md_code(problem$actual[seq_len(problem$n_values)])
    )

    problem$expected_msg <- problem$expected_msg %||%
      if (is_problem(problem, "column")) {
        ngettext(
          problem$n_values,
          "The first value of your `{column}` column should be {expected}, not {actual},",
          "The first {n_values} values of your `{column}` column should be {expected},"
        )
      } else {
        ngettext(
          problem$n_values,
          "The first value of your result should be {expected},",
          "The first {n_values} values of your result should be {expected},"
        )
      }

    problem$actual_message <- problem$actual_message %||%
      " not {actual}."

    return(
      glue::glue_data(problem, problem$expected_msg, problem$actual_message)
    )
  }

  # Next, alert if there are values in `actual` that aren't in `expected`
  problem$unexpected <- setdiff(problem$actual, problem$expected)

  if (length(problem$unexpected)) {
    problem$unexpected <- problem$unexpected[
      seq_len(min(max_diffs, length(problem$unexpected)))
    ]

    problem$msg <-
      if (is_problem(problem, "column")) {
        ngettext(
          length(problem$unexpected),
          "I didn't expect your `{column}` column to include the value {unexpected}.",
          "I didn't expect your `{column}` column to include the values {unexpected}."
        )
      } else {
        ngettext(
          length(problem$unexpected),
          "I didn't expect your result to include the value {unexpected}.",
          "I didn't expect your result to include the values {unexpected}."
        )
      }

    problem$unexpected <- knitr::combine_words(md_code(problem$unexpected))

    return(glue::glue_data(problem, problem$msg))
  }

  # Next, alert if there are values in `expected` that aren't in `actual`
  problem$missing <- setdiff(problem$expected, problem$actual)

  if (length(problem$missing)) {
    problem$missing <- problem$missing[
      seq_len(min(max_diffs, length(problem$missing)))
    ]
    problem$msg <-
      if (is_problem(problem, "column")) {
        ngettext(
          length(problem$missing),
          "I expected your `{column}` column to include the value {missing}.",
          "I expected your `{column}` column to include the values {missing}."
        )
      } else {
        ngettext(
          length(problem$missing),
          "I expected your result to include the value {missing}.",
          "I expected your result to include the values {missing}."
        )
      }

    problem$missing <- knitr::combine_words(md_code(problem$missing))

    return(glue::glue_data(problem, problem$msg))
  }

  # If all else fails, return vague message
  problem_message(problem("values"))
}
