#' Checks that two objects have the same classes
#'
#' Checks if `object` and `expected` have the same [class][class()].
#' If the classes differ
#' - `tbl_check_class()` and `vec_check_class()` return a list describing
#'   the problem
#' - `tbl_grade_class()` and `vec_grade_class()` return a failing grade and
#'   informative message with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `class`: The object does not have the expected classes
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @param all_differences `[logical(1)]`\cr If `FALSE`, the default,
#'   inconsequential class differences will be skipped.
#'   If `TRUE`, all class differences will be reported.
#'   See section "Inconsequential differences" for more information.
#' @inheritParams tbl_check_table
#' 
#' @section Inconsequential differences:
#' Unless `all_differences` is set to `TRUE`, the following class differences
#' will not generate a problem:
#' 
#' - [integer] vs. [numeric]
#' - [POSIXct] vs. [POSIXlt]
#' - [glue][glue::glue] vs. [character]
#'
#' @return If there are any issues, a [list] from `tbl_check_class()` and
#'   `vec_check_class()` or a [gradethis::fail()] message from
#'   `tbl_grade_class()` and `vec_grade_class()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' @examples 
#' .result <- 1:10
#' .solution <- as.character(1:10)
#' vec_check_class()
#' vec_grade_class()
#' 
#' .result <- data.frame(a = 1:10)
#' .solution <- tibble::tibble(a = 1:10)
#' tbl_check_class()
#' tbl_grade_class()
#' 
#' .result <- tibble::tibble(a = 1:10, b = a %% 2 == 0)
#' .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a %% 2 == 0), b)
#' tbl_check_class()
#' tbl_grade_class()
tbl_check_class <- function(
  object = .result,
  expected = .solution,
  all_differences = FALSE,
  env = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", env)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", env)
  }
  
  obj_class <- class(object)
  exp_class <- class(expected)
  
  if (!identical(obj_class, exp_class)) {
    if (!all_differences && has_inconsequential_class_diff(obj_class, exp_class)) {
      return(invisible())
    }
    
    problem(
      "class",
      exp_class,
      obj_class,
      # Object lengths are stored so the correct pluralization
      # can be applied in tbl_message.class_problem()
      expected_length = length(expected),
      actual_length = length(object)
    )
  }
}

#' @rdname tbl_check_class
#' @export
vec_check_class <- tbl_check_class

#' @rdname tbl_check_class
#' @export
tbl_grade_class <- function(
  object = .result,
  expected = .solution,
  all_differences = FALSE,
  env = parent.frame()
) {
  tbl_grade(
    tbl_check_class(object, expected, all_differences, env),
    env = env
  )
}

#' @rdname tbl_check_class
#' @export
vec_grade_class <- tbl_grade_class

tbl_message.class_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    "Your result should be {expected}, but it is {actual}."
  
  hinted_class_message <- hinted_class_message(problem$actual, problem$expected)
  if (!is.null(hinted_class_message)) {
    return(hinted_class_message)
  }
  
  problem$expected <- friendly_class(problem$expected, problem$expected_length)
  problem$actual   <- friendly_class(problem$actual,   problem$actual_length)
  
  glue::glue_data(problem, problem$msg)
}

tbl_message.column_class_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    "Your `{column}` column should be {expected}, but it is {actual}."
  
  NextMethod()
}

tbl_message.table_class_problem <- function(problem, ...) {
  problem$msg <- problem$msg %||%
    "Your table should be {expected}, but it is {actual}."
  
  NextMethod()
}

has_inconsequential_class_diff <- function(exp_class, obj_class) {
  diff <- union(setdiff(exp_class, obj_class), setdiff(obj_class, exp_class))
  
  inconsequential_diff_list <- list(
    c("integer", "numeric"),
    c("glue"),
    c("POSIXct", "POSIXlt")
  )
  
  # Check if the differences between `exp_class` and `obj_class` is in the
  # list of inconsequential class differences
  for (inconsequential_diff in inconsequential_diff_list) {
    if (unordered_identical(diff, inconsequential_diff)) {
      return(TRUE)
    }
  }
  
  FALSE
}

hinted_class_message <- function(obj_class, exp_class) {
  list <- hinted_class_message_list()
  
  for (i in seq_along(list)) {
    if (
      all(list[[i]]$obj_class %in% obj_class) &&
      all(list[[i]]$exp_class %in% exp_class)
    ) {
      return(list[[i]]$message)
    }
  }
  
  invisible()
}

hinted_class_message_list <- function() {
  # If `obj_class` or `exp_class` is unspecified, any class will be matched
  list(
    list(
      obj_class = "rowwise_df",
      exp_class = "grouped_df",
      message   = "Your table is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      exp_class = "grouped_df",
      message   = "Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      obj_class = "grouped_df",
      message   = "Your table is a grouped data frame, but I wasn't expecting it to be grouped. Maybe you need to use `ungroup()`?"
    ),
    list(
      exp_class = "rowwise_df",
      message   = "Your table isn't a rowwise data frame, but I was expecting it to be rowwise. Maybe you need to use `rowwise()`?"
    ),
    list(
      obj_class = "rowwise_df",
      message   = "Your table is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?"
    )
  )
}

friendly_class <- function(class, length) {
  list <- friendly_class_list()
  
  for (i in seq_along(list)) {
    if (unordered_identical(list[[i]]$class, class)) {
      if (length > 1) return(list[[i]]$multiple %||% list[[i]]$single)
      return(list[[i]]$single)
    }
  }
  
  class_str <- knitr::combine_words(md_code(class))
  
  glue::glue(
    ifelse(
      length > 1,
      ngettext(
        length(class),
        "a vector with class {class_str}",
        "a vector with classes {class_str}"
      ),
      ngettext(
        length(class),
        "an object with class {class_str}",
        "an object with classes {class_str}"
      )
    )
  )
}

friendly_class_list <- function() {
  list(
    list(
      class    = "character",
      single   = "a text string (class `character`)",
      multiple = "a vector of text (class `character`)"
    ),
    list(
      class    = "numeric",
      single   = "a number (class `numeric`)",
      multiple = "a vector of numbers (class `numeric`)"
    ),
    list(
      class    = "integer",
      single   = "an integer (class `integer`)",
      multiple = "a vector of integers (class `integer`)"
    ),
    list(
      class    = "logical",
      single   = "a TRUE/FALSE value (class `logical`)",
      multiple = "a vector of TRUE/FALSE values (class `logical`)"
    ),
    list(
      class    = "complex",
      single   = "a complex number (class `complex`)",
      multiple = "a vector of complex numbers (class `complex`)"
    ),
    list(
      class    = "raw",
      single   = "a raw byte value (class `raw`)",
      multiple = "a vector of raw byte values (class `raw`)"
    ),
    list(
      class    = "factor",
      single   = "a factor (class `factor`)",
      multiple = "a vector of factors (class `factor`)"
    ),
    list(
      class    = c("POSIXct", "POSIXt"),
      single   = "a date-time (class `POSIXct`)",
      multiple = "a vector of date-times (class `POSIXct`)"
    ),
    list(
      class    = c("POSIXlt", "POSIXt"),
      single   = "a date-time (class `POSIXlt`)",
      multiple = "a vector of date-times (class `POSIXlt`)"
    ),
    list(
      class    = c("tbl_df", "tbl", "data.frame"),
      single   = "a tibble (class `tbl_df`)"
    ),
    list(
      class    = "data.frame",
      single   = "a data frame (class `data.frame`)"
    ),
    list(
      class    = "list",
      single   = "a list (class `list`)"
    ),
    list(
      class    = "matrix",
      single   = "a matrix (class `matrix`)"
    ),
    list(
      class    = c("matrix", "array"),
      single   = "a matrix (class `matrix`)"
    ),
    list(
      class    = "array",
      single   = "an array (class `array`)"
    )
  )
}

# Test that two vectors are identical
# with the exception that they may be in different orders
unordered_identical <- function(x, y) {
  all(x %in% y) && all(y %in% x)
}
