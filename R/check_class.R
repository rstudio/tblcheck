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
#'
#' # Ignore the difference between tibble and data frame
#' .result <- data.frame(a = 1:10)
#' .solution <- tibble::tibble(a = 1:10)
#' tbl_check_class(ignore_class = c("tbl_df", "tbl"))
#' tbl_grade_class(ignore_class = c("tbl_df", "tbl"))
#'
#' # Ignore the difference between integer and double
#' .result <- 1L
#' .solution <- 1
#' vec_check_class(ignore_class = c("integer" = "numeric"))
#' vec_grade_class(ignore_class = c("integer" = "numeric"))
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @param ignore_class `[character()]`\cr A vector of classes to ignore when
#'   finding differences between `object` and `expected`.
#'
#'   If an element is named, differences will only be ignored between the pair
#'   of the element and its name.
#'   For example, `ignore_class = c("integer" = "numeric")` will ignore class
#'   differences only if `object` has class [integer] and `expected` has class
#'   [numeric], or vice versa.
#'
#'   If all the classes of `expected` are included in `ignore_class`,
#'   a `class` problem will never be returned.
#' @inheritParams tbl_check
#' @inheritDotParams gradethis::fail -message
#'
#' @return If there are any issues, a [list] from `tbl_check_class()` and
#'   `vec_check_class()` or a [gradethis::fail()] message from
#'   `tbl_grade_class()` and `vec_grade_class()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
tbl_check_class <- function(
  object = .result,
  expected = .solution,
  ignore_class = NULL,
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

  paired <- rlang::names2(ignore_class) != ""

  obj_class_ignored <- setdiff(obj_class, ignore_class[!paired])
  exp_class_ignored <- setdiff(exp_class, ignore_class[!paired])

  if (length(exp_class_ignored) == 0) {
    return(invisible())
  }

  # Replace classes that match named elements of `ignore_class` with the
  # element's name. This allows us to ignore differences between the element
  # class and the name class.
  for (i in seq_along(ignore_class[paired])) {
    obj_class_ignored[obj_class_ignored == ignore_class[paired][[i]]] <-
      names(ignore_class[paired])[[i]]
    exp_class_ignored[exp_class_ignored == ignore_class[paired][[i]]] <-
      names(ignore_class[paired])[[i]]
  }

  if (!setequal(obj_class_ignored, exp_class_ignored)) {
    problem(
      "class",
      exp_class,
      obj_class,
      # Object lengths are stored so the correct pluralization
      # can be applied in tblcheck_message.class_problem()
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
  ignore_class = NULL,
  env = parent.frame(),
  ...
) {
  tblcheck_grade(
    tbl_check_class(object, expected, ignore_class, env),
    env = env,
    ...
  )
}

#' @rdname tbl_check_class
#' @export
vec_grade_class <- tbl_grade_class

#' @export
tblcheck_message.class_problem <- function(problem, ...) {
  if (is_problem(problem, "column")) {
    problem$msg <- problem$msg %||%
      "Your `{column}` column should be {expected}, but it is {actual}."
  } else if (is_problem(problem, "table")) {
    problem$msg <- problem$msg %||%
      "Your table should be {expected}, but it is {actual}."
  }

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
      exp_class = "py_grouped_df",
      message   = "I was only expecting 1 value for each grouping in the table, but you have multiple values per grouping. Maybe you are missing a .groupby() call?"
    ),
    list(
      obj_class = "py_grouped_df",
      message   = "Your table rows (i.e. index) are not a numbered sequence. You can tell by the extra spacing around the column names. You can fix this with .reset_index()"
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
      class    = c("py_tbl_df", "tbl_df", "tbl", "data.frame"),
      single   = "a DataFrame"
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
