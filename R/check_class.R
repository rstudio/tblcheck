#' Checks that two objects have the same classes
#'
#' Checks if `object` and `expected` have the same [class][class()].
#' If the classes differ
#' - `tbl_check_class()` returns a list describing the problem
#' - `tbl_grade_class()` returns a failing grade and informative message
#' with [gradethis::fail()]
#' 
#' @section Problems:
#' 
#' 1. `class`: The object does not have the expected classes
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @inheritParams tbl_check_table
#'
#' @return If there are any issues, a [list] from `tbl_check_class()` or a
#'   [gradethis::fail()] message from `tbl_grade_class()`.
#'   Otherwise, invisibly returns [`NULL`].
#' @export
#' @examples 
#' .result <- 1:10
#' .solution <- as.character(1:10)
#' tbl_check_class()
#' tbl_grade_class()
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
  object = .result, expected = .solution, envir = parent.frame()
) {
  if (inherits(object, ".result")) {
    object <- get(".result", envir)
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", envir)
  }
  
  obj_class <- class(object)
  exp_class <- class(expected)
  
  if (!identical(obj_class, exp_class)) {
    problem <- problem(
      "class",
      exp_class,
      obj_class,
      # Object lengths are stored so the correct pluralization
      # can be applied in tbl_message.class_problem()
      expected_length = length(expected),
      actual_length = length(object)
    )
    
    if (!has_meaningful_class_difference(obj_class, exp_class)) {
      problem$message <- FALSE
    }
    
    return(problem)
  }
}

#' @rdname tbl_check_class
#' @export
tbl_grade_class <- function(
  object = .result, expected = .solution, envir = parent.frame()
) {
  return_if_graded(
    tbl_grade(tbl_check_class(object, expected, envir))
  )
}

tbl_message.class_problem <- function(problem, ...) {
  glue::glue_data(
    build_class_message_list(problem),
    "Your result should be {friendly_exp_class}, but it is {friendly_obj_class}."
  )
}

tbl_message.column_class_problem <- function(problem, ...) {
  column_name <- problem$column
  
  glue::glue_data(
    build_class_message_list(problem),
    "Your `{column_name}` column should be {friendly_exp_class}, but it is {friendly_obj_class}."
  )
}

tbl_message.table_class_problem <- function(problem, ...) {
  glue::glue_data(
    build_class_message_list(problem),
    "Your table should be {friendly_exp_class}, but it is {friendly_obj_class}."
  )
}

build_class_message_list <- function(problem, env = parent.frame()) {
  exp_class <- problem$expected
  obj_class <- problem$actual
  
  hinted_class_message <- hinted_class_message(obj_class, exp_class)
  if (!is.null(hinted_class_message)) {
    rlang::return_from(env, hinted_class_message)
  }
  
  exp_length <- problem$expected_length
  obj_length <- problem$actual_length
  
  list(
    friendly_exp_class = friendly_class(exp_class, exp_length),
    friendly_obj_class = friendly_class(obj_class, obj_length)
  )
}

has_meaningful_class_difference <- function(exp_class, obj_class) {
  differences <- union(setdiff(exp_class, obj_class), setdiff(obj_class, exp_class))
  
  insignificant_class_differences <- list(
    c("integer", "numeric"),
    c("glue"),
    c("POSIXct", "POSIXlt")
  )
  
  # Check that the differences between `exp_class` and `obj_class` is not in the
  # list of insignificant class differences
  !any(
    purrr::map_lgl(
      insignificant_class_differences,
      unordered_identical,
      differences
    )
  )
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
