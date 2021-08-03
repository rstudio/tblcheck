#' Checks that two objects have the same classes
#'
#' Checks if `object` and `expected` have the same [class][class()].
#' If the classes differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `class`: `object` doesn't have the same classes as `expected`
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @param unit `[character(1)]`\cr The label used to describe the object in
#'   feedback messages. Defaults to `"result"`.
#' @param problem_prefix `[character(1)]`\cr The prefix appended to the
#'   `problem` label in [gradethis::fail()] objects.
#'   Defaults to `""`, which appends no prefix.
#'
#' @inherit check_table return
#' @export


check_class <- function(
  object = .result, expected = .solution, unit = "result", problem_prefix = ""
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  obj_class <- class(object)
  exp_class <- class(expected)
  
  assert_internally({
    checkmate::assert_string(unit)
    checkmate::assert_string(problem_prefix)
  })
  
  if (!identical(obj_class, exp_class)) {
    if (!has_meaningful_class_difference(exp_class, obj_class)) {
      return()
    }
    
    class_problem <- problem(paste0(problem_prefix, "class"), exp_class, obj_class)
    
    hinted_class_message <- hinted_class_message(obj_class, exp_class)
    if (!is.null(hinted_class_message)) {
      return_fail(hinted_class_message, problem = class_problem)
    }
    
    friendly_exp_class <- friendly_class(exp_class, expected)
    friendly_obj_class <- friendly_class(obj_class, object)
    message <- glue::glue(
      "Your {unit} should be {friendly_exp_class}, but it is {friendly_obj_class}."
    )
    
    return_fail(message, problem = class_problem)
  }
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

friendly_class <- function(class, x) {
  friendly_class_list <- friendly_class_list()
  
  for (i in seq_along(friendly_class_list)) {
    list <- friendly_class_list[[i]]
    
    if (unordered_identical(list$class, class)) {
      if (length(x) > 1) return(list$multiple %||% list$single)
      return(list$single)
    }
  }
  
  class_str <- knitr::combine_words(md_code(class))
  
  glue::glue(
    ifelse(
      length(x) > 1,
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
      single   = "an TRUE/FALSE value (class `integer`)",
      multiple = "a vector of TRUE/FALSE values (class `integer`)"
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
    )
  )
}

hinted_class_message <- function(obj_class, exp_class) {
  hinted_class_message_list <- hinted_class_message_list()
  
  for (i in seq_along(hinted_class_message_list)) {
    list <- hinted_class_message_list[[i]]
    
    if (
      list$obj_test(list$obj_class, obj_class) &&
      list$exp_test(list$exp_class, exp_class)
    ) {
      return(list$message)
    }
  }
  
  invisible()
}

hinted_class_message_list <- function() {
  list(
    list(
      obj_class = "rowwise_df",
      obj_test  = `%in%`,
      exp_class = "grouped_df",
      exp_test  = `%in%`,
      message   = "Your {unit} is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      obj_test  = function(...) TRUE,
      exp_class = "grouped_df",
      exp_test  = `%in%`,
      message   = "Your {unit} isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      obj_class = "grouped_df",
      obj_test  = `%in%`,
      exp_test  = function(...) TRUE,
      message   = "Your {unit} is a grouped data frame, but I wasn't expecting it to be grouped. Maybe you need to use `ungroup()`?"
    ),
    list(
      obj_test  = function(...) TRUE,
      exp_class = "rowwise_df",
      exp_test  = `%in%`,
      message   = "Your {unit} isn't a rowwise data frame, but I was expecting it to be rowwise. Maybe you need to use `rowwise()`?"
    ),
    list(
      obj_class = "rowwise_df",
      obj_test  = `%in%`,
      exp_test  = function(...) TRUE,
      message   = "Your {unit} is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?"
    )
  )
}

# Test that two vectors are identical
# with the exception that they may be in different orders
unordered_identical <- function(x, y) {
  all(x %in% y) && all(y %in% x)
}
