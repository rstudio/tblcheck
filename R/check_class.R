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
#' @param object A vector to be compared to `expected`.
#' @param expected A vector containing the expected result.
#' @param unit `[character(1)]`\cr The label used to describe the object in
#'   feedback messages. Defaults to `"result"`.
#' @param problem_prefix `[character(1)]`\cr The prefix appended to the `problem` label 
#'   in [gradethis::fail()] objects. Defaults to `""`, which appends no prefix.
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
  
  if (!identical(obj_class, exp_class)) {
    class_problem <- problem(paste0(problem_prefix, "class"), exp_class, obj_class)
    
    t_exp_class <- ngettext(length(exp_class), "class", "classes")
    exp_class <- exp_class %>% md_code() %>% knitr::combine_words()
    
    t_obj_class <- ngettext(length(obj_class), "class", "classes")
    obj_class <- obj_class %>% md_code() %>% knitr::combine_words()
    
    return_fail(
      "Your {unit} should have {t_exp_class} {exp_class}, but it has {t_obj_class} {obj_class}.",
      problem = class_problem
    )
  }
}
