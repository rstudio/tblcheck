catch_internal_problem <- function(expr, ...) {
  tryCatch(expr, ..., error = function(err) {
    message("An error occurred in the grading code: ", err$message)
    problem("tblcheck_internal", error = err$message)
  })
}

return_if_internal_problem <- function(expr, ..., env = parent.frame()) {
  return_if_problem(catch_internal_problem(expr, ...), env = env)
}

tbl_grade.tblcheck_internal_problem <- function(
  problem, max_diffs = 3, env = parent.frame(), ...
) {
  gradethis::graded(
    message = paste(
      "Uh-oh! We can't provide feedback at this time. Don't worry, it's not", 
      "your fault! There's an issue behind-the-scenes with this exercise."
    ),
    correct = logical(0),
    type = "warning",
    location = "replace",
    problem = problem
  )
}
