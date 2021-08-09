tbl_grade <- function(problem, unit = "result", max_diffs = 3) {
  if (is.null(problem)) {
    return(invisible())
  }
  
  assert_internally({
    checkmate::assert_class(problem, "tblcheck_problem")
    checkmate::assert_string(unit)
    checkmate::assert_number(max_diffs, lower = 1)
  })
  
  if (grepl("class$", problem$type)) {
    return(tbl_message_class(problem))
  }
  if (grepl("names$", problem$type)) {
    return(tbl_message_names(problem, max_diffs))
  }
  if (grepl("length$", problem$type)) {
    return(tbl_message_length(problem))
  }
}
