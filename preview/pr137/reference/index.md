# Package index

## Exercise Checking Functions

Fully automated table or vector checking. Choose one of these functions
to use in the `*-check` chunk of your exercise.

- [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
  : Grade this table
- [`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
  : Grade this vector

## General checks

Check for multiple problems in a table, column, or vector

- [`tbl_check()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
  [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
  : Check that the rows and columns of two tables are the same
- [`tbl_check_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
  [`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
  : Checks that a column is identical across two tables
- [`vec_check()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
  [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
  : Checks that two vectors are the same

## Specific checks

Check for a specific type of problem in tables or vectors

- [`tbl_check_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md)
  [`vec_check_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md)
  [`tbl_grade_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md)
  [`vec_grade_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md)
  : Checks that two objects have the same classes
- [`tbl_check_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  [`vec_check_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  [`vec_check_length()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  [`tbl_grade_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  [`vec_grade_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  [`vec_grade_length()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)
  : Check that the dimensions of two object are the same
- [`tbl_check_groups()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_groups.md)
  [`tbl_grade_groups()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_groups.md)
  : Check that the groups of two object are the same
- [`tbl_check_is_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_is_table.md)
  [`tbl_grade_is_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_is_table.md)
  : Checks that an object is a table
- [`vec_check_levels()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_levels.md)
  [`vec_grade_levels()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_levels.md)
  : Check that the levels of two factors are the same
- [`tbl_check_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)
  [`vec_check_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)
  [`tbl_grade_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)
  [`vec_grade_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)
  : Check that the names of two object are the same
- [`vec_check_values()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_values.md)
  [`vec_grade_values()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_values.md)
  : Checks that two vectors are contain the same values

## Problems

Inspect the contents of a `problem` from a check function or transform a
`problem` into a grade.

- [`problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem.md)
  : Declare a problem
- [`problem_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_grade.md)
  : Apply automatic grading to a problem object
- [`problem_message()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_message.md)
  : Create a message from a problem object
- [`problem_type()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
  [`is_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
  [`is_tblcheck_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
  [`as_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
  : Problem helper functions

## Helper functions

- [`tbl_equal()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_equal.md)
  : Check that the rows and columns of two tables are the same
- [`friendly_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/friendly_class.md)
  : Generate a human-readable description of an object's class
- [`hinted_class_message()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/hinted_class_message.md)
  : Generate a hint for how to convert one object type to another
