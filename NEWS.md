<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# tblcheck 0.1.0

- New package for inspecting data frames and vectors for grading with `gradethis`.
- Includes drop-in grading for tables and vectors:
  - `grade_this_table()` grades the student's **table** by comparing it with the table produced by the `-solution` code
  - `grade_this_vector()` grades the student's **vector** by comparing it with the vector produced by the `-solution` code
- Additional, lower-level functions come in two variants, a `grade` function that returns a `gradethis` grade and a `check` function that returns a list detailing the differences between actual and expected output.
- Includes three general purpose checking functions:
  - `tbl_grade()` and `tbl_check()` check that a tibble or data frame matches an expected output.
  - `vec_grade()` and `vec_check()` check that a vector marches an expected output.
  - `tbl_grade_column()` and `tbl_check_column()` check that a single column of a tibble or data frame matches an expected output (powered by `vec_grade()` and `vec_check()`).
- Includes six even more specific checking functions:
  - `tbl_grade_class()`, `tbl_check_class()`, `vec_grade_class()` and `vec_check_class()` check that the class of an object matches that of an expected output.
  - `tbl_grade_dimensions()`, `tbl_check_dimensions()`, `vec_grade_dimensions()` and `vec_check_dimensions()` check that the length, number of rows and columns, and number of dimensions of an object match that of an expected output.
  - `tbl_grade_groups()` and `tbl_check_groups()` check that the groups of a table (created by `dplyr::group_by()`) match that of an expected output.
  - `vec_grade_levels()` and `vec_check_levels()` check that the levels of a factor match that of an expected output.
  - `tbl_grade_names()`, `tbl_check_names()`, `vec_grade_names()` and `vec_check_names()` check that the names of an object match that of an expected output.
  - `vec_grade_values()` and `vec_check_values()` check that the values of a vector match that of an expected output.
- Also includes a number of functions for dealing with the `problem` output of `check` functions.
