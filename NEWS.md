<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# tblcheck 0.1.3

* Add `tolerance` argument to `vec_*_values()` (#111).

# tblcheck 0.1.2

* Adds argument `ignore_class` to class checks (and passed by `table`, `vector`, and `column` checks), specifying class differences to ignore (#109).
* Table checking now ignores the `grouped_df` class if `check_groups = FALSE` (#109).
* Classes are now checked with `setequal()` instead of `identical()`, so objects with the same classes in different orders will not trigger a `class` problem (#109).
* Fixed a bug where a message would not be generated for a values problem where two vectors were different but were made up of the same unique values (e.g. `c(1, 2, 3)` and `c(2, 1, 3, 2)`) (#104).

# tblcheck 0.1.1

* Fixed the messages for values problems originating in columns to mention the problematic column (#100).
* A new vignette with exercise examples demonstrates the types of feedback `grade_this_table()` and `grade_this_vector()` provide (#95).
* `vec_grade_values()` now treats `NA` values in the same position of the `object` and `expected` vectors as equal (#96).
* `grade_this_table()` was updated to include the `cols` and `check_column_order` arguments that were recently added to `tbl_grade()` (#92).


# tblcheck 0.1.0

- New package for inspecting data frames and vectors for grading with `gradethis`.
- Includes drop-in `gradethis`-style grading for tables and vectors that compare the student's result with the exercies `-solution`:
  - `grade_this_table()` for automated grading of **tables**
  - `grade_this_vector()` for automated grading of **vectors**
- Additional, lower-level functions come in two variants, a `grade` function that returns a `gradethis` grade and a `check` function that returns a list detailing the differences between actual and expected output.
- Includes three general purpose checking functions:
  1. `tbl_grade()` and `tbl_check()` check that a tibble or data frame matches an expected output.
  2. `vec_grade()` and `vec_check()` check that a vector marches an expected output.
  3. `tbl_grade_column()` and `tbl_check_column()` check that a single column of a tibble or data frame matches an expected output (powered by `vec_grade()` and `vec_check()`).
- Includes six even more specific checking functions:
  1. `tbl_grade_class()`, `tbl_check_class()`, `vec_grade_class()` and `vec_check_class()` check that the class of an object matches that of an expected output.
  2. `tbl_grade_dimensions()`, `tbl_check_dimensions()`, `vec_grade_dimensions()` and `vec_check_dimensions()` check that the length, number of rows and columns, and number of dimensions of an object match that of an expected output.
  3. `tbl_grade_groups()` and `tbl_check_groups()` check that the groups of a table (created by `dplyr::group_by()`) match that of an expected output.
  4. `vec_grade_levels()` and `vec_check_levels()` check that the levels of a factor match that of an expected output.
  5. `tbl_grade_names()`, `tbl_check_names()`, `vec_grade_names()` and `vec_check_names()` check that the names of an object match that of an expected output.
  6. `vec_grade_values()` and `vec_check_values()` check that the values of a vector match that of an expected output.
- Also includes a number of functions for dealing with the `problem` output of `check` functions.
