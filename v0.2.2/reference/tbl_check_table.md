# Check that the rows and columns of two tables are the same

**\[deprecated\]**

`tbl_check_table()` and `tbl_grade_table()` were renamed to
[`tbl_check()`](https://rstudio.github.io/tblcheck/v0.2.2/reference/tbl_check.md)
and
[`tbl_grade()`](https://rstudio.github.io/tblcheck/v0.2.2/reference/tbl_check.md).

## Usage

``` r
tbl_check_table(
  object = .result,
  expected = .solution,
  check_class = TRUE,
  check_names = TRUE,
  check_column_order = FALSE,
  check_dimensions = TRUE,
  check_groups = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_values = check_columns,
  env = parent.frame()
)

tbl_grade_table(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_names = TRUE,
  check_column_order = FALSE,
  check_dimensions = TRUE,
  check_groups = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_values = check_columns,
  env = parent.frame(),
  ...
)
```
