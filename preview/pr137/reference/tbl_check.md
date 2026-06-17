# Check that the rows and columns of two tables are the same

Checks for differences between `object` and `expected` in the following
order:

1.  Check table class with
    [`tbl_check_class()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md)

2.  Check column names with
    [`tbl_check_names()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)

3.  Check number of rows and columns with
    [`tbl_check_dimensions()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md)

4.  Check [groups](https://dplyr.tidyverse.org/reference/group_by.html)
    with
    [`tbl_check_groups()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_groups.md)

5.  Check that each column is the same with
    [`tbl_check_column()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)

If the tables differ

- `tbl_check()` returns a list describing the problem

- `tbl_grade()` returns a failing grade and informative message with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check(
  object = .result,
  expected = .solution,
  cols = NULL,
  check_class = TRUE,
  ignore_class = NULL,
  check_names = TRUE,
  check_column_order = FALSE,
  check_dimensions = TRUE,
  check_groups = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_levels = check_columns,
  check_column_values = check_columns,
  tolerance = sqrt(.Machine$double.eps),
  check_row_order = check_columns,
  env = parent.frame()
)

tbl_grade(
  object = .result,
  expected = .solution,
  cols = NULL,
  max_diffs = 3,
  check_class = TRUE,
  ignore_class = NULL,
  check_names = TRUE,
  check_column_order = FALSE,
  check_dimensions = TRUE,
  check_groups = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_levels = check_columns,
  check_column_values = check_columns,
  tolerance = sqrt(.Machine$double.eps),
  check_row_order = check_columns,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  A data frame to be compared to `expected`.

- expected:

  A data frame containing the expected result.

- cols:

  \[[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\]  
  A selection of columns to compare between `object` and `expected`.
  Differences in other columns will be ignored. If
  [`NULL`](https://rdrr.io/r/base/NULL.html), the default, all columns
  will be checked.

- check_class:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same classes
  with
  [`tbl_check_class()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md).

- ignore_class:

  `[character()]`  
  A vector of classes to ignore when finding differences between
  `object` and `expected`.

  If an element is named, differences will only be ignored between the
  pair of the element and its name. For example,
  `ignore_class = c("integer" = "numeric")` will ignore class
  differences only if `object` has class
  [integer](https://rdrr.io/r/base/integer.html) and `expected` has
  class [numeric](https://rdrr.io/r/base/numeric.html), or vice versa.

  If all the classes of `expected` are included in `ignore_class`, a
  `class` problem will never be returned.

- check_names:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same column
  names with
  [`tbl_check_names()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md).

- check_column_order:

  `[logical(1)]`  
  Whether to check that the columns of `object` are in the same order as
  `expected` with
  [`tbl_check_names()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md).
  Defaults to `FALSE`.

- check_dimensions:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same number of
  rows and columns with
  [`tbl_check_dimensions()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md).

- check_groups:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same
  [groups](https://dplyr.tidyverse.org/reference/group_by.html) with
  [`dplyr::group_vars()`](https://dplyr.tidyverse.org/reference/group_data.html).

- check_columns:

  `[logical(1)]`  
  Whether to check that all columns have the same contents with
  [`tbl_check_column()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md).

- check_column_class:

  `[logical(1)]`  
  Whether to check that each column has the same class in `object` and
  `expected`.

- check_column_levels:

  `[logical(1)]`  
  Whether to check that each column has the same [factor
  levels](https://rdrr.io/r/base/levels.html) in `object` and
  `expected`.

- check_column_values:

  `[logical(1)]`  
  Whether to check that each column has the same values in `object` and
  `expected`.

- tolerance:

  `[numeric(1) ≥ 0]`  
  `values` differences smaller than `tolerance` are ignored. The default
  value is close to `1.5e-8`.

- check_row_order:

  `[logical(1)]`  
  Whether to check that the values in each column are in the same order
  in `object` and `expected`.

- env:

  The environment in which to find `.result` and `.solution`.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of mismatched values to display in an informative
  failure message. Passed to
  [`tbl_check_names()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md)
  to determine the number of mismatched column names to display and the
  `n_values` argument of
  [`tbl_check_column()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
  to determine the number of mismatched column values to display.
  Defaults to 3.

- ...:

  Arguments passed on to
  [`gradethis::fail`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

  `correct`

  :   A logical value of whether or not the checked code is correct.

  `type,location`

  :   The `type` and `location` of the feedback object provided to
      learnr. See
      <https://rstudio.github.io/learnr/exercises.html#Custom_checking>
      for more details.

      `type` may be one of "auto", "success", "info", "warning",
      "error", or "custom".

      `location` may be one of "append", "prepend", or "replace".

  `praise`

  :   Include a random praising phrase with
      [`random_praise()`](https://pkgs.rstudio.com/gradethis/reference/praise.html)?
      The default value of `praise` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.pass.praise` option.

  `hint`

  :   Include a code feedback hint with the failing message? This
      argument only applies to `fail()` and `fail_if_equal()` and the
      message is added using the default options of
      [`give_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html)
      and
      [`maybe_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html).
      The default value of `hint` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.fail.hint` option.

  `encourage`

  :   Include a random encouraging phrase with
      [`random_encouragement()`](https://pkgs.rstudio.com/gradethis/reference/praise.html)?
      The default value of `encourage` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.fail.encourage` option.

## Value

If there are any issues, a [list](https://rdrr.io/r/base/list.html) from
`tbl_check()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `class`: The table does not have the expected classes.

2.  `not_table`: `object` does not inherit the
    [data.frame](https://rdrr.io/r/base/data.frame.html) class.

3.  `names`: The table has column names that are not expected, or is
    missing names that are expected.

4.  `names_order`: The table has the same column names as expected, but
    in a different order.

5.  `ncol`: The table doesn't have the expected number of columns.

6.  `nrow`: The table doesn't have the expected number of rows.

7.  `groups`: The table has
    [groups](https://dplyr.tidyverse.org/reference/group_by.html) that
    are not expected, or is missing groups that are expected.

Additional problems may be produced by
[`tbl_check_column()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md).

## Examples

``` r
.result <- data.frame(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = 1:10, b = 11:20)
tbl_check()
#> <tblcheck problem>
#> Your table should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`).
#> $ type    : chr "class"
#> $ expected: tibble [10 × 2] (S3: tbl_df/tbl/data.frame)
#>  ..$ a: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  ..$ b: int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ actual  :'data.frame': 10 obs. of  2 variables:
#>  ..$ a: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  ..$ b: int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ location: chr "table"
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   Your table should be a tibble (class `tbl_df`), but it is a
#>   data frame (class `data.frame`).
#> >

.result <- tibble::tibble(a = 1:10, b = a, c = a, d = a, e = a, f = a)
.solution <- tibble::tibble(z = 1:10, y = z, x = z, w = z, v = z, u = z)
tbl_check()
#> <tblcheck problem>
#> Your table should have columns named `z`, `y`, `x`, and 3 more. Your table should not have columns named `a`, `b`, `c`, or 3 more.
#> $ type      : chr "names"
#> $ missing   : chr [1:6] "z" "y" "x" "w" ...
#> $ unexpected: chr [1:6] "a" "b" "c" "d" ...
#> $ location  : chr "table"
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   Your table should have columns named `z`, `y`, `x`, and 3
#>   more. Your table should not have columns named `a`, `b`,
#>   `c`, or 3 more.
#> >
tbl_grade(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   Your table should have columns named `z`, `y`, `x`, `w`,
#>   `v`, and 1 more. Your table should not have columns named
#>   `a`, `b`, `c`, `d`, `e`, or 1 more.
#> >
tbl_grade(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   Your table should have columns named `z`, `y`, `x`, `w`,
#>   `v`, and `u`. Your table should not have columns named `a`,
#>   `b`, `c`, `d`, `e`, or `f`.
#> >

.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = 1:11, b = 12:22)
tbl_check()
#> <tblcheck problem>
#> Your table should have 11 rows, but it has 10 rows.
#> $ type    : chr "nrow"
#> $ expected: int 11
#> $ actual  : int 10
#> $ location: chr "table"
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   Your table should have 11 rows, but it has 10 rows.
#> >

.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = letters[1:10], b = letters[11:20])
tbl_check()
#> <tblcheck problem>
#> Your `a` column should be a vector of text (class `character`), but it is a vector of integers (class `integer`).
#> $ type       : chr "class"
#> $ expected   : chr [1:10] "a" "b" "c" "d" ...
#> $ actual     : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ location   : chr "column"
#> $ column     : chr "a"
#> $ check_order: logi TRUE
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   Your `a` column should be a vector of text (class
#>   `character`), but it is a vector of integers (class
#>   `integer`).
#> >

.result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
.solution <- tibble::tibble(a = 1:10, b = 11:20)
tbl_check(cols = any_of(names(.solution)))
tbl_grade(cols = any_of(names(.solution)))

.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = 11:20, b = 1:10)
tbl_check()
#> <tblcheck problem>
#> The first 3 values of your `a` column should be `11`, `12`, and `13`, not `1`, `2`, and `3`.
#> $ type       : chr "values"
#> $ expected   : int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ actual     : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ location   : chr "column"
#> $ column     : chr "a"
#> $ check_order: logi TRUE
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   The first 3 values of your `a` column should be `11`, `12`,
#>   and `13`, not `1`, `2`, and `3`.
#> >
tbl_grade(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   The first 5 values of your `a` column should be `11`, `12`,
#>   `13`, `14`, and `15`, not `1`, `2`, `3`, `4`, and `5`.
#> >
tbl_grade(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   The first 10 values of your `a` column should be `11`, `12`,
#>   `13`, `14`, `15`, `16`, `17`, `18`, `19`, and `20`, not `1`,
#>   `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, and `10`.
#> >

.result <- tibble::tibble(a = 1:10, b = rep(1:2, 5))
.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = rep(1:2, 5)), b)
tbl_check()
#> <tblcheck problem>
#> Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?
#> $ type    : chr "class"
#> $ expected: gropd_df [10 × 2] (S3: grouped_df/tbl_df/tbl/data.frame)
#>  ..$ a: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  ..$ b: int [1:10] 1 2 1 2 1 2 1 2 1 2
#>  ..- attr(*, "groups")= tibble [2 × 2] (S3: tbl_df/tbl/data.frame)
#>  .. ..$ b    : int [1:2] 1 2
#>  .. ..$ .rows: list<int> [1:2] 
#>  .. .. ..$ : int [1:5] 1 3 5 7 9
#>  .. .. ..$ : int [1:5] 2 4 6 8 10
#>  .. .. ..@ ptype: int(0) 
#>  .. ..- attr(*, ".drop")= logi TRUE
#> $ actual  : tibble [10 × 2] (S3: tbl_df/tbl/data.frame)
#>  ..$ a: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  ..$ b: int [1:10] 1 2 1 2 1 2 1 2 1 2
#> $ location: chr "table"
tbl_grade()
#> <gradethis_graded: [Incorrect]
#>   Your table isn't a grouped data frame, but I was expecting
#>   it to be grouped. Maybe you need to use `group_by()`?
#> >
tbl_grade(check_groups = FALSE)
```
