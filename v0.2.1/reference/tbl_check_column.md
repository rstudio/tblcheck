# Checks that a column is identical across two tables

Checks for differences between the `name` column in `object` and in
`expected` in the following order:

1.  Check that the `name` column exists in `object`

2.  Check class with
    [`vec_check_class()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/tbl_check_class.md)

3.  Check length with
    [`vec_check_dimensions()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/tbl_check_dimensions.md)

4.  If the column is a factor, check factor levels with
    [`vec_check_levels()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/vec_check_levels.md)

5.  Check column values with
    [`vec_check_values()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/vec_check_values.md)

If the columns differ

- `tbl_check_column()` returns a list describing the problem

- `tbl_grade_column()` returns a failing grade and informative message
  with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_column(
  column,
  object = .result,
  expected = .solution,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_values = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  check_names = FALSE,
  env = parent.frame()
)

tbl_grade_column(
  column,
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_values = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  check_names = FALSE,
  env = parent.frame(),
  ...
)
```

## Arguments

- column:

  `[character(1)]`  
  The name of the column to check.

- object:

  A data frame to be compared to `expected`.

- expected:

  A data frame containing the expected result.

- check_class:

  `[logical(1)]`  
  Whether to check that `column` has the same class in `object` and
  `expected`.

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

- check_length:

  `[logical(1)]`  
  Whether to check that `column` has the same length in `object` and
  `expected`.

- check_values:

  `[logical(1)]`  
  Whether to check that `column` has the same values in `object` and
  `expected`.

- tolerance:

  `[numeric(1) ≥ 0]`  
  `values` differences smaller than `tolerance` are ignored. The default
  value is close to `1.5e-8`.

- check_names:

  `[logical(1)]`  
  Whether to check that `column` has the same
  [names](https://rdrr.io/r/base/names.html) in `object` and `expected`.
  Defaults to `FALSE`.

- env:

  The environment in which to find `.result` and `.solution`.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of mismatched values to print. Defaults to 3.

- ...:

  Arguments passed on to
  [`gradethis::fail`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

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
`tbl_check_column()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_column()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `names` (`table_problem`): `object` doesn't contain a column named
    column.

2.  `class`: Any mismatch in the classes of the `column`.

3.  `length`: The `column` doesn't have the expected length.

4.  `levels_n`, `levels`, `levels_reversed`, `levels_order`: See
    [`vec_check_levels()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/vec_check_levels.md).

5.  `values`: The `column` doesn't have the expected values.

6.  `names` (`column_problem`): The `column` has different
    [names](https://rdrr.io/r/base/names.html) than expected.

7.  `names_order`: The `column` has the same
    [names](https://rdrr.io/r/base/names.html) as expected, but in a
    different order.

## Examples

``` r
.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = letters[1:10], b = letters[11:20])
tbl_check_column("a")
#> <tblcheck problem>
#> Your `a` column should be a vector of text (class `character`), but it is a vector of integers (class `integer`).
#> $ type           : chr "class"
#> $ expected       : chr "character"
#> $ actual         : chr "integer"
#> $ expected_length: int 10
#> $ actual_length  : int 10
#> $ location       : chr "column"
#> $ column         : chr "a"
tbl_grade_column("a")
#> <gradethis_graded: [Incorrect]
#>   Your `a` column should be a vector of text (class `character`),
#>   but it is a vector of integers (class `integer`).
#> >

.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = 1:11, b = 12:22)
tbl_check_column("a")
#> <tblcheck problem>
#> Your `a` column should contain 11 values, but it has 10 values. I expected your result to include the value `11`.
#> $ type           : chr "length"
#> $ expected       : int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
#> $ actual         : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ expected_length: int 11
#> $ actual_length  : int 10
#> $ location       : chr "column"
#> $ column         : chr "a"
tbl_grade_column("a")
#> <gradethis_graded: [Incorrect]
#>   Your `a` column should contain 11 values, but it has 10 values.
#>   I expected your result to include the value `11`.
#> >

.result <- tibble::tibble(a = 1:10, b = 11:20)
.solution <- tibble::tibble(a = 11:20, b = 1:10)
tbl_check_column("a")
#> <tblcheck problem>
#> The first 3 values of your `a` column should be `11`, `12`, and `13`, not `1`, `2`, and `3`.
#> $ type    : chr "values"
#> $ expected: int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ actual  : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ location: chr "column"
#> $ column  : chr "a"
tbl_grade_column("a")
#> <gradethis_graded: [Incorrect]
#>   The first 3 values of your `a` column should be `11`, `12`, and
#>   `13`, not `1`, `2`, and `3`.
#> >
tbl_grade_column("a", max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   The first 5 values of your `a` column should be `11`, `12`,
#>   `13`, `14`, and `15`, not `1`, `2`, `3`, `4`, and `5`.
#> >
tbl_grade_column("a", max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   The first 10 values of your `a` column should be `11`, `12`,
#>   `13`, `14`, `15`, `16`, `17`, `18`, `19`, and `20`, not `1`,
#>   `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, and `10`.
#> >
```
