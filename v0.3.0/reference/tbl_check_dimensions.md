# Check that the dimensions of two object are the same

Checks if `object` and `expected` have the same
[dimenisons](https://rdrr.io/r/base/dim.html). If the dimensions differ

- `tbl_check_dimensions()` returns a list describing the problem

- `tbl_grade_dimensions()` returns a failing grade and informative
  message with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_dimensions(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame()
)

vec_check_dimensions(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame()
)

vec_check_length(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame()
)

tbl_grade_dimensions(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame(),
  ...
)

vec_grade_dimensions(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame(),
  ...
)

vec_grade_length(
  object = .result,
  expected = .solution,
  check_ncol = TRUE,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  An object to be compared to `expected`.

- expected:

  An object containing the expected result.

- check_ncol:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same number of
  columns.

- env:

  The environment in which to find `.result` and `.solution`.

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
`tbl_check_dimensions()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_dimensions()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `dimensions_n`: `object` and `expected` have a different number of
    dimensions

2.  `length`: `object` and `expected` are one-dimensional vectors of
    different lengths

3.  `ncol`: `object` and `expected` are two-dimensional objects with a
    different number of columns

4.  `nrow`: `object` and `expected` are two-dimensional objects with a
    different number of rows

5.  `dimensions`: `object` and `expected` are multi-dimensional arrays
    with different dimensions

## Examples

``` r
.result <- 1:10
.solution <- 1:5
tbl_check_dimensions()
#> <tblcheck problem>
#> Your result should contain 5 values, but it has 10 values.
#> $ type           : chr "length"
#> $ expected       : int [1:5] 1 2 3 4 5
#> $ actual         : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ expected_length: int 5
#> $ actual_length  : int 10
tbl_grade_dimensions()
#> <gradethis_graded: [Incorrect]
#>   Your result should contain 5 values, but it has 10 values.
#> >

.result <- tibble::tibble(a = 1:10, b = 1:10, c = 1:10)
.solution <- tibble::tibble(a = 1:10, b = 1:10)
tbl_check_dimensions()
#> <tblcheck problem>
#> Your result should have 2 columns, but it has 3 columns.
#> $ type    : chr "ncol"
#> $ expected: int 2
#> $ actual  : int 3
tbl_grade_dimensions()
#> <gradethis_graded: [Incorrect]
#>   Your result should have 2 columns, but it has 3 columns.
#> >

.result <- tibble::tibble(a = 1:10, b = 1:10)
.solution <- tibble::tibble(a = 1:5, b = 1:5)
tbl_check_dimensions()
#> <tblcheck problem>
#> Your result should have 5 rows, but it has 10 rows.
#> $ type    : chr "nrow"
#> $ expected: int 5
#> $ actual  : int 10
tbl_grade_dimensions()
#> <gradethis_graded: [Incorrect]
#>   Your result should have 5 rows, but it has 10 rows.
#> >

.result <- 1:12
.solution <- matrix(1:12, 3)
tbl_check_dimensions()
#> <tblcheck problem>
#> Your result should have 2 dimensions, but it has 1 dimension.
#> $ type    : chr "dimensions_n"
#> $ expected: int 2
#> $ actual  : int 1
tbl_grade_dimensions()
#> <gradethis_graded: [Incorrect]
#>   Your result should have 2 dimensions, but it has 1 dimension.
#> >
```
