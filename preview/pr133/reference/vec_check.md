# Checks that two vectors are the same

Checks for differences between `object` and `expected` in the following
order:

1.  Check class with
    [`vec_check_class()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/tbl_check_class.md)

2.  Check length with
    [`vec_check_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/tbl_check_dimensions.md)

3.  If the vector is a factor, check factor levels are the same with
    [`vec_check_levels()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check_levels.md)

4.  Check vector values are the same with
    [`vec_check_values()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check_values.md)

5.  Check names with
    [`vec_check_names()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/tbl_check_names.md)

If the vectors differ

- `vec_check()` returns a list describing the problem

- `vec_grade()` returns a failing grade and informative message with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
vec_check(
  object = .result,
  expected = .solution,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_levels = TRUE,
  check_values = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  check_names = TRUE,
  env = parent.frame()
)

vec_grade(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_levels = TRUE,
  check_values = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  check_names = TRUE,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  A vector to be compared to `expected`.

- expected:

  A vector containing the expected result.

- check_class:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same classes.

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
  Whether to check that `object` and `expected` have the same length.

- check_levels:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same [factor
  levels](https://rdrr.io/r/base/levels.html).

- check_values:

  `[logical(1)]`  
  Whether to check that `object` and `expected` contain the same values.

- tolerance:

  `[numeric(1) ≥ 0]`  
  `values` differences smaller than `tolerance` are ignored. The default
  value is close to `1.5e-8`.

- check_names:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same names.

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
`vec_check()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `vec_grade()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `class`: `object` doesn't have the same classes as `expected`.

2.  `length`: `object` doesn't have the same length as `expected`.

3.  `levels_n`, `levels`, `levels_reversed`, `levels_order`: See
    [`vec_check_levels()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check_levels.md).

4.  `values`: `object` doesn't contain the same values as `expected`.

5.  `names`: `object` has different
    [names](https://rdrr.io/r/base/names.html) than `expected`.

6.  `names_order`: `object` has the same
    [names](https://rdrr.io/r/base/names.html) as `expected`, but in a
    different order.

## Examples

``` r
.result <- 1:10
.solution <- letters[1:10]
vec_check()
#> <tblcheck problem>
#> Your result should be a vector of text (class `character`), but it is a vector of integers (class `integer`).
#> $ type    : chr "class"
#> $ expected: chr [1:10] "a" "b" "c" "d" ...
#> $ actual  : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ location: chr "vector"
vec_grade()
#> <gradethis_graded: [Incorrect]
#>   Your result should be a vector of text (class `character`), but
#>   it is a vector of integers (class `integer`).
#> >

.result <- 1:10
.solution <- 1:11
vec_check()
#> <tblcheck problem>
#> Your result should contain 11 values, but it has 10 values. I expected your result to include the value `11`.
#> $ type           : chr "length"
#> $ expected       : int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
#> $ actual         : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ expected_length: int 11
#> $ actual_length  : int 10
#> $ location       : chr "vector"
vec_grade()
#> <gradethis_graded: [Incorrect]
#>   Your result should contain 11 values, but it has 10 values. I
#>   expected your result to include the value `11`.
#> >

.result <- 1:10
.solution <- rlang::set_names(1:10, letters[1:10])
vec_check()
#> <tblcheck problem>
#> Your result should have the names `a`, `b`, `c`, and 7 more. 
#> $ type      : chr "names"
#> $ missing   : chr [1:10] "a" "b" "c" "d" ...
#> $ unexpected: chr(0) 
#> $ location  : chr "vector"
vec_grade()
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, and 7 more.
#> >
vec_grade(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, `d`, `e`, and
#>   5 more.
#> >
vec_grade(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, `d`, `e`, `f`,
#>   `g`, `h`, `i`, and `j`.
#> >

.result <- 1:10
.solution <- 11:20
vec_check()
#> <tblcheck problem>
#> The first 3 values of your result should be `11`, `12`, and `13`, not `1`, `2`, and `3`.
#> $ type    : chr "values"
#> $ expected: int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ actual  : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ location: chr "vector"
vec_grade()
#> <gradethis_graded: [Incorrect]
#>   The first 3 values of your result should be `11`, `12`, and
#>   `13`, not `1`, `2`, and `3`.
#> >
vec_grade(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   The first 5 values of your result should be `11`, `12`, `13`,
#>   `14`, and `15`, not `1`, `2`, `3`, `4`, and `5`.
#> >
vec_grade(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   The first 10 values of your result should be `11`, `12`, `13`,
#>   `14`, `15`, `16`, `17`, `18`, `19`, and `20`, not `1`, `2`,
#>   `3`, `4`, `5`, `6`, `7`, `8`, `9`, and `10`.
#> >
```
