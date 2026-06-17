# Checks that two objects have the same classes

Checks if `object` and `expected` have the same
[class](https://rdrr.io/r/base/class.html). If the classes differ

- `tbl_check_class()` and `vec_check_class()` return a list describing
  the problem

- `tbl_grade_class()` and `vec_grade_class()` return a failing grade and
  informative message with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_class(
  object = .result,
  expected = .solution,
  ignore_class = NULL,
  env = parent.frame()
)

vec_check_class(
  object = .result,
  expected = .solution,
  ignore_class = NULL,
  env = parent.frame()
)

tbl_grade_class(
  object = .result,
  expected = .solution,
  ignore_class = NULL,
  env = parent.frame(),
  ...
)

vec_grade_class(
  object = .result,
  expected = .solution,
  ignore_class = NULL,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  An object to be compared to `expected`.

- expected:

  An object containing the expected result.

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
`tbl_check_class()` and `vec_check_class()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_class()` and `vec_grade_class()`. Otherwise,
invisibly returns [`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `class`: The object does not have the expected classes

## Examples

``` r
.result <- 1:10
.solution <- as.character(1:10)
vec_check_class()
#> <tblcheck problem>
#> Your result should be a vector of text (class `character`), but it is a vector of integers (class `integer`).
#> $ type           : chr "class"
#> $ expected       : chr "character"
#> $ actual         : chr "integer"
#> $ expected_length: int 10
#> $ actual_length  : int 10
vec_grade_class()
#> <gradethis_graded: [Incorrect]
#>   Your result should be a vector of text (class `character`), but
#>   it is a vector of integers (class `integer`).
#> >

.result <- data.frame(a = 1:10)
.solution <- tibble::tibble(a = 1:10)
tbl_check_class()
#> <tblcheck problem>
#> Your result should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`).
#> $ type           : chr "class"
#> $ expected       : chr [1:3] "tbl_df" "tbl" "data.frame"
#> $ actual         : chr "data.frame"
#> $ expected_length: int 1
#> $ actual_length  : int 1
tbl_grade_class()
#> <gradethis_graded: [Incorrect]
#>   Your result should be a tibble (class `tbl_df`), but it is a
#>   data frame (class `data.frame`).
#> >

.result <- tibble::tibble(a = 1:10, b = a %% 2 == 0)
.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a %% 2 == 0), b)
tbl_check_class()
#> <tblcheck problem>
#> Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?
#> $ type           : chr "class"
#> $ expected       : chr [1:4] "grouped_df" "tbl_df" "tbl" "data.frame"
#> $ actual         : chr [1:3] "tbl_df" "tbl" "data.frame"
#> $ expected_length: int 2
#> $ actual_length  : int 2
tbl_grade_class()
#> <gradethis_graded: [Incorrect]
#>   Your table isn't a grouped data frame, but I was expecting it
#>   to be grouped. Maybe you need to use `group_by()`?
#> >

# Ignore the difference between tibble and data frame
.result <- data.frame(a = 1:10)
.solution <- tibble::tibble(a = 1:10)
tbl_check_class(ignore_class = c("tbl_df", "tbl"))
tbl_grade_class(ignore_class = c("tbl_df", "tbl"))

# Ignore the difference between integer and double
.result <- 1L
.solution <- 1
vec_check_class(ignore_class = c("integer" = "numeric"))
vec_grade_class(ignore_class = c("integer" = "numeric"))
```
