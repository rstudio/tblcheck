# Check that the rows and columns of two tables are the same

**\[deprecated\]**

`vec_check_vector()` and `vec_grade_vector()` were renamed to
[`vec_check()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/vec_check.md)
and
[`vec_grade()`](https://rstudio.github.io/tblcheck/v0.2.1/reference/vec_check.md).

## Usage

``` r
vec_check_vector(
  object = .result,
  expected = .solution,
  check_class = TRUE,
  check_length = TRUE,
  check_levels = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  env = parent.frame()
)

vec_grade_vector(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  env = parent.frame(),
  ...
)
```
