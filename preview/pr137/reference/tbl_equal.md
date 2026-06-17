# Check that the rows and columns of two tables are the same

Test if two tables are equivalent using the same process as
[`tbl_check()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md).
Unlike
[`tbl_check()`](https:/rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md),
which returns either a
[problem](https:/rstudio.github.io/tblcheck/preview/pr137/reference/problem.md)
object or [`NULL`](https://rdrr.io/r/base/NULL.html), `tbl_equal()`
returns either [`TRUE`](https://rdrr.io/r/base/logical.html) or
[`FALSE`](https://rdrr.io/r/base/logical.html).

## Usage

``` r
tbl_equal(
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

## Value

A [`TRUE`](https://rdrr.io/r/base/logical.html) or
[`FALSE`](https://rdrr.io/r/base/logical.html) value.

## Examples

``` r
tbl_equal(
  data.frame(a = 1:10, b = 11:20),
  data.frame(b = 11:20, a = 1:10)
)
#> [1] TRUE
```
