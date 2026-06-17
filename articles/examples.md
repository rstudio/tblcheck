# Examples of tblcheck in action

## Overview

To help you get a feel for the kind of advice tblcheck provides, we’ve
collected a few example exercises that use
[`grade_this_table()`](https://rstudio.github.io/tblcheck/reference/grade_this_table.md)
or
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/reference/grade_this_vector.md)
from tblcheck to grade a [learnr](https://rstudio.github.io/learnr/)
exercise.

In the examples below, we’ll show the specific R chunks you’d need to
prepare the exercise. Globally, we assume
[learnr](https://rstudio.github.io/learnr/) and
[gradethis](https://rstudio.github.io/gradethis/) are already loaded,
and we’ve made a few adjustments to the default gradethis options using
[`gradethis::gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html):

```` markdown
```{r setup}
library(learnr)
library(gradethis)
library(tblcheck)

gradethis_setup(
  pass.praise = TRUE,
  fail.hint = TRUE,
  fail.encourage = TRUE,
  maybe_code_feedback.before = "\n\n"
)
```
````

Note that in all examples below we’re using either
[`grade_this_table()`](https://rstudio.github.io/tblcheck/reference/grade_this_table.md)
or
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/reference/grade_this_vector.md)
without any additional customizations. We’ve also made sure that each
exercise contains a `-solution` chunk. ([Read more about how to set up
an
exercise](https://rstudio.github.io/tblcheck/articles/tblcheck.html#usage)
for use with tblcheck.)

## Create a Table

The first example repeats the example presented in the [*Get started*
article](https://rstudio.github.io/tblcheck/articles/tblcheck.md). The
goal is for the learner to use the
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
function to create a table, but it takes them a few tries.

> **Prompt**  
> Create a `tibble` with three columns: `food`, `vegetable`, and
> `color`.
>
> - In `food`, store “lettuce” and “tomato”.
> - In `vegetable`, store whether the food is a vegetable.
> - In `color`, store the color of the food.

- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xLjc1IDEuNWEuMjUuMjUgMCAwMC0uMjUuMjV2MTIuNWMwIC4xMzguMTEyLjI1LjI1LjI1aDEyLjVhLjI1LjI1IDAgMDAuMjUtLjI1VjEuNzVhLjI1LjI1IDAgMDAtLjI1LS4yNUgxLjc1ek0wIDEuNzVDMCAuNzg0Ljc4NCAwIDEuNzUgMGgxMi41QzE1LjIxNiAwIDE2IC43ODQgMTYgMS43NXYxMi41QTEuNzUgMS43NSAwIDAxMTQuMjUgMTZIMS43NUExLjc1IDEuNzUgMCAwMTAgMTQuMjVWMS43NXptOS4yMiAzLjcyYS43NS43NSAwIDAwMCAxLjA2TDEwLjY5IDggOS4yMiA5LjQ3YS43NS43NSAwIDEwMS4wNiAxLjA2bDItMmEuNzUuNzUgMCAwMDAtMS4wNmwtMi0yYS43NS43NSAwIDAwLTEuMDYgMHpNNi43OCA2LjUzYS43NS43NSAwIDAwLTEuMDYtMS4wNmwtMiAyYS43NS43NSAwIDAwMCAxLjA2bDIgMmEuNzUuNzUgMCAxMDEuMDYtMS4wNkw1LjMxIDhsMS40Ny0xLjQ3eiIgLz48L3N2Zz4=)
  Exercise Code
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik04IDE2QTggOCAwIDEwOCAwYTggOCAwIDAwMCAxNnptMy43OC05LjcyYS43NS43NSAwIDAwLTEuMDYtMS4wNkw2Ljc1IDkuMTkgNS4yOCA3LjcyYS43NS43NSAwIDAwLTEuMDYgMS4wNmwyIDJhLjc1Ljc1IDAgMDAxLjA2IDBsNC41LTQuNXoiIC8+PC9zdmc+)
  Correct Answer
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Class
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Names
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Length
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Column Class
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Column Values

```` markdown
```{r tomato-setup}
library(tibble)
```

```{r tomato, exercise=TRUE}

```

```{r tomato-solution}
tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red")
)
```

```{r tomato-check}
grade_this_table()
```
````

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <lgl>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  FALSE     red
```

That’s a first-class answer! Correct!

``` r

list(
    food = "lettuce",
    fruit = "TRUE",
    color = "green"
)
#> $food
#> [1] "lettuce"
#> 
#> $fruit
#> [1] "TRUE"
#> 
#> $color
#> [1] "green"
```

Your table should be a tibble (class `tbl_df`), but it is a list (class
`list`). Try it again. I have a good feeling about this.

``` r

tibble(
    food = "lettuce",
    fruit = "TRUE",
    color = "green"
)
#> # A tibble: 1 × 3
#>   food    fruit color
#>   <chr>   <chr> <chr>
#> 1 lettuce TRUE  green
```

Your table should have a column named `vegetable`. Your table should not
have a column named `fruit`. Let’s try it again.

``` r

tibble(
    food = "lettuce",
    vegetable = "TRUE",
    color = "green"
)
#> # A tibble: 1 × 3
#>   food    vegetable color
#>   <chr>   <chr>     <chr>
#> 1 lettuce TRUE      green
```

Your table should have 2 rows, but it has 1 row. Try it again. You get
better each time.

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c("TRUE", "TRUE"),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <chr>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  TRUE      red
```

Your `vegetable` column should be a vector of `TRUE`/`FALSE` values
(class `logical`), but it is a vector of text (class `character`). Try
it again. I have a good feeling about this.

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, TRUE),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <lgl>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  TRUE      red
```

The first 2 values of your `vegetable` column should be `TRUE` and
`FALSE`, not `TRUE` and `TRUE`. That’s okay: you learn more from
mistakes than successes. Let’s do it one more time.

## Transform a Table with `transmute()`

The [first example](#create-a-table) shows *all* of the problems that
tblcheck will discover. This example, however, is more realistic.

In this problem students are asked to use the
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
function to transform the `starwars` dataset.

> **Prompt**  
> Consider the `starwars` dataset. Use `transmute()` to turn convert
> `height` from centimeters into inches and `mass` from kilgrams into
> pounds, keeping only those columns.

- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xLjc1IDEuNWEuMjUuMjUgMCAwMC0uMjUuMjV2MTIuNWMwIC4xMzguMTEyLjI1LjI1LjI1aDEyLjVhLjI1LjI1IDAgMDAuMjUtLjI1VjEuNzVhLjI1LjI1IDAgMDAtLjI1LS4yNUgxLjc1ek0wIDEuNzVDMCAuNzg0Ljc4NCAwIDEuNzUgMGgxMi41QzE1LjIxNiAwIDE2IC43ODQgMTYgMS43NXYxMi41QTEuNzUgMS43NSAwIDAxMTQuMjUgMTZIMS43NUExLjc1IDEuNzUgMCAwMTAgMTQuMjVWMS43NXptOS4yMiAzLjcyYS43NS43NSAwIDAwMCAxLjA2TDEwLjY5IDggOS4yMiA5LjQ3YS43NS43NSAwIDEwMS4wNiAxLjA2bDItMmEuNzUuNzUgMCAwMDAtMS4wNmwtMi0yYS43NS43NSAwIDAwLTEuMDYgMHpNNi43OCA2LjUzYS43NS43NSAwIDAwLTEuMDYtMS4wNmwtMiAyYS43NS43NSAwIDAwMCAxLjA2bDIgMmEuNzUuNzUgMCAxMDEuMDYtMS4wNkw1LjMxIDhsMS40Ny0xLjQ3eiIgLz48L3N2Zz4=)
  Exercise Code
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik04IDE2QTggOCAwIDEwOCAwYTggOCAwIDAwMCAxNnptMy43OC05LjcyYS43NS43NSAwIDAwLTEuMDYtMS4wNkw2Ljc1IDkuMTkgNS4yOCA3LjcyYS43NS43NSAwIDAwLTEuMDYgMS4wNmwyIDJhLjc1Ljc1IDAgMDAxLjA2IDBsNC41LTQuNXoiIC8+PC9zdmc+)
  Correct Answer
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Function
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Column Order
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Column Classes
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Calculation

```` markdown
```{r starwars-setup}
library(dplyr)
```

```{r starwars, exercise=TRUE}

```

```{r starwars-solution}
starwars %>%
    transmute(
        height = height / 2.54,
        mass = mass * 2.205
    )
```

```{r starwars-check}
grade_this_table(check_column_order = TRUE)
```
````

The correct answer uses
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
to divide the `height` column by `2.54` (to convert height from *cm* to
*in*) and to multiply the `mass` column by `2.205` (to convert *kg* to
*lb*). Note that `transmute()` keeps only the columns named in its
calculations, so our final table has only two columns.

``` r

starwars %>%
    transmute(
        height = height / 2.54,
        mass = mass * 2.205
    )
#> # A tibble: 87 × 2
#>    height  mass
#>     <dbl> <dbl>
#>  1   67.7 170. 
#>  2   65.7 165. 
#>  3   37.8  70.6
#>  4   79.5 300. 
#>  5   59.1 108. 
#>  6   70.1 265. 
#>  7   65.0 165. 
#>  8   38.2  70.6
#>  9   72.0 185. 
#> 10   71.7 170. 
#> # ℹ 77 more rows
```

In this example, the student uses
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
which is certainly a reasonable choice but not what was requested. As a
result, their submission contains more columns than expected.

``` r

starwars %>%
    mutate(
        height = height / 2.54,
        mass = mass * 2.205
    )
#> # A tibble: 87 × 14
#>    name  height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>  <dbl> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke…   67.7 170.  blond      fair       blue            19   male 
#>  2 C-3PO   65.7 165.  NA         gold       yellow         112   none 
#>  3 R2-D2   37.8  70.6 NA         white, bl… red             33   none 
#>  4 Dart…   79.5 300.  none       white      yellow          41.9 male 
#>  5 Leia…   59.1 108.  brown      light      brown           19   fema…
#>  6 Owen…   70.1 265.  brown, gr… light      blue            52   male 
#>  7 Beru…   65.0 165.  brown      light      blue            47   fema…
#>  8 R5-D4   38.2  70.6 NA         white, red red             NA   none 
#>  9 Bigg…   72.0 185.  black      light      brown           24   male 
#> 10 Obi-…   71.7 170.  auburn, w… fair       blue-gray       57   male 
#> # ℹ 77 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
```

Your table should not have columns named `name`, `hair_color`,
`skin_color`, or 9 more. But no need to fret, try it again.

Here the student has done more or less the right thing, but they
specified the columns in the opposite order.

``` r

starwars %>%
    transmute(
        mass = mass * 2.205,
        height = height / 2.54
    )
#> # A tibble: 87 × 2
#>     mass height
#>    <dbl>  <dbl>
#>  1 170.    67.7
#>  2 165.    65.7
#>  3  70.6   37.8
#>  4 300.    79.5
#>  5 108.    59.1
#>  6 265.    70.1
#>  7 165.    65.0
#>  8  70.6   38.2
#>  9 185.    72.0
#> 10 170.    71.7
#> # ℹ 77 more rows
```

Your table’s columns were not in the expected order. The first 2 columns
of your table should be `height` and `mass`. Try it again. Perseverance
is the key to success.

If we decide we don’t mind if the columns aren’t in the right order, we
could set `check_column_order = FALSE` in the check code for this
exercise.

```` markdown
```{r starwars-check}
grade_this_table(check_column_order = FALSE)
```
````

Someone knows what they’re doing :) Correct!

If the student were to somehow create columns with an incorrect class,
e.g. integer values where doubles were expected, tblcheck will alert
them to their mistake.

``` r

starwars %>%
    transmute(
        mass = as.integer(mass * 2.205),
        height = as.integer(height / 2.54)
    )
#> # A tibble: 87 × 2
#>     mass height
#>    <int>  <int>
#>  1   169     67
#>  2   165     65
#>  3    70     37
#>  4   299     79
#>  5   108     59
#>  6   264     70
#>  7   165     64
#>  8    70     38
#>  9   185     72
#> 10   169     71
#> # ℹ 77 more rows
```

Your table’s columns were not in the expected order. The first 2 columns
of your table should be `height` and `mass`. Give it another try.

If the values are completely wrong — here the student is multiplying
`height` by `2.54` rather than dividing — then tblcheck points out which
column is incorrect and gives a hint about the expected values.

``` r

starwars %>%
    transmute(
        height = height * 2.54,
        mass = mass * 2.205
    )
#> # A tibble: 87 × 2
#>    height  mass
#>     <dbl> <dbl>
#>  1   437. 170. 
#>  2   424. 165. 
#>  3   244.  70.6
#>  4   513. 300. 
#>  5   381  108. 
#>  6   452. 265. 
#>  7   419. 165. 
#>  8   246.  70.6
#>  9   465. 185. 
#> 10   462. 170. 
#> # ℹ 77 more rows
```

The first 3 values of your `height` column should be `67.7`, `65.7`, and
`37.8`, not `437`, `424`, and `244`. Try it again. You get better each
time.

## Transform a Factor Column

This example comes from a lesson on `factors` in R. Students have just
learned about functions from the [forcats
package](https://forcats.tidyverse.org), and are now asked to practice
using
[`forcats::fct_reorder()`](https://forcats.tidyverse.org/reference/fct_reorder.html).

> **Prompt**  
> Consider the `marital_age` dataset. Use `fct_reorder()` to turn the
> `marital` column into a factor with levels ordered based on `avg_age`.

- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xLjc1IDEuNWEuMjUuMjUgMCAwMC0uMjUuMjV2MTIuNWMwIC4xMzguMTEyLjI1LjI1LjI1aDEyLjVhLjI1LjI1IDAgMDAuMjUtLjI1VjEuNzVhLjI1LjI1IDAgMDAtLjI1LS4yNUgxLjc1ek0wIDEuNzVDMCAuNzg0Ljc4NCAwIDEuNzUgMGgxMi41QzE1LjIxNiAwIDE2IC43ODQgMTYgMS43NXYxMi41QTEuNzUgMS43NSAwIDAxMTQuMjUgMTZIMS43NUExLjc1IDEuNzUgMCAwMTAgMTQuMjVWMS43NXptOS4yMiAzLjcyYS43NS43NSAwIDAwMCAxLjA2TDEwLjY5IDggOS4yMiA5LjQ3YS43NS43NSAwIDEwMS4wNiAxLjA2bDItMmEuNzUuNzUgMCAwMDAtMS4wNmwtMi0yYS43NS43NSAwIDAwLTEuMDYgMHpNNi43OCA2LjUzYS43NS43NSAwIDAwLTEuMDYtMS4wNmwtMiAyYS43NS43NSAwIDAwMCAxLjA2bDIgMmEuNzUuNzUgMCAxMDEuMDYtMS4wNkw1LjMxIDhsMS40Ny0xLjQ3eiIgLz48L3N2Zz4=)
  Exercise Code
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik04IDE2QTggOCAwIDEwOCAwYTggOCAwIDAwMCAxNnptMy43OC05LjcyYS43NS43NSAwIDAwLTEuMDYtMS4wNkw2Ljc1IDkuMTkgNS4yOCA3LjcyYS43NS43NSAwIDAwLTEuMDYgMS4wNmwyIDJhLjc1Ljc1IDAgMDAxLjA2IDBsNC41LTQuNXoiIC8+PC9zdmc+)
  Correct Answer
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Function
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Column
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Direction

We created a new table specifically for this exercise in the
`factor-setup` chunk, drawing from the data provided in
[`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html).

```` markdown
```{r factor-setup}
library(dplyr)
library(forcats)

marital_age <- gss_cat %>%
    mutate(marital = as.character(marital)) %>%
    group_by(marital) %>%
    summarize(avg_age = mean(age, na.rm = TRUE)) %>%
    ungroup()
```

```{r factor, exercise=TRUE}
marital_age
```

```{r factor-solution}
marital_age %>%
    mutate(marital = fct_reorder(marital, avg_age))
```

```{r factor-check}
grade_this_table()
```
````

Since students aren’t familiar with the `marital_age` table, we’ve
included the starter code so they can see the initial state of the table
when they start the exercise.

``` r

marital_age
#> # A tibble: 6 × 2
#>   marital       avg_age
#>   <chr>           <dbl>
#> 1 Divorced         51.1
#> 2 Married          48.7
#> 3 Never married    33.9
#> 4 No answer        52.4
#> 5 Separated        45.3
#> 6 Widowed          71.7
```

In the expected solution, the student will use the
`forecats::fct_reorder()` function to transform the `marital` column,
reordering its factor levels by increasing `avg_age`.

``` r

marital_age %>%
    mutate(marital = fct_reorder(marital, avg_age))
#> # A tibble: 6 × 2
#>   marital       avg_age
#>   <fct>           <dbl>
#> 1 Divorced         51.1
#> 2 Married          48.7
#> 3 Never married    33.9
#> 4 No answer        52.4
#> 5 Separated        45.3
#> 6 Widowed          71.7
```

Splendid! Correct!

Here the student forgets about the
[`forcats::fct_reorder()`](https://forcats.tidyverse.org/reference/fct_reorder.html)
function and tries to use [`order()`](https://rdrr.io/r/base/order.html)
to set the order of the factor levels in the `marital` column.

``` r

marital_age %>%
    mutate(marital = order(marital, avg_age))
#> # A tibble: 6 × 2
#>   marital avg_age
#>     <int>   <dbl>
#> 1       1    51.1
#> 2       2    48.7
#> 3       3    33.9
#> 4       4    52.4
#> 5       5    45.3
#> 6       6    71.7
```

Your `marital` column should be a vector of factors (class `factor`),
but it is a vector of integers (class `integer`). Try it again. You get
better each time.

In this case the student remembers to use `forecats::fct_reorder()` but
incorrectly assigns the result to an unexpected column.

``` r

marital_age %>%
    mutate(marital_fct = fct_reorder(marital, avg_age))
#> # A tibble: 6 × 3
#>   marital       avg_age marital_fct  
#>   <chr>           <dbl> <fct>        
#> 1 Divorced         51.1 Divorced     
#> 2 Married          48.7 Married      
#> 3 Never married    33.9 Never married
#> 4 No answer        52.4 No answer    
#> 5 Separated        45.3 Separated    
#> 6 Widowed          71.7 Widowed
```

Your table should not have a column named `marital_fct`. Try it again.
You get better each time.

In another case, the student uses `forecats::fct_reorder()` but sets
`.desc = TRUE`, resulting in an unexpected ordering of the factor
levels.

``` r

marital_age %>%
    mutate(marital = fct_reorder(marital, avg_age, .desc = TRUE))
#> # A tibble: 6 × 2
#>   marital       avg_age
#>   <fct>           <dbl>
#> 1 Divorced         51.1
#> 2 Married          48.7
#> 3 Never married    33.9
#> 4 No answer        52.4
#> 5 Separated        45.3
#> 6 Widowed          71.7
```

The order of the levels in your `marital` column are the reverse of the
expected order. Try it again. You get better each time.

## Subset a Vector with stringr

In addition to table-checking functions, tblcheck includes [functions
for checking
vectors](https://rstudio.github.io/tblcheck/articles/tblcheck.html#grading-vectors)
— after all, columns in a table are vectors!

Here’s an example exercise from a tutorial on string transformations
with the [stringr package](https://stringr.tidyverse.org). Students are
asked to practice concepts they just discovered in the tutorial:

> **Prompt**  
> Consider the `fruit` data. Use a function from
> [stringr](https://stringr.tidyverse.org) to create a vector containing
> only the fruits with more than one word in their name.
>
> *Hint: a fruit named with more than one word also has at least one
> space in its name.*

- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xLjc1IDEuNWEuMjUuMjUgMCAwMC0uMjUuMjV2MTIuNWMwIC4xMzguMTEyLjI1LjI1LjI1aDEyLjVhLjI1LjI1IDAgMDAuMjUtLjI1VjEuNzVhLjI1LjI1IDAgMDAtLjI1LS4yNUgxLjc1ek0wIDEuNzVDMCAuNzg0Ljc4NCAwIDEuNzUgMGgxMi41QzE1LjIxNiAwIDE2IC43ODQgMTYgMS43NXYxMi41QTEuNzUgMS43NSAwIDAxMTQuMjUgMTZIMS43NUExLjc1IDEuNzUgMCAwMTAgMTQuMjVWMS43NXptOS4yMiAzLjcyYS43NS43NSAwIDAwMCAxLjA2TDEwLjY5IDggOS4yMiA5LjQ3YS43NS43NSAwIDEwMS4wNiAxLjA2bDItMmEuNzUuNzUgMCAwMDAtMS4wNmwtMi0yYS43NS43NSAwIDAwLTEuMDYgMHpNNi43OCA2LjUzYS43NS43NSAwIDAwLTEuMDYtMS4wNmwtMiAyYS43NS43NSAwIDAwMCAxLjA2bDIgMmEuNzUuNzUgMCAxMDEuMDYtMS4wNkw1LjMxIDhsMS40Ny0xLjQ3eiIgLz48L3N2Zz4=)
  Exercise Code
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik04IDE2QTggOCAwIDEwOCAwYTggOCAwIDAwMCAxNnptMy43OC05LjcyYS43NS43NSAwIDAwLTEuMDYtMS4wNkw2Ljc1IDkuMTkgNS4yOCA3LjcyYS43NS43NSAwIDAwLTEuMDYgMS4wNmwyIDJhLjc1Ljc1IDAgMDAxLjA2IDBsNC41LTQuNXoiIC8+PC9zdmc+)
  Correct Answer
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Class
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Length
- ![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij48cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0yLjM0MyAxMy42NTdBOCA4IDAgMTExMy42NTcgMi4zNDMgOCA4IDAgMDEyLjM0MyAxMy42NTd6TTYuMDMgNC45N2EuNzUuNzUgMCAwMC0xLjA2IDEuMDZMNi45NCA4IDQuOTcgOS45N2EuNzUuNzUgMCAxMDEuMDYgMS4wNkw4IDkuMDZsMS45NyAxLjk3YS43NS43NSAwIDEwMS4wNi0xLjA2TDkuMDYgOGwxLjk3LTEuOTdhLjc1Ljc1IDAgMTAtMS4wNi0xLjA2TDggNi45NCA2LjAzIDQuOTd6IiAvPjwvc3ZnPg==)
  Wrong Subset

In the exercise markdown, we call
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/reference/grade_this_vector.md)
in the `string-check` chunk to automatically compare the result of the
student’s submission with our `string-solution`.

```` markdown
```{r string-setup}
library(dplyr)
library(stringr)
```

```{r string, exercise=TRUE}

```

```{r string-solution}
str_subset(fruit, " ")
```

```{r string-check}
grade_this_vector()
```
````

Note that `fruit` is from
[`stringr::fruit`](https://stringr.tidyverse.org/reference/stringr-data.html),
a vector containing 80 fruit names.

``` r

head(stringr::fruit, 20)
#>  [1] "apple"        "apricot"      "avocado"      "banana"      
#>  [5] "bell pepper"  "bilberry"     "blackberry"   "blackcurrant"
#>  [9] "blood orange" "blueberry"    "boysenberry"  "breadfruit"  
#> [13] "canary melon" "cantaloupe"   "cherimoya"    "cherry"      
#> [17] "chili pepper" "clementine"   "cloudberry"   "coconut"
```

11 of the 80 fruits have more than one word. What’s your favorite
two-worded fruit name? (Mine is the [purple
mangosteen](https://en.wikipedia.org/wiki/Mangosteen).)

``` r

str_subset(fruit, " ")
#>  [1] "bell pepper"       "blood orange"      "canary melon"     
#>  [4] "chili pepper"      "goji berry"        "kiwi fruit"       
#>  [7] "purple mangosteen" "rock melon"        "salal berry"      
#> [10] "star fruit"        "ugli fruit"
```

Lovely job! Correct!

This student submission makes a potentially common mistake of hoping
that
[`stringr::str_which()`](https://stringr.tidyverse.org/reference/str_which.html)
will return the `fruit` which have `pattern`. Instead, `str_which()`
follows the base R function
[`which()`](https://rdrr.io/r/base/which.html), and returns an `integer`
index indicating the items in `fruit` where the `pattern` is matched.

``` r

str_which(fruit, " ")
#>  [1]  5  9 13 17 32 42 66 72 73 75 79
```

Your result should be a vector of text (class `character`), but it is a
vector of integers (class `integer`). Try it again. You get better each
time.

In another mistake example, this student probably expected
[`stringr::str_extract()`](https://stringr.tidyverse.org/reference/str_extract.html)
to *extract* elements from the vector. However, `str_extract()` instead
extracts the pattern from each item, so it returns a vector with the
same length as the input.

``` r

str_extract(fruit, " ")
#>  [1] NA  NA  NA  NA  " " NA  NA  NA  " " NA  NA  NA  " " NA  NA  NA 
#> [17] " " NA  NA  NA 
#>  [ reached 'max' / getOption("max.print") -- omitted 60 entries ]
```

Your result should contain 11 values, but it has 80 values. But no need
to fret, try it again.

Finally, a student might have chosen the right function, but they missed
our hint and came up with a pattern that’s too clever. Here the student
probably looked at the first 20 fruit names and took a guess at the
answer.

``` r

str_subset(fruit, " (pepper|orange|melon|fruit)")
#> [1] "bell pepper"  "blood orange" "canary melon" "chili pepper"
#> [5] "kiwi fruit"   "rock melon"   "star fruit"   "ugli fruit"
```

Your result should contain 11 values, but it has 8 values. I expected
your result to include the values `goji berry`, `purple mangosteen`, and
`salal berry`. That’s okay: you learn more from mistakes than successes.
Let’s do it one more time.
