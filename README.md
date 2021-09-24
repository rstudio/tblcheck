
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tblcheck

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/tblcheck?color=red)](https://cran.r-project.org/package=tblcheck)
[![R build
status](https://github.com/rstudio/tblcheck/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/tblcheck/actions)
[![](https://codecov.io/gh/rstudio/tblcheck/branch/main/graph/badge.svg)](https://codecov.io/gh/rstudio/tblcheck)
[![](http://cranlogs.r-pkg.org/badges/last-month/tblcheck?color=blue)](https://cran.r-project.org/package=tblcheck)
<!-- badges: end -->

tblcheck provides functions for grading tibbles, data frames, and
vectors with [gradethis](https://pkgs.rstudio.com/gradethis/).

gradethis is designed for use with
[gradethis](https://pkgs.rstudio.com/gradethis/) in
[learnr](https://rstudio.github.io/learnr/) tutorials. We recommend that
you first be comfortable grading
[learnr](https://rstudio.github.io/learnr/) tutorials with
[gradethis](https://pkgs.rstudio.com/gradethis/) before you begin using
tblcheck. You can learn more with the [gradethis package
documentation](https://pkgs.rstudio.com/gradethis/) and the [learnr
package documentation](https://rstudio.github.io/learnr/).

![A screenshot of the tblcheck package in action. An exercise starts
with the following instructions: ‘Create a tibble with two columns. The
first column should be called “fruit” and contain the value “tomato”.
The second column should be called “color” and contain the value “red”.’
The student has entered the following code: ‘tibble(vegetable =
“tomato”, color = “red”)’. The tblcheck package generates the following
message: ‘Your table should have a column named “fruit”. Your table
should not have a column named “vegetable”. Please try
again.’](man/figures/screenshot.png)

## Installation

<!-- You can install the released version of tblcheck from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("tblcheck") -->
<!-- ``` -->

tblcheck is still in development and not on
[CRAN](https://CRAN.R-project.org) yet. The development version of
tblcheck can be installed from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("rstudio/tblcheck")
```

## Usage

To use tblcheck in a learnr tutorial, start by loading tblcheck after
learnr and gradethis in the `setup` chunk of your tutorial:

```` markdown
```{r setup}
library(learnr)
library(gradethis)
library(tblcheck)
```
````

Then include one of the tblcheck functions in your gradethis code:

```` markdown
```{r exercise-check}
grade_this({
  pass_if_equal()
  tbl_grade_table()
  fail()
})
```
````

Learn more about the various tblcheck functions in vignette(“tblcheck”).

## Code of Conduct

Please note that the tblcheck project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
