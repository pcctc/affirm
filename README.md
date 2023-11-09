
<!-- README.md is generated from README.Rmd. Please edit that file -->

# affirm <a href="https://github.com/pages/pcctc/affirm/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/pcctc/affirm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pcctc/affirm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/pcctc/affirm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pcctc/affirm?branch=main)
<!-- badges: end -->

The {affirm} package makes daily affirmation against our data. We use
this package to affirm our raw data is as expected, and our derived
variables continue to be accurate as data is updated.

1.  Raw EDC Data: Use the affirmation functions to make assertions about
    the raw EDC data. Report errors back to data management, and in some
    cases make ‘corrections’ to the data. The issue will still be
    reported to DM, but these small corrections let us continue our work
    without dealing with data that should not be present in the data
    base and simplifies much of our logic downstream.

2.  Derived Variables: Each time we derive a new variable, use data
    affirmation functions to affirm the derivation *continues* to be
    accurate as the data updates. We’ll often use the
    `affirm_true(error=TRUE)` in this setting, which throws an error
    when our underlying assumptions are not true and will require us to
    address the issue before continuing.

The [{pointblank}](https://rstudio.github.io/pointblank/) is
another package that performs data validations. {pointblank} is far more
comprehensive than {affirm}, and {affirm} utilizes many of the ideas and
reporting introduced in {pointblank} with defaults and reports tailored
for the needs of the PCCTC organization. If {affirm} is not a perfect
match for you, {pointblank} will likely meet all your validation needs!

## Installation

1.  Install the most recent release of {affirm} with

    ``` r
    devtools::install_github("pcctc/affirm@*release")
    ```

2.  Install from a development branch for testing

    ``` r
    devtools::install_github("pcctc/affirm")
    ```

## Examples

Load the package and initialize a new affirmation session with
`affirm_init()`

``` r
library(affirm)
#> 
#> Attaching package: 'affirm'
#> The following object is masked from 'package:stats':
#> 
#>     filter

# initiate an affirmation session
affirm_init(replace = TRUE)
#> ✔ We're ready to make data affirmations...
```

Run the individual affirmations…

``` r
as_tibble(mtcars) |>
  affirm_true(
    label = "No. cylinders must be 4, 6, or 8",
    condition = cyl %in% c(4, 6, 8),
    id = 1,
    data_frames = "mtcars"
  ) |>
  affirm_true(
    label = "MPG should be less than 33",
    condition = mpg < 33,
    id = 2,
    data_frames = "mtcars"
  )
#> • No. cylinders must be 4, 6, or 8
#>   0 issues identified.
#> • MPG should be less than 33
#>   1 issue identified.
#> # A tibble: 32 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> # ℹ 29 more rows
```

Create a report of the results

``` r
affirm_report_gt()
```

![](man/figures/README-gt-report.png)

### About the hex logo

The hex art is a depiction of Plato’s [allegory of the
cave](https://en.wikipedia.org/wiki/Allegory_of_the_cave). Each data
table has an ideal
[form](https://en.wikipedia.org/wiki/Theory_of_forms), and the {affirm}
package will help us confirm that we are viewing it. Also, “forms”…get
it? 😜
