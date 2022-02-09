
<img src="vignettes/SepsisR.png" alt="SepsisR logo" align="right" width = "25%" height="25%"/>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# SepsisR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/trentgillin/SepsisR.svg?branch=master)](https://travis-ci.org/trentgillin/SepsisR)
[![Codecov test
coverage](https://codecov.io/gh/trentgillin/SepsisR/branch/master/graph/badge.svg)](https://codecov.io/gh/trentgillin/SepsisR?branch=master)
<!-- badges: end -->

Sepsis can present in many different ways. Many professionals recognize
sepsis’ presentation, but the literature is undecided when it comes to
an exact clinical definition. The purpose of this package is to provide
functions in order to make the process of identifying and analyzing
sepsis patients easier. The assess\_organ function is the main function
of this package; it allows you to assess organ failure using one of the
three common organ failure scores, SIRS, SOFA, and qSOFA. Currently, all
score vital thresholds are based off the MD Calc Website. Please view
the description file to see MD Calc references for individual organ
failure scores. This package also has a few helper functions including
find\_bx\_window and find\_qad, from the CDC (Centers for Disease and
Control) definition of Sepsis. This package is not in anyway supported
by the CDC or Emory Healthcare.

## Installation

Get the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("trentgillin/Sepsis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SepsisR)

assess_organ(sirs_data, method = "SIRS",
                 patientid = Encounter, 
                time = Service_Timestamp, 
                vitals = c("RR" = "RR", 
                           "WBC" = "Wbc", 
                    "Temperature" = "Temperature",
                    "HR" = "HR"))
#> # A tibble: 9 × 7
#>   Encounter Service_Timestamp      HR Temperature    RR   Wbc sirs_total
#>       <dbl> <dttm>              <dbl>       <dbl> <dbl> <dbl>      <dbl>
#> 1         1 2018-12-11 08:00:00   120          31    20  5000          2
#> 2         1 2018-12-11 08:30:00    72          31    20  5000          1
#> 3         1 2018-12-11 09:00:00    85          33    21  5000          2
#> 4         1 2018-12-11 09:30:00    90          31    22  5000          2
#> 5         1 2018-12-11 10:00:00   111          31    20  5000          2
#> 6         1 2018-12-11 10:30:00    64          32    20  5000          1
#> 7         1 2018-12-11 11:00:00    70          38    20  5000          0
#> 8         1 2018-12-11 11:30:00    75          33    20  5000          1
#> 9         1 2018-12-11 12:00:00    81          31    20  5000          1
```
