
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qwickr

<!-- badges: start -->

<!-- badges: end -->

qwickr provides a quick and easy way to analyze clinical trial and other
similar data, and report results in Word format.

## Installation

You can install the released version of qwickr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("qwickr")
```

## Example

This is a basic example which shows you how to run a Chi Square test or
Fisher’s Exact test and export the result Word:

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
library(qwickr)
```
