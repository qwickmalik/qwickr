
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qwickr

A quick and easy way to summarize clinical trial and other similar data,
including hypothesis testing and reporting results in Excel and Word
formats

## Installation

`qwickr` is still in development and therefore is not on
[CRAN](https://CRAN.R-project.org) yet. However, users may install the
development version using

``` r
remotes::install_github("qwickmalik/qwickr")
```

Alternatively, the zip file can be downloaded from the
“qwickmalik/qwickr/install” folder

## Use

Load the package using

``` r
library(qwickr)
```

The main functions in this package are prefixed with `qwickr.`, while
helper functions are prefixed with `q.`

## Example

Summaries for categorical data

``` r
group <- rep(c("A", "B"), 10)
gender <- rep(c(1,1,0,0), 5)
time <- rep(1, 10)
df <- data.frame(group, gender, time)

qwickr.cat(x=df, outcomevar="gender", groupvar = "group", timevar = "time")
```
