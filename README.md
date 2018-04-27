
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [statsr: Companion Package for Statistics with R](http://github.org/StatsWithR/statsr)

[![Build
Status](http://travis-ci.org/StatsWithR/statsr.svg?branch=master)](http://travis-ci.org/StatsWithR/statsr)

The `R` package `statsr` provides functions and datasets to support the
Coursera [*Statistics with `R`
Specialization*](https://www.coursera.org/specializations/statistics)
videos and open access book [*An Introduction to Bayesian
Thinking*](https://statswithr.github.io/book) for learning Bayesian and
freqentist statistics using `R`.

To install the latest version from github, verify that there is a
passing badge above on the README page. In `R` enter

``` r
library(devtools)
devtools::install_github("statswithr/statsr",
                         dependencies=TRUE,
                         upgrade_dependencies = TRUE)
```

This will install the packages and any packages that are required, as
well as updating any installed packages to their latest versions.
