<!-- README.md is generated from README.Rmd. Please edit that file -->
pedestrian
==========

The goal of pedestrian is to provide API to the pedestrian data from the City of Melbourne in tidy data form.

Installation
------------

You could install the development version from Github using

``` r
# install.packages("devtools")
devtools::install_github("earowang/pedestrian")
```

Usage
-----

Currently there is only one function in the package, `get_pedestrian()`. The starting and ending date inform which period to be scraped.

``` r
get_pedestrian()
get_pedestrian(from = as.Date("2017-01-01"), to = as.Date("2017-02-28"))
```
