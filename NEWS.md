# rwalkr 0.3.0

## New functions

* `run_melb()` pulls Melbourne pedestrian data using Socrata, which is faster than `walk_melb()`.
* `pull_sensor()` pulls Melbourne pedestrian sensor locations using Socrata.
* `lookup_sensor()` provides a dictionary for sensor names used in `walk_melb()` and `run_melb()`.

## Minor changes

* Added new arguments `na.rm = FALSE` and `tweak = FALSE` to the function `walk_melb()`. If `na.rm = TRUE`, it removes `NA`s from the data. If `tweak = TRUE`, it ensures the consistency of sensor names to `run_melb()`.

# rwalkr 0.2.0

* Added the function `shine_melb()` to launch a shiny app. It provides two basic plots to take a glimpse at the data: one is an overlaying time series plot and the other showing a dot plot of missing values.

# rwalkr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added the function `walk_melb()` to scrape Melbourne pedestrian data.


