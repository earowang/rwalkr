# rwalkr 0.4.0

* Fixed status "400" for `melb_walk_fast()` (previously `run_melb()`) due to Socrata API changes.
* Deprecated `walk_melb()`, `run_melb()` and `shine_melb()` in favour of suffixed function names.

# rwalkr 0.3.4

* Fixed date-time formatting for Socrata data update

# rwalkr 0.3.3

## API changes

* Dropped the argument of `tweak` in `walk_melb()`, as the sensor names from the data source match with `run_melb()`.

## Minor changes

* Dropped the `match` column in the data frame called from `lookup_sensor()`.

# rwalkr 0.3.2

## Major changes

* Returned a tibble (`tbl_ts`) instead of data.frame.

## Minor changes

* Specified the requirement version of shiny to the DESCRIPTION file.

# rwalkr 0.3.1

## Bug fixes

* Fixed "Count" to be returned as integers instead of characters in `run_melb(na.rm = FALSE)`.
* Fixed duplicated data entries when `walk_melb(tweak = TRUE)`.
* Fixed one non-matching sensor in `lookup_sensor()`.

## Updates

* A new sensor "Pelham St (S)" added to `run_melb()`, `pull_sensor()`, and `lookup_sensor()` using Socrata.
* Changed the shiny app using `shine_melb()` to use `walk_melb(tweak = TRUE)`.

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


