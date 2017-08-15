globalVariables(c(
  "Time", "Hourly_Counts", "Sensor_ID", "Sensor_Name", "Date_Time", "Day", "ID",
  "Mdate", "Month", "Year"
))

#' API using Socrata to Melbourne pedestrian data using R
#'
#' Provides API using Socrata to Melbourne pedestrian data in a tidy data form.
#'
#' @param year Numeric(s). By default, it's the current year.
#' @param sensor Sensor names. By default, it pulls all the sensors.
#' @param tz Time zone. By default, "" is the current time zone. For this dataset,
#'   the local time zone is "Australia/Melbourne" which would be the most
#'   appropriate, depending on OS.
#'
#' @details It provides API using [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/mxb8-wn4w), 
#'   where counts are uploaded on a monthly basis. The up-to-date data would be
#'   till the previous month. The data is sourced from [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp). Please 
#'   refer to Melbourne Open Data Portal for more details about the dataset and 
#'   its policy.
#' @return A data frame including these variables as follows:
#'   * Date_Time: Date time when the pedestrian counts are recorded
#'   * Day: Week day
#'   * ID
#'   * Mdate: Day of month
#'   * Month: Month
#'   * Hourly_Counts: Hourly counts
#'   * Sensor_ID: Sensor ID
#'   * Sensor_Name: Sensor name
#'   * Time: Time of day
#'   * Year: Year
#'   Implicit missingness may occur to the data over a specified period.
#'
#' @export
#' @seealso [walk_melb]
#'
#' @examples
#' \dontrun{
#'   # Retrieve the year 2017
#'   ped_df17 <- run_melb(year = 2017)
#'   head(ped_df17)
#'   
#'   # Retrieve the year 2017 for Southern Cross Station
#'   sx_df17 <- run_melb(year = 2017, sensor = "Southern Cross Station")
#'   head(sx_df17)
#' }
run_melb <- function(year = NULL, sensor = NULL, tz = "") {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(year)) {
    year <- this_year
  }
  stopifnot(year > 2008 && year < (this_year + 1L))
  base_url <- "https://data.melbourne.vic.gov.au/resource/mxb8-wn4w.csv?"
  year_str <- paste(year, collapse = ", ")
  url <- paste0(base_url, "$where=year in", "(", year_str, ")")
  if (!is.null(sensor)) {
    sensor_str <- paste(
      vapply(sensor, function(x) paste0("'", x, "'"), character(1)), 
      collapse = ", "
    )
    url <- paste0(url, "AND sensor_name in", "(", sensor_str, ")")
  } 
  ped <- RSocrata::read.socrata(url)
  colnames(ped) <- c("Date_Time", "Day", "ID", "Mdate", "Month", 
    "Hourly_Counts", "Sensor_ID", "Sensor_Name", "Time", "Year")
  ped <- dplyr::mutate(
    ped, 
    Date_Time = as.POSIXct(strptime(Date_Time, format = "%d-%B-%Y %H:%M"),
      tz = tz),
    ID = as.integer(ID),
    Mdate = as.integer(Mdate),
    Hourly_Counts = as.integer(Hourly_Counts),
    Sensor_ID = as.integer(Sensor_ID),
    Time = as.integer(Time),
    Year = as.integer(Year)
  )
  dplyr::arrange(ped, Date_Time)
}

