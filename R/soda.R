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
#'   * Sensor: Sensor name (43 sensors up to date)
#'   * Date_Time: Date time when the pedestrian counts are recorded
#'   * Date: Date associated with Date_Time
#'   * Time: Time of day
#'   * Count: Hourly counts
#'   Explicit missingness (`NA`) may occur to the data over a specified period.
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
  sel_cols <- paste(
    "$query=SELECT sensor_name AS Sensor,",
    "daet_time AS Date_Time,",
    "time AS Time,",
    "qv_market_peel_st AS Count"
  )
  year_str <- paste(year, collapse = ", ")
  query <- paste0(sel_cols, " WHERE year in", "(", year_str, ")")
  nsensors <- 43L
  if (!is.null(sensor)) {
    sensor_str <- paste(
      vapply(sensor, function(x) paste0("'", x, "'"), character(1)), 
      collapse = ", "
    )
    query <- paste0(query, "AND sensor_name in", "(", sensor_str, ")")
    nsensors[] <- length(sensor) # overwrite nsensors
  } 
  query <- paste0(query, " ORDER BY :id LIMIT 50000")
  limit <- 50000L

  # roughly the number of pages going through
  npages <- ceiling((366L * 24L * nsensors * length(year)) / limit)

  p <- dplyr::progress_estimated(npages)
  lst_dat <- lapply(seq_len(npages), function(x) {
    offset <- sprintf("%i", limit * (x - 1))
    update_query <- paste0(query, " OFFSET ", offset)
    url <- paste0(base_url, update_query)
    response <- httr::GET(httr::parse_url(url))
    content <- httr::content(response, as = "text", type = "text/csv", 
      encoding = "UTF-8")
    dat <- utils::read.csv(
      textConnection(content), 
      colClasses = rep("character", 4L),
      stringsAsFactors = FALSE,
      nrows = limit
    )
    p$tick()$print()
    dat
  })

  ped <- dplyr::bind_rows(lst_dat)
  ped <- dplyr::mutate(
    ped, 
    Date_Time = as.POSIXct(strptime(Date_Time, format = "%d-%B-%Y %H:%M"),
      tz = ""),
    Date = as.Date.POSIXct(Date_Time, tz = ""),
    Count = as.integer(Count),
    Time = as.integer(Time),
  )
  ped <- dplyr::distinct(ped) # remove duplicates
  ped <- dplyr::select(ped, Sensor, Date_Time, Date, Time, Count)

  dplyr::arrange(ped, Date_Time)
}
