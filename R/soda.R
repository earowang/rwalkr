globalVariables(c(
  "Time", "Count", "Sensor", "Date", "Date_Time", # run_melb()
  "Sensor_ID", "Longitude", "Latitude", "Location_Type", "Year_Installed",
  "sensorloc", "sensorid", "longitude", "latitude", "loctype", "xdate"
))

#' API using Socrata to Melbourne pedestrian data
#'
#' Provides API using Socrata to Melbourne pedestrian data in a tidy data form.
#'
#' @param year An integer or a vector of integers. By default, it's the current #'   year.
#' @param sensor Sensor names. By default, it pulls all the sensors.
#' @param tz Time zone. By default, "" is the current time zone. For this dataset,
#'   the local time zone is "Australia/Melbourne" which would be the most
#'   appropriate, depending on OS.
#' @param na.rm Logical. `FALSE` is the default suggesting to include `NA` in #'   the datset. `TRUE` removes the `NA`s.
#'
#' @details It provides API using [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/mxb8-wn4w), 
#'   where counts are uploaded on a monthly basis. The up-to-date data would be
#'   till the previous month. The data is sourced from [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp). Please 
#'   refer to Melbourne Open Data Portal for more details about the dataset and 
#'   its policy.
#' @return A data frame including these variables as follows:
#'   * Sensor: Sensor name (45 sensors up to date)
#'   * Date_Time: Date time when the pedestrian counts are recorded
#'   * Date: Date associated with Date_Time
#'   * Time: Time of day
#'   * Count: Hourly counts
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
run_melb <- function(year = NULL, sensor = NULL, tz = "", na.rm = FALSE) {
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
  nsensors <- 50L
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
      tz = tz),
    Date = as.Date.POSIXct(Date_Time, tz = tz),
    Count = as.integer(Count),
    Time = as.integer(Time),
  )
  from_time <- as.POSIXct(paste0(min(year, na.rm = TRUE), "-01-01 00:00"),
      tz = tz)
  max_year <- max(year, na.rm = TRUE)
  to_time <- if (max_year == this_year) {
    max(ped$Date_Time)
  } else {
    as.POSIXct(paste0(max_year, "-12-31 23:00"), tz = tz)
  }
  ped <- dplyr::distinct(ped) # remove duplicates
  ped <- dplyr::filter(ped, Date_Time >= from_time, Date_Time <= to_time)

  if (!na.rm) {
    # Make implicit missingness to explicit
    full_time <- seq.POSIXt(from = from_time, to = to_time, by = "hour")
    len_time <- length(full_time)
    available_sensors <- unique(ped$Sensor)
    len_sensor <- length(available_sensors)
    full_df <- data.frame(
      Sensor = available_sensors[rep.int(
        seq_along(available_sensors), len_time
      )],
      Date_Time = rep(full_time, each = len_sensor),
      stringsAsFactors = FALSE
    )
    ped <- dplyr::left_join(full_df, ped, by = c("Sensor", "Date_Time"))
    ped <- dplyr::mutate(ped, Time = as.integer(substr(Date_Time, 12, 13)))
  }

  ped <- dplyr::select(ped, Sensor, Date_Time, Date, Time, Count)
  dplyr::arrange(ped, Date_Time)
}

#' API using Socrata to Melbourne pedestrian sensor locations
#'
#' Provides API using Socrata to Melbourne pedestrian sensor locations.
#'
#' @details It provides API using [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/xbm5-bb4n).
#'
#' @return A data frame including these variables as follows:
#'   * Sensor: Sensor name (43 sensors up to date)
#'   * Sensor_ID: Sensor identifier
#'   * Longitude: Longitude
#'   * Latitude: Latitude
#'   * Location_Type: Location type
#'   * Year_Installed: Year installed
#'
#' @export
#' @seealso [run_melb]
#'
#' @examples
#' \dontrun{
#'   pull_sensor()
#' }
pull_sensor <- function() {
  base_url <- "https://data.melbourne.vic.gov.au/resource/xbm5-bb4n.csv"
  response <- httr::GET(httr::parse_url(base_url))
  content <- httr::content(response, as = "text", type = "text/csv", 
    encoding = "UTF-8")
  sensor_info <- utils::read.csv(
    textConnection(content), 
    colClasses = rep("character", 10L),
    stringsAsFactors = FALSE,
    nrows = 50L # give a buffer to 43
  )
  sensor_info <- dplyr::select(
    sensor_info, sensorloc, sensorid, longitude, latitude, loctype, xdate
  )
  colnames(sensor_info) <- c("Sensor", "Sensor_ID", "Longitude", "Latitude",
    "Location_Type", "Year_Installed")
  sensor_info <- dplyr::mutate(
    sensor_info,
    Sensor_ID = as.integer(Sensor_ID),
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    Year_Installed = as.integer(Year_Installed)
  )
  sensor_info
}
