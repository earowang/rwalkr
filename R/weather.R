globalVariables(c("site", "sensor_type", "units", "value",
                  "site_id", "description", "last_data",
                  "last_update", "type"))

#' API to access Melbourne microclimate sensor data
#'
#' @param from Starting date. Earliest measurement is 2019-11-15
#' @param to Ending date.
#' @param site The site identifier. By default will pull in all locations that have weather sensors [pull_weather_sensors()].
#' @param sensor_type The type of microclimate measurement to extract see [pull_weather_types()] for details.
#' @param app_token Characters giving the application token. A limited number of
#' requests can be made without an app token (`NULL`), but they are subject
#' to much lower throttling limits than request that do include one. Sign up
#' for an app token [here](https://data.melbourne.vic.gov.au/profile/app_tokens).
#'
#' @details It provides the API using [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/u4vh-84j8),
#' where microclimate measurements are updated on a dailly basis.
#' For data documentation, including a data dictionary see the [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Environment/Microclimate-Sensor-Readings/u4vh-84j8).
#' Please refer to Melbourne Open Data Portal for more details about the dataset and
#' its policy.
#'
#' @return A tibble including these variables as follows:
#' * `site`: Site identifier, this is the location of the weather sensor
#' * `date_time`: Date time when the measurement was recorded
#' * `date`: Date associated with `date_time`
#' * `sensor_type`: The type of microclimate sensor reading
#' * `units`: The units that `value` is in
#' * `value`: The value of the reading
#' @seealso [melb_walk], [pull_weather_sensors], [pull_weather_types]
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve the last weeks data
#' melb_weather()
#'
#' # Retrieve the last week but for a single location (Pelham St, Carlton)
#' melb_weather(site = "arc1047")
#'
#' # Retrieve the last week but only ambient air temperature
#' melb_weather(sensor_type = "TPH.TEMP")
#'
#' }
melb_weather <- function(
  from = to - 6L, to = Sys.Date(), site = NULL, sensor_type = NULL,
  app_token = NULL
) {

  tz <- "Australia/Melbourne"
  stopifnot(class(from) == "Date" && class(to) == "Date")
  stopifnot(from >= as.Date("2019-11-15"))
  stopifnot(from <= to)
  today <- Sys.Date()
  if (to > today) {
    warning(
      sprintf("The data is only avaiable up to %s.", today),
      call. = FALSE
    )
    to <- today
  }

  base_url <- "https://data.melbourne.vic.gov.au/resource/u4vh-84j8.csv?"
  sel_cols <- paste(
    "SELECT site_id AS site,",
    "local_time AS date_time,",
    "type AS sensor_type,",
    "units AS units,",
    "value AS value"
  )

  # filter on API side
  query <- paste0(sel_cols,
                  " WHERE local_time BETWEEN ",
                  paste0("'", from, "T00:00:00'"),
                  " AND ",
                  paste0("'", to, "T23:59:59'"))

  # set up endpoints for sites and sensor measurements
  nsites <- 5L
  if (!is.null(site)) {
    site_str <- paste(
      vapply(site, function(x) paste0("'", x, "'"), character(1)),
      collapse = ", "
    )
    query <- paste0(query, "AND site_id in", "(", site_str, ")")
    nsites[] <- length(site)
  }
  nsensors <- 18L
  if (!is.null(sensor_type)) {
    sensor_str <- paste(
      vapply(sensor_type, function(x) paste0("'", x, "'"), character(1)),
      collapse = ", "
    )
    query <- paste0(query, "AND type in", "(", sensor_str, ")")
    nsensors[] <- length(sensor_type)
  }

  query <- paste0(query, " ORDER BY :id LIMIT 50000")
  limit <- 50000L

  # roughly the number of pages going through
  ndays <- as.integer(to - from)
  npages <- ceiling((ndays * 48L * nsensors * nsites) / limit)

  p <- progress::progress_bar$new(total = npages,
    format = "downloading [:bar] :percent eta: :eta")
  lst_dat <- lapply(seq_len(npages), function(x) {
    offset <- sprintf("%i", limit * (x - 1))
    update_query <- paste0(query, " OFFSET ", offset)
    if (!is.null(app_token)) {
      app_token <- paste0("$$app_token=", app_token)
      base_url <- paste0(base_url, app_token)
    }
    response <- httr::GET(base_url, query = list("$query" = update_query))
    content <- httr::content(response, as = "text", type = "text/csv",
                             encoding = "UTF-8")
    dat <- dplyr::as_tibble(utils::read.csv(
      textConnection(content),
      stringsAsFactors = FALSE,
      colClasses = rep("character", 5),
      nrows = limit
    ))
    p$tick()
    dat
  })

  weather <- dplyr::bind_rows(lst_dat)
  weather <- dplyr::mutate(
    weather,
    date_time = as.POSIXct(strptime(date_time, format = "%Y-%m-%dT%H:%M:%S"),
                           tz = tz),
    date = as.Date.POSIXct(date_time, tz = tz),
    value = as.numeric(value)

  )
  weather <- dplyr::arrange(weather, date_time)
  weather <- dplyr::select(weather, site, date_time,
                           date, sensor_type, units, value)
  weather
}


#' API using Socrata to Melbourne microclimate measurement types
#'
#'
#' @inheritParams pull_sensor
#'
#' @details Extract all available climate sensor types and their identifiers
#' [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/u4vh-84j8).
#'
#' @export
#' @seealso [melb_weather]
#'
#' @examples
#' \dontrun{
#' pull_weather_types()
#' }
pull_weather_types <- function(app_token = NULL) {
  base_url <- "https://data.melbourne.vic.gov.au/resource/u4vh-84j8.csv?"
  query <- "SELECT DISTINCT sensor_id, type ORDER BY sensor_id"

  if (!is.null(app_token)) {
    app_token <- paste0("$$app_token=", app_token)
    base_url <- paste0(base_url, app_token)
  }

  response <- httr::GET(base_url, query = list("$query" = query))
  content <- httr::content(response, as = "text", type = "text/csv",
                           encoding = "UTF-8")
  dat <- dplyr::as_tibble(utils::read.csv(
    textConnection(content),
    colClasses = rep("character", 2L),
    stringsAsFactors = FALSE,
    nrows = 100
  ))

  dplyr::select(dat,
                sensor_id = sensor_id,
                sensor_type = type
   )
}



#' API using Socrata to extract Melbourne microclimate sensor locations
#'
#'
#' @inheritParams pull_sensor
#'
#' @details Extract all available climate sensor types and their identifiers
#' [Socrata](https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/irqv-hjr4).
#'
#' @export
#' @seealso [melb_weather]
#'
#' @examples
#' \dontrun{
#' pull_weather_types()
#' }
pull_weather_sensors <- function(app_token = NULL) {
  base_url <- "https://data.melbourne.vic.gov.au/resource/irqv-hjr4.csv"
  p_url <- httr::parse_url(base_url)
  if (!is.null(app_token)) p_url$query$`$$app_token` <- app_token
  response <- httr::GET(p_url)
  content <- httr::content(response, as = "text", type = "text/csv",
                           encoding = "UTF-8")
  sensor_info <- utils::read.csv(
    textConnection(content),
    colClasses = rep("character", 6L),
    stringsAsFactors = FALSE,
    na.strings = "",
    nrows = 5L
  )

  sensor_info <- dplyr::select(
    sensor_info,
    site_id, longitude, latitude,
    description,
    last_update = last_data
  )
  sensor_info <- dplyr::mutate(
    sensor_info,
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    last_update = as.POSIXct(
      strptime(last_update, format = "%Y-%m-%dT%H:%M:%S"),
      tz = "Australia/Melbourne")
  )
  dplyr::as_tibble(sensor_info)
}


