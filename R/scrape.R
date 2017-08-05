globalVariables(c("Time", "Count", "Sensor", "Date", "Date_Time"))

#' API to Melbourne pedestrian data using R
#'
#' Provides API to Melbourne pedestrian data in a tidy data form.
#'
#' @param from Starting date.
#' @param to Ending date.
#' @param tz Time zone. By default, "" is the current time zone. For this dataset,
#'   "Australia/Melbourne" is the most appropriate, depending on OS.
#' @param session `NULL` or "shiny". For internal use only.
#'
#' @details The data is sourced from [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp).
#'   At its heart, this function scrapes the data through the
#'   "https://compedapi.herokuapp.com" api. A progress bar shows the approximate
#'   download status. Please refer to Melbourne Open Data Portal for more
#'   details about the dataset and its policy.
#' @return A data frame including these variables as follows:
#'   * Sensor: Sensor name (43 sensors)
#'   * Date_Time: Date time when the pedestrian counts are recorded
#'   * Date: Date associated with Date_Time
#'   * Time: Time of day
#'   * Count: Hourly counts
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Retrieve last week data
#'   ped_df1 <- walk_melb()
#'   head(ped_df1)
#'   
#'   # Retrieve data of a speficied period
#'   start_date <- as.Date("2017-07-01")
#'   end_date <- start_date + 6L
#'   ped_df2 <- walk_melb(from = start_date, to = end_date)
#'   head(ped_df2)
#' }
walk_melb <- function(
  from = to - 6L, to = Sys.Date() - 1L, tz = "", session = NULL
) {
  stopifnot(class(from) == "Date" && class(to) == "Date")
  stopifnot(from > as.Date("2009-05-31"))
  stopifnot(from <= to)
  yesterday <- Sys.Date() - 1L
  if (to > yesterday) {
    warning(
      paste0("The data is only avaiable up to ", yesterday, "."),
      call. = FALSE
    )
    to <- yesterday
  }

  date_range <- seq.Date(from = from, to = to, by = 1L)
  prefix_url <- "https://compedapi.herokuapp.com/api/bydatecsv/"

  fmt_date <- format(date_range, "%d-%m-%Y")
  urls <- paste0(prefix_url, fmt_date)
  len_urls <- length(urls)

  if (is.null(session)) {
    p <- dplyr::progress_estimated(len_urls)
    lst_dat <- lapply(urls, function(x) {
      dat <- read_file(file = x)
      p$tick()$print()
      dat
    })
  } else {
    # shiny session
    shiny::withProgress(
      message = "Retrieving data", value = 0, {
        lst_dat <- lapply(urls, function(x) {
          dat <- read_file(file = x)
          shiny::incProgress(1 / len_urls)
          dat
        })
      })
  }
  Sys.sleep(0.1)

  lst_dat[] <- Map(
    function(x, y) dplyr::mutate(x, Date = y),
    lst_dat, date_range
  )
  df_dat <- dplyr::bind_rows(lapply(lst_dat, function(x)
    tidyr::gather(x, Time, Count, -c(Sensor, Date))
  ))
  df_dat <- dplyr::mutate(df_dat, Time = interp_time(Time))
  df_dat <- dplyr::mutate(
    df_dat,
    Date_Time = as.POSIXct(paste(
      Date, paste0(formatC(Time, width = 2, flag = "0"), ":00:00")), tz = tz
    )
  )
  dplyr::select(df_dat, Sensor, Date_Time, Date, Time, Count)
}

### helper functions

interp_time <- function(x) {
  output <- integer(length = length(x))
  morning <- grepl("am", x)
  arvo <- grepl("pm", x)
  num <- as.integer(gsub("[^0-9]", "", x))
  output[morning] <- num[morning]
  output[arvo] <- num[arvo] + 12L
  output[x %in% "Noon"] <- 12L
  output
}

read_file <- function(file) {
  utils::read.csv(
    file, skip = 8, nrows = 43,
    colClasses = c("character", rep("integer", 24)),
    na.strings = "N/A", stringsAsFactors = FALSE, check.names = FALSE
  )
}

