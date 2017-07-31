globalVariables(c("Time", "Count", "Sensor", "Date", "Date_Time"))
#' API to Melbourne pedestrian data using R
#'
#' @param from Starting date.
#' @param to Ending date.
#' @param tz Time zone.
#'
#' @details The data is sourced from [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp).
#'   At its heart, this function scrapes the data through the
#'   "https://compedapi.herokuapp.com" api. A progress bar shows the download
#'   status. Please refer to Melbourne Open Data Portal for more details about
#'   the dataset and its policy.
#' @return A data frame including "Sensor", "Date_Time", "Date", "Time",
#'   "Count" variables.
#' @export
#'
#' @examples
#' \dontrun{
#'   ped_df <- walk_melb()
#'   head(ped_df)
#' }
walk_melb <- function(from = to - 6L, to = Sys.Date() - 1L, tz = "") {
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
  p <- dplyr::progress_estimated(length(urls))
  lst_dat <- lapply(urls, function(x) {
    dat <- utils::read.csv(
      x, skip = 8, nrows = 43, colClasses = c("character", rep("integer", 24)),
      na.strings = "N/A", stringsAsFactors = FALSE, check.names = FALSE
    )
    p$tick()$print()
    dat
  })
  lst_dat_x <- suppressWarnings(mapply(
    function(x, y) dplyr::mutate(x, Date = y),
    lst_dat, date_range,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
  df_dat <- dplyr::bind_rows(lapply(lst_dat_x, function(x)
    tidyr::gather(x, Time, Count, -c(Sensor, Date))
  ))
  df_dat <- dplyr::mutate(df_dat, Time = interp_time(Time))
  df_dat <- dplyr::mutate(
    df_dat,
    Date_Time = as.POSIXct(paste(
      Date, paste0(formatC(Time, width = 2, flag = "0"), ":00:00")), tz = tz
    )
  )
  df_dat[, c("Sensor", "Date_Time", "Date", "Time", "Count")]
}

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
