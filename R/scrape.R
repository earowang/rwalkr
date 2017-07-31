globalVariables(c("Time", "Count", "Sensor", "Date"))
#' API to the pedestrian data sourced from the City of Melbourne
#'
#' @param from Starting date.
#' @param to Ending date.
#' @param tz Time zone.
#'
#' @return A data frame contains "Sensor", "Date_Time", "Count".
#' @export
#'
#' @examples
#' \dontrun{
#'   ped_df <- get_pedestrain()
#' }
#' @importFrom utils read.csv
get_pedestrian <- function(from = to - 6L, to = Sys.Date() - 1L, tz = "") {
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
    dat <- read.csv(
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
    Date_Time = as.POSIXct(paste(Date, paste0(Time, ":00:00")), tz = tz)
  )
  df_dat[, c("Sensor", "Date_Time", "Count")]
}

interp_time <- function(x) {
  output <- integer(length = length(x))
  morning <- grepl("am", x)
  arvo <- grepl("pm", x)
  num <- as.integer(gsub("[^0-9]", "", x))
  output[morning] <- num[morning]
  output[arvo] <- num[arvo] + 12L
  output[x %in% "Noon"] <- 12L
  formatC(output, width = 2, flag = "0")
}
