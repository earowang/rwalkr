globalVariables(c("Time", "Count", "Sensor", "Date", "Date_Time", "walk"))

#' API using compedapi to Melbourne pedestrian data
#'
#' Provides API using compedapi to Melbourne pedestrian data in a tidy data form.
#'
#' @param from Starting date.
#' @param to Ending date.
#' @param tz Time zone. By default, "" is the current time zone. For this dataset,
#'   the local time zone is "Australia/Melbourne" which would be the most
#'   appropriate, depending on OS.
#' @param na.rm Logical. `FALSE` is the default suggesting to include `NA` in 
#'   the dataset. `TRUE` removes the `NA`s.
#' @param session `NULL` or "shiny". For internal use only.
#'
#' @details It provides API using compedapi, where counts are uploaded on a 
#'   daily basis. The up-to-date data would be
#'   till the previous day. The data is sourced from [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp). Please 
#'   refer to Melbourne Open Data Portal for more details about the dataset and 
#'   its policy.
#' @return A tibble including these variables as follows:
#'   * Sensor: Sensor name (43 sensors up to date)
#'   * Date_Time: Date time when the pedestrian counts are recorded
#'   * Date: Date associated with Date_Time
#'   * Time: Time of day
#'   * Count: Hourly counts
#'
#' @export
#' @seealso [run_melb]
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
  from = to - 6L, to = Sys.Date() - 1L, tz = "", na.rm = FALSE, session = NULL
) {
  stopifnot(class(from) == "Date" && class(to) == "Date")
  stopifnot(from > as.Date("2009-05-31"))
  stopifnot(from <= to)
  yesterday <- Sys.Date() - 1L
  if (to > yesterday) {
    warning(
      sprintf("The data is only avaiable up to %s.", yesterday),
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
      dat <- tibble::as_tibble(read_url(url = x))
      p$tick()$print()
      dat
    })
  } else {
    # shiny session
    stopifnot(shiny::isRunning())
    shiny::withProgress(
      message = "Retrieving data", value = 0, {
        lst_dat <- lapply(urls, function(x) {
          dat <- read_url(url = x)
          shiny::incProgress(1 / len_urls)
          dat
        })
      })
  }

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
  if (na.rm) df_dat <- dplyr::filter(df_dat, !is.na(Count))

  # if (tweak) {
  #   dif <- dplyr::filter(sensor_dict, match == FALSE, walk != "NA")
  #   seq_sensor <- seq_len(nrow(dif))
  #   changed_df <- dplyr::bind_rows(lapply(seq_sensor, function(x) {
  #     df_tmp <- df_dat[df_dat$Sensor == dif[x, "walk"], , drop = FALSE]
  #     dif_run <- dif[x, "run"]
  #     if (!is.na(dif_run)) {
  #       df_tmp$Sensor <- dif_run
  #     }
  #     df_tmp
  #   }))
  #   same <- dplyr::filter(sensor_dict, match == TRUE)$walk
  #   unchanged_df <- dplyr::filter(df_dat, Sensor %in% same)
  #   df_dat <- dplyr::bind_rows(unchanged_df, changed_df)
  # }

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

read_url <- function(url) {
  utils::read.csv(
    url, skip = 8, nrows = 43,
    colClasses = c("character", rep("integer", 24)),
    na.strings = "N/A", stringsAsFactors = FALSE, check.names = FALSE
  )
}

#' Look up sensor names between `run_melb()` and `walk_melb()`
#'
#' One-to-one corresponding sensor names between `run_melb()` and `walk_melb()`
#'
#' @details Two APIs (Socrata and compedapi) code some sensors using different 
#'   names. This functions returns a data frame that allows to compare sensor 
#'   names obtained from these two APIs.
#' @return A data frame including three columns:
#'   * run: Sensor names obtained from the `run_melb()` function using Socrata
#'   * walk: Sensor names obtained from the `walk_melb()` function using compedapi
#'   * match: whether sensor names are identical or not
#'
#' @export
#'
#' @examples
#'   lookup_sensor()
lookup_sensor <- function() {
  sensor_dict
}
