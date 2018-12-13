#' Deprecated functions
#' @rdname deprecated
#' @keywords internal
#' @usage NULL
#' @export
walk_melb <- function(
  from = to - 6L, to = Sys.Date() - 1L, tz = "", na.rm = FALSE, session = NULL
) {
  .Deprecated("melb_walk")
  melb_walk(from = from, to = to, tz = tz, na.rm = na.rm, session = session)
}


#' @rdname deprecated
#' @keywords internal
#' @usage NULL
#' @export
run_melb <- function(year = NULL, sensor = NULL, tz = "", na.rm = FALSE,
  app_token = NULL) {
  .Deprecated("melb_walk_fast")
  melb_walk_fast(year = year, sensor = sensor, tz = tz, na.rm = na.rm,
    app_token = app_token)
}

#' @rdname deprecated
#' @keywords internal
#' @usage NULL
#' @export
shine_melb <- function() {
  .Deprecated("melb_shine")
  melb_shine()
}
