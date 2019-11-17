#' Defunct functions
#' @rdname rwalkr-defunct
#' @keywords internal
#' @usage NULL
#' @export
walk_melb <- function(
  from = to - 6L, to = Sys.Date() - 1L, tz = "", na.rm = FALSE, session = NULL
) {
  .Defunct("melb_walk")
}


#' @rdname rwalkr-defunct
#' @keywords internal
#' @usage NULL
#' @export
run_melb <- function(year = NULL, sensor = NULL, tz = "", na.rm = FALSE,
  app_token = NULL) {
  .Defunct("melb_walk_fast")
}

#' @rdname rwalkr-defunct
#' @keywords internal
#' @usage NULL
#' @export
shine_melb <- function() {
  .Defunct("melb_shine")
}
