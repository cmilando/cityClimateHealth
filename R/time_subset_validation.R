#' Validate time_subset input
#'
#' @param time_subset
#' @export

time_subset_validation <- function(time_subset) {
  time_fns <- list(
    month = month,
    year  = year,
    wday  = wday
  )

  if (missing(time_subset)) {
    stop("`time_subset` must be explicitly provided, e.g. list(month = 5:9), ",
         "or NULL to use all time periods.")
  }

  if (!is.null(time_subset)) {
    if (!is.list(time_subset))
      stop("`time_subset` must be a named list, e.g. list(month = 5:9, year = 2010:2015)")
    if (!all(names(time_subset) %in% names(time_fns)))
      stop("`time_subset` names must be one of: month, year, wday")
    if ("month" %in% names(time_subset) && !all(time_subset$month %in% 1:12))
      stop("`time_subset$month` must be values in 1:12")
    if ("wday" %in% names(time_subset) && !all(time_subset$wday %in% 1:7))
      stop("`time_subset$wday` must be values in 1:7")
  }

  invisible(NULL)
}
