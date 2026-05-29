#' validation of the time_subset argumetn
#'
#' @param time_subset a time_subset input
#' @importFrom data.table setDT as.data.table wday year month
#' @returns a cleaned time_subset
#'
#' @export
#'
#' @examples
time_subset_validate <- function(time_subset) {

  if (missing(time_subset)) {
    stop("A `time_subset` must be explicitly provided, e.g. list(month = 5:9).
    To indicate using all available time, put time_subset = 'use_all'")
  }

  ## TODO for CAROLINE: How to code the 'use_all' ? Something like this?
  if(time_subset == 'use_all') {
    time_subset = list(month = 1:12)
  }

  ##
  time_fns <- list(
    month = month,
    year  = year,
    wday   = wday
  )

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

  # ...


  ## add fcns
  time_subset$time_fns <- time_fns

  ## if everything passes, return the cleaned time_subset
  return(time_subset)

}
