#' Function to add US holidays
#'
#' @param exposure_matrix
#'
#' @returns an outcome table object with an additional covariate
#' @export
#'
#' @importFrom data.table year
#' @importFrom almanac cal_us_federal alma_in
#'
#' @examples
#' exposure_columns <- list(
#'   "date" = "date",
#'   "exposure" = "tmax_C",
#'   "geo_unit" = "TOWN20",
#'   "geo_unit_grp" = "COUNTY20"
#' )
#'
#' exp_data = subset(ma_exposure, COUNTY20 %in% c('MIDDLESEX', 'WORCESTER'))
#'
#' exposure_matrix <- make_exposure_matrix(
#'   data = exp_data,
#'   column_mapping = exposure_columns,
#'   time_subset = list(year = 2012:2015)
#' )
#'
#' exposure_matrix <- add_us_holiday(exposure_matrix)
add_US_holiday <- function(exposure_matrix) {

  stopifnot("exposure" %in% class(exposure_matrix))

  # column mapping
  column_mapping = attributes(exposure_matrix)$column_mapping

  # convert all dates
  dt_all <- as.Date(exposure_matrix[, get(column_mapping$date)])
  dt_range = range(dt_all)

  # get almanac object
  xhol <- almanac::cal_us_federal(since = as.Date(dt_range[1]),
                          until = as.Date(dt_range[2]))

  # check if in
  exposure_matrix$is_holiday <- almanac::alma_in(dt_all, xhol)

  # update column_mapping
  # well does the covariate exist yet or no
  if("covariate" %in% names(column_mapping)) {
    if("is_holiday" %in% column_mapping$covariate)
      stop("`is_holiday` already exists as a covariate")
    column_mapping$covariate <- c(column_mapping$covariate, 'is_holiday')
  }else {
    column_mapping[["covariate"]] <- 'is_holiday'
  }
  attributes(exposure_matrix)$column_mapping <- column_mapping

  return(exposure_matrix)
}


