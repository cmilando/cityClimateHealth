#' Set the strata column of xgrid
#'
#' @param xgrid the xgrid data.table
#' @param column_mapping column mapping vector
#' @param dt_by what is the date to aggregate by
#' @param grp_level logical, group level aggregatation
#' @param keep_unit logical, with or without unit level outcomes or exposures
#' @importFrom data.table setDT as.data.table wday year month
#' @returns a strata vector
#'
#' @examples
set_strata_value <- function(xgrid,
                             column_mapping,
                             dt_by) {
  #
  date_col <- column_mapping$date
  dow <- wday(xgrid[, get(date_col)])
  mn  <- month(xgrid[, get(date_col)])
  yr  <- year(xgrid[, get(date_col)])


  if(dt_by == 'day') {
    # ************
    # DT = DAY
    # ************
    cat("strata dt_by = 'day', ")
    cat("setting strata as geo_unit:yr:mn:dow\n")
    strata <- paste0(xgrid[, get(column_mapping$geo_unit)],
                     ":yr", yr,
                     ":mn", sprintf("%02i", mn),
                     ":dow", sprintf("%02i", dow))
    # if((grp_level & keep_unit) | !grp_level) {
    #
    # } else {
    #   cat("setting strata as geo_unit_grp:yr:mn:dow\n")
    #   strata <- paste0(xgrid[, get(column_mapping$geo_unit_grp)],
    #                          ":yr",yr,
    #                          ":mn",sprintf("%02i", mn),
    #                          ":dow", sprintf("%02i", dow))
    # }
  } else if(dt_by == 'week') {
    # ************
    # DT = WEEK
    # ************
    cat("strata dt_by = 'week', ")
    cat("setting strata as geo_unit:yr:mn:dow\n")
    strata <- paste0(xgrid[, get(column_mapping$geo_unit)],
                     ":yr", yr,
                     ":mn", sprintf("%02i", mn),
                     ":dow", sprintf("%02i", dow))
    # if((grp_level & keep_unit) | !grp_level) {
    #
    # } else {
    #   cat("setting strata as geo_unit_grp:yr:mn:dow\n")
    #   strata <- paste0(xgrid[, get(column_mapping$geo_unit_grp)],
    #                    ":yr",yr,
    #                    ":mn",sprintf("%02i", mn),
    #                    ":dow", sprintf("%02i", dow))
    # }

  } else if(dt_by == 'month') {
    # ************
    # DT = MONTH
    # ************
    cat("strata dt_by = 'month', ")
    cat("setting strata as geo_unit:yr:mn\n")
    strata <- paste0(xgrid[, get(column_mapping$geo_unit)],
                     ":yr", yr,
                     ":mn", sprintf("%02i", mn),
                     ":dow", sprintf("%02i", dow))
    # if((grp_level & keep_unit) | !grp_level) {
    #
    # } else {
    #   cat("setting strata as geo_unit_grp:yr:mn\n")
    #   strata <- paste0(xgrid[, get(column_mapping$geo_unit_grp)],
    #                    ":yr",yr,
    #                    ":mn",sprintf("%02i", mn),
    #                    ":dow", sprintf("%02i", dow))
    # }
  } else {
    stop("unknown dt_by value")
  }

  return(strata)

}

