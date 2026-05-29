#' An internal function to make the xgrid
#'
#' @param data
#' @param column_mapping
#' @param time_subset
#' @param dt_by either by day or by week
#' @param collapse_is_spatial
#' @param collapse_is_temporal
#' @importFrom data.table setDT as.data.table wday
#' @importFrom lubridate make_date
#' @importFrom tidyr expand_grid
#' @returns
#'
#' @examples
#' \dontrun{
#'   column_mapping <- list(
#'     date = "date",
#'     geo_unit = "city",
#'     geo_unit_grp = "state"
#'   )
#'   xgrid <- make_xgrid(data, column_mapping, months_subset = 5:9)
#' }

make_xgrid <- function(data, column_mapping, time_subset = NULL,
                       dt_by = 'day', collapse_is_spatial = FALSE,
                       collapse_is_temporal = FALSE) {
  #
  setDT(data)

  stopifnot(dt_by %in% c('day', 'week', 'month'))

  ##validation for time_subset

  if (missing(time_subset)) {
    stop("`time_subset` must be explicitly provided, e.g. list(month = 5:9), or NULL to use all time periods.")
  }

  time_subset_validation(time_subset)

  # check they are all the same day of week
  date_col <- column_mapping$date
  date_vec <- data[, get(date_col)]
  dow_vec <- data.table::wday(date_vec)

  if(dt_by == 'week' & length(unique(dow_vec)) > 1) {
    stop('`dt_by` == "week" but there are more than 1 day-of-week in the
         dataset, so the strata will not work as desired.')
  }


  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' GET ALL DATES
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////


  years   <- sort(unique(data[, year(get(date_col))]))

  # make the skeleton you need later
  # this is one of the first key stumbling blocks
  # correct! it is.
  # for "week" you need to also define what the start of the week is
  if(dt_by == 'day') {
    get_dt <- function(yy) {
      st = make_date(yy, 1, 1)
      ed = make_date(yy, 12, 31)
      as.IDate(seq.Date(st, ed, by = 'day'))
    }
    all_dt <- do.call(c, lapply(years, get_dt))
  }

  if(dt_by == 'week') {
    st = as.IDate(min(data[, get(date_col)]))
    ed = as.IDate(max(data[, get(date_col)]))
    dt = seq.Date(st, ed, by = 'week')
    all_dt = as.IDate(dt[year(dt) %in% years])
  }

  if(dt_by == 'month') {
    get_dt <- function(yy) {
      st = make_date(yy, 1, 1)
      ed = make_date(yy, 12, 1)
      as.IDate(seq.Date(st, ed, by = 'month'))
    }
    all_dt <- do.call(c, lapply(years, get_dt))
  }

  # CHANGED: apply time_subset filter to all_dt using the shared time_fns map
  if (!is.null(time_subset)) {
    time_fns <- list(month = month, year = year, wday = wday)
    keep <- rep(TRUE, length(all_dt))
    for (unit in names(time_subset)) {
      keep <- keep & time_fns[[unit]](all_dt) %in% time_subset[[unit]]
    }
    all_dt <- all_dt[keep]
  }


  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' CREATE XGRID SKELETON
  #'
  #' For dates, you need all the dates
  #'
  #' For outcomes, you also need all the dates within strata
  #' that have SOME data so you can add 0s to low data places
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

  geo_col <- column_mapping$geo_unit
  unique_areas <- unlist(unique(data[, get(geo_col)]))

  if(any(names(column_mapping) == 'factor')) {

    stopifnot(length(which(names(column_mapping) == 'factor')) == 1)

    factor_col <- column_mapping$factor

    unique_fcts <- unlist(unique(data[, get(factor_col)]))

    xgrid <- tidyr::expand_grid(date = all_dt,
                                geo_unit = unique_areas,
                                fct = unique_fcts)

    # if collapse is spatial, reduce this
    # so what this means is that
    # not every _geo_unit_ exists in every fct
    # so you need to subset xgrid
    if(collapse_is_spatial) {

      xcols = c(geo_col, factor_col)
      uq <- unique(data[, ..xcols])
      names(uq) = c('geo_unit', 'fct')

      xg <- as.data.table(xgrid)

      join_cols <- c('geo_unit', 'fct')
      xgrid <- xg[uq, ,on = join_cols]

    }

    # if collapse is temporal, reduce this
    # so what this means is that not every
    # not every _date_ exists in every fct
    # so you
    if(collapse_is_temporal) {

      xcols = c(date_col, factor_col)
      uq <- unique(data[, ..xcols])
      names(uq) = c('date', 'fct')

      xg <- as.data.table(xgrid)

      join_cols <- c('date', 'fct')
      xgrid <- xg[uq, ,on = join_cols]

    }

    names(xgrid) = c(column_mapping$date,
                     column_mapping$geo_unit,
                     column_mapping$factor)

  } else {

    xgrid <- tidyr::expand_grid(date = all_dt,
                                geo_unit = unique_areas)

    names(xgrid) = c(column_mapping$date, column_mapping$geo_unit)

  }

  setDT(xgrid)

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' JOIN WITH GROUP DATA
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

  geo_cols <- c(
    column_mapping$geo_unit,
    column_mapping$geo_unit_grp
  )

  geo_unit_mapping <- unique(data[, ..geo_cols])

  # if any geo_unit appears more than once then its not 1:1
  if(length(unique_areas) != nrow(geo_unit_mapping)) {
    stop("need to enforce that there is a 1:1 mapping of geo_unit to geo_unit_grp.
         That is, a single geo_unit cannot belong to more than one geo_unit_grp")
  }

  setDT(geo_unit_mapping)

  join_col <- column_mapping$geo_unit

  xgrid <- geo_unit_mapping[
    xgrid,
    on = setNames(join_col, join_col)
  ]

  stopifnot(nrow(xgrid) > 0)

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' FINALLY - JOIN WITH DATA
  #'
  #' This shouldn't add any rows
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

  if(any(names(column_mapping) == 'factor')) {
    spatial_join_col <- c(
      column_mapping$date,
      column_mapping$geo_unit,
      column_mapping$geo_unit_grp,
      column_mapping$factor
    )
  } else {
    spatial_join_col <- c(
      column_mapping$date,
      column_mapping$geo_unit,
      column_mapping$geo_unit_grp
    )
  }

  xgrid <- data[
    xgrid,
    on = setNames(spatial_join_col, spatial_join_col)
  ]

  stopifnot(nrow(xgrid) > 0)

  return(xgrid)

}
