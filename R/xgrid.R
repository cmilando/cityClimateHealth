#' An internal function to make the xgrid
#'
#' @param data a matrix of exposures or outcomes
#' @param column_mapping  named list that indicates relevant columns in `data`.
#' @param dt_by either by day or by week
#' @param collapse_is_spatial logical, is the collapse spatial
#' @param collapse_is_temporal logical, is the collapse temporal
#' @importFrom data.table setDT as.data.table wday year month as.IDate
#' @importFrom lubridate make_date
#' @importFrom tidyr expand_grid
#' @returns a datatable of all date and geo unit combinations
#'
#' @examples \dontrun{
#' exposure_columns <- list("date" = "date",
#' "exposure" = "tmax_C", "geo_unit" = "TOWN20",
#' "geo_unit_grp" = "COUNTY20")
#'
#' make_xgrid(subset(ma_exposure, TOWN20 == 'BOSTON'),
#' exposure_columns, time_subset = list(month = 5:9))
#'}

make_xgrid <- function(data,
                       column_mapping,
                       dt_by = 'day',
                       collapse_is_spatial = FALSE,
                       collapse_is_temporal = FALSE) {

  #
  setDT(data)

  stopifnot(dt_by %in% c('day', 'week', 'month'))

  # check they are all the same day of week
  date_col <- column_mapping$date
  date_vec <- data[, get(date_col)]
  dow_vec <- data.table::wday(date_vec)

  if(dt_by == 'week' & length(unique(dow_vec)) > 1) {
    stop('`dt_by` == "week" but there are more than 1 day-of-week in the
         dataset, so the strata will not work as desired.')
  }


  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # GET ALL DATES
  #
  # make the skeleton you need later
  # this is one of the first key stumbling blocks
  # correct! it is.
  # for "week" you need to also define what the start of the week is
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  years   <- sort(unique(data[, year(get(date_col))]))

  get_dt <- function(yy, dt_by) {
    st = lubridate::make_date(yy, 1, 1)
    ed = lubridate::make_date(yy, 12, 31)
    dt = seq.Date(st, ed, by = dt_by)
    return(as.IDate(dt))
  }

  # DAY
  if(dt_by == 'day') {
    all_dt <- lapply(years, get_dt, dt_by = dt_by)
    all_dt <- do.call(c, all_dt)
  }

  # WEEK
  if(dt_by == 'week') {
    st = as.IDate(min(data[, get(date_col)]))
    ed = as.IDate(max(data[, get(date_col)]))
    dt = seq.Date(st, ed, by = 'week')
    all_dt = as.IDate(dt)
  }

  # MONTH
  if(dt_by == 'month') {
    all_dt <- lapply(years, get_dt, dt_by = dt_by)
    all_dt <- do.call(c, all_dt)
  }


  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # CREATE XGRID SKELETON
  #
  # For dates, you need all the dates
  #
  # For outcomes, you also need all the dates within strata
  # that have SOME data so you can add 0s to low data places
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  geo_col <- column_mapping$geo_unit
  unique_areas <- unlist(unique(data[, get(geo_col)]))

  if(any(names(column_mapping) == 'factor')) {

    stopifnot(length(which(names(column_mapping) == 'factor')) == 1)

    factor_vector = unlist(column_mapping$factor)

    input_list = list(date = all_dt, geo_unit = unique_areas)

    for(ff_i in 1:length(factor_vector)) {
      factor_col = factor_vector[ff_i]
      unique_fcts <- unlist(unique(data[, get(factor_col)]))
      input_list[[factor_col]] = unique_fcts
    }

    xgrid <- do.call(tidyr::expand_grid, input_list)

    # if collapse is spatial, reduce this
    # so what this means is that
    # not every _geo_unit_ exists in every fct
    # so you need to subset xgrid
    if(collapse_is_spatial) {

      xcols = c(geo_col, factor_vector)
      uq <- unique(data[, ..xcols])
      names(uq) = c('geo_unit', factor_vector)

      xg <- as.data.table(xgrid)

      join_cols <- c('geo_unit', factor_vector)
      xgrid <- xg[uq, ,on = join_cols]

    }

    # if collapse is temporal, reduce this
    # so what this means is that not every
    # not every _date_ exists in every fct
    # so you
    if(collapse_is_temporal) {

      xcols = c(date_col, factor_vector)
      uq <- unique(data[, ..xcols])
      names(uq) = c('date', factor_vector)

      xg <- as.data.table(xgrid)

      join_cols <- c('date', factor_vector)
      xgrid <- xg[uq, ,on = join_cols]

    }

    names(xgrid) = c(column_mapping$date,
                     column_mapping$geo_unit,
                     factor_vector)

  } else {

    xgrid <- tidyr::expand_grid(date = all_dt,
                                geo_unit = unique_areas)

    names(xgrid) = c(column_mapping$date, column_mapping$geo_unit)

  }

  setDT(xgrid)

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # JOIN WITH GROUP DATA
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

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

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # FINALLY - JOIN WITH DATA
  #
  # This shouldnt add any rows
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  if(any(names(column_mapping) == 'factor')) {
    spatial_join_col <- c(
      column_mapping$date,
      column_mapping$geo_unit,
      column_mapping$geo_unit_grp,
      factor_vector
    )
  } else {
    spatial_join_col <- c(
      column_mapping$date,
      column_mapping$geo_unit,
      column_mapping$geo_unit_grp
    )
  }

  if(nrow(xgrid) < nrow(data)) {
    stop("rows of xgrid < rows of data, this means an averaging step
         has been skipped. report to GitHub")
  }

  xgrid <- data[
    xgrid,
    on = setNames(spatial_join_col, spatial_join_col)
  ]

  stopifnot(nrow(xgrid) > 0)


  return(xgrid)

}
