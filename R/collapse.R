#' collapse the data based on factors and group level strata
#'
#' @param data
#' @param column_mapping
#' @param collapse_fcn
#' @param data_type
#' @param grp_level
#' @param keep_unit
#'
#' @returns
#' @export
#'
#' @examples
collapse_data <- function(data,
                          column_mapping,
                          fcn,
                          data_type,
                          grp_level,
                          keep_unit) {

  # **************
  ## both by collapse to and by group
  date_col         = column_mapping$date
  geo_unit_col     = column_mapping$geo_unit
  geo_unit_grp_col = column_mapping$geo_unit_grp

  if(data_type == 'exposure') {
    x_col      = column_mapping$exposure
  } else {
    x_col      = column_mapping$outcome
  }

  # check for factors -- these are things you stratify on later
  factor_col <- NULL
  if("factor" %in% names(column_mapping)) {
    d2 <- add_factor_col(data, column_mapping)
    data = d2$data
    column_mapping <- d2$column_mapping
    factor_col <- unlist(column_mapping[["factor"]])
  }

  # check for covariates -- these are variables included in each model
  covariate_cols <- NULL
  if("covariate" %in% names(column_mapping)) {
    covariate_cols <- unlist(column_mapping[['covariate']])
  }

  # *************
  # Next check about collapsing across geo_unit_groups

  if(grp_level == TRUE & keep_unit == FALSE) {

    cat("> grp_level == TRUE and keep_unit == FALSE, so
        aggregating to geo_unit_grp and using geo_unit_grp as strata\n")

    by_cols <- c(date_col, geo_unit_grp_col,
                 factor_col, covariate_cols)

    data <- data[,.(
      xcol_agg = fcn(get(x_col))
    ), by = by_cols]

    names(data) <- c(by_cols, x_col)

    data$spatial_grp <- 'ALL'

    column_mapping <- list(
      "date" = date_col,
      data_type = x_col,
      "geo_unit" = geo_unit_grp_col,
      "geo_unit_grp" = 'spatial_grp'
    )
    names(column_mapping)[2] = data_type

    #
    if(!is.null(factor_col)) {
      column_mapping[['factor']] = factor_col
    }
    if(!is.null(covariate_cols)) {
      column_mapping[['covariate']] = covariate_cols
    }

  } else {

    # all other circumstances follow this pattern
    by_cols <- c(date_col, geo_unit_col, geo_unit_grp_col,
                 factor_col, covariate_cols)

    data <- data[,.(
      xcol_agg = fcn(get(x_col))
    ), by = by_cols]

    names(data) <- c(by_cols, x_col)

  }

  # overwrite date
  data[, (column_mapping$date) := as.IDate(get(column_mapping$date))]

  # NA check
  if(any(is.na(data))) {
    stop("some NA in data, check why")
  }

  return(list(data = data,
              column_mapping = column_mapping))

}
