#' collapse the data based on factors and group level strata
#'
#' @param data
#' @param collapse_fcn
#' @param data_type
#' @param grp_level
#' @param keep_unit
#' @param collapse_to
#' @param exposure_is_factor
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
                          keep_unit,
                          collapse_to) {

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

  # Next check about collapsing across factors
  if(is.null(collapse_to)) {

    cat("> No factors to collapse to, using all data\n")

    if(grp_level == FALSE) {

      # collapse to = NULL --> so this collapses across factors
      # grp_level = FALSE  --> and doesn't summarize to the group level
      cat("> grp_level == FALSE, so using geo_unit as strata\n")

      by_cols <- c(date_col, geo_unit_col, geo_unit_grp_col)

      data <- data[,.(
        xcol_agg = fcn(get(x_col))
      ), by = by_cols]

      names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col, x_col)

      column_mapping <- list(
        "date" = date_col,
        data_type = x_col,
        "geo_unit" = geo_unit_col,
        "geo_unit_grp" = geo_unit_grp_col
      )
      names(column_mapping)[2] = data_type

    } else {

      # collapse to = NULL --> so this collapses across factors
      # grp_level = TRUE  --> and does summarize to the group level

      if(keep_unit == FALSE) {

        cat("> grp_level == TRUE and keep_unit == FALSE, so
            aggregating to geo_unit_grp and using geo_unit_grp as strata\n")

        by_cols <- c(date_col, geo_unit_grp_col)

        data <- data[,.(
          xcol_agg = fcn(get(x_col))
        ), by = by_cols]

        names(data) <- c(date_col, geo_unit_grp_col, x_col)

        data$spatial_grp <- 'ALL'

        column_mapping <- list(
          "date" = date_col,
          data_type = x_col,
          "geo_unit" = geo_unit_grp_col,
          "geo_unit_grp" = 'spatial_grp'
        )
        names(column_mapping)[2] = data_type

      } else {

        cat("> grp_level == TRUE and keep_unit == TRUE, so
            keeping to geo_unit data but using geo_unit_grp as strata\n")

        by_cols <- c(date_col, geo_unit_col, geo_unit_grp_col)

        data <- data[,.(
          xcol_agg = fcn(get(x_col))
        ), by = by_cols]

        names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col, x_col)

        # and SKIP column re-mapping until later
        # except remove factor
        rr <- which(names(column_mapping) == 'factor')
        column_mapping[rr] <- NULL

      }

    }

  } else {

    cat("> Factors in data\n")
    factor_cols <- which(names(column_mapping) == 'factor')
    factor_cols <- unlist(column_mapping[factor_cols])
    if(!all(collapse_to %in% factor_cols)) {
      check_which <- which(!(collapse_to %in% factor_cols))
      stop(paste0("Check `collapse_to` argument, because '",
                 collapse_to[check_which], "' is not in factor cols: ",
                 paste0(factor_cols, collapse = ", ")))
    }
    stopifnot(collapse_to %in% factor_cols)

    if(grp_level == FALSE) {

      # collapse to = NOT NULL
      # grp_level = FALSE
      cat("> grp_level == FALSE, so using geo_unit as strata\n")

      by_cols <- c(date_col, geo_unit_col, geo_unit_grp_col, collapse_to)

      data <- data[,.(
        xcol_agg = fcn(get(x_col))
      ), by = by_cols]

      names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col,
                       collapse_to, x_col)

      # update the properties here
      column_mapping <- list(
        "date" = date_col,
        data_type = x_col,
        "geo_unit" = geo_unit_col,
        "geo_unit_grp" = geo_unit_grp_col,
        "factor" = collapse_to
      )
      names(column_mapping)[2] = data_type

    } else {

      # collapse to = NOT NULL
      # grp_level = TRUE

      if(keep_unit == FALSE) {

        cat("> grp_level == TRUE and keep_unit == FALSE, so
            aggregating to geo_unit_grp and using geo_unit_grp as strata\n")

        by_cols <- c(date_col, geo_unit_grp_col, collapse_to)

        data <- data[,.(
          xcol_agg = fcn(get(x_col))
        ), by = by_cols]

        names(data) <- c(date_col, geo_unit_grp_col,
                         collapse_to, x_col)
        #
        data$spatial_grp <- 'ALL'

        # update the properties here
        column_mapping <- list(
          "date" = date_col,
          data_type = x_col,
          "geo_unit" = geo_unit_grp_col,
          "geo_unit_grp" = 'spatial_grp',
          "factor" = collapse_to
        )
        names(column_mapping)[2] = data_type

      } else {

        cat("> grp_level == TRUE and keep_unit == TRUE, so
            keeping to geo_unit data but using geo_unit_grp as strata\n")

        by_cols <- c(date_col, geo_unit_col, geo_unit_grp_col, collapse_to)

        data <- data[,.(
          xcol_agg = fcn(get(x_col))
        ), by = by_cols]

        names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col,
                         collapse_to, x_col)

        # just over-write the collapse_to
        rr <- which(names(column_mapping) == 'factor')
        column_mapping[rr] <- NULL
        column_mapping[['factor']] <- collapse_to

      }

    }
  }

  # overwrite date
  data[, (column_mapping$date) := as.IDate(get(column_mapping$date))]

  #
  if(any(is.na(data))) {
    stop("some NA in data, check why")
  }

  return(list(data = data,
              column_mapping = column_mapping,
              collapse_to = collapse_to))

}
