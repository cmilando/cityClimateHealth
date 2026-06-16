#' Function to create the outcome table
#' TO DO : EDIT XGRID
#' @param data a dataset of outcomes
#' @param column_mapping a named list that indicates relevant columns in `data`. for the exposure
#' data table, these need to be one of: c('date', "outcome",'factor, 'geo_unit', 'geo_unit_grp')
#' @param time_subset the time period of interest for analysis, specified as years or months
#' must be specified by user - no default
#' @param collapse_to which factors to collapse across
#' @param collapse_is_spatial is collapse a spatial variable
#' @param collapse_is_temporal is collapse a temporal variable
#' @param grp_level whether to summarize to the group level or not (default)
#' @param keep_unit_outcomes if grp_level is true, whether to keep original unit-level outcomes
#' @param dt_by is it daily data, or weekly or ...
#'
#' @importFrom data.table setDT setorderv `:=`
#' @importFrom lubridate days_in_month
#'
#' @returns a data.table of class("outcome")
#' @export
#'
#' @examples
#' outcome_columns <- list(
#'   "date" = "date",
#'   "outcome" = "daily_deaths",
#'   "factor" = 'age_grp',
#'   "factor" = 'sex',
#'   "geo_unit" = "TOWN20",
#'   "geo_unit_grp" = "COUNTY20"
#' )
#' ma_outcomes_tbl <- make_outcome_table(
#'  subset(ma_deaths,COUNTY20 %in% c('MIDDLESEX', 'WORCESTER') &
#'            outcome_columns,
#'            time_subset = list(year = 2012:2015))
#' make_outcome_table
make_outcome_table <- function(data,
                               column_mapping,
                               time_subset,
                               dt_by = 'day',
                               collapse_to = NULL,
                               collapse_is_spatial = FALSE,
                               collapse_is_temporal = FALSE,
                               grp_level = FALSE,
                               keep_unit_outcomes = FALSE) {

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # VALIDATIONS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  ##
  stopifnot(nrow(data) > 0)
  setDT(data)

  #
  stopifnot(typeof(column_mapping) == 'list')

  ##validation for time_subset

  if (missing(time_subset)) {
    stop("`time_subset` must be explicitly provided, e.g. list(month = 5:9), or NULL to use all time periods.")
  }

  time_subset_validation(time_subset)

  # column types
  col_types <- c('date', 'factor', 'outcome', 'geo_unit', 'geo_unit_grp')

  # check that all the types are valid
  if(!all(names(column_mapping) %in% col_types))
    stop('Names of column mapping is not one of the valid types:
          date, exposure, geo_unit, geo_unit_grp')

  # check that all values are correct
  if(!all(column_mapping %in% names(data)))
    stop('Values of column mapping not matched with column names of data.
          Look for a typo')

  #check for duplicate column mapping (there should not be column repeats in the inputs)
  if(length(unlist(column_mapping)) != length(unique(unlist(column_mapping)))) {
    stop("Duplicate columns found in `column_mapping` — each column must be mapped to a unique value")
  }

  # check that geo_unit and geo_unit_grp are not the same column
  if(column_mapping$geo_unit == column_mapping$geo_unit_grp) {
    stop("`geo_unit` and `geo_unit_grp` cannot be mapped to the same column: '",
         column_mapping$geo_unit, "'")
  }

  ## Time_subset validation
  ## this isn't applied until the very end
  time_subset <- time_subset_validate(time_subset)

  # type checks
  stopifnot(
    inherits(data[[column_mapping$date]], "Date"),
    is.integer(data[[column_mapping$outcome]]),
    is.character(data[[column_mapping$geo_unit]]),
    is.character(data[[column_mapping$geo_unit_grp]])
  )
  if("factor" %in% names(column_mapping)) {
    stopifnot(is.character(data[[column_mapping$factor]]))
  }

  # overwrite date
  data[, (column_mapping$date) := as.IDate(get(column_mapping$date))]

  # at the beginning there shouldn't be any outcomes < 0
  outcome_col <- column_mapping$outcome
  if(any(data[, get(outcome_col)] < 0)) {
    stop("some outcomes < 0, investigate")
  }

  ##remove NAs automatically

  if(any(is.na(data))) {
    warning("NA values automatically removed")
    data <- na.omit(data)
  }

  # check that all are unique 1:1
  geo_cols <- c(
    column_mapping$geo_unit,
    column_mapping$geo_unit_grp
  )
  unique_geos <- unique(data[, get(column_mapping$geo_unit)])
  unique_geos_and_grps <- unique(data[, ..geo_cols])
  if(length(unique_geos) != nrow(unique_geos_and_grps)) {
    stop("`geo_unit` repeated across multiple `grps`")
  }


  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # COLLAPSE AND SUMMARIZE
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # **************
  ## first collapse to by summing
  ## both by collapse to and by group
  date_col         = column_mapping$date
  geo_unit_col     = column_mapping$geo_unit
  geo_unit_grp_col = column_mapping$geo_unit_grp
  outcome_col      = column_mapping$outcome

  # Next check about collapsing across factors
  if(is.null(collapse_to)) {

    cat("> No factors to collapse to, using all data\n")

    collapse_to = 'ALL'

    if(grp_level == FALSE) {

      # collapse to = NULL --> so this collapses across factors
      # grp_level = FALSE  --> and doesn't summarize to the group level
      cat("> grp_level == FALSE, so using geo_unit as strata\n")

      data <- data[,.(
        xoutcome = sum(get(outcome_col))
      ), by = .(get(date_col),
                get(geo_unit_col),
                get(geo_unit_grp_col))]

      names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col, outcome_col)

      column_mapping <- list(
        "date" = date_col,
        "outcome" = outcome_col,
        "geo_unit" = geo_unit_col,
        "geo_unit_grp" = geo_unit_grp_col
      )

    } else {

      # collapse to = NULL --> so this collapses across factors
      # grp_level = TRUE  --> and does summarize to the group level

      if(keep_unit_outcomes == FALSE) {

        cat("> grp_level == TRUE and keep_unit_outcomes == FALSE, so
            aggregating to geo_unit_grp and using geo_unit_grp as strata\n")

        data <- data[,.(
          xoutcome = sum(get(outcome_col))
        ), by = .(get(date_col),
                  get(geo_unit_grp_col))]

        names(data) <- c(date_col, geo_unit_grp_col, outcome_col)

        data$spatial_grp <- 'ALL'

        column_mapping <- list(
          "date" = date_col,
          "outcome" = outcome_col,
          "geo_unit" = geo_unit_grp_col,
          "geo_unit_grp" = 'spatial_grp'
        )

      } else {

        cat("> grp_level == TRUE and keep_unit_outcomes == TRUE, so
            keeping to geo_unit data but using geo_unit_grp as strata\n")

        data <- data[,.(
          xoutcome = sum(get(outcome_col))
        ), by = .(get(date_col),
                  get(geo_unit_col),
                  get(geo_unit_grp_col))]

        names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col, outcome_col)

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
    stopifnot(collapse_to %in% factor_cols)

    if(grp_level == FALSE) {

      # collapse to = NOT NULL
      # grp_level = FALSE
      cat("> grp_level == FALSE, so using geo_unit as strata\n")

      data <- data[,.(
        xoutcome = sum(get(outcome_col))
      ), by = .(get(date_col),
                get(geo_unit_col),
                get(geo_unit_grp_col),
                get(collapse_to))]

      names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col,
                       collapse_to, outcome_col)

      # update the properties here
      column_mapping <- list(
        "date" = date_col,
        "outcome" = outcome_col,
        "geo_unit" = geo_unit_col,
        "geo_unit_grp" = geo_unit_grp_col,
        "factor" = collapse_to
      )
    } else {

      # collapse to = NOT NULL
      # grp_level = TRUE

      if(keep_unit_outcomes == FALSE) {

        cat("> grp_level == TRUE and keep_unit_outcomes == FALSE, so
            aggregating to geo_unit_grp and using geo_unit_grp as strata\n")

        data <- data[,.(
          xoutcome = sum(get(outcome_col))
        ), by = .(get(date_col),
                  get(geo_unit_grp_col),
                  get(collapse_to))]

        names(data) <- c(date_col, geo_unit_grp_col,
                         collapse_to, outcome_col)

        #
        data$spatial_grp <- 'ALL'

        # update the properties here
        column_mapping <- list(
          "date" = date_col,
          "outcome" = outcome_col,
          "geo_unit" = geo_unit_grp_col,
          "geo_unit_grp" = 'spatial_grp',
          "factor" = collapse_to
        )
      } else {

        cat("> grp_level == TRUE and keep_unit_outcomes == TRUE, so
            keeping to geo_unit data but using geo_unit_grp as strata\n")

        data <- data[,.(
          xoutcome = sum(get(outcome_col))
        ), by = .(get(date_col),
                  get(geo_unit_col),
                  get(geo_unit_grp_col),
                  get(collapse_to))]

        names(data) <- c(date_col, geo_unit_col, geo_unit_grp_col,
                         collapse_to, outcome_col)

        # just over-write the collapse_to
        rr <- which(names(column_mapping) == 'factor')
        column_mapping[rr] <- NULL
        column_mapping[['factor']] <- collapse_to

      }

    }
  }

  # overwrite date
  data[, (column_mapping$date) := as.IDate(get(column_mapping$date))]

  geo_unit_col = column_mapping$geo_unit
  geo_unit_grp_col = column_mapping$geo_unit_grp

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # MAKE XGRID and SET STRATA
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  ## fill in the blanks with 0s
  ## so make xgrid again
  xgrid <- make_xgrid(data = data,
                      column_mapping = column_mapping,
                      dt_by = dt_by,
                      collapse_is_spatial = collapse_is_spatial,
                      collapse_is_temporal = collapse_is_temporal)

  # **************
  ## set missing outcome values to 0

  rr <- which(is.na(xgrid[[outcome_col]]))

  if(length(rr) > 0) {

    message("Missing outcome values introduced by xgrid were set to 0;
            assumes that every time in the dataset should have an outcome value")

    # default
    xgrid[rr, (outcome_col) := 0]

    # --> could also switch to just removing them altogether
    # xgrid <- xgrid[-rr, ]
  }

  if(any(is.na(xgrid))) {
    message('some remaining NA in outcome xgrid, investigate')
    return(xgrid)
  }

  # ******************
  # set strata
  # should this be set to false ??? here ??? why ???
  xgrid$strata = set_strata_value(xgrid,
                                  column_mapping = column_mapping,
                                  dt_by = dt_by,
                                  grp_level = FALSE,
                                  keep_unit = keep_unit_outcomes)

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # STRATA GROUP TOTALS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # Label strata that have no cases, these will be removed later
  # Extract outcome column name programmatically
  group_col   <- "strata"

  # 1. Aggregate by group and create the 'keep' flag
  xgrid_agg <- xgrid[, .(
    strata_total = sum(get(outcome_col))
  ), by = group_col]

  # 2. Join back to the original data (left join)
  xgrid_comb <- xgrid[xgrid_agg, on = group_col]

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # CREATE MATCH STRATA
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # now that you have a final version, make match_strata
  # also make match strata
  xgrid_comb$match_strata = paste0(
    xgrid_comb[, get(column_mapping$geo_unit)], ":",
    xgrid_comb[, get(date_col)])

  # **************
  # Lastly, re-set column mapping if group-level = TRUE but keep_orig = TRUE
  if(grp_level & keep_unit_outcomes) {
    xgrid_comb$spatial_grp <- 'ALL'
    if(collapse_to == 'ALL') {
      column_mapping <- list(
        "date" = date_col,
        "outcome" = outcome_col,
        "geo_unit" = geo_unit_grp_col,
        "geo_unit_grp" = 'spatial_grp'
      )
    } else {
      column_mapping <- list(
        "date" = date_col,
        "outcome" = outcome_col,
        "geo_unit" = geo_unit_grp_col,
        "geo_unit_grp" = 'spatial_grp',
        "factor" = collapse_to
      )
    }
  }

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # OUTPUT
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # Get the subset expressed in time-subset
  time_fns <- time_subset$time_fns
  time_subset$time_fns <- NULL
  rr <- rep(TRUE, nrow(xgrid_comb))
  for (unit in names(time_subset)) {
    fn  <- time_fns[[unit]]
    rr  <- rr & fn(xgrid_comb[, get(column_mapping$date)]) %in% time_subset[[unit]]
  }
  xgrid_comb <- xgrid_comb[rr, ]
  stopifnot(length(rr) > 1)  # CHANGED: moved down, now checks after filtering

  # reset the order
  date_col <- column_mapping$date
  setorderv(
    xgrid_comb,
    'match_strata'
  )

  # set the class as an exposure
  class(xgrid_comb) <- c(class(xgrid_comb), "outcome")

  # set attributes
  attr(xgrid_comb, "column_mapping") <- column_mapping

  # at the end there shouldn't be any NAs, so give a warning to investigate
  if(any(is.na(xgrid_comb))) {
    stop("some NAs persist, investigate and submit a Github issue :) ")
  }

  return(xgrid_comb)
}
