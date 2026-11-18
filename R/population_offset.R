#' Add a population column to an outcome table, for use as a population
#' offset in `timeseries_1stage_glm()`
#'
#' `make_outcome_table()` doesn't produce a population column, since it's not
#' needed for the conditional Poisson design (`condPois_1stage`). It IS
#' needed for the time-series design (`timeseries_1stage_glm`) if you want to
#' adjust for population size/changes via `population_col`. This helper joins
#' a separate population dataset onto an already-built `outcomes_tbl`,
#' matching on geo_unit + either year or date (whichever granularity your
#' population data has), and validates that every row got a match before
#' returning.
#'
#' @param outcomes_tbl an object of class "outcome", as produced by
#'   `make_outcome_table()`
#' @param population_data a data.frame/data.table of population estimates.
#'   Commonly one row per geo_unit per year (e.g. annual census/ACS
#'   estimates), but can also be one row per geo_unit per day if you have
#'   finer-grained estimates.
#' @param population_columns a named list mapping columns in
#'   `population_data`. Must include:
#'   - `"geo_unit"`: column identifying the geographic unit (must use the
#'      same codes/values as outcomes_tbl's geo_unit column)
#'   - `"population"`: column with the population value (numeric, > 0)
#'   - EITHER `"year"` (integer year, for annual population data -- the
#'     typical case) OR `"date"` (for daily/finer population data) -- supply
#'     exactly one of these, not both
#' @param population_col_name name of the new column to add to
#'   `outcomes_tbl`. Default "population". This is the string you'd then
#'   pass as `population_col` to `timeseries_1stage_glm()`.
#' @param verbose print a short confirmation message. Default TRUE.
#'
#' @importFrom data.table setDT setorderv copy merge.data.table as.IDate
#'
#' @returns `outcomes_tbl` with a new population column added, class
#'   "outcome" and column_mapping attributes preserved
#' @export
#'
#' @examples
#' # outcomes_tbl already built via make_outcome_table()
#' # boston_deaths_tbl <- make_outcome_table(boston_deaths, outcome_columns,
#' #                                         time_subset = list(month = 5:9))
#'
#' # suppose you have annual town-level population estimates:
#' # head(ma_population)
#' #   TOWN20 year population
#' #   BOSTON 2010     617594
#' #   BOSTON 2011     624567
#'
#' population_columns <- list(
#'   "geo_unit"   = "TOWN20",
#'   "year"       = "year",
#'   "population" = "population"
#' )
#'
#' # boston_deaths_tbl <- add_population_offset(
#' #   outcomes_tbl        = boston_deaths_tbl,
#' #   population_data     = ma_population,
#' #   population_columns  = population_columns
#' # )
#'
#' # then:
#' # m1_ts <- timeseries_1stage_glm(
#' #   exposure_matrix = boston_exposure_mat,
#' #   outcomes_tbl    = boston_deaths_tbl,
#' #   time_trend      = "season",
#' #   population_col  = "population"
#' # )
add_population_offset <- function(outcomes_tbl,
                                  population_data,
                                  population_columns,
                                  join_on) {

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # VALIDATIONS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  stopifnot("outcome" %in% class(outcomes_tbl))
  stopifnot(is.list(population_columns))
  stopifnot(nrow(population_data) > 0)

  if(!("geo_unit" %in% names(population_columns))) {
    stop("population_columns must include 'geo_unit'")
  }

  if(!("population" %in% names(population_columns))) {
    stop("population_columns must include 'population'")
  }

  setDT(population_data)

  if(!all(unlist(population_columns) %in% names(population_data))) {
    stop("Values in population_columns not found as column names in population_data.
         Look for a typo.")
  }

  geo_col_pop   <- population_columns$geo_unit
  pop_col       <- population_columns$population

  # type checks on population_data
  if(!(is.character(population_data[[geo_col_pop]]))) {
    stop("geo_unit column in population_data must be character or factor")
  }
  if(!is.numeric(population_data[[pop_col]])) {
    stop("population column in population_data must be numeric")
  }
  if(any(is.na(population_data[[pop_col]]))) {
    stop("NA population values found in population_data -- fill these in or
         remove those rows before proceeding")
  }
  if(any(population_data[[pop_col]] <= 0)) {
    stop("population values in population_data must be > 0
         (log(population) is undefined otherwise)")
  }

  # check join_on
  stopifnot(all(join_on %in% names(outcomes_tbl)))
  stopifnot(all(join_on %in% names(population_data)))

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # METHODS - will need to be updated
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # save class and attributes
  xclass <- class(outcomes_tbl)
  tmp = attributes(outcomes_tbl)$column_mapping
  xdim = dim(outcomes_tbl)
  col.names = colnames(outcomes_tbl)
  ncol = length(col.names)
  col.classes = sapply(outcomes_tbl, class)

  # join
  outcomes_tbl = population_data[outcomes_tbl, on = join_on]

  # checks
  new_dim = dim(outcomes_tbl)
  stopifnot(new_dim[1] == xdim[1])
  stopifnot(new_dim[2] == xdim[2]+1)

  # reset columns
  setcolorder(outcomes_tbl, c(col.names, "population"))
  stopifnot(identical(sapply(outcomes_tbl, class)[1:ncol],col.classes))

  # reset class and attributes
  class(outcomes_tbl) = xclass
  attributes(outcomes_tbl)$column_mapping = tmp


  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # OUTPUT
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # final sanity check
  if(any(is.na(outcomes_tbl$population))) {
    stop("some NA population values persist after merging, investigate and
         submit a Github issue :) ")
  }

  return(outcomes_tbl)
}
