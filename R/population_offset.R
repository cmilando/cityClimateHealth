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
#' @importFrom data.table setDT setorderv copy merge.data.table
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
                                  population_col_name = "population",
                                  verbose = TRUE) {

  #
  #VALIDATIONS
  #

  stopifnot("outcome" %in% class(outcomes_tbl))
  stopifnot(is.list(population_columns))
  stopifnot(nrow(population_data) > 0)

  valid_types <- c("geo_unit", "year", "date", "population")
  if(!all(names(population_columns) %in% valid_types)) {
    stop("Names of population_columns must be one of: geo_unit, year, date, population")
  }
  if(!("geo_unit" %in% names(population_columns))) {
    stop("population_columns must include 'geo_unit'")
  }
  if(!("population" %in% names(population_columns))) {
    stop("population_columns must include 'population'")
  }
  if(!any(c("year", "date") %in% names(population_columns))) {
    stop("population_columns must include either 'year' or 'date' to match
         the temporal granularity of your population data")
  }
  if(all(c("year", "date") %in% names(population_columns))) {
    stop("population_columns should include only one of 'year' or 'date', not both")
  }

  setDT(population_data)
  population_data <- copy(population_data)

  if(!all(unlist(population_columns) %in% names(population_data))) {
    stop("Values in population_columns not found as column names in population_data.
         Look for a typo.")
  }

  use_year      <- "year" %in% names(population_columns)
  geo_col_pop   <- population_columns$geo_unit
  pop_col       <- population_columns$population
  time_col_pop  <- if(use_year) population_columns$year else population_columns$date

  # type checks on population_data
  if(!(is.character(population_data[[geo_col_pop]]) | is.factor(population_data[[geo_col_pop]]))) {
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

  population_data[[geo_col_pop]] <- as.character(population_data[[geo_col_pop]])
  if(use_year) {
    population_data[[time_col_pop]] <- as.integer(population_data[[time_col_pop]])
  } else {
    population_data[[time_col_pop]] <- as.Date(population_data[[time_col_pop]])
  }

  # duplicate geo_unit x time rows in population_data are ambiguous to join
  dup_keys <- population_data[, .N, by = c(geo_col_pop, time_col_pop)][N > 1]
  if(nrow(dup_keys) > 0) {
    stop(sprintf(
      "population_data has duplicate rows for the same geo_unit + %s combination
      - resolve before merging. First few duplicate keys:\n%s",
      ifelse(use_year, "year", "date"),
      paste(utils::capture.output(print(head(dup_keys))), collapse = "\n")
    ))
  }

  #PREP outcomes_tbl AND MERGE

  out_column_mapping <- attributes(outcomes_tbl)$column_mapping
  out_class <- class(outcomes_tbl)

  date_col     <- out_column_mapping$date
  geo_unit_col <- out_column_mapping$geo_unit

  if(is.null(date_col)) {
    stop("outcomes_tbl must have a `date` entry in its column_mapping")
  }
  if(is.null(geo_unit_col)) {
    stop("outcomes_tbl must have a `geo_unit` entry in its column_mapping")
  }
  if(population_col_name %in% names(outcomes_tbl)) {
    stop(sprintf("outcomes_tbl already has a column called '%s' -- pick a
                 different population_col_name or drop the existing column first",
                 population_col_name))
  }

  outcomes_tbl <- copy(outcomes_tbl)
  outcomes_tbl[, (date_col) := as.Date(get(date_col))]
  outcomes_tbl[, .join_geo := as.character(get(geo_unit_col))]
  if(use_year) {
    outcomes_tbl[, .join_time := data.table::year(get(date_col))]
  } else {
    outcomes_tbl[, .join_time := get(date_col)]
  }

  pop_small <- population_data[, c(geo_col_pop, time_col_pop, pop_col), with = FALSE]
  names(pop_small) <- c(".join_geo", ".join_time", population_col_name)

  # check coverage BEFORE merging onto the full table, so the error message
  # is a clean list of missing combinations rather than a wall of NA rows
  needed <- unique(outcomes_tbl[, .(.join_geo, .join_time)])
  coverage_check <- merge(needed, pop_small, by = c(".join_geo", ".join_time"), all.x = TRUE)
  missing_combo <- coverage_check[is.na(get(population_col_name))]

  if(nrow(missing_combo) > 0) {
    stop(sprintf(
      "population_data is missing population values for %d geo_unit/%s
      combination(s) present in outcomes_tbl. Examples (up to 10 shown):\n%s",
      nrow(missing_combo), ifelse(use_year, "year", "date"),
      paste(utils::capture.output(print(head(missing_combo[, .(.join_geo, .join_time)], 10))),
            collapse = "\n")
    ))
  }

  outcomes_tbl <- merge(outcomes_tbl, pop_small, by = c(".join_geo", ".join_time"), all.x = TRUE)
  outcomes_tbl[, c(".join_geo", ".join_time") := NULL]

  # merges can scramble row order, restore a sensible order
  if("match_strata" %in% names(outcomes_tbl)) {
    setorderv(outcomes_tbl, "match_strata")
  } else {
    setorderv(outcomes_tbl, c(geo_unit_col, date_col))
  }

  # data.table's merge() drops custom attributes - reattach class + column_mapping
  class(outcomes_tbl) <- out_class
  attr(outcomes_tbl, "column_mapping") <- out_column_mapping

  # final sanity check
  if(any(is.na(outcomes_tbl[[population_col_name]]))) {
    stop("some NA population values persist after merging, investigate and
         submit a Github issue :) ")
  }

  if(verbose) {
    cat("> population successfully joined as column: '", population_col_name, "'\n", sep = "")
    cat("> matched on geo_unit + ", ifelse(use_year, "year", "date"), "\n", sep = "")
    cat("> use population_col = '", population_col_name,
        "' in timeseries_1stage_glm()\n", sep = "")
  }

  return(outcomes_tbl)
}
