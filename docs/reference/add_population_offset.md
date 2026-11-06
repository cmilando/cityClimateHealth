# Add a population column to an outcome table, for use as a population offset in `timeseries_1stage_glm()`

[`make_outcome_table()`](https://climatehealth.city/reference/make_outcome_table.md)
doesn't produce a population column, since it's not needed for the
conditional Poisson design (`condPois_1stage`). It IS needed for the
time-series design (`timeseries_1stage_glm`) if you want to adjust for
population size/changes via `population_col`. This helper joins a
separate population dataset onto an already-built `outcomes_tbl`,
matching on geo_unit + either year or date (whichever granularity your
population data has), and validates that every row got a match before
returning.

## Usage

``` r
add_population_offset(
  outcomes_tbl,
  population_data,
  population_columns,
  population_col_name = "population",
  verbose = TRUE
)
```

## Arguments

- outcomes_tbl:

  an object of class "outcome", as produced by
  [`make_outcome_table()`](https://climatehealth.city/reference/make_outcome_table.md)

- population_data:

  a data.frame/data.table of population estimates. Commonly one row per
  geo_unit per year (e.g. annual census/ACS estimates), but can also be
  one row per geo_unit per day if you have finer-grained estimates.

- population_columns:

  a named list mapping columns in `population_data`. Must include:

  - `"geo_unit"`: column identifying the geographic unit (must use the
    same codes/values as outcomes_tbl's geo_unit column)

  - `"population"`: column with the population value (numeric, \> 0)

  - EITHER `"year"` (integer year, for annual population data – the
    typical case) OR `"date"` (for daily/finer population data) – supply
    exactly one of these, not both

- population_col_name:

  name of the new column to add to `outcomes_tbl`. Default "population".
  This is the string you'd then pass as `population_col` to
  [`timeseries_1stage_glm()`](https://climatehealth.city/reference/timeseries_1stage_glm.md).

- verbose:

  print a short confirmation message. Default TRUE.

## Value

`outcomes_tbl` with a new population column added, class "outcome" and
column_mapping attributes preserved

## Examples

``` r
# outcomes_tbl already built via make_outcome_table()
# boston_deaths_tbl <- make_outcome_table(boston_deaths, outcome_columns,
#                                         time_subset = list(month = 5:9))

# suppose you have annual town-level population estimates:
# head(ma_population)
#   TOWN20 year population
#   BOSTON 2010     617594
#   BOSTON 2011     624567

population_columns <- list(
  "geo_unit"   = "TOWN20",
  "year"       = "year",
  "population" = "population"
)

# boston_deaths_tbl <- add_population_offset(
#   outcomes_tbl        = boston_deaths_tbl,
#   population_data     = ma_population,
#   population_columns  = population_columns
# )

# then:
# m1_ts <- timeseries_1stage_glm(
#   exposure_matrix = boston_exposure_mat,
#   outcomes_tbl    = boston_deaths_tbl,
#   time_trend      = "season",
#   population_col  = "population"
# )
```
