# Run a time series model for a single geographic unit One-stage POOLED time-series version of `condPois_1stage()`.

Run a time series model for a single geographic unit One-stage POOLED
time-series version of
[`condPois_1stage()`](https://climatehealth.city/reference/condPois_1stage.md).

## Usage

``` r
timeseries_1stage_glm(
  exposure_matrix,
  outcomes_tbl,
  argvar = NULL,
  arglag = NULL,
  maxlag = NULL,
  min_n = 50,
  time_trend = c("annual", "season"),
  dfseas = 7,
  df_time = NULL,
  population_col = NULL,
  add_dow = TRUE,
  global_cen = NULL,
  multi_zone = FALSE,
  verbose = TRUE,
  truncate = 0.025
)
```

## Arguments

- exposure_matrix:

  a matrix of exposures, with columns for lag, usually created by
  `make_exposure_matrix`

- outcomes_tbl:

  a data.table of outcomes, created by `make_outcome_table`; must have
  date mapping

- argvar:

  a list containing the `argvar` components for the `crossbasis`

- arglag:

  a list containing the `arglag` components for the `crossbasis`

- maxlag:

  an integer of the maximum lag

- min_n:

  an integer describing the minimum number of cases for a single
  geo_unit

- time_trend:

  one of "annual" or "season". "annual" (default) fits a single smooth
  spline over calendar date (ns(time_num, df = df_time)) and is
  appropriate for continuous, full-year daily data. "season" is for data
  that has been subset to a recurring part of the year (e.g., summer, or
  time_subset = list(month = 5:9) in make_outcome_table()/
  make_exposure_matrix()) - calendar date jumps discontinuously between
  seasons/years in that case, so "season" instead fits factor(year) (to
  soak up inter-annual variation) plus a smooth spline over
  day-of-season (ns(day_of_season, df = df_time), which restarts each
  year and captures the within-season shape of the trend, shared across
  years).

- dfseas:

  degrees of freedom PER YEAR OF DATA for the "annual" time trend
  spline. Default 7 (standard, see: Gasparrini). Ignored if df_time is
  supplied directly, and ignored entirely when time_trend = "season"
  (see df_time).

- df_time:

  degrees of freedom for the time-trend spline. Meaning depends on
  time_trend: for "annual" this is the total df for ns(date, df =
  df_time) and if null is computed as round(dfseas \* n_years). For
  "season" this is the df for the within-season smooth,
  ns(day_of_season, df = df_time), and if null defaults to 4.

- population_col:

  optional name of a population column in `outcomes_tbl`, used as an
  offset; see
  [`add_population_offset()`](https://climatehealth.city/reference/add_population_offset.md)

- add_dow:

  logical, whether to add factor(dow) to the model as another time trend
  adjustment. Default TRUE. \#' @param population_col optional, can be
  incorporated as population offset if that data is available

- global_cen:

  global centering point

- multi_zone:

  must be true

- verbose:

  used to print crossbasis args the first time

- truncate:

  proportion of the exposure distribution to trim from each tail when
  choosing the centering point (passed to `get_centered_cp`)

## Value

a timeseries_1stage model object

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
                                   outcome_columns, time_subset = time_subset)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#> 
#> crossbasis args for geo_unit  BOSTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.6 31.9
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> min_n: 50 
#> 
#> time_trend: annual 
#> n_years (approx): 1.42 
#> df_time for ns(date): 10 
#> add_dow: TRUE 
#> 
#> use_geo_int (multi-zone interactions): FALSE 
#> population offset: none
#> model formula:
#> daily_deaths ~ cb + ns(time_num, df = 10) + dow
#> <environment: 0x38cf122e0>
#> 
```
