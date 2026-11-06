# Calculate attributable number and attributable rates

Calculate attributable number and attributable rates

## Usage

``` r
calc_AN(
  model,
  outcomes_tbl,
  pop_data,
  spatial_agg_type,
  spatial_join_col,
  by_year = FALSE,
  scale = 1,
  nsim = 300,
  verbose = 0
)
```

## Arguments

- model:

  a model object of class `condPois_2stage`, `condPois_1stage`, or
  `condPois_sb`

- outcomes_tbl:

  a table of outcomes, of class `outcomes`

- pop_data:

  population data

- spatial_agg_type:

  what is the spatial resolution you are aggregating to

- spatial_join_col:

  how should you join population data to the outcome table

- by_year:

  Export annual counts

- scale:

  scalar that you multiply outcomes by, mostly used for
  factor_is_temporal adjustment

- nsim:

  number of simulations required for calculation of empirical CI
  (default = 300)

- verbose:

  0 = no printing, 1 = headers, 2 = detailed

## Value

calc_AN object that contains attributable numbers, rate tables, and
other components necessary for plotting AN

## Examples

``` r
towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 %in% towns),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 %in% towns),
                                   outcome_columns, time_subset = time_subset)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
m <- condPois_2stage(exposure_mat, outcomes_tbl, global_cen = 20)
#> 
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.6 32.1
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> CHELSEA:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
pop <- data.table::data.table(TOWN20 = ma_pop_data$TOWN20,
                              population = rowSums(ma_pop_data[, -1]))
pop <- subset(pop, TOWN20 %in% towns)
an <- calc_AN(m, outcomes_tbl, pop, spatial_agg_type = "TOWN20",
              spatial_join_col = "TOWN20", nsim = 20)
```
