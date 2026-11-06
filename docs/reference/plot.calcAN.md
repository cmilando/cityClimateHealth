# Plot method for calcAN

Plot method for calcAN

## Usage

``` r
# S3 method for class 'calcAN'
plot(x, table_type, above_MMT, spatial_sub = NULL, override_limit = FALSE, ...)
```

## Arguments

- x:

  an object of class calcAN

- table_type:

  showing the rate table "rate" or number table "num"

- above_MMT:

  plot attributable numbers above or below the MMT

- spatial_sub:

  an option argument to subset the geo_units investigated

- override_limit:

  override the built-in plot limit

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a plot of either the attributable rate or number

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
plot(an, table_type = "num", above_MMT = TRUE)
```
