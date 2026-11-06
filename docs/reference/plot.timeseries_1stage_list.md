# Plot method for timeseries_1stage_list

Plot method for timeseries_1stage_list

## Usage

``` r
# S3 method for class 'timeseries_1stage_list'
plot(x, xlab = NULL, ylab = NULL, title = NULL, ...)
```

## Arguments

- x:

  an object of class timeseries_1stage_list

- xlab:

  xlab override

- ylab:

  ylab override

- title:

  title override

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a ggplot object

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "factor" = "age_grp",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
                                   outcome_columns, time_subset = time_subset)
#> > Combined factor is  age_grp 
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
# with a factor, a timeseries_1stage_list is returned
m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#> < age_grp : 0-17 >
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
#> <environment: 0x151d73d78>
#> 
#> < age_grp : 18-64 >
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
#> <environment: 0x3ad06df90>
#> 
#> < age_grp : 65+ >
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
#> <environment: 0x3b21b9838>
#> 
plot(m)
```
