# getRR method for timeseries_1stage_list

getRR method for timeseries_1stage_list

## Usage

``` r
# S3 method for class 'timeseries_1stage_list'
getRR(x, ...)
```

## Arguments

- x:

  an object of class timeseries_1stage_list

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates across factor levels

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
#> <environment: 0x37c20f6a0>
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
#> <environment: 0x37cbff6a8>
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
#> <environment: 0x1370ef358>
#> 
getRR(m)
#>      tmax_C age_grp       RR     RRlb     RRub            model_class
#>       <num>  <char>    <num>    <num>    <num>                 <char>
#>   1:    8.6    0-17 1.000000 1.000000 1.000000 timeseries_1stage_list
#>   2:    8.7    0-17 1.001834 1.001071 1.002599 timeseries_1stage_list
#>   3:    8.8    0-17 1.003672 1.002143 1.005204 timeseries_1stage_list
#>   4:    8.9    0-17 1.005513 1.003216 1.007816 timeseries_1stage_list
#>   5:    9.0    0-17 1.007358 1.004290 1.010435 timeseries_1stage_list
#>  ---                                                                 
#> 812:   35.3     65+ 1.486694 1.361118 1.623855 timeseries_1stage_list
#> 813:   35.4     65+ 1.492488 1.364984 1.631902 timeseries_1stage_list
#> 814:   35.5     65+ 1.498314 1.368845 1.640027 timeseries_1stage_list
#> 815:   35.6     65+ 1.504168 1.372699 1.648228 timeseries_1stage_list
#> 816:   35.7     65+ 1.510048 1.376545 1.656498 timeseries_1stage_list
```
