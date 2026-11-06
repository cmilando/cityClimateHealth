# getRR method for timeseries_1stage

getRR method for timeseries_1stage

## Usage

``` r
# S3 method for class 'timeseries_1stage'
getRR(x, ...)
```

## Arguments

- x:

  an object of class timeseries_1stage

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates

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
#> <environment: 0x137b0e000>
#> 
getRR(m)
#>      tmax_C       RR     RRlb     RRub n_geo_names       model_class
#>       <num>    <num>    <num>    <num>      <char>            <char>
#>   1:    8.6 1.000000 1.000000 1.000000      BOSTON timeseries_1stage
#>   2:    8.7 1.001568 1.000812 1.002324      BOSTON timeseries_1stage
#>   3:    8.8 1.003138 1.001625 1.004654      BOSTON timeseries_1stage
#>   4:    8.9 1.004711 1.002439 1.006989      BOSTON timeseries_1stage
#>   5:    9.0 1.006286 1.003253 1.009328      BOSTON timeseries_1stage
#>  ---                                                                
#> 268:   35.3 1.488931 1.363113 1.626363      BOSTON timeseries_1stage
#> 269:   35.4 1.494788 1.367034 1.634481      BOSTON timeseries_1stage
#> 270:   35.5 1.500676 1.370949 1.642678      BOSTON timeseries_1stage
#> 271:   35.6 1.506594 1.374858 1.650951      BOSTON timeseries_1stage
#> 272:   35.7 1.512537 1.378760 1.659295      BOSTON timeseries_1stage
```
