# getRR method for condPois_1stage

getRR method for condPois_1stage

## Usage

``` r
# S3 method for class 'condPois_1stage'
getRR(x, ...)
```

## Arguments

- x:

  an object of class condPois_1stage

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
m <- condPois_1stage(exposure_mat, outcomes_tbl)
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
#> strata:
#> BOSTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
getRR(m)
#>      tmax_C       RR     RRlb     RRub n_geo_names     model_class
#>       <num>    <num>    <num>    <num>      <char>          <char>
#>   1:    8.6 1.060175 1.028071 1.093281      BOSTON condPois_1stage
#>   2:    8.7 1.060175 1.028071 1.093281      BOSTON condPois_1stage
#>   3:    8.8 1.060175 1.028071 1.093281      BOSTON condPois_1stage
#>   4:    8.9 1.060175 1.028071 1.093281      BOSTON condPois_1stage
#>   5:    9.0 1.060175 1.028071 1.093281      BOSTON condPois_1stage
#>  ---                                                              
#> 268:   35.3 1.347634 1.271823 1.427964      BOSTON condPois_1stage
#> 269:   35.4 1.347634 1.271823 1.427964      BOSTON condPois_1stage
#> 270:   35.5 1.347634 1.271823 1.427964      BOSTON condPois_1stage
#> 271:   35.6 1.347634 1.271823 1.427964      BOSTON condPois_1stage
#> 272:   35.7 1.347634 1.271823 1.427964      BOSTON condPois_1stage
```
