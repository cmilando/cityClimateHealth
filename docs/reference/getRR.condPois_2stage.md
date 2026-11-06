# getRR method for condPois_2stage

getRR method for condPois_2stage

## Usage

``` r
# S3 method for class 'condPois_2stage'
getRR(x, ...)
```

## Arguments

- x:

  an object of class condPois_2stage

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates

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
getRR(m)
#>        TOWN20 COUNTY20 tmax_C       RR      RRlb     RRub  stage
#>        <char>   <char>  <num>    <num>     <num>    <num> <char>
#>    1: CHELSEA  SUFFOLK    8.0 0.918482 0.8279871 1.018868 stage2
#>    2: CHELSEA  SUFFOLK    8.1 0.918482 0.8279871 1.018868 stage2
#>    3: CHELSEA  SUFFOLK    8.2 0.918482 0.8279871 1.018868 stage2
#>    4: CHELSEA  SUFFOLK    8.3 0.918482 0.8279871 1.018868 stage2
#>    5: CHELSEA  SUFFOLK    8.4 0.918482 0.8279871 1.018868 stage2
#>   ---                                                           
#> 2130:  REVERE  SUFFOLK   34.2 1.229536 1.1670953 1.295318 stage1
#> 2131:  REVERE  SUFFOLK   34.3 1.230282 1.1663414 1.297728 stage1
#> 2132:  REVERE  SUFFOLK   34.4 1.231024 1.1655499 1.300176 stage1
#> 2133:  REVERE  SUFFOLK   34.5 1.231764 1.1647249 1.302662 stage1
#> 2134:  REVERE  SUFFOLK   34.6 1.232504 1.1638704 1.305184 stage1
#>           model_class
#>                <char>
#>    1: condPois_2stage
#>    2: condPois_2stage
#>    3: condPois_2stage
#>    4: condPois_2stage
#>    5: condPois_2stage
#>   ---                
#> 2130: condPois_2stage
#> 2131: condPois_2stage
#> 2132: condPois_2stage
#> 2133: condPois_2stage
#> 2134: condPois_2stage
```
