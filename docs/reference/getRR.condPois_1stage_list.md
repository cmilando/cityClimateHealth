# getRR method for condPois_1stage_list

getRR method for condPois_1stage_list

## Usage

``` r
# S3 method for class 'condPois_1stage_list'
getRR(x, ...)
```

## Arguments

- x:

  an object of class condPois_1stage_list

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
# with a factor, a condPois_1stage_list is returned
m <- condPois_1stage(exposure_mat, outcomes_tbl)
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
#>      tmax_C age_grp n_geo_names       RR     RRlb     RRub          model_class
#>       <num>  <char>      <char>    <num>    <num>    <num>               <char>
#>   1:    8.6    0-17      BOSTON 1.075541 1.042560 1.109565 condPois_1stage_list
#>   2:    8.7    0-17      BOSTON 1.075541 1.042560 1.109565 condPois_1stage_list
#>   3:    8.8    0-17      BOSTON 1.075541 1.042560 1.109565 condPois_1stage_list
#>   4:    8.9    0-17      BOSTON 1.075541 1.042560 1.109565 condPois_1stage_list
#>   5:    9.0    0-17      BOSTON 1.075541 1.042560 1.109565 condPois_1stage_list
#>  ---                                                                           
#> 812:   35.3     65+      BOSTON 1.346682 1.270917 1.426964 condPois_1stage_list
#> 813:   35.4     65+      BOSTON 1.346682 1.270917 1.426964 condPois_1stage_list
#> 814:   35.5     65+      BOSTON 1.346682 1.270917 1.426964 condPois_1stage_list
#> 815:   35.6     65+      BOSTON 1.346682 1.270917 1.426964 condPois_1stage_list
#> 816:   35.7     65+      BOSTON 1.346682 1.270917 1.426964 condPois_1stage_list
```
