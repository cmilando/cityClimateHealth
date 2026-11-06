# getRR method for condPois_2stage_list

getRR method for condPois_2stage_list

## Usage

``` r
# S3 method for class 'condPois_2stage_list'
getRR(x, ...)
```

## Arguments

- x:

  an object of class condPois_2stage_list

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates across factor levels

## Examples

``` r
towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "factor" = "age_grp",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 %in% towns),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 %in% towns),
                                   outcome_columns, time_subset = time_subset)
#> > Combined factor is  age_grp 
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
# with a factor, a condPois_2stage_list is returned
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
#>        TOWN20 COUNTY20 tmax_C        RR     RRlb      RRub  stage age_grp
#>        <char>   <char>  <num>     <num>    <num>     <num> <char>  <char>
#>    1: CHELSEA  SUFFOLK    8.0 0.8962769 0.805826 0.9968806 stage2    0-17
#>    2: CHELSEA  SUFFOLK    8.1 0.8962769 0.805826 0.9968806 stage2    0-17
#>    3: CHELSEA  SUFFOLK    8.2 0.8962769 0.805826 0.9968806 stage2    0-17
#>    4: CHELSEA  SUFFOLK    8.3 0.8962769 0.805826 0.9968806 stage2    0-17
#>    5: CHELSEA  SUFFOLK    8.4 0.8962769 0.805826 0.9968806 stage2    0-17
#>   ---                                                                    
#> 6398:  REVERE  SUFFOLK   34.2 1.2283424 1.165341 1.2947499 stage1     65+
#> 6399:  REVERE  SUFFOLK   34.3 1.2291841 1.164665 1.2972775 stage1     65+
#> 6400:  REVERE  SUFFOLK   34.4 1.2300230 1.163951 1.2998453 stage1     65+
#> 6401:  REVERE  SUFFOLK   34.5 1.2308602 1.163204 1.3024516 stage1     65+
#> 6402:  REVERE  SUFFOLK   34.6 1.2316968 1.162427 1.3050947 stage1     65+
#>                model_class
#>                     <char>
#>    1: condPois_2stage_list
#>    2: condPois_2stage_list
#>    3: condPois_2stage_list
#>    4: condPois_2stage_list
#>    5: condPois_2stage_list
#>   ---                     
#> 6398: condPois_2stage_list
#> 6399: condPois_2stage_list
#> 6400: condPois_2stage_list
#> 6401: condPois_2stage_list
#> 6402: condPois_2stage_list
```
