# time_subset

``` r

library(cityClimateHealth)
library(data.table)
```

To create an exposure matrix for analysis, the time period must be
specified in years, months, or days. For example, we can make an
exposure matrix for Massachusetts maximum daily heat exposure between
2012 and 2015:

``` r

exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

ma_exposure_matrix <- make_exposure_matrix(
  ma_exposure,
  exposure_columns,
  time_subset = list(year = 2012:2015) 
)
#> Warning in make_exposure_matrix(ma_exposure, exposure_columns, time_subset = list(year = 2012:2015)): check about any NA, some corrections for this later,
#>             but only in certain columns
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

ma_exposure_matrix
#>               date  tmax_C   TOWN20   COUNTY20                     strata
#>             <IDat>   <num>   <char>     <char>                     <char>
#>      1: 2012-01-01  4.0592 ABINGTON   PLYMOUTH ABINGTON:yr2012:mn01:dow01
#>      2: 2012-01-02  9.6909 ABINGTON   PLYMOUTH ABINGTON:yr2012:mn01:dow02
#>      3: 2012-01-03  4.5612 ABINGTON   PLYMOUTH ABINGTON:yr2012:mn01:dow03
#>      4: 2012-01-04 -4.9586 ABINGTON   PLYMOUTH ABINGTON:yr2012:mn01:dow04
#>      5: 2012-01-05 -4.5164 ABINGTON   PLYMOUTH ABINGTON:yr2012:mn01:dow05
#>     ---                                                                  
#> 511346: 2015-12-27  9.7547 YARMOUTH BARNSTABLE YARMOUTH:yr2015:mn12:dow01
#> 511347: 2015-12-28 10.6210 YARMOUTH BARNSTABLE YARMOUTH:yr2015:mn12:dow02
#> 511348: 2015-12-29 -0.4467 YARMOUTH BARNSTABLE YARMOUTH:yr2015:mn12:dow03
#> 511349: 2015-12-30 -0.5062 YARMOUTH BARNSTABLE YARMOUTH:yr2015:mn12:dow04
#> 511350: 2015-12-31  1.1314 YARMOUTH BARNSTABLE YARMOUTH:yr2015:mn12:dow05
#>                match_strata explag1 explag2 explag3 explag4 explag5
#>                      <char>   <num>   <num>   <num>   <num>   <num>
#>      1: ABINGTON:2012-01-01  3.0964  0.1248  5.5685 12.8328  4.4747
#>      2: ABINGTON:2012-01-02  4.0592  3.0964  0.1248  5.5685 12.8328
#>      3: ABINGTON:2012-01-03  9.6909  4.0592  3.0964  0.1248  5.5685
#>      4: ABINGTON:2012-01-04  4.5612  9.6909  4.0592  3.0964  0.1248
#>      5: ABINGTON:2012-01-05 -4.9586  4.5612  9.6909  4.0592  3.0964
#>     ---                                                            
#> 511346: YARMOUTH:2015-12-27 16.5056 18.9899 11.3674 13.1147  9.1770
#> 511347: YARMOUTH:2015-12-28  9.7547 16.5056 18.9899 11.3674 13.1147
#> 511348: YARMOUTH:2015-12-29 10.6210  9.7547 16.5056 18.9899 11.3674
#> 511349: YARMOUTH:2015-12-30 -0.4467 10.6210  9.7547 16.5056 18.9899
#> 511350: YARMOUTH:2015-12-31 -0.5062 -0.4467 10.6210  9.7547 16.5056
```

We can also specify the exposure matrix for months. For example, we can
create a matrix that uses time exclusively from June through August, the
highest-heat months in Massachusetts. Note that months should be
specified as a plain integer, not a named list.

``` r

exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

ma_exposure_matrix <- make_exposure_matrix(
  ma_exposure,
  exposure_columns,
  time_subset = list(month = 6:8)   # 6 = June, 7 = July, 8 = August 
)
#> Warning in make_exposure_matrix(ma_exposure, exposure_columns, time_subset = list(month = 6:8)): check about any NA, some corrections for this later,
#>             but only in certain columns
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

ma_exposure_matrix
#>               date  tmax_C   TOWN20   COUNTY20                     strata
#>             <IDat>   <num>   <char>     <char>                     <char>
#>      1: 2010-06-01 28.5766 ABINGTON   PLYMOUTH ABINGTON:yr2010:mn06:dow03
#>      2: 2010-06-02 26.1360 ABINGTON   PLYMOUTH ABINGTON:yr2010:mn06:dow04
#>      3: 2010-06-03 29.0638 ABINGTON   PLYMOUTH ABINGTON:yr2010:mn06:dow05
#>      4: 2010-06-04 27.8157 ABINGTON   PLYMOUTH ABINGTON:yr2010:mn06:dow06
#>      5: 2010-06-05 29.4976 ABINGTON   PLYMOUTH ABINGTON:yr2010:mn06:dow07
#>     ---                                                                  
#> 354196: 2020-08-27 23.1338 YARMOUTH BARNSTABLE YARMOUTH:yr2020:mn08:dow05
#> 354197: 2020-08-28 18.9940 YARMOUTH BARNSTABLE YARMOUTH:yr2020:mn08:dow06
#> 354198: 2020-08-29 26.0081 YARMOUTH BARNSTABLE YARMOUTH:yr2020:mn08:dow07
#> 354199: 2020-08-30 24.1151 YARMOUTH BARNSTABLE YARMOUTH:yr2020:mn08:dow01
#> 354200: 2020-08-31 22.4953 YARMOUTH BARNSTABLE YARMOUTH:yr2020:mn08:dow02
#>                match_strata explag1 explag2 explag3 explag4 explag5
#>                      <char>   <num>   <num>   <num>   <num>   <num>
#>      1: ABINGTON:2010-06-01 26.9500 25.3234 26.3356 26.0529 35.4982
#>      2: ABINGTON:2010-06-02 28.5766 26.9500 25.3234 26.3356 26.0529
#>      3: ABINGTON:2010-06-03 26.1360 28.5766 26.9500 25.3234 26.3356
#>      4: ABINGTON:2010-06-04 29.0638 26.1360 28.5766 26.9500 25.3234
#>      5: ABINGTON:2010-06-05 27.8157 29.0638 26.1360 28.5766 26.9500
#>     ---                                                            
#> 354196: YARMOUTH:2020-08-27 29.2008 31.6397 32.1062 30.4607 30.6083
#> 354197: YARMOUTH:2020-08-28 23.1338 29.2008 31.6397 32.1062 30.4607
#> 354198: YARMOUTH:2020-08-29 18.9940 23.1338 29.2008 31.6397 32.1062
#> 354199: YARMOUTH:2020-08-30 26.0081 18.9940 23.1338 29.2008 31.6397
#> 354200: YARMOUTH:2020-08-31 24.1151 26.0081 18.9940 23.1338 29.2008
```
