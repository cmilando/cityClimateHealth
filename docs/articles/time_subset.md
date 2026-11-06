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
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

ma_exposure_matrix
#>               date   TOWN20   COUNTY20 tmax_C                     strata
#>             <IDat>   <char>     <char>  <num>                     <char>
#>      1: 2012-01-01 ABINGTON   PLYMOUTH    4.1 ABINGTON:yr2012:mn01:dow01
#>      2: 2012-01-02 ABINGTON   PLYMOUTH    9.7 ABINGTON:yr2012:mn01:dow02
#>      3: 2012-01-03 ABINGTON   PLYMOUTH    4.6 ABINGTON:yr2012:mn01:dow03
#>      4: 2012-01-04 ABINGTON   PLYMOUTH   -5.0 ABINGTON:yr2012:mn01:dow04
#>      5: 2012-01-05 ABINGTON   PLYMOUTH   -4.5 ABINGTON:yr2012:mn01:dow05
#>     ---                                                                 
#> 511346: 2015-12-27 YARMOUTH BARNSTABLE    9.8 YARMOUTH:yr2015:mn12:dow01
#> 511347: 2015-12-28 YARMOUTH BARNSTABLE   10.6 YARMOUTH:yr2015:mn12:dow02
#> 511348: 2015-12-29 YARMOUTH BARNSTABLE   -0.4 YARMOUTH:yr2015:mn12:dow03
#> 511349: 2015-12-30 YARMOUTH BARNSTABLE   -0.5 YARMOUTH:yr2015:mn12:dow04
#> 511350: 2015-12-31 YARMOUTH BARNSTABLE    1.1 YARMOUTH:yr2015:mn12:dow05
#>                match_strata explag1 explag2 explag3 explag4 explag5
#>                      <char>   <num>   <num>   <num>   <num>   <num>
#>      1: ABINGTON:2012-01-01     3.1     0.1     5.6    12.8     4.5
#>      2: ABINGTON:2012-01-02     4.1     3.1     0.1     5.6    12.8
#>      3: ABINGTON:2012-01-03     9.7     4.1     3.1     0.1     5.6
#>      4: ABINGTON:2012-01-04     4.6     9.7     4.1     3.1     0.1
#>      5: ABINGTON:2012-01-05    -5.0     4.6     9.7     4.1     3.1
#>     ---                                                            
#> 511346: YARMOUTH:2015-12-27    16.5    19.0    11.4    13.1     9.2
#> 511347: YARMOUTH:2015-12-28     9.8    16.5    19.0    11.4    13.1
#> 511348: YARMOUTH:2015-12-29    10.6     9.8    16.5    19.0    11.4
#> 511349: YARMOUTH:2015-12-30    -0.4    10.6     9.8    16.5    19.0
#> 511350: YARMOUTH:2015-12-31    -0.5    -0.4    10.6     9.8    16.5
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
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

ma_exposure_matrix
#>               date   TOWN20   COUNTY20 tmax_C                     strata
#>             <IDat>   <char>     <char>  <num>                     <char>
#>      1: 2010-06-01 ABINGTON   PLYMOUTH   28.6 ABINGTON:yr2010:mn06:dow03
#>      2: 2010-06-02 ABINGTON   PLYMOUTH   26.1 ABINGTON:yr2010:mn06:dow04
#>      3: 2010-06-03 ABINGTON   PLYMOUTH   29.1 ABINGTON:yr2010:mn06:dow05
#>      4: 2010-06-04 ABINGTON   PLYMOUTH   27.8 ABINGTON:yr2010:mn06:dow06
#>      5: 2010-06-05 ABINGTON   PLYMOUTH   29.5 ABINGTON:yr2010:mn06:dow07
#>     ---                                                                 
#> 193196: 2015-08-27 YARMOUTH BARNSTABLE   25.9 YARMOUTH:yr2015:mn08:dow05
#> 193197: 2015-08-28 YARMOUTH BARNSTABLE   23.8 YARMOUTH:yr2015:mn08:dow06
#> 193198: 2015-08-29 YARMOUTH BARNSTABLE   24.8 YARMOUTH:yr2015:mn08:dow07
#> 193199: 2015-08-30 YARMOUTH BARNSTABLE   25.3 YARMOUTH:yr2015:mn08:dow01
#> 193200: 2015-08-31 YARMOUTH BARNSTABLE   27.8 YARMOUTH:yr2015:mn08:dow02
#>                match_strata explag1 explag2 explag3 explag4 explag5
#>                      <char>   <num>   <num>   <num>   <num>   <num>
#>      1: ABINGTON:2010-06-01   26.95   25.30   26.30   26.10   35.50
#>      2: ABINGTON:2010-06-02   28.60   26.95   25.30   26.30   26.10
#>      3: ABINGTON:2010-06-03   26.10   28.60   26.95   25.30   26.30
#>      4: ABINGTON:2010-06-04   29.10   26.10   28.60   26.95   25.30
#>      5: ABINGTON:2010-06-05   27.80   29.10   26.10   28.60   26.95
#>     ---                                                            
#> 193196: YARMOUTH:2015-08-27   27.90   26.60   22.20   26.00   27.10
#> 193197: YARMOUTH:2015-08-28   25.90   27.90   26.60   22.20   26.00
#> 193198: YARMOUTH:2015-08-29   23.80   25.90   27.90   26.60   22.20
#> 193199: YARMOUTH:2015-08-30   24.80   23.80   25.90   27.90   26.60
#> 193200: YARMOUTH:2015-08-31   25.30   24.80   23.80   25.90   27.90
```
