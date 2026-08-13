# Demo of factor uses

``` r

library(cityClimateHealth)
```

``` r


outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20",
  "factor" = "age_grp"
)

ma_outcomes_tbl_fct <- make_outcome_table(
  ma_deaths, outcome_columns, 
  time_subset = list(
       month = 5:9,
       year = 2012:2015
     ))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

head(ma_outcomes_tbl_fct)
#>          date   TOWN20 COUNTY20 age_grp daily_deaths                     strata
#>        <IDat>   <char>   <char>  <char>        <int>                     <char>
#> 1: 2012-05-01 ABINGTON PLYMOUTH    0-17           20 ABINGTON:yr2012:mn05:dow03
#> 2: 2012-05-01 ABINGTON PLYMOUTH   18-64           20 ABINGTON:yr2012:mn05:dow03
#> 3: 2012-05-01 ABINGTON PLYMOUTH     65+           20 ABINGTON:yr2012:mn05:dow03
#> 4: 2012-05-02 ABINGTON PLYMOUTH    0-17           20 ABINGTON:yr2012:mn05:dow04
#> 5: 2012-05-02 ABINGTON PLYMOUTH   18-64           20 ABINGTON:yr2012:mn05:dow04
#> 6: 2012-05-02 ABINGTON PLYMOUTH     65+           20 ABINGTON:yr2012:mn05:dow04
#>    strata_total        match_strata
#>           <int>              <char>
#> 1:          112 ABINGTON:2012-05-01
#> 2:          102 ABINGTON:2012-05-01
#> 3:          107 ABINGTON:2012-05-01
#> 4:          116 ABINGTON:2012-05-02
#> 5:          105 ABINGTON:2012-05-02
#> 6:          113 ABINGTON:2012-05-02
```

``` r

outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20",
  "factor" = c("age_grp", 'sex')
)

ma_outcomes_tbl_fct <- make_outcome_table(
  ma_deaths, outcome_columns, 
  time_subset = list(
       month = 5:9,
       year = 2012:2015
     ))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

head(ma_outcomes_tbl_fct)
#>          date   TOWN20 COUNTY20 age_grp|sex daily_deaths
#>        <IDat>   <char>   <char>      <char>        <int>
#> 1: 2012-05-01 ABINGTON PLYMOUTH      0-17|M           10
#> 2: 2012-05-01 ABINGTON PLYMOUTH     18-64|M           10
#> 3: 2012-05-01 ABINGTON PLYMOUTH       65+|M           10
#> 4: 2012-05-01 ABINGTON PLYMOUTH      0-17|F           10
#> 5: 2012-05-01 ABINGTON PLYMOUTH     18-64|F           10
#> 6: 2012-05-01 ABINGTON PLYMOUTH       65+|F           10
#>                        strata strata_total        match_strata
#>                        <char>        <int>              <char>
#> 1: ABINGTON:yr2012:mn05:dow03           55 ABINGTON:2012-05-01
#> 2: ABINGTON:yr2012:mn05:dow03           52 ABINGTON:2012-05-01
#> 3: ABINGTON:yr2012:mn05:dow03           53 ABINGTON:2012-05-01
#> 4: ABINGTON:yr2012:mn05:dow03           57 ABINGTON:2012-05-01
#> 5: ABINGTON:yr2012:mn05:dow03           50 ABINGTON:2012-05-01
#> 6: ABINGTON:yr2012:mn05:dow03           54 ABINGTON:2012-05-01
```
