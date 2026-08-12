# Demo of Collapse funcationality

``` r

library(cityClimateHealth)
```

``` r

library(dlnm)
#> This is dlnm 2.4.10. For details: help(dlnm) and vignette('dlnmOverview').
library(gnm)
library(ggplot2)
library(data.table)

# create exposure matrix
exposure_columns <- list(
  "date" = "date",
 "exposure" = "tmax_C",
 "geo_unit" = "TOWN20",
 "geo_unit_grp" = "COUNTY20"
)

## basic
exposure_mat <- 
  make_exposure_matrix(
    subset(ma_exposure, TOWN20 %in% c('BOSTON', 'CHELSEA')),
    exposure_columns, 
    time_subset = list(month = 5:9)
)
#> -- NA values automatically removed
#> > No factors to collapse to, using all data
#> > grp_level == FALSE, so using geo_unit as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
head(exposure_mat)
#>          date TOWN20 COUNTY20  tmax_C                   strata
#>        <IDat> <char>   <char>   <num>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK 23.1386 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-02 BOSTON  SUFFOLK 26.1014 BOSTON:yr2010:mn05:dow01
#> 3: 2010-05-03 BOSTON  SUFFOLK 31.5648 BOSTON:yr2010:mn05:dow02
#> 4: 2010-05-04 BOSTON  SUFFOLK 27.7814 BOSTON:yr2010:mn05:dow03
#> 5: 2010-05-05 BOSTON  SUFFOLK 26.2820 BOSTON:yr2010:mn05:dow04
#> 6: 2010-05-06 BOSTON  SUFFOLK 25.8546 BOSTON:yr2010:mn05:dow05
#>         match_strata  explag1  explag2  explag3  explag4  explag5
#>               <char>    <num>    <num>    <num>    <num>    <num>
#> 1: BOSTON:2010-05-01 15.73815  8.33770 10.85230 16.44320 18.74090
#> 2: BOSTON:2010-05-02 23.13860 15.73815  8.33770 10.85230 16.44320
#> 3: BOSTON:2010-05-03 26.10140 23.13860 15.73815  8.33770 10.85230
#> 4: BOSTON:2010-05-04 31.56480 26.10140 23.13860 15.73815  8.33770
#> 5: BOSTON:2010-05-05 27.78140 31.56480 26.10140 23.13860 15.73815
#> 6: BOSTON:2010-05-06 26.28200 27.78140 31.56480 26.10140 23.13860

## group 
exposure_mat <- 
  make_exposure_matrix(
    subset(ma_exposure, TOWN20 %in% c('BOSTON', 'CHELSEA')),
    exposure_columns, 
    time_subset = list(month = 5:9), 
    grp_level = T,
    keep_unit_exposures = F
  )
#> -- NA values automatically removed
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
head(exposure_mat)
#>          date COUNTY20   tmax_C spatial_grp                    strata
#>        <IDat>   <char>    <num>      <char>                    <char>
#> 1: 2010-05-01  SUFFOLK 23.12585         ALL SUFFOLK:yr2010:mn05:dow07
#> 2: 2010-05-02  SUFFOLK 26.17395         ALL SUFFOLK:yr2010:mn05:dow01
#> 3: 2010-05-03  SUFFOLK 31.47430         ALL SUFFOLK:yr2010:mn05:dow02
#> 4: 2010-05-04  SUFFOLK 27.80825         ALL SUFFOLK:yr2010:mn05:dow03
#> 5: 2010-05-05  SUFFOLK 26.34880         ALL SUFFOLK:yr2010:mn05:dow04
#> 6: 2010-05-06  SUFFOLK 25.82610         ALL SUFFOLK:yr2010:mn05:dow05
#>          match_strata  explag1  explag2  explag3  explag4  explag5
#>                <char>    <num>    <num>    <num>    <num>    <num>
#> 1: SUFFOLK:2010-05-01 14.88210  8.35975 10.84310 16.44295 18.77265
#> 2: SUFFOLK:2010-05-02 23.12585 14.88210  8.35975 10.84310 16.44295
#> 3: SUFFOLK:2010-05-03 26.17395 23.12585 14.88210  8.35975 10.84310
#> 4: SUFFOLK:2010-05-04 31.47430 26.17395 23.12585 14.88210  8.35975
#> 5: SUFFOLK:2010-05-05 27.80825 31.47430 26.17395 23.12585 14.88210
#> 6: SUFFOLK:2010-05-06 26.34880 27.80825 31.47430 26.17395 23.12585

## group but keep unit exposures
exposure_mat <- 
  make_exposure_matrix(
    subset(ma_exposure, TOWN20 %in% c('BOSTON', 'CHELSEA')),
    exposure_columns, 
    time_subset = list(month = 5:9), 
    grp_level = T,
    keep_unit_exposures = T
  )
#> -- NA values automatically removed
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
head(exposure_mat)
#>          date TOWN20 COUNTY20  tmax_C                   strata
#>        <IDat> <char>   <char>   <num>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK 23.1386 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-02 BOSTON  SUFFOLK 26.1014 BOSTON:yr2010:mn05:dow01
#> 3: 2010-05-03 BOSTON  SUFFOLK 31.5648 BOSTON:yr2010:mn05:dow02
#> 4: 2010-05-04 BOSTON  SUFFOLK 27.7814 BOSTON:yr2010:mn05:dow03
#> 5: 2010-05-05 BOSTON  SUFFOLK 26.2820 BOSTON:yr2010:mn05:dow04
#> 6: 2010-05-06 BOSTON  SUFFOLK 25.8546 BOSTON:yr2010:mn05:dow05
#>         match_strata spatial_grp explag1  explag2 explag3  explag4 explag5
#>               <char>      <char>   <num>    <num>   <num>    <num>   <num>
#> 1: BOSTON:2010-05-01         ALL 14.8821 11.63195  8.3818  8.33770 10.8339
#> 2: BOSTON:2010-05-02         ALL 23.1131 23.13860 14.8821 11.63195  8.3818
#> 3: BOSTON:2010-05-03         ALL 26.2465 26.10140 23.1131 23.13860 14.8821
#> 4: BOSTON:2010-05-04         ALL 31.3838 31.56480 26.2465 26.10140 23.1131
#> 5: BOSTON:2010-05-05         ALL 27.8351 27.78140 31.3838 31.56480 26.2465
#> 6: BOSTON:2010-05-06         ALL 26.4156 26.28200 27.8351 27.78140 31.3838
```

Now try with outcomes

``` r


# create outcome table
outcome_columns <- list(
 "date" = "date",
 "outcome" = "daily_deaths",
 "factor" = c('age_grp', 'sex'),
 "geo_unit" = "TOWN20",
 "geo_unit_grp" = "COUNTY20"
)

boston_deaths <- subset(ma_deaths, TOWN20 %in% c('BOSTON', 'CHELSEA'))
head(boston_deaths)
#>          date TOWN20 daily_deaths age_grp    sex COUNTY20
#>        <Date> <char>        <int>  <char> <char>   <char>
#> 1: 2010-05-01 BOSTON          385    0-17      M  SUFFOLK
#> 2: 2010-05-02 BOSTON          367    0-17      M  SUFFOLK
#> 3: 2010-05-03 BOSTON          431    0-17      M  SUFFOLK
#> 4: 2010-05-04 BOSTON          431    0-17      M  SUFFOLK
#> 5: 2010-05-05 BOSTON          456    0-17      M  SUFFOLK
#> 6: 2010-05-06 BOSTON          400    0-17      M  SUFFOLK

## basic
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9)
)
#> > No factors to collapse to, using all data
#> > grp_level == FALSE, so using geo_unit as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9)): 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date TOWN20 COUNTY20 daily_deaths                   strata
#>        <IDat> <char>   <char>        <int>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK         2238 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-02 BOSTON  SUFFOLK         2089 BOSTON:yr2010:mn05:dow01
#> 3: 2010-05-03 BOSTON  SUFFOLK         2374 BOSTON:yr2010:mn05:dow02
#> 4: 2010-05-04 BOSTON  SUFFOLK         2354 BOSTON:yr2010:mn05:dow03
#> 5: 2010-05-05 BOSTON  SUFFOLK         2489 BOSTON:yr2010:mn05:dow04
#> 6: 2010-05-06 BOSTON  SUFFOLK         2191 BOSTON:yr2010:mn05:dow05
#>    strata_total      match_strata
#>           <int>            <char>
#> 1:        11312 BOSTON:2010-05-01
#> 2:        10929 BOSTON:2010-05-02
#> 3:        11435 BOSTON:2010-05-03
#> 4:         9372 BOSTON:2010-05-04
#> 5:         9193 BOSTON:2010-05-05
#> 6:         8657 BOSTON:2010-05-06

## group
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9),
  grp_level = T,
  keep_unit_outcomes = F
)
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9), : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date COUNTY20 daily_deaths spatial_grp                    strata
#>        <IDat>   <char>        <int>      <char>                    <char>
#> 1: 2010-05-01  SUFFOLK         2370         ALL SUFFOLK:yr2010:mn05:dow07
#> 2: 2010-05-02  SUFFOLK         2220         ALL SUFFOLK:yr2010:mn05:dow01
#> 3: 2010-05-03  SUFFOLK         2511         ALL SUFFOLK:yr2010:mn05:dow02
#> 4: 2010-05-04  SUFFOLK         2492         ALL SUFFOLK:yr2010:mn05:dow03
#> 5: 2010-05-05  SUFFOLK         2634         ALL SUFFOLK:yr2010:mn05:dow04
#> 6: 2010-05-06  SUFFOLK         2323         ALL SUFFOLK:yr2010:mn05:dow05
#>    strata_total       match_strata
#>           <int>             <char>
#> 1:        11954 SUFFOLK:2010-05-01
#> 2:        11593 SUFFOLK:2010-05-02
#> 3:        12108 SUFFOLK:2010-05-03
#> 4:         9909 SUFFOLK:2010-05-04
#> 5:         9728 SUFFOLK:2010-05-05
#> 6:         9176 SUFFOLK:2010-05-06

## group but keep unit outcomes
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9),
  grp_level = T,
  keep_unit_outcomes = T
)
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9), : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date TOWN20 COUNTY20 daily_deaths                   strata
#>        <IDat> <char>   <char>        <int>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK         2238 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-02 BOSTON  SUFFOLK         2089 BOSTON:yr2010:mn05:dow01
#> 3: 2010-05-03 BOSTON  SUFFOLK         2374 BOSTON:yr2010:mn05:dow02
#> 4: 2010-05-04 BOSTON  SUFFOLK         2354 BOSTON:yr2010:mn05:dow03
#> 5: 2010-05-05 BOSTON  SUFFOLK         2489 BOSTON:yr2010:mn05:dow04
#> 6: 2010-05-06 BOSTON  SUFFOLK         2191 BOSTON:yr2010:mn05:dow05
#>    strata_total      match_strata spatial_grp
#>           <int>            <char>      <char>
#> 1:        11312 BOSTON:2010-05-01         ALL
#> 2:        10929 BOSTON:2010-05-02         ALL
#> 3:        11435 BOSTON:2010-05-03         ALL
#> 4:         9372 BOSTON:2010-05-04         ALL
#> 5:         9193 BOSTON:2010-05-05         ALL
#> 6:         8657 BOSTON:2010-05-06         ALL

## add a factor
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9),
  grp_level = T,
  keep_unit_outcomes = T,
  collapse_to = c('sex', 'age_grp')
)
#> > Factors in data
#> > grp_level == TRUE and keep_unit == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9), : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.
```
