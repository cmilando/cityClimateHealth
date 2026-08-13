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
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
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
#> > Combined factor is  age_grp|sex
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9)): 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date TOWN20 COUNTY20 age_grp|sex daily_deaths                   strata
#>        <IDat> <char>   <char>      <char>        <int>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK      0-17|M          385 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-01 BOSTON  SUFFOLK     18-64|M          363 BOSTON:yr2010:mn05:dow07
#> 3: 2010-05-01 BOSTON  SUFFOLK       65+|M          374 BOSTON:yr2010:mn05:dow07
#> 4: 2010-05-01 BOSTON  SUFFOLK      0-17|F          378 BOSTON:yr2010:mn05:dow07
#> 5: 2010-05-01 BOSTON  SUFFOLK     18-64|F          365 BOSTON:yr2010:mn05:dow07
#> 6: 2010-05-01 BOSTON  SUFFOLK       65+|F          373 BOSTON:yr2010:mn05:dow07
#>    strata_total      match_strata
#>           <int>            <char>
#> 1:         2002 BOSTON:2010-05-01
#> 2:         1790 BOSTON:2010-05-01
#> 3:         1893 BOSTON:2010-05-01
#> 4:         1936 BOSTON:2010-05-01
#> 5:         1810 BOSTON:2010-05-01
#> 6:         1881 BOSTON:2010-05-01

## group
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9),
  grp_level = T,
  keep_unit_outcomes = F
)
#> > Combined factor is  age_grp|sex 
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9), : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date COUNTY20 age_grp|sex daily_deaths spatial_grp
#>        <IDat>   <char>      <char>        <int>      <char>
#> 1: 2010-05-01  SUFFOLK      0-17|M          408         ALL
#> 2: 2010-05-01  SUFFOLK     18-64|M          384         ALL
#> 3: 2010-05-01  SUFFOLK       65+|M          396         ALL
#> 4: 2010-05-01  SUFFOLK      0-17|F          400         ALL
#> 5: 2010-05-01  SUFFOLK     18-64|F          387         ALL
#> 6: 2010-05-01  SUFFOLK       65+|F          395         ALL
#>                       strata strata_total       match_strata
#>                       <char>        <int>             <char>
#> 1: SUFFOLK:yr2010:mn05:dow07         2115 SUFFOLK:2010-05-01
#> 2: SUFFOLK:yr2010:mn05:dow07         1891 SUFFOLK:2010-05-01
#> 3: SUFFOLK:yr2010:mn05:dow07         2000 SUFFOLK:2010-05-01
#> 4: SUFFOLK:yr2010:mn05:dow07         2046 SUFFOLK:2010-05-01
#> 5: SUFFOLK:yr2010:mn05:dow07         1914 SUFFOLK:2010-05-01
#> 6: SUFFOLK:yr2010:mn05:dow07         1988 SUFFOLK:2010-05-01

## group but keep unit outcomes
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9),
  grp_level = T,
  keep_unit_outcomes = T
)
#> > Combined factor is  age_grp|sex
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9), : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

head(outcomes_tbl)
#>          date TOWN20 COUNTY20 age_grp|sex daily_deaths                   strata
#>        <IDat> <char>   <char>      <char>        <int>                   <char>
#> 1: 2010-05-01 BOSTON  SUFFOLK      0-17|M          385 BOSTON:yr2010:mn05:dow07
#> 2: 2010-05-01 BOSTON  SUFFOLK     18-64|M          363 BOSTON:yr2010:mn05:dow07
#> 3: 2010-05-01 BOSTON  SUFFOLK       65+|M          374 BOSTON:yr2010:mn05:dow07
#> 4: 2010-05-01 BOSTON  SUFFOLK      0-17|F          378 BOSTON:yr2010:mn05:dow07
#> 5: 2010-05-01 BOSTON  SUFFOLK     18-64|F          365 BOSTON:yr2010:mn05:dow07
#> 6: 2010-05-01 BOSTON  SUFFOLK       65+|F          373 BOSTON:yr2010:mn05:dow07
#>    strata_total      match_strata spatial_grp
#>           <int>            <char>      <char>
#> 1:         2002 BOSTON:2010-05-01         ALL
#> 2:         1790 BOSTON:2010-05-01         ALL
#> 3:         1893 BOSTON:2010-05-01         ALL
#> 4:         1936 BOSTON:2010-05-01         ALL
#> 5:         1810 BOSTON:2010-05-01         ALL
#> 6:         1881 BOSTON:2010-05-01         ALL
```
