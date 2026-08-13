# covariate

``` r

library(cityClimateHealth)
```

No covariates are added to the exposure matrix, this is just the
exposures

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
```

Ok now you can add covariates to the outcome This includes environmental
covariates

``` r


## *** (1) you could add it before in exposure columns ***
## add covariate for something else, like is 2010
# * assume the user 
ma_deaths$is_2010 <- data.table::year(ma_deaths$date) == 2010

##
# create outcome table
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = c('age_grp', 'sex'),
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20",
  "covariate" = "is_2010"
)

boston_deaths <- subset(ma_deaths, TOWN20 %in% c('BOSTON', 'CHELSEA'))

## basic
outcomes_tbl <- make_outcome_table(
  boston_deaths,  
  outcome_columns,
  time_subset = list(month = 5:9)
)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(boston_deaths, outcome_columns, time_subset = list(month = 5:9)): 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.
head(outcomes_tbl)
#>          date TOWN20 COUNTY20 age_grp|sex is_2010 daily_deaths
#>        <IDat> <char>   <char>      <char>  <lgcl>        <int>
#> 1: 2010-05-01 BOSTON  SUFFOLK      0-17|M    TRUE          385
#> 2: 2010-05-01 BOSTON  SUFFOLK     18-64|M    TRUE          363
#> 3: 2010-05-01 BOSTON  SUFFOLK       65+|M    TRUE          374
#> 4: 2010-05-01 BOSTON  SUFFOLK      0-17|F    TRUE          378
#> 5: 2010-05-01 BOSTON  SUFFOLK     18-64|F    TRUE          365
#> 6: 2010-05-01 BOSTON  SUFFOLK       65+|F    TRUE          373
#>                      strata strata_total      match_strata
#>                      <char>        <int>            <char>
#> 1: BOSTON:yr2010:mn05:dow07         2002 BOSTON:2010-05-01
#> 2: BOSTON:yr2010:mn05:dow07         1790 BOSTON:2010-05-01
#> 3: BOSTON:yr2010:mn05:dow07         1893 BOSTON:2010-05-01
#> 4: BOSTON:yr2010:mn05:dow07         1936 BOSTON:2010-05-01
#> 5: BOSTON:yr2010:mn05:dow07         1810 BOSTON:2010-05-01
#> 6: BOSTON:yr2010:mn05:dow07         1881 BOSTON:2010-05-01
```

Special case of adding US holidays

``` r


outcomes_tbl <- add_us_holiday(outcomes_tbl)
head(outcomes_tbl)
#>          date TOWN20 COUNTY20 age_grp|sex is_2010 daily_deaths
#>        <IDat> <char>   <char>      <char>  <lgcl>        <int>
#> 1: 2010-05-01 BOSTON  SUFFOLK      0-17|M    TRUE          385
#> 2: 2010-05-01 BOSTON  SUFFOLK     18-64|M    TRUE          363
#> 3: 2010-05-01 BOSTON  SUFFOLK       65+|M    TRUE          374
#> 4: 2010-05-01 BOSTON  SUFFOLK      0-17|F    TRUE          378
#> 5: 2010-05-01 BOSTON  SUFFOLK     18-64|F    TRUE          365
#> 6: 2010-05-01 BOSTON  SUFFOLK       65+|F    TRUE          373
#>                      strata strata_total      match_strata is_holiday
#>                      <char>        <int>            <char>     <lgcl>
#> 1: BOSTON:yr2010:mn05:dow07         2002 BOSTON:2010-05-01      FALSE
#> 2: BOSTON:yr2010:mn05:dow07         1790 BOSTON:2010-05-01      FALSE
#> 3: BOSTON:yr2010:mn05:dow07         1893 BOSTON:2010-05-01      FALSE
#> 4: BOSTON:yr2010:mn05:dow07         1936 BOSTON:2010-05-01      FALSE
#> 5: BOSTON:yr2010:mn05:dow07         1810 BOSTON:2010-05-01      FALSE
#> 6: BOSTON:yr2010:mn05:dow07         1881 BOSTON:2010-05-01      FALSE

attributes(outcomes_tbl)$column_mapping
#> $date
#> [1] "date"
#> 
#> $outcome
#> [1] "daily_deaths"
#> 
#> $factor
#> [1] "age_grp|sex"
#> 
#> $geo_unit
#> [1] "TOWN20"
#> 
#> $geo_unit_grp
#> [1] "COUNTY20"
#> 
#> $covariate
#> [1] "is_2010"    "is_holiday"
```

And then two other cases: \* of adding a single variable after \* and
adding a crosbasis variable after
