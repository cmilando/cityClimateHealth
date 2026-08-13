# Group level aggregations check

``` r

library(data.table)
library(cityClimateHealth)

data("ma_exposure")
data("ma_deaths")

exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

TOWNLIST <- c('CHELSEA', 'EVERETT', 'REVERE', 'MALDEN')

exposure <- subset(ma_exposure, TOWN20 %in%  TOWNLIST)

# create outcome table
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths   <- subset(ma_deaths, TOWN20 %in%  TOWNLIST)
```

### Check 1

grp_level = False

``` r

exposure_mat <- make_exposure_matrix(exposure, 
                                     exposure_columns, 
                                     grp_level = F,
                                     time_subset = list(month = 5:9))
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow



deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = F, time_subset = list(month = 5:9)): 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with factor
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "age_grp",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = F, time_subset = list(month = 5:9)): 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$isCHELSEA = ifelse(deaths$TOWN20 == 'CHELSEA', "isChelsea", "notChelsea")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "isCHELSEA",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_spatial = T,
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_spatial = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = ifelse(wday(deaths$date) %in% c(6,7), "weekend", "weekday")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "fctWeekend",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_temporal = T,
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_temporal = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))
```

### Check 2

grp_level = True and keep unit-level exposures and outcomes

``` r

exposure_mat <- make_exposure_matrix(exposure, 
                                     exposure_columns, 
                                     grp_level = T,
                                     keep_unit_exposures = T,
                                     time_subset = list(month = 5:9))
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow



deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = T, keep_unit_outcomes = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))


# with factor
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "age_grp",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = T, keep_unit_outcomes = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$isCHELSEA = ifelse(deaths$TOWN20 == 'CHELSEA', "isChelsea", "notChelsea")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "isCHELSEA",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_spatial = T,
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_spatial = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = ifelse(wday(deaths$date) %in% c(6,7), "weekend", "weekday")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "fctWeekend",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_temporal = T,
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_temporal = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))
```

### Check 3

grp_level = True and don’t keep unit-level exposures and outcomes

``` r

exposure_mat <- make_exposure_matrix(exposure, 
                                     exposure_columns, 
                                     grp_level = T,
                                     keep_unit_exposures = F,
                                     time_subset = list(month = 5:9))
#> -- NA values automatically removed
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = T, keep_unit_outcomes = F, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with factor
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "age_grp",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, grp_level = T, keep_unit_outcomes = F, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$isCHELSEA = ifelse(deaths$TOWN20 == 'CHELSEA', "isChelsea", "notChelsea")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "isCHELSEA",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_spatial = T,
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_spatial = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = ifelse(wday(deaths$date) %in% c(6,7), "weekend", "weekday")
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = "fctWeekend",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 factor_is_temporal = T,
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > grp_level == TRUE and keep_unit == FALSE, so
#>         aggregating to geo_unit_grp and using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
#> Warning in make_outcome_table(deaths, outcome_columns, factor_is_temporal = T, : 2020 in data years, Outcome counts likely impacted by the
#>             COVID-19 Pandemic. Be sure to include a covariate adjustment
#>             or exclude this year from analysis.

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))
```
