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
  "factor" = 'age_grp',
  "factor" = 'sex',
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
#> Warning in make_exposure_matrix(exposure, exposure_columns, grp_level = F, : check about any NA, some corrections for this later,
#>             but only in certain columns
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow



deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> > No factors to collapse to, using all data
#> > grp_level == FALSE, so using geo_unit as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with factor
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 collapse_to = 'age_grp',
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == FALSE, so using geo_unit as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$fctTOWN20 = deaths$TOWN20
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctTOWN20'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctTOWN20',
                                 collapse_is_spatial = T,
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == FALSE, so using geo_unit as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = wday(deaths$date) %in% c(6,7)
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctWeekend'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctWeekend',
                                 collapse_is_temporal = T,
                                 grp_level = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == FALSE, so using geo_unit as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

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
#> Warning in make_exposure_matrix(exposure, exposure_columns, grp_level = T, : check about any NA, some corrections for this later,
#>             but only in certain columns
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow



deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit_outcomes == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))


# with factor
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 collapse_to = 'age_grp',
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$fctTOWN20 = deaths$TOWN20
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctTOWN20'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctTOWN20',
                                 collapse_is_spatial = T,
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = wday(deaths$date) %in% c(6,7)
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctWeekend'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctWeekend',
                                 collapse_is_temporal = T,
                                 grp_level = T,
                                 keep_unit_outcomes = T,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == TRUE, so
#>             keeping to geo_unit data but using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

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
#> Warning in make_exposure_matrix(exposure, exposure_columns, grp_level = T, : check about any NA, some corrections for this later,
#>             but only in certain columns
#> strata dt_by = 'day', setting strata as geo_unit_grp:yr:mn:dow

deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > No factors to collapse to, using all data
#> > grp_level == TRUE and keep_unit_outcomes == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with factor
deaths_tbl <- make_outcome_table(deaths,  
                                 outcome_columns, 
                                 collapse_to = 'age_grp',
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with spatial collapse
deaths$fctTOWN20 = deaths$TOWN20
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctTOWN20'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctTOWN20',
                                 collapse_is_spatial = T,
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))

# with temporal collapse
deaths$fctWeekend = wday(deaths$date) %in% c(6,7)
tmp_cols <- outcome_columns
tmp_cols$factor2 = 'fctWeekend'
names(tmp_cols)[7] = 'factor'
deaths_tbl <- make_outcome_table(deaths,  
                                 tmp_cols, 
                                 collapse_to = 'fctWeekend',
                                 collapse_is_temporal = T,
                                 grp_level = T,
                                 keep_unit_outcomes = F,
                                 time_subset = list(month = 5:9))
#> > Factors in data
#> > grp_level == TRUE and keep_unit_outcomes == FALSE, so
#>             aggregating to geo_unit_grp and using geo_unit_grp as strata
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# checks
stopifnot(all(deaths_tbl$match_strata %in% exposure_mat$match_strata ))
stopifnot(all(deaths_tbl$strata %in% exposure_mat$strata ))
```
