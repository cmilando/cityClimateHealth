# Function to create the outcome table

Function to create the outcome table

## Usage

``` r
make_outcome_table(
  data,
  column_mapping,
  time_subset,
  dt_by = "day",
  factor_is_spatial = FALSE,
  factor_is_temporal = FALSE,
  grp_level = FALSE,
  keep_unit_outcomes = FALSE,
  population_data = NULL,
  population_columns = NULL,
  population_col_name = "population"
)
```

## Arguments

- data:

  a dataset of outcomes

- column_mapping:

  a named list that indicates relevant columns in `data`. for the
  exposure data table, these need to be one of: c('date',
  "outcome",'factor, 'geo_unit', 'geo_unit_grp')

- time_subset:

  the time period of interest for analysis, specified as years or months
  must be specified by user - no default

- dt_by:

  is it daily data, or weekly or ...

- factor_is_spatial:

  is collapse a spatial variable

- factor_is_temporal:

  is collapse a temporal variable

- grp_level:

  whether to summarize to the group level or not (default)

- keep_unit_outcomes:

  if grp_level is true, whether to keep original unit-level outcomes

- population_data:

  optional data.frame of population estimates to join on as a population
  column; see
  [`add_population_offset()`](https://climatehealth.city/reference/add_population_offset.md)

- population_columns:

  named list mapping columns in `population_data`; required with
  `population_data`. See
  [`add_population_offset()`](https://climatehealth.city/reference/add_population_offset.md)

- population_col_name:

  name of the population column added to the output (default
  `"population"`)

## Value

a data.table of class("outcome")

## Examples

``` r
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = 'age_grp',
  "factor" = 'sex',
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
ma_outcomes_tbl <- make_outcome_table(
  subset(ma_deaths, TOWN20 == "BOSTON"),
  outcome_columns,
  time_subset = list(year = 2012)
)
#> > Combined factor is  age_grp|sex 
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
head(ma_outcomes_tbl)
#>          date TOWN20 COUNTY20 age_grp|sex daily_deaths                   strata
#>        <IDat> <char>   <char>      <char>        <int>                   <char>
#> 1: 2012-01-01 BOSTON  SUFFOLK      0-17|M            0 BOSTON:yr2012:mn01:dow01
#> 2: 2012-01-01 BOSTON  SUFFOLK     18-64|M            0 BOSTON:yr2012:mn01:dow01
#> 3: 2012-01-01 BOSTON  SUFFOLK       65+|M            0 BOSTON:yr2012:mn01:dow01
#> 4: 2012-01-01 BOSTON  SUFFOLK      0-17|F            0 BOSTON:yr2012:mn01:dow01
#> 5: 2012-01-01 BOSTON  SUFFOLK     18-64|F            0 BOSTON:yr2012:mn01:dow01
#> 6: 2012-01-01 BOSTON  SUFFOLK       65+|F            0 BOSTON:yr2012:mn01:dow01
#>    strata_total      match_strata
#>           <int>            <char>
#> 1:            0 BOSTON:2012-01-01
#> 2:            0 BOSTON:2012-01-01
#> 3:            0 BOSTON:2012-01-01
#> 4:            0 BOSTON:2012-01-01
#> 5:            0 BOSTON:2012-01-01
#> 6:            0 BOSTON:2012-01-01
```
