# Function to add US holidays

Function to add US holidays

## Usage

``` r
add_us_holiday(outcome_table)
```

## Arguments

- outcome_table:

## Value

an outcome table object with an additional covariate

## Examples

``` r
exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

exp_data = subset(ma_exposure, COUNTY20 %in% c('MIDDLESEX', 'WORCESTER'))

outcome_table <- make_outcome_table(
  data = exp_data,
  column_mapping = exposure_columns,
  time_subset = list(year = 2012:2015)
)
#> Error in make_outcome_table(data = exp_data, column_mapping = exposure_columns,     time_subset = list(year = 2012:2015)): Names of column mapping is not one of the valid types:
#>           date, exposure, geo_unit, geo_unit_grp

outcome_table <- add_us_holiday(outcome_table)
#> Error: object 'outcome_table' not found
```
