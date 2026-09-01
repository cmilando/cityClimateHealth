# Function to add US holidays

Function to add US holidays

## Usage

``` r
add_US_holiday(exposure_matrix)
```

## Arguments

- exposure_matrix:

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

exposure_matrix <- make_exposure_matrix(
  data = exp_data,
  column_mapping = exposure_columns,
  time_subset = list(year = 2012:2015)
)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

exposure_matrix <- add_us_holiday(exposure_matrix)
#> Error in add_us_holiday(exposure_matrix): could not find function "add_us_holiday"
```
