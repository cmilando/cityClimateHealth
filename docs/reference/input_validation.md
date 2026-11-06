# Input validation

Input validation

## Usage

``` r
input_validation(exposure_matrix, outcomes_tbl)
```

## Arguments

- exposure_matrix:

  an exposure matrix object of class "exposure"

- outcomes_tbl:

  an outcomes table object of class "outcome"

## Value

a list with validated `exposure_matrix` and `outcomes_tbl`

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
                                   outcome_columns, time_subset = time_subset)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
checked <- input_validation(exposure_mat, outcomes_tbl)
names(checked)
#> [1] "outcomes_tbl"    "exposure_matrix"
```
