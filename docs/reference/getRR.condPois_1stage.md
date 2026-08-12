# getRR method for condPois_1stage

getRR method for condPois_1stage

## Usage

``` r
# S3 method for class 'condPois_1stage'
getRR(x)
```

## Arguments

- x:

  an object of class condPois_1stage

## Value

a data.table of relative risk estimates

## Examples

``` r
# create exposure matrix
exposure_columns <- list(
 "date" = "date",
 "exposure" = "tmax_C",
 "geo_unit" = "TOWN20",
)
#> Error in list(date = "date", exposure = "tmax_C", geo_unit = "TOWN20",     ): argument 4 is empty
middlesex_exposure <- subset(ma_exposure, COUNTY20 == 'MIDDLESEX')
middlesex_exposure_mat <- make_exposure_matrix(middlesex_exposure, exposure_columns)
#> Error: object 'exposure_columns' not found
# create outcome table
outcome_columns <- list(
 "date" = "date",
 "outcome" = "daily_deaths",
 "factor" = 'age_grp',
 "factor" = 'sex',
 "geo_unit" = "TOWN20",
 "geo_unit_grp" = "COUNTY20"
)
middlesex_deaths   <- subset(ma_deaths, COUNTY20 == 'MIDDLESEX')
middlesex_deaths_tbl <- make_outcome_table(middlesex_deaths,  outcome_columns)
#> Error in time_subset_validate(time_subset, data_years): A `time_subset` must be explicitly provided, e.g. list(month = 5:9).
#>     To indicate using all available time, put time_subset = 'use_all'
# run the model
m2 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
outcomes_tbl = middlesex_deaths_tbl, multi_zone = TRUE,
global_cen = 15)
#> Error: object 'middlesex_exposure_mat' not found
getRR(m2)
#> Error: object 'm2' not found
```
