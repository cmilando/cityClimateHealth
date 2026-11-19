# Plot method for timeseries_1stage

Plot method for timeseries_1stage

## Usage

``` r
# S3 method for class 'timeseries_1stage'
plot(x, xlab = NULL, ylab = NULL, title = NULL, ...)
```

## Arguments

- x:

  an object of class timeseries_1stage

- xlab:

  xlab override

- ylab:

  ylab override

- title:

  title override

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a ggplot object

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
m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#> strata:
#> BOSTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> Error in timeseries_1stage_glm(exposure_mat, outcomes_tbl): `df_per_year` cannot be null. For Annual, GASPARRINI uses 8. For Seasonal, GASPARRINI uses 4
plot(m)
#> Error: object 'm' not found
```
