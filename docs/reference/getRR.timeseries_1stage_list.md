# getRR method for timeseries_1stage_list

getRR method for timeseries_1stage_list

## Usage

``` r
# S3 method for class 'timeseries_1stage_list'
getRR(x, ...)
```

## Arguments

- x:

  an object of class timeseries_1stage_list

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates across factor levels

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "factor" = "age_grp",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
                                   outcome_columns, time_subset = time_subset)
#> > Combined factor is  age_grp 
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
# with a factor, a timeseries_1stage_list is returned
m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#> < age_grp : 0-17 >
#> Error in timeseries_1stage_glm(exposure_matrix = exposure_matrix, outcomes_tbl = subset_outcomes_tbl,     global_cen = global_cen, argvar = argvar, arglag = arglag,     maxlag = maxlag, min_n = min_n, strata_min = strata_min,     time_trend = time_trend, df_per_year = df_per_year, df_time = df_time,     add_dow = add_dow, population_col = population_col, multi_zone = multi_zone,     verbose = verbose, truncate = truncate): unused argument (df_time = df_time)
getRR(m)
#> Error: object 'm' not found
```
