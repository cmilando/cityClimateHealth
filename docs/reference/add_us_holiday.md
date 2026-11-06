# Function to add US holidays

Function to add US holidays

## Usage

``` r
add_US_holiday(exposure_matrix)
```

## Arguments

- exposure_matrix:

  an exposure matrix of class `exposure`, from
  [`make_exposure_matrix()`](https://climatehealth.city/reference/make_exposure_matrix.md)

## Value

an outcome table object with an additional covariate

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
exposure_mat <- add_US_holiday(exposure_mat)
head(exposure_mat)
#>          date TOWN20 COUNTY20 tmax_C                   strata      match_strata
#>        <IDat> <char>   <char>  <num>                   <char>            <char>
#> 1: 2012-05-01 BOSTON  SUFFOLK   17.1 BOSTON:yr2012:mn05:dow03 BOSTON:2012-05-01
#> 2: 2012-05-02 BOSTON  SUFFOLK    8.6 BOSTON:yr2012:mn05:dow04 BOSTON:2012-05-02
#> 3: 2012-05-03 BOSTON  SUFFOLK   11.7 BOSTON:yr2012:mn05:dow05 BOSTON:2012-05-03
#> 4: 2012-05-04 BOSTON  SUFFOLK   12.4 BOSTON:yr2012:mn05:dow06 BOSTON:2012-05-04
#> 5: 2012-05-05 BOSTON  SUFFOLK   12.7 BOSTON:yr2012:mn05:dow07 BOSTON:2012-05-05
#> 6: 2012-05-06 BOSTON  SUFFOLK   17.8 BOSTON:yr2012:mn05:dow01 BOSTON:2012-05-06
#>    explag1 explag2 explag3 explag4 explag5 is_holiday
#>      <num>   <num>   <num>   <num>   <num>     <lgcl>
#> 1:    14.3    14.3    12.7    18.3    15.8      FALSE
#> 2:    17.1    14.3    14.3    12.7    18.3      FALSE
#> 3:     8.6    17.1    14.3    14.3    12.7      FALSE
#> 4:    11.7     8.6    17.1    14.3    14.3      FALSE
#> 5:    12.4    11.7     8.6    17.1    14.3      FALSE
#> 6:    12.7    12.4    11.7     8.6    17.1      FALSE
```
