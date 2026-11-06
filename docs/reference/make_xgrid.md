# An internal function to make the xgrid

An internal function to make the xgrid

## Usage

``` r
make_xgrid(
  data,
  column_mapping,
  dt_by = "day",
  factor_is_spatial = FALSE,
  factor_is_temporal = FALSE
)
```

## Arguments

- data:

  a matrix of exposures or outcomes

- column_mapping:

  named list that indicates relevant columns in `data`.

- dt_by:

  either by day or by week

- factor_is_spatial:

  logical, is the collapse spatial

- factor_is_temporal:

  logical, is the collapse temporal

## Value

a datatable of all date and geo unit combinations

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
boston <- subset(ma_exposure, TOWN20 == "BOSTON" & date < as.Date("2010-02-01"))
cityClimateHealth:::make_xgrid(boston, exposure_columns)
#>            date tmax_C TOWN20 COUNTY20
#>          <IDat>  <num> <char>   <char>
#>   1: 2010-01-01   -0.4 BOSTON  SUFFOLK
#>   2: 2010-01-02    1.4 BOSTON  SUFFOLK
#>   3: 2010-01-03   -1.4 BOSTON  SUFFOLK
#>   4: 2010-01-04   -0.4 BOSTON  SUFFOLK
#>   5: 2010-01-05    0.7 BOSTON  SUFFOLK
#>  ---                                  
#> 361: 2010-12-27     NA BOSTON  SUFFOLK
#> 362: 2010-12-28     NA BOSTON  SUFFOLK
#> 363: 2010-12-29     NA BOSTON  SUFFOLK
#> 364: 2010-12-30     NA BOSTON  SUFFOLK
#> 365: 2010-12-31     NA BOSTON  SUFFOLK
```
