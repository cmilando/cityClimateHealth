# An internal function to make the xgrid

An internal function to make the xgrid

## Usage

``` r
make_xgrid(
  data,
  column_mapping,
  dt_by = "day",
  collapse_is_spatial = FALSE,
  collapse_is_temporal = FALSE
)
```

## Arguments

- data:

  a matrix of exposures or outcomes

- column_mapping:

  named list that indicates relevant columns in `data`.

- dt_by:

  either by day or by week

- collapse_is_spatial:

  logical, is the collapse spatial

- collapse_is_temporal:

  logical, is the collapse temporal

## Value

a datatable of all date and geo unit combinations

## Examples

``` r
if (FALSE) { # \dontrun{
exposure_columns <- list("date" = "date",
"exposure" = "tmax_C", "geo_unit" = "TOWN20",
"geo_unit_grp" = "COUNTY20")

make_xgrid(subset(ma_exposure, TOWN20 == 'BOSTON'),
exposure_columns, time_subset = list(month = 5:9))
} # }
```
