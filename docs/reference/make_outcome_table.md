# Function to create the outcome table TO DO : EDIT XGRID

Function to create the outcome table TO DO : EDIT XGRID

## Usage

``` r
make_outcome_table(
  data,
  column_mapping,
  time_subset,
  dt_by = "day",
  collapse_to = NULL,
  collapse_is_spatial = FALSE,
  collapse_is_temporal = FALSE,
  grp_level = FALSE,
  keep_unit_outcomes = FALSE
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

  the time period of interest for analysis, specified as years, months,
  or days must be specified by user - no default

- dt_by:

  is it daily data, or weekly or ...

- collapse_to:

  which factors to collapse across

- collapse_is_spatial:

  is collapse a spatial variable

- collapse_is_temporal:

  is collapse a temporal variable

- grp_level:

  whether to summarize to the group level or not (default)

- keep_unit_outcomes:

  if grp_level is true, whether to keep original unit-level outcomes

## Value

a data.table of class("outcome")

## Examples
