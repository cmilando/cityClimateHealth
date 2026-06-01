# Set the strata column of xgrid

Set the strata column of xgrid

## Usage

``` r
set_strata_value(xgrid, column_mapping, dt_by, grp_level, keep_unit)
```

## Arguments

- xgrid:

  the xgrid data.table

- column_mapping:

  column mapping vector

- dt_by:

  what is the date to aggregate by

- grp_level:

  logical, group level aggregatation

- keep_unit:

  logical, with or without unit level outcomes or exposures

## Value

a strata vector
