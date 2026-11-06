# collapse the data based on factors and group level strata

collapse the data based on factors and group level strata

## Usage

``` r
collapse_data(data, column_mapping, fcn, data_type, grp_level, keep_unit)
```

## Arguments

- data:

  a data.table of exposures or outcomes

- column_mapping:

  a named list mapping column roles to column names in `data`

- fcn:

  function used to aggregate the exposure or outcome column, e.g. `sum`
  or `mean`

- data_type:

  either `'exposure'` or `'outcome'`; selects which column is aggregated

- grp_level:

  logical; if `TRUE`, aggregate to `geo_unit_grp`

- keep_unit:

  logical; if `TRUE`, keep unit-level rows when `grp_level` is `TRUE`
