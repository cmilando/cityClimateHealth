# make factor column

make factor column

## Usage

``` r
add_factor_col(data, column_mapping)
```

## Arguments

- data:

  a data.table containing the columns named in `column_mapping`

- column_mapping:

  a named list mapping column roles to column names in `data`; all
  `factor` entries are combined into a single factor column
