# Calculate attributable number and attributable rates

Calculate attributable number and attributable rates

## Usage

``` r
calc_AN(
  model,
  outcomes_tbl,
  pop_data,
  spatial_agg_type,
  spatial_join_col,
  by_year = FALSE,
  nsim = 300,
  verbose = 0
)
```

## Arguments

- model:

  a model object of class `condPois_2stage`, `condPois_1stage`, or
  `condPois_sb`

- outcomes_tbl:

  a table of outcomes, of class `outcomes`

- pop_data:

  population data

- spatial_agg_type:

  what is the spatial resolution you are aggregating to

- spatial_join_col:

  how should you join population data to the outcome table

- by_year:

  Export annual counts

- nsim:

  number of simulations required for calculation of empirical CI
  (default = 300)

- verbose:

  0 = no printing, 1 = headers, 2 = detailed

## Value

calc_AN object that contains attributable numbers, rate tables, and
other components necessary for plotting AN

## Examples
