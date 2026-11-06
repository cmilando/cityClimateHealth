# Make crossbasis object

Make crossbasis object

## Usage

``` r
make_crossbasis(
  exposure_matrix,
  outcomes_tbl,
  unique_geos,
  maxlag,
  argvar,
  arglag,
  strata_min,
  min_n,
  verbose
)
```

## Arguments

- exposure_matrix:

  an exposure matrix of class `exposure`, from
  [`make_exposure_matrix()`](https://climatehealth.city/reference/make_exposure_matrix.md)

- outcomes_tbl:

  an outcome table of class `outcome`, from
  [`make_outcome_table()`](https://climatehealth.city/reference/make_outcome_table.md)

- unique_geos:

  character vector of the geo_units being modelled

- maxlag:

  integer, the maximum lag

- argvar:

  a list of `argvar` arguments for
  [`dlnm::crossbasis()`](https://rdrr.io/pkg/dlnm/man/crossbasis.html)

- arglag:

  a list of `arglag` arguments for
  [`dlnm::crossbasis()`](https://rdrr.io/pkg/dlnm/man/crossbasis.html)

- strata_min:

  minimum number of cases for a stratum to be kept

- min_n:

  minimum number of cases for a geo_unit

- verbose:

  logical or integer; if truthy, print the crossbasis settings
