# join exposure covariate

join exposure covariate

## Usage

``` r
join_exposure_covariate(exposure_mat, covariate_mat)
```

## Arguments

- exposure_mat:

  an exposure matrix of class `exposure`, from
  [`make_exposure_matrix()`](https://climatehealth.city/reference/make_exposure_matrix.md)

- covariate_mat:

  a covariate matrix (with a `column_mapping` attribute) to join onto
  `exposure_mat`
