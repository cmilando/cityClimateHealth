# Calculate the variance-covariance matrix of a Conditional poisson model

https://link.springer.com/article/10.1007/s11222-005-4069-4
https://statomics.github.io/SGA2019/assets/poissonIRWLS-implemented.html#variance-covariance-matrix-of-the-model-parameters

## Usage

``` r
calc_vcov(y, X, beta, stratum_vector)
```

## Arguments

- y:

  a vector of outcomes

- X:

  a matrix of predictors, typically the crossbasis output

- beta:

  a vector of coefficients

- stratum_vector:

  a vector describing the stratum

## Value

a numeric matrix, the variance-covariance matrix

## Details

and extended to multi-nomial case, with help from an LLM, confirmed it
works.

here are other refs https://doi.org/10.1111/j.2517-6161.1996.tb02079.x
https://doi.org/10.1093/biomet/68.2.563

## Examples

``` r
if (FALSE) { # \dontrun{
# after running condPois_1stage on a single geo_unit:
result <- condPois_1stage(exposure_matrix, outcomes_tbl)
vcov_mat <- calc_vcov(
  y = result$`_`$out[[1]]$outcomes,
  X = as.matrix(result$`_`$out[[1]]$orig_basis),
  beta = result$`_`$out[[1]]$orig_coef,
  stratum_vector = result$`_`$out[[1]]$strata_vec
)
} # }
```
