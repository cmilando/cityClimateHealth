# Calculate the dispersion parameter for a quasi-poisson model

Converted from STATA from armstrong 2014 supplement with LLM guidance.
Minimal error checking of these inputs at present.

## Usage

``` r
calc_dispersion(y, X, beta, stratum_vector)
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

## Examples

``` r
dispersion <- calc_dispersion(y = boston_deaths_tbl$daily_deaths,
              X = cb,
              stratum_vector = boston_deaths_tbl$strata,
              beta = coef(m_sub))
#> Error: object 'cb' not found
dispersion
#> Error: object 'dispersion' not found
```
