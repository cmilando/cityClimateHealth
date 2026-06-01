# Gets the matrix of strata, used in `condPois_sb`

Gets the matrix of strata, used in `condPois_sb`

## Usage

``` r
getSmat(strata_vector)
```

## Arguments

- strata_vector:

  a factor or character vector of stratum labels

## Value

a binary integer matrix indicating stratum membership

## Examples

``` r
strata <- factor(c("a", "a", "b", "b", "c"))
getSmat(strata)
#> Error in getSmat(strata): could not find function "getSmat"
```
