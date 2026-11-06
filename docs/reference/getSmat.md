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
cityClimateHealth:::getSmat(strata)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    0    0    0
#> [2,]    1    1    0    0    0
#> [3,]    0    0    1    1    0
#> [4,]    0    0    1    1    0
#> [5,]    0    0    0    0    1
```
