# Title

Facilitates recursive calls get neighbors

## Usage

``` r
nx(xx, ni, include_self = T)
```

## Arguments

- xx:

  a list of neighbor indices (nb-style)

- ni:

  integer; neighbor order

- include_self:

  logical; whether to include each node itself

## Value

a list of neighbor index vectors

## Examples

``` r
xx <- list(c(2, 3), c(1, 3), c(1, 2))
cityClimateHealth:::nx(xx, ni = 1, include_self = TRUE)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 1 2 3
#> 
```
