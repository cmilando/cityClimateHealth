# Compute scaling factors for neighbor list components

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
scale_nb_components(x)
```

## Arguments

- x:

  an nb object (neighbor list)

## Value

a numeric vector of scaling factors, one per connected component

## Examples

``` r
nb <- spdep::cell2nb(3, 3)
cityClimateHealth:::scale_nb_components(nb)
#> [1] 0.409709
```
