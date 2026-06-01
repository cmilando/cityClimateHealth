# Index nodes by connected component

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
indexByComponent(x)
```

## Arguments

- x:

  a vector of component ids

## Value

a vector of per-component consecutive node ids

## Examples

``` r
comp_ids <- c(1, 1, 2, 2, 2, 1)
indexByComponent(comp_ids)
#> Error in indexByComponent(comp_ids): could not find function "indexByComponent"
```
