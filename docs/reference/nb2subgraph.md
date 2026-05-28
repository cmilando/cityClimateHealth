# Title

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
nb2subgraph(x, c_id, comp_ids, offsets)
```

## Arguments

- offsets:

## Examples

``` r
library(spdep)
coords <- matrix(c(0,0, 1,0, 0,1, 1,1), ncol = 2, byrow = TRUE)
nb <- knn2nb(knearneigh(coords, k = 2))
#> Warning: k greater than one-third of the number of data points
comp_ids <- n.comp.nb(nb)[[2]]
offsets <- indexByComponent(comp_ids)
#> Error in indexByComponent(comp_ids): could not find function "indexByComponent"
nb2subgraph(nb, c_id = 1, comp_ids = comp_ids, offsets = offsets)
#> Error in nb2subgraph(nb, c_id = 1, comp_ids = comp_ids, offsets = offsets): could not find function "nb2subgraph"
```
