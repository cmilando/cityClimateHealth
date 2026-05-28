# Title

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
scale_nb_components(x)
```

## Arguments

- x:

## Examples

``` r
library(spdep)
coords <- matrix(c(0,0, 1,0, 0,1, 1,1), ncol = 2, byrow = TRUE)
nb <- knn2nb(knearneigh(coords, k = 2))
#> Warning: k greater than one-third of the number of data points
scale_nb_components(nb)
#> Error in scale_nb_components(nb): could not find function "scale_nb_components"
```
