# Title

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
nb2graph(x)
```

## Arguments

- x:

## Examples

``` r
library(spdep)
#> Loading required package: spData
#> To access larger datasets in this package, install the spDataLarge
#> package with: `install.packages('spDataLarge',
#> repos='https://nowosad.github.io/drat/', type='source')`
#> Loading required package: sf
#> Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE
coords <- matrix(c(0,0, 1,0, 0,1, 1,1), ncol = 2, byrow = TRUE)
nb <- knn2nb(knearneigh(coords, k = 2))
#> Warning: k greater than one-third of the number of data points
nb2graph(nb)
#> Error in nb2graph(nb): could not find function "nb2graph"
```
