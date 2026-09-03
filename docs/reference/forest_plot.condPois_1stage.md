# forest_plot method for condPois_1stage

forest_plot method for condPois_1stage

## Usage

``` r
# S3 method for class 'condPois_1stage'
forest_plot(x, ...)
```

## Arguments

- x:

  an object of class condPois_1stage

- ...:

  other elements passed to spatial_plot

## Value

called for its side-effect (warning); returns NULL invisibly

## Examples

``` r
x <- structure(list(), class = "condPois_1stage")
forest_plot(x)
#> Error in data.frame(x = x$`_`$out[[1]]$cr$predvar, RR = x$`_`$out[[1]]$cr$RRfit,     RRlb = x$`_`$out[[1]]$cr$RRlow, RRub = x$`_`$out[[1]]$cr$RRhigh,     n_geo_names = n_geo_names, model_class = class(x)): arguments imply differing number of rows: 0, 1
```
