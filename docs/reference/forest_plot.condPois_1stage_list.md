# forest_plot method for condPois_1stage_list

forest_plot method for condPois_1stage_list

## Usage

``` r
# S3 method for class 'condPois_1stage_list'
forest_plot(x, ...)
```

## Arguments

- x:

  an object of class condPois_1stage_list

- ...:

  other elements passed to spatial_plot

## Value

called for its side-effect (warning); returns NULL invisibly

## Examples

``` r
x <- structure(list(a = 1, b = 2), class = "condPois_1stage_list")
forest_plot(x)
#> Error in x[[names(x)[1]]]$`_`: $ operator is invalid for atomic vectors
```
