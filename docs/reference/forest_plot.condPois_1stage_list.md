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
#> Warning: `forest_plot` method not implemented for objects of class `condPois_1stage_list`,
#>       since there is only one 1_stage relative risk curve so all plot
#>       values would be the same. 1stage attributable number results will change
#>       over space, so those can be viewed instead by running `spatial_plot` on the
#>       output of `calcAN` for a 1stage model!
```
