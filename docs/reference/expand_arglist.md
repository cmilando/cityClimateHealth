# IN-PROGRESS functions that creates a list of cross-basis objects to try

IN-PROGRESS functions that creates a list of cross-basis objects to try

## Usage

``` r
expand_arglist(arglist)
```

## Arguments

- arglist:

  a list of argument lists to expand

## Value

a list of all combinations of the input arguments

## Examples

``` r
arglist <- list(
  list(fun = "ns", df = c(3, 4)),
  list(fun = "bs", degree = c(2, 3))
)
cityClimateHealth:::expand_arglist(arglist)
#> [[1]]
#> [[1]]$fun
#> [1] "ns"
#> 
#> [[1]]$df
#> [1] 3
#> 
#> 
#> [[2]]
#> [[2]]$fun
#> [1] "ns"
#> 
#> [[2]]$df
#> [1] 4
#> 
#> 
#> [[3]]
#> [[3]]$fun
#> [1] "bs"
#> 
#> [[3]]$degree
#> [1] 2
#> 
#> 
#> [[4]]
#> [[4]]$fun
#> [1] "bs"
#> 
#> [[4]]$degree
#> [1] 3
#> 
#> 
```
