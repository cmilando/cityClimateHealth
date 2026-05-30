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
expand_arglist(arglist)
#> Error in expand_arglist(arglist): could not find function "expand_arglist"
```
