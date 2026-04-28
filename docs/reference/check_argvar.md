# Internal function to check argvar

Internal function to check argvar

## Usage

``` r
check_argvar(argvar, this_exp)
```

## Arguments

- argvar:

  the argvar argument that is passed in

- this_exp:

  the exposure vector

## Value

A validated version of `argvar`, potentially coerced into a standard
form (e.g., a symbol or expression). Throws an error if validation
fails.

## Details

this function is not meant to be run directly by the user

## Examples

``` r
set.seed(123)
argvar_raw = NULL      #
this_exp = rnorm(100)  # some random vector of exposures

check_argvar(argvar_raw, this_exp)
#> Error in check_argvar(argvar_raw, this_exp): could not find function "check_argvar"
```
