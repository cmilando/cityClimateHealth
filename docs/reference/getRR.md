# getRR base class

getRR base class

## Usage

``` r
getRR(x, ...)
```

## Arguments

- x:

  an object to dispatch to the appropriate getRR S3 method

- ...:

  further arguments passed to the method

## Value

a data.table of relative risk estimates; exact format depends on the
class of x

## Examples

``` r
if (FALSE) { # \dontrun{
# after running a condPois model:
result <- condPois_2stage(exposure_matrix, outcomes_tbl)
getRR(result)
} # }
```
