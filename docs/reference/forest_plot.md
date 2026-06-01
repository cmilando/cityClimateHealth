# forest_plot base class

forest_plot base class

## Usage

``` r
forest_plot(x, ...)
```

## Arguments

- x:

  an object to dispatch to the appropriate forest_plot S3 method

- ...:

  further arguments passed to the method

## Value

output depends on the class of x; see method-specific documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# after running a condPois model:
result <- condPois_2stage(exposure_matrix, outcomes_tbl)
forest_plot(result, exposure_val = 30.0)
} # }
```
