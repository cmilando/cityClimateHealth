# getRR method for condPois_2stage

getRR method for condPois_2stage

## Usage

``` r
# S3 method for class 'condPois_2stage'
getRR(x)
```

## Arguments

- x:

  an object of class condPois_2stage

## Value

a data.table of relative risk estimates

## Examples

``` r
if (FALSE) { # \dontrun{
  # set up exposure matrix and outcome table first (see condPois_2stage example)
  model <- condPois_2stage(ma_exposure_matrix, ma_outcomes_tbl, global_cen = 20)
  getRR(model)
} # }
```
