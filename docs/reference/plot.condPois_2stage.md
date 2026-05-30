# Plot method for condPois_2stage

Plot method for condPois_2stage

## Usage

``` r
# S3 method for class 'condPois_2stage'
plot(x, geo_unit, xlab = NULL, ylab = NULL, title = NULL)
```

## Arguments

- x:

  an object of class condPois_2stage

- geo_unit:

  a geo_unit to investigate

- xlab:

  xlab override

- ylab:

  ylab override

- title:

  title override

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
  # set up exposure matrix and outcome table first (see condPois_2stage example)
  model <- condPois_2stage(ma_exposure_matrix, ma_outcomes_tbl, global_cen = 20)
  plot(model, geo_unit = "BOSTON")
} # }
```
