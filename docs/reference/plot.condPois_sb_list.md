# Plot method for condPois_sb_list

Plot method for condPois_sb_list

## Usage

``` r
# S3 method for class 'condPois_sb_list'
plot(x, geo_unit, xlab = NULL, ylab = NULL, title = NULL)
```

## Arguments

- x:

  an object of class condPois_sb_list

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
  model_list <- condPois_sb(ma_exposure_matrix, ma_outcomes_tbl, global_cen = 20)
  plot(model_list, geo_unit = "BOSTON")
} # }
```
