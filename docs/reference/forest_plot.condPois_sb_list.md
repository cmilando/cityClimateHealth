# forest_plot method for condPois_sb_list

forest_plot method for condPois_sb_list

## Usage

``` r
# S3 method for class 'condPois_sb_list'
forest_plot(x, exposure_val)
```

## Arguments

- x:

  an object of class condPois_sb_list

- exposure_val:

  exposure value at which to plot

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
  # set up exposure matrix and outcome table first (see condPois_2stage example)
  model_list <- condPois_sb(ma_exposure_matrix, ma_outcomes_tbl, global_cen = 20)
  forest_plot(model_list, exposure_val = 30.0)
} # }
```
