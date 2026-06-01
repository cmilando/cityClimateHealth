# spatial_plot method for condPois_sb_list

spatial_plot method for condPois_sb_list

## Usage

``` r
# S3 method for class 'condPois_sb_list'
spatial_plot(x, shp, exposure_val)
```

## Arguments

- x:

  an object of class condPois_sb_list

- shp:

  an sf shapefile with an appropriate column at which to join

- exposure_val:

  exposure value at which to plot

## Value

a patchwork of ggplot objects, one per factor level

## Examples

``` r
if (FALSE) { # \dontrun{
  # set up exposure matrix and outcome table first (see condPois_2stage example)
  model_list <- condPois_sb(ma_exposure_matrix, ma_outcomes_tbl, global_cen = 20)
  spatial_plot(model_list, shp = ma_shp, exposure_val = 30.0)
} # }
```
