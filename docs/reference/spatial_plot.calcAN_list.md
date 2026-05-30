# spatial_plot method for calcAN_list

spatial_plot method for calcAN_list

## Usage

``` r
# S3 method for class 'calcAN_list'
spatial_plot(x, shp, table_type, above_MMT)
```

## Arguments

- x:

  an object of class calcAN_list

- shp:

  an sf shapefile with an appropriate column at which to join

- table_type:

  showing the rate table "rate" or number table "num"

- above_MMT:

  plot attributable numbers above or below the MMT

## Value

a spatial plot showing attributable numbers or rates, with one graph for
each factor level

## Examples

``` r
if (FALSE) { # \dontrun{
# after running calcAN on a condPois model with a factor:
an_result <- calcAN(my_model_list)
spatial_plot(an_result, shp = my_shapefile, table_type = "num", above_MMT = TRUE)
} # }
```
