# spatial_plot base class

spatial_plot base class

## Usage

``` r
spatial_plot(x, ...)
```

## Arguments

- x:

  an object to dispatch to the appropriate spatial_plot S3 method

- ...:

  further arguments passed to the method

## Value

output depends on the class of x; see method-specific documentation

## Examples

``` r
if (FALSE) { # \dontrun{
# after running a condPois model:
result <- condPois_2stage(exposure_matrix, outcomes_tbl)
spatial_plot(result, shp = my_shapefile, exposure_val = 30.0)
} # }
```
