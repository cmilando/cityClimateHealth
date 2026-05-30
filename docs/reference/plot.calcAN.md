# Plot method for calcAN

Plot method for calcAN

## Usage

``` r
# S3 method for class 'calcAN'
plot(x, table_type, above_MMT, spatial_sub = NULL, override_limit = FALSE)
```

## Arguments

- x:

  an object of class calcAN

- table_type:

  showing the rate table "rate" or number table "num"

- above_MMT:

  plot attributable numbers above or below the MMT

- spatial_sub:

  an option argument to subset the geo_units investigated

- override_limit:

  override the built-in plot limit

## Value

a plot of either the attributable rate or number

## Examples

``` r
if (FALSE) { # \dontrun{
# after running calcAN on a condPois model:
an_result <- calcAN(my_model)
plot(an_result, table_type = "num", above_MMT = TRUE)
} # }
```
