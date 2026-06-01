# Plot method for calcAN_list

Plot method for calcAN_list

## Usage

``` r
# S3 method for class 'calcAN_list'
plot(
  x,
  table_type,
  above_MMT,
  spatial_sub = NULL,
  ordered_levels = NULL,
  override_limit = FALSE
)
```

## Arguments

- x:

  an object of class calcAN_list

- table_type:

  showing the rate table "rate" or number table "num"

- above_MMT:

  plot attributable numbers above or below the MMT

- spatial_sub:

  an option argument to subset the geo_units investigated

- ordered_levels:

  factor levels

- override_limit:

  override the built-in plot limit

## Value

a plot with the attributable number or rate with bars for each factor
level

## Examples

``` r
if (FALSE) { # \dontrun{
# after running calcAN on a condPois model with a factor:
an_result <- calcAN(my_model_list)
plot(an_result, table_type = "num", above_MMT = TRUE)
} # }
```
