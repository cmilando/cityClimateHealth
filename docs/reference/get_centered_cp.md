# Internal function to get centered cp objects

Needed a function for this because we do it twice: once for regional RRs
and once for BLUP

## Usage

``` r
get_centered_cp(
  argvar,
  xcoef,
  xvcov,
  this_exp,
  x_b,
  global_cen,
  cen,
  exposure_is_factor
)
```

## Arguments

- exposure_is_factor:

## Examples

``` r
if (FALSE) { # \dontrun{
# after running condPois_1stage on a single geo_unit:
result <- condPois_1stage(exposure_matrix, outcomes_tbl)
centered <- get_centered_cp(
  argvar = result$`_`$out[[1]]$argvar,
  xcoef = result$`_`$out[[1]]$coef,
  xvcov = result$`_`$out[[1]]$vcov,
  this_exp = result$`_`$out[[1]]$this_exp,
  x_b = c(0, 40),
  global_cen = NULL,
  cen = result$`_`$out[[1]]$cen,
  exposure_is_factor = FALSE
)
} # }
```
