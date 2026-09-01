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
  exposure_is_factor,
  truncate
)
```

## Arguments

- argvar:

  the list of argvar elements used to determine the exposure dimension
  of `crossbasis`

- xcoef:

  the centered basis coefficients that resulted

- xvcov:

  the variance covariance matrix for the centered basis coefficients

- this_exp:

  a vector for the exposure

- x_b:

  a list that is the boundary of this_exp

- global_cen:

  a global centering point, if it exists

- cen:

  the local centering point

- exposure_is_factor:

  logical, if exposure is a factor

- truncate:

## Value

a list object, with the centered basis and centered crosspred output

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
