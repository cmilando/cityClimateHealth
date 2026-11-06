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

  proportion of `this_exp` to trim from each tail when bounding the
  centering point; 0 for no trimming

## Value

a list object, with the centered basis and centered crosspred output

## Examples

``` r
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
                                   outcome_columns, time_subset = time_subset)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
m <- condPois_1stage(exposure_mat, outcomes_tbl)
#> 
#> crossbasis args for geo_unit  BOSTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.6 31.9
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> BOSTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
fit <- m$`_`$out[[1]]
centered <- cityClimateHealth:::get_centered_cp(
  argvar = fit$argvar, xcoef = coef(fit$cr), xvcov = vcov(fit$cr),
  this_exp = fit$this_exp, x_b = c(floor(min(fit$this_exp)), ceiling(max(fit$this_exp))),
  global_cen = NULL, cen = fit$cen, exposure_is_factor = FALSE, truncate = 0.025)
names(centered)
#> [1] "cp"        "basis_cen"
```
