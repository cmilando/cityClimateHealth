# Calculate the variance-covariance matrix of a Conditional poisson model

https://link.springer.com/article/10.1007/s11222-005-4069-4
https://statomics.github.io/SGA2019/assets/poissonIRWLS-implemented.html#variance-covariance-matrix-of-the-model-parameters

## Usage

``` r
calc_vcov(y, X, beta, stratum_vector)
```

## Arguments

- y:

  a vector of outcomes

- X:

  a matrix of predictors, typically the crossbasis output

- beta:

  a vector of coefficients

- stratum_vector:

  a vector describing the stratum

## Value

a numeric matrix, the variance-covariance matrix

## Details

and extended to multi-nomial case, with help from an LLM, confirmed it
works.

here are other refs https://doi.org/10.1111/j.2517-6161.1996.tb02079.x
https://doi.org/10.1093/biomet/68.2.563

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
calc_vcov(y = fit$outcomes, X = as.matrix(fit$orig_basis),
          beta = fit$orig_coef, stratum_vector = fit$strata_vec)
#>                [,1]          [,2]          [,3]          [,4]          [,5]
#>  [1,]  2.250658e-04 -1.262486e-04 -6.847833e-05  4.512025e-05  4.220339e-04
#>  [2,] -1.262486e-04  2.094289e-04 -2.895417e-06 -4.427415e-05 -2.130393e-04
#>  [3,] -6.847833e-05 -2.895417e-06  6.788002e-05 -2.708596e-05 -1.486679e-04
#>  [4,]  4.512025e-05 -4.427415e-05 -2.708596e-05  7.872669e-05  8.023775e-05
#>  [5,]  4.220339e-04 -2.130393e-04 -1.486679e-04  8.023775e-05  1.920088e-03
#>  [6,] -2.616243e-04  3.726437e-04  5.336917e-06 -8.356130e-05 -1.060069e-03
#>  [7,] -1.341343e-04 -2.449864e-05  1.069028e-04 -2.818100e-05 -5.756462e-04
#>  [8,]  9.855844e-05 -9.769123e-05 -4.457140e-05  1.077853e-04  3.396914e-04
#>  [9,]  8.927599e-05 -5.085091e-05 -2.938316e-05  1.945930e-05  3.863380e-04
#> [10,] -5.019059e-05  7.885293e-05 -2.571366e-06 -2.110306e-05 -1.886558e-04
#> [11,] -3.282484e-05 -4.620921e-06  1.975373e-05 -2.418327e-06 -1.335190e-04
#> [12,]  2.080401e-05 -2.178969e-05 -7.742987e-06  1.596646e-05  5.806627e-05
#>                [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -2.616243e-04 -1.341343e-04  9.855844e-05  8.927599e-05 -5.019059e-05
#>  [2,]  3.726437e-04 -2.449864e-05 -9.769123e-05 -5.085091e-05  7.885293e-05
#>  [3,]  5.336917e-06  1.069028e-04 -4.457140e-05 -2.938316e-05 -2.571366e-06
#>  [4,] -8.356130e-05 -2.818100e-05  1.077853e-04  1.945930e-05 -2.110306e-05
#>  [5,] -1.060069e-03 -5.756462e-04  3.396914e-04  3.863380e-04 -1.886558e-04
#>  [6,]  1.800223e-03 -6.035226e-05 -3.799009e-04 -2.410134e-04  3.850823e-04
#>  [7,] -6.035226e-05  6.163791e-04 -2.048768e-04 -1.058263e-04 -3.559279e-05
#>  [8,] -3.799009e-04 -2.048768e-04  6.758140e-04  6.539649e-05 -8.371422e-05
#>  [9,] -2.410134e-04 -1.058263e-04  6.539649e-05  2.970871e-04 -1.761427e-04
#> [10,]  3.850823e-04 -3.559279e-05 -8.371422e-05 -1.761427e-04  2.447035e-04
#> [11,] -1.048850e-05  1.279413e-04 -3.021617e-05 -9.753863e-05  4.380892e-06
#> [12,] -7.833762e-05 -3.120006e-05  1.280376e-04  7.892425e-05 -7.229547e-05
#>               [,11]         [,12]
#>  [1,] -3.282484e-05  2.080401e-05
#>  [2,] -4.620921e-06 -2.178969e-05
#>  [3,]  1.975373e-05 -7.742987e-06
#>  [4,] -2.418327e-06  1.596646e-05
#>  [5,] -1.335190e-04  5.806627e-05
#>  [6,] -1.048850e-05 -7.833762e-05
#>  [7,]  1.279413e-04 -3.120006e-05
#>  [8,] -3.021617e-05  1.280376e-04
#>  [9,] -9.753863e-05  7.892425e-05
#> [10,]  4.380892e-06 -7.229547e-05
#> [11,]  9.549947e-05 -2.877216e-05
#> [12,] -2.877216e-05  8.171428e-05
```
