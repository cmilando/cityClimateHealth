# Run a conditional poisson model for across many geographic units

MixMeta nope its this:
https://www.sciencedirect.com/science/article/pii/S0140673614621140?via%3Dihub
avgtmax and range and random effect for county unclear, Gasp does both
here
https://github.com/gasparrini/RiskExtrapolation/blob/3caa5c46a748d6a789021554f435e53f9960d4c1/24_MetaRegression.R
and it goes counrty/city
https://github.com/gasparrini/MCC-SO2/blob/1a158b31bf83f3bbca3449b553a6570280306299/Rcode/06.nonlin.R#L47
nlmeta \<- mixmeta(nlcoefall, nlvcovall, random=~1\|country/city,
data=cities, control=list(showiter=T, igls.inititer=10)) basically, did
Gasp 2015 (temp_mean, temp_iqr, random effect for county) and included
the hierarchicacy structure for ids within countries
https://github.com/gasparrini/2015_gasparrini_EHP_Rcodedata
https://github.com/gasparrini/2019_sera_StatMed_Rcode/blob/master/03.MultilevelMA.R

## Usage

``` r
condPois_2stage(
  exposure_matrix,
  outcomes_tbl,
  global_cen = NULL,
  argvar = NULL,
  arglag = NULL,
  maxlag = NULL,
  min_n = NULL,
  rf = NULL,
  strata_min = 0,
  verbose = 0,
  truncate = 0.01
)
```

## Arguments

- exposure_matrix:

  a matrix of exposures, with columns for lag, usually created by
  `make_exposure_matrix`

- outcomes_tbl:

  a data.table of outcomes, created by `make_outcome_table`

- global_cen:

  a global centering point

- argvar:

  a list containing the `argvar` components for the `crossbasis`

- arglag:

  a list containing the `arglag` components for the `crossbasis`

- maxlag:

  an integer of the maximum lag

- min_n:

  an integer describing the minimum number of cases for a single region

- rf:

  optional character string to use as the random formula in mixmeta

- strata_min:

  minimum number of cases per strata

- verbose:

  whether to print geo_units as they are run

- truncate:

  proportion of the exposure distribution to trim from each tail when
  choosing the centering point (default 0.01)

## Value

an object of class condPois_2stage

## Details

update as of 2025.12.21, its this
https://github.com/gasparrini/Extended2stage/blob/cf82fd2f0d616210e8aa6911d2fb49a732f65795/02.multilevel.R#L68

## Examples

``` r
towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 %in% towns),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 %in% towns),
                                   outcome_columns, time_subset = time_subset)
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
m <- condPois_2stage(exposure_mat, outcomes_tbl, global_cen = 20)
#> 
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.6 32.1
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> CHELSEA:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
```
