# Validation of spatial methods in \`cityClimateHealth\`

``` r

library(cityClimateHealth)
```

need a validation rmd of the data with larger population sizes and low
variance and maybe just one lag so its easy to see

or maybe just one populations segment

### Compare: Gasparinni et al. 

We can compare the functionality of cityclimateHealth to [Gasparrini et
al.’s
work](https://CRAN.R-project.org/package=dlnm/vignettes/dlnmTS.pdf).

First, let us look at Gasparrini et al.’s simulated dataset, looking at
cardiovascular, respiratory, and mortality outcomes in a Chicago
population:

``` r


library(dlnm)
#> This is dlnm 2.4.10. For details: help(dlnm) and vignette('dlnmOverview').
head(chicagoNMMAPS)
#>         date time year month doy      dow death cvd resp       temp   dptp
#> 1 1987-01-01    1 1987     1   1 Thursday   130  65   13 -0.2777778 31.500
#> 2 1987-01-02    2 1987     1   2   Friday   150  73   14  0.5555556 29.875
#> 3 1987-01-03    3 1987     1   3 Saturday   101  43   11  0.5555556 27.375
#> 4 1987-01-04    4 1987     1   4   Sunday   135  72    7 -1.6666667 28.625
#> 5 1987-01-05    5 1987     1   5   Monday   126  64   12  0.0000000 28.875
#> 6 1987-01-06    6 1987     1   6  Tuesday   130  63   12  4.4444444 35.125
#>     rhum     pm10       o3
#> 1 95.500 26.95607 4.376079
#> 2 88.250       NA 4.929803
#> 3 89.500 32.83869 3.751079
#> 4 84.500 39.95607 4.292746
#> 5 74.500       NA 4.751079
#> 6 77.375 40.95607 6.334412
```

Gasparrini et al. specify a model to examine the association between
mortality and particulate matter 10 microns across or smaller (PM10),
while adjusting for temperature. The research team defined the
crossbasis objects and models accordingly:

``` r


library(dlnm)
library(splines)
library(data.table)

# CONSTANTS
maxlag = 15
pct = c(0.1, 0.5, 0.9)
argvar_fcn = 'bs'
argvar_degree = 3
arglag_fcn = 'poly'
arglag_degree = 4
df_per_year = 7

# DATA
chicagoNMMAPS <- copy(chicagoNMMAPS)
setDT(chicagoNMMAPS)

# add group levels
chicagoNMMAPS$town <- "Chicago"
chicagoNMMAPS$county <- "Cook"

# add dow
chicagoNMMAPS[, dt_fmt := as.IDate(date)]
chicagoNMMAPS[, dow_int := wday(dt_fmt)]
chicagoNMMAPS[, dow := factor(weekdays(dt_fmt))]

# =============================================================================
# /////////////////////////////////////////////////////////////////////////////
# EXPOSURE
# /////////////////////////////////////////////////////////////////////////////
# =============================================================================

# make XMAT
xmat <- matrix(NA, nrow = nrow(chicagoNMMAPS), ncol = maxlag + 1)
xmat[,1] = chicagoNMMAPS$temp
for(j in 2:ncol(xmat)) {
  xmat[,j] = dplyr::lag(chicagoNMMAPS$temp, j - 1)
}
head(xmat)
#>            [,1]       [,2]       [,3]       [,4]       [,5]       [,6] [,7]
#> [1,] -0.2777778         NA         NA         NA         NA         NA   NA
#> [2,]  0.5555556 -0.2777778         NA         NA         NA         NA   NA
#> [3,]  0.5555556  0.5555556 -0.2777778         NA         NA         NA   NA
#> [4,] -1.6666667  0.5555556  0.5555556 -0.2777778         NA         NA   NA
#> [5,]  0.0000000 -1.6666667  0.5555556  0.5555556 -0.2777778         NA   NA
#> [6,]  4.4444444  0.0000000 -1.6666667  0.5555556  0.5555556 -0.2777778   NA
#>      [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16]
#> [1,]   NA   NA    NA    NA    NA    NA    NA    NA    NA
#> [2,]   NA   NA    NA    NA    NA    NA    NA    NA    NA
#> [3,]   NA   NA    NA    NA    NA    NA    NA    NA    NA
#> [4,]   NA   NA    NA    NA    NA    NA    NA    NA    NA
#> [5,]   NA   NA    NA    NA    NA    NA    NA    NA    NA
#> [6,]   NA   NA    NA    NA    NA    NA    NA    NA    NA

# get knots
kn = quantile(chicagoNMMAPS$temp, probs =  pct)
kn
#>       10%       50%       90% 
#> -3.888889 10.555556 23.888889

cb1.temp <- crossbasis(
  xmat, lag = maxlag, 
  argvar=list(fun=argvar_fcn, knots = kn, degree=argvar_degree),
  arglag=list(fun=arglag_fcn,degree=arglag_degree))

dim(cb1.temp)
#> [1] 5114   30

# also make it in cityClimateHealth
# create exposure matrix
exposure_columns <- list(
  "date" = "date",
  "exposure" = "temp", 
  "geo_unit" = "town",
  "geo_unit_grp" = "county"
)

chicago_exposure_mat <- make_exposure_matrix(
    chicagoNMMAPS, 
    exposure_columns, 
    maxlag = maxlag,
    time_subset = 'use_all')
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# looks good
quantile(chicago_exposure_mat$temp, probs = pct)
#>       10%       50%       90% 
#> -3.888889 10.555556 23.888889
quantile(chicagoNMMAPS$temp, probs = pct)
#>       10%       50%       90% 
#> -3.888889 10.555556 23.888889

# =============================================================================
# /////////////////////////////////////////////////////////////////////////////
# OUTCOME
# /////////////////////////////////////////////////////////////////////////////
# =============================================================================

# create outcome table
outcome_columns <- list(
  "date" = "date",
  "outcome" = "death",
  "geo_unit" = "town",
  "geo_unit_grp" = "county"
)

chicago_deaths_tbl <- make_outcome_table(
  chicagoNMMAPS,  
  outcome_columns,
  time_subset = 'use_all'
)
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow


# =============================================================================
# /////////////////////////////////////////////////////////////////////////////
# MODEL
# /////////////////////////////////////////////////////////////////////////////
# =============================================================================

# similar to our package, 7 df is used per year
n_years = length(unique(year(chicagoNMMAPS$dt_fmt)))

# get a numeric index for time
chicagoNMMAPS[, time_num := as.numeric(dt_fmt)]

# removing NA values due to maxlag
# -- make sure to reset crossbasis attributes
cb1.temp_not_na <- cb1.temp[(maxlag+1):nrow(cb1.temp), ]
tmp = attributes(cb1.temp)
tmp$dim = dim(cb1.temp_not_na)
attributes(cb1.temp_not_na) = tmp
# and in outcomes
chicagoNMMAPS_not_na <- chicagoNMMAPS[(maxlag+1):nrow(chicagoNMMAPS), ]

# run the model
gasparrini_model <- glm(death ~ cb1.temp_not_na + 
                          ns(time_num, df_per_year * n_years) + dow,
                        data = chicagoNMMAPS_not_na, 
                        family=quasipoisson())

# get a centered crosspred
gasp_cp <- crosspred(cb1.temp, gasparrini_model, cen = 5, by = 0.1)
xcen = gasp_cp$predvar[which.min(gasp_cp$allRRfit)]
gasp_cp <- crosspred(cb1.temp, gasparrini_model, cen = xcen)
# plot(gasp_cp, "overall")

#xs
data.frame(
  temp = 30,
  RR = gasp_cp$allRRfit["30"],
  RRlb= gasp_cp$allRRlow["30"],
  RRub = gasp_cp$allRRhigh["30"]
)
#>    temp       RR     RRlb     RRub
#> 30   30 1.592442 1.475681 1.718442

# =============================================================================
# /////////////////////////////////////////////////////////////////////////////
# MODEL
# /////////////////////////////////////////////////////////////////////////////
# =============================================================================

cityclimatehealth_model <- timeseries_1stage_glm(
  exposure_matrix = chicago_exposure_mat,
  outcomes_tbl = chicago_deaths_tbl,
  maxlag = maxlag,
  argvar = list(fun = argvar_fcn, pct = pct, degree = argvar_degree),   
  arglag = list(fun = arglag_fcn, degree = arglag_degree),
  df_per_year = df_per_year,
  time_trend = "annual",
  global_cen = xcen,
  truncate = 0.0
)
#> strata:
#> Chicago:yr1987:mn01:dow06
#> strata_min: 0 
#> 
#> 
#> crossbasis args for geo_unit  Chicago :
#> 
#> maxlag: 15 
#> 
#> argvar:
#> List of 3
#>  $ fun   : chr "bs"
#>  $ knots : Named num [1:3] -3.89 10.56 23.89
#>   ..- attr(*, "names")= chr [1:3] "10%" "50%" "90%"
#>  $ degree: num 3
#> Warning in make_crossbasis(exposure_matrix = exposure_matrix, outcomes_tbl =
#> outcomes_tbl, : check that arglag is valid
#> arglag:
#> List of 2
#>  $ fun   : chr "poly"
#>  $ degree: num 4
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> time_trend: annual 
#> n_years: 14 
#> df_time for ns(date): 98 
#> add_dow: TRUE 
#> 
#> population offset: none
#> model formula:
#> death ~ cb + ns(time_num, df = 98) + dow

#plot(cityclimatehealth_model)

# model, and you see that it matches above
subset(getRR(cityclimatehealth_model), temp == 30)
#>     temp       RR     RRlb     RRub n_geo_names       model_class
#>    <num>    <num>    <num>    <num>      <char>            <char>
#> 1:    30 1.592442 1.475681 1.718442     Chicago timeseries_1stage
```
