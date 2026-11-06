# Using Spatial Bayesian methods in \`cityClimateHealth\`

Note: This page is in development

Another way that this model can be solved is by using Bayesian
inference, implemented in STAN. We are including this implementation
here so that it makes sense why we are including it later on.

The innovation here is combining the method of Armstrong 2014 with a
spatial method, in this case BYM2.

We implemented the spatial bayesian method of BYM2 but instead of
regular poisson as a conditional poisson (i.e., multinomial) which has
performance gains that they articulate in Amrstrong.

This requires bringing in a shapefile, so you can define the network.

The standard application is using MCMC, we also include all STAN model
types:

- MCMC
- laplace
- variational
- pathfinder

You can also experiment with speeding things up (at the risk of less
precise estimates) using the laplace or variational method. see Jack’s
notes as so what is going on here

Sb, basically a combination of these two things:
<https://doi.org/10.1093/ije/dyae061>
<https://link.springer.com/article/10.1186/1471-2288-14-122>

A reminder that this data is **simulated**.

``` r

library(cityClimateHealth)
library(data.table)
data("ma_exposure")
data("ma_deaths")

# create exposure matrix
exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

TOWNLIST <- c('CHELSEA', 'EVERETT', 'REVERE', 'MALDEN')

exposure <- subset(ma_exposure, TOWN20 %in% TOWNLIST)

exposure_mat <- make_exposure_matrix(exposure, 
                                     exposure_columns,
                                     time_subset = list(
                                       month = 5:9,
                                       year = 2012:2015
                                     ))
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# create outcome table
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
deaths   <- subset(ma_deaths, TOWN20 %in% TOWNLIST)
deaths_tbl <- make_outcome_table(deaths,  outcome_columns,
                                 time_subset = list(
                                       month = 5:9,
                                       year = 2012:2015
                                     ))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow

# plot
data("ma_towns")

library(ggplot2)
local_shp <- subset(ma_towns, TOWN20 %in%  TOWNLIST)
ggplot(local_shp) + geom_sf(aes(fill = TOWN20))
```

![](bayesian_demo_files/figure-html/setup2-1.png)

Now get initial estimates for each `geo_unit`

``` r

beta_l <- vector("list", 4) 
cr_l <- vector("list", 4) 
plot_l <- vector("list", 4)

cb_list <- vector("list", 4)
oo_list <- vector("list", 4)

for(bb in 1:4) {
  m1 <- condPois_1stage(
    subset(exposure_mat, TOWN20 == TOWNLIST[bb]),
    subset(deaths_tbl, TOWN20 == TOWNLIST[bb]),
    global_cen = 15)
  
  cb_list[[bb]] <- m1$`_`$out[[1]]$orig_basis
  oo_list[[bb]] <- m1$`_`$out[[1]]$outcomes
  
  beta_l[[bb]] <- m1$`_`$out[[1]]$orig_coef
  
  cr_l[[bb]] <- m1$`_`$out[[1]]$coef
  
  plot_l[[bb]] <- plot(m1)
  
}
#> 
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.8 31.3
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
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> crossbasis args for geo_unit  EVERETT :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.8 31
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> EVERETT:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> crossbasis args for geo_unit  REVERE :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.1 30.1
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> REVERE:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> crossbasis args for geo_unit  MALDEN :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 24.4 29
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> MALDEN:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0.025 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
mx <- do.call(cbind, beta_l) # COEFS NOT THE SAME
colnames(mx)  = TOWNLIST
mx
#>               CHELSEA     EVERETT       REVERE      MALDEN
#> cbv1.l1  4.511617e-02  0.05338664 -0.001194186  0.01796081
#> cbv1.l2 -2.221577e-02 -0.03613332  0.018706819  0.01235838
#> cbv1.l3  8.741108e-02  0.08597635  0.102871969  0.09324842
#> cbv1.l4 -3.370273e-02 -0.03575240 -0.033852436 -0.04133211
#> cbv2.l1  8.417004e-02  0.05004409 -0.053846967  0.04152872
#> cbv2.l2  1.879824e-02 -0.11837812  0.075557758  0.05431641
#> cbv2.l3  1.930366e-01  0.18558230  0.138719648  0.11890112
#> cbv2.l4 -4.655282e-02 -0.04588762 -0.060968615 -0.05426867
#> cbv3.l1  1.700927e-02  0.07284660  0.026149729  0.03952017
#> cbv3.l2 -6.231896e-05 -0.06923218  0.021941238 -0.00249271
#> cbv3.l3  1.725174e-01  0.12522816  0.118202522  0.13035167
#> cbv3.l4 -4.905026e-02 -0.01917009 -0.065144530 -0.04482534

mcr <- do.call(cbind, cr_l)   # COEFS THE SAME
colnames(mcr)  = TOWNLIST
mcr
#>      CHELSEA   EVERETT    REVERE    MALDEN
#> b1 0.1849001 0.1778403 0.2027540 0.1921185
#> b2 0.4649782 0.3075503 0.2509884 0.2930160
#> b3 0.3392233 0.2627241 0.2434500 0.2758572

library(patchwork)
wrap_plots(plot_l)
```

![](bayesian_demo_files/figure-html/b2-1.png)

the cr coefs are similar

the orig_coefs are not, which is why beta-wise implementation of SB_DLNM
method doesn’t work - because the don’t have to be the same to produce
similar curves.

So, instead of forcing Beta to be similar, we can use bym2

refs:

- <https://mc-stan.org/learn-stan/case-studies/icar_stan.html>
- <https://link.springer.com/article/10.1186/1476-072X-4-31>
- <https://github.com/stan-dev/example-models/blob/e5b7d9e2e9ecc375805c7e49e4a4d4c1882b5e3b/knitr/car-iar-poisson/bym2_predictor_plus_offset.stan#L4>

ok here’s the ref of how LAPLACE works:

- <https://mc-stan.org/cmdstanr/reference/model-method-laplace.html>
- <https://statmodeling.stat.columbia.edu/2023/02/08/implementing-laplace-approximation-in-stan-whats-happening-under-the-hood/>

I think this makes for a good candidate because betas are normal and the
model is not hierarchical

``` r

m_sb1 <- condPois_sb(exposure_mat, deaths_tbl, local_shp, 
                     stan_type = 'mcmc',
                     verbose = 2,
                     global_cen = 15,
                     stan_opts = list(refresh = 200),
                     use_spatial_model = 'none')
#>  STAN TYPE = mcmc 
#>  SPATIAL MODEL = none 
#> -- validation passed
#> truncate final RR and basis cen:  0.01 
#> -- prepare inputs
#> CHELSEA  
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.8 31.3
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> EVERETT  MALDEN  REVERE  
#> has to be one polygon per row in `shp`
#> 
#> -- run STAN
#> ld: warning: object file (/Users/cwm/.cmdstan/cmdstan-2.38.0/src/cmdstan/main.o) was built for newer 'macOS' version (26.0) than being linked (15.5)
#>  ...mcmc... 
#> Running MCMC with 2 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 31.4 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 31.6 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 31.5 seconds.
#> Total execution time: 31.8 seconds.
#> 
#>  ...mcmc draws... 
#> CHELSEA  EVERETT     MALDEN  REVERE  
#> -- apply estimates
#> truncate final RR and basis cen:  0.01
```

Compare, first you can see that with spatial_model = F, there is
similarity in beta coefs

``` r

mx
#>               CHELSEA     EVERETT       REVERE      MALDEN
#> cbv1.l1  4.511617e-02  0.05338664 -0.001194186  0.01796081
#> cbv1.l2 -2.221577e-02 -0.03613332  0.018706819  0.01235838
#> cbv1.l3  8.741108e-02  0.08597635  0.102871969  0.09324842
#> cbv1.l4 -3.370273e-02 -0.03575240 -0.033852436 -0.04133211
#> cbv2.l1  8.417004e-02  0.05004409 -0.053846967  0.04152872
#> cbv2.l2  1.879824e-02 -0.11837812  0.075557758  0.05431641
#> cbv2.l3  1.930366e-01  0.18558230  0.138719648  0.11890112
#> cbv2.l4 -4.655282e-02 -0.04588762 -0.060968615 -0.05426867
#> cbv3.l1  1.700927e-02  0.07284660  0.026149729  0.03952017
#> cbv3.l2 -6.231896e-05 -0.06923218  0.021941238 -0.00249271
#> cbv3.l3  1.725174e-01  0.12522816  0.118202522  0.13035167
#> cbv3.l4 -4.905026e-02 -0.01917009 -0.065144530 -0.04482534

m_sb1$`_`$beta_mat
#>            CHELSEA     EVERETT      MALDEN        REVERE
#>  [1,]  0.044531710  0.05348716  0.01929582 -0.0002008558
#>  [2,] -0.022060907 -0.03631032  0.01127519  0.0187679526
#>  [3,]  0.088632620  0.08633764  0.09259006  0.1024805189
#>  [4,] -0.034238599 -0.03572824 -0.04092922 -0.0339941279
#>  [5,]  0.083626949  0.05383650  0.04439957 -0.0493618664
#>  [6,]  0.018917850 -0.12142401  0.05288333  0.0724931553
#>  [7,]  0.195357060  0.18419304  0.11701583  0.1384325542
#>  [8,] -0.047496696 -0.04454840 -0.05202188 -0.0616162347
#>  [9,]  0.018203985  0.07559309  0.04008491  0.0278580807
#> [10,] -0.000858069 -0.07142697 -0.00267879  0.0209899657
#> [11,]  0.172457202  0.12455484  0.13053198  0.1177271426
#> [12,] -0.049519840 -0.01821028 -0.04507365 -0.0650840263
```

### Compare with spatial model

using laplace in this case, but you could try mcmc

``` r

m_sb2 <- condPois_sb(exposure_mat, deaths_tbl, local_shp, 
                     stan_type = 'laplace',
                     verbose = 2,
                     global_cen = 15,
                     stan_opts = list(refresh = 200),
                     use_spatial_model = 'bym2')
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = bym2 
#> -- validation passed
#> truncate final RR and basis cen:  0.01 
#> -- prepare inputs
#> CHELSEA  
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.8 31.3
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> EVERETT  MALDEN  REVERE  
#> has to be one polygon per row in `shp`
#> 
#> -- run STAN
#>  ...laplace optimize... 
#> Initial log joint probability = -6739.08 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      199      -6737.27   0.000981306       1.60031           1           1      233    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      240      -6737.27    8.4327e-05      0.819004           1           1      283    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.3 seconds.
#>  ...laplace sample... 
#> Calculating Hessian 
#> Calculating inverse of Cholesky factor 
#> Generating draws 
#> iteration: 0 
#> iteration: 100 
#> iteration: 200 
#> iteration: 300 
#> iteration: 400 
#> iteration: 500 
#> iteration: 600 
#> iteration: 700 
#> iteration: 800 
#> iteration: 900 
#> Finished in  0.5 seconds.
#>  ...laplace draws... 
#> CHELSEA  EVERETT     MALDEN  REVERE  
#> -- apply estimates
#> truncate final RR and basis cen:  0.01
```

Compare, now you can see these are different

``` r

mx
#>               CHELSEA     EVERETT       REVERE      MALDEN
#> cbv1.l1  4.511617e-02  0.05338664 -0.001194186  0.01796081
#> cbv1.l2 -2.221577e-02 -0.03613332  0.018706819  0.01235838
#> cbv1.l3  8.741108e-02  0.08597635  0.102871969  0.09324842
#> cbv1.l4 -3.370273e-02 -0.03575240 -0.033852436 -0.04133211
#> cbv2.l1  8.417004e-02  0.05004409 -0.053846967  0.04152872
#> cbv2.l2  1.879824e-02 -0.11837812  0.075557758  0.05431641
#> cbv2.l3  1.930366e-01  0.18558230  0.138719648  0.11890112
#> cbv2.l4 -4.655282e-02 -0.04588762 -0.060968615 -0.05426867
#> cbv3.l1  1.700927e-02  0.07284660  0.026149729  0.03952017
#> cbv3.l2 -6.231896e-05 -0.06923218  0.021941238 -0.00249271
#> cbv3.l3  1.725174e-01  0.12522816  0.118202522  0.13035167
#> cbv3.l4 -4.905026e-02 -0.01917009 -0.065144530 -0.04482534

m_sb2$`_`$beta_mat
#>            CHELSEA     EVERETT       MALDEN        REVERE
#>  [1,]  0.045683862  0.05151539  0.019796080  0.0003013382
#>  [2,] -0.021494459 -0.03372052  0.011077340  0.0168698189
#>  [3,]  0.086096009  0.08582853  0.091744080  0.1023937249
#>  [4,] -0.033112367 -0.03700927 -0.040158490 -0.0335025131
#>  [5,]  0.088224601  0.04795432  0.039277007 -0.0493627705
#>  [6,]  0.021207434 -0.11014507  0.059585169  0.0738176920
#>  [7,]  0.187510071  0.18297589  0.116038875  0.1358083687
#>  [8,] -0.042401884 -0.04760588 -0.053604996 -0.0605499147
#>  [9,]  0.016442589  0.07195723  0.038661510  0.0276266661
#> [10,]  0.001655267 -0.06602024 -0.000726477  0.0213456122
#> [11,]  0.172807556  0.12453876  0.129239992  0.1180066875
#> [12,] -0.048677235 -0.01990712 -0.044943852 -0.0650558245
```

### Compare with spatial model for leroux

using laplace in this case, but you could try mcmc

``` r

m_sb3 <- condPois_sb(exposure_mat, 
                     deaths_tbl, local_shp, 
                     stan_type = 'laplace',
                     verbose = 2,
                     stan_opts = list(refresh = 200),
                     use_spatial_model = 'leroux')
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = leroux 
#> -- validation passed
#> truncate final RR and basis cen:  0.01 
#> -- prepare inputs
#> CHELSEA  
#> crossbasis args for geo_unit  CHELSEA :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.8 31.3
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> Warning in condPois_1stage(exposure_matrix = single_exposure_matrix, outcomes_tbl = single_outcomes_tbl, : Centering point is outside the range of exposures in geo-unit CHELSEA: Cen = 6.80, x_b = (7.00, 36.00).
#>         This means your zones are across too large of an area, or if exposure is factor there could
#>         be too few events in this area, or
#>         there are differences in exposures so much that the bases are quite different. Try limiting the geo-units passed in to those that are more similar, manually setting a centering point that you know each geo-unit has, or changing your exposure variable.
#> EVERETT  MALDEN  REVERE  
#> Warning in condPois_1stage(exposure_matrix = single_exposure_matrix, outcomes_tbl = single_outcomes_tbl, : Centering point is outside the range of exposures in geo-unit REVERE: Cen = 6.10, x_b = (7.00, 35.00).
#>         This means your zones are across too large of an area, or if exposure is factor there could
#>         be too few events in this area, or
#>         there are differences in exposures so much that the bases are quite different. Try limiting the geo-units passed in to those that are more similar, manually setting a centering point that you know each geo-unit has, or changing your exposure variable.
#> has to be one polygon per row in `shp`
#> 
#> -- run STAN
#> ld: warning: object file (/Users/cwm/.cmdstan/cmdstan-2.38.0/src/cmdstan/main.o) was built for newer 'macOS' version (26.0) than being linked (15.5)
#>  ...laplace optimize... 
#> Initial log joint probability = -7000.25 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      199      -6510.09     0.0411323       555.021           1           1      216    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      399      -6444.44   0.000112724       350.294           1           1      431    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      599      -6443.53   1.58764e-05        197.88      0.7892      0.7892      639    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      799      -6443.03   3.21988e-05       176.443           1           1      850    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      999      -6442.66   0.000162787       276.479           1           1     1061    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     1199       -6442.4   2.11414e-05       209.903      0.2741      0.2741     1273    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     1399      -6441.88   0.000481651        430.66           1           1     1483    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     1599      -6441.51   4.77388e-05       256.801      0.9802      0.9802     1696    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     1799      -6441.37   0.000154318       224.935           1           1     1907    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     1999      -6441.14   1.39977e-05       134.994      0.2213      0.2213     2120    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     2199      -6441.06   4.28725e-05       94.6985           1           1     2332    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     2399      -6441.02   3.43345e-05       144.376           1           1     2542    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     2599      -6440.64   1.09279e-05       166.073           1           1     2752    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     2799      -6440.39   5.14175e-05       116.461           1           1     2966    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     2999      -6440.23   2.61604e-05       177.236           1           1     3180    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     3199      -6440.09   5.51966e-05       203.934           1           1     3386    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     3399      -6439.86   0.000435984       314.696           1           1     3600    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     3599      -6439.21   0.000397895       319.139      0.8223      0.8223     3807    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     3799      -6438.72   0.000180162       673.577       0.723       0.723     4016    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     3999      -6436.58    0.00308928       1773.19      0.2994      0.2994     4224    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     4199      -6431.39    0.00019311       1033.07           1           1     4436    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     4399      -6431.15   4.76349e-06        111.72      0.7299      0.7299     4649    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     4599      -6431.09   2.81814e-05       148.924           1           1     4856    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     4799      -6430.67   8.12289e-05       259.544           1           1     5064    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     4999      -6428.78   0.000799708       878.395           1           1     5274    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     5199      -6425.22    0.00585889       2316.82           1           1     5489    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     5399      -6421.45   2.89607e-05       746.661           1           1     5698    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     5599      -6420.65   1.12738e-05       243.411           1           1     5911    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     5799      -6420.51   9.69088e-05       480.727           1           1     6122    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     5999      -6420.27   5.53782e-05       475.649           1           1     6331    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     6199       -6420.2   8.22464e-06       300.364      0.3509      0.3509     6541    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     6399      -6420.11    1.2521e-05        339.58           1           1     6749    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     6599      -6420.06   2.14527e-06       42.2959       0.868       0.868     6961    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     6799      -6419.86   0.000168749       153.376           1           1     7168    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     6999       -6419.1   0.000655584       808.709           1           1     7379    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     7199      -6402.25    0.00158032        3936.4           1           1     7589    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     7399      -6399.11   3.21052e-05       2805.75           1           1     7799    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     7599      -6397.06    0.00198421       9229.76           1           1     8007    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     7799      -6395.73   4.71463e-05       1354.14           1           1     8217    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     7999      -6395.29   5.01887e-05        959.77           1           1     8425    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     8199      -6395.07   6.82588e-05       845.551      0.9306      0.9306     8633    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     8399       -6394.5   2.75018e-05       605.455           1           1     8842    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     8599      -6394.38   7.85478e-05       891.764           1           1     9055    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>     8738      -6394.27   4.98759e-07       74.5157           1           1     9201    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  4.0 seconds.
#>  ...laplace sample... 
#> Calculating Hessian 
#> Calculating inverse of Cholesky factor 
#> Generating draws 
#> iteration: 0 
#> iteration: 100 
#> iteration: 200 
#> iteration: 300 
#> iteration: 400 
#> iteration: 500 
#> iteration: 600 
#> iteration: 700 
#> iteration: 800 
#> iteration: 900 
#> Finished in  0.6 seconds.
#>  ...laplace draws... 
#> CHELSEA  EVERETT     MALDEN  REVERE  
#> -- apply estimates
#> truncate final RR and basis cen:  0.01
```

As you can see, lots of smoothing to a central estimate !

``` r

mx
#>               CHELSEA     EVERETT       REVERE      MALDEN
#> cbv1.l1  4.511617e-02  0.05338664 -0.001194186  0.01796081
#> cbv1.l2 -2.221577e-02 -0.03613332  0.018706819  0.01235838
#> cbv1.l3  8.741108e-02  0.08597635  0.102871969  0.09324842
#> cbv1.l4 -3.370273e-02 -0.03575240 -0.033852436 -0.04133211
#> cbv2.l1  8.417004e-02  0.05004409 -0.053846967  0.04152872
#> cbv2.l2  1.879824e-02 -0.11837812  0.075557758  0.05431641
#> cbv2.l3  1.930366e-01  0.18558230  0.138719648  0.11890112
#> cbv2.l4 -4.655282e-02 -0.04588762 -0.060968615 -0.05426867
#> cbv3.l1  1.700927e-02  0.07284660  0.026149729  0.03952017
#> cbv3.l2 -6.231896e-05 -0.06923218  0.021941238 -0.00249271
#> cbv3.l3  1.725174e-01  0.12522816  0.118202522  0.13035167
#> cbv3.l4 -4.905026e-02 -0.01917009 -0.065144530 -0.04482534

m_sb3$`_`$beta_mat
#>            CHELSEA      EVERETT      MALDEN       REVERE
#>  [1,]  0.023429853  0.023430355  0.02343185  0.023429708
#>  [2,] -0.005867785 -0.005869148 -0.00586696 -0.005868023
#>  [3,]  0.098209122  0.098211973  0.09821093  0.098210033
#>  [4,] -0.040395900 -0.040397844 -0.04039874 -0.040396633
#>  [5,]  0.018906442  0.018916291  0.01892275  0.018922711
#>  [6,] -0.005517501 -0.005508787 -0.00551068 -0.005515031
#>  [7,]  0.173943906  0.173942940  0.17394155  0.173943860
#>  [8,] -0.068068580 -0.068072618 -0.06806932 -0.068071241
#>  [9,]  0.036113697  0.036114723  0.03611573  0.036116123
#> [10,] -0.015239227 -0.015237272 -0.01523638 -0.015236366
#> [11,]  0.139001041  0.139001286  0.13900057  0.139001327
#> [12,] -0.048392043 -0.048391984 -0.04838974 -0.048391572
```

And you can also see that the leroux `q` value is quite high

``` r

subset(m_sb3$`_`$stan_summary, variable == 'q')
#> # A tibble: 1 × 7
#>   variable  mean median    sd    mad    q5   q95
#>   <chr>    <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 q        0.945  0.986 0.110 0.0179 0.754 0.999
```

All of the other objects associated with `condPois_1stage` or
`condPois_2stage` will also work here, along with the `_list` and factor
coding
