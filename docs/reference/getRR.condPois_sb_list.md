# getRR method for condPois_sb_list

getRR method for condPois_sb_list

## Usage

``` r
# S3 method for class 'condPois_sb_list'
getRR(x, ...)
```

## Arguments

- x:

  an object of class condPois_sb_list

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a data.table of relative risk estimates across factor levels

## Examples

``` r
towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
                        "factor" = "age_grp",
                        "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
time_subset <- list(month = 5:9, year = 2012:2013)
exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 %in% towns),
                                     exposure_columns, time_subset = time_subset)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 %in% towns),
                                   outcome_columns, time_subset = time_subset)
#> > Combined factor is  age_grp 
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
shp <- subset(ma_towns, TOWN20 %in% towns)
# with a factor, a condPois_sb_list is returned
m <- condPois_sb(exposure_mat, outcomes_tbl, shp, global_cen = 20)
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = bym2 
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = bym2 
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> has to be one polygon per row in `shp`
#>  ...laplace optimize... 
#> Initial log joint probability = -2718.82 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99      -2717.22    0.00164841       36.2425     0.05718           1      135    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      199      -2717.01   0.000486974      0.998158      0.5431      0.5431      268    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      238         -2717   0.000163777      0.261991           1           1      317    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
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
#> Finished in  0.4 seconds.
#>  ...laplace draws... 
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = bym2 
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> has to be one polygon per row in `shp`
#>  ...laplace optimize... 
#> Initial log joint probability = -2647.92 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99      -2646.12    0.00292772       2.86771       0.801       0.801      145    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      187      -2646.11     0.0010345      0.349034           1           1      254    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
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
#> Finished in  0.3 seconds.
#>  ...laplace draws... 
#>  STAN TYPE = laplace 
#>  SPATIAL MODEL = bym2 
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
#> min_n: 1 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> has to be one polygon per row in `shp`
#>  ...laplace optimize... 
#> Initial log joint probability = -2685.53 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99      -2683.95     0.0044776       3.91176           1           1      122    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      199      -2683.78   0.000479992       8.62947      0.2392      0.2392      254    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      299      -2683.73    0.00798623       2.12416           1           1      382    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      395      -2683.71    0.00030256      0.272813      0.9702      0.9702      495    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.2 seconds.
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
#> Finished in  0.4 seconds.
#>  ...laplace draws... 
getRR(m)
#>        TOWN20 COUNTY20 tmax_C        RR      RRlb      RRub        stage
#>        <char>   <char>  <num>     <num>     <num>     <num>       <char>
#>    1: CHELSEA  SUFFOLK    8.0 0.9025864 0.8628453 0.9441579 SpatialBayes
#>    2: CHELSEA  SUFFOLK    8.1 0.9025864 0.8628453 0.9441579 SpatialBayes
#>    3: CHELSEA  SUFFOLK    8.2 0.9025864 0.8628453 0.9441579 SpatialBayes
#>    4: CHELSEA  SUFFOLK    8.3 0.9025864 0.8628453 0.9441579 SpatialBayes
#>    5: CHELSEA  SUFFOLK    8.4 0.9025864 0.8628453 0.9441579 SpatialBayes
#>   ---                                                                   
#> 6398:  REVERE  SUFFOLK   34.2 1.2283424 1.1653410 1.2947499       stage1
#> 6399:  REVERE  SUFFOLK   34.3 1.2291841 1.1646649 1.2972775       stage1
#> 6400:  REVERE  SUFFOLK   34.4 1.2300230 1.1639512 1.2998453       stage1
#> 6401:  REVERE  SUFFOLK   34.5 1.2308602 1.1632038 1.3024516       stage1
#> 6402:  REVERE  SUFFOLK   34.6 1.2316968 1.1624267 1.3050947       stage1
#>       age_grp      model_class
#>        <char>           <char>
#>    1:    0-17 condPois_sb_list
#>    2:    0-17 condPois_sb_list
#>    3:    0-17 condPois_sb_list
#>    4:    0-17 condPois_sb_list
#>    5:    0-17 condPois_sb_list
#>   ---                         
#> 6398:     65+ condPois_sb_list
#> 6399:     65+ condPois_sb_list
#> 6400:     65+ condPois_sb_list
#> 6401:     65+ condPois_sb_list
#> 6402:     65+ condPois_sb_list
```
