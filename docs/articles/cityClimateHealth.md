# Getting started with \`cityClimateHealth\`

## Getting started with `cityClimateHealth`

Here is code that shows the basic skeleton of how this package works. We
can run the model and then calculate attributable numbers easily, and
provide a number of outputs.

``` r

library(cityClimateHealth)
```

## Run the model

### Exposure

First, create the exposure object - you will need to define the
`exposure_columns`.

``` r

library(data.table)

# load a built-in dataset and get a subset
data("ma_exposure") 

exposure_sub <- 
  subset(ma_exposure,
         COUNTY20 %in% c('MIDDLESEX', 'WORCESTER'))

# define columns of ma_exposure
exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

# create the object
ma_exposure_matrix <- make_exposure_matrix(exposure_sub, 
                                           exposure_columns,
                                           time_subset = list(
                                       month = 5:9,
                                       year = 2012:2015
                                     ))
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
```

And lets preview this

``` r

head(ma_exposure_matrix)
#>          date TOWN20  COUNTY20  tmax_C                  strata     match_strata
#>        <IDat> <char>    <char>   <num>                  <char>           <char>
#> 1: 2012-05-01  ACTON MIDDLESEX 16.4633 ACTON:yr2012:mn05:dow03 ACTON:2012-05-01
#> 2: 2012-05-02  ACTON MIDDLESEX  8.6743 ACTON:yr2012:mn05:dow04 ACTON:2012-05-02
#> 3: 2012-05-03  ACTON MIDDLESEX 11.1778 ACTON:yr2012:mn05:dow05 ACTON:2012-05-03
#> 4: 2012-05-04  ACTON MIDDLESEX 12.4253 ACTON:yr2012:mn05:dow06 ACTON:2012-05-04
#> 5: 2012-05-05  ACTON MIDDLESEX 12.8489 ACTON:yr2012:mn05:dow07 ACTON:2012-05-05
#> 6: 2012-05-06  ACTON MIDDLESEX 17.7602 ACTON:yr2012:mn05:dow01 ACTON:2012-05-06
#>    explag1 explag2 explag3 explag4 explag5
#>      <num>   <num>   <num>   <num>   <num>
#> 1: 14.0179 14.1931 12.7975 17.5538 16.2753
#> 2: 16.4633 14.0179 14.1931 12.7975 17.5538
#> 3:  8.6743 16.4633 14.0179 14.1931 12.7975
#> 4: 11.1778  8.6743 16.4633 14.0179 14.1931
#> 5: 12.4253 11.1778  8.6743 16.4633 14.0179
#> 6: 12.8489 12.4253 11.1778  8.6743 16.4633
```

### Outcome

Next, create the outcome object. As seen in other tutorials, you can
`collapse_to` a factor level and get outputs that way later on.

``` r

# load a built-in dataset, and get a subset, for speed
data("ma_deaths") 

deaths_sub <- 
  subset(ma_deaths,
        COUNTY20 %in% c('MIDDLESEX', 'WORCESTER'))

# define columns of ma_deaths
outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

# create the object
ma_outcomes_tbl <- make_outcome_table(deaths_sub, outcome_columns,
                                      time_subset = list(
                                       month = 5:9,
                                       year = 2012:2015
                                     ))
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
```

And lets preview this

``` r

head(ma_outcomes_tbl)
#>          date TOWN20  COUNTY20 daily_deaths                  strata
#>        <IDat> <char>    <char>        <int>                  <char>
#> 1: 2012-05-01  ACTON MIDDLESEX           73 ACTON:yr2012:mn05:dow03
#> 2: 2012-05-02  ACTON MIDDLESEX           78 ACTON:yr2012:mn05:dow04
#> 3: 2012-05-03  ACTON MIDDLESEX           78 ACTON:yr2012:mn05:dow05
#> 4: 2012-05-04  ACTON MIDDLESEX           78 ACTON:yr2012:mn05:dow06
#> 5: 2012-05-05  ACTON MIDDLESEX           78 ACTON:yr2012:mn05:dow07
#> 6: 2012-05-06  ACTON MIDDLESEX           72 ACTON:yr2012:mn05:dow01
#>    strata_total     match_strata
#>           <int>           <char>
#> 1:          423 ACTON:2012-05-01
#> 2:          420 ACTON:2012-05-02
#> 3:          414 ACTON:2012-05-03
#> 4:          327 ACTON:2012-05-04
#> 5:          334 ACTON:2012-05-05
#> 6:          334 ACTON:2012-05-06
```

### Run the conditional poisson model

we then run a conditional poisson model.

#### Cross-basis arguments

There are built-in arguments for `argvar` and `arglag` that you can
override if you’d like, but the defaults are:

- `maxlag`: default is 5 (days)
- `argvar`: default is `ns()` and knots at the 50th and 90th percentile
  of unit-specific exposure.
- `arglag`: default is
  `list(fun = 'ns', knots = logknots(maxlag, nk = 2))`

You can also affect the global centering point:

- the default behavior is `global_cen = NULL`, meaning that the mininum
  RR will be used
- you can override this by setting `global_cen`

#### Model types

Now you have several options for running the conditional poisson model:

| Design | Function | Description |
|----|----|----|
| **1-stage design** | `condPois_1stage` | Produces a single set of beta coefficients across all included spatial units. If multiple `geo_units` are present in the input objects, `multi_zone = TRUE` must be set. This option does not use `mixmeta` or `blup`. |
| **2-stage design** | `condPois_2stage` | Estimates beta coefficients for each spatial unit and then uses `mixmeta` and `blup` to obtain more stable estimates. |
| **Spatial Bayes** | `condPois_sb` | Also estimates beta coefficients for each spatial unit, but applies Bayesian methods to stabilize estimates by borrowing information from neighboring spatial units, rather than from the full dataset as in `mixmeta`. This approach is especially useful in settings with small outcome numbers. |

We show code for each but just run `condPois_2stage` in this vignette.

``` r

ma_model <- condPois_2stage(ma_exposure_matrix, 
                            ma_outcomes_tbl,
                            verbose = 1,
                            global_cen = 15)
#> -- validation passed
#> -- stage 1
#> 
#> crossbasis args for geo_unit  ACTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.7 31.4
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> ACTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> -- mixmeta
#> formula: coef_matrix ~ exp_mean + exp_IQR
#> random: ~ 1 | COUNTY20/TOWN20 
#> -- stage 2
#> truncate final RR and basis cen:  0.01
```

For `condPois_1stage` the call would look like this, where you’d need to
add the argument `multi_zone = T` because there are multiple `geo_units`
in `ma_exposure_matrix`:

``` r

ma_model <- condPois_1stage(ma_exposure_matrix, ma_outcomes_tbl, 
                            multi_zone = T,
                            global_cen = 15)
```

See
[`vignette("one_stage_demo")`](http://climatehealth.city/articles/one_stage_demo.md)
for more details. Note that `forest_plot` and `spatial_plot` are not
implemented for `condPois_1stage` since you can get all of that
information from the RR plot.

And for `condPois_sb`, the only additional information you’d need is a
shapefile showing how the `geo_unit`s are arranged, in this case
`ma_towns` (in a test run this code took 20 minutes to complete for the
full MA dataset \[with maybe some additional bugs to work out\]):

``` r

data("ma_towns")
ma_model <- condPois_sb(ma_exposure_matrix, ma_outcomes_tbl, 
                        global_cen = 15, ma_towns)
```

See
[`vignette("bayesian_demo")`](http://climatehealth.city/articles/bayesian_demo.md)
for more details.

### Plot outputs

And are several plots you can make.

First, a basic RR plot by `geo_unit`:

``` r

plot(ma_model, "CAMBRIDGE")
```

![](cityClimateHealth_files/figure-html/plot1-1.png)

You can also make a forest plot at a specific exposure value

``` r

forest_plot(ma_model, exposure_val = 25.1)
#> Warning in forest_plot.condPois_2stage(ma_model, exposure_val = 25.1): plotting
#> by group since n_geos > 20
```

![](cityClimateHealth_files/figure-html/forest_plot-1.png)

You can also make a spatial plot at a specific exposure value:

``` r

spatial_plot(ma_model, shp = ma_towns, exposure_val = 25.1)
```

![](cityClimateHealth_files/figure-html/spatial_plot-1.png)

### getRR

For your own purposes, each of these objects has a `getRR` function

``` r

getRR(ma_model)
#>           TOWN20  COUNTY20 tmax_C        RR      RRlb      RRub  stage
#>           <char>    <char>  <num>     <num>     <num>     <num> <char>
#>     1:     ACTON MIDDLESEX    7.0 0.9772782 0.9587192 0.9961965 stage2
#>     2:     ACTON MIDDLESEX    7.1 0.9772782 0.9587192 0.9961965 stage2
#>     3:     ACTON MIDDLESEX    7.2 0.9772782 0.9587192 0.9961965 stage2
#>     4:     ACTON MIDDLESEX    7.3 0.9772782 0.9587192 0.9961965 stage2
#>     5:     ACTON MIDDLESEX    7.4 0.9772782 0.9587192 0.9961965 stage2
#>    ---                                                                
#> 64868: WORCESTER WORCESTER   33.0 1.2067807 1.1632921 1.2518951 stage1
#> 64869: WORCESTER WORCESTER   33.1 1.2079470 1.1632097 1.2544049 stage1
#> 64870: WORCESTER WORCESTER   33.2 1.2091133 1.1631117 1.2569342 stage1
#> 64871: WORCESTER WORCESTER   33.3 1.2102799 1.1630002 1.2594816 stage1
#> 64872: WORCESTER WORCESTER   33.4 1.2114471 1.1628771 1.2620457 stage1
#>            model_class
#>                 <char>
#>     1: condPois_2stage
#>     2: condPois_2stage
#>     3: condPois_2stage
#>     4: condPois_2stage
#>     5: condPois_2stage
#>    ---                
#> 64868: condPois_2stage
#> 64869: condPois_2stage
#> 64870: condPois_2stage
#> 64871: condPois_2stage
#> 64872: condPois_2stage
```

## Calculate attributable numbers

See more details in
[`vignette("attributable_number")`](http://climatehealth.city/articles/attributable_number.md),
but here is a brief demo

### Population data

The first step of calculating attributable numbers is having a
population data estimate.

This varies a lot by place and dataset, so we don’t include
functionality for it (but an example of how this could be done can be
seen in
[`vignette("get_pop_estimates")`](http://climatehealth.city/articles/get_pop_estimates.md)).

Assume you are starting with a dataset for the entire timeframe that
looks like this:

``` r

library(data.table)
data("ma_pop_data")
setDT(ma_pop_data)
ma_pop_data
#>               TOWN20 Female_0-17 Female_18-64 Female_65+ Male_0-17 Male_18-64
#>               <char>       <num>        <num>      <num>     <num>      <num>
#>   1:      BARNSTABLE        3899        15017       6014      4499      14035
#>   2:          BOURNE        1891         5751       3212      1489       5302
#>   3:        BREWSTER         634         2518       2007       833       2628
#>   4:         CHATHAM         163         1477       1759       480       1265
#>   5:          DENNIS         573         3792       3133       784       4101
#>  ---                                                                         
#> 347:   WEST BOYLSTON         619         2021       1107       604       2554
#> 348: WEST BROOKFIELD         343         1162        578       243       1002
#> 349:     WESTMINSTER         847         2371       1131       762       2028
#> 350:      WINCHENDON        1254         3318        711      1031       3134
#> 351:       WORCESTER       18779        67750      15995     21129      69365
#>      Male_65+
#>         <num>
#>   1:     5458
#>   2:     2810
#>   3:     1721
#>   4:     1463
#>   5:     2359
#>  ---         
#> 347:      790
#> 348:      495
#> 349:     1081
#> 350:      924
#> 351:    11173
```

Need to do some transformations:

- pivot longer
- variable clean

Note again, this processing will vary by application so this approach is
not prescriptive !

Pivot longer:

``` r

ma_pop_data_long <- melt(
  ma_pop_data,
  id.vars = "TOWN20",
  variable.name = "sex_age",
  value.name = "population"
)
```

Variable clean:

``` r

ma_pop_data_long$sex_age <- as.character(ma_pop_data_long$sex_age)
varnames <- strsplit(ma_pop_data_long$sex_age, "_", fixed = T)
varnames <- data.frame(do.call(rbind, varnames))
names(varnames) <- c('sex', 'age_grp')
rr <- which(varnames$sex == 'Female')
varnames$sex[rr] <- 'F'
rr <- which(varnames$sex == 'Male')
varnames$sex[rr] <- 'M'
ma_pop_data_long$sex = varnames$sex
ma_pop_data_long$age_grp = varnames$age_grp
ma_pop_data_long$sex_age <- NULL
```

Lets look at it:

``` r

ma_pop_data_long
#>                TOWN20 population    sex age_grp
#>                <char>      <num> <char>  <char>
#>    1:      BARNSTABLE       3899      F    0-17
#>    2:          BOURNE       1891      F    0-17
#>    3:        BREWSTER        634      F    0-17
#>    4:         CHATHAM        163      F    0-17
#>    5:          DENNIS        573      F    0-17
#>   ---                                          
#> 2102:   WEST BOYLSTON        790      M     65+
#> 2103: WEST BROOKFIELD        495      M     65+
#> 2104:     WESTMINSTER       1081      M     65+
#> 2105:      WINCHENDON        924      M     65+
#> 2106:       WORCESTER      11173      M     65+
```

We assume that these properties hold for the entire timeframe of our
analysis, but you could also make a version of this dataset with a
‘year’ column.

### Calculate AN

Now, you can easily calculate attributrable numbers (and rates) using
`calcAN()`.

There are two new inputs that this function needs, in addition to
population data:

- `spatial_agg_type` - what spatial resolution are you summarizing to:
  ‘geo_unit’, ‘geo_unit_grp’, or ‘all’
- `spatial_join_col` - which columns in `ma_outcomes_tbl` are you
  joining `ma_pop_data_long` by

``` r

ma_AN <- calc_AN(ma_model, ma_outcomes_tbl, ma_pop_data_long,
                 spatial_agg_type = 'TOWN20', spatial_join_col = 'TOWN20')
```

From this you get a `rate_table` :

``` r

ma_AN$`_`$rate_table
#>          TOWN20  COUNTY20 population above_MMT mean_annual_attr_rate_est
#>          <char>    <char>      <num>    <lgcl>                     <num>
#>   1:      ACTON MIDDLESEX      23864      TRUE                5187.73047
#>   2:      ACTON MIDDLESEX      23864     FALSE                 -26.19008
#>   3:  ARLINGTON MIDDLESEX      45906      TRUE                5741.07960
#>   4:  ARLINGTON MIDDLESEX      45906     FALSE                 -35.94301
#>   5: ASHBURNHAM WORCESTER       6337      TRUE                4025.95866
#>  ---                                                                    
#> 224: WINCHESTER MIDDLESEX      22809     FALSE                 -42.74628
#> 225:     WOBURN MIDDLESEX      40992      TRUE                5059.82875
#> 226:     WOBURN MIDDLESEX      40992     FALSE                 -18.29625
#> 227:  WORCESTER WORCESTER     204191      TRUE                5008.72957
#> 228:  WORCESTER WORCESTER     204191     FALSE                 -28.71086
#>      mean_annual_attr_rate_lb mean_annual_attr_rate_ub
#>                         <num>                    <num>
#>   1:               4083.45206              6434.168622
#>   2:                -52.38015                -2.042826
#>   3:               4683.17050              6829.703634
#>   4:                -56.63748               -16.051823
#>   5:               3161.88654              4979.091053
#>  ---                                                  
#> 224:                -64.66746               -23.017230
#> 225:               4010.41667              6208.955406
#> 226:                -33.25344                -4.238632
#> 227:               4029.92786              5993.083681
#> 228:                -46.09961               -12.895402
```

and a `number_table`:

``` r

ma_AN$`_`$number_table
#>          TOWN20  COUNTY20 population above_MMT mean_annual_attr_num_est
#>          <char>    <char>      <num>    <lgcl>                    <num>
#>   1:      ACTON MIDDLESEX      23864      TRUE                 1238.000
#>   2:      ACTON MIDDLESEX      23864     FALSE                   -6.250
#>   3:  ARLINGTON MIDDLESEX      45906      TRUE                 2635.500
#>   4:  ARLINGTON MIDDLESEX      45906     FALSE                  -16.500
#>   5: ASHBURNHAM WORCESTER       6337      TRUE                  255.125
#>  ---                                                                   
#> 224: WINCHESTER MIDDLESEX      22809     FALSE                   -9.750
#> 225:     WOBURN MIDDLESEX      40992      TRUE                 2074.125
#> 226:     WOBURN MIDDLESEX      40992     FALSE                   -7.500
#> 227:  WORCESTER WORCESTER     204191      TRUE                10227.375
#> 228:  WORCESTER WORCESTER     204191     FALSE                  -58.625
#>      mean_annual_attr_num_lb mean_annual_attr_num_ub
#>                        <num>                   <num>
#>   1:               974.47500              1535.45000
#>   2:               -12.50000                -0.48750
#>   3:              2149.85625              3135.24375
#>   4:               -26.00000                -7.36875
#>   5:               200.36875               315.52500
#>  ---                                                
#> 224:               -14.75000                -5.25000
#> 225:              1643.95000              2545.17500
#> 226:               -13.63125                -1.73750
#> 227:              8228.75000             12237.33750
#> 228:               -94.13125               -26.33125
```

And you can plot either one

``` r

plot(ma_AN, "num", above_MMT = T)
#> Warning in plot.calcAN(ma_AN, "num", above_MMT = T): plot elements > 20,
#> subsetting to top 20
```

![](cityClimateHealth_files/figure-html/calcAN_plot-1.png)

You can also plot spatially

``` r

spatial_plot(ma_AN, shp = ma_towns, table_type = "num", above_MMT = T)
```

![](cityClimateHealth_files/figure-html/multi_plot3d-1.png)

## Running with factors

Very often, we also get asked to run these results, with differences by
both modifiable and non-modifiable factors:

- age group
- sex
- the prevalence of air conditioning in a certain town

We can easily do this, by using the `collapse_to` argument:

``` r

outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20",
  "factor" = "age_grp"
)

ma_outcomes_tbl_fct <- make_outcome_table(
  deaths_sub, outcome_columns, 
  time_subset = list(
       month = 5:9,
       year = 2012:2015
     ))
#> > Combined factor is  age_grp
#> Missing outcome values introduced by xgrid were set to 0;
#>             assumes that every time in the dataset should have an outcome value
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
```

Lets look at the result:

``` r

head(ma_outcomes_tbl_fct)
#>          date TOWN20  COUNTY20 age_grp daily_deaths                  strata
#>        <IDat> <char>    <char>  <char>        <int>                  <char>
#> 1: 2012-05-01  ACTON MIDDLESEX    0-17           25 ACTON:yr2012:mn05:dow03
#> 2: 2012-05-01  ACTON MIDDLESEX   18-64           24 ACTON:yr2012:mn05:dow03
#> 3: 2012-05-01  ACTON MIDDLESEX     65+           24 ACTON:yr2012:mn05:dow03
#> 4: 2012-05-02  ACTON MIDDLESEX    0-17           26 ACTON:yr2012:mn05:dow04
#> 5: 2012-05-02  ACTON MIDDLESEX   18-64           26 ACTON:yr2012:mn05:dow04
#> 6: 2012-05-02  ACTON MIDDLESEX     65+           26 ACTON:yr2012:mn05:dow04
#>    strata_total     match_strata
#>           <int>           <char>
#> 1:          147 ACTON:2012-05-01
#> 2:          136 ACTON:2012-05-01
#> 3:          140 ACTON:2012-05-01
#> 4:          145 ACTON:2012-05-02
#> 5:          136 ACTON:2012-05-02
#> 6:          139 ACTON:2012-05-02
```

Now, all of our other functions can stay the same:

Running the model (adding the `verbose` argument so you can follow
along)

``` r

ma_model_fct <- condPois_2stage(ma_exposure_matrix, ma_outcomes_tbl_fct,
                                verbose = 1, global_cen = 15)
#> < age_grp : 0-17 >
#> -- validation passed
#> -- stage 1
#> 
#> crossbasis args for geo_unit  ACTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.7 31.4
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> ACTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> -- mixmeta
#> formula: coef_matrix ~ exp_mean + exp_IQR
#> random: ~ 1 | COUNTY20/TOWN20 
#> -- stage 2
#> truncate final RR and basis cen:  0.01 
#> 
#> < age_grp : 18-64 >
#> -- validation passed
#> -- stage 1
#> 
#> crossbasis args for geo_unit  ACTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.7 31.4
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> ACTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> -- mixmeta
#> formula: coef_matrix ~ exp_mean + exp_IQR
#> random: ~ 1 | COUNTY20/TOWN20 
#> -- stage 2
#> truncate final RR and basis cen:  0.01 
#> 
#> < age_grp : 65+ >
#> -- validation passed
#> -- stage 1
#> 
#> crossbasis args for geo_unit  ACTON :
#> 
#> maxlag: 5 
#> 
#> argvar:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: Named num [1:2] 25.7 31.4
#>   ..- attr(*, "names")= chr [1:2] "50%" "90%"
#> 
#> arglag:
#> List of 2
#>  $ fun  : chr "ns"
#>  $ knots: num [1:2] 0.878 2.095
#> 
#> strata:
#> ACTON:yr2012:mn05:dow03
#> strata_min: 0 
#> 
#> min_n: 50 
#> 
#> truncate final RR and basis cen:  0 
#> formula:
#>    daily_deaths ~ cb
#> family: quasipoisson
#> 
#> -- mixmeta
#> formula: coef_matrix ~ exp_mean + exp_IQR
#> random: ~ 1 | COUNTY20/TOWN20 
#> -- stage 2
#> truncate final RR and basis cen:  0.01
```

And plotting the output

``` r

plot(ma_model_fct, "CAMBRIDGE")
```

![](cityClimateHealth_files/figure-html/plot1b-1.png)

``` r

forest_plot(ma_model_fct, exposure_val = 25.1)
#> Warning in forest_plot.condPois_2stage_list(ma_model_fct, exposure_val = 25.1):
#> plotting by group since n_geos > 20
```

![](cityClimateHealth_files/figure-html/forest_plot2b-1.png)

You can also make a spatial plot at a specific exposure value:

``` r

spatial_plot(ma_model_fct, shp = ma_towns, exposure_val = 25.1)
```

![](cityClimateHealth_files/figure-html/spatial_plot2-1.png)

You can also getRR:

``` r

getRR(ma_model_fct)
#>            TOWN20  COUNTY20 tmax_C        RR      RRlb      RRub  stage age_grp
#>            <char>    <char>  <num>     <num>     <num>     <num> <char>  <char>
#>      1:     ACTON MIDDLESEX    7.0 0.9692331 0.9445686 0.9945417 stage2    0-17
#>      2:     ACTON MIDDLESEX    7.1 0.9692331 0.9445686 0.9945417 stage2    0-17
#>      3:     ACTON MIDDLESEX    7.2 0.9692331 0.9445686 0.9945417 stage2    0-17
#>      4:     ACTON MIDDLESEX    7.3 0.9692331 0.9445686 0.9945417 stage2    0-17
#>      5:     ACTON MIDDLESEX    7.4 0.9692331 0.9445686 0.9945417 stage2    0-17
#>     ---                                                                        
#> 194612: WORCESTER WORCESTER   33.0 1.2047947 1.1612618 1.2499595 stage1     65+
#> 194613: WORCESTER WORCESTER   33.1 1.2058778 1.1610980 1.2523846 stage1     65+
#> 194614: WORCESTER WORCESTER   33.2 1.2069606 1.1609184 1.2548287 stage1     65+
#> 194615: WORCESTER WORCESTER   33.3 1.2080433 1.1607252 1.2572904 stage1     65+
#> 194616: WORCESTER WORCESTER   33.4 1.2091265 1.1605202 1.2597685 stage1     65+
#>                  model_class
#>                       <char>
#>      1: condPois_2stage_list
#>      2: condPois_2stage_list
#>      3: condPois_2stage_list
#>      4: condPois_2stage_list
#>      5: condPois_2stage_list
#>     ---                     
#> 194612: condPois_2stage_list
#> 194613: condPois_2stage_list
#> 194614: condPois_2stage_list
#> 194615: condPois_2stage_list
#> 194616: condPois_2stage_list
```

And finally, you can `calcAN`, note that both `ma_outcomes_tbl_fct` and
`ma_model_fct` need to have factors, again adding the verbose so you can
see the progress

``` r

ma_AN_fct <- calc_AN(ma_model_fct, 
                     ma_outcomes_tbl_fct, 
                     ma_pop_data_long,
                 spatial_agg_type = 'TOWN20', spatial_join_col = 'TOWN20',
                 verbose = 1)
#> -- over-writing `scale` argument with factor scales
#> < age_grp : 0-17 >
#> Warning in calc_AN(model = sub_model, outcomes_tbl = sub_outcomes_tbl, pop_data
#> = sub_pop_data, : some pop data are zero
#> -- validation passed
#> -- estimate in each geo_unit
#> -- summarize by simulation
#> < age_grp : 18-64 >
#> -- validation passed
#> -- estimate in each geo_unit
#> -- summarize by simulation
#> < age_grp : 65+ >
#> -- validation passed
#> -- estimate in each geo_unit
#> -- summarize by simulation
```

And you can plot either one – some empty bars not because there are no
adults there but because this takes the top 20 in each bin, which don’t
have to overlap. Probably a better way to do this in the future but fine
for diagnostics.

``` r

plot(ma_AN_fct, "num", above_MMT = T)
#> Warning in plot.calcAN_list(ma_AN_fct, "num", above_MMT = T): plot elements >
#> 20, subsetting to top 20
#> Warning in plot.calcAN_list(ma_AN_fct, "num", above_MMT = T): plot elements >
#> 20, subsetting to top 20
#> Warning in plot.calcAN_list(ma_AN_fct, "num", above_MMT = T): plot elements >
#> 20, subsetting to top 20
```

![](cityClimateHealth_files/figure-html/calcAN_plot2-1.png)

You can also make use of some additional arguments to get plot subsets,
including `spatial_sub` and `ordered_levels`.

``` r

plot(ma_AN_fct, 'rate', above_MMT = T, 
     spatial_sub = c('BOSTON', 'CAMBRIDGE'),
     ordered_levels = c("0-17", "18-64", "65+"))
```

![](cityClimateHealth_files/figure-html/calcAN_plot2z-1.png)

You can also plot spatially

``` r

spatial_plot(ma_AN_fct, shp = ma_towns, table_type = "num", above_MMT = T)
```

![](cityClimateHealth_files/figure-html/multi_plot3db-1.png)
