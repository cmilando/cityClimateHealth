# spatial_plot method for calcAN_list

spatial_plot method for calcAN_list

## Usage

``` r
# S3 method for class 'calcAN_list'
spatial_plot(x, shp, table_type, above_MMT, ...)
```

## Arguments

- x:

  an object of class calcAN_list

- shp:

  an sf shapefile with an appropriate column at which to join

- table_type:

  showing the rate table "rate" or number table "num"

- above_MMT:

  plot attributable numbers above or below the MMT

- ...:

  further arguments passed to or from other methods (currently unused)

## Value

a spatial plot showing attributable numbers or rates, with one graph for
each factor level

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
library(data.table)
pop <- melt(as.data.table(ma_pop_data), id.vars = "TOWN20",
            variable.name = "sex_age", value.name = "population")
pop[, age_grp := sub(".*_", "", sex_age)]
#>                TOWN20     sex_age population age_grp
#>                <char>      <fctr>      <num>  <char>
#>    1:      BARNSTABLE Female_0-17       3899    0-17
#>    2:          BOURNE Female_0-17       1891    0-17
#>    3:        BREWSTER Female_0-17        634    0-17
#>    4:         CHATHAM Female_0-17        163    0-17
#>    5:          DENNIS Female_0-17        573    0-17
#>   ---                                               
#> 2102:   WEST BOYLSTON    Male_65+        790     65+
#> 2103: WEST BROOKFIELD    Male_65+        495     65+
#> 2104:     WESTMINSTER    Male_65+       1081     65+
#> 2105:      WINCHENDON    Male_65+        924     65+
#> 2106:       WORCESTER    Male_65+      11173     65+
pop <- pop[TOWN20 %in% towns, .(population = sum(population)),
           by = .(TOWN20, age_grp)]
an <- calc_AN(m, outcomes_tbl, pop, spatial_agg_type = "TOWN20",
              spatial_join_col = "TOWN20", nsim = 20)
#> -- over-writing `scale` argument with factor scales
spatial_plot(an, shp = subset(ma_towns, TOWN20 %in% towns),
             table_type = "num", above_MMT = TRUE)
```
