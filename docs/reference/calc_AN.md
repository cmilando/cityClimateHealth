# Calculate attributable number and attributable rates

Calculate attributable number and attributable rates

## Usage

``` r
calc_AN(
  model,
  outcomes_tbl,
  pop_data,
  spatial_agg_type,
  spatial_join_col,
  by_year = FALSE,
  scale = 1,
  nsim = 300,
  verbose = 0
)
```

## Arguments

- model:

  a model object of class `condPois_2stage`, `condPois_1stage`, or
  `condPois_sb`

- outcomes_tbl:

  a table of outcomes, of class `outcomes`

- pop_data:

  population data

- spatial_agg_type:

  what is the spatial resolution you are aggregating to

- spatial_join_col:

  how should you join population data to the outcome table

- by_year:

  Export annual counts

- scale:

  scalar that you multiply outcomes by, mostly used for
  factor_is_temporal adjustment

- nsim:

  number of simulations required for calculation of empirical CI
  (default = 300)

- verbose:

  0 = no printing, 1 = headers, 2 = detailed

## Value

calc_AN object that contains attributable numbers, rate tables, and
other components necessary for plotting AN

## Examples

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

ma_pop_data_long <- melt(
  ma_pop_data,
  id.vars = "TOWN20",
  variable.name = "sex_age",
  value.name = "population"
)


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

library(data.table)
exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

ma_exposure_matrix <- make_exposure_matrix(
  subset(ma_exposure, COUNTY20 %in% c('MIDDLESEX', 'WORCESTER') &
           year(date) %in% 2012:2015),
  exposure_columns)
#> -- NA values automatically removed
#> Error in time_subset_validate(time_subset, data_years): A `time_subset` must be explicitly provided, e.g. list(month = 5:9).
#>     To indicate using all available time, put time_subset = 'use_all'

outcome_columns <- list(
  "date" = "date",
  "outcome" = "daily_deaths",
  "factor" = 'age_grp',
  "factor" = 'sex',
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)

ma_outcomes_tbl <- make_outcome_table(
  subset(ma_deaths,COUNTY20 %in% c('MIDDLESEX', 'WORCESTER') &
           year(date) %in% 2012:2015), outcome_columns)
#> Error in time_subset_validate(time_subset, data_years): A `time_subset` must be explicitly provided, e.g. list(month = 5:9).
#>     To indicate using all available time, put time_subset = 'use_all'

ma_model <- condPois_2stage(ma_exposure_matrix, ma_outcomes_tbl, verbose = 1, global_cen = 20)
#> Error: object 'ma_exposure_matrix' not found

ma_AN <- calc_AN(ma_model, ma_outcomes_tbl, ma_pop_data_long,
                 spatial_agg_type = 'TOWN20',
                 spatial_join_col = 'TOWN20',
                 nsim = 100,
                 verbose = 2)
#> Error: object 'ma_model' not found
calc_AN
#> function (model, outcomes_tbl, pop_data, spatial_agg_type, spatial_join_col, 
#>     by_year = FALSE, scale = 1, nsim = 300, verbose = 0) 
#> {
#>     stopifnot(class(model) %in% c("condPois_1stage", "condPois_1stage_list", 
#>         "condPois_2stage", "condPois_2stage_list", "condPois_sb", 
#>         "condPois_sb_list"))
#>     stopifnot("outcome" %in% class(outcomes_tbl))
#>     stopifnot("population" %in% names(pop_data))
#>     stopifnot(all(spatial_join_col %in% names(pop_data)))
#>     stopifnot(is.data.table(pop_data))
#>     if (("factor" %in% names(attributes(outcomes_tbl)$column_mapping)) & 
#>         grepl("_list", class(model))) {
#>         factor_col <- attributes(outcomes_tbl)$column_mapping$factor
#>         unique_fcts <- unlist(unique(outcomes_tbl[, get(factor_col)]))
#>         fct_outlist <- vector("list", length(unique_fcts))
#>         cat("-- over-writing `scale` argument with factor scales\n")
#>         for (fct_i in seq_along(fct_outlist)) {
#>             if (verbose > 0) {
#>                 cat("<", factor_col, ":", unique_fcts[fct_i], 
#>                   ">\n")
#>             }
#>             sub_model <- model[[unique_fcts[fct_i]]]
#>             stopifnot(class(sub_model) %in% c("condPois_1stage", 
#>                 "condPois_2stage", "condPois_sb"))
#>             stopifnot("_" %in% names(sub_model))
#>             stopifnot(factor_col %in% names(outcomes_tbl))
#>             rr <- which(outcomes_tbl[, get(factor_col)] == unique_fcts[fct_i])
#>             sub_outcomes_tbl <- outcomes_tbl[rr, , drop = FALSE]
#>             attributes(sub_outcomes_tbl)$column_mapping$factor <- NULL
#>             stopifnot(factor_col %in% names(pop_data))
#>             rr <- which(pop_data[, get(factor_col)] == unique_fcts[fct_i])
#>             sub_pop_data <- pop_data[rr, , drop = FALSE]
#>             factor_scale = sub_model$factor_scale
#>             fct_outlist[[fct_i]] <- calc_AN(model = sub_model, 
#>                 outcomes_tbl = sub_outcomes_tbl, pop_data = sub_pop_data, 
#>                 spatial_agg_type = spatial_agg_type, spatial_join_col = spatial_join_col, 
#>                 by_year = by_year, scale = factor_scale, nsim = nsim, 
#>                 verbose = verbose)
#>             fct_outlist[[fct_i]]$factor_col <- factor_col
#>             fct_outlist[[fct_i]]$factor_val <- unique_fcts[fct_i]
#>         }
#>         names(fct_outlist) = unique_fcts
#>         class(fct_outlist) = "calcAN_list"
#>         return(fct_outlist)
#>     }
#>     outcomes_col <- attributes(outcomes_tbl)$column_mapping$outcome
#>     date_col <- attributes(outcomes_tbl)$column_mapping$date
#>     geo_unit_col <- attributes(outcomes_tbl)$column_mapping$geo_unit
#>     geo_unit_grp_col <- attributes(outcomes_tbl)$column_mapping$geo_unit_grp
#>     stopifnot(spatial_agg_type %in% c(geo_unit_col, geo_unit_grp_col, 
#>         "all"))
#>     stopifnot(length(spatial_join_col) == 1)
#>     stopifnot(length(spatial_agg_type) == 1)
#>     setDT(pop_data)
#>     stopifnot(all(spatial_join_col %in% names(pop_data)))
#>     pop_data_collapse <- pop_data[, .(population = sum(population)), 
#>         by = spatial_join_col]
#>     if (any(pop_data_collapse$population == 0)) {
#>         warning("some pop data are zero")
#>         pop_data_collapse <- subset(pop_data_collapse, population > 
#>             0)
#>     }
#>     safe_geos <- unique(pop_data_collapse[, ..spatial_join_col])
#>     outcomes_tbl <- safe_geos[outcomes_tbl, on = spatial_join_col, 
#>         nomatch = 0]
#>     if (verbose > 0) {
#>         cat("-- validation passed\n")
#>     }
#>     if (verbose > 0) {
#>         cat("-- estimate in each geo_unit\n")
#>     }
#>     x <- model$`_`
#>     n_geo_units <- length(x$out)
#>     if (!(n_geo_units >= 1)) {
#>         stop("the model object is empty, check that model and outcomes are the same type\n         (e.g., that one is not a `_list` type while the other is a standard.")
#>     }
#>     stopifnot(n_geo_units >= 1)
#>     AN <- vector("list", n_geo_units)
#>     exposure_col <- x$out[[1]]$exposure_col
#>     for (i in 1:n_geo_units) {
#>         if (verbose > 1) {
#>             if (i%%5 == 0) 
#>                 cat(i, "\t")
#>         }
#>         this_geo <- x$out[[i]]$geo_unit
#>         basis_cen <- x$out[[i]]$basis_cen
#>         xcoef <- x$out[[i]]$coef
#>         xvcov <- x$out[[i]]$vcov
#>         outcomes <- x$out[[i]]$outcomes
#>         match_strata <- x$out[[i]]$match_strata
#>         this_exp <- x$out[[i]]$this_exp
#>         cen <- x$out[[i]]$cen
#>         global_cen <- x$out[[i]]$global_cen
#>         required_fields <- c("geo_unit", "basis_cen", "coef", 
#>             "vcov", "outcomes", "match_strata", "this_exp", "cen", 
#>             "global_cen")
#>         null_fields <- required_fields[sapply(required_fields, 
#>             function(f) is.null(x$out[[i]][[f]]))]
#>         if (length(null_fields) > 0) {
#>             stop(paste("The following required field(s) are NULL in x$out[[", 
#>                 i, "]]:", paste(null_fields, collapse = ", ")))
#>         }
#>         if (!(this_geo %in% as.vector(unlist(safe_geos)))) {
#>             cat("skipping based on no population data: ", this_geo, 
#>                 "\t")
#>             next
#>         }
#>         rr <- which(outcomes_tbl$match_strata %in% match_strata)
#>         single_outcomes_tbl <- outcomes_tbl[rr, , drop = FALSE]
#>         date_fmt <- single_outcomes_tbl[, get(date_col)]
#>         if (!identical(single_outcomes_tbl[, get(outcomes_col)], 
#>             outcomes)) {
#>             cat("This geo:\n")
#>             cat(this_geo, "\n")
#>             cat("Outcomes table being passed in:\n")
#>             print(head(single_outcomes_tbl))
#>             cat("Outcomes vector associated with the model object for this geo:\n")
#>             print(head(outcomes))
#>             stop("Outcomes not the same - usually this means there is a spatial\n           mismatch somewhere in one of the other datasets - in this case\n           either outcomes_tbl or the model doesn't have the spatial resolution\n           that you are asking for. Check that each one has the spatial units\n           and each level that are required for matching.")
#>         }
#>         bvar_mat <- as.matrix(basis_cen)
#>         af_updated <- (1 - exp(-bvar_mat %*% xcoef))
#>         if (any(af_updated < -0.001) & is.null(global_cen)) {
#>             print(summary(af_updated))
#>             stop(paste0("Attributable Fraction (AF) < -0.001 for ", 
#>                 this_geo, "and global_cen is NULL,\n                  which means that centering was likely not done\n                  correctly in an earlier step"))
#>         }
#>         coefsim <- MASS::mvrnorm(nsim, xcoef, xvcov)
#>         AN_SIM <- vector("list", nsim)
#>         for (s in seq(nsim)) {
#>             AN_SIM[[s]] <- (1 - exp(-bvar_mat %*% coefsim[s, 
#>                 ])) * outcomes
#>         }
#>         AN_SIM_mat <- data.table(do.call(cbind, AN_SIM))
#>         colnames(AN_SIM_mat) <- paste0("X", 1:nsim)
#>         AN_SIM_mat$this_exp = this_exp
#>         AN_SIM_mat$cen = cen
#>         AN_SIM_mat$date_fmt = date_fmt
#>         AN_SIM_mat[, `:=`((colnames(single_outcomes_tbl)), single_outcomes_tbl)]
#>         AN_SIM_mat[, `:=`(year, year(AN_SIM_mat[, get(date_col)]))]
#>         AN[[i]] <- AN_SIM_mat
#>     }
#>     if (verbose > 1) {
#>         cat("\n")
#>     }
#>     if (verbose > 0) {
#>         cat("-- summarize by simulation\n")
#>     }
#>     geo_cols <- c(geo_unit_col, geo_unit_grp_col)
#>     unique_geos <- unique(outcomes_tbl[, ..geo_cols])
#>     unique_years <- unique(year(outcomes_tbl[, get(date_col)]))
#>     xgrid <- tidyr::expand_grid(data.frame(unique_geos), year = unique_years, 
#>         above_MMT = c(T, F))
#>     setDT(xgrid)
#>     AN_ANNUAL <- vector("list", nsim)
#>     for (xi in 1:nsim) {
#>         if (verbose > 1) {
#>             if (xi%%5 == 0) 
#>                 cat(xi, "\t")
#>         }
#>         this_col <- paste0("X", xi)
#>         AN_sub <- vector("list", n_geo_units)
#>         get_cols <- c(geo_unit_col, geo_unit_grp_col, "this_exp", 
#>             "year", "cen", "date_fmt", this_col)
#>         for (ani in 1:n_geo_units) {
#>             if (!is.null(AN[[ani]])) {
#>                 setDT(AN[[ani]])
#>                 x1 <- AN[[ani]][, ..get_cols]
#>                 x1$nsim <- xi
#>                 AN_sub[[ani]] <- x1
#>             }
#>         }
#>         AN_sub_all <- do.call(rbind, AN_sub)
#>         names(AN_sub_all)[length(get_cols)] <- "attributable_number"
#>         AN_sub_all$above_MMT = AN_sub_all$this_exp >= AN_sub_all$cen
#>         group_cols = c(geo_unit_col, geo_unit_grp_col, "year", 
#>             "above_MMT")
#>         AN_ANNUAL[[xi]] <- AN_sub_all[, .(annual_AN = round(sum(attributable_number))), 
#>             by = group_cols]
#>         sum_AN_orig <- sum(AN_ANNUAL[[xi]]$annual_AN)
#>         AN_ANNUAL[[xi]] <- AN_ANNUAL[[xi]][xgrid, on = setNames(group_cols, 
#>             group_cols)]
#>         AN_ANNUAL[[xi]]$nsim = xi
#>         rr <- which(is.na(AN_ANNUAL[[xi]]$annual_AN))
#>         AN_ANNUAL[[xi]]$annual_AN[rr] <- 0
#>         if (any(is.na(AN_ANNUAL[[xi]]))) 
#>             stop("AN expand_grid didn't work correctly")
#>         AN_ANNUAL[[xi]] <- pop_data_collapse[AN_ANNUAL[[xi]], 
#>             on = setNames(spatial_join_col, spatial_join_col)]
#>         sum_AN_new <- sum(AN_ANNUAL[[xi]]$annual_AN)
#>         if (!(sum_AN_new == sum_AN_orig)) {
#>             stop("sum_AN after merge is not the same, some error is happening")
#>         }
#>         if (spatial_agg_type == geo_unit_col) {
#>             invisible(1)
#>         }
#>         else if (spatial_agg_type == geo_unit_grp_col) {
#>             group_cols = c(geo_unit_grp_col, "year", "nsim", 
#>                 "above_MMT")
#>             AN_ANNUAL[[xi]] = AN_ANNUAL[[xi]][, .(annual_AN = sum(annual_AN, 
#>                 na.rm = T), population = sum(population, na.rm = T)), 
#>                 by = group_cols]
#>         }
#>         else {
#>             AN_ANNUAL[[xi]]$all <- "ALL"
#>             group_cols = c("all", "year", "nsim", "above_MMT")
#>             AN_ANNUAL[[xi]] = AN_ANNUAL[[xi]][, .(annual_AN = sum(annual_AN, 
#>                 na.rm = T), population = sum(population, na.rm = T)), 
#>                 by = group_cols]
#>         }
#>     }
#>     AN_ANNUAL <- do.call(rbind, AN_ANNUAL)
#>     if (verbose > 1) {
#>         cat("\n")
#>     }
#>     c1 <- which(!(names(AN_ANNUAL) %in% c("population", "year", 
#>         "nsim", "annual_AN", "above_MMT")))
#>     g1_cols <- c(names(AN_ANNUAL)[c1], "population", "nsim", 
#>         "above_MMT")
#>     if (verbose > 1) {
#>         cat("-- applying scale of : ", scale, "\n")
#>     }
#>     if (by_year == TRUE) {
#>         x1 <- AN_ANNUAL[, .(mean_annual_AN = mean(annual_AN) * 
#>             scale), by = c(g1_cols, "year")]
#>     }
#>     else {
#>         x1 <- AN_ANNUAL[, .(mean_annual_AN = mean(annual_AN) * 
#>             scale), by = g1_cols]
#>     }
#>     if (any(is.na(x1$mean_annual_AN))) {
#>         rr <- which(is.na(x1$mean_annual_AN))
#>         print(x1[rr, ])
#>         warning("annual_AN has NA, find out why")
#>         return(x1)
#>     }
#>     if (any(is.na(x1$population))) {
#>         rr <- which(is.na(x1$population))
#>         print(x1[rr, ])
#>         warning("population has NA, find out why")
#>         return(x1)
#>     }
#>     x1[, `:=`(mean_annual_AN_rate, mean_annual_AN/population * 
#>         1e+05)]
#>     if (any(is.na(x1$mean_annual_AN_rate))) {
#>         rr <- which(is.na(x1$mean_annual_AN_rate))
#>         print(x1[rr, ])
#>         warning("mean_annual_AN_rate has NA, find out why")
#>         return(x1)
#>     }
#>     if (by_year == TRUE) {
#>         g2_cols <- c(names(AN_ANNUAL)[c1], "year", "population", 
#>             "above_MMT")
#>     }
#>     else {
#>         g2_cols <- c(names(AN_ANNUAL)[c1], "population", "above_MMT")
#>     }
#>     rate_table <- x1[, .(mean_annual_attr_rate_est = quantile(mean_annual_AN_rate, 
#>         0.5), mean_annual_attr_rate_lb = quantile(mean_annual_AN_rate, 
#>         0.025), mean_annual_attr_rate_ub = quantile(mean_annual_AN_rate, 
#>         0.975)), by = g2_cols]
#>     number_table <- x1[, .(mean_annual_attr_num_est = quantile(mean_annual_AN, 
#>         0.5), mean_annual_attr_num_lb = quantile(mean_annual_AN, 
#>         0.025), mean_annual_attr_num_ub = quantile(mean_annual_AN, 
#>         0.975)), by = g2_cols]
#>     outlist <- list(list(rate_table = rate_table, number_table = number_table))
#>     names(outlist) = "_"
#>     class(outlist) <- "calcAN"
#>     return(outlist)
#> }
#> <bytecode: 0x393624388>
#> <environment: namespace:cityClimateHealth>
```
