#' Run a time series model for a single geographic unit
#' One-stage POOLED time-series version of `condPois_1stage()`.
#' @param exposure_matrix a matrix of exposures, with columns for lag, usually created by `make_exposure_matrix`
#' @param outcomes_tbl a data.table of outcomes, created by `make_outcome_table`; must have date mapping
#' @param argvar a list containing the `argvar` components for the `crossbasis`
#' @param arglag a list containing the `arglag` components for the `crossbasis`
#' @param maxlag an integer of the maximum lag
#' @param min_n an integer describing the minimum number of cases for a single geo_unit
#' @param time_trend one of "annual" or "season". "annual" (default) fits a
#'   single smooth spline over calendar date (ns(time_num, df = df_time)) and
#'   is appropriate for continuous, full-year daily data. "season" is for data
#'   that has been subset to a recurring part of the year (e.g., summer, or
#'   time_subset = list(month = 5:9) in make_outcome_table()/
#'   make_exposure_matrix()) - calendar date jumps discontinuously between
#'   seasons/years in that case, so "season" instead fits factor(year) (to
#'   soak up inter-annual variation) plus a smooth spline over day-of-season
#'   (ns(doy, df = df_time), which restarts each year and captures
#'   the within-season shape of the trend, shared across years).
#' @param df_per_year degrees of freedom PER YEAR OF DATA for the "annual" time
#'   trend spline or PER SEASON for the "seasonal" time spline.
#'   No Default (standard, see: Gasparrini).
#' @param add_dow logical, whether to add factor(dow) to the model as another time trend adjustment.
#' Default TRUE.
#' @param population_col optional, can be incorporated as population offset if that data is available
#' @param global_cen global centering point
#' @param multi_zone must be true
#' @param verbose used to print crossbasis args the first time
#' @param truncate proportion of the exposure distribution to trim from each
#'   tail when choosing the centering point (passed to `get_centered_cp`)
#' @param population_col optional name of a population column in `outcomes_tbl`,
#'   used as an offset; see `add_population_offset()`
#'
#' @importFrom splines ns
#' @importFrom data.table setDT
#' @importFrom dlnm crossbasis
#' @importFrom dlnm crosspred
#' @importFrom dlnm crossreduce
#' @importFrom dlnm logknots
#'
#' @returns a timeseries_1stage model object
#' @export
#'
#' @examples
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
#'                                    outcome_columns, time_subset = time_subset)
#' m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
timeseries_1stage_glm <- function(exposure_matrix, outcomes_tbl,
                                  argvar = NULL,
                                  arglag = NULL,
                                  maxlag = NULL,
                                  min_n = 50,
                                  strata_min = 0,
                                  time_trend = c("annual", "season"),
                                  df_per_year = NULL,
                                  population_col = NULL,
                                  add_dow = TRUE,
                                  global_cen = NULL,
                                  multi_zone = FALSE,
                                  verbose = TRUE,
                                  truncate = 0.025) {

  ## Check 1: that both inputs are the right class of variables
  stopifnot("exposure" %in% class(exposure_matrix))
  stopifnot("outcome" %in% class(outcomes_tbl))

  ## Check 2: do time trend arguments match?
  time_trend <- match.arg(time_trend)

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # RECURSIVE CALL IF FACTOR
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  if("factor" %in% names(attributes(outcomes_tbl)$column_mapping)) {

    factor_col <- attributes(outcomes_tbl)$column_mapping$factor

    unique_fcts <- unlist(unique(outcomes_tbl[, get(factor_col)]))

    fct_outlist <- vector("list", length(unique_fcts))

    for(fct_i in seq_along(fct_outlist)) {

      if(verbose > 0) {
        cat("<",factor_col,":", unique_fcts[fct_i], ">\n")
      }

      # cat("<",factor_col,":", unique_fcts[fct_i], ">\n")
      rr <- which(outcomes_tbl[, get(factor_col)] == unique_fcts[fct_i])
      subset_outcomes_tbl <- outcomes_tbl[rr, , drop = FALSE]
      attributes(subset_outcomes_tbl)$column_mapping$factor <- NULL

      # re-call the function, but with just one subset
      fct_outlist[[fct_i]] <- timeseries_1stage_glm(exposure_matrix = exposure_matrix,
                                              outcomes_tbl = subset_outcomes_tbl,
                                              global_cen = global_cen,
                                              argvar = argvar,
                                              arglag = arglag,
                                              maxlag = maxlag,
                                              min_n = min_n,
                                              strata_min = strata_min,
                                              time_trend = time_trend,
                                              df_per_year = df_per_year,
                                              df_time = df_time,
                                              add_dow = add_dow,
                                              population_col = population_col,
                                              multi_zone = multi_zone,
                                              verbose = verbose,
                                              truncate = truncate)

      fct_outlist[[fct_i]]$factor_col <- factor_col
      fct_outlist[[fct_i]]$factor_val <- unique_fcts[fct_i]

      # also include a scaling factor that can be used to adjust the AN
      # this is useful in scenarios with a temporal collapse factor
      fct_outlist[[fct_i]]$factor_scale <- 1

    }

    names(fct_outlist) = unique_fcts

    class(fct_outlist) = 'timeseries_1stage_list'

    return(fct_outlist)


  }

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # VALIDATIONS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # Generic validation tests
  validated <- input_validation(exposure_matrix, outcomes_tbl)
  exposure_matrix <- validated$exposure_matrix
  outcomes_tbl    <- validated$outcomes_tbl

  # make objects available
  exp_geo_unit_col     <- attributes(exposure_matrix)$column_mapping$geo_unit
  exp_geo_unit_grp_col <- attributes(exposure_matrix)$column_mapping$geo_unit_grp
  exposure_col         <- attributes(exposure_matrix)$column_mapping$exposure

  out_geo_unit_col     <- attributes(outcomes_tbl)$column_mapping$geo_unit
  out_geo_unit_grp_col <- attributes(outcomes_tbl)$column_mapping$geo_unit_grp
  outcome_col          <- attributes(outcomes_tbl)$column_mapping$outcome
  date_col             <- attributes(outcomes_tbl)$column_mapping$date

  ## CHECK 3: make sure date is an actual Date and build a numeric time index + dow
  ##both for annual and seasonal exposure periods

  if(is.null(date_col)) {
    stop("outcomes_tbl must have a `date` entry in its column_mapping to run
         a time-series (non-conditional) model. Unlike a conditional poisson model,
         there is no `strata` to condition on here, so a date column is required for the trend/
         seasonality spline.")
  }

  ## CHECK 4 - minN for all geo_units
  geos_to_remove <- c()
  unique_geos <- unique(outcomes_tbl[, get(out_geo_unit_col)])

  for(geo_i in 1:length(unique_geos)) {
    this_geo <- unique_geos[geo_i]
    rr <- which(outcomes_tbl[, get(out_geo_unit_col)] == this_geo)
    if(sum(outcomes_tbl[rr, get(outcome_col)]) < min_n) {
      geos_to_remove <- c(geos_to_remove, this_geo)
    }
  }

  if(verbose) {
    cat("strata:\n")
    cat(paste(outcomes_tbl$strata[1]))
    cat("\n")
  }

  if(verbose) {
    cat("strata_min:",strata_min, "\n")
    cat("\n")
  }

  if(is.null(df_per_year)) {
    stop("`df_per_year` cannot be null. For Annual, GASPARRINI uses 8. For Seasonal, GASPARRINI uses 4")
  }

  if(length(geos_to_remove) > 0) {
    cat("Removed due to min_n:", geos_to_remove, "\n")
    #
    rr <- which(outcomes_tbl[, get(out_geo_unit_col)] %in% geos_to_remove)
    outcomes_tbl <- outcomes_tbl[-rr, ]
    #
    rr <- which(exposure_matrix[, get(exp_geo_unit_col)] %in% geos_to_remove)
    exposure_matrix <- exposure_matrix[-rr, ]
  }

  # Check 5 multizone. Same semantics as condPois_1stage: multi_zone is a
  # safety flag the user must set to TRUE if >1 geo-units are passed in.
  # A single geo-unit works fine either way.
  out_geo_unit <- sort(unlist(unique(outcomes_tbl[, get(out_geo_unit_col)])))
  if(!multi_zone) {
    if(length(out_geo_unit) != 1)
      stop("N geo_units passed in are > 1, if you are running a 1stage model this means
         you need to set multi_zone = T")
  }

  # CHECK5
  if(!is.null(global_cen)) {
    stopifnot(is.numeric(global_cen))

  }

  # some basic checks of df_per_year
  if(!is.null(df_per_year)) {
    stopifnot(is.numeric(df_per_year))
    stopifnot(df_per_year > 0)
  }

  # TRUNCATE
  stopifnot(length(truncate) == 1)
  stopifnot(truncate >= 0 & truncate <= 0.05)

  # Check for global_cen being in range of exposures!
  exp_range <- range(exposure_matrix[,get(exposure_col)], na.rm = TRUE)

  # Check if global_cen is within the exposure matrix range
  if(!is.null(global_cen)) {
    stopifnot(is.numeric(global_cen))

    exp_range <- range(exposure_matrix[, get(exposure_col)], na.rm = TRUE)
    if (global_cen < exp_range[1] || global_cen > exp_range[2]) {
      stop("global_cen is outside the exposure matrix range!")
    }

  }

  # CHECK 7: population_col, if supplied
  if(!is.null(population_col)) {
    if(!(population_col %in% names(outcomes_tbl))) {
      stop(sprintf("population_col '%s' not found in outcomes_tbl", population_col))
    }
    pop_vec <- outcomes_tbl[, get(population_col)]
    if(!is.numeric(pop_vec)) {
      stop(sprintf("population_col '%s' must be numeric", population_col))
    }
    if(any(is.na(pop_vec))) {
      stop(sprintf("population_col '%s' has NA values -- fill these in or
                   remove those rows before fitting", population_col))
    }
    if(any(pop_vec <= 0)) {
      stop(sprintf("population_col '%s' has values <= 0 -- log(population)
                   is undefined for these rows", population_col))
    }
  }

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # CREATE CROSSBASIS for this single zone
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  cb_out <- make_crossbasis(exposure_matrix = exposure_matrix,
                            outcomes_tbl = outcomes_tbl,
                            unique_geos = unique_geos,
                            maxlag = maxlag,
                            argvar = argvar,
                            arglag = arglag,
                            strata_min = strata_min,
                            min_n = min_n,
                            verbose = verbose)

  cb <- cb_out$cb
  exposure_is_factor <- cb_out$exposure_is_factor
  argvar <- cb_out$argvar

  if(verbose > 0) {
    if(!exposure_is_factor) {
      cat("truncate final RR and basis cen: ", truncate, "\n")
    } else {
      cat("exposure_is_factor means truncate is not applied \n")
    }
  }

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # ADD TIME TERMS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  # day of week
  if(!("dow" %in% names(outcomes_tbl))) {
    outcomes_tbl[, dow := factor(weekdays(get(date_col)))]
  }

  # as a number
  outcomes_tbl[, time_num := as.numeric(get(date_col))]

  # add a year factor
  outcomes_tbl[, yr_fct := factor(data.table::year(get(date_col)))]

  # add a doy of week
  outcomes_tbl[, doy := data.table::yday(get(date_col))]

  ## time spline df
  n_years <- length(unique(outcomes_tbl$yr_fct))

  if(time_trend == 'season') {
    if(n_years < 2) {
      stop("only 1 year of data is present: factor(yr) will have no effect and this is equivalent to just
              fitting the within-season spline alone. Consider time_trend =
              'annual' if you only have a single season/year.")
    }

  }

  # setting df_time
  if(time_trend == "annual") {

    ## 7 df/year is the standard full-year time-series-DLNM default
    df_time <- df_per_year * n_years

  } else {

    ## "season": within-season smooth, doesn't scale with n_years --
    ## inter-annual variation is instead absorbed by factor(yr)
    df_time <- df_per_year

  }

  if(verbose) {
    cat("time_trend:", time_trend, "\n")
    cat("n_years:", n_years, "\n")
    if(time_trend == "annual") {
      cat("df_time for ns(date):", df_time, "\n")
    } else {
      cat("df_time for ns(doy):", df_time, "\n")
      cat("n_seasons (years):", length(unique(outcomes_tbl$yr_fct)), "\n")
    }
    cat("add_dow:", add_dow, "\n\n")
  }



  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # RUN MODEL
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  if(time_trend == "annual") {

    trend_terms <- sprintf("ns(time_num, df = %d)", df_time)

  } else {

    trend_terms <- sprintf("ns(doy, df = %d) * yr_fct", df_time)

  }

  # cross basis and time-trend
  rhs_terms <- c("cb", trend_terms)

  # day of week
  if(add_dow) rhs_terms <- c(rhs_terms, "dow")

  # population
  if(!is.null(population_col)) {
    rhs_terms <- c(rhs_terms, sprintf("offset(log(%s))", population_col))
  }

  # FORMULA
  ff <- as.formula(paste(outcome_col, "~",
                         paste(rhs_terms, collapse = " + ")))

  # printing
  if(verbose) {
    if(!is.null(population_col)) {
      cat("population offset: offset(log(", population_col, "))\n", sep = "")
    } else {
      cat("population offset: none\n")
      if(multi_zone) {
        stop("If multi-zone == TRUE, population needs to be included")
      }
    }
    cat("model formula:\n")
    print(ff, showEnv = F)
    cat("\n")
  }

  # RUN THE MODEL
  ##link to gasparrini repo: how did he define formula, which terms were brought out individually
  # TODO: include population offset here
  m_sub <- glm(formula = ff,
               data = outcomes_tbl,
               family = quasipoisson(link = "log"))

  # get the coefficients
  m_coef <- coef(m_sub)
  m_vcov <- vcov(m_sub)

  # there should be no NAs
  if(any(is.na(m_coef)))
    stop("coef has NULL, something went wrong.
  Usually this happens:
  (1) when strata counts are too low,
  (2) when maxlag is low (< 3) and you haven't adjusted
  argvar and arglag (switching to fun='lin' can be a good starting point)
  (3) if exposure_is_factor then
  you need to make sure that `breaks` is set correctly or
  (4) if you have a covariate that has no variation within each strata -- e.g.,
       if you have US_holiday and you are doing factors by month, August has no
       federal holidays so there will be no contrast for this variable.")

  if(any(is.na(m_vcov))) stop("vcov has NULL, something went wrong")

  # and subset to just the ones for the crossbasis
  rr <- grepl("^cb", names(m_coef))
  m_coef <- m_coef[rr]
  m_vcov <- m_vcov[rr, rr]

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # CROSSPRED and CROSSREDUCE
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  exposure_col <- attributes(exposure_matrix)$column_mapping$exposure
  geo_unit_col <- attributes(outcomes_tbl)$column_mapping$geo_unit
  geo_unit_grp_col <- attributes(outcomes_tbl)$column_mapping$geo_unit_grp

  if(exposure_is_factor) {
    xmode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    exp_mean = xmode(exposure_matrix[, get(exposure_col)])
    exp_IQR = range(exposure_matrix[, get(exposure_col)])
  } else {
    exp_mean = mean(exposure_matrix[, get(exposure_col)])
    exp_IQR = IQR(exposure_matrix[, get(exposure_col)])
  }


  # the crossreduce coefficients are not affected by the centering point
  # but it does make a message if you dont put something there
  # so i center on the min to avoid that fate
  cp <- crosspred(cb,
                  coef = m_coef,
                  vcov = m_vcov,
                  model.link = "log",
                  cen = exp_mean,
                  by = 0.1)

  # if it's centered at global cen use that, otherwise get the cp min
  if(!is.null(global_cen)) {
    cen = global_cen
  } else {
    cen = cp$predvar[which.min(cp$allRRfit)]
  }

  # also save the cp object for first stage analyses
  stage1_cp <- crosspred(cb,
                         coef = m_coef,
                         vcov = m_vcov,
                         model.link = "log",
                         cen = cen,
                         by = 0.1)

  # now apply to cr and export
  cr <- crossreduce(cb,
                    coef = m_coef,
                    vcov = m_vcov,
                    model.link = "log",
                    cen = cen,
                    by = 0.1)



  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # TRUNCATE THE CR
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  #
  if(truncate > 0 & !exposure_is_factor) {
    this_exp <- as.vector(exposure_matrix[, get(exposure_col)])

    # truncate CR
    qX = quantile(this_exp, probs = 1 - truncate)
    rr = which.min(abs(cr$predvar - qX))
    br1 = cr$RRfit[rr]
    br2 = cr$RRlow[rr]
    br3 = cr$RRhigh[rr]
    tt = which(cr$predvar > qX)
    for(i in 1:length(tt)) {
      cr$RRfit[tt[i]] = br1
      cr$RRlow[tt[i]] = br2
      cr$RRhigh[tt[i]] = br3
    }

    qX = quantile(this_exp, probs = 0 + truncate)
    rr = which.min(abs(cr$predvar - qX))
    br1 = cr$RRfit[rr]
    br2 = cr$RRlow[rr]
    br3 = cr$RRhigh[rr]
    tt = which(cr$predvar < qX)
    for(i in 1:length(tt)) {
      cr$RRfit[tt[i]] = br1
      cr$RRlow[tt[i]] = br2
      cr$RRhigh[tt[i]] = br3
    }
  }

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # Make a single centered basis
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  this_exp <- exposure_matrix[, get(exposure_col)]
  x_b <- c(floor(min(this_exp)), ceiling(max(this_exp)))

  centered_basis <- get_centered_cp(argvar = argvar,
                                    xcoef = coef(cr),
                                    xvcov = vcov(cr),
                                    global_cen = global_cen,
                                    cen = cen,
                                    this_exp = this_exp,
                                    x_b = x_b,
                                    exposure_is_factor = exposure_is_factor,
                                    truncate = truncate)

  overall_centered_basis <- centered_basis$basis_cen

  # //////////////////////////////////////////////////////////////////////////
  # ==========================================================================
  # OUTPUT OBJECTS
  # ==========================================================================
  # //////////////////////////////////////////////////////////////////////////

  outcome_columns <- attributes(outcomes_tbl)$column_mapping

  geo_cols <- c(
    outcome_columns$geo_unit,
    outcome_columns$geo_unit_grp
  )
  unique_geos <- unique(outcomes_tbl[, ..geo_cols])

  oo_list <- vector("list", length(out_geo_unit))
  names(oo_list) <- unique_geos[, get(out_geo_unit_col)]

  printerror1 <- T
  printerror2 <- T

  for(i in 1:nrow(unique_geos)) {

    # get the name, which you know exists in both datasets
    this_geo <- unique_geos[i, get(outcome_columns$geo_unit)]
    this_geo_grp <- unique_geos[i, get(outcome_columns$geo_unit_grp)]

    # this cities exposure matrix
    rr <- exposure_matrix[, get(exp_geo_unit_col)] == this_geo
    single_exposure_matrix = exposure_matrix[rr, ,drop = FALSE]
    this_exp <- single_exposure_matrix[, get(exposure_col)]
    x_b <- c(floor(min(this_exp)), ceiling(max(this_exp)))
    this_exp_mean = mean(single_exposure_matrix[, get(exposure_col)])
    this_exp_IQR = IQR(single_exposure_matrix[, get(exposure_col)])
    if(cen < x_b[1] | cen > x_b[2]) {
      warning(sprintf(
        "Centering point is outside the range of exposures in geo-unit %s: Cen = %s, x_b = %s.
        This means your zones are across too large of an area, or if exposure is factor there could
        be too few events in this area, or
        there are differences in exposures so much that the bases are quite different. Try limiting the geo-units passed in to those that are more similar, manually setting a centering point that you know each geo-unit has, or changing your exposure variable.",
        this_geo, sprintf("%1.2f", cen), sprintf("(%1.2f, %1.2f)", x_b[1], x_b[2])
      ))
    }

    # another check
    centered_check <- tryCatch({
      get_centered_cp(argvar = argvar,
                      xcoef = coef(cr),
                      xvcov = vcov(cr),
                      global_cen = global_cen,
                      cen = cen,
                      this_exp = this_exp,
                      x_b = x_b,
                      exposure_is_factor = exposure_is_factor,
                      truncate = truncate
      )
    }, error = function(e) {
      warning(sprintf('a check of making the centered basis for a geo-unit %s did not pass. this likely means that the knots for the overall basis are outside the range of exposures in this geographic unit. Consider adjusting either the geo-units you are passing in, or the exposure variable (e.g., switching from absolute to relative measures)', this_geo))
    })

    # this cities cb, with attributes!
    rr <- exposure_matrix[, get(exp_geo_unit_col)] == this_geo
    this_cb <- cb[rr, ]
    cb_att <- attributes(cb)

    # reset-dim --> another little trick here!
    if(!is.null(dim(this_cb))) {
      cb_att$dim = dim(this_cb)
      attributes(this_cb) = cb_att
    } else {
      if(printerror1) {
        warning("dim(cb) is NULL so output dim was not reset.
            Unlikely to happen, so investigate.")
        printerror1 <- F
      }
    }

    # do the same thing with centered_cb
    this_centered_cb <- overall_centered_basis[rr, ]
    cb_cen_att <- attributes(overall_centered_basis)

    # reset-dim --> another little trick here!
    if(!is.null(dim(this_centered_cb))) {
      cb_cen_att$dim = dim(this_centered_cb)
      attributes(this_centered_cb) = cb_cen_att
    } else{
      if(printerror2) {
        warning("dim(cr) is NULL so output dim was not reset.
      Probably happens when there are too few columns that result from argvar and arglag.
              Unclear what the consequences are yet ...")
        printerror2 <- F
      }
    }

    # this city's outcome
    rr <- outcomes_tbl[, get(out_geo_unit_col)] == this_geo
    single_outcomes_tbl = outcomes_tbl[rr, ,drop = FALSE]
    outcomes <- single_outcomes_tbl[, get(outcome_col)]

    # and get centered crosspred
    # Ah ha! this fails is cen is outside of x_b
    #


    # each of these things you need for BLUP and MIXMETA later
    oo_list[[i]] <- list(geo_unit = this_geo,     ## --> individual
                         geo_unit_grp = this_geo_grp,       ## --> individual
                         basis_cen = this_centered_cb,      ## --> individual
                         strata_vec = single_outcomes_tbl$strata, ## --> individual
                         match_strata = single_outcomes_tbl$match_strata, ## --> individual
                         exposure_is_factor = exposure_is_factor,
                         orig_basis = this_cb,              ## --> individual
                         orig_coef = m_coef,                ## whole group
                         orig_vcov = m_vcov,                ## whole group
                         cr = cr,                           ## whole group
                         stage1_cp = stage1_cp,             ## whole group
                         coef = coef(cr),                   ## whole group
                         vcov = vcov(cr),                   ## whole group
                         exposure_col = exposure_col,       ## whole group
                         this_exp = this_exp,               ## --> individual
                         outcomes = outcomes,               ## --> individual
                         cen = cen,                         ## whole group
                         global_cen = global_cen,           ## whole group
                         argvar = argvar,                   ## whole group
                         exp_mean = exp_mean,               ## whole group
                         exp_IQR = exp_IQR)                 ## whole group

  }

  outlist = list(list(out = oo_list))
  names(outlist) = "_"
  class(outlist) <- 'timeseries_1stage'

  return(outlist)

}


#' Print method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @param ... further arguments passed to or from other methods (currently unused)
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(), class = "timeseries_1stage")
#' print(x)
print.timeseries_1stage <- function(x, ...) {
  cat("< an object of class `timeseries_1stage` >\n")
  invisible(x)
}


#' Print method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#' @param ... further arguments passed to or from other methods (currently unused)
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "timeseries_1stage_list")
#' print(x)
print.timeseries_1stage_list <- function(x, ...) {
  cat("< an object of class `timeseries_1stage_list`:",
      paste(names(x), collapse = ",")," >\n")
}


#' getRR method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @param ... further arguments passed to or from other methods (currently unused)
#' @importFrom data.table setDT
#' @returns a data.table of relative risk estimates
#' @export
#'
#' @examples
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
#'                                    outcome_columns, time_subset = time_subset)
#' m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#' getRR(m)
getRR.timeseries_1stage <- function(x, ...) {

  n_geo_names <- paste0(names(x$`_`$out), collapse = ":")

  if(nchar(n_geo_names) > 20)
    n_geo_names = paste0(substr(n_geo_names, 1, 15), "...(truncated)")

  plot_cp = data.frame(
    x = x$`_`$out[[1]]$cr$predvar,
    RR = x$`_`$out[[1]]$cr$RRfit,
    RRlb = x$`_`$out[[1]]$cr$RRlow,
    RRub = x$`_`$out[[1]]$cr$RRhigh,
    n_geo_names = n_geo_names,
    model_class = class(x)
  )

  names(plot_cp)[1] <- x$`_`$out[[1]]$exposure_col

  setDT(plot_cp)

  return(plot_cp)
}



#' Plot method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @param xlab xlab override
#' @param ylab ylab override
#' @param title title override
#' @param ... further arguments passed to or from other methods (currently unused)
#' @returns a ggplot object
#' @export
#'
#' @examples
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
#'                                    outcome_columns, time_subset = time_subset)
#' m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#' plot(m)
plot.timeseries_1stage <- function(x, xlab = NULL, ylab = NULL, title = NULL, ...) {

  n_geo_names <- paste0(names(x$`_`$out), collapse = ":")

  # these will all be the same, so just pick 1
  plot_cp = getRR(x)

  if(is.null(xlab)) xlab = x$`_`$out[[1]]$exposure_col
  if(is.null(ylab)) ylab = "RR"
  if(is.null(title)) title = n_geo_names

  ggplot(plot_cp, aes(x = !!sym(names(plot_cp)[1]),
                      y = RR, ymin = RRlb, ymax = RRub)) +
    geom_hline(yintercept = 1, linetype = '11') +
    theme_classic() +
    ggtitle(title) +
    scale_y_continuous(transform = 'log') +
    geom_ribbon(fill = 'lightblue', alpha = 0.2) +
    geom_line() + xlab(xlab) + ylab(ylab)
}

#' getRR method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#' @param ... further arguments passed to or from other methods (currently unused)
#' @importFrom data.table setDT
#' @returns a data.table of relative risk estimates across factor levels
#' @export
#'
#' @examples
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "factor" = "age_grp",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
#'                                    outcome_columns, time_subset = time_subset)
#' # with a factor, a timeseries_1stage_list is returned
#' m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#' getRR(m)
getRR.timeseries_1stage_list <- function(x, ...) {

  fct_names <- names(x)

  plot_cl_l <- vector("list", length(names(x)))

  for(i in 1:length(names(x))) {

    # these will all be the same so just pick the first one
    plot_cl_l[[i]] = data.frame(
      x = x[[names(x)[i]]]$`_`$out[[1]]$cr$predvar,
      fct = names(x)[i],
      RR = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRfit,
      RRlb = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRlow,
      RRub = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRhigh
    )

    factor_col <- x[[names(x)[i]]]$factor_col
    names(plot_cl_l[[i]])[2] <- factor_col

    exp_col <- x[[names(x)[i]]]$`_`$out[[1]]$exposure_col
    names(plot_cl_l[[i]])[1] <- exp_col

  }

  plot_cp <- do.call(rbind, plot_cl_l)
  plot_cp$model_class = class(x)

  setDT(plot_cp)

  return(plot_cp)
}

#' Plot method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#' @param xlab xlab override
#' @param ylab ylab override
#' @param title title override
#' @param ... further arguments passed to or from other methods (currently unused)
#' @returns a ggplot object
#' @export
#'
#' @examples
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "factor" = "age_grp",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 == "BOSTON"),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 == "BOSTON"),
#'                                    outcome_columns, time_subset = time_subset)
#' # with a factor, a timeseries_1stage_list is returned
#' m <- timeseries_1stage_glm(exposure_mat, outcomes_tbl)
#' plot(m)
plot.timeseries_1stage_list <- function(x, xlab = NULL, ylab = NULL, title = NULL, ...) {

  fct_names <- names(x)

  plot_cl_l <- vector("list", length(names(x)))

  for(i in 1:length(names(x))) {

    # these will all be the same so just pick the first one
    plot_cl_l[[i]] = data.frame(
      x = x[[names(x)[i]]]$`_`$out[[1]]$cr$predvar,
      fct = names(x)[i],
      RR = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRfit,
      RRlb = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRlow,
      RRub = x[[names(x)[i]]]$`_`$out[[1]]$cr$RRhigh
    )

    n_geo_names <- paste0(names(x[[names(x)[i]]]$`_`$out), collapse = ":")
    factor_col <- x[[names(x)[i]]]$factor_col
    names(plot_cl_l[[i]])[2] <- factor_col

  }

  plot_cp <- do.call(rbind, plot_cl_l)

  if(is.null(xlab)) xlab = x[[1]]$exposure_col
  if(is.null(ylab)) ylab = "RR"
  if(is.null(title)) title = n_geo_names

  ggplot(plot_cp,
         aes(x = !!sym(names(plot_cp)[1]), y = RR,
             ymin = RRlb, ymax = RRub)) +
    geom_hline(yintercept = 1, linetype = '11') +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic() +
    ggtitle(title) +
    scale_y_continuous(transform = 'log') +
    geom_ribbon(aes(fill = !!sym(factor_col)), alpha = 0.2) +
    geom_line(aes(color = !!sym(factor_col))) +
    xlab(xlab) + ylab(ylab)

}



#' forest_plot method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
forest_plot.timeseries_1stage <- function(x, ...) {
  warning("`forest_plot` method not implemented for objects of class `timeseries_1stage`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}



#' forest_plot method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
forest_plot.timeseries_1stage_list <- function(x, ...) {
  warning("`forest_plot` method not implemented for objects of class `timeseries_1stage_list`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}



#' spatial_plot method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
spatial_plot.timeseries_1stage <- function(x, ...) {
  warning("`spatial_plot` method not implemented for objects of class `timeseries_1stage`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}


#' spatial_plot method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
spatial_plot.timeseries_1stage_list <- function(x, ...) {
  warning("`spatial_plot` method not implemented for objects of class `timeseries_1stage_list`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}
