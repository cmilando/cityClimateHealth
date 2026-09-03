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
#'   (ns(day_of_season, df = df_time), which restarts each year and captures
#'   the within-season shape of the trend, shared across years).
#' @param dfseas degrees of freedom PER YEAR OF DATA for the "annual" time
#'   trend spline. Default 7 (standard, see: Gasparrini). Ignored
#'   if df_time is supplied directly, and ignored entirely when
#'   time_trend = "season" (see df_time).
#' @param df_time degrees of freedom for the time-trend spline. Meaning
#'   depends on time_trend: for "annual" this is the total df for
#'   ns(date, df = df_time) and if null is computed as round(dfseas * n_years).
#'   For "season" this is the df for the within-season smooth,
#'   ns(day_of_season, df = df_time), and if null defaults to 4.
#' @param add_dow logical, whether to add factor(dow) to the model as another time trend adjustment.
#' Default TRUE.
#' #' @param population_col optional, can be incorporated as population offset if that data is available
#' @param global_cen global centering point
#' @param multi_zone must be true
#' @param verbose used to print crossbasis args the first time
#'
#' @importFrom data.table setDT
#' @importFrom dlnm crossbasis
#' @importFrom dlnm crosspred
#' @importFrom dlnm crossreduce
#' @importFrom dlnm logknots
#' @importFrom glm
#'
#' @returns a timeseries_1stage model object
#' @export
#'
#' @examples
#' library(dlnm)
#' library(glm)
#' library(ggplot2)
#' library(data.table)
#' # create exposure matrix
#' exposure_columns <- list(
#'   "date" = "date",
#'  "exposure" = "tmax_C",
#'  "geo_unit" = "TOWN20",
#'  "geo_unit_grp" = "COUNTY20"
#')
#' boston_exposure_mat <- make_exposure_matrix(
#' subset(ma_exposure, TOWN20 == 'BOSTON'),
#' exposure_columns)
#'
#'# create outcome table
#'outcome_columns <- list(
#'  "date" = "date",
#'  "outcome" = "daily_deaths",
#'  "factor" = 'age_grp',
#'  "factor" = 'sex',
#'  "geo_unit" = "TOWN20",
#'  "geo_unit_grp" = "COUNTY20"
#')
#'boston_deaths_tbl <- make_outcome_table(boston_deaths,  outcome_columns)
#'
#'# run the model
#' m1 <- timeseries_1stage(exposure_matrix = boston_exposure_mat,
#'                     outcomes_tbl = boston_deaths_tbl)
#' timeseries_1stage
timeseries_1stage_glm <- function(exposure_matrix, outcomes_tbl,
                                     argvar = NULL, arglag = NULL, maxlag = NULL,
                                     min_n = 50, time_trend = c("annual", "season"),
                                     dfseas = 7, df_time = NULL,
                                     population_col = NULL,
                                     add_dow = TRUE, global_cen = NULL,
                                     multi_zone = FALSE, verbose = TRUE) {

  ## Check 1: that both inputs are the right class of variables
  stopifnot("exposure" %in% class(exposure_matrix))
  stopifnot("outcome" %in% class(outcomes_tbl))

  ##Check 2: do time trend arguments match?
  time_trend <- match.arg(time_trend)

  ###############################################
  #' IF the outcomes_tbl has a FACTOR, enter a recursive loop
  ###############################################
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
                                              time_trend = time_trend,
                                              dfseas = dfseas,
                                              df_time = df_time,
                                              add_dow = add_dow,
                                              population_col = population_col,
                                              multi_zone = multi_zone,
                                              verbose = verbose)

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

  ###############################################
  # VALIDATIONS
  ###############################################

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

  outcomes_tbl[, (date_col) := as.Date(get(date_col))]
  if(!("dow" %in% names(outcomes_tbl))) {
    outcomes_tbl[, dow := factor(weekdays(get(date_col)))]
  }
  outcomes_tbl[, time_num := as.numeric(get(date_col))]

  outcomes_tbl[, yr := factor(data.table::year(get(date_col)))]
  outcomes_tbl[, day_of_season := data.table::yday(get(date_col))]

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
  n_geo <- length(out_geo_unit)

  if(!multi_zone & n_geo != 1) {
    stop("N geo_units passed in are > 1, if you are running a 1stage model this
         means you need to set multi_zone = TRUE")
  }

  # whether to include factor(geo_unit) main effect + interactions in the
  # model formula; only meaningful (and only identifiable) once there's
  # more than one geo-unit to distinguish
  use_geo_int <- n_geo > 1

  # CHECK 6
  if(!is.null(global_cen)) {
    stopifnot(is.numeric(global_cen))

  }

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

  ###############################################
  # CREATE CROSSBASIS
  ###############################################

  exposure_col <- attributes(exposure_matrix)$column_mapping$exposure

  if(verbose) {
    cat("\n")
    cat("crossbasis args for geo_unit ",
        paste0(unique_geos, collapse = ","),
        ":\n")
    cat("\n")
  }

  # maxlag
  if(is.null(maxlag)) {
    maxlag = 5
  } else {
    stopifnot(maxlag %in% 1:50)
  }
  if(verbose) {
    cat("maxlag:",maxlag,"\n")
    cat("\n")
  }

  # argvar
  this_exp = exposure_matrix[, get(exposure_col)]
  argvar <- check_argvar(argvar, this_exp)
  exposure_is_factor <- argvar$fun == 'strata'
  if(verbose) {
    cat("argvar:\n")
    str(argvar)
    cat("\n")
  }

  # arglag
  if(is.null(arglag)) {
    arglag <- list(fun = 'ns', knots = logknots(maxlag, nk = 2))
  } else {
    if(verbose) {
      warning("check that arglag is valid")
    }
  }
  if(verbose) {
    cat("arglag:\n")
    str(arglag)
    cat("\n")
  }

  if(verbose) {
    cat("min_n:",min_n, "\n")
    cat("\n")
  }

  ## time spline df
  n_years <- as.numeric(diff(range(outcomes_tbl[, get(date_col)]))) / 365

  if(time_trend == "annual") {
    ## 7 df/year is the standard full-year time-series-DLNM default
    if(is.null(df_time)) df_time <- max(4, round(dfseas * n_years))
  } else {
    ## "season": within-season smooth, doesn't scale with n_years --
    ## inter-annual variation is instead absorbed by factor(yr)
    if(is.null(df_time)) df_time <- 4
    n_seasons <- length(unique(outcomes_tbl$yr))
    if(n_seasons < 2 && verbose) {
      warning("time_trend = 'season' but only 1 year of data is present:
              factor(yr) will have no effect and this is equivalent to just
              fitting the within-season spline alone. Consider time_trend =
              'annual' if you only have a single season/year.")
    }
  }

  if(verbose) {
    cat("time_trend:", time_trend, "\n")
    cat("n_years (approx):", round(n_years, 2), "\n")
    if(time_trend == "annual") {
      cat("df_time for ns(date):", df_time, "\n")
    } else {
      cat("df_time for ns(day_of_season):", df_time, "\n")
      cat("n_seasons (years):", length(unique(outcomes_tbl$yr)), "\n")
    }
    cat("add_dow:", add_dow, "\n\n")
  }

  ## get the columns you need
  ## TODO: HAVE TO CONFIRM THAT THESE COLUMNS EXIST
  xcols <- c(exposure_col, paste0('explag',1:maxlag))
  x_mat <- exposure_matrix[, ..xcols]

  ## if you are safe to proceed, make the x_mat
  ## since you are passing in a matrix, you dont need to do
  ## group = year or location, because all the data you need
  ## are in each row, and this also means the order doesn't matter
  cb <- crossbasis(x_mat, lag = maxlag, argvar = argvar, arglag = arglag)

  # there should be no NAs here
  if(any(is.na(cb))) stop("crossbasis has NULL, something went wrong")

  ###############################################
  #' RUN QUASI-POISSON GLM
  ###############################################

  geo_suffix <- if(use_geo_int) ":factor(geo_unit)" else ""

  if(time_trend == "annual") {
    trend_terms <- sprintf("ns(time_num, df = %d)%s", df_time, geo_suffix)
  } else {
    if(use_geo_int) {
      trend_terms <- c(paste0("yr", geo_suffix),
                       sprintf("ns(day_of_season, df = %d)%s * yr", df_time, geo_suffix))
    } else {
      trend_terms <- sprintf("ns(day_of_season, df = %d) * yr", df_time)
    }
  }

  rhs_terms <- c("cb", trend_terms)
  if(use_geo_int) rhs_terms <- c(rhs_terms, "factor(geo_unit)")
  if(add_dow) rhs_terms <- c(rhs_terms, paste0("dow", geo_suffix))
  if(!is.null(population_col)) {
    rhs_terms <- c(rhs_terms, sprintf("offset(log(%s))", population_col))
  }

  ff <- as.formula(paste(outcome_col, "~", paste(rhs_terms, collapse = " + ")))

  model_data <- copy(outcomes_tbl)
  model_data[, geo_unit := get(out_geo_unit_col)]

  if(verbose) {
    cat("use_geo_int (multi-zone interactions):", use_geo_int, "\n")
    if(!is.null(population_col)) {
      cat("population offset: offset(log(", population_col, "))\n", sep = "")
    } else {
      cat("population offset: none\n")
    }
    cat("model formula:\n")
    print(ff)
    cat("\n")
  }

  m_sub <- glm(formula = ff, ##link to gasparrini repo: how did he define formula, which terms were brought out individually
               data = model_data,
               family = quasipoisson(link = "log"))

  # Extract full model coefficients and covariance matrix
  m_coef_all <- coef(m_sub)
  m_vcov_all <- vcov(m_sub)

  ## subset to just the pooled crossbasis (cb) terms for crosspred/crossreduce
  ## everything else (intercepts, trend, dow) is nuisance/adjustment
  cb_idx <- grep("^cb", names(m_coef_all))
  if(length(cb_idx) == 0) stop("could not find `cb` terms in the fitted coefficients")

  m_coef <- m_coef_all[cb_idx]
  m_vcov <- m_vcov_all[cb_idx, cb_idx, drop = FALSE]

  if(any(is.na(m_vcov))) stop("vcov has NULL, something went wrong")

  if(any(is.na(m_coef_all))) stop("coef has NULL, something went wrong.
                              Usually this happens (1) when a geo_unit has too
                              few observations/events relative to df_time,
                              (2) when maxlag is low (< 3) and you haven't
                              adjusted argvar/arglag, (3) collinearity between
                              the geo_unit:trend and geo_unit:dow terms -- try
                              lowering df_time or dropping add_dow, or
                              (4) if time_trend = 'season' and a geo_unit has
                              too few years of data for factor(yr):factor(geo_unit)
                              to be identifiable alongside the within-season
                              spline. Try time_trend = 'annual' instead.")


  ###############################################
  # CROSSPRED and CROSSREDUCE
  ###############################################

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

  # now apply to cr and export
  cr <- crossreduce(cb,
                    coef = m_coef,
                    vcov = m_vcov,
                    model.link = "log",
                    cen = cen,
                    by = 0.1)

  ###############################################
  # Make a single centered basis
  ###############################################

  this_exp <- exposure_matrix[, get(exposure_col)]
  x_b <- c(floor(min(this_exp)), ceiling(max(this_exp)))

  centered_basis <- get_centered_cp(argvar = argvar,
                                    xcoef = coef(cr),
                                    xvcov = vcov(cr),
                                    global_cen = global_cen,
                                    cen = cen,
                                    this_exp = this_exp,
                                    x_b = x_b,
                                    exposure_is_factor = exposure_is_factor)

  overall_centered_basis <- centered_basis$basis_cen

  ###############################################
  # OUTPUT OBJECTS
  ###############################################

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
                      exposure_is_factor = exposure_is_factor
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
    #


    # each of these things you need for BLUP and MIXMETA later
    oo_list[[i]] <- list(geo_unit = this_geo,
                         geo_unit_grp = this_geo_grp,
                         basis_cen = this_centered_cb,
                         dates = single_outcomes_tbl[, get(date_col)],  ## replaces strata_vec
                         dow = single_outcomes_tbl$dow,                 ## replaces match_strata
                         exposure_is_factor = exposure_is_factor,
                         orig_basis = this_cb,
                         orig_coef = m_coef_all,       ## full model coefs (whole group)
                         orig_vcov = m_vcov_all,        ## full model vcov (whole group)
                         cr = cr,
                         coef = coef(cr),
                         vcov = vcov(cr),
                         exposure_col = exposure_col,
                         this_exp = this_exp,
                         outcomes = outcomes,
                         cen = cen,
                         global_cen = global_cen,
                         argvar = argvar,
                         exp_mean = exp_mean,
                         exp_IQR = exp_IQR,
                         time_trend = time_trend,
                         df_time = df_time,
                         add_dow = add_dow,
                         population_col = population_col)

  }

  outlist = list(list(out = oo_list))
  names(outlist) = "_"
  class(outlist) <- 'timeseries_1stage'

  return(outlist)

}


#' Print method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(), class = "timeseries_1stage")
#' print(x)
print.timeseries_1stage <- function(x) {
  cat("< an object of class `timeseries_1stage` >\n")
  invisible(x)
}


#' Print method for timeseries_1stage_list
#'
#' @param x an object of class timeseries_1stage_list
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "timeseries_1stage_list")
#' print(x)
print.timeseries_1stage_list <- function(x) {
  cat("< an object of class `timeseries_1stage_list`:",
      paste(names(x), collapse = ",")," >\n")
}


#' getRR method for timeseries_1stage
#'
#' @param x an object of class timeseries_1stage
#' @importFrom data.table setDT
#' @returns a data.table of relative risk estimates
#' @export
#'
#' @examples
#' # create exposure matrix
#'exposure_columns <- list(
#'  "date" = "date",
#'  "exposure" = "tmax_C",
#'  "geo_unit" = "TOWN20",
#"  "geo_unit_grp" = "COUNTY20"
#')
#'middlesex_exposure <- subset(ma_exposure, COUNTY20 == 'MIDDLESEX')
#'middlesex_exposure_mat <- make_exposure_matrix(middlesex_exposure, exposure_columns)

#'# create outcome table
#'outcome_columns <- list(
#'  "date" = "date",
#'  "outcome" = "daily_deaths",
#'  "factor" = 'age_grp',
#'  "factor" = 'sex',
#'  "geo_unit" = "TOWN20",
#'  "geo_unit_grp" = "COUNTY20"
#')
#'middlesex_deaths   <- subset(ma_deaths, COUNTY20 == 'MIDDLESEX')
#'middlesex_deaths_tbl <- make_outcome_table(middlesex_deaths,  outcome_columns)

#'# run the model
#' m2 <- timeseries_1stage(exposure_matrix = middlesex_exposure_mat,
#' outcomes_tbl = middlesex_deaths_tbl, multi_zone = TRUE,
#' global_cen = 15)
#' getRR(m2)

getRR.timeseries_1stage <- function(x) {

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
#' @import ggplot2
#' @returns a ggplot object
#' @export
#'
#' @examples
#'# create exposure matrix
#'exposure_columns <- list(
#'  "date" = "date",
#'  "exposure" = "tmax_C",
#'  "geo_unit" = "TOWN20",
#"  "geo_unit_grp" = "COUNTY20"
#')
#'middlesex_exposure <- subset(ma_exposure, COUNTY20 == 'MIDDLESEX')
#'middlesex_exposure_mat <- make_exposure_matrix(middlesex_exposure, exposure_columns)

#'# create outcome table
#'outcome_columns <- list(
#'  "date" = "date",
#'  "outcome" = "daily_deaths",
#'  "factor" = 'age_grp',
#'  "factor" = 'sex',
#'  "geo_unit" = "TOWN20",
#'  "geo_unit_grp" = "COUNTY20"
#')
#'middlesex_deaths   <- subset(ma_deaths, COUNTY20 == 'MIDDLESEX')
#'middlesex_deaths_tbl <- make_outcome_table(middlesex_deaths,  outcome_columns)

#'# run the model
#' m2 <- timeseries_1stage(exposure_matrix = middlesex_exposure_mat,
#' outcomes_tbl = middlesex_deaths_tbl, multi_zone = TRUE,
#' global_cen = 15)
#' plot(m2)
plot.timeseries_1stage <- function(x, xlab = NULL, ylab = NULL, title = NULL) {

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
#' @importFrom data.table setDT
#' @returns a data.table of relative risk estimates across factor levels
#' @export
#'
#' @examples
#'middlesex_deaths_tbl <- make_outcome_table(
#'middlesex_deaths,  outcome_columns, collapse_to = 'age_grp')
#'
#'# run the model
#'m3 <- timeseries_1stage(exposure_matrix = middlesex_exposure_mat,
#'                      outcomes_tbl = middlesex_deaths_tbl,
#'                      global_cen = 15,
#'                      multi_zone = TRUE,
#'                      verbose = 1)
#'getRR(m3)

getRR.timeseries_1stage_list <- function(x) {

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
#' @import ggplot2
#' @returns a ggplot object
#' @export
#'
#' @examples
#'middlesex_deaths_tbl <- make_outcome_table(
#'middlesex_deaths,  outcome_columns, collapse_to = 'age_grp')
#'
#'# run the model
#'m3 <- timeseries_1stage(exposure_matrix = middlesex_exposure_mat,
#'                      outcomes_tbl = middlesex_deaths_tbl,
#'                      global_cen = 15,
#'                      multi_zone = TRUE,
#'                      verbose = 1)
#'plot(m3)
plot.timeseries_1stage_list <- function(x, xlab = NULL, ylab = NULL, title = NULL) {

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
#'
#' @examples
#' x <- structure(list(), class = "timeseries_1stage")
#' forest_plot(x)
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
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "timeseries_1stage_list")
#' forest_plot(x)
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
#'
#' @examples
#' x <- structure(list(), class = "timeseries_1stage")
#' spatial_plot(x)
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
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "timeseries_1stage_list")
#' spatial_plot(x)
spatial_plot.timeseries_1stage_list <- function(x, ...) {
  warning("`spatial_plot` method not implemented for objects of class `timeseries_1stage_list`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}
