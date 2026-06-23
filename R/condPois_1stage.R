#' Run a conditional poisson model for a single geographic unit
#'
#' @param exposure_matrix a matrix of exposures, with columns for lag, usually created by `make_exposure_matrix`
#' @param outcomes_tbl a data.table of outcomes, created by `make_outcome_table`
#' @param argvar a list containing the `argvar` components for the `crossbasis`
#' @param arglag a list containing the `arglag` components for the `crossbasis`
#' @param maxlag an integer of the maximum lag
#' @param min_n an integer describing the minimum number of cases for a single region
#' @param strata_min an integer describing the minimum number of cases for a single strata
#' @param global_cen global centering point
#' @param multi_zone are multiple strata being used.
#' @param verbose used to print crossbasis args the first time
#'
#' @importFrom data.table setDT
#' @importFrom dlnm crossbasis
#' @importFrom dlnm crosspred
#' @importFrom dlnm crossreduce
#' @importFrom dlnm logknots
#' @importFrom gnm gnm
#'
#' @returns a condPois_1stage model object
#' @export
#'
#' @examples
#' library(dlnm)
#' library(gnm)
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
#' m1 <- condPois_1stage(exposure_matrix = boston_exposure_mat,
#'                     outcomes_tbl = boston_deaths_tbl)
#' condPois_1stage
condPois_1stage <- function(exposure_matrix, outcomes_tbl,
                        argvar = NULL, arglag = NULL, maxlag = NULL,
                       min_n = 50, strata_min = 0, global_cen = NULL,
                       multi_zone = FALSE, verbose = TRUE) {

  ## Check 1 -- that both inputs are the right class of variables
  stopifnot("exposure" %in% class(exposure_matrix))
  stopifnot("outcome" %in% class(outcomes_tbl))

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' IF the outcomes_tbl has a FACTOR, enter a recursive loop
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////
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
      fct_outlist[[fct_i]] <- condPois_1stage(exposure_matrix = exposure_matrix,
                                              outcomes_tbl = subset_outcomes_tbl,
                                              global_cen = global_cen,
                                              argvar = argvar,
                                              arglag = arglag,
                                              maxlag = maxlag,
                                              min_n = min_n,
                                              strata_min = strata_min,
                                              multi_zone = multi_zone,
                                              verbose = verbose)

      fct_outlist[[fct_i]]$factor_col <- factor_col
      fct_outlist[[fct_i]]$factor_val <- unique_fcts[fct_i]

      # also include a scaling factor that can be used to adjust the AN
      # this is useful in scenarios with a temporal collapse factor
      fct_outlist[[fct_i]]$factor_scale <- 1

    }

    names(fct_outlist) = unique_fcts

    class(fct_outlist) = 'condPois_1stage_list'

    return(fct_outlist)


  }

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' VALIDATIONS
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

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

  ## CHECK 6 - minN for all geo_units
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

  # CHECK 7
  stopifnot(strata_min >= 0)
  stopifnot(strata_min < min_n)

  # CHECK 8 check if multizone
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

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' CREATE CROSSBASIS for this single zone
  #'
  #' TODO: HOW TO HANDLE MULTIPLE exposures ...
  #'       maybe this becomes a function and you have?
  #'       well no because then arglag etc would have to be lists
  #'       so do you want to move this into exposure ?
  #'       no i think thats what you do, you move this into exposure
  #'       and then you keep one main exposure and the other gets labeled
  #'       as "control" and some of the control are cb and
  #'       some are single vectors
  #'       maybe exposure_col also needs to be a named list
  #'       so you know if its a one off or a cb
  #'       or you could do control_cb or
  #'
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////
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
    stopifnot(maxlag %in% 1:10)
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
    cat("strata:\n")
    cat(paste(outcomes_tbl$strata[1]))
    cat("\n")
  }

  if(verbose) {
    cat("strata_min:",strata_min, "\n")
    cat("\n")
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

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' RUN GNM
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

  ## (1) if using GNM, you get COEF and VCOV as part of the model objects
  ##
  ## (2) instead of doing keep == 1, i updated to be a strata total variable
  ## so you can raise the floor if you want to.
  ##
  ## (3) you also don't need to do offset = log(poplation)
  ## because the population is not changing within the strata,
  ## TODO: <<< PERHAPS NOT TRUE, maybe you should make this the default
  ## since you are doing conditional poisson. if you are doing time-series
  ## you would need to do this
  ff = as.formula(paste(outcome_col, "~ cb"))

  m_sub <- gnm(formula = ff,
               data = outcomes_tbl,
               family = quasipoisson,
               eliminate = factor(strata),
               subset = strata_total > strata_min)

  m_coef <- coef(m_sub)
  m_vcov <- vcov(m_sub)

  # there should be no NAs
  if(any(is.na(m_coef))) stop("coef has NULL, something went wrong.
                              Usually this happens (1) when strata counts are too low,
                              or (2) when maxlag is low (< 3) and you haven't adjusted
                              argvar and arglag (switching to fun='lin' can be a good starting point)
                              or (3) if exposure_is_factor then
                              you need to make sure that `breaks` is set correctly")

  if(any(is.na(m_vcov))) stop("vcov has NULL, something went wrong")

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' CROSSPRED and CROSSREDUCE
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

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

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' Make a single centered basis
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

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

  #' //////////////////////////////////////////////////////////////////////////
  #' ==========================================================================
  #' OUTPUT OBJECTS
  #' ==========================================================================
  #' //////////////////////////////////////////////////////////////////////////

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
  class(outlist) <- 'condPois_1stage'

  return(outlist)

}


#' Print method for condPois_1stage
#'
#' @param x an object of class condPois_1stage
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(), class = "condPois_1stage")
#' print(x)
print.condPois_1stage <- function(x) {
  cat("< an object of class `condPois_1stage` >\n")
  invisible(x)
}


#' Print method for condPois_1stage_list
#'
#' @param x an object of class condPois_1stage_list
#'
#' @returns invisibly returns x
#' @export
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "condPois_1stage_list")
#' print(x)
print.condPois_1stage_list <- function(x) {
  cat("< an object of class `condPois_1stage_list`:",
      paste(names(x), collapse = ",")," >\n")
}


#' getRR method for condPois_1stage
#'
#' @param x an object of class condPois_1stage
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
#' m2 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
#' outcomes_tbl = middlesex_deaths_tbl, multi_zone = TRUE,
#' global_cen = 15)
#' getRR(m2)

getRR.condPois_1stage <- function(x) {

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



#' Plot method for condPois_1stage
#'
#' @param x an object of class condPois_1stage
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
#' m2 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
#' outcomes_tbl = middlesex_deaths_tbl, multi_zone = TRUE,
#' global_cen = 15)
#' plot(m2)
plot.condPois_1stage <- function(x, xlab = NULL, ylab = NULL, title = NULL) {

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

#' getRR method for condPois_1stage_list
#'
#' @param x an object of class condPois_1stage_list
#' @importFrom data.table setDT
#' @returns a data.table of relative risk estimates across factor levels
#' @export
#'
#' @examples
#'middlesex_deaths_tbl <- make_outcome_table(
#'middlesex_deaths,  outcome_columns, collapse_to = 'age_grp')
#'
#'# run the model
#'m3 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
#'                      outcomes_tbl = middlesex_deaths_tbl,
#'                      global_cen = 15,
#'                      multi_zone = TRUE,
#'                      verbose = 1)
#'getRR(m3)

getRR.condPois_1stage_list <- function(x) {

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

#' Plot method for condPois_1stage_list
#'
#' @param x an object of class condPois_1stage_list
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
#'m3 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
#'                      outcomes_tbl = middlesex_deaths_tbl,
#'                      global_cen = 15,
#'                      multi_zone = TRUE,
#'                      verbose = 1)
#'plot(m3)
plot.condPois_1stage_list <- function(x, xlab = NULL, ylab = NULL, title = NULL) {

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



#' forest_plot method for condPois_1stage
#'
#' @param x an object of class condPois_1stage
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
#'
#' @examples
#' x <- structure(list(), class = "condPois_1stage")
#' forest_plot(x)
forest_plot.condPois_1stage <- function(x, ...) {
  warning("`forest_plot` method not implemented for objects of class `condPois_1stage`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}



#' forest_plot method for condPois_1stage_list
#'
#' @param x an object of class condPois_1stage_list
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "condPois_1stage_list")
#' forest_plot(x)
forest_plot.condPois_1stage_list <- function(x, ...) {
  warning("`forest_plot` method not implemented for objects of class `condPois_1stage_list`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}



#' spatial_plot method for condPois_1stage
#'
#' @param x an object of class condPois_1stage
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
#'
#' @examples
#' x <- structure(list(), class = "condPois_1stage")
#' spatial_plot(x)
spatial_plot.condPois_1stage <- function(x, ...) {
  warning("`spatial_plot` method not implemented for objects of class `condPois_1stage`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}


#' spatial_plot method for condPois_1stage_list
#'
#' @param x an object of class condPois_1stage_list
#' @param ... other elements passed to spatial_plot
#' @returns called for its side-effect (warning); returns NULL invisibly
#' @export
#'
#' @examples
#' x <- structure(list(a = 1, b = 2), class = "condPois_1stage_list")
#' spatial_plot(x)
spatial_plot.condPois_1stage_list <- function(x, ...) {
  warning("`spatial_plot` method not implemented for objects of class `condPois_1stage_list`,
      since there is only one 1_stage relative risk curve so all plot
      values would be the same. 1stage attributable number results will change
      over space, so those can be viewed instead by running `spatial_plot` on the
      output of `calcAN` for a 1stage model!")
}
