#' Make crossbasis object
#'
#' @param exposure_matrix
#' @param outcomes_tbl
#' @param unique_geos
#' @param maxlag
#' @param argvar
#' @param arglag
#' @param strata_min
#' @param min_n
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
make_crossbasis <- function(exposure_matrix, outcomes_tbl,
                            unique_geos, maxlag,
                            argvar, arglag,
                            strata_min, min_n,
                            verbose) {

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
    stopifnot(maxlag %in% 0:50)
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

  if(verbose) {
    cat("min_n:",min_n, "\n")
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

  return(list(cb = cb,
              argvar = argvar,
              exposure_is_factor = exposure_is_factor))

}


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
