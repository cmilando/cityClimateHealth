#' Internal function to get centered cp objects
#'
#' Needed a function for this because we do it twice: once for regional RRs
#' and once for BLUP
#'
#' @param argvar the list of argvar elements used to determine the exposure dimension of `crossbasis`
#' @param xcoef the centered basis coefficients that resulted
#' @param xvcov the variance covariance matrix for the centered basis coefficients
#' @param this_exp a vector for the exposure
#' @param x_b a list that is the boundary of this_exp
#' @param global_cen a global centering point, if it exists
#' @param cen the local centering point
#' @param exposure_is_factor logical, if exposure is a factor
#' @param truncate
#'
#' @importFrom dlnm onebasis
#' @importFrom dlnm crosspred
#' @returns a list object, with the centered basis and centered crosspred output
#'
#' @examples
#' \dontrun{
#' # after running condPois_1stage on a single geo_unit:
#' result <- condPois_1stage(exposure_matrix, outcomes_tbl)
#' centered <- get_centered_cp(
#'   argvar = result$`_`$out[[1]]$argvar,
#'   xcoef = result$`_`$out[[1]]$coef,
#'   xvcov = result$`_`$out[[1]]$vcov,
#'   this_exp = result$`_`$out[[1]]$this_exp,
#'   x_b = c(0, 40),
#'   global_cen = NULL,
#'   cen = result$`_`$out[[1]]$cen,
#'   exposure_is_factor = FALSE
#' )
#' }

get_centered_cp <- function(argvar, xcoef, xvcov,
                            this_exp, x_b,
                            global_cen, cen,
                            exposure_is_factor,
                            truncate) {

  # define grid
  grid <- seq(from =  x_b[1], to = x_b[2], by = 0.1)

  # (1) get onebasis
  if(exposure_is_factor) {
    basis_x <- do.call("onebasis",
                       modifyList(argvar,
                                  list(x = this_exp)))
  } else {
    basis_x <- do.call("onebasis",
                       modifyList(argvar,
                                  list(x = this_exp,
                                       Boundary.knots = x_b)))
  }


  # *********
  # (2) Center basis
  # either MMT or GLOBAL CEN
  # you need boundary knots because the centerpoint is almost
  # always outside of the percentiles in this work
  # so this creates a full range to test over
  if(!is.null(global_cen)) {

    cen = global_cen
    stopifnot(global_cen >= x_b[1] & global_cen <= x_b[2])

    if(exposure_is_factor) {
      basis_mmt <- do.call("onebasis", modifyList(argvar,
                                                  list(x=global_cen)))
    } else {
      basis_mmt <- do.call("onebasis", modifyList(argvar,
                                                  list(x=global_cen,
                                                       Boundary.knots = x_b)))
    }

  } else {

    # if global cen isn't set, re-center to the local minimum
    b2 <- crosspred(basis_x,
                    cen = mean(this_exp),
                    coef = xcoef,
                    vcov = xvcov,
                    model.link = "log",
                    at = grid)

    # which is the min
    cen = b2$predvar[which.min(b2$allRRfit)]

    # get a basis for the MMT
    if(exposure_is_factor) {
      basis_mmt <- do.call("onebasis",
                           modifyList(argvar,
                                      list(x=cen)))
    } else{
      basis_mmt <- do.call("onebasis",
                           modifyList(argvar,
                                      list(x=cen, Boundary.knots = x_b)))
    }

  }

  # *********

  # (3) Center and scale
  basis_cen <- scale(basis_x, center = basis_mmt, scale = FALSE)


  # get the cross-pred object
  # cen is passed forward from before
  # the main reason  you need this for the RR plot
  # and this gives back out BLUP coef and vcov which you can use
  # in the AN calc
  centered_cp <- crosspred(basis_cen,
                       cen = cen,
                       coef = xcoef,
                       vcov = xvcov,
                       model.link = "log",
                       at = grid)

  # *********************
  # TRUNCATE THE BASIS
  if(truncate > 0 & !exposure_is_factor) {
    qX = quantile(this_exp, probs = 1 - truncate)
    rr = which.min(abs(this_exp - qX))
    br = basis_cen[rr, ]
    tt = which(this_exp > qX)
    for(i in 1:length(tt)) {
      basis_cen[tt[i], ] = br
    }

    qX = quantile(this_exp, probs = 0 + truncate)
    rr = which.min(abs(this_exp - qX))
    br = basis_cen[rr, ]
    tt = which(this_exp < qX)
    for(i in 1:length(tt)) {
      basis_cen[tt[i], ] = br
    }
  }
  # *********************

  # return the centered cp and the basis_cen, which you need for AN
  return(list(cp = centered_cp, basis_cen = basis_cen))

}
