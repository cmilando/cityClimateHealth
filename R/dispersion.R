#' Calculate the dispersion parameter for a quasi-poisson model
#'
#' Converted from STATA from armstrong 2014 supplement with LLM guidance.
#' Minimal error checking of these inputs at present.
#'
#' @param y a vector of outcomes
#' @param X a matrix of predictors, typically the crossbasis output
#' @param beta a vector of coefficients
#' @param stratum_vector a vector describing the stratum
#'
#' @returns a numeric scalar, the estimated dispersion parameter
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
#' m <- condPois_1stage(exposure_mat, outcomes_tbl)
#' fit <- m$`_`$out[[1]]
#' calc_dispersion(y = fit$outcomes, X = as.matrix(fit$orig_basis),
#'                 beta = fit$orig_coef, stratum_vector = fit$strata_vec)
calc_dispersion <- function(y, X, beta, stratum_vector) {

  # get dims of X
  N = nrow(X)
  K = ncol(X)

  # replace stratum_vector with counts
  n_strata <- length(unique(stratum_vector))
  stratum_vector <- as.numeric(factor(stratum_vector, labels = 1:n_strata))

  # sum observed and predicted counts per stratum
  sum_y_stratum    = rep(0, n_strata)
  sum_pred_stratum = rep(0, n_strata)
  xBeta_out = exp(X %*% beta)
  for (n in 1:N) {
    s = stratum_vector[n]
    sum_y_stratum[s] = sum_y_stratum[s] + y[n]
    sum_pred_stratum[s] = sum_pred_stratum[s] + xBeta_out[n]
  }

  # rescale predictions so that the sum of the predicted in each group
  # equals the sum of the observed in each group
  pred_rescaled <- rep(NA, N)
  for (n in 1:N) {
    s = stratum_vector[n]
    pred_rescaled[n] = xBeta_out[n] * sum_y_stratum[s] / sum_pred_stratum[s]
  }
  if(any(is.na(pred_rescaled))) {
    stop("some dispersion params = NA, which means an error occurred")
  }

  if(any(pred_rescaled == 0)) {
    stop("some dispersion = 0, which means that you are using a temporal
         collapse, which hasn't been coded yet :)")
  }

  # compute pearson chi-squared
  pearson_x2 = 0
  for (n in 1:N) {
    pearson_x2 = pearson_x2 + (y[n] - pred_rescaled[n])^2 / pred_rescaled[n]
  }
  pearson_x2

  # calculate dispersion
  df_resid = N - K - n_strata
  dispersion = pearson_x2 / df_resid

  return(dispersion)

}
