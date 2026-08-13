
#' check covariate names
#'
#' @param str_vec
#'
#' @returns
#' @export
#'
#' @examples
check_covariate_names <- function(str_vec) {

  # check1: none of them can start with cb
  if(any(grepl("^cb", str_vec))) {
    stop("covariate names cannot include `cb` as this will conflict
         internally with the main exposure crossbasis variable.
         Please rename this variable")
  }

  return(NULL)

}

#' join exposure covariate
#'
#' @param exposure_mat
#' @param covariate_mat
#'
#' @returns
#' @export
#'
#' @examples
join_exposure_covariate <- function(exposure_mat, covariate_mat) {

  warning("This could use more error checking")

  ##
  exp_col_map = attributes(exposure_mat)$column_mapping
  exp_date_col = exp_col_map$date
  exp_geo_unit_col = exp_col_map$geo_unit
  exp_geo_unit_grp_col = exp_col_map$geo_unit_grp
  exp_factor_col = NULL
  if("factor" %in% names(exp_col_map)) {
    exp_factor_col = exp_col_map$factor
  }

  cov_col_map = attributes(covariate_mat)$column_mapping
  cov_date_col = cov_col_map$date
  cov_geo_unit_col = cov_col_map$geo_unit
  cov_geo_unit_grp_col = cov_col_map$geo_unit_grp
  cov_factor_col = NULL
  if("factor" %in% names(cov_col_map)) {
    cov_factor_col = cov_col_map$factor
  }

  ##
  covariate_cols = cov_col_map$exposure
  if(any(grepl("^explag", names(covariate_mat)))) {
    rr <- which(grepl("^explag", names(covariate_mat)))
    names_to_sub <- names(covariate_mat)[rr]
    xy <- gsub(pattern = "explag",
               replacement = paste0(covariate_cols, "lag"),
               x = names_to_sub)
    names(covariate_mat)[rr] = xy
    covariate_cols <- c(covariate_cols, xy)
  }

  ## join
  left_names  <- c(exp_date_col, exp_geo_unit_col,
                   exp_geo_unit_grp_col, exp_factor_col,
                   'strata', 'match_strata')

  right_names <- c(cov_date_col, cov_geo_unit_col,
                   cov_geo_unit_grp_col, cov_factor_col,
                   'strata', 'match_strata')

  join_keys <- setNames(right_names, left_names)

  xjoin <- exposure_mat[covariate_mat, on = join_keys]

  if("covariate" %in% names(exp_col_map)) {
    exp_col_map$covariate <- c(exp_col_map$covariate, covariate_cols)
  }else {
    exp_col_map[["covariate"]] <- covariate_cols
  }

  attributes(xjoin)$column_mapping <- exp_col_map

  return(xjoin)

}
