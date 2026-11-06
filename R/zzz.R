#' spatial_plot base class
#'
#' @param x an object to dispatch to the appropriate spatial_plot S3 method
#' @param ... further arguments passed to the method
#'
#' @returns output depends on the class of x; see method-specific documentation
#' @export
#'
#' @examples
#' towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
#' exposure_columns <- list("date" = "date", "exposure" = "tmax_C",
#'                          "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' outcome_columns <- list("date" = "date", "outcome" = "daily_deaths",
#'                         "geo_unit" = "TOWN20", "geo_unit_grp" = "COUNTY20")
#' time_subset <- list(month = 5:9, year = 2012:2013)
#' exposure_mat <- make_exposure_matrix(subset(ma_exposure, TOWN20 %in% towns),
#'                                      exposure_columns, time_subset = time_subset)
#' outcomes_tbl <- make_outcome_table(subset(ma_deaths, TOWN20 %in% towns),
#'                                    outcome_columns, time_subset = time_subset)
#' m <- condPois_2stage(exposure_mat, outcomes_tbl, global_cen = 20)
#' spatial_plot(m, shp = subset(ma_towns, TOWN20 %in% towns), exposure_val = 30)
spatial_plot <- function(x, ...) {
  UseMethod("spatial_plot")
}


#' forest_plot base class
#'
#' @param x an object to dispatch to the appropriate forest_plot S3 method
#' @param ... further arguments passed to the method
#'
#' @returns output depends on the class of x; see method-specific documentation
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
#' forest_plot(m, exposure_val = 30)
forest_plot <- function(x, ...) {
  UseMethod("forest_plot")
}


#' getRR base class
#'
#' @param x an object to dispatch to the appropriate getRR S3 method
#' @param ... further arguments passed to the method
#'
#' @returns a data.table of relative risk estimates; exact format depends on the class of x
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
#' getRR(m)
getRR <- function(x, ...) {
  UseMethod("getRR")
}
