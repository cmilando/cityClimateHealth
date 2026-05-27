#' spatial_plot base class
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' # after running a condPois model:
#' result <- condPois_2stage(exposure_matrix, outcomes_tbl)
#' spatial_plot(result, shp = my_shapefile, exposure_val = 30.0)
#' }

spatial_plot <- function(x, ...) {
  UseMethod("spatial_plot")
}


#' forest_plot base class
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' # after running a condPois model:
#' result <- condPois_2stage(exposure_matrix, outcomes_tbl)
#' forest_plot(result, exposure_val = 30.0)
#' }

forest_plot <- function(x, ...) {
  UseMethod("forest_plot")
}


#' getRR base class
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' # after running a condPois model:
#' result <- condPois_2stage(exposure_matrix, outcomes_tbl)
#' getRR(result)
#' }
getRR <- function(x, ...) {
  UseMethod("getRR")
}
