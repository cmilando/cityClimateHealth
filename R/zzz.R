#' spatial_plot base class
#'
#' @param x an object to dispatch to the appropriate spatial_plot S3 method
#' @param ... further arguments passed to the method
#'
#' @returns output depends on the class of x; see method-specific documentation
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
#' @param x an object to dispatch to the appropriate forest_plot S3 method
#' @param ... further arguments passed to the method
#'
#' @returns output depends on the class of x; see method-specific documentation
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
#' @param x an object to dispatch to the appropriate getRR S3 method
#' @param ... further arguments passed to the method
#'
#' @returns a data.table of relative risk estimates; exact format depends on the class of x
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
