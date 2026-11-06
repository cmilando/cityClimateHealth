#' The 'cityClimateHealth' package.
#'
#' @description Tools for estimating heat-related health impacts using daily exposure and
#' outcome data at small spatial scales. The package supports one-stage
#' conditional Poisson models, two-stage meta-analytic designs, and spatial
#' Bayesian approaches that borrow strength across neighboring geographic
#' units when case counts are small. Methods include distributed lag
#' non-linear models and attributable number calculations, with workflows
#' designed for messy real-world epidemiologic data.
#'
#' @name cityClimateHealth-package
#' @aliases cityClimateHealth
#'
#' @importFrom Matrix Diagonal sparseMatrix rowSums diag
#'
#' @importFrom ggplot2 aes coord_cartesian element_blank element_text facet_wrap
#' @importFrom ggplot2 geom_boxplot geom_col geom_errorbar geom_hline geom_label
#' @importFrom ggplot2 geom_line geom_pointrange geom_ribbon geom_sf geom_vline
#' @importFrom ggplot2 ggplot ggtitle position_dodge2 position_jitterdodge
#' @importFrom ggplot2 scale_color_manual scale_color_viridis_d scale_fill_binned
#' @importFrom ggplot2 scale_fill_gradient2 scale_fill_manual scale_fill_viridis_d
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous sym theme
#' @importFrom ggplot2 theme_classic xlab ylab
#' @importFrom scales number
#'
#' @importFrom sf st_geometry
#' @importFrom spdep droplinks n.comp.nb nb2listw poly2nb
#'
#' @importFrom patchwork wrap_plots
#' @importFrom MASS ginv
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom dlnm crossbasis
#' @importFrom dlnm crosspred
#' @importFrom dlnm crossreduce
#' @importFrom dlnm logknots
#' @importFrom dlnm onebasis
#' @importFrom gnm gnm
#' @importFrom lubridate days_in_month
#' @importFrom lubridate make_date
#' @importFrom mixmeta blup
#' @importFrom mixmeta mixmeta
#' @importFrom tidyr expand_grid
#' @importFrom zoo na.approx
#' @importFrom stats IQR as.formula coef glm predict qnorm quantile
#' @importFrom stats quasipoisson reorder setNames vcov
#' @importFrom utils head modifyList str type.convert
#'
"_PACKAGE"
NULL

# Column names used inside data.table `[` and ggplot2 `aes()` calls, plus
# data.table's special symbols. Declared so R CMD check doesn't report them
# as undefined global variables.
utils::globalVariables(c(
  ".", "..covariate_cols", "..factor_col", "..geo_cols", "..get_cols",
  "..spatial_join_col", "..xcols", ".N", ".SD", ".join_geo", ".join_time",
  "N", "RR", "RRlb", "RRub", "annual_AN", "attributable_number", "category",
  "cen", "day_of_season", "dow", "geo_unit", "lag", "mean_annual_AN",
  "mean_annual_AN_rate", "mean_annual_attr_num_est", "mean_annual_attr_num_lb",
  "mean_annual_attr_num_ub", "mean_annual_attr_rate_est",
  "mean_annual_attr_rate_lb", "mean_annual_attr_rate_ub", "population",
  "stage", "strata", "strata_total", "time_num", "x", "yr"
))
