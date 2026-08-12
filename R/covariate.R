
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
