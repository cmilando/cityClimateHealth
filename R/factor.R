#' make factor column
#'
#' @param data
#' @param column_mapping
#'
#' @returns
#' @export
#'
#' @examples
add_factor_col <- function(data, column_mapping) {

  #
  factor_col <- which(names(column_mapping) == 'factor')
  factor_col <- unlist(column_mapping[factor_col])

  #
  if(!all(factor_col %in% names(data))) {
    check_which <- which(!(factor_col %in% names(data)))
    stop(paste0("Check `collapse_to` argument, because '",
                factor_col[check_which], "' is not in data columns"))
  }

  # now you have to make a joined factor here, and then its just one
  make_factor <- function(...) paste0(..., collapse = "|")
  factor_col_name <- make_factor(factor_col)
  data[[factor_col_name]] = apply(data[, ..factor_col], 1, make_factor)
  factor_col = factor_col_name

  # update column mapping
  column_mapping[['factor']] = factor_col

  return(list(data = data, column_mapping = column_mapping))
}
