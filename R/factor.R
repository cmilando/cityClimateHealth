#' make factor column
#'
#' @param data a data.table containing the columns named in `column_mapping`
#' @param column_mapping a named list mapping column roles to column names in
#'   `data`; all `factor` entries are combined into a single
#'   factor column
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

  # update column mapping: drop every original 'factor' entry,
  # then add back the single combined one
  column_mapping <- column_mapping[names(column_mapping) != 'factor']
  column_mapping[['factor']] = factor_col

  return(list(data = data, column_mapping = column_mapping))
}
