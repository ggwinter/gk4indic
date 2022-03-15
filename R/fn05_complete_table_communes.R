#' fn05_complete_table_communes
#'
#' @param data
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_if
#' @importFrom tidyr expand_grid
#' @importFrom tidyr replace_na
#'
#' @return df
#' @export
#'
fn05_complete_table_communes <- function(data = ls_onglets$cor_com) {
  tidyr::expand_grid(
    g_com_cd = ls_verif$liste[["cor_com"]],
    dt_date = unique(data[["dt_date"]])
  ) -> df0
  if (nrow(df0) == nrow(data)) {
    x -> result
  } else {
    dplyr::left_join(df0, data, by = c("g_com_cd", "dt_date")) %>%
      dplyr::mutate_if(
        .predicate = is.numeric,
        .funs = ~ tidyr::replace_na(.x, 0)
      ) -> result
  }
  return(result)
}
