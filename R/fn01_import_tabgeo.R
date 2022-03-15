#' fn01_import_tabgeo
#'
#' @param x nom de la table
#'
#' @return tibble tab_geo
#' @export
#'
#' @examples
fn01_import_tabgeo <- function(x = "table_supracom_2020"){
  eval(substitute(COGugaison::x))-> tab_geo
  return(tab_geo)
}

