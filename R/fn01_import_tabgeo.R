#' fn01_import_tabgeo
#'
#' @param x nom de la table
#'
#' @return tibble tab_geo
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_black
#' @importFrom cli col_yellow
#' @export
#'
#' @examples
#' # table des epci 2020
#' fn01_import_tabgeo(x = "table_supracom_2020")
fn01_import_tabgeo <- function(x = params$tab_epci){
  tab_geo <- eval(substitute(COGugaison::x))
  if (nrow(tab_geo) > 1) {
    cat(cli::bg_green(cli::col_black(
      paste0(
        'Le package COGugaison est a jour, la table ',
        x,
        ' a été chargée'
      )
    )))
  } else{
    cat(cli::bg_red(
      cli::col_yellow(
        "La table n'a pas été trouvée, mettre à jour COGugaison so vous êtes sur qu'elle existe"
      )
    ))
  }
  return(tab_geo)
}

