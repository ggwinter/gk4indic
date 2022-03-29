#' fn01_import_tabgeo
#'
#' @param x nom de la table
#'
#' @return tibble tab_geo
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_black
#' @importFrom cli col_yellow
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' # table des epci 2020
#' fn01_import_tabgeo(x = "table_supracom_2020")
fn01_import_tabgeo <- function(x = params$tab_epci){
  chaine <- paste0("COGugaison::", x)
  express <- parse(text = chaine)
  eval(express) %>% tibble::as_tibble()-> tab_geo

  if (nrow(tab_geo) > 1) {
    cat(cli::bg_green(cli::col_black(
      paste0(
        'Le package COGugaison est \u0027 jour, la table ',
        x,
        ' a \u00e9t\u00e9 charg\u00e9e'
      )
    )))
  } else{
    cat(cli::bg_red(
      cli::col_yellow(
        "La table n\u0027a pas \u00e9t\u00e9 trouv\u00e9e, mettre \u00e0 jour COGugaison si vous \u00eates sur qu\u0027elle existe"
      )
    ))
  }
  return(tab_geo)
}

