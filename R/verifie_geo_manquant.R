#' Verifie les territoires manquants
#'
#' @param data a list
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @return a list
#' @export
verifie_geo_manquant <- function(data) {
  ls_verif <- list()
  if(exists('tab_geo')==FALSE) tab_geo <- COGugaison::table_supracom_2020

  unique(tab_geo$REG) -> ls_verif$region$liste
  ls_verif$region$liste[as.integer(ls_verif$region$liste) > 10L] %>% sort() -> ls_verif$region$liste
  all(
    purrr::map(ls_verif$region$liste, ~ .x %in% data$region$codegeo) %>% unlist() == TRUE
  ) -> ls_verif$region$valid

  unique(tab_geo$CODGEO[tab_geo$REG %in% 94]) -> ls_verif$communes$liste
  all(
    purrr::map(ls_verif$communes$liste, ~ .x %in% data$communes$codegeo) %>% unlist() == TRUE
  ) -> ls_verif$communes$valid

  unique(tab_geo$EPCI[tab_geo$REG %in% 94]) -> ls_verif$epci$liste
  all(purrr::map(ls_verif$epci$liste, ~ .x %in% data$epci$codegeo) %>% unlist() == TRUE) -> ls_verif$epci$valid
  purrr::transpose(ls_verif) -> ls_verif

  return(ls_verif)
}
