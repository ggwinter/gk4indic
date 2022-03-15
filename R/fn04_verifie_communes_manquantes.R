#' fn04_verifie_communes_manquantes
#'
#' @param data df
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @return liste
#' @export
#'
fn04_verifie_communes_manquantes <- function(data = ls_onglets) {
  ls_verif <- list()

  unique(tab_geo$CODGEO[tab_geo$REG %in% 94]) -> ls_verif$cor_com$liste
  all(
    purrr::map(ls_verif$cor_com$liste, ~ .x %in% data$cor_com$g_com_cd) %>% unlist() == TRUE
  ) -> ls_verif$cor_com$valid

  purrr::transpose(ls_verif) -> ls_verif

  return(ls_verif)
}
