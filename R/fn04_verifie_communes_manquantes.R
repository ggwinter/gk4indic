#' fn04_verifie_communes_manquantes
#'
#' @param data df
#' @importFrom cli bg_red
#' @importFrom cli col_yellow
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @importFrom tibble is_tibble
#' @return liste
#' @export
#'
fn04_verifie_communes_manquantes <- function(data = ls_onglets) {
  ls_verif <- list()
  if(tibble::is_tibble(tab_geo) == TRUE){ls_verif$cor_com$liste <- unique(tab_geo$CODGEO[tab_geo$REG %in%
                                                                                   94])
  ls_verif$cor_com$valid <- all(purrr::map(ls_verif$cor_com$liste,
                                           ~.x %in% data$cor_com$g_com_cd) %>% unlist() == TRUE)}
  else{
    ls_verif$cor_com$valid <- NA
    ls_verif$cor_com$liste <- NA_character_
    message(cli::bg_red(cli::col_yellow("la table tab_geo n'a pas été chargée")))
  }
  ls_verif <- purrr::transpose(ls_verif)
  return(ls_verif)
}
