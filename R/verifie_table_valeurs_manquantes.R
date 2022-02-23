#' Verification completude table geokit
#' Les sorties geokit peuvent être incompletes certaines communes n'apparaissent pas
#'
#'
#' @param data df une table issue de geokit
#'
#' @return un mot ok ou pb
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @noRd
verifie_table_valeurs_manquantes <- function(data) {
  unique(data$codegeo) %>% length() -> nb_codegeo
  unique(data$date) %>% length() -> nb_date
  dplyr::if_else(nrow(data) != nb_codegeo * nb_date, "pb", "ok")-> verif

  return(verif)
}

