#' modifie 20201 en 2020-03-31
#' on utilise la fonction modifie_date pour modifier le contenu du tableau region par ex
#' @param data un df avec une colonne date au format trim 20201
#' @return la fonction renvoie un df avec la colonne date modifiée
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @noRd
table_modifie_date <- function(data) {
  data %>% dplyr::mutate(date = modifie_date(date))-> data
  return(data)
}

