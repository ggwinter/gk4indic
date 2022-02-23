#' A partir des onglets du tableau excel copies dans une liste de tableaux
#' on va transformer d'un seul coup la date dans tous les tableaux
#' @param data une liste de df avec chacune une colonne date au format 20201
#' @importFrom purrr map
#' @export
liste_modifie_date <- function(data) {
  purrr::map(data, table_modifie_date)-> data
  return(data)
}

