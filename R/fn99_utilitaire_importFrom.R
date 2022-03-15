#' utilitaire_importFrom
#'
#' utile pour lister les fonctions externes utilisees par chaque fonction
#'
#' @param x nom d'un fichier de fonction
#'
#' @return les packages et fonctions utilisées
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom purrr map
#' @noRd
#'
fn99_utilitaire_importFrom <- function(x = 'fn06_met_en_forme_liste.R') {

  # list.files(file.path("R"), pattern =".R")
  readLines(file.path("R", x))-> txt

  stringr::str_extract_all(txt, "[:word:]*::[:word:].{3,15}(?=\\()") %>% as.list() -> extract
  extract[which(purrr::map(extract, length) != 0) %>% unlist()] %>% unlist() %>%
    unique() %>% sort() %>% stringr::str_replace(., "::", " ")-> extract
  return(extract)
}


