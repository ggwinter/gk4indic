#' on cree les repertoires 2_data, 4_resultats dans le nouveau dossier s ils n'existent pas
#'
#' @param dossier le repertoire où est le projet
#'
#' @return cree les sous-repertoires
#' @importFrom purrr walk
#' @export
cree_repertoires <- function(dossier = here::here()) {
  repertoires <- c("1_scripts", "2_data", "3_tables", "4_resultats")
  purrr::walk(repertoires, ~ if (dir.exists(here::here(dossier, .x)) == FALSE)
    dir.create(here::here(dossier, .x)))

}


