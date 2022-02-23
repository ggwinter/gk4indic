#' les dates geokit sont au format 20201 1er trimestre
#' on les transforme en 2020-03-31 dernier jour du premier trimestre
#'
#' @param x un vecteur numérique de 5 caractères
#' @return la fonction renvoie un vecteur au format date du jour de fin de trimestre
#' @importFrom magrittr %>%
#' @examples
#' modifie_date(c("20201", "20202"))
#' @noRd
modifie_date <- function(x = c("20201", "20202")) {
  stringr::str_sub(x, 5, 5) -> trim
  dplyr::if_else(
    trim %in% "1",
    paste0(stringr::str_sub(x, 1, 4), "-03-31"),
    dplyr::if_else(
      trim %in% "2",
      paste0(stringr::str_sub(x, 1, 4), "-06-30"),
      dplyr::if_else(
        trim %in% "3",
        paste0(stringr::str_sub(x, 1, 4), "-09-30"),
        dplyr::if_else(trim %in% "4",
                       paste0(stringr::str_sub(x, 1, 4), "-12-31"),
                       "pb")
      )
    )
  ) %>% lubridate::ymd() -> ma_date
  return(ma_date)
}

