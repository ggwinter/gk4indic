#' Fonction principale va transformer le tableau excel en plusieurs csv
#' les fichiers csv vont etre enregistres dans 4_resultats
#'
#' @param x le nom du fichier excel à modifier
#'
#' @return plusieurs fichiers csv mis en forme sont copiés dans 4_resultats
#'
#' @importFrom attempt attempt
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom here here
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom purrr map_chr
#' @importFrom purrr map2
#' @importFrom purrr walk2
#' @importFrom purrr imap
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @importFrom readr write_csv
#' @importFrom stringr str_which
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr expand_grid
#' @export
transforme_tableaux <- function(x = "Indicateurs_ecln_trim.xlsx") {


    adr_fich <- file.path(getwd(), "2_data", x)
  attempt::attempt(
    file.exists(adr_fich) == FALSE,
    msg = utf8::as_utf8("Le fichier xlsx n'est pas dans 2_data")
  )


  if (file.exists(file.path(getwd(), "2_data", x))) {
    # on liste les onglets du tableau excel
    onglets <- readxl::excel_sheets(adr_fich)

    # on lit les onglets du tableau excel
    ls_tab <-
      purrr::map(onglets, ~ readxl::read_xlsx(adr_fich, .x)) %>%
      purrr::set_names(onglets)

    liste_modifie_date(ls_tab) -> ls_tab
    purrr::map(ls_tab, ~ .x %>% dplyr::filter(complete.cases("date")))-> ls_tab
    # purrr::map_chr(ls_tab, verifie_table_valeurs_manquantes) -> test_tab_na
    verifie_geo_manquant(ls_tab) -> ls_verif

    fn_complete_table <- function(x, y) {
      if (x == TRUE) {
        ls_tab[[y]]
      } else{
        tidyr::expand_grid(codegeo = ls_verif$liste[[y]],
                           date = unique(ls_tab[[y]]$date)) -> df0

        dplyr::left_join(df0, ls_tab[[y]], by = c('codegeo', 'date')) %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = ~ tidyr::replace_na(.x, 0)) -> ls_tab[[y]]
      }

    }
    purrr::imap(.x = ls_verif$valid, fn_complete_table) -> ls_tab

    purrr::map(ls_tab, ~ names(.x)[!names(.x) %in% c("codegeo", "date")]) -> ls_cd_indic

    # regroupement des communes par EPCI
    #

    tab_geo[tab_geo$REG %in% 94, ] %>%
      tibble::as_tibble() %>% dplyr::select(CODGEO, EPCI)-> tab_epci
    ls_tab$communes %>% dplyr::left_join(tab_epci, by = c('codegeo'='CODGEO')) %>%
      dplyr::group_by(EPCI, date) %>% dplyr::summarise(ECLN_MEV_EPCI_AG_T = sum(ECLN_MEV_COM_AG_T)) %>%
      dplyr::rename('codegeo' = 'EPCI')-> ls_tab$communes

    ls_cd_indic$communes <- 'ECLN_MEV_EPCI_AG_T'


    # Mise en forme des tableaux finaux
    #
    purrr::map2(
      ls_tab,
      ls_cd_indic,
      ~ .x %>% purrr::set_names(c("codegeo", "date", "indic")) %>%
        tidyr::pivot_wider(
          .,
          names_from = codegeo,
          names_prefix = paste0(.y, "\u00A7"),
          values_from = indic
        ) %>% dplyr::arrange(date)
    ) -> ls_tab_ok2

    ls_cd_indic$epci <- 'ECLN_MEV_EPCI_AG_T_verif'
    purrr::walk2(ls_tab_ok2,
                 ls_cd_indic,
                 ~ readr::write_csv(.x, here::here(
                   "4_resultats", paste0(.y, "_", Sys.Date(), ".csv")
                 )))
    print("Les fichiers sont dans 4_resultats")
  } else{
    print("Copier le fichier xlsx dans 2_data et relancer/kniter le script")
  }

}
