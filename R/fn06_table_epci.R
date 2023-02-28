#' fn06_table_epci
#'
#' @param data list
#' @importFrom cli bg_green
#' @importFrom cli col_black
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom here here
#' @importFrom readr write_csv2
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @return nothing
#' @export
#'
fn06_table_epci <- function(data = ls_onglets) {
  ls_onglets$cor_epci <-
    fn05_complete_table_communes(data = ls_onglets$cor_com)

  t_epci <- tab_geo %>% dplyr::filter(REG %in% "94") %>%
    tibble::as_tibble() %>% dplyr::select(CODGEO, EPCI) %>%
    dplyr::rename(c(g_com_cd = "CODGEO", g_epci_cd = "EPCI"))

  ls_onglets$cor_epci <-
    ls_onglets$cor_epci %>%
    dplyr::left_join(t_epci, by = "g_com_cd") %>%
    dplyr::group_by(g_epci_cd, dt_date) %>%
    dplyr::summarise(lgt_mev = sum(lgt_mev)) %>%
    tidyr::pivot_wider(
      names_from = g_epci_cd,
      names_prefix = "ECLN_MEV_EPCI_AG_T\u00a7",
      values_from = lgt_mev
    ) %>%
    dplyr::rename(c("Date" = "dt_date")) %>%
    dplyr::arrange(desc(Date))

  geokit_lasttrim <- vector(mode = "character")
  ls_onglets$cor_epci %>% dplyr::pull(Date) %>% max() %>%
    stringr::str_replace("-12-31", "t4") %>%
    stringr::str_replace("-09-30", "t3") %>%
    stringr::str_replace("-06-30", "t2") %>%
    stringr::str_replace("-03-31", "t1") -> geokit_lasttrim

  readr::write_csv2(geokit_lasttrim,
                    here::here(
                      "4_resultats",
                      paste0(
                        geokit_lasttrim,
                        "_",
                        Sys.Date(),
                        ".csv"
                      )
                    ))

  readr::write_csv2(ls_onglets$cor_epci,
                    here::here(
                      "4_resultats",
                      paste0(
                        "ECLN_MEV_EPCI_AG_T_",
                        geokit_lasttrim,
                        "_",
                        Sys.Date(),
                        ".csv"
                      )
                    ))
  cat(cli::bg_green(
    cli::col_black(
      "Tous les tableaux issus de la requete geokit sont dans 4_resultats\n"
    )
  ))


}
