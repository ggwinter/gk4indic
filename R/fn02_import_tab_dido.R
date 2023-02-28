#' fn02_import_tab_dido
#'
#' @param x ommercialisation par defaut
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_black
#' @importFrom cli rule
#' @importFrom didor convert
#' @importFrom didor datafiles
#' @importFrom didor datasets
#' @importFrom didor dido_search
#' @importFrom didor get_data
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr join_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom here here
#' @importFrom janitor make_clean_names
#' @importFrom readr write_csv2
#' @importFrom readr read_csv2
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_sub
#' @importFrom tibble tribble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @return nothing
#' @export
#'
fn02_import_tab_dido <- function(x = "ommercialisation") {
  result_ecln <- didor::datasets() %>%
    didor::dido_search(x)

  result_ecln %>%
    dplyr::filter(stringr::str_detect(title, "r.gion")) %>%
    didor::datafiles() %>%
    dplyr::filter(stringr::str_detect(title, "parti")) -> eff

  eff %>% dplyr::pull(temporal_coverage_end) %>%
    stringr::str_replace("-12-31", "t4") %>%
    stringr::str_replace("-09-30", "t3") %>%
    stringr::str_replace("-06-30", "t2") %>%
    stringr::str_replace("-03-31", "t1") -> fich_dido_trim

  geokit_lasttrim <- readr::read_rds(here::here("4_resultats",
                                                paste0("geokitlasttrim_",
                                                       Sys.Date(),
                                                       ".rds")))

  if(fich_dido_trim != geokit_lasttrim) {
    cat(cli::bg_red(
      cli::col_black(
        "ATTENTION les fichiers de geokit et de Dido ne sont pas de la meme date\n"
      )
    ))
  }

  eff %>%
    didor::get_data() %>%
    didor::convert() %>%
    janitor::clean_names() -> df_part

  rm(eff)


  tibble::tribble(
    ~ ind_lib,
    ~ dido_ok,
    ~ type_lgt,
    ~ indicateur,
    ~ iodd_doublon,
    ~ dido_pb,
    ~ ind_appli_trim,
    ~ ind_appli_an,
    "ECLN - D\u00e9lai d\u0027\u00e9coulement trimestriel moyen des appartements",
    1L,
    "Collectif",
    "delai_ecoul",
    0L,
    NA,
    "DEC_T_A",
    NA,
    "ECLN - D\u00e9lai d\u0027\u00e9coulement trimestriel moyen des appartements",
    1L,
    "Individuel",
    "delai_ecoul",
    0L,
    NA,
    "DEC_T_M",
    NA,
    "ECLN - Encours de logements propos\u00e9s \u00e1 la vente en fin de trimestre",
    1L,
    "Collectif",
    "stock",
    0L,
    NA,
    "ENC_T_A",
    NA,
    "ECLN - Encours de logements propos\u00e9s \u00e1 la vente en fin de trimestre",
    1L,
    "Individuel",
    "stock",
    0L,
    NA,
    "ENC_T_M",
    NA,
    "ECLN - Prix de vente trimestriel moyen des appartements",
    1L,
    "Collectif",
    "prix_m2",
    0L,
    NA,
    "PVMM2_T_A",
    NA,
    "ECLN - Prix de vente trimestriel moyen des appartements par r\u00e9gion",
    1L,
    "Collectif",
    "prix_m2",
    0L,
    NA,
    "ECLN_PRIXM_REG_T",
    NA,
    "ECLN - Prix de vente trimestriel moyen des maisons",
    1L,
    "Individuel",
    "prix_moy_ind",
    0L,
    NA,
    "PVM_T_M",
    NA,
    "ECLN - R\u00e9partion trimestrielle des logement r\u00e9serv\u00e9s \u00e1 la vente par type",
    0L,
    "Collectif",
    "resa",
    NA,
    "nb_pieces manquant",
    NA,
    NA,
    "ECLN - R\u00e9partion trimestrielle des logement r\u00e9serv\u00e9s \u00e1 la vente par type",
    0L,
    "Individuel",
    "resa",
    NA,
    "nb_pieces manquant",
    NA,
    NA,
    "ECLN - R\u00e9partition trimestrielle des logements mis en vente par type",
    1L,
    "Collectif",
    "mev",
    0L,
    NA,
    "MEV_T_A",
    "ECLN_MEV_AG_T_A",
    "ECLN - R\u00e9partition trimestrielle des logements mis en vente par type",
    1L,
    "Individuel",
    "mev",
    0L,
    NA,
    "MEV_T_M",
    "ECLN_MEV_AG_T_M",
    "ECLN - Total des mises en vente trimestrielles de logements sur une ann\u00e9e glissante (recal\u00e9 au dernier trimestre)",
    1L,
    "Collectif",
    "mev",
    1L,
    NA,
    NA,
    "ECLN_MEV_AG_T_A",
    "ECLN - Total des mises en vente trimestrielles de logements sur une ann\u00e9e glissante (recal\u00e9 au dernier trimestre)",
    1L,
    "Individuel",
    "mev",
    1L,
    NA,
    NA,
    "ECLN_MEV_AG_T_M",
    "ECLN - Total des r\u00e9servations trimestrielles de logements sur une ann\u00e9e glissante",
    1L,
    "Collectif",
    "resa",
    0L,
    NA,
    "RESV_T_A",
    "ECLN_RESV_AG_T_A",
    "ECLN - Total des r\u00e9servations trimestrielles de logements sur une ann\u00e9e glissante",
    1L,
    "Individuel",
    "resa",
    0L,
    NA,
    "RESV_T_M",
    "ECLN_RESV_AG_T_M",
    "ECLN - Total trimestriel des mises en vente de logements par EPCI sur une ann\u00e9e glissante (non recal\u00e9 au dernier trimestre)",
    0L,
    "Tous logements",
    "resa",
    NA,
    "epci manquant",
    NA,
    NA
  ) -> tioddc_ecln

  unique(tioddc_ecln$ind_appli_trim)

  df_part %>%
    dplyr::filter(nature_projet %in% "Toutes constructions") %>%
    dplyr::select(-region_libelle, -nature_projet) %>%
    tidyr::pivot_longer(
      cols = -c(trimestre:type_lgt),
      names_to = "indicateur",
      values_to = "valeur"
    ) %>%
    dplyr::left_join(
      tioddc_ecln %>%
        dplyr::filter(dido_ok == 1L, iodd_doublon == 0L) %>%
        dplyr::select(-c(dido_ok, iodd_doublon)),
      by = dplyr::join_by(type_lgt, indicateur), multiple = "all"
    ) -> df_part1

  df_part1 %>% dplyr::filter(stats::complete.cases(ind_appli_trim)) -> df_part1v

  df_part1v %>% dplyr::filter(ind_appli_trim %in% "ECLN_PRIXM_REG_T") -> t_ECLN_PRIXM_REG_T

  if (any(stringr::str_detect(t_ECLN_PRIXM_REG_T$region_code, "[:alpha:]") ==
          TRUE)) {
    t_ECLN_PRIXM_REG_T <- t_ECLN_PRIXM_REG_T %>%
      dplyr::filter(stringr::str_detect(t_ECLN_PRIXM_REG_T$region_code, "[:digit:]"))
  }

  df_part1v %>% dplyr::filter(!ind_appli_trim %in% "ECLN_PRIXM_REG_T") -> df_part1v
  df_part1v %>% dplyr::pull(ind_appli_trim) %>% unique() -> liste_indics

  df_part1v %>% dplyr::filter(region_code %in% "94") %>%
    dplyr::select(trimestre, ind_appli_trim, valeur) %>%
    tidyr::pivot_wider(names_from = "ind_appli_trim", values_from = "valeur") %>%
    dplyr::mutate(
      "ECLN_MEV_AG_T_A" = MEV_T_A,
      "ECLN_MEV_AG_T_M" = MEV_T_M,
      "ECLN_RESV_AG_T_A" = RESV_T_A,
      "ECLN_RESV_AG_T_M" = RESV_T_M,
      "annee" = stringr::str_sub(trimestre, 1, 4),
      "Date" = paste0(
        dplyr::case_when(
          stringr::str_detect(trimestre, "4$") ~ "31/12/",
          stringr::str_detect(trimestre, "3$") ~ "30/09/",
          stringr::str_detect(trimestre, "2$") ~ "30/06/",
          stringr::str_detect(trimestre, "1$") ~ "31/03/"
        ),
        annee
      ) %>%
        lubridate::dmy(.)
    ) %>% dplyr::select(-annee, -trimestre) %>%
    dplyr::select(Date , dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(Date)) -> df_part1v

  readr::write_csv2(df_part1v, here::here(
    "4_resultats",
    paste0("Dido_ECLN_tab_",
           fich_dido_trim, "_", Sys.Date(), ".csv")
  ), append = FALSE)


  t_ECLN_PRIXM_REG_T %>%
    dplyr::select(trimestre, region_code, valeur) %>%
    dplyr::mutate(
      "annee" = stringr::str_sub(trimestre, 1, 4),
      "Date" = paste0(
        dplyr::case_when(
          stringr::str_detect(trimestre, "4$") ~ "31/12/",
          stringr::str_detect(trimestre, "3$") ~ "30/09/",
          stringr::str_detect(trimestre, "2$") ~ "30/06/",
          stringr::str_detect(trimestre, "1$") ~ "31/03/"
        ),
        annee
      ) %>%
        lubridate::dmy(.)
    ) %>% dplyr::select(-annee, -trimestre) %>%
    dplyr::select(Date , dplyr::everything()) %>%
    tidyr::pivot_wider(
      names_from = region_code,
      names_prefix = "ECLN_PRIXM_REG_T\u00a7",
      values_from = valeur
    ) %>%
    dplyr::arrange(dplyr::desc(Date)) -> t_ECLN_PRIXM_REG_T_V

  readr::write_csv2(t_ECLN_PRIXM_REG_T_V,
                    here::here(
                      "4_resultats",
                      paste0(
                        "Dido_ECLN_PRIXM_REG_T_",
                        fich_dido_trim,
                        "_",
                        Sys.Date(),
                        ".csv"
                      )
                    ),
                    append = FALSE)

  result_ecln %>%
    dplyr::filter(stringr::str_detect(title, "natio")) %>%
    didor::datafiles() %>%
    dplyr::slice(1) %>%
    didor::get_data() %>%
    didor::convert() %>% janitor::clean_names() %>%
    dplyr::filter(nature_projet %in% "Toutes constructions") %>%
    dplyr::select(-nature_projet) %>%
    tidyr::pivot_longer(
      cols = -c(trimestre:type_lgt),
      names_to = "indicateur",
      values_to = "valeur"
    ) %>%
    dplyr::right_join(
      tioddc_ecln %>%
        dplyr::filter(
          dido_ok == 1L,
          iodd_doublon == 0L,
          ind_appli_trim %in% "PVMM2_T_A"
        ) %>%
        dplyr::select(-c(dido_ok, iodd_doublon)),
      by = dplyr::join_by(type_lgt, indicateur)
    ) %>%
    dplyr::select(trimestre, valeur) %>%
    dplyr::rename(c("PVMM2_T_A_FRM" = "valeur")) %>%
    dplyr::mutate(
      "annee" = stringr::str_sub(trimestre, 1, 4),
      "Date" = paste0(
        dplyr::case_when(
          stringr::str_detect(trimestre, "4$") ~ "31/12/",
          stringr::str_detect(trimestre, "3$") ~ "30/09/",
          stringr::str_detect(trimestre, "2$") ~ "30/06/",
          stringr::str_detect(trimestre, "1$") ~ "31/03/"
        ),
        annee
      ) %>%
        lubridate::dmy(.)
    ) %>% dplyr::select(-annee, -trimestre) %>%
    dplyr::select(Date , dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(Date)) -> t_ECLN_PVMM2_T_A_FRM

  readr::write_csv2(t_ECLN_PVMM2_T_A_FRM,
                    here::here(
                      "4_resultats",
                      paste0(
                        "Dido_ECLN_PVMM2_T_A_FRM",
                        fich_dido_trim,
                        "_",
                        Sys.Date(),
                        ".csv"
                      )
                    ),
                    append = FALSE)

  cat(
    cli::rule(line = 2),
    cli::bg_green(
      cli::col_black(
        "\nTous les tableaux issus du fichier ECLN_tab_dido sont dans 4_resultats"
      )
    ),
    cli::bg_green(cli::col_black("\n------Fin du traitement-----\n")),
    cli::rule(line = 2)
  )
}
