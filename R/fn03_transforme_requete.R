#' fn03_transforme_requete
#'
#' @param x le nom du fichier issu de geokit
#'
#' @importFrom attempt attempt
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom dplyr tibble
#' @importFrom janitor clean_names
#' @importFrom purrr flatten_chr
#' @importFrom purrr imap
#' @importFrom purrr iwalk
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom purrr transpose
#' @importFrom readr write_csv
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @importFrom rio export
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom tibble as_tibble
#' @importFrom tibble tribble
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom utf8 as_utf8
#'
#' @return nothing
#' @export
fn03_transforme_requete <- function(x = "Indicateurs_ecln_trim2022.xlsx") {

  # chem_dir <- here("2_data")
  # fich_nom <- list.files(chem_dir, pattern = ".xlsx$") %>% str_subset("2022")
  adr_fich <- file.path(getwd(), "2_data", x)

  # teste existence du fichier
  attempt::attempt(
    file.exists(adr_fich) == FALSE,
    msg = utf8::as_utf8(paste0("Le fichier ", x, "n'est pas dans 2_data"))
  )


  if (file.exists(file.path(getwd(), "2_data", x))) {
    readxl::excel_sheets(adr_fich) %>% purrr::set_names() -> fich_onglets

    purrr::map(fich_onglets, ~ readxl::read_xlsx(adr_fich, .x) %>%
      janitor::clean_names()) -> ls_onglets

    names(ls_onglets) -> eff

    # tibble(
    #   champs0 = map(ls_onglets, ~ names(.x)) %>% flatten_chr() %>% unique() %>% sort(),
    #   champs = c(
    #     "g_com_cd",
    #     "g_reg_cd",
    #     "g_com_lib",
    #     "g_reg_lib",
    #     "lgt_enc",
    #     "lgt_mev",
    #     "lgt_res",
    #     "mod_nbpieces",
    #     "lgt_prixtot",
    #     "lgt_surftot",
    #     "dt_trimestre",
    #     "mod_type"
    #   )
    # )-> t_champs
    # write_csv(t_champs, here("3_tables", "t_champs.csv"))
    tibble::tribble(
      ~champs0, ~champs,
      "code_de_la_commune", "g_com_cd",
      "code_de_la_region", "g_reg_cd",
      "libelle_de_la_commune", "g_com_lib",
      "libelle_de_la_region", "g_reg_lib",
      "nb_encours_lgts_proposes_a_la_vente", "lgt_enc",
      "nb_lgt_mis_en_vente", "lgt_mev",
      "nb_lgt_reserves_a_la_vente", "lgt_res",
      "nombre_de_pieces", "mod_nbpieces",
      "prix_total_des_ventes_euros", "lgt_prixtot",
      "surface_totale_m2", "lgt_surftot",
      "trimestre", "dt_trimestre",
      "type_de_construction", "mod_type"
    ) -> t_champs


    purrr::map_dfr(ls_onglets, ~ dplyr::tibble(champs0 = names(.x)), .id = "onglet") %>%
      dplyr::inner_join(t_champs, by = "champs0") -> t_champs0

    # add a test !!!!!

    purrr::map(
    names(ls_onglets),
    ~ ls_onglets[[.x]] %>%
      purrr::set_names(t_champs0 %>% dplyr::filter(onglet %in% .x) %>%
                         dplyr::pull(champs)) %>%
      dplyr::mutate(
        "trim_day" = dplyr::case_when(
          stringr::str_sub(dt_trimestre, 5, 5) %in% "1" ~ "03-31",
          stringr::str_sub(dt_trimestre, 5, 5) %in% "2" ~ "06-30",
          stringr::str_sub(dt_trimestre, 5, 5) %in% "3" ~ "09-30",
          stringr::str_sub(dt_trimestre, 5, 5) %in% "4" ~ "12-31",
          TRUE ~ "Pb"
        ),
        "dt_date" = paste(stringr::str_sub(dt_trimestre, 1, 4), trim_day, sep = "-")
      ) %>%
      dplyr::select(-trim_day) %>%
      dplyr::select(
        dplyr::starts_with("g_"),
        dplyr::starts_with("dt_"),
        dplyr::starts_with("mod_"),
        dplyr::starts_with("lgt_")
      )
  ) %>% purrr::set_names(eff) -> ls_onglets

    purrr::map(ls_onglets, ~ .x %>%
      dplyr::pull(dt_trimestre) %>%
      max() %>%
      as.character()) %>%
      purrr::flatten_chr() %>%
      unique() -> trim_dernier



    # Requete nombre de pièces -----

    nb_pieces = vector(mode = "character", value = 0)

    ls_onglets$cor_pieces %>%
      dplyr::mutate(
        nb_pieces = dplyr::case_when(
          stringr::str_sub(mod_nbpieces, 1, 1) == 1 ~ "t1",
          stringr::str_sub(mod_nbpieces, 1, 1) == 2 ~ "t2",
          stringr::str_sub(mod_nbpieces, 1, 1) == 3 ~ "t3",
          stringr::str_sub(mod_nbpieces, 1, 1) == 4 ~ "t4",
          stringr::str_sub(mod_nbpieces, 1, 1) == 5 ~ "t5",
          stringr::str_sub(mod_nbpieces, 1, 1) == 6 ~ "t6"
        )
      ) %>%
      dplyr::select(-lgt_enc) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("lgt_"),
        names_to = "indic",
        values_to = "valeur"
      ) %>%
      dplyr::group_by(dt_trimestre, dt_date, nb_pieces, indic) %>%
      dplyr::summarise(valeur = sum(valeur)) -> ls_onglets$cor_nbpieces_trim

    fn_extrait_tab_pieces <-
      function(x = "lgt_mev", y = "nb_lgt_") {
        ls_onglets$cor_nbpieces_trim %>%
          dplyr::filter(indic %in% x) %>%
          tidyr::pivot_wider(
            names_from = nb_pieces,
            names_prefix = y,
            values_from = valeur
          ) %>%
          dplyr::select(dt_trimestre, dplyr::starts_with("nb_"), dt_date) %>%
          dplyr::rename(c("trimestre" = "dt_trimestre", "date" = "dt_date")) -> df_pieces
        return(df_pieces)
      }


    noms_tab_pieces <-
      stringr::str_c(
        c("ECLN_mev_type_lgt_", "ECLN_resv_type_lgt_"),
        stringr::str_c(stringr::str_sub(trim_dernier, 1, 4), "T", stringr::str_sub(trim_dernier, 5, 5))
      )

    purrr::imap(
      c("lgt_mev", "lgt_res") %>% purrr::set_names(c("nb_lgt_", "nb_resa_")),
      fn_extrait_tab_pieces
    ) %>% purrr::set_names(noms_tab_pieces) -> ls_pieces

    purrr::iwalk(ls_pieces, ~ rio::export(.x, here("4_resultats", paste0(.y, ".csv"))))


    # mises en vente par EPCI ------

    tab_geo <- fn01_import_tabgeo(params$tab_epci)

    # Sortir la fonction !!!
    # fn04_verifie_communes_manquantes <- function(data = ls_onglets) {
    #   ls_verif <- list()
    #
    #   unique(tab_geo$CODGEO[tab_geo$REG %in% 94]) -> ls_verif$cor_com$liste
    #   all(
    #     purrr::map(ls_verif$cor_com$liste, ~ .x %in% data$cor_com$g_com_cd) %>% unlist() == TRUE
    #   ) -> ls_verif$cor_com$valid
    #
    #   purrr::transpose(ls_verif) -> ls_verif
    #
    #   return(ls_verif)
    # }


    fn04_verifie_communes_manquantes(data = ls_onglets) -> ls_verif

    # Sortir la fonction !!!
    # fn_complete_table_com <- function(data = ls_onglets$cor_com) {
    #   tidyr::expand_grid(
    #     g_com_cd = ls_verif$liste[["cor_com"]],
    #     dt_date = unique(data[["dt_date"]])
    #   ) -> df0
    #   if (nrow(df0) == nrow(data)) {
    #     x -> result
    #   } else {
    #     dplyr::left_join(df0, data, by = c("g_com_cd", "dt_date")) %>%
    #       dplyr::mutate_if(
    #         .predicate = is.numeric,
    #         .funs = ~ tidyr::replace_na(.x, 0)
    #       ) -> result
    #   }
    #   return(result)
    # }

    fn_complete_table_com(data = ls_onglets$cor_com) %>%
      dplyr::select(g_com_cd, dt_date, lgt_mev) -> ls_onglets$cor_epci


    # regroupement des communes par EPCI
    #

    tab_geo %>%
      dplyr::filter(REG %in% "94") %>%
      tibble::as_tibble() %>%
      dplyr::select(CODGEO, EPCI) %>%
      dplyr::rename(c("g_com_cd" = "CODGEO", "g_epci_cd" = "EPCI")) -> tab_epci

    ls_onglets$cor_epci %>%
      dplyr::left_join(tab_epci, by = "g_com_cd") %>%
      dplyr::group_by(g_epci_cd, dt_date) %>%
      dplyr::summarise(lgt_mev = sum(lgt_mev)) %>%
      tidyr::pivot_wider(
        names_from = g_epci_cd,
        names_prefix = "ECLN_MEV_EPCI_AG_T\u00a7",
        values_from = lgt_mev
      ) %>%
      dplyr::rename(c("date" = "dt_date")) -> ls_onglets$cor_epci

    readr::write_csv(
      ls_onglets$cor_epci,
      here(
        "4_resultats",
        paste0("ECLN_MEV_EPCI_AG_T_", Sys.Date(), ".csv")
      )
    )

    ls_onglets$reg_coll %>%
      dplyr::filter(stringr::str_detect(mod_type, "Collectif")) %>%
      dplyr::mutate(prix = round(lgt_prixtot / lgt_surftot, 0)) %>%
      dplyr::select(dt_date, g_reg_cd, prix) %>%
      tidyr::pivot_wider(
        names_from = g_reg_cd,
        names_prefix = "ECLN_PRIXM_REG_T\u00a7",
        values_from = prix
      ) %>%
      dplyr::rename(c("date" = "dt_date")) -> tab_reg_prix_appart


    print("Les fichiers sont dans 4_resultats\n")
  } else {
    print("Copier le fichier xlsx dans 2_data et relancer/kniter le script\n")
  }
}
