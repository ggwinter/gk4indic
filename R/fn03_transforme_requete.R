#' fn03_transforme_requete
#'
#' @param x le nom du fichier issu de geokit
#'
#' @importFrom attempt attempt
#' @importFrom cli bg_green
#' @importFrom cli bg_red
#' @importFrom cli col_black
#' @importFrom cli col_yellow
#' @importFrom dplyr arrange
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
#' @importFrom here here
#' @importFrom janitor clean_names
#' @importFrom purrr flatten_chr
#' @importFrom purrr imap
#' @importFrom purrr iwalk
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom purrr transpose
#' @importFrom readr write_csv2
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
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
fn03_transforme_requete <-
  function(x = "Indicateurs_ecln_trim2022.xlsx") {
    adr_fich <- file.path(getwd(), "2_data", x)


    attempt::attempt(file.exists(adr_fich) == FALSE,
                     msg = cli::bg_red(cli::col_yellow(utf8::as_utf8(
                       paste0("Le fichier ", x, "n\u0027est pas dans 2_data")
                     ))))
    if (file.exists(file.path(getwd(), "2_data", x))) {
      fich_onglets <- readxl::excel_sheets(adr_fich) %>%
        purrr::set_names()

      ls_onglets <-
        purrr::map(fich_onglets,
                   ~ readxl::read_xlsx(adr_fich,
                                       .x) %>% janitor::clean_names())
      eff <- names(ls_onglets)

      tibble::tribble(
        ~ champs0,
        ~ champs,
        "code_de_la_commune",
        "g_com_cd",
        "code_de_la_region",
        "g_reg_cd",
        "libelle_de_la_commune",
        "g_com_lib",
        "libelle_de_la_region",
        "g_reg_lib",
        "nb_encours_lgts_proposes_a_la_vente",
        "lgt_enc",
        "nb_lgt_mis_en_vente",
        "lgt_mev",
        "nb_lgt_reserves_a_la_vente",
        "lgt_res",
        "nombre_de_pieces",
        "mod_nbpieces",
        "prix_total_des_ventes_euros",
        "lgt_prixtot",
        "surface_totale_m2",
        "lgt_surftot",
        "trimestre",
        "dt_trimestre",
        "type_de_construction",
        "mod_type"
      ) -> t_champs


      t_champs0 <-
        purrr::map_dfr(ls_onglets, ~ dplyr::tibble(champs0 = names(.x)),
                       .id = "onglet") %>% dplyr::inner_join(t_champs,
                                                             by = "champs0")

      ls_onglets <-
        purrr::map(
          names(ls_onglets),
          ~ ls_onglets[[.x]] %>%
            purrr::set_names(
              t_champs0 %>%
                dplyr::filter(onglet %in%
                                .x) %>% dplyr::pull(champs)
            ) %>% dplyr::mutate(
              trim_day = dplyr::case_when(
                stringr::str_sub(dt_trimestre,
                                 5, 5) %in% "1" ~ "03-31",
                stringr::str_sub(dt_trimestre,
                                 5, 5) %in% "2" ~ "06-30",
                stringr::str_sub(dt_trimestre,
                                 5, 5) %in% "3" ~ "09-30",
                stringr::str_sub(dt_trimestre,
                                 5, 5) %in% "4" ~ "12-31",
                TRUE ~ "Pb"
              ),
              dt_date = paste(stringr::str_sub(dt_trimestre, 1,
                                               4), trim_day, sep = "-")
            ) %>% dplyr::select(-trim_day) %>%
            dplyr::select(
              dplyr::starts_with("g_"),
              dplyr::starts_with("dt_"),
              dplyr::starts_with("mod_"),
              dplyr::starts_with("lgt_")
            )
        ) %>%
        purrr::set_names(eff)

      trim_dernier <-
        purrr::map(ls_onglets,
                   ~ .x %>% dplyr::pull(dt_trimestre) %>%
                     max() %>% as.character()) %>% purrr::flatten_chr() %>%
        unique()

      nb_pieces = vector(mode = "character", length = 0)

      ls_onglets$cor_nbpieces_trim <- ls_onglets$cor_pieces %>%
        dplyr::mutate(
          nb_pieces = dplyr::case_when(
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 1 ~ "t1",
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 2 ~ "t2",
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 3 ~ "t3",
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 4 ~ "t4",
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 5 ~ "t5",
            stringr::str_sub(mod_nbpieces,
                             1, 1) == 6 ~ "t6"
          )
        ) %>% dplyr::select(-lgt_enc) %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("lgt_"),
          names_to = "indic",
          values_to = "valeur"
        ) %>%
        dplyr::group_by(dt_trimestre, dt_date, nb_pieces,
                        indic) %>%
        dplyr::summarise(valeur = sum(valeur), .groups = "drop")

      fn_extrait_tab_pieces <- function(x = "lgt_mev",
                                        y = "nb_lgt_") {
        df_pieces <-
          ls_onglets$cor_nbpieces_trim %>% dplyr::filter(indic %in%
                                                           x) %>% tidyr::pivot_wider(
                                                             names_from = nb_pieces,
                                                             names_prefix = y,
                                                             values_from = valeur
                                                           ) %>% dplyr::select(dt_date,
                                                                               dplyr::starts_with("nb_")) %>%
          dplyr::rename(c("Date" = "dt_date")) %>% dplyr::arrange(desc(Date))
        return(df_pieces)
      }

      noms_tab_pieces <- stringr::str_c(
        c("ECLN_mev_type_lgt_",
          "ECLN_resv_type_lgt_"),
        stringr::str_c(
          stringr::str_sub(trim_dernier,
                           1, 4),
          "T",
          stringr::str_sub(trim_dernier,
                           5, 5)
        )
      )

      ls_pieces <- purrr::imap(c("lgt_mev", "lgt_res") %>%
                                 purrr::set_names(c("nb_lgt_", "nb_resa_")),
                               fn_extrait_tab_pieces) %>% purrr::set_names(noms_tab_pieces)

      purrr::iwalk(ls_pieces, ~ readr::write_csv2(.x, here::here("4_resultats",
                                                           paste0(.y, ".csv"))))



      tab_reg_prix_appart <-
        ls_onglets$reg_coll %>% dplyr::filter(stringr::str_detect(mod_type,
                                                                  "Collectif")) %>%
        dplyr::mutate(prix = round(lgt_prixtot /
                                     lgt_surftot,
                                   0)) %>% dplyr::select(dt_date, g_reg_cd, prix) %>%
        tidyr::pivot_wider(
          names_from = g_reg_cd,
          names_prefix = "ECLN_PRIXM_REG_T\u00a7",
          values_from = prix
        ) %>% dplyr::rename(c("Date" = "dt_date")) %>% dplyr::arrange(desc(Date))

      cat(cli::bg_green(
        cli::col_black(
          "Les premiers fichiers issus de la requete geokit sont dans 4_resultats\n"
        )
      ))
      return(ls_onglets)
    }
    else {
      cat(cli::bg_red(
        cli::col_yellow(
          "Copier le fichier xlsx dans 2_data et relancer_kniter le script\n"
        )
      ))
    }
    return(NA)
  }
