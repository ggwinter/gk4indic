#' transforme_tab_rd1
#'
#' fonction qui va transformer le tableau rd1_commercialisation
#'
#' @param x un vecteur de characteres
#' @importFrom attempt stop_if_not
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr starts_with
#' @importFrom dplyr tibble
#' @importFrom here here
#' @importFrom janitor make_clean_names
#' @importFrom purrr flatten_chr
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom readr write_csv2
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xls
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_which
#' @importFrom tibble tribble
#' @importFrom tidyr fill
#' @importFrom tidyr replace_na
#'
#' @return le tableau mis en forme copie dans le repetoire 4_resultats
#' @export
#'
transforme_tab_rd1 <- function(x = "_rd1_commercialisation") {
  list.files(here::here("2_data"), pattern = x) -> nom_fich
  attempt::stop_if_not(
    .x = length(nom_fich),
    .p = ~ .x < 2,
    msg = "PB il y a plusieurs fichiers rd1 dans 2_data\n"
  )
  attempt::stop_if_not(
    .x = length(nom_fich),
    .p = ~ .x > 0,
    msg = "Pb pas de fichier rd1 dans 2_data\n"
  )

  fich_rd1_trim <-
    stringr::str_extract(nom_fich, "[:digit:]{4}t[:digit:]")

  cat(stringr::str_c(
    "Le fichier rd1 du trimestre ",
    fich_rd1_trim,
    " va \u00eatre import\u00e9\n"
  ))

  vector(mode = "character", length = 0) -> fich_onglets
  readxl::excel_sheets(here::here("2_data", nom_fich)) -> fich_onglets

  readxl::read_xls(here::here("2_data", nom_fich), "94", col_names = FALSE) -> tab1
  vector(mode = "character", length = 0) -> ...1
  tab1 %>%
    dplyr::pull(...1)  %>%
    stringr::str_which("ANNEE") -> lgn_annee

  tab1 %>%
    dplyr::pull(...1) %>%
    stringr::str_which("Source") -> lgn_source

  purrr::map(
    c(lgn_annee, lgn_annee + 1, lgn_annee + 2),
    ~ tab1 %>%
      dplyr::slice(.x) %>%
      purrr::flatten_chr()
  ) %>%
    purrr::map(.f =  ~ stringr::str_replace_all(.x, "[\r\n]", " ")) -> ls_champs

  vector(mode = "character", length = 0) -> lgn0
  vector(mode = "character", length = 0) -> lgn1
  vector(mode = "character", length = 0) -> lgn2
  vector(mode = "character", length = 0) -> champs
  vector(mode = "character", length = 0) -> champsp
  vector(mode = "character", length = 0) -> indic_cd

  t_champs <- dplyr::tibble(
    "lgn0" = ls_champs[[1]],
    "lgn1" = ls_champs[[2]],
    "lgn2" = ls_champs[[3]]
  ) %>%
    purrr::map_dfr(.f = ~ stringr::str_squish(.x))

  t_champs %>%
    tidyr::fill(lgn0, lgn1) %>%
    dplyr::mutate(
      "lgn0" = dplyr::case_when(
        stringr::str_detect(lgn0, "Maisons") ~ "Maisons",
        stringr::str_detect(lgn0, "appartements") ~ "Appartements",
        TRUE ~ lgn0
      ),
      "lgn1" = tidyr::replace_na(lgn1, ""),
      "lgn2" = tidyr::replace_na(lgn2, ""),
      "champs" = paste(lgn0, lgn1, lgn2, sep = "-") %>%
        stringr::str_replace("-$|--$", "") %>% stringr::str_squish(),
      "champsp" = janitor::make_clean_names(champs)
    ) %>%
    dplyr::select(champs, champsp) -> t_champs

  t_champs_valides <- tibble::tribble(
    ~ champs,
    ~ champsp,
    ~ indic_cd,
    "ANNEE",
    "annee",
    "ANNEE",
    "TRIMESTRE",
    "trimestre",
    "TRIMESTRE",
    "Appartements-Logts mis en vente au cours du trimestre-Total",
    "appartements_logts_mis_en_vente_au_cours_du_trimestre_total",
    "MEV_T_A",
    "Appartements-Logts r\u00e9serv\u00e9s au cours du trimestre-Total",
    "appartements_logts_reserves_au_cours_du_trimestre_total",
    "RESV_T_A",
    "Appartements-Encours de logts propos\u00e9s \u00e0 la vente \u00e0 la fin du trimestre-Total",
    "appartements_encours_de_logts_proposes_a_la_vente_a_la_fin_du_trimestre_total",
    "ENC_T_A",
    "Appartements-Prix de vente en euros/m\u00b2 (1)-Total",
    "appartements_prix_de_vente_en_euros_m2_1_total",
    "PVMM2_T_A",
    "Appartements-Encours de logts propos\u00e9s \u00e0 la vente \u00e0 la fin du trimestre-En projet",
    "appartements_encours_de_logts_proposes_a_la_vente_a_la_fin_du_trimestre_en_projet",
    NA,
    "Appartements-Encours de logts propos\u00e9s \u00e0 la vente \u00e0 la fin du trimestre-En cours de construction",
    "appartements_encours_de_logts_proposes_a_la_vente_a_la_fin_du_trimestre_en_cours_de_construction",
    NA,
    "Appartements-Encours de logts propos\u00e9s \u00e0 la vente \u00e0 la fin du trimestre-Achev\u00e9s",
    "appartements_encours_de_logts_proposes_a_la_vente_a_la_fin_du_trimestre_acheves",
    NA,
    "Appartements-\u0025 des logts achev\u00e9s de l\u0027encours total",
    "appartements_percent_des_logts_acheves_de_lencours_total",
    NA,
    "Appartements-D\u00e9lai d\u0027\u00e9coulement",
    "appartements_delai_decoulement",
    "DEC_T_A",
    "Maisons-Logts mis en vente au cours du trimestre-Total",
    "maisons_logts_mis_en_vente_au_cours_du_trimestre_total",
    "MEV_T_M",
    "Maisons-Logts r\u00e9serv\u00e9s au cours du trimestre-Total",
    "maisons_logts_reserves_au_cours_du_trimestre_total",
    "RESV_T_M",
    "Maisons-Encours de logts propos\u00e9s \u00e0 la vente \u00e0 la fin du trimestre-Total",
    "maisons_encours_de_logts_proposes_a_la_vente_a_la_fin_du_trimestre_total",
    "ENC_T_M",
    "Maisons-Prix de vente moyen en euros \u00281\u0029-Total",
    "maisons_prix_de_vente_moyen_en_euros_1_total",
    "PVM_T_M",
    "Maisons-D\u00e9lai d\u0027\u00e9coulement",
    "maisons_delai_decoulement",
    "DEC_T_M"
  )
  # verification que la structure du fichier n"a pas changée nombre et nom des colonnes
  attempt::stop_if_not(
    .x = length(
      which(
        !t_champs %>% dplyr::pull(champsp) %in% t_champs_valides$champsp
      )
    ),
    .p = ~ .x == 0,
    msg = "Pb la structure du fichier rd1 a chang\u00e9\n"
  )

  cat("Ok La structure du fichier rd1 est inchang\u00e9e\n")

  t_champs_valides %>%
    dplyr::filter(stats::complete.cases(indic_cd)) -> t_champs_valides

  which(t_champs %>%
          dplyr::pull(champsp) %in% t_champs_valides$champsp) -> col_a_garder

  vector(mode = "character", length = 0) -> TRIMESTRE
  vector(mode = "character", length = 0) -> ANNEE
  vector(mode = "numeric", length = 0) -> MEV_T_A
  vector(mode = "numeric", length = 0) -> MEV_T_M
  vector(mode = "numeric", length = 0) -> RESV_T_A
  vector(mode = "numeric", length = 0) -> RESV_T_M
  vector(mode = "character", length = 0) -> TRIM_DAY


  readxl::read_xls(
    here::here("2_data", nom_fich),
    "94",
    col_names = FALSE,
    skip = lgn_annee + 3,
    n_max = lgn_source - (lgn_annee + 4),
    na = c("", "nd")
  ) %>%
    dplyr::select(dplyr::all_of(col_a_garder)) %>%
    purrr::set_names(t_champs_valides$indic_cd) %>%
    tidyr::fill(.data$ANNEE) %>%
    dplyr::mutate(
      "ECLN_MEV_AG_T_A" = .data$MEV_T_A,
      "ECLN_MEV_AG_T_M" = .data$MEV_T_M,
      "ECLN_RESV_AG_T_A" = .data$RESV_T_A,
      "ECLN_RESV_AG_T_M" = .data$RESV_T_M,
      "TRIM_DAY" = dplyr::case_when(
        stringr::str_detect(.data$TRIMESTRE, "T1") ~ "03-31",
        stringr::str_detect(.data$TRIMESTRE, "T2") ~ "06-30",
        stringr::str_detect(.data$TRIMESTRE, "T3") ~ "09-30",
        stringr::str_detect(.data$TRIMESTRE, "T4") ~ "12-31",
        TRUE ~ "Pb"
      ),
      "DATE" = paste(.data$ANNEE, .data$TRIM_DAY, sep = "-")
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(dplyr::across(
      c(
        "MEV_T_A",
        "MEV_T_M",
        "RESV_T_A",
        "RESV_T_M",
        "ENC_T_A",
        "ENC_T_M",
        "ECLN_MEV_AG_T_A",
        "ECLN_MEV_AG_T_M",
        "ECLN_RESV_AG_T_A",
        "ECLN_RESV_AG_T_M"
      ),
      as.integer
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("DEC_T"), ~ round(.x, 2))) %>%
    dplyr::mutate(dplyr::across(c("PVMM2_T_A", "PVM_T_M"), ~ round(.x) %>%
      as.integer())) %>%
    dplyr::select(-.data$TRIM_DAY) -> tab_rd1

  readr::write_csv2(tab_rd1,
    here::here(
      "4_resultats",
      paste0("ECLN_tab_rd1_", fich_rd1_trim, "_", Sys.Date(), ".csv")
    ),
    append = FALSE
  )
}
