#' verification existence colonnes codegeo et date
#' @param data une df
#' @return La fonction renvoie ok si les colonnes sont présentes ou pb si elles n'y sont pas
#' @noRd
verifie_format_table <- function(data) {
  stopifnot(all(c("codegeo", "date") %in% names(data)))
}


