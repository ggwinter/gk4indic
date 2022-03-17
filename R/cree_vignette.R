#' cree_vignette
#'
#' @param x lcharactere
#'
#' @return nothing
#' @noRd
cree_vignette <- function(x = "inst/doc"){
  tools::buildVignettes(dir = ".", tangle = TRUE)
  dir.create("inst/doc")
  file.copy(dir("vignettes", full.names = TRUE), "inst/doc", overwrite = TRUE)
}
