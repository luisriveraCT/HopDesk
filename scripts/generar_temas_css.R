# =============================================================================
# scripts/generar_temas_css.R
# Writes www/hd_temas.css and www/hd_clasico_legado.css from
# R/tema_registro.R (TEMA). Run it after any change to the registry, bump
# HD_TEMAS_CSS_VERSION, and commit the three files. tests/test_temas_guardian.R
# fails when a committed file and the registry disagree.
#
# Run from the repo root:
#   "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla scripts/generar_temas_css.R
# =============================================================================
if (!file.exists("R/tema_registro.R")) stop("Run from the repo root.")
e <- new.env()
sys.source("R/tema_registro.R", envir = e)
escribir <- function(lineas, destino) {
  con <- file(destino, open = "wb")   # LF, as git stores every text file
  writeLines(lineas, con, sep = "\n", useBytes = TRUE)
  close(con)
  cat("Escrito:", destino, "\n")
}
escribir(e$tema_css(),        "www/hd_temas.css")
escribir(e$tema_legado_css(), "www/hd_clasico_legado.css")
