# =============================================================================
# scripts/retirar_legado_clasico.R
# The day Clásico's exact look is no longer needed (Luis' call), every legacy
# token becomes its role:
#     var(--hd-leg-texto-0d1b2a)      -> var(--hd-texto)
#     tema_color("leg-texto-0d1b2a")  -> tema_color("texto")
# over app.R, R/ and www/ (.css/.js). The rule itself is tema_legado_retirar()
# in R/tema_registro.R, which tests/test_temas_guardian.R checks.
#
# Without --aplicar it only lists what would change (dry run). With it, it
# rewrites the files; then empty .HD_LEGADO by hand, run
# scripts/generar_temas_css.R, and the guardian (its legacy ratchet goes to 0).
#
# Run from the repo root:
#   Rscript --vanilla scripts/retirar_legado_clasico.R            (dry run)
#   Rscript --vanilla scripts/retirar_legado_clasico.R --aplicar
# =============================================================================
if (!file.exists("R/tema_registro.R")) stop("Run from the repo root.")
aplicar <- "--aplicar" %in% commandArgs(trailingOnly = TRUE)
e <- new.env()
sys.source("R/tema_registro.R", envir = e)

archivos <- c("app.R",
  list.files("R",   pattern = "\\.R$",        recursive = TRUE, full.names = TRUE),
  list.files("www", pattern = "\\.(css|js)$", recursive = TRUE, full.names = TRUE))
# The registry and the generated legacy file are rewritten by hand / by the generator.
archivos <- setdiff(archivos, c("R/tema_registro.R", "www/hd_clasico_legado.css"))

total <- 0L
for (f in archivos) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  y <- e$tema_legado_retirar(x)
  cambian <- which(x != y)
  if (!length(cambian)) next
  total <- total + length(cambian)
  cat(sprintf("%s: %d line(s)\n", f, length(cambian)))
  for (i in cambian) cat(sprintf("  %d: %s\n   -> %s\n", i, trimws(x[i]), trimws(y[i])))
  if (aplicar) {
    crlf <- any(grepl("\r", readChar(f, file.size(f), useBytes = TRUE), fixed = TRUE))
    con <- file(f, open = "wb")
    writeLines(y, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE)
    close(con)
  }
}
cat(sprintf("\n%d line(s) %s.\n", total, if (aplicar) "rewritten" else "would change (dry run; --aplicar to write)"))
if (aplicar) cat("Now empty .HD_LEGADO in R/tema_registro.R and run scripts/generar_temas_css.R.\n")
