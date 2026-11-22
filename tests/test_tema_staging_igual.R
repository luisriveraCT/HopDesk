# =============================================================================
# tests/test_tema_staging_igual.R
# TEMA stage 4: R/staging_browse_module.R's manual-entry modal ("Cobro — CxC" /
# "Pago — CxP", show_combined_entry_modal()).
#   - The dead .sb_css block (classes .sb-* nobody uses since the table moved
#     to hd_table in T25) is gone: no .sb-* class anywhere in R/, www/,
#     app.R or tests/ (checked here too), and the modal has no <style> of it.
#   - "Guardar como provisión" paints with roles: var(--hd-provision) and
#     var(--hd-texto-inverso); in Clásico they are its old #7c3aed and white.
# The base and the branch build the modal for AR and AP (showModal() captured)
# in clean processes; the branch's var(--hd-*) are resolved with Clásico.
#
#   TEMA_BASE=<checkout before this change> Rscript --vanilla tests/test_tema_staging_igual.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_staging_igual ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

# The modal of one checkout, AR and AP, as HTML (showModal() replaced to keep it).
modal <- function(raiz, idioma = "es") callr::r(function(raiz, idioma) {
  setwd(raiz)
  options(hopdesk.idioma = idioma)
  suppressPackageStartupMessages({ library(shiny); library(dplyr) })
  `%||%` <- function(a, b) if (is.null(a)) b else a
  assign("%||%", `%||%`, envir = globalenv())
  for (f in c("R/i18n.R", "R/zona.R", "R/tema_registro.R", "R/monedas.R", "R/ui_components.R", "R/hd_registro.R", "R/hd_piezas.R",
              "R/hd_columnas.R", "R/hd_table.R", "R/manual_entry_handlers.R", "R/staging_browse_module.R"))
    if (file.exists(f)) suppressMessages(sys.source(f, envir = globalenv()))
  capturado <- NULL
  assign("showModal", function(ui, ...) capturado <<- ui, envir = globalenv())
  sap <- data.frame(ledger = c("AR", "AP"), Empresa = "NL", Moneda = "MXN", stringsAsFactors = FALSE)
  html <- function(l) {
    show_combined_entry_modal(l, sap, session = NULL, empresa_vals = c("NL", "NG"))
    as.character(capturado)
  }
  c(AR = html("AR"), AP = html("AP"))
}, args = list(raiz = raiz, idioma = idioma))

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
TK <- reg$tema_tokens("clasico")
resolver <- function(html) {
  vs <- unique(regmatches(html, gregexpr("var\\(--hd-[a-z0-9-]+\\)", html))[[1]])
  for (v in vs) html <- gsub(v, TK[[sub("^var\\(--hd-(.+)\\)$", "\\1", v)]], html, fixed = TRUE)
  html
}
sin_sb_css <- function(html) sub("(?s)\\s*<style>\\s*\\.sb-tbl.*?</style>", "", html, perl = TRUE)
norm_color <- function(html) gsub("#7c3aed", "#7C3AED", gsub("color:white", "color:#FFFFFF", html, fixed = TRUE), fixed = TRUE)

codigo <- c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
            list.files("www", pattern = "\\.(css|js)$", recursive = TRUE, full.names = TRUE),
            setdiff(list.files("tests", pattern = "\\.R$", full.names = TRUE), "tests/test_tema_staging_igual.R"))
usa_sb <- codigo[vapply(codigo, function(f) any(grepl("\\bsb-[a-z]", readLines(f, warn = FALSE, encoding = "UTF-8"))), logical(1))]
ok(sprintf("no .sb-* class is used anywhere (R/, www/, app.R, tests/)%s",
           if (length(usa_sb)) paste0(": ", paste(usa_sb, collapse = ", ")) else ""), !length(usa_sb))
ok("R/staging_browse_module.R has no hex or rgb() of its own", {
  x <- readLines("R/staging_browse_module.R", warn = FALSE, encoding = "UTF-8"); x <- x[!grepl("^\\s*#", x)]
  !any(grepl("(?<![&\\w])#[0-9A-Fa-f]{3,8}(?![0-9A-Za-z_-])|\\brgba?\\(", x, perl = TRUE))
})

rama <- modal(RAMA)
ok("the branch builds the modal for AR and AP, with no <style> block", length(rama) == 2L &&
   all(nchar(rama) > 1000L) && !any(grepl("<style>", rama, fixed = TRUE)))
ok("\"Guardar como provisión\" paints with var(--hd-provision) and var(--hd-texto-inverso), only in AP",
   grepl("background-color:var(--hd-provision);color:var(--hd-texto-inverso);", rama[["AP"]], fixed = TRUE) &&
   !grepl("me_save_as_provision", rama[["AR"]], fixed = TRUE))

if (nzchar(BASE)) {
  base <- modal(BASE)
  for (k in names(base)) {
    ok(sprintf("%s: without the dead <style>, with the roles resolved in Clásico, the base's modal byte for byte", k),
       identical(norm_color(sin_sb_css(base[[k]])), norm_color(resolver(rama[[k]]))))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
