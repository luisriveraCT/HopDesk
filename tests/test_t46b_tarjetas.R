# =============================================================================
# tests/test_t46b_tarjetas.R
# T46b: Panorama's cards speak the weekly emails' language (semana.tarjeta.*,
# T45). The key "No incluye … · ver Cartera vencida" became two
# (…excluye, …excluye_ver "ver {destino}") so Panorama can point to its own
# place; the Monday email must not change by a single character.
#
# Run from project root:  Rscript tests/test_t46b_tarjetas.R
# =============================================================================
cat("=== test_t46b_tarjetas ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/monedas.R", "R/panorama_datos.R", "R/ui_components.R", "R/tier_registry.R",
            "R/permisos.R", "R/avisos_umbrales.R", "R/avisos_correos.R"))
  source(f)

cat("-- the Monday email: the split key says exactly what the old one said --\n")
lunes <- list(
  lunes = as.Date("2026-11-16"), moneda_base = "MXN",
  cobros = c(MXN = 1120000, USD = 40000), pagos = c(MXN = 842300), cobros_n = 12L, pagos_n = 4L,
  pasivos_n = 2L, pasivos = c(MXN = 96500), cobros_atrasados = c(MXN = 5000, USD = 12.5),
  vencidos_pct = 12.4, vencidos_delta = 1.8, vencido = c(MXN = 120000, USD = 3000),
  por_dia = data.frame(fecha = as.Date("2026-11-16") + 0:4, cobros = 1e5, pagos = 1e5),
  contexto = data.frame(lunes = as.Date("2026-11-16") + 7L * (-4:4), cobros = 1e6, pagos = 9e5,
                        clase = c(rep("real", 4), "esta", rep("programado", 4))),
  top_cobros = NULL, top_pagos = NULL,
  datos_sap = as.POSIXct("2026-11-14 00:40:00", tz = "UTC"), url = "https://hopdesk.example/app")
# The keys' text before T46b (60a44d5), filled by hand.
ANTES <- c(es = "No incluye 5,000.00 MXN · 12.50 USD ya vencidos sin fecha nueva · ver Cartera vencida",
           en = "Does not include 5,000.00 MXN · 12.50 USD already past due with no new date · see Past-due receivables")
for (l in names(ANTES)) {
  h <- correo_lunes_html(lunes, list(idioma = l, ver_cxc = TRUE))
  ok(sprintf("%s: the email's line is the old key's text, character for character", l),
     grepl(paste0("<br>", ANTES[[l]], "</"), h, fixed = TRUE))
}
ok("the two halves exist in es and en, with their placeholders",
   all(vapply(c("es", "en"), function(l) {
     d <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", l))
     grepl("{monto}", d[["semana.tarjeta.por_cobrar.excluye"]], fixed = TRUE) &&
       !grepl("·", d[["semana.tarjeta.por_cobrar.excluye"]], fixed = TRUE) &&
       grepl("{destino}", d[["semana.tarjeta.por_cobrar.excluye_ver"]], fixed = TRUE)
   }, logical(1))))
sin_atrasos <- lunes; sin_atrasos$cobros_atrasados <- NULL
ok("without past-due collections there is no such line",
   !grepl("No incluye", correo_lunes_html(sin_atrasos, list(idioma = "es", ver_cxc = TRUE)), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
