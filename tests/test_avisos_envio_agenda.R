# =============================================================================
# tests/test_avisos_envio_agenda.R
# T27 · 2: what sending invoices to the Agenda says comes from the dictionary
# (es and en). The day's send (.dia_agregar, R/ledger_module.R) and
# Intercompany's (.ic_send_rows and its caller, R/interco_module.R) wrote
# their notices by hand, in Spanish: the failed save, the success, "no
# invoices" and "N factura(s) añadida(s)".
#   1. no notice in those functions is written by hand;
#   2. each one names its key;
#   3. the keys are in es and en, translated, and fill in their counts.
# (By behavior in a real browser: tests/test_interco_hd_chromote.R reads the
# failed-save notice.)
# Run from project root:  Rscript tests/test_avisos_envio_agenda.R
# =============================================================================
cat("=== test_avisos_envio_agenda ===\n\n")
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressMessages(source("R/i18n.R"))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# A function's text: from its "<- function" line to its closing brace at the
# same indentation.
cuerpo <- function(archivo, inicio) {
  x <- readLines(archivo, encoding = "UTF-8")
  i <- grep(inicio, x)[1]
  if (is.na(i)) return(NA_character_)
  sangria <- sub("^(\\s*).*", "\\1", x[i])
  fin <- which(x == paste0(sangria, "}"))
  fin <- fin[fin > i][1]
  paste(x[i:fin], collapse = "\n")
}
dia <- cuerpo("R/ledger_module.R", "^\\s*\\.dia_agregar <- function")
ic  <- cuerpo("R/interco_module.R", "^\\s*\\.ic_send_rows <- function")
# The "añadida(s)" notice is in the caller of .ic_send_rows, the detail's action.
ic_txt <- paste(readLines("R/interco_module.R", encoding = "UTF-8"), collapse = "\n")
ok("found .dia_agregar and .ic_send_rows", !is.na(dia) && !is.na(ic))

a_mano <- function(txt) grepl("showNotification\\(\\s*(paste0?\\(|\"|if \\([^)]*\\)\\)?\\s*\\n?\\s*paste0?\\()", txt, perl = TRUE)
ok("1: no notice in .dia_agregar is written by hand", !a_mano(dia))
ok("1: no notice in .ic_send_rows is written by hand", !a_mano(ic))
ok("1: Intercompany's \"añadida(s)\" is not written by hand",
   !grepl("factura a\\\\u00f1adida|factura\\(s\\) a\\\\u00f1adida|factura añadida", ic_txt))

ok("2: .dia_agregar's failed save says tablas.agenda.no_enviado",
   grepl('tr("tablas.agenda.no_enviado", n = stage_result$attempts)', dia, fixed = TRUE))
ok("2: .dia_agregar's success says tablas.agenda.enviadas_cobros / _pagos",
   grepl('"tablas.agenda.enviadas_cobros"', dia, fixed = TRUE) && grepl('"tablas.agenda.enviadas_pagos"', dia, fixed = TRUE))
ok("2: .dia_agregar's \"no invoices\" says tablas.agenda.sin_facturas_provisiones",
   grepl('tr("tablas.agenda.sin_facturas_provisiones")', dia, fixed = TRUE))
ok("2: .ic_send_rows' failed save says tablas.agenda.no_enviado",
   grepl('tr("tablas.agenda.no_enviado", n = res$attempts)', ic, fixed = TRUE))
ok("2: Intercompany's \"añadida(s)\" says tablas.interco.anadida_una / anadidas",
   grepl('tr("tablas.interco.anadida_una", n = added)', ic_txt, fixed = TRUE) &&
     grepl('tr("tablas.interco.anadidas", n = added)', ic_txt, fixed = TRUE))

claves <- c("tablas.agenda.no_enviado", "tablas.agenda.sin_facturas_provisiones", "tablas.agenda.enviadas_cobros",
            "tablas.agenda.enviadas_pagos", "tablas.interco.anadida_una", "tablas.interco.anadidas")
es <- fromJSON("www/i18n/es.json"); en <- fromJSON("www/i18n/en.json")
ok("3: every key is in es and en, translated",
   all(claves %in% names(es)) && all(claves %in% names(en)) && !any(unlist(es[claves]) == unlist(en[claves])))
ok("3: the failed save keeps its words and fills in the attempts",
   identical(tr("tablas.agenda.no_enviado", n = 3, idioma = "es"),
             "No se pudo enviar a Agenda tras 3 intento(s) (posible problema de conexión). Actualiza la página e intenta de nuevo.") &&
     grepl("after 3 attempt", tr("tablas.agenda.no_enviado", n = 3, idioma = "en"), fixed = TRUE))
ok("3: the success names the count and the companies",
   identical(tr("tablas.agenda.enviadas_pagos", n = 2, empresas = "NTS, NCS", idioma = "es"),
             "✓ 2 factura(s) enviadas a Agenda del día (Pagos) — NTS, NCS."))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
