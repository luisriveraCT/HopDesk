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

cat("\n-- Panorama's data --\n")
for (f in c("R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
HOY <- Sys.Date()
ar <- data.frame(FechaEff = HOY + c(0, -1, -30, 1), Moneda = c("MXN", "MXN", "USD", "MXN"),
                 `Saldo vencido` = c(100, 500, 7, 900), check.names = FALSE, stringsAsFactors = FALSE)
at <- panorama_cobros_atrasados(ar, HOY)
ok("panorama_cobros_atrasados: the CxC rows whose date already went by, by currency (the email's rule)",
   identical(at$moneda, c("MXN", "USD")) && identical(at$total, c(500, 7)))
ok("panorama_cobros_atrasados: nothing without FechaEff", nrow(panorama_cobros_atrasados(data.frame(x = 1), HOY)) == 0)
ok("panorama_cobros_atrasados: `hoy` has no default (the client's day, never the server's Sys.Date(); CM27)",
   identical(formals(panorama_cobros_atrasados)$hoy, quote(expr = )))

cat("\n-- Panorama's cards, drawn by the server --\n")
ph <- data.frame(ledger = "AP", status = "pending", tipo_item = "factura", Moneda = "MXN",
                 Importe = c(100, 200), FechaVenc = HOY + c(0, -2), stringsAsFactors = FALSE)
provs <- data.frame(estado = "provisional", fecha_efectiva = HOY + 1, fecha_efectiva_override = NA,
                    amount_pago = 50, amount_pago_override = NA, moneda_pago = "MXN", stringsAsFactors = FALSE)
pintar <- function(idioma) {
  op <- options(hopdesk.idioma = idioma); on.exit(options(op))
  shared <- list(
    effective_client_id = function() "cli", usuario = function() list(user = "ana", tier = nivel_ti(), permisos = list()),
    current_user_info = function() list(user = "ana", name = "Ana", is_staff = TRUE),
    tier_efectivo = function() nivel_ti(), pagar_hoy_db = function() ph,
    pasivos_provisions_db = function() provs, sap_snapshot_info = function() list(),
    df_combined_AR = function() ar, fx_fix = function() NULL, moneda_base_cliente = function() list(moneda = "MXN"))
  out <- NULL
  testServer(panoramaServer, args = list(shared = shared, activa = shiny::reactive(FALSE)), {
    session$flushReact()
    out <<- list(hoy = output$hoy$html, clima = output$clima$html, cinta = output$cinta$html)
  })
  lapply(out, function(x) htmltools::HTML(as.character(x)))
}
es <- pintar("es")
tiene <- function(x, s) grepl(s, as.character(x), fixed = TRUE)
ok("the Agenda's payments are 'Programado para pagar hoy', with what they are and where they lead",
   tiene(es$hoy, "Programado para pagar hoy") && tiene(es$hoy, "300.00 MXN") &&
     tiene(es$hoy, "Lo que está en la Agenda de Hoy, incluidos atrasados · 2 pagos") &&
     tiene(es$hoy, "Ver en Agenda de Hoy →"))
ok("never 'Por pagar' on screen: another measure, another word", !tiene(es$hoy, "Por pagar"))
ok("'Por cobrar hoy' explains itself with the email's own words (semana.tarjeta.por_cobrar.*)",
   tiene(es$hoy, "Por cobrar hoy") && tiene(es$hoy, "100.00 MXN") &&
     tiene(es$hoy, paste(tr("semana.tarjeta.por_cobrar.explica", idioma = "es", rango = "hoy"), "·",
                         tr_n("semana.tarjeta.por_cobrar.n", 1L, idioma = "es"))) &&
     tiene(es$hoy, "Facturas de clientes que vencen hoy · 1 factura"))
ok("…and says what it leaves out, as the email does, pointing to Vencidos",
   tiene(es$hoy, "No incluye 500.00 MXN · 7.00 USD ya vencidos sin fecha nueva · ver Vencidos"))
ok("'Pasivos próximos (3 días)' with its line and its tab",
   tiene(es$hoy, "Pasivos próximos (3 días)") && tiene(es$hoy, "Provisiones abiertas que vencen de hoy al") &&
     tiene(es$hoy, "1 pasivo · vence el") && tiene(es$hoy, "Ver en Pasivos →"))
ok("the old labels are gone", !tiene(es$hoy, "Pagos de hoy") && !tiene(es$hoy, "Cobros que vencen hoy") &&
     !tiene(es$hoy, "· Agenda de Hoy →"))
ok("Clima: one short line of what it sums up, and each thing by its name",
   tiene(es$clima, "Resume SAP, avisos urgentes, alertas y solicitudes") &&
     tiene(es$clima, "2 pagos por atender hoy · ningún aviso urgente") && !tiene(es$clima, "pendientes"))
ok("Clima: no jargon ('umbral', 'fuera de rango')", !tiene(es$clima, "umbral") && !tiene(es$clima, "fuera de rango"))
ok("La Cinta uses the cards' words",
   tiene(es$cinta, "Programado para pagar 300.00 MXN") && tiene(es$cinta, "Por cobrar 100.00 MXN"))
en <- pintar("en")
ok("in English, in English", tiene(en$hoy, "Scheduled to pay today") && tiene(en$hoy, "To collect today") &&
     tiene(en$hoy, "Customer invoices due today · 1 invoice") && tiene(en$hoy, "see Past due") &&
     tiene(en$clima, "2 payments to handle today · no urgent notices"))
crudas <- function(x) grepl("panorama\\.|semana\\.|\\{[a-z_]+\\}", paste(vapply(x, as.character, ""), collapse = " "))
ok("no raw keys nor unfilled placeholders (es and en)", !crudas(es) && !crudas(en))

cat("\n-- the dictionary --\n")
dic <- lapply(c(es = "es", en = "en"), function(l) jsonlite::fromJSON(sprintf("www/i18n/%s.json", l)))
ok("the clima's line is short: 8 words at most, es and en",
   all(vapply(dic, function(d) length(strsplit(d[["panorama.clima.explica"]], "\\s+")[[1]]) <= 8L, logical(1))))
ok("the replaced keys are gone (es and en)",
   !any(c("panorama.hoy.pagos", "panorama.hoy.cobros", "panorama.hoy.pasivos", "panorama.clima.pendientes.uno",
          "panorama.clima.ninguno_urgente") %in% c(names(dic$es), names(dic$en))))
ok("the same new keys in es and en",
   setequal(grep("^panorama\\.(hoy|destino|clima\\.cuenta)\\.", names(dic$es), value = TRUE),
            grep("^panorama\\.(hoy|destino|clima\\.cuenta)\\.", names(dic$en), value = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
