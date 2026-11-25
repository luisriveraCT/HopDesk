# =============================================================================
# tests/test_t109_amortizacion.R
# T109 (Luis, validating against the client's contracts): a financiero's
# Revisión.
#   A  ONE rule, all from the same schedule: "todo el plazo" = the sum of
#      EVERY payment; "por pagar" = the sum of the payments dated today or
#      later; "próximos 12 meses" = [hoy, hoy + 1 year). Checked on Luis's
#      shape (8.75 %, 14 payments, IVA 16 % on interest, 4 months in) against
#      pasivos_costeo(), the table's two views and the Revisión's lines -- and
#      the counter-proof: 9daf0ee0's "Impuestos en todo el plazo" was the
#      "por pagar" sum.
#   B  the same schedule for the summary and the table: a variable rate and
#      the client's day.
#   C  saldo insoluto in every financiero (francesa, arrendamiento puro,
#      grace, an imported schedule with and without principal).
#   D  the full table's estado: pagada (its provision paid), pasada, por pagar.
#   E  the table: columnas_opc and excel (hd_registro), its columns, the
#      switch between views, the Excel with Saldo insoluto; the modal "xl".
# Run from project root:  Rscript --vanilla tests/test_t109_amortizacion.R
# =============================================================================
cat("=== test_t109_amortizacion ===\n\n")
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/monedas.R")
source("R/zona.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R", "R/pasivos_edit_confirm_module.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")
isTRUE_safe <- function(x) !is.na(x) & x      # R/pasivos_observers.R
HOY <- as.Date("2026-11-26")   # the 4th payment (Nov 25) is behind
hoy_cliente <- function(...) HOY
hoy_en <- function(...) HOY + 40               # B: the wizard must not read this one
ESTIMACION <- NA_real_
forecasting_get_estimate <- function(...) ESTIMACION

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
casi <- function(a, b) length(a) == length(b) && all(abs(a - b) < 0.005)
IVA16 <- list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses")
prestamo <- function(...) modifyList(list(
  categoria = "financiero", flavor = "credito_simple", nombre = "Préstamo", empresa = "NG", parte = "Banco",
  moneda_pago = "USD", principal_original = 100000, tasa_tipo = "fija", tasa_actual = 8.75,
  plazo_meses = 14L, dia_pago = 25L, frecuencia_pago = "mensual", periodo_gracia = 0L,
  fecha_inicio = as.Date("2026-07-25"), metodo_amortizacion = "francesa", impuestos = list(IVA16)), list(...))
calendario <- function(d) {
  s <- pasivos_generate_schedule(pasivos_wizard_draft_to_liability(d, mode = "create", user = "x"),
                                 today = HOY, usar_indice = FALSE)
  s[order(s$fecha), ]
}
fila <- function(rows, clave) { r <- Filter(function(x) identical(x[1], tr(clave)), rows); if (length(r)) r[[1]][2] else NA_character_ }
resumen <- function(d) {   # the Revisión summary's lines, label -> value
  h <- paste(as.character(.wiz_revision_summary_html(d)), collapse = "")
  dt <- regmatches(h, gregexpr("<dt[^>]*>.*?</dt>", h))[[1]]; dd <- regmatches(h, gregexpr("<dd[^>]*>.*?</dd>", h))[[1]]
  stats::setNames(gsub("<[^>]+>", "", dd), gsub("<[^>]+>", "", dt))
}

# ── A ────────────────────────────────────────────────────────────────────────
cat("-- A. one rule: todo el plazo, por pagar, próximos 12 meses --\n")
d <- prestamo()
s <- calendario(d)
falta <- s$fecha >= HOY
costo <- s$interes + s$fees + s$impuestos
ok(nrow(s) == 14L && sum(!falta) == 4L, "A: Luis's shape -- 14 payments, 4 already behind")
co <- .wiz_costeo(d)$filas
ok(casi(co$impuestos, sum(s$impuestos)) && casi(co$total, sum(costo)) && casi(co$intereses, sum(s$interes)),
   "A: pasivos_costeo: todo el plazo = Σ every payment (taxes, cost, interest)")
ok(casi(co$impuestos_faltan, sum(s$impuestos[falta])) && casi(co$total_faltan, sum(costo[falta])),
   "A: pasivos_costeo: por pagar = Σ the payments dated today or later")
un_ano <- seq(HOY, by = "1 year", length.out = 2)[2]
ok(casi(co$anual, sum(costo[s$fecha >= HOY & s$fecha < un_ano])), "A: próximos 12 meses = Σ [hoy, hoy + 1 year)")
vc <- pasivos_vista_amortizacion(d, completa = TRUE); vp <- pasivos_vista_amortizacion(d)
ok(nrow(vc) == 14L && casi(sum(vc$impuestos), co$impuestos) && casi(sum(vc$interes), co$intereses),
   "A: the full table adds up to todo el plazo")
ok(nrow(vp) == 10L && casi(sum(vp$impuestos), co$impuestos_faltan), "A: the next payments add up to por pagar")
ok(casi(sum(vc$capital), 100000) && casi(sum(vc$capital + vc$interes + vc$impuestos), sum(vc$total_pago)),
   "A: the full table's capital is the principal; its payments add up")
r <- resumen(d)
mon <- function(x) monto_moneda(x, "USD")
ok(identical(unname(r[tr("tablas.pasivos.impuestos.total_plazo")]), mon(co$impuestos)), "A: 'Impuestos en todo el plazo' = Σ every payment")
ok(identical(unname(r[tr("tablas.pasivos.impuestos.por_pagar")]), mon(co$impuestos_faltan)), "A: 'Impuestos por pagar' = Σ the next payments")
ok(grepl(mon(co$impuestos), r[tr("tablas.pasivos.costeo.desglose_titulo")], fixed = TRUE),
   "A: the breakdown says the same taxes as 'en todo el plazo' (the screenshot's two figures agree)")
ok(identical(unname(r[tr("tablas.pasivos.costeo.total_financiamiento")]), mon(co$total)) &&
     identical(unname(r[tr("tablas.pasivos.costeo.por_pagar")]), mon(co$total_faltan)) &&
     identical(unname(r[tr("tablas.pasivos.costeo.por_ano")]), mon(co$anual)),
   "A: Costo del financiamiento (todo el plazo), Costo por pagar, Costo en los próximos 12 meses")
ok(grepl("todo el plazo", tr("tablas.pasivos.costeo.total_financiamiento"), fixed = TRUE) &&
     grepl("próximos 12 meses", tr("tablas.pasivos.costeo.por_ano"), fixed = TRUE) &&
     grepl("12", tr("tablas.pasivos.costeo.columna"), fixed = TRUE), "A: each label says which rule it is (the Lista's column too)")
nuevo <- prestamo(fecha_inicio = HOY)
rn <- resumen(nuevo)
ok(!tr("tablas.pasivos.impuestos.por_pagar") %in% names(rn) && !tr("tablas.pasivos.costeo.por_pagar") %in% names(rn),
   "A: a loan with nothing behind: no 'por pagar' lines (they would repeat the whole term)")
# Counter-proof: the 9daf0ee0 summary said the 'por pagar' sum as the whole term.
base <- new.env(parent = globalenv())
hb <- tryCatch({
  for (e in parse(text = system2("git", c("show", "9daf0ee0:R/pasivos_wizard_module.R"), stdout = TRUE), keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) eval(e, base)
  paste(as.character(base$.wiz_revision_summary_html(d)), collapse = "")
}, error = function(e) "")
ok(grepl(mon(sum(s$impuestos[falta])), hb, fixed = TRUE) && !grepl(paste0(">", mon(sum(s$impuestos)), "<"), hb, fixed = TRUE),
   "A: counter-proof -- 9daf0ee0 called the 'por pagar' sum 'Impuestos en todo el plazo'")

# ── B ────────────────────────────────────────────────────────────────────────
cat("-- B. the same schedule for the summary and the table --\n")
var <- prestamo(tasa_tipo = "variable_tiie28", tasa_actual = 9, tasa_spread = 250)
ESTIMACION <- 20   # an index far from the rate just fetched
liab_var <- pasivos_wizard_draft_to_liability(var, mode = "create", user = "x")
con_indice <- pasivos_costeo(liab_var, hoy = HOY, usar_indice = TRUE)$filas
ok(!casi(con_indice$intereses, sum(pasivos_vista_amortizacion(var, completa = TRUE)$interes)),
   "B: (with the index, the old costeo and the table disagree -- the stub bites)")
ok(casi(.wiz_costeo(var)$filas$intereses, sum(pasivos_vista_amortizacion(var, completa = TRUE)$interes)),
   "B: variable rate -- the summary's interest = the table's (the rate just fetched)")
ESTIMACION <- NA_real_
ok(casi(.wiz_costeo(d)$filas$impuestos_faltan, sum(pasivos_vista_amortizacion(d)$impuestos)),
   "B: the client's day (hoy_cliente), not hoy_en(), for the summary as for the table")

# ── C ────────────────────────────────────────────────────────────────────────
cat("-- C. saldo insoluto --\n")
ok(casi(vc$saldo[1], 100000 - vc$capital[1]) && all(diff(vc$saldo) < 0) && casi(utils::tail(vc$saldo, 1), 0),
   "C: francesa -- the capital left after each payment, down to 0")
ok(casi(vc$saldo, 100000 - cumsum(vc$capital)), "C: saldo = principal − Σ capital so far, row by row")
puro <- pasivos_vista_amortizacion(prestamo(flavor = "arrendamiento_puro", tasa_actual = 0, impuestos = list()), completa = TRUE)
ok(!is.null(puro) && casi(utils::tail(puro$saldo, 1), 0) && all(diff(puro$saldo) <= 0), "C: arrendamiento puro: down to 0")
gracia <- pasivos_vista_amortizacion(prestamo(periodo_gracia = 3L), completa = TRUE)
ok(casi(gracia$saldo[1:3], rep(100000, 3)) && casi(utils::tail(gracia$saldo, 1), 0), "C: grace periods keep the whole principal")
tabla <- data.frame(periodo = 1:3, fecha = as.Date(c("2026-12-25", "2027-01-25", "2027-02-25")),
                    capital = c(400, 300, 300), interes = c(10, 8, 5), fees = 0, total = c(410, 308, 305))
imp <- prestamo(metodo_amortizacion = "custom", principal_original = 1000, impuestos = list())
imp$schedule_imported <- list(tabla)
li <- pasivos_wizard_draft_to_liability(imp, mode = "create", user = "x"); li$schedule_imported <- list(tabla)
s_imp <- pasivos_generate_schedule(li, today = HOY, usar_indice = FALSE)
ok(casi(.wiz_saldo_insoluto(s_imp, 1000), c(600, 300, 0)), "C: an imported schedule without saldo: principal − Σ capital")
ok(all(is.na(.wiz_saldo_insoluto(s_imp, NA_real_))), "C: ...and without a principal: unknown (NA), never made up")

# ── D ────────────────────────────────────────────────────────────────────────
cat("-- D. the full table's estado --\n")
provs <- data.frame(liability_id = c("L1", "L1", "L1", "L2"), occurrence_index = c(2L, 6L, 7L, 3L),
                    estado = c("item_confirmed", "closed", "provisional", "closed"), stringsAsFactors = FALSE)
ve <- pasivos_vista_amortizacion(d, completa = TRUE, provs = provs, liability_id = "L1")
est <- stats::setNames(ve$estado, ve$periodo)
ok(identical(unname(est[c("2", "6")]), c("pagada", "pagada")), "D: a paid provision (item_confirmed, closed): pagada -- also one not yet due")
ok(identical(unname(est[c("1", "3", "4")]), rep("pasada", 3)), "D: behind today and not paid: pasada")
ok(all(est[as.character(c(5, 7:14))] == "por_pagar"), "D: the rest, a provisional one included: por pagar")
ok(!"pagada" %in% pasivos_vista_amortizacion(d, completa = TRUE, provs = provs, liability_id = NULL)$estado &&
     !"pagada" %in% pasivos_vista_amortizacion(d, completa = TRUE)$estado,
   "D: a new liability (no id, no provisions): only pasada and por pagar; another liability's are never its own")

# ── E ────────────────────────────────────────────────────────────────────────
cat("-- E. the table --\n")
ok(all(c("columnas_opc", "excel") %in% hd_registro$amortizacion$agregar), "E: hd_registro: amortizacion has columnas_opc and excel")
CAPT <- list()
.real <- hd_table_server
hd_table_server <- function(id, ...) { CAPT[[id]] <<- list(...); .real(id, ...) }
datos_p <- datos_c <- NULL
testServer(function(input, output, session) {
  wiz <- reactiveValues(draft = d, amort_vista = "proximas", edit_target_id = "L1")
  sh <- list(pasivos_provisions_db = function() provs)
  .wiz_tablas_vista_previa(wiz, sh)
  session$userData$wiz <- wiz
}, {
  session$flushReact()
  datos_p <<- CAPT$pwiz_amort$datos()
  session$userData$wiz$amort_vista <- "completa"
  session$flushReact()
  datos_c <<- CAPT$pwiz_amort$datos()
})
hd_table_server <- .real
cols <- CAPT$pwiz_amort$columnas
campo <- function(k) Find(function(c) identical(c$campo, k), cols)
ok(nrow(datos_p) == 10L && nrow(datos_c) == 14L && identical(datos_c$estado[datos_c$periodo == 2], "pagada"),
   "E: the switch: next payments, then the whole contract with its estado (the session's provisions)")
ok(!is.null(campo("saldo")) && identical(campo("saldo")$titulo, "tablas.pasivos.amort.saldo") && isTRUE(campo("saldo")$visible %||% TRUE),
   "E: Saldo insoluto, shown")
ok(!is.null(campo("estado")) && identical(campo("estado")$tipo, "insignia"), "E: Estado, a badge")
ok(isTRUE(campo("cargos")$opcional) && identical(campo("cargos")$visible, FALSE) &&
     isTRUE(campo("total")$opcional) && identical(campo("total")$visible, FALSE),
   "E: Cargos and Total (without taxes): optional, hidden by default")
arch <- tempfile(fileext = ".xlsx")
hd_excel_escribir(datos_c, cols, datos_c$id, "id", arch)
x <- openxlsx::read.xlsx(arch, check.names = FALSE, sep.names = " "); unlink(arch)
ok(tr("tablas.pasivos.amort.saldo") %in% names(x) && nrow(x) == 14L && casi(utils::tail(x[[tr("tablas.pasivos.amort.saldo")]], 1), 0),
   "E: the Excel carries every payment with its Saldo insoluto")
ok(grepl("modal-xl", paste(as.character(.pwiz_modal()), collapse = ""), fixed = TRUE), "E: the wizard's modal is xl (read whole at 1293)")
pasos <- paste(readLines("R/pasivos_wizard_steps.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok(!grepl("Amortización (próximas cuotas)", pasos, fixed = TRUE), "E: the table's title comes from the dictionary")
for (idioma in c("es", "en")) {
  dic <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", idioma))
  claves <- c("tablas.pasivos.costeo.por_pagar", "tablas.pasivos.impuestos.por_pagar",
              paste0("tablas.pasivos.amort.", c("cargos", "saldo", "estado", "estado_pagada", "estado_pasada", "estado_por_pagar",
                                                "titulo_proximas", "titulo_completa", "ver_completa", "ver_proximas")))
  ok(all(claves %in% names(dic)), sprintf("E: the new keys in %s.json", idioma))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
