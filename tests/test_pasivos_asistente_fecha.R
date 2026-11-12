# =============================================================================
# tests/test_pasivos_asistente_fecha.R
# The Pasivos wizard's dates (fix/pasivos-asistente-fecha, after CM22):
#   A  fecha_inicio missing in any shape -- NULL, NA, "", a zero-length Date
#      (a cleared dateInput) -- is an error for regular and financiero; it
#      used to throw "argument is of length zero" on the last one;
#   B  "Cada N días" needs its anchor date;
#   C  an initial fee with an amount needs its date (amount 0 = empty row,
#      ignored); a residual paid "separate" needs its date;
#   D  "Siguiente" on the cargos step validates it;
#   E  end to end, why: without the checks, a fee with no date becomes a
#      provision with no date, and an anchor with no date stops generation.
# Run from project root:  Rscript tests/test_pasivos_asistente_fecha.R
# VALIDA_R=<file> checks another copy of the validators (the contraprueba).
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(lubridate); library(jsonlite); library(uuid)
})
cat("=== test_pasivos_asistente_fecha ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
CURRENCIES <- c("MXN", "USD")
suppressMessages(source("R/i18n.R"))
source("R/pasivos_schemas.R")
source(Sys.getenv("VALIDA_R", "R/pasivos_wizard_validate.R"))

draft <- function(...) {
  base <- list(categoria = "regular", flavor = NA_character_, nombre = "Renta", empresa = "NCS",
               parte = "Proveedor A", moneda_pago = "MXN", recurrence_type = "monthly_day",
               recurrence_params = list(day = 15L), fecha_inicio = as.Date("2026-06-01"),
               amount_default = 1500, principal_original = 100000, tasa_tipo = "fija", tasa_actual = 12,
               plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 5L,
               metodo_amortizacion = "francesa", schedule_csv_raw = "",
               cargos_iniciales = NULL, valor_residual = NULL)
  o <- list(...); for (nm in names(o)) base[nm] <- list(o[[nm]]); base
}
recoge <- function(v) tryCatch(as.Date(v %||% NA), error = function(e) as.Date(NA))   # what each step's collector does
vacias <- list(`NULL` = NULL, `NA` = NA, `""` = "", `Date(0)` = as.Date(character(0)))
valida <- function(d) tryCatch(pasivos_wizard_validate_final(d, "NCS"), error = function(e) e)

# ── A. fecha_inicio ──────────────────────────────────────────────────────────
for (cat in c("regular", "financiero")) for (nm in names(vacias)) {
  v <- valida(draft(categoria = cat, fecha_inicio = recoge(vacias[[nm]])))
  ok(sprintf("A %s, fecha de inicio %s: error en el campo, sin tronar", cat, nm),
     !inherits(v, "error") && !v$ok && "fecha_inicio" %in% names(v$errors))
}
for (cat in c("regular", "financiero")) {
  v <- valida(draft(categoria = cat))
  ok(sprintf("A %s con fecha: sin error de fecha", cat), !inherits(v, "error") && v$ok)
}
ok("A el mensaje sale del diccionario",
   identical(valida(draft(fecha_inicio = NA))$errors$fecha_inicio, tr("tablas.pasivos.asistente.fecha_inicio")) &&
   identical(valida(draft(categoria = "financiero", fecha_inicio = NA))$errors$fecha_inicio,
             tr("tablas.pasivos.asistente.fecha_inicio_contrato")))

# ── B. "Cada N días" anchor ─────────────────────────────────────────────────
for (nm in names(vacias)) {
  d <- draft(recurrence_type = "interval_days",
             recurrence_params = list(anchor_date = recoge(vacias[[nm]]), interval_days = 30L))
  v <- valida(d)
  ok(sprintf("B Cada N días, ancla %s: error", nm), !inherits(v, "error") && "anchor_date" %in% names(v$errors))
}
ok("B Cada N días con ancla: sin error",
   valida(draft(recurrence_type = "interval_days",
                recurrence_params = list(anchor_date = as.Date("2026-06-01"), interval_days = 30L)))$ok)

# ── C. Fees and residual ────────────────────────────────────────────────────
fin <- function(...) draft(categoria = "financiero", ...)
cargo <- function(monto, fecha) list(desc = "Comisión", monto = monto, moneda = "MXN", fecha = fecha)
v <- valida(fin(cargos_iniciales = list(cargo(500, as.Date("2026-06-01")), cargo(250, recoge(NA)))))
ok("C un cargo con monto y sin fecha: error, y nombra cuál (el 2)",
   !v$ok && identical(names(v$errors), "cargo_fecha_2") && grepl("2", v$errors$cargo_fecha_2))
ok("C un cargo con monto 0 y sin fecha es una fila vacía: se ignora",
   valida(fin(cargos_iniciales = list(cargo(0, recoge(NA)))))$ok)
ok("C un cargo con fecha vacía de longitud 0 no truena",
   !inherits(valida(fin(cargos_iniciales = list(cargo(500, as.Date(character(0)))))), "error"))
res <- function(behavior, fecha) list(monto = 1000, moneda = "MXN", fecha = fecha, behavior = behavior)
ok("C residual aparte sin fecha: error", "residual_fecha" %in% names(valida(fin(valor_residual = res("separate", recoge(NA))))$errors))
ok("C residual aparte con fecha: sin error", valida(fin(valor_residual = res("separate", as.Date("2027-06-01"))))$ok)
ok("C residual que reemplaza o se suma a la última cuota no pide fecha",
   valida(fin(valor_residual = res("replace_last", recoge(NA))))$ok &&
   valida(fin(valor_residual = res("add_to_last", recoge(NA))))$ok)

# ── D. "Siguiente" on the cargos step ───────────────────────────────────────
modulo <- paste(readLines("R/pasivos_wizard_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("D Siguiente valida el paso de cargos",
   grepl("cargos      = pasivos_wizard_validate_cargos(wiz$draft)$errors", modulo, fixed = TRUE))

# ── E. End to end: what the checks prevent ──────────────────────────────────
e <- tryCatch({
  suppressMessages(suppressWarnings({
    source("R/persistence.R"); source("R/bancos_persistence.R"); source("R/pasivos_persistence.R")
    source("R/pasivos_capabilities.R"); source("R/pasivos_audit.R"); source("R/forecasting_resolve.R")
    source("R/policy_engine.R"); source("R/pasivos_engine.R"); source("R/pasivos_wizard_module.R")
  })); TRUE }, error = function(e) conditionMessage(e))
ok("E el motor y el asistente cargan", isTRUE(e))
genera <- function(d) {
  liab <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "prueba")
  tryCatch(pasivos_generate_provisions(liab, window_start = as.Date("2026-06-01"), window_end = NULL,
                                       policies_for_liability = list(), holidays_cache = list()),
           error = function(e) e)
}
g <- genera(fin(cargos_iniciales = list(cargo(500, recoge(NA)))))
ok("E sin la revisión, un cargo sin fecha se vuelve una provisión sin fecha (o truena)",
   inherits(g, "error") || any(is.na(g$fecha_efectiva)))
g_ok <- genera(fin(cargos_iniciales = list(cargo(500, as.Date("2026-06-15")))))
ok("E con fecha, ninguna provisión queda sin fecha", !inherits(g_ok, "error") && nrow(g_ok) > 0L && !any(is.na(g_ok$fecha_efectiva)))
g2 <- genera(draft(recurrence_type = "interval_days", recurrence_params = list(anchor_date = recoge(NA), interval_days = 30L)))
ok("E sin la revisión, Cada N días sin ancla no genera provisiones", inherits(g2, "error") || !nrow(g2))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
