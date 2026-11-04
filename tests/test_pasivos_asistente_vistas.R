# =============================================================================
# tests/test_pasivos_asistente_vistas.R
# T18: the wizard's Revisión previews on hd_table (amortizacion and
# provisiones_asistente, class E). Luis: the amortization shows EVERY row,
# with a scroll -- it used to show the first 12, "…" and the last 3. The
# provisions preview keeps its first 12. Pure functions plus both tables'
# servers under testServer (they must start, and switch, without failing).
# Run from project root:  Rscript tests/test_pasivos_asistente_vistas.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
source("R/persistence.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_observers.R",
            "R/pasivos_wizard_validate.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_wizard_module.R"))
  suppressMessages(source(f))
fmt_money <- function(x) format(round(x, 2), nsmall = 2)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

credito <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito", empresa = "NTS",
                parte = "Banco", moneda_pago = "USD", principal_original = 120000, saldo_capital = 120000,
                tasa_tipo = "fija", tasa_actual = 12, fecha_inicio = Sys.Date(), plazo_meses = 36L,
                frecuencia_pago = "mensual", dia_pago = 15L, periodo_gracia = 0L,
                metodo_amortizacion = "francesa")
renta <- list(categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "Inmobiliaria",
              moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
              fecha_inicio = Sys.Date(), amount_default = 100)

# ── A: amortization, every row ───────────────────────────────────────────────
a <- pasivos_vista_amortizacion(credito)
sched <- pasivos_generate_schedule(pasivos_wizard_draft_to_liability(credito, user = "x"), today = Sys.Date())
futuras <- sched[!isTRUE_safe(sched$historico), ]
ok(nrow(a) == nrow(futuras) && nrow(a) > 15L,
   sprintf("A1: every future period is a row (%d; the old preview showed 15)", nrow(a)))
ok(identical(a$periodo, as.integer(futuras$periodo)) && !anyDuplicated(a$id), "A1: in order, one id per period")
ok(isTRUE(all.equal(sum(a$capital), 120000, tolerance = 1e-6)), "A2: the principal column adds up to the principal")
ok(all(a$moneda == "USD"), "A2: amounts in the liability's currency")
ok(is.null(pasivos_vista_amortizacion(list(categoria = "financiero"))) ||
     !nrow(pasivos_vista_amortizacion(list(categoria = "financiero"))),
   "A3: an incomplete draft gives no rows (the table says so), never an error")

# ── P: provisions preview, first 12 as before ────────────────────────────────
p <- pasivos_vista_provisiones(renta)
ok(nrow(p) == 12L && !anyDuplicated(p$id), "P1: the first 12 provisions, one id each")
ok(all(p$monto == 100) && identical(p$fecha, sort(p$fecha)), "P1: amount 100, by date")

# ── S: both tables' servers start and follow the draft ───────────────────────
mensajes <- character(0)
withCallingHandlers(
  testServer(function(input, output, session) {
    wiz <- reactiveValues(draft = list())
    .wiz_tablas_vista_previa(wiz)
    session$userData$wiz <- wiz
  }, {
    session$flushReact()
    session$userData$wiz$draft <- credito; session$flushReact()
    session$userData$wiz$draft <- renta;   session$flushReact()
  }),
  message = function(m) { mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage") })
ok(!any(grepl("the (datos|aviso) failed", mensajes)), "S1: neither preview's data fails (empty, financiero, regular)")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
