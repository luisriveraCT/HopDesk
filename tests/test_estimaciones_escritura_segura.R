# =============================================================================
# tests/test_estimaciones_escritura_segura.R
# T26 · 1: the Pasivos estimates (pasivos_estimates, they feed Pasivos'
# amounts) are written on S3's table as it is now, through
# modificar_pasivos_estimates(). Their writers -- set_estimate / freeze /
# unfreeze, in forecasting_resolve.R (the only definition since
# pron-duplicadas; they were forecasting_service.R's) -- read with
# error -> empty table and saved the whole table: a failed read saved just
# the new row over every other metric's estimate (unfreeze saved it empty).
# On the in-memory S3 of tests/helpers/s3_memoria.R.
#   A  a failed read: nothing is written, the estimates stay, and the error
#      reaches the caller (set / freeze / unfreeze);
#   B  a good read: only that metric changes, the others stay;
#   (C, the second copy of the writers, went with pron-duplicadas)
#   D  no client: nothing is read or written;
#   E  the Pasivos wizard tells the user when the rate could not be recorded.
# Run from project root:  Rscript tests/test_estimaciones_escritura_segura.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R", "R/forecasting_resolve.R"))
  suppressMessages(source(f))
pasivos_log_audit <- function(...) invisible(NULL)
log_action <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(expr)))

CID  <- "acme"
KEY  <- paste0(CID, "/", S3_KEYS$pasivos_estimates)
est  <- function(metric, value, frozen = FALSE)
  tibble::tibble(metric = metric, fecha = as.Date("2026-11-01"), value = value, source_method = "manual",
                 is_frozen = frozen, updated_by = "ana", updated_at = as.POSIXct("2026-11-01 10:00:00"))
EST0 <- dplyr::bind_rows(est("sofr", 4.3), est("tiie28", 10.5, frozen = TRUE), est("fx_usd_mxn", 18.2))
sembrar <- function() { s3_memoria_reset(); callado(save_pasivos_estimates(EST0, client_id = CID)) }
en_s3   <- function() get(KEY, envir = .mem)
falla_lectura <- function() .mem_falla <<- KEY
intento <- function(expr) tryCatch({ callado(expr); NULL }, error = function(e) conditionMessage(e))

# ── A: a failed read writes nothing ──────────────────────────────────────────
CASOS <- list(
  "set_estimate" = function() forecasting_set_estimate("cetes", Sys.Date(), 11.1, client_id = CID),
  "freeze"       = function() forecasting_freeze_metric("sofr", 5, client_id = CID),
  "unfreeze"     = function() forecasting_unfreeze_metric("tiie28", client_id = CID))
for (n in names(CASOS)) {
  sembrar(); antes <- .mem_escrituras; falla_lectura()
  err <- intento(CASOS[[n]]())
  .mem_falla <- character()
  ok(!is.null(err) && .mem_escrituras == antes && identical(nrow(en_s3()), 3L),
     sprintf("A %s, lectura fallida: no escribe, las 3 estimaciones siguen y el error llega al llamador", n))
}

# ── B: a good read changes only that metric ──────────────────────────────────
sembrar()
callado(forecasting_set_estimate("cetes", as.Date("2026-11-01"), 11.1, client_id = CID))
s <- en_s3()
ok(nrow(s) == 4L && setequal(s$metric, c("sofr", "tiie28", "fx_usd_mxn", "cetes")) &&
     s$value[s$metric == "sofr"] == 4.3, "B1 set_estimate: agrega la suya y las demás siguen")
callado(forecasting_set_estimate("sofr", as.Date("2026-11-01"), 4.9, client_id = CID))
s <- en_s3()
ok(nrow(s) == 4L && s$value[s$metric == "sofr"] == 4.9 && s$value[s$metric == "fx_usd_mxn"] == 18.2,
   "B2 set_estimate de la misma fecha: la reemplaza, sin duplicar")
callado(forecasting_unfreeze_metric("tiie28", client_id = CID))
s <- en_s3()
ok(nrow(s) == 4L && !any(s$is_frozen), "B3 unfreeze: solo descongela, no borra nada")
# Another session added an estimate since this one last read: kept.
assign(KEY, dplyr::bind_rows(en_s3(), est("udis", 8.1)), envir = .mem)
callado(forecasting_freeze_metric("sofr", 5, client_id = CID))
s <- en_s3()
ok("udis" %in% s$metric && sum(s$metric == "sofr" & s$is_frozen) == 1L,
   "B4 freeze sobre lo fresco: conserva la estimación que otra sesión agregó")

# ── D: no client ─────────────────────────────────────────────────────────────
sembrar(); antes <- .mem_escrituras
err <- intento(forecasting_set_estimate("cetes", Sys.Date(), 1, client_id = NULL))
ok(!is.null(err) && .mem_escrituras == antes, "D sin client_id: no lee ni escribe (nunca la llave por defecto)")

# ── E: the wizard says so ────────────────────────────────────────────────────
wiz <- paste(readLines("R/pasivos_wizard_module.R", encoding = "UTF-8"), collapse = "\n")
ok(grepl('forecasting_set_estimate failed[^\n]*\n[^\n]*showNotification\\(tr\\("tablas.pasivos.tasa_no_guardada"', wiz),
   "E el asistente avisa al usuario si la tasa no se pudo registrar (antes solo un warning en el log)")

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
