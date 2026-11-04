# =============================================================================
# tests/test_pasivos_papelera.R
# T18 (Luis): a deleted liability ("Borrar") leaves every view together with
# ALL its provisions, which stay on S3 untouched, and recovering it brings them
# back exactly. Real Lista module under testServer, in-memory S3.
# Run from project root:  Rscript tests/test_pasivos_papelera.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_calendar_glue.R",
            "R/i18n.R", "R/pasivos_list_module.R"))
  suppressMessages(source(f))
PERMITIDO <- TRUE
has_capability    <- function(user, cap) PERMITIDO
permiso_nombre    <- function(p) p
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
strip_accents     <- function(x) stringi::stri_trans_general(x %||% "", "Latin-ASCII")
AUDIT <- list()
pasivos_log_audit <- function(...) { AUDIT[[length(AUDIT) + 1L]] <<- list(...); invisible(NULL) }

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date() - 70, amount_default = 100), mode = "create", user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date() - 70)
futuras <- provs0$id[provs0$fecha_efectiva >= Sys.Date()]
pasadas <- provs0$id[provs0$fecha_efectiva <  Sys.Date()]
provs0$amount_pago_override[provs0$id == futuras[2]] <- 150      # a hand edit
# One payment already in the Agenda (converted and staged)
provs0$estado[provs0$id == pasadas[1]]       <- "converted"
provs0$pagar_hoy_id[provs0$id == pasadas[1]] <- "PH-1"
# Other liabilities' provisions share the table, as in production. (With L1
# alone, the old code's drop of its future provisions was refused on S3 by the
# CM09 net -- over half the table -- and only the session lost them.)
otras <- dplyr::bind_rows(lapply(c("L7", "L8", "L9"), function(l) {
  x <- liab0; x$id <- l; pasivos_generate_provisions(x, window_start = Sys.Date() - 70)
}))
provs0 <- dplyr::bind_rows(provs0, otras)

nuevo_shared <- function(liab = liab0, provs = provs0) {
  L <- reactiveVal(liab); P <- reactiveVal(provs)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       visible_initials = function() NULL,
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function(liab = liab0, provs = provs0) {
  s3_memoria_reset(); AUDIT <<- list()
  save_pasivos_liabilities(liab, client_id = CID)
  .s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
}
provs_s3 <- function() load_pasivos_provisions(client_id = CID)
liab_s3  <- function(id = "L1") {          # raw: the loader drops POSIXct columns (T18a report)
  l <- get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)
  l[l$id == id, , drop = FALSE]
}
firma <- function(p) { p <- p[order(p$id), c("id", "estado", "amount_pago", "amount_pago_override", "fecha_efectiva", "pagar_hoy_id")]; paste(capture.output(print(as.data.frame(p))), collapse = "\n") }


# ── D0: what the modal counts ────────────────────────────────────────────────
ok(pasivos_pagos_en_agenda(provs0, "L1") == 1L, "D0: 1 payment of L1 is in the Agenda")
ok(pasivos_pagos_en_agenda(provs0, "OTRO") == 0L, "D0: none for another liability")
ok(pasivos_pagos_en_agenda(provs0[0, ], "L1") == 0L, "D0: none with no provisions")

# ── D1: Borrar keeps every provision on S3 ───────────────────────────────────
sembrar(); sh <- nuevo_shared()
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()   # observers start here, so the click below is not their ignored first run
  session$setInputs(pasivos_list_archive = "L1")
  session$setInputs(plist_confirm_archive = 1)
})
ok(identical(liab_s3()$estado, "deleted"), "D1: L1 is deleted on S3")
ok(identical(isolate(sh$pasivos_liabilities_db())$estado, "deleted"), "D1: and in the session")
ok(identical(firma(provs_s3()), firma(provs0)),
   "D1: all of L1's provisions are still on S3, unchanged (future ones and the hand edit too)")
ok(identical(firma(isolate(sh$pasivos_provisions_db())), firma(provs0)),
   "D1: and in the session's copy")
a <- Filter(function(x) identical(x$action_type, "liability.deleted"), AUDIT)
ok(length(a) == 1L && !length(Filter(function(x) identical(x$action_type, "provision.deleted"), AUDIT)),
   "D1: one liability.deleted in the audit, no provision.deleted")

# ── D2: and they leave every view (Calendar, Agenda, Flujo read these rows) ──
borrado <- liab0; borrado$estado <- "deleted"
ok(!nrow(pasivos_provisions_as_ledger_rows(provs0[provs0$liability_id == "L1", ], borrado)),
   "D2: none of L1's provisional provisions reaches the ledger once L1 is deleted")
ok(nrow(pasivos_provisions_as_ledger_rows(provs0[provs0$liability_id == "L1", ], liab0)) > 0L,
   "D2: (they do while L1 is active)")

# ── D3: without permission nothing is written ────────────────────────────────
# The confirm itself checks on the server: the permission is gone between the
# click (which opened the modal) and the confirm.
sembrar(); sh <- nuevo_shared()
antes <- .mem_escrituras
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()   # observers start here, so the click below is not their ignored first run
  session$setInputs(pasivos_list_archive = "L1")
  PERMITIDO <<- FALSE
  session$setInputs(plist_confirm_archive = 1)
})
PERMITIDO <- TRUE
ok(.mem_escrituras == antes && identical(liab_s3()$estado, "active"),
   "D3: without can_manage_pasivos at the confirm, Borrar writes nothing")

# ── R1: Borrar, then Recuperar: everything comes back exactly ────────────────
sembrar(); sh <- nuevo_shared()
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()
  session$setInputs(pasivos_list_archive = "L1")
  session$setInputs(plist_confirm_archive = 1)
  session$setInputs(pasivos_list_restore = "L1")
})
ok(identical(liab_s3()$estado, "active"), "R1: L1 is active again on S3")
ok(identical(firma(provs_s3()), firma(provs0)),
   "R1: with every provision as it was: same ids, the hand edit, the Agenda payment")
p_s3 <- provs_s3()
ok(identical(sort(pasivos_provisions_as_ledger_rows(p_s3[p_s3$liability_id %in% "L1", ], liab_s3())$Documento),
             sort(pasivos_provisions_as_ledger_rows(provs0[provs0$liability_id == "L1", ], liab0)$Documento)),
   "R1: and they reach the ledger again, the same rows as before Borrar")
r <- Filter(function(x) identical(x$action_type, "liability.restored"), AUDIT)
ok(length(r) == 1L && identical(r[[1]]$target_id, "L1"), "R1: one liability.restored in the audit")

# ── R2: Recuperar without permission writes nothing ──────────────────────────
borrado_s3 <- liab0; borrado_s3$estado <- "deleted"
sembrar(borrado_s3); sh <- nuevo_shared(borrado_s3); PERMITIDO <- FALSE
antes <- .mem_escrituras
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()
  session$setInputs(pasivos_list_restore = "L1")
})
PERMITIDO <- TRUE
ok(.mem_escrituras == antes && identical(liab_s3()$estado, "deleted"),
   "R2: without can_manage_pasivos, Recuperar writes nothing")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
