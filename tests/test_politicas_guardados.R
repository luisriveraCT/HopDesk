# =============================================================================
# tests/test_politicas_guardados.R
# CM12 (e), 2026-11-04: Revertir, saving a partner's policies and Aplicar
# (settings_companies.R) wrote this session's policy_moves / partner_policies
# whole: with the copy stale or empty from a failed read, another session's
# rows were lost; clearing a partner's old dates swallowed its error. Now
# each runs on S3's current table (modificar_policy_moves() /
# modificar_partner_policies(), strict read), removing only what it means to.
#
# Runs the REAL settings_companies_observer in shiny::testServer over the
# in-memory S3; compute_policy_moves is stubbed (one date per invoice).
#
# Run from project root:  Rscript tests/test_politicas_guardados.R
# =============================================================================
cat("=== test_politicas_guardados ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/settings/settings_companies.R"))
  suppressMessages(source(f))
AVISOS <- character()
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible("id") }
showModal <- function(...) invisible(NULL); removeModal <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
compute_policy_moves <- function(invoices_df, partner_policies_df, policy_catalog_df,
                                 holidays_cache = list(), overrides_df = NULL,
                                 ledger = "AP", applied_by = "system", applied_at = NULL)
  tibble::tibble(ledger = ledger, Empresa = invoices_df$Empresa, Moneda = invoices_df$Moneda,
                 Documento = invoices_df$Documento, Parte = invoices_df$Parte,
                 FechaVenc_Politica = as.Date("2026-12-31"), policy_ids = "p1",
                 applied_by = applied_by, applied_at = Sys.time())

CID <- "acme"
ADMIN <- list(user = "admin", tier = "admin", permisos = permisos_resueltos("admin"))
k_pm <- .s3_key(S3_KEYS$policy_moves,     client_id = CID)
k_pp <- .s3_key(S3_KEYS$partner_policies, client_id = CID)
k_mv <- .s3_key(S3_KEYS$moves,            client_id = CID)
move <- function(doc, parte, fecha = "2026-11-20", por = "luis")
  .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = doc, Parte = parte,
                            FechaVenc_Politica = as.Date(fecha), policy_ids = "manual", applied_by = por,
                            applied_at = as.POSIXct("2026-11-01", tz = "UTC")), .schema_policy_moves)
socio <- function(parte, pid = "p1")
  .normalize(tibble::tibble(parte = parte, policy_id = pid, policy_order = 1L, ledger = "AP",
                            is_interco = FALSE, is_manual = TRUE, linked_by = "luis",
                            linked_at = as.POSIXct("2026-11-01", tz = "UTC")), .schema_partner_policies)
facturas <- function(docs, parte) tibble::tibble(Empresa = "NCS", Moneda = "MXN", Documento = docs, Parte = parte)
nombres <- unique(sub("^shared\\$", "", unlist(regmatches(
  readLines("R/settings/settings_companies.R", warn = FALSE),
  gregexpr("shared\\$[A-Za-z_.]+", readLines("R/settings/settings_companies.R", warn = FALSE))))))

# Runs `clics` with the session's copies `pm` (policy_moves) and `pp` (partner_policies).
correr <- function(pm, pp, sembrar, clics, sap = NULL) {
  s3_memoria_reset(); AVISOS <<- character()
  sembrar()
  shared <- lapply(setNames(nombres, nombres), function(n) reactiveVal(NULL))
  shared$effective_client_id <- function() CID
  shared$home_client_id      <- function() CID
  shared$current_user        <- function() "admin"
  shared$current_user_info   <- reactiveVal(list(user = "admin", username = "admin"))
  shared$usuario             <- reactiveVal(ADMIN)
  shared$policy_catalog_db(tibble::tibble(id = "p1", name = "Viernes"))
  shared$partner_policies_db(pp)
  shared$policy_moves_db(pm)
  # SAP data present from the start: the auto-apply observer (ignoreInit)
  # does not run, so Aplicar meets the session copy as given.
  if (!is.null(sap)) shared$sap_data(sap)
  testServer(function(input, output, session) settings_companies_observer(input, output, session, shared), {
    session$flushReact()
    clics(session)
  })
  invisible(shared)
}
s3 <- function(k) get(k, envir = .mem)

# ── 1. Revertir ───────────────────────────────────────────────────────────────
revertir <- function(s) s$setInputs(cmp_revert_partes_sel = "PROV A", cmp_revert_confirm = 1)
correr(NULL, socio("PROV A"), function() {
  assign(k_pm, dplyr::bind_rows(move("D1", "PROV A"), move("D2", "PROV B"), move("D3", "PROV C")), envir = .mem)
}, revertir)
ok("revert, NULL session copy: only PROV A's dates go; B and C (other sessions') stay",
   identical(sort(s3(k_pm)$Parte), c("PROV B", "PROV C")))
correr(move("D1", "PROV A"), socio("PROV A"), function() {
  assign(k_pm, dplyr::bind_rows(move("D1", "PROV A"), move("D2", "PROV B")), envir = .mem)
  .mem_falla <<- k_pm
}, revertir)
ok("revert, failed read: nothing written, the error is told",
   .mem_escrituras == 0L && any(grepl("Error al revertir", AVISOS)))
.mem_falla <- character()

# ── 2. Saving a partner's policies ───────────────────────────────────────────
guardar_socio <- function(s) { s$setInputs(cmp_partner_sel = "PROV A"); s$setInputs(cmp_btn_save = 1) }
correr(move("D1", "PROV A"), socio("PROV A"), function() {
  assign(k_pp, dplyr::bind_rows(socio("PROV A"), socio("PROV B")), envir = .mem)       # B from another session
  assign(k_pm, dplyr::bind_rows(move("D1", "PROV A"), move("D2", "PROV B")), envir = .mem)
}, guardar_socio)
ok("partner save: another session's partner PROV B stays in partner_policies",
   "PROV B" %in% s3(k_pp)$parte && "PROV A" %in% s3(k_pp)$parte)
ok("partner save: only PROV A's stale dates are cleared, PROV B's stay",
   identical(s3(k_pm)$Parte, "PROV B"))
correr(move("D1", "PROV A"), socio("PROV A"), function() {
  assign(k_pp, socio("PROV A"), envir = .mem)
  assign(k_pm, dplyr::bind_rows(move("D1", "PROV A"), move("D2", "PROV B")), envir = .mem)
}, function(s) { s$setInputs(cmp_partner_sel = "PROV A"); .mem_falla <<- k_pm; s$setInputs(cmp_btn_save = 1) })
ok("partner save, clearing the dates fails: told, not swallowed (policies themselves saved)",
   tr("comun.error_guardar") %in% AVISOS && "PROV A" %in% s3(k_pp)$parte)
.mem_falla <- character()

# ── 3. Aplicar ───────────────────────────────────────────────────────────────
aplicar <- function(scope) function(s) s$setInputs(cmp_apply_scope = scope, cmp_apply_confirm = 1)
sap <- list(AP = facturas(c("D1", "D4"), "PROV A"))
for (scope in c("new", "skip_manual", "all")) {
  correr(NULL, socio("PROV A"), function() {
    assign(k_pm, dplyr::bind_rows(move("D1", "PROV A"), move("D9", "PROV B")), envir = .mem)
    assign(k_mv, .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "D7",
                                           Parte = "PROV B"), .schema_moves), envir = .mem)
  }, aplicar(scope), sap = sap)
  pm <- s3(k_pm)
  ok(sprintf("apply '%s', empty session copy: PROV B's date (S3 only) stays", scope), "D9" %in% pm$Documento)
  ok(sprintf("apply '%s': D4 gets its policy date", scope), "D4" %in% pm$Documento)
}
ok("apply 'all': the manual moves of other documents are kept (pruned from S3's moves)",
   "D7" %in% s3(k_mv)$Documento)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
