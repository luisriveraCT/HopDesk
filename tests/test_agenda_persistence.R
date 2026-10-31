# =============================================================================
# tests/test_agenda_persistence.R
# Verifies the Agenda de Hoy persistence fixes:
#   1. saldos_apertura per-user load/save round-trip
#   2. save_pagar_hoy() writes the right file for THAT client's sync setting
#   3. sync_bus per_session_loader reads S3 for the session's own client
#   4. Sync deactivation: every user gets a personal copy, read back from S3
# Since 2026-10-31 there is no process-global agenda state (.agenda_sync /
# .agenda_user_cache): the sync flag is per client, in agenda_sync_config.rds.
#
# Sourced by tests/_run_agenda.R — do not run directly.
# =============================================================================

cat("── Agenda de Hoy Persistence ───────────────────────────────────────────\n")

# ── assertion helper (follows project convention) ─────────────────────────────
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── state reset helper ────────────────────────────────────────────────────────
# Sync is a per-client setting in S3 now: .reset() writes networks' config.
.per_session_loader_app <- function(shared) {   # same body as app.R's
  cid  <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
  user <- tryCatch(shared$current_user(), error = function(e) "")
  if (is.null(cid) || !nzchar(cid)) return(NULL)
  safe_load_pagar_hoy(username = user, client_id = cid)
}
.reset <- function(sync_on = FALSE) {
  if (!is.list(.GlobalEnv$.sync_versions)) .GlobalEnv$.sync_versions <- list()
  .GlobalEnv$.sync_versions[["pagar_hoy_db"]] <- 0L
  register_synced("pagar_hoy_db", S3_KEYS$pagar_hoy_sync,
                  loader = function(client_id = NULL) NULL,
                  per_session_loader = .per_session_loader_app)
  rm(list = ls(.mock_s3_store),    envir = .mock_s3_store)
  rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)
  rm(list = ls(.s3_missing_cache), envir = .s3_missing_cache)
  save_agenda_sync_config(sync_on, "test", client_id = "networks")
}
.sesion <- function(user, cid = "networks")
  list(current_user = function() user, effective_client_id = function() cid)

# ── minimal pagar_hoy row builder ─────────────────────────────────────────────
.make_ph_row <- function(id = "row-001", doc = "DOC-001", emp = "Networks Group",
                          ledger = "AP", importe = 100000, status = "pending") {
  .schema_pagar_hoy() |> dplyr::bind_rows(tibble::tibble(
    id           = id,
    ledger       = ledger,
    Empresa      = emp,
    Moneda       = "MXN",
    Documento    = doc,
    Parte        = "PROVEEDOR TEST",
    Codigo       = NA_character_,
    tipo_item    = NA_character_,
    Importe      = as.numeric(importe),
    FechaVenc    = as.Date("2026-06-26"),
    staged_by    = "luis",
    staged_at    = as.POSIXct("2026-06-26 08:00:00"),
    status       = status,
    provision_id = NA_character_,
    liability_id = NA_character_,
    source       = "sap",
    confirmed_at = as.POSIXct(NA)
  ))
}

# =============================================================================
# GROUP 1: saldos_apertura persistence
# =============================================================================
cat("\n  [1] saldos_apertura persistence\n")
.reset()

sa_input <- list(
  "Networks Group"   = list(MXN = 5000000, USD = 200000),
  "Networks Trucking" = list(MXN = 1500000)
)

# 1a: save writes to expected S3 key
save_saldos_apertura_user(sa_input, "luis", client_id = "networks")
expected_key <- "networks/saldos_apertura_user_luis.rds"
.chk(exists(expected_key, envir = .mock_s3_store), TRUE,
     "save_saldos_apertura_user → correct S3 key")

# 1b: load round-trips all values
loaded <- load_saldos_apertura_user("luis", client_id = "networks")
.chk(is.list(loaded), TRUE, "load_saldos_apertura_user returns a list")
.chk(loaded[["Networks Group"]][["MXN"]], 5000000,
     "MXN balance survives S3 round-trip")
.chk(loaded[["Networks Group"]][["USD"]], 200000,
     "USD balance survives S3 round-trip")
.chk(loaded[["Networks Trucking"]][["MXN"]], 1500000,
     "second company MXN survives S3 round-trip")

# 1c: missing key returns empty list — not NULL, not an error
missing <- load_saldos_apertura_user("nobody", client_id = "networks")
.chk(is.list(missing), TRUE, "missing key → returns list()")
.chk(length(missing), 0L,   "missing key → returns empty list")

# 1d: non-list input rejected without throwing
result <- save_saldos_apertura_user("not_a_list", "luis", client_id = "networks")
.chk(isFALSE(result), TRUE, "non-list input → returns FALSE without error")

# 1e: updated values overwrite correctly (second save wins)
sa_updated <- list("Networks Group" = list(MXN = 7777777))
save_saldos_apertura_user(sa_updated, "luis", client_id = "networks")
reloaded <- load_saldos_apertura_user("luis", client_id = "networks")
.chk(reloaded[["Networks Group"]][["MXN"]], 7777777,
     "second save overwrites first — latest value wins")

# =============================================================================
# GROUP 2: save_pagar_hoy() follows the client's own sync setting
# =============================================================================
cat("
  [2] save_pagar_hoy per-user / shared mode
")
.reset(sync_on = FALSE)

ph <- .make_ph_row(id = "ph-001", doc = "FAC-2026-001")
save_pagar_hoy(ph, username = "luis", client_id = "networks")
.chk(exists("networks/pagar_hoy_user_luis.rds", envir = .mock_s3_store), TRUE,
     "sync OFF → pagar_hoy_user_luis.rds written")
.chk(exists("networks/pagar_hoy_sync.rds", envir = .mock_s3_store), FALSE,
     "sync OFF → pagar_hoy_sync.rds NOT written")
.chk((.GlobalEnv$.sync_versions[["pagar_hoy_db"]] %||% 0L) >= 1L, TRUE,
     "per-user save → sync version bumped")

ph2 <- .make_ph_row(id = "ph-002", doc = "FAC-2026-002")
save_pagar_hoy(ph2, username = "claudia", client_id = "networks")
.chk(load_pagar_hoy_user("claudia", client_id = "networks")$Documento[1], "FAC-2026-002",
     "second user → own file")
.chk(load_pagar_hoy_user("luis", client_id = "networks")$Documento[1], "FAC-2026-001",
     "first user's file unchanged after second user save")

.reset(sync_on = TRUE)
save_pagar_hoy(ph, username = "luis", client_id = "networks")
.chk(exists("networks/pagar_hoy_sync.rds", envir = .mock_s3_store), TRUE,
     "sync ON → pagar_hoy_sync.rds written")
.chk(exists("networks/pagar_hoy_user_luis.rds", envir = .mock_s3_store), FALSE,
     "sync ON → per-user file NOT written")
.chk(exists(".agenda_sync", envir = .GlobalEnv), FALSE,
     "no process-global agenda copy exists")

# Another client with sync off writes its own per-user file and does not
# switch networks' shared agenda off.
save_agenda_sync_config(FALSE, "test", client_id = "otro")
save_pagar_hoy(ph2, username = "luis", client_id = "otro")
.chk(exists("otro/pagar_hoy_user_luis.rds", envir = .mock_s3_store), TRUE,
     "client 'otro' (sync OFF) → its own per-user file")
.chk(agenda_sync_activa("networks"), TRUE, "networks' sync stays ON")

# =============================================================================
# GROUP 3: per_session_loader reads S3 for the session's own client
# =============================================================================
cat("
  [3] sync_bus per_session_loader
")
.reset(sync_on = FALSE)
entry <- .SYNC_REGISTRY[["pagar_hoy_db"]]
.chk(is.function(entry$per_session_loader), TRUE, "per_session_loader registered")
.chk(is.null(entry$loader()), TRUE, "global loader → NULL (no process-global copy)")

save_pagar_hoy(.make_ph_row(id = "luis-row", doc = "L"), "luis", client_id = "networks")
save_pagar_hoy(.make_ph_row(id = "claudia-row", doc = "C"), "claudia", client_id = "networks")
.chk(entry$per_session_loader(.sesion("luis"))$id[1], "luis-row", "luis → his own agenda")
.chk(entry$per_session_loader(.sesion("claudia"))$id[1], "claudia-row", "claudia → her own agenda")
.chk(nrow(entry$per_session_loader(.sesion("nadie"))), 0L, "user without a file → empty agenda")
.chk(is.null(entry$per_session_loader(list(current_user = function() "luis",
                                             effective_client_id = function() NULL))), TRUE,
     "no client → NULL (fails closed)")

save_agenda_sync_config(TRUE, "test", client_id = "networks")
save_pagar_hoy(.make_ph_row(id = "shared-row", doc = "S"), "luis", client_id = "networks")
.chk(entry$per_session_loader(.sesion("claudia"))$id[1], "shared-row",
     "sync ON → every session gets the shared agenda from S3")

# =============================================================================
# GROUP 4: sync deactivation gives every user a personal copy
# =============================================================================
cat("
  [4] sync deactivation — personal copies in S3
")
.reset(sync_on = TRUE)
shared_data <- .make_ph_row(id = "shared-001", doc = "SHARED-DOC", importe = 999999)
save_pagar_hoy_sync(shared_data, client_id = "networks")
active_users <- c("luis", "claudia", "admin")
# Same steps as settings_sincro.R's do_sincro_desactivar
current_data <- .pagar_hoy_vivo(TRUE, NULL, "networks")
for (u in active_users) save_pagar_hoy_user(current_data, u, client_id = "networks")
save_agenda_sync_config(FALSE, "test", client_id = "networks")

for (u in active_users) {
  .chk(exists(paste0("networks/pagar_hoy_user_", u, ".rds"), envir = .mock_s3_store), TRUE,
       paste0("deactivation → personal S3 file written for '", u, "'"))
  v <- entry$per_session_loader(.sesion(u))
  .chk(v$Documento[1], "SHARED-DOC", paste0("post-deactivation loader → '", u, "' gets the copy"))
}

save_pagar_hoy(.make_ph_row(id = "edit-001", doc = "LUIS-ONLY", importe = 111), "luis",
               client_id = "networks")
.chk(entry$per_session_loader(.sesion("luis"))$Documento[1], "LUIS-ONLY",
     "after deactivation luis gets his own edit")
.chk(entry$per_session_loader(.sesion("claudia"))$Documento[1], "SHARED-DOC",
     "claudia does not see luis's edit")

cat("
")
