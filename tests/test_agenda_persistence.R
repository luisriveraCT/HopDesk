# =============================================================================
# tests/test_agenda_persistence.R
# Verifies the Agenda de Hoy persistence fixes:
#   1. saldos_apertura per-user load/save round-trip
#   2. save_pagar_hoy() per-user: S3 write, .agenda_user_cache, version bump
#   3. sync_bus per_session_loader registration and dispatch
#   4. Sync deactivation: .agenda_user_cache population + per_session_loader serves it
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
# Also ensures pagar_hoy_db is registered in the sync bus (mirrors app startup).
.reset <- function(sync_on = FALSE) {
  .GlobalEnv$.agenda_sync      <- list(is_on = sync_on, data = NULL, version = "")
  .GlobalEnv$.agenda_user_cache <- list()
  # Reset sync version counters for pagar_hoy_db
  if (!is.list(.GlobalEnv$.sync_versions)) .GlobalEnv$.sync_versions <- list()
  .GlobalEnv$.sync_versions[["pagar_hoy_db"]] <- 0L
  # Ensure pagar_hoy_db is registered so bump_sync_version doesn't warn/bail
  register_synced(
    "pagar_hoy_db",
    S3_KEYS$pagar_hoy_sync,
    loader = function(client_id = NULL) {
      if (isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE)))
        .GlobalEnv$.agenda_sync$data
      else NULL
    },
    per_session_loader = function(shared) {
      sync_on <- isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE))
      if (sync_on) return(NULL)
      ukey <- tolower(trimws(tryCatch(shared$current_user(), error = function(e) "")))
      if (!nzchar(ukey)) return(NULL)
      cache <- tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)
      if (!is.list(cache)) return(NULL)
      cache[[ukey]]
    }
  )
  # Wipe mock S3 store and caches so state doesn't bleed across groups
  rm(list = ls(.mock_s3_store),    envir = .mock_s3_store)
  rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)
  rm(list = ls(.s3_missing_cache), envir = .s3_missing_cache)
}

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
# GROUP 2: save_pagar_hoy() per-user mode
# =============================================================================
cat("\n  [2] save_pagar_hoy per-user mode\n")
.reset(sync_on = FALSE)

ph <- .make_ph_row(id = "ph-001", doc = "FAC-2026-001")

# 2a: per-user write goes to correct S3 key
save_pagar_hoy(ph, username = "luis", client_id = "networks")
.chk(exists("networks/pagar_hoy_user_luis.rds", envir = .mock_s3_store), TRUE,
     "per-user save → pagar_hoy_user_luis.rds written")

# 2b: shared sync key NOT written in per-user mode
.chk(exists("networks/pagar_hoy_sync.rds", envir = .mock_s3_store), FALSE,
     "per-user save → pagar_hoy_sync.rds NOT written")

# 2c: .agenda_user_cache populated
.chk(is.list(.GlobalEnv$.agenda_user_cache), TRUE,
     "per-user save → .agenda_user_cache is a list")
.chk(!is.null(.GlobalEnv$.agenda_user_cache[["luis"]]), TRUE,
     "per-user save → cache entry for 'luis' exists")
cached <- .GlobalEnv$.agenda_user_cache[["luis"]]
.chk(nrow(cached), 1L,              "cached agenda → 1 row")
.chk(cached$Documento[1], "FAC-2026-001", "cached agenda → correct Documento")

# 2d: sync version bumped
ver_after <- .GlobalEnv$.sync_versions[["pagar_hoy_db"]] %||% 0L
.chk(ver_after >= 1L, TRUE, "per-user save → sync version bumped")

# 2e: second user's cache is independent
ph2 <- .make_ph_row(id = "ph-002", doc = "FAC-2026-002")
save_pagar_hoy(ph2, username = "claudia", client_id = "networks")
.chk(exists("networks/pagar_hoy_user_claudia.rds", envir = .mock_s3_store), TRUE,
     "second user → own S3 file written")
.chk(.GlobalEnv$.agenda_user_cache[["claudia"]]$Documento[1], "FAC-2026-002",
     "second user cache independent of first")
.chk(.GlobalEnv$.agenda_user_cache[["luis"]]$Documento[1], "FAC-2026-001",
     "first user cache unchanged after second user save")

# 2f: sync-ON mode → shared key written, per-user key NOT written
.reset(sync_on = TRUE)
save_pagar_hoy(ph, username = "luis", client_id = "networks")
.chk(exists("networks/pagar_hoy_sync.rds", envir = .mock_s3_store), TRUE,
     "sync-ON save → pagar_hoy_sync.rds written")
.chk(exists("networks/pagar_hoy_user_luis.rds", envir = .mock_s3_store), FALSE,
     "sync-ON save → per-user file NOT written")

# 2g: sync-ON updates in-memory .agenda_sync$data
.chk(!is.null(.GlobalEnv$.agenda_sync$data), TRUE,
     "sync-ON save → .agenda_sync$data updated in memory")

# =============================================================================
# GROUP 3: sync_bus per_session_loader
# =============================================================================
cat("\n  [3] sync_bus per_session_loader\n")
.reset(sync_on = FALSE)

# 3a: register_synced stores per_session_loader
global_called  <- FALSE
session_called <- FALSE

register_synced(
  "pagar_hoy_db",
  S3_KEYS$pagar_hoy_sync,
  loader = function(client_id = NULL) {
    global_called <<- TRUE
    NULL    # returns NULL in per-user mode → triggers per_session_loader
  },
  per_session_loader = function(shared) {
    session_called <<- TRUE
    tibble::tibble(id = "from-session-loader", ledger = "AP")
  }
)

.chk(is.function(.SYNC_REGISTRY[["pagar_hoy_db"]]$per_session_loader), TRUE,
     "register_synced → per_session_loader stored in registry")

# 3b: simulate the setup_sync_bus dispatch loop
entry <- .SYNC_REGISTRY[["pagar_hoy_db"]]
mock_shared <- list(current_user = function() "luis")

new_value <- tryCatch(entry$loader(), error = function(e) NULL)
.chk(isTRUE(global_called), TRUE, "dispatch loop → global loader called first")
.chk(is.null(new_value), TRUE,    "dispatch loop → global loader returned NULL")

if (is.null(new_value) && is.function(entry$per_session_loader)) {
  new_value <- tryCatch(entry$per_session_loader(mock_shared), error = function(e) NULL)
}
.chk(isTRUE(session_called), TRUE,  "dispatch loop → per_session_loader called when global returns NULL")
.chk(!is.null(new_value), TRUE,     "dispatch loop → per_session_loader return value used")
.chk(new_value$id[1], "from-session-loader",
     "dispatch loop → per_session_loader value is correct")

# 3c: per_session_loader returns user-specific data from .agenda_user_cache
.reset(sync_on = FALSE)
.GlobalEnv$.agenda_user_cache <- list(
  "luis"    = tibble::tibble(id = "luis-row",    ledger = "AP"),
  "claudia" = tibble::tibble(id = "claudia-row", ledger = "AR")
)

# Register the real per_session_loader from app.R
register_synced(
  "pagar_hoy_db",
  S3_KEYS$pagar_hoy_sync,
  loader = function(client_id = NULL) {
    if (isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE)))
      .GlobalEnv$.agenda_sync$data
    else
      NULL
  },
  per_session_loader = function(shared) {
    sync_on <- isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE))
    if (sync_on) return(NULL)
    ukey  <- tolower(trimws(tryCatch(shared$current_user(), error = function(e) "")))
    if (!nzchar(ukey)) return(NULL)
    cache <- tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)
    if (!is.list(cache)) return(NULL)
    cache[[ukey]]
  }
)

entry <- .SYNC_REGISTRY[["pagar_hoy_db"]]

local({
  shared_luis <- list(current_user = function() "luis")
  v <- entry$per_session_loader(shared_luis)
  .chk(v$id[1], "luis-row", "per_session_loader → returns luis's cache entry for luis session")
})
local({
  shared_claudia <- list(current_user = function() "claudia")
  v <- entry$per_session_loader(shared_claudia)
  .chk(v$id[1], "claudia-row", "per_session_loader → returns claudia's cache entry for claudia session")
})

# 3d: per_session_loader returns NULL when sync is ON (defers to global loader)
.GlobalEnv$.agenda_sync$is_on <- TRUE
local({
  shared_luis <- list(current_user = function() "luis")
  v <- entry$per_session_loader(shared_luis)
  .chk(is.null(v), TRUE, "per_session_loader → NULL when sync is ON (global loader handles it)")
})

# 3e: per_session_loader returns NULL for unknown user (cache miss)
.GlobalEnv$.agenda_sync$is_on <- FALSE
local({
  shared_unk <- list(current_user = function() "unknownuser")
  v <- entry$per_session_loader(shared_unk)
  .chk(is.null(v), TRUE, "per_session_loader → NULL for user not in cache (cache miss)")
})

# =============================================================================
# GROUP 4: sync deactivation populates .agenda_user_cache
# =============================================================================
cat("\n  [4] sync deactivation — .agenda_user_cache populated for all users\n")
.reset(sync_on = TRUE)

shared_data <- .make_ph_row(id = "shared-001", doc = "SHARED-DOC", importe = 999999)
.GlobalEnv$.agenda_sync$data <- shared_data

active_users <- c("luis", "claudia", "admin")

# Simulate the deactivation block from settings_sincro.R
if (!is.list(tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)))
  .GlobalEnv$.agenda_user_cache <- list()

for (u in active_users) {
  tryCatch(
    save_pagar_hoy_user(shared_data, u, client_id = "networks"),
    error = function(e) message("[SINCRO] copy failed for '", u, "': ", e$message)
  )
  .GlobalEnv$.agenda_user_cache[[tolower(trimws(u))]] <- shared_data
}

.GlobalEnv$.agenda_sync$is_on   <- FALSE
.GlobalEnv$.agenda_sync$data    <- NULL

# 4a: personal S3 files written for all users
for (u in active_users) {
  key <- paste0("networks/pagar_hoy_user_", tolower(u), ".rds")
  .chk(exists(key, envir = .mock_s3_store), TRUE,
       paste0("deactivation → personal S3 file written for '", u, "'"))
}

# 4b: .agenda_user_cache populated for all users
for (u in active_users) {
  .chk(!is.null(.GlobalEnv$.agenda_user_cache[[tolower(u)]]), TRUE,
       paste0("deactivation → .agenda_user_cache populated for '", u, "'"))
}

# 4c: cached data contains the shared snapshot
for (u in active_users) {
  d <- .GlobalEnv$.agenda_user_cache[[tolower(u)]]
  .chk(nrow(d), 1L, paste0("deactivation → cache for '", u, "' has 1 row"))
  .chk(d$Documento[1], "SHARED-DOC",
       paste0("deactivation → cache for '", u, "' has correct Documento"))
}

# 4d: per_session_loader serves personal copy after deactivation
for (u in active_users) {
  local({
    .u   <- tolower(u)
    sh   <- list(current_user = function() .u)
    val  <- entry$per_session_loader(sh)
    .chk(!is.null(val), TRUE,
         paste0("post-deactivation per_session_loader → non-NULL for '", .u, "'"))
    .chk(val$Documento[1], "SHARED-DOC",
         paste0("post-deactivation per_session_loader → correct data for '", .u, "'"))
  })
}

# 4e: after deactivation, a new per-user edit is isolated
.reset(sync_on = FALSE)  # fresh state
ph_luis_edit <- .make_ph_row(id = "edit-001", doc = "LUIS-ONLY", importe = 111)
save_pagar_hoy(ph_luis_edit, username = "luis", client_id = "networks")

# Claudia has no entry → per_session_loader returns NULL (correct, not luis's data)
local({
  sh <- list(current_user = function() "claudia")
  v  <- entry$per_session_loader(sh)
  .chk(is.null(v), TRUE,
       "post-deactivation per_session_loader → claudia gets NULL (not luis's edit)")
})

# Luis's session gets his own edit
local({
  sh <- list(current_user = function() "luis")
  v  <- entry$per_session_loader(sh)
  .chk(v$Documento[1], "LUIS-ONLY",
       "post-deactivation per_session_loader → luis gets his own edited data")
})

cat("\n")
