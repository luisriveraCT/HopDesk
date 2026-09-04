# =============================================================================
# tests/test_payment_audit_stage.R
#
# Stage (2026-08-25): structured, permanent per-client audit trail of every
# payment file generated, a new can_view_payment_audit permission (default
# ON for dev/admin, OFF for finance/analysis, staff always allowed), and a
# new Activity switch-view button to browse it (CLABE masked in the summary,
# full detail + a PDF receipt one click away, no raw file bytes stored).
#
# Sourced by _run_saas.R. This file sources the new persistence + viewer
# modules itself (not part of the harness's default source list) since it's
# the only test file that currently needs them.
# =============================================================================

cat("── Payment-file audit trail (structured log + permission + viewer) ────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

source("R/pagos_generados_persistence.R", local = FALSE)
source("R/audit_log_viewer_module.R",     local = FALSE)  # .audit_viewer_resolve_perms()
source("R/pagos_audit_viewer_module.R",   local = FALSE)
source("R/tiers_module.R",                local = FALSE)  # PERM_LABELS, .PRINCIPAL_LOCKED_KEYS

# Sourcing the whole (huge) pagar_hoy_module.R just to reach the one function
# under test here (.log_pago_generado, module-scope, no dependency on the
# rest of the file's reactive machinery) -- this only DEFINES functions, it
# never executes pagarHoyServer's body, so none of that module's other
# dependencies (SAP data, proveedores catalog, etc.) need to be present.
source("R/pagar_hoy_module.R", local = FALSE)

# =============================================================================
# Part A -- persistence layer
# =============================================================================
cat("\n-- Part A: R/pagos_generados_persistence.R --\n")

sch <- .schema_pagos_generados()
expected_cols <- c("id","ts","user","client_id","empresa","moneda","cuenta_origen",
                    "banco_origen","n_pagos","total","beneficiarios","source","archivo",
                    "tipo","detalle")
.chk(names(sch), expected_cols, "schema has exactly the expected columns, in order")
.chk(nrow(sch), 0L, "schema starts with zero rows")

# ── mask_clabe() ────────────────────────────────────────────────────────────
.chk(mask_clabe("012345678901234567"), "••••4567",
     "mask_clabe: standard 18-digit CLABE masks to bullets + last 4 digits")
.chk(mask_clabe("1234"), "••••1234", "mask_clabe: exactly-4-char input still masks (bullets + itself)")
.chk(mask_clabe("123"), "", "mask_clabe: shorter than 4 chars returns empty (never a misleading partial mask)")
.chk(mask_clabe(""), "", "mask_clabe: empty string returns empty")
.chk(mask_clabe(NA), "", "mask_clabe: NA returns empty")
.chk(mask_clabe(NA_character_), "", "mask_clabe: NA_character_ returns empty")
.chk(mask_clabe(c("012345678901234567", "9876")),
     c("••••4567", "••••9876"),
     "mask_clabe: vectorized input masks each element independently")

# ── parse_pago_beneficiarios() ──────────────────────────────────────────────
empty_ben <- parse_pago_beneficiarios(NA_character_)
.chk(nrow(empty_ben), 0L, "parse_pago_beneficiarios: NA input returns zero rows")
.chk(names(empty_ben), c("alias","clabe","banco","importe","concepto"),
     "parse_pago_beneficiarios: empty result still has the right columns")
.chk(nrow(parse_pago_beneficiarios("")), 0L, "parse_pago_beneficiarios: empty string returns zero rows")
.chk(nrow(parse_pago_beneficiarios("not valid json{{{")), 0L,
     "parse_pago_beneficiarios: malformed JSON returns zero rows, does not error")

sample_ben <- list(
  list(alias = "ACME", clabe = "012345678901234567", banco = "030", importe = 1000.50, concepto = "FAC-1"),
  list(alias = "BETA", clabe = "098765432109876543", banco = "072", importe = 2500.00, concepto = "FAC-2")
)
ben_json <- jsonlite::toJSON(sample_ben, auto_unbox = TRUE)
parsed <- parse_pago_beneficiarios(ben_json)
.chk(nrow(parsed), 2L, "parse_pago_beneficiarios: round-trips 2 real entries")
.chk(parsed$alias, c("ACME", "BETA"), "parse_pago_beneficiarios: alias round-trips correctly")
.chk(parsed$importe, c(1000.50, 2500.00), "parse_pago_beneficiarios: importe round-trips as numeric")
.chk(parsed$clabe[1], "012345678901234567", "parse_pago_beneficiarios: full (unmasked) CLABE round-trips")

# ── add_pago_generado() + read_pagos_generados() ────────────────────────────
Sys.setenv(CLIENT_ID = "networks")
TEST_CID <- "pagos-test-client"

mk_record <- function(user = "tesoreria3", empresa = "Networks Group", moneda = "MXN",
                      n = 2L, total = 1500) {
  list(
    id = uuid::UUIDgenerate(), ts = Sys.time(), user = user, client_id = TEST_CID,
    empresa = empresa, moneda = moneda, cuenta_origen = "012180001234567890",
    banco_origen = NA_character_, n_pagos = n, total = total,
    beneficiarios = as.character(ben_json), source = "individual",
    archivo = "pagos_test_001.txt"
  )
}

r1 <- add_pago_generado(mk_record(), client_id = TEST_CID)
.chk(isTRUE(r1$ok), TRUE, "add_pago_generado: first append succeeds")
.chk(r1$attempts, 1L, "add_pago_generado: succeeds on the first attempt under normal conditions")

all1 <- read_pagos_generados(client_id = TEST_CID)
.chk(nrow(all1), 1L, "read_pagos_generados: one row visible after one append")
.chk(all1$user[1], "tesoreria3", "read_pagos_generados: the appended row's fields are readable back")

r2 <- add_pago_generado(mk_record(user = "larm3"), client_id = TEST_CID)
.chk(isTRUE(r2$ok), TRUE, "add_pago_generado: second append also succeeds")
all2 <- read_pagos_generados(client_id = TEST_CID)
.chk(nrow(all2), 2L, "read_pagos_generados: both appended rows are visible (append, not overwrite)")
.chk(setequal(all2$user, c("tesoreria3", "larm3")), TRUE,
     "read_pagos_generados: both distinct users' rows are present")
.chk(all2$ts[1] >= all2$ts[2], TRUE, "read_pagos_generados: newest-first ordering")

# Date-range filtering
past_record <- mk_record(user = "old_user")
past_record$ts <- Sys.time() - 100 * 86400
add_pago_generado(past_record, client_id = TEST_CID)
recent_only <- read_pagos_generados(client_id = TEST_CID, since = Sys.time() - 7 * 86400)
.chk("old_user" %in% recent_only$user, FALSE,
     "read_pagos_generados: since= correctly excludes an old row")
all3 <- read_pagos_generados(client_id = TEST_CID)
.chk("old_user" %in% all3$user, TRUE,
     "read_pagos_generados: without a since= filter, the old row is still there (not deleted)")

# Client isolation
other_cid <- "pagos-test-client-other"
add_pago_generado(mk_record(user = "other_client_user"), client_id = other_cid)
mine <- read_pagos_generados(client_id = TEST_CID)
.chk("other_client_user" %in% mine$user, FALSE,
     "read_pagos_generados: a different client's rows never leak into this client's read")

# Empty/missing client
empty_client <- read_pagos_generados(client_id = "pagos-test-client-never-used")
.chk(nrow(empty_client), 0L, "read_pagos_generados: a client with no data returns zero rows, not an error")

# ── Retry behavior: temporarily force .pagos_write_chunk to fail twice, then
# succeed, proving the retry loop actually retries (mirrors
# save_pagar_hoy_with_retry()'s own pattern) ────────────────────────────────
.real_pagos_write_chunk <- .pagos_write_chunk
.fail_count <- 0L
.pagos_write_chunk <<- function(df, key) {
  .fail_count <<- .fail_count + 1L
  if (.fail_count <= 2L) stop("simulated transient S3 failure")
  .real_pagos_write_chunk(df, key)
}
r_retry <- add_pago_generado(mk_record(user = "retry_user"), client_id = TEST_CID,
                             max_attempts = 5, retry_delay = 0.01)
.chk(isTRUE(r_retry$ok), TRUE, "add_pago_generado: succeeds after 2 simulated transient failures")
.chk(r_retry$attempts, 3L, "add_pago_generado: took exactly 3 attempts (2 failures + 1 success)")
.pagos_write_chunk <<- .real_pagos_write_chunk  # restore immediately, regardless of outcome

.fail_count_permanent <- 0L
.pagos_write_chunk <<- function(df, key) { stop("simulated permanent S3 failure") }
r_fail <- add_pago_generado(mk_record(user = "always_fails"), client_id = TEST_CID,
                            max_attempts = 3, retry_delay = 0.01)
.chk(isTRUE(r_fail$ok), FALSE, "add_pago_generado: reports failure (not silently ok) when every attempt fails")
.chk(r_fail$attempts, 3L, "add_pago_generado: gives up after exactly max_attempts tries")
.pagos_write_chunk <<- .real_pagos_write_chunk  # restore

.chk("always_fails" %in% read_pagos_generados(client_id = TEST_CID)$user, FALSE,
     "add_pago_generado: a fully-failed write leaves no partial/corrupt row behind")

# ── Rotation ─────────────────────────────────────────────────────────────────
# Directly exercise rotate_pagos_if_needed() with a temporarily lowered
# threshold rather than inserting 10,000 real rows one at a time.
.real_max <- .MAX_PAGOS_ROWS
.MAX_PAGOS_ROWS <<- 3L

rot_cid <- "pagos-test-rotation"
big_df <- dplyr::bind_rows(lapply(1:3, function(i) mk_record(user = paste0("rot_user_", i))))
big_df <- tibble::as_tibble(big_df)
rotated <- rotate_pagos_if_needed(rot_cid, big_df)
.chk(isTRUE(rotated), TRUE, "rotate_pagos_if_needed: rotates when row count reaches the (lowered) threshold")

archive_hit <- tryCatch(
  aws.s3::s3readRDS(object = paste0(rot_cid, "/pagos_generados_001.rds"), bucket = "mock-bucket"),
  error = function(e) NULL
)
.chk(is.null(archive_hit), FALSE, "rotate_pagos_if_needed: writes the archive chunk (pagos_generados_001.rds)")
if (!is.null(archive_hit)) .chk(nrow(archive_hit), 3L, "rotate_pagos_if_needed: archive chunk has all 3 original rows")

current_after_rotate <- tryCatch(
  aws.s3::s3readRDS(object = paste0(rot_cid, "/pagos_generados.rds"), bucket = "mock-bucket"),
  error = function(e) NULL
)
.chk(is.null(current_after_rotate), FALSE, "rotate_pagos_if_needed: current key still exists after rotation")
if (!is.null(current_after_rotate))
  .chk(nrow(current_after_rotate), 0L, "rotate_pagos_if_needed: current chunk is reset to empty after rotation")

not_rotated <- rotate_pagos_if_needed(rot_cid, big_df[1:2, ])
.chk(isTRUE(not_rotated), FALSE, "rotate_pagos_if_needed: does NOT rotate when below the threshold")

.MAX_PAGOS_ROWS <<- .real_max  # restore immediately

# =============================================================================
# Part A2 -- .log_pago_generado() itself, end to end, with realistic data
#
# CRITICAL regression coverage: the original version of this function passed
# jsonlite::toJSON()'s return value (class "json", not plain "character")
# straight into the beneficiarios field. dplyr::bind_rows() cannot reconcile
# a "json"-classed column against the schema's plain-character column, so
# EVERY SINGLE CALL silently failed after 3 retries -- caught live 2026-08-26
# when no payment file ever produced a log entry at all, for either
# generation path. Part F below only static-scans that the call sites exist;
# it would NOT have caught this, because the bug was inside the shared
# helper both call sites correctly invoke. This is why the actual function
# is exercised here with real data, not just grepped for.
# =============================================================================
cat("\n-- Part A2: .log_pago_generado() end-to-end with realistic ppl_rows --\n")

realistic_ppl_rows <- tibble::tibble(
  alias      = c("ACME SA DE CV", "BETA LOGISTICS"),
  clabe_dest = c("012345678901234567", "098765432109876543"),
  banco_dest = c("030", "072"),
  medio_pago = c("SPI", "SPI"),
  importe    = c(1000.50, 2500.00),
  concepto   = c("FAC-1", "FAC-2")
)
fake_shared_a2 <- list(
  effective_client_id = function() "pagos-test-e2e",
  current_user         = function() "tesoreria_e2e"
)

e2e_error <- tryCatch({
  .log_pago_generado(fake_shared_a2, "Networks Group", "MXN", "012180001234567890",
                     realistic_ppl_rows, fname = "pagos_e2e_001.txt", source_type = "individual")
  NULL
}, error = function(e) e)
.chk(is.null(e2e_error), TRUE,
     "log_pago_generado: does not throw when called with realistic data")

e2e_rows <- read_pagos_generados(client_id = "pagos-test-e2e")
.chk(nrow(e2e_rows), 1L,
     "log_pago_generado: a record actually lands in pagos_generados (THE regression this section exists for)")
if (nrow(e2e_rows)) {
  .chk(e2e_rows$user[1], "tesoreria_e2e", "log_pago_generado: user resolved from shared$current_user()")
  .chk(e2e_rows$client_id[1], "pagos-test-e2e", "log_pago_generado: client_id resolved from shared$effective_client_id()")
  .chk(e2e_rows$empresa[1], "Networks Group", "log_pago_generado: empresa recorded correctly")
  .chk(e2e_rows$moneda[1], "MXN", "log_pago_generado: moneda recorded correctly")
  .chk(e2e_rows$n_pagos[1], 2L, "log_pago_generado: n_pagos matches the number of ppl_rows")
  .chk(e2e_rows$total[1], 3500.50, "log_pago_generado: total is the correct sum of importe")
  .chk(e2e_rows$source[1], "individual", "log_pago_generado: source_type is recorded correctly")
  .chk(is.character(e2e_rows$beneficiarios[1]) && !inherits(e2e_rows$beneficiarios[1], "json"),
       TRUE, "log_pago_generado: beneficiarios is stored as plain character, not a 'json'-classed object")
  e2e_ben <- parse_pago_beneficiarios(e2e_rows$beneficiarios[1])
  .chk(nrow(e2e_ben), 2L, "log_pago_generado: both beneficiaries round-trip out of the stored JSON")
  .chk(e2e_ben$alias, c("ACME SA DE CV", "BETA LOGISTICS"),
       "log_pago_generado: beneficiary aliases round-trip correctly")
  .chk(e2e_ben$clabe[1], "012345678901234567",
       "log_pago_generado: full (unmasked) CLABE round-trips correctly in storage")
}

# Zero-row ppl_rows must be a silent no-op, not an error and not an empty record.
no_op_error <- tryCatch({
  .log_pago_generado(fake_shared_a2, "Networks Group", "MXN", "012180001234567890",
                     tibble::tibble(alias=character(), clabe_dest=character(), banco_dest=character(),
                                    medio_pago=character(), importe=double(), concepto=character()),
                     fname = "empty.txt", source_type = "individual")
  NULL
}, error = function(e) e)
.chk(is.null(no_op_error), TRUE, "log_pago_generado: zero-row ppl_rows does not throw")
.chk(nrow(read_pagos_generados(client_id = "pagos-test-e2e")), 1L,
     "log_pago_generado: zero-row ppl_rows does not create a spurious record (still just the 1 from above)")

# NULL shared accessors (a malformed caller context) must degrade gracefully,
# not silently lose the record's client attribution.
fake_shared_a2_broken <- list(
  effective_client_id = function() stop("no client context"),
  current_user         = function() stop("no user context")
)
Sys.setenv(CLIENT_ID = "pagos-test-e2e-fallback")
broken_error <- tryCatch({
  .log_pago_generado(fake_shared_a2_broken, "Networks Group", "MXN", "0121800012",
                     realistic_ppl_rows, fname = "fallback.txt", source_type = "zip_bulk")
  NULL
}, error = function(e) e)
.chk(is.null(broken_error), TRUE,
     "log_pago_generado: a shared accessor that throws does not propagate/crash the caller")
fallback_rows <- read_pagos_generados(client_id = "pagos-test-e2e-fallback")
.chk(nrow(fallback_rows), 1L,
     "log_pago_generado: falls back to Sys.getenv('CLIENT_ID') when shared$effective_client_id() throws")
if (nrow(fallback_rows))
  .chk(fallback_rows$user[1], "unknown",
       "log_pago_generado: falls back to 'unknown' user when shared$current_user() throws")
Sys.unsetenv("CLIENT_ID")
Sys.setenv(CLIENT_ID = "networks")

# =============================================================================
# Part B -- permission wiring (auth_resolve_perms + PERM_LABELS)
# =============================================================================
cat("\n-- Part B: can_view_payment_audit tier defaults --\n")

expected_defaults <- c(principal = TRUE, hopdesk = TRUE, dev = TRUE,
                       admin = TRUE, finance = FALSE, analysis = FALSE)
for (tier in names(expected_defaults)) {
  got <- auth_resolve_perms(tier, "{}")$can_view_payment_audit
  .chk(isTRUE(got), isTRUE(expected_defaults[[tier]]),
       sprintf("auth_resolve_perms('%s')$can_view_payment_audit defaults to %s",
               tier, expected_defaults[[tier]]))
}

.chk("can_view_payment_audit" %in% names(PERM_LABELS), TRUE,
     "can_view_payment_audit is registered in PERM_LABELS (so Config. de Tiers renders a toggle for it)")
.chk(nzchar(PERM_LABELS[["can_view_payment_audit"]] %||% ""), TRUE,
     "can_view_payment_audit has a non-empty display label")
.chk("can_view_payment_audit" %in% .PRINCIPAL_LOCKED_KEYS, FALSE,
     "can_view_payment_audit is NOT a principal-locked key -- it's a regular, toggleable permission")

# Exhaustive consistency check: every key in PERM_LABELS must exist in
# EVERY tier's auth_resolve_perms() defaults -- catches "added the label but
# forgot a tier's default" the same way Stage 14c's exhaustive scan catches
# a missed staging site.
for (tier in c("principal","hopdesk","dev","admin","finance","analysis")) {
  resolved <- auth_resolve_perms(tier, "{}")
  missing <- setdiff(names(PERM_LABELS), names(resolved))
  .chk(length(missing), 0L,
       sprintf("tier '%s': every PERM_LABELS key has a default (missing: %s)",
               tier, if (length(missing)) paste(missing, collapse=", ") else "none"))
}

# Override mechanism applies correctly to the new key too (finance defaults
# OFF; an explicit override can still turn it on for one user).
ov_on  <- auth_resolve_perms("finance", '{"can_view_payment_audit": true}')
ov_off <- auth_resolve_perms("dev", '{"can_view_payment_audit": false}')
.chk(isTRUE(ov_on$can_view_payment_audit), TRUE,
     "auth_resolve_perms: a finance user's explicit override can turn can_view_payment_audit ON")
.chk(isTRUE(ov_off$can_view_payment_audit), FALSE,
     "auth_resolve_perms: a dev user's explicit override can turn can_view_payment_audit OFF")

# =============================================================================
# Part C -- .payment_audit_can_view() staff bypass
# =============================================================================
cat("\n-- Part C: .payment_audit_can_view() staff bypass --\n")

.mk_shared <- function(user, tier, is_staff, permisos_json = "{}", client_id = "networks") {
  list(
    current_user_info = function() list(user = user, tier = tier, client_id = client_id,
                                        is_staff = is_staff),
    home_client_id      = function() client_id,
    effective_client_id = function() client_id
  )
}

.seed_usuario_pa <- function(cid, username, tier, permisos_json = "{}") {
  existing <- tryCatch(mock_s3readRDS(paste0(cid, "/usuarios.rds"), "mock-bucket"),
                       error = function(e) NULL)
  row <- data.frame(username = username, tier = tier, permisos = permisos_json,
                    stringsAsFactors = FALSE)
  df <- if (is.null(existing)) row else {
    existing <- existing[tolower(existing$username) != tolower(username), , drop = FALSE]
    rbind(existing[, names(row), drop = FALSE], row)
  }
  mock_s3saveRDS(df, paste0(cid, "/usuarios.rds"), "mock-bucket")
}

.seed_usuario_pa("networks", "finance_user_pa", "finance")
.seed_usuario_pa("networks", "dev_user_pa", "dev")
.seed_usuario_pa("networks", "finance_override_pa", "finance", '{"can_view_payment_audit": true}')

.chk(.payment_audit_can_view(.mk_shared("bunny3", "hopdesk", TRUE)), TRUE,
     "staff (is_staff=TRUE) always passes, regardless of tier")
.chk(.payment_audit_can_view(.mk_shared("mouse3", "principal", TRUE)), TRUE,
     "principal staff always passes")
.chk(.payment_audit_can_view(.mk_shared("finance_user_pa", "finance", FALSE)), FALSE,
     "non-staff finance-tier user with no override is refused")
.chk(.payment_audit_can_view(.mk_shared("dev_user_pa", "dev", FALSE)), TRUE,
     "non-staff dev-tier user is allowed (tier default)")
.chk(.payment_audit_can_view(.mk_shared("finance_override_pa", "finance", FALSE)), TRUE,
     "non-staff finance-tier user WITH an explicit override is allowed")

# =============================================================================
# Part D -- pagosAuditViewerServer via testServer()
# =============================================================================
cat("\n-- Part D: pagosAuditViewerServer functional coverage --\n")

mock_s3saveRDS(.schema_pagos_generados(), "networks/pagos_generados.rds", "mock-bucket")
add_pago_generado(mk_record(user = "tesoreria3", empresa = "Networks Group"), client_id = "networks")
add_pago_generado(mk_record(user = "larm3", empresa = "Networks Realtors"), client_id = "networks")

shiny::testServer(pagosAuditViewerServer, args = list(
  shared = .mk_shared("finance_user_pa", "finance", FALSE)
), {
  session$flushReact()
  .chk(can_view(), FALSE, "D1: non-staff finance viewer's can_view() is FALSE")
})

shiny::testServer(pagosAuditViewerServer, args = list(
  shared = .mk_shared("dev_user_pa", "dev", FALSE)
), {
  session$flushReact()
  .chk(can_view(), TRUE, "D2: non-staff dev viewer's can_view() is TRUE")

  df <- fetched()
  .chk(is.data.frame(df) && nrow(df) >= 2L, TRUE, "D2: dev viewer's fetched() returns the seeded rows")

  session$setInputs(pagos_emp_sel = "Networks Group")
  session$flushReact()
  f <- filtered()
  .chk(nrow(f) >= 1L && all(f$empresa == "Networks Group"), TRUE,
       "D2: Empresa filter narrows correctly")

  session$setInputs(pagos_emp_sel = "Todas", pagos_user_filter = "larm3")
  session$flushReact()
  f2 <- filtered()
  .chk(nrow(f2) == 1L && f2$user[1] == "larm3", TRUE,
       "D2: Usuario filter narrows correctly")

  session$setInputs(pagos_emp_sel = "Todas", pagos_user_filter = "")
  session$flushReact()
  .chk(nrow(filtered()) >= 2L, TRUE, "D2: clearing filters restores all rows")

  # Selecting a row and clicking "Ver detalle" populates selected_record()
  session$setInputs(pagos_tbl_rows_selected = 1)
  session$flushReact()
  session$setInputs(btn_ver_detalle = 1)
  session$flushReact()
  rec <- selected_record()
  .chk(is.data.frame(rec) && nrow(rec) == 1L, TRUE,
       "D2: selecting a row + clicking Ver detalle populates selected_record()")
})

shiny::testServer(pagosAuditViewerServer, args = list(
  shared = .mk_shared("bunny3", "hopdesk", TRUE)
), {
  session$flushReact()
  .chk(can_view(), TRUE, "D3: staff viewer's can_view() is TRUE even on a client's own effective_client_id")
  df <- fetched()
  .chk(is.data.frame(df) && nrow(df) >= 2L, TRUE, "D3: staff viewer sees the same client rows")
})

# =============================================================================
# Part E -- receipt HTML builder (.build_pago_receipt_html)
# =============================================================================
cat("\n-- Part E: .build_pago_receipt_html() --\n")

one_row <- read_pagos_generados(client_id = "networks")[1, , drop = FALSE]
html <- .build_pago_receipt_html(one_row)
.chk(grepl("<html", html, fixed = TRUE), TRUE, "receipt HTML: produces a well-formed HTML document")
.chk(grepl(htmltools::htmlEscape(one_row$user[1]), html, fixed = TRUE), TRUE,
     "receipt HTML: includes the generating user's name")
.chk(grepl(htmltools::htmlEscape(one_row$empresa[1]), html, fixed = TRUE), TRUE,
     "receipt HTML: includes the empresa")
ben_in_row <- parse_pago_beneficiarios(one_row$beneficiarios[1])
if (nrow(ben_in_row))
  .chk(grepl(htmltools::htmlEscape(ben_in_row$clabe[1]), html, fixed = TRUE), TRUE,
       "receipt HTML: includes the FULL (unmasked) CLABE -- correct for the already-permission-gated download path")
.chk(grepl("Total:", html, fixed = TRUE), TRUE, "receipt HTML: includes a total line")

# HTML-injection safety: a malicious alias/concepto must not break out of
# its table cell.
malicious <- one_row
malicious$beneficiarios <- jsonlite::toJSON(list(list(
  alias = "<script>alert(1)</script>", clabe = "0000", banco = "X",
  importe = 1, concepto = "\"><img src=x>"
)), auto_unbox = TRUE)
html2 <- .build_pago_receipt_html(malicious)
.chk(grepl("<script>alert", html2, fixed = TRUE), FALSE,
     "receipt HTML: a malicious alias is escaped, not injected as raw HTML")
.chk(grepl("<img src=x>", html2, fixed = TRUE), FALSE,
     "receipt HTML: a malicious concepto is escaped, not injected as raw HTML")

# =============================================================================
# Part F -- static scan: logging hooks + UI wiring exist in the real source
# =============================================================================
cat("\n-- Part F: static scan of hook/wiring sites --\n")

ph_txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/pagar_hoy_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/pagar_hoy_module.R still parses without syntax errors")

.chk(length(grep("^\\.log_pago_generado <- function", ph_txt)), 1L,
     ".log_pago_generado() is defined exactly once, at module scope")

# The definition line uses " <- function(", not a literal call shape, so
# this pattern only matches the two actual call sites (not the definition).
call_hits <- grep('^\\s*\\.log_pago_generado\\(', ph_txt)
.chk(length(call_hits), 2L,
     "exactly 2 call sites invoke .log_pago_generado() (individual handler + ZIP bulk handler)")

.chk(length(grep('source_type = "individual"', ph_txt)), 1L,
     "the individual PPL download handler logs with source_type=\"individual\"")
.chk(length(grep('source_type = "zip_bulk"', ph_txt)), 1L,
     "the ZIP bulk download handler logs with source_type=\"zip_bulk\"")

tm_txt <- readLines("R/tiers_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/tiers_module.R still parses without syntax errors")
.chk(length(grep('pagosAuditViewerServer\\("pagos_viewer"', tm_txt)), 1L,
     "pagosAuditViewerServer is wired into tiers_module.R's server")
.chk(length(grep('pagosAuditViewerUI\\(ns\\("pagos_viewer"\\)\\)', tm_txt)), 1L,
     "pagosAuditViewerUI is wired into the activity_section render")
# Generalized 2026-09-03 from a binary activity_switch_view toggle into the
# data-driven .ACTIVITY_VIEWS registry (see tests/test_activity_multiview_selector.R
# for the dedicated Stage 1 coverage of that registry's own behavior) --
# these two checks now confirm the "pagos" view is REGISTERED (a list entry
# with the right id/can_view), not that a specific hardcoded input id exists.
.chk(length(grep('id = "pagos"', tm_txt)), 1L,
     "the \"pagos\" view is registered in .ACTIVITY_VIEWS")
.chk(length(grep("can_view = \\.payment_audit_can_view", tm_txt)), 1L,
     "the \"pagos\" view's visibility is gated by .payment_audit_can_view (via the registry, not a standalone if-check)")

app_txt_pa <- readLines("app.R", warn = FALSE)
.chk(length(grep('source\\("R/pagos_audit_viewer_module.R"\\)', app_txt_pa)), 1L,
     "app.R sources R/pagos_audit_viewer_module.R")
.chk_true({
  audit_ln <- grep('source\\("R/audit_log_viewer_module.R"\\)', app_txt_pa)
  pagos_ln <- grep('source\\("R/pagos_audit_viewer_module.R"\\)', app_txt_pa)
  tiers_ln <- grep('source\\("R/tiers_module.R"\\)', app_txt_pa)
  length(audit_ln) && length(pagos_ln) && length(tiers_ln) &&
    audit_ln[1] < pagos_ln[1] && pagos_ln[1] < tiers_ln[1]
}, "sourcing order is correct: audit_log_viewer_module.R, then pagos_audit_viewer_module.R, then tiers_module.R (which calls both)")

global_txt <- readLines("R/global.R", warn = FALSE)
.chk(length(grep('source\\("R/pagos_generados_persistence.R"\\)', global_txt)), 1L,
     "global.R sources R/pagos_generados_persistence.R")

# ── DT rebuild-avoidance in pagos_audit_viewer_module.R (same fix class as
# Part F of test_activity_live_refresh_toggle.R) ────────────────────────────
pav_txt <- readLines("R/pagos_audit_viewer_module.R", warn = FALSE)
.chk(length(grep("isolate\\(\\.pagos_display_df\\(filtered\\(\\)\\)\\)", pav_txt)), 1L,
     "output$pagos_tbl's initial render is isolate()-wrapped (renders once, not on every filtered() change)")
.chk(length(grep('pagos_tbl_proxy <- DT::dataTableProxy\\("pagos_tbl"\\)', pav_txt)), 1L,
     "pagos_tbl_proxy (DT::dataTableProxy) is defined")
.chk(length(grep('DT::replaceData\\(pagos_tbl_proxy, \\.pagos_display_df\\(filtered\\(\\)\\)', pav_txt)), 1L,
     "DT::replaceData() is called to push data updates without a full rebuild")
.chk(length(grep('clearSelection = "none"', pav_txt)), 1L,
     "replaceData() preserves the user's row selection (clearSelection=\"none\") across data refreshes")

# =============================================================================
# Part G -- generic download/export logging (2026-08-26 extension: "every
# time I download anything... it needs to appear here" -- Word/Excel/PDF
# cash flow exports, not just PPL payment files)
# =============================================================================
cat("\n-- Part G: log_archivo_generado() + tipo/detalle handling --\n")

source("R/cashflow_export_module.R", local = FALSE)

# ── .tipo_label() ───────────────────────────────────────────────────────────
.chk(.tipo_label("pago_ppl"), "Pago", ".tipo_label: pago_ppl maps to 'Pago'")
.chk(.tipo_label("reporte_word"), "Reporte Word", ".tipo_label: reporte_word maps correctly")
.chk(.tipo_label("reporte_excel"), "Reporte Excel", ".tipo_label: reporte_excel maps correctly")
.chk(.tipo_label("reporte_pdf"), "Reporte PDF", ".tipo_label: reporte_pdf maps correctly")
.chk(.tipo_label("algo_futuro"), "algo_futuro",
     ".tipo_label: an unknown future tipo passes through as-is (not silently blanked)")
.chk(.tipo_label(NA_character_), "", ".tipo_label: NA tipo returns empty string")
.chk(.tipo_label(""), "", ".tipo_label: empty-string tipo returns empty string")

# ── parse_pago_detalle() ────────────────────────────────────────────────────
.chk(length(parse_pago_detalle(NA_character_)), 0L, "parse_pago_detalle: NA returns an empty list")
.chk(length(parse_pago_detalle("")), 0L, "parse_pago_detalle: empty string returns an empty list")
.chk(length(parse_pago_detalle("not json{{{")), 0L, "parse_pago_detalle: malformed JSON returns an empty list, does not error")

detalle_json <- jsonlite::toJSON(list(desde = "2026-08-01", hasta = "2026-08-31",
                                      intercompania = "exclude", empresas = c("Networks Group", "Networks Realtors")),
                                 auto_unbox = TRUE)
parsed_detalle <- parse_pago_detalle(as.character(detalle_json))
.chk(parsed_detalle$desde, "2026-08-01", "parse_pago_detalle: scalar field round-trips correctly")
.chk(parsed_detalle$intercompania, "exclude", "parse_pago_detalle: another scalar field round-trips correctly")
.chk(length(parsed_detalle$empresas), 2L, "parse_pago_detalle: a multi-value field round-trips with the right length")

# ── log_archivo_generado() end to end ───────────────────────────────────────
fake_shared_g <- list(
  effective_client_id = function() "pagos-test-export",
  current_user         = function() "dev_export_test"
)

g_error <- tryCatch({
  log_archivo_generado(
    fake_shared_g, tipo = "reporte_word", empresa = "Networks Group, Networks Realtors",
    moneda = "MXN",
    detalle_list = list(desde = "2026-08-01", hasta = "2026-08-31", agrupacion = "semanal"),
    n_items = 2L, fname = "cashflow_26082026.docx"
  )
  NULL
}, error = function(e) e)
.chk(is.null(g_error), TRUE, "log_archivo_generado: does not throw for a Word-export-shaped call")

g_rows <- read_pagos_generados(client_id = "pagos-test-export")
.chk(nrow(g_rows), 1L, "log_archivo_generado: a record actually lands in pagos_generados")
if (nrow(g_rows)) {
  .chk(g_rows$tipo[1], "reporte_word", "log_archivo_generado: tipo recorded correctly")
  .chk(g_rows$source[1], "reporte_word", "log_archivo_generado: source mirrors tipo for non-payment rows")
  .chk(g_rows$empresa[1], "Networks Group, Networks Realtors", "log_archivo_generado: empresa recorded correctly")
  .chk(g_rows$moneda[1], "MXN", "log_archivo_generado: moneda recorded correctly")
  .chk(g_rows$n_pagos[1], 2L, "log_archivo_generado: n_items lands in the n_pagos column")
  .chk(is.na(g_rows$total[1]), TRUE, "log_archivo_generado: total is NA when not supplied")
  .chk(is.na(g_rows$beneficiarios[1]), TRUE, "log_archivo_generado: beneficiarios is NA for a non-payment row")
  .chk(is.na(g_rows$cuenta_origen[1]), TRUE, "log_archivo_generado: cuenta_origen is NA for a non-payment row")
  .chk(is.character(g_rows$detalle[1]) && !inherits(g_rows$detalle[1], "json"), TRUE,
       "log_archivo_generado: detalle is stored as plain character, not a 'json'-classed object (same class of bug as .log_pago_generado's)")
  g_det <- parse_pago_detalle(g_rows$detalle[1])
  .chk(g_det$agrupacion, "semanal", "log_archivo_generado: detalle round-trips correctly")
}

# A second call with a numeric total (the PDF/Excel shape).
log_archivo_generado(fake_shared_g, tipo = "reporte_pdf", empresa = "Networks Group",
                     moneda = "USD", detalle_list = list(desde = "2026-08-01"),
                     total = 15000.75, n_items = 1L, fname = "cashflow.pdf")
g_rows2 <- read_pagos_generados(client_id = "pagos-test-export")
.chk(nrow(g_rows2), 2L, "log_archivo_generado: a second, different-tipo record also lands correctly")
pdf_row <- g_rows2[g_rows2$tipo == "reporte_pdf", , drop = FALSE]
.chk(nrow(pdf_row) == 1L && pdf_row$total[1] == 15000.75, TRUE,
     "log_archivo_generado: an explicit total is recorded correctly")

# ── Receipt HTML branches correctly for a non-payment row ───────────────────
cat("\n-- Part G2: receipt HTML for a non-payment (reporte_word) row --\n")
word_row <- g_rows[1, , drop = FALSE]
word_html <- .build_pago_receipt_html(word_row)
.chk(grepl("Recibo de archivo generado", word_html, fixed = TRUE), TRUE,
     "receipt HTML: non-payment rows get the generic title, not 'Recibo de archivo de pagos'")
.chk(grepl("agrupacion", word_html, fixed = TRUE), TRUE,
     "receipt HTML: detalle keys appear in the non-payment receipt")
.chk(grepl("semanal", word_html, fixed = TRUE), TRUE,
     "receipt HTML: detalle values appear in the non-payment receipt")
.chk(grepl("Cuenta origen", word_html, fixed = TRUE), FALSE,
     "receipt HTML: a non-payment receipt never shows a (meaningless) Cuenta origen line")

# ── Static scan: the 3 export handlers actually call log_archivo_generado() ─
cat("\n-- Part G3: static scan of export handler hooks --\n")
ce_txt <- readLines("R/cashflow_export_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/cashflow_export_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/cashflow_export_module.R still parses without syntax errors")
.chk(length(grep('tipo = "reporte_word"', ce_txt)), 1L, "btn_export_word logs with tipo=\"reporte_word\"")
.chk(length(grep('tipo = "reporte_excel"', ce_txt)), 1L, "btn_download_excel_ready logs with tipo=\"reporte_excel\"")
.chk(length(grep('tipo = "reporte_pdf"', ce_txt)), 1L, "btn_export_pdf logs with tipo=\"reporte_pdf\"")
.chk(length(grep("\\.xl_log_params <- reactiveVal\\(NULL\\)", ce_txt)), 1L,
     ".xl_log_params reactiveVal exists to capture export parameters at job-trigger time")
.chk(length(grep("\\.xl_log_params\\(list\\(", ce_txt)), 1L,
     ".xl_log_params is populated when the async Excel job is triggered")
.chk(length(grep("params <- \\.xl_log_params\\(\\) %\\|\\|% list\\(\\)", ce_txt)), 1L,
     "the Excel download handler reads .xl_log_params() (job-trigger-time params, not stale/re-read UI state)")

# ── Part G4: R/bancos_module.R's Libro de Bancos Excel export got the same
# treatment (still true). R/reporte_module.R's HTML/PDF financial report
# used to as well, but was DELIBERATELY MOVED OFF this pagos_generados/
# "Ver archivos de pago" path (2026-09-04, Reporte/Grupo activity logging
# stage) -- a financial summary report isn't a payment file, so mixing it
# into the same store as real bank-transfer instructions (masked CLABEs,
# beneficiaries) behind a payment-specific permission was a miscategorization,
# not a feature. It now logs via log_action(module="reporte", ...) directly
# instead -- see tests/test_reporte_activity_logging.R for that coverage. ───
cat("\n-- Part G4: reporte_module.R / bancos_module.R hooks --\n")

rep_txt <- readLines("R/reporte_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/reporte_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/reporte_module.R still parses without syntax errors")
.chk(length(grep('tipo = "reporte_financiero_html"', rep_txt)), 0L,
     "dl_html (Reporte tab) no longer logs via the payment-file tipo= path (moved to log_action(module=\"reporte\"))")
.chk(length(grep('tipo = "reporte_financiero_pdf"', rep_txt)), 0L,
     "dl_pdf (Reporte tab) no longer logs via the payment-file tipo= path")
.chk(length(grep('log_archivo_generado(', rep_txt, fixed = TRUE)), 0L,
     "R/reporte_module.R no longer CALLS log_archivo_generado() (the name may still appear in an explanatory comment)")

banc_txt <- readLines("R/bancos_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/bancos_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/bancos_module.R still parses without syntax errors")
.chk(length(grep('tipo = "bancos_libro_excel"', banc_txt)), 1L,
     "download_libro (Bancos tab) logs with tipo=\"bancos_libro_excel\" (unchanged, out of scope for this stage)")

for (t in c("bancos_libro_excel")) {
  .chk(nzchar(.tipo_label(t)), TRUE, sprintf(".tipo_label('%s') has a real (non-empty) display label", t))
}
.chk(is.na(unname(.TIPO_LABELS["reporte_financiero_html"])), TRUE,
     "the now-dead reporte_financiero_html tipo label was removed from .TIPO_LABELS, not left dangling")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
