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

# =============================================================================
# Part A -- persistence layer
# =============================================================================
cat("\n-- Part A: R/pagos_generados_persistence.R --\n")

sch <- .schema_pagos_generados()
expected_cols <- c("id","ts","user","client_id","empresa","moneda","cuenta_origen",
                    "banco_origen","n_pagos","total","beneficiarios","source","archivo")
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
.chk(length(grep("activity_switch_view", tm_txt)) >= 2, TRUE,
     "the switch-view button (input id + observeEvent) is wired")
.chk(length(grep("\\.payment_audit_can_view\\(shared\\)", tm_txt)), 1L,
     "the switch-view button's visibility is gated by .payment_audit_can_view()")

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

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
