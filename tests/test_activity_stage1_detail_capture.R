# =============================================================================
# tests/test_activity_stage1_detail_capture.R
#
# Stage 1 of the Actividad audit overhaul (2026-08-26): richer metadata
# capture at the log_action() call sites Mouse asked to be able to audit --
# payments (pagar_hoy confirmar_pago/confirmar_cobro, "the one I need detail
# on the most"), policy application (settings_companies aplicar_politicas/
# auto_aplicar_politicas), and brand-new login/logout tracking with active-
# session duration. This file unit-tests the three pure helpers that build
# that richer detail, plus the JSON round-trip through log_action()/
# read_audit_log() for the exact metadata shapes those call sites now emit.
#
# Sourced by _run_saas.R. Sources R/pagar_hoy_module.R and
# R/settings/settings_companies.R itself (not part of the harness's default
# source list) purely to reach their module-scope helper functions -- same
# pattern as tests/test_payment_audit_stage.R. Neither server function's body
# ever executes; only their top-level function definitions are read.
# =============================================================================

cat("── Activity audit Stage 1: payment/policy/login detail capture ───────────\n")

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
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

source("R/pagar_hoy_module.R",           local = FALSE)
source("R/settings/settings_companies.R", local = FALSE)

# =============================================================================
# Part A -- .pago_audit_detalle() (R/pagar_hoy_module.R)
# =============================================================================
cat("\n-- Part A: .pago_audit_detalle() --\n")

.chk(.pago_audit_detalle(NULL, "MXN", "cta1", as.Date("2026-08-26")), list(),
     "NULL factura_rows returns empty list")

empty_df <- data.frame(Parte = character(), Documento = character(),
                       Importe = numeric(), stringsAsFactors = FALSE)
.chk(.pago_audit_detalle(empty_df, "MXN", "cta1", as.Date("2026-08-26")), list(),
     "zero-row factura_rows returns empty list")

one_row <- data.frame(Parte = "ACME SA", Documento = "FAC-100", Importe = 1500.75,
                      stringsAsFactors = FALSE)
det1 <- .pago_audit_detalle(one_row, "MXN", "CTA-01", as.Date("2026-08-26"))
.chk(length(det1), 1L, "single row: one detail entry")
.chk(det1[[1]]$parte,     "ACME SA",  "single row: parte captured")
.chk(det1[[1]]$documento, "FAC-100",  "single row: documento captured")
.chk(det1[[1]]$importe,   1500.75,    "single row: importe captured")
.chk(det1[[1]]$moneda,    "MXN",      "single row: moneda captured")
.chk(det1[[1]]$cuenta_id, "CTA-01",   "single row: cuenta_id captured")
.chk(det1[[1]]$fecha,     "2026-08-26", "single row: fecha captured as character")

multi_row <- data.frame(
  Parte     = c("ACME SA", "BETA SA DE CV", "GAMMA CORP"),
  Documento = c("FAC-1", "FAC-2", "FAC-3"),
  Importe   = c(100.00, 250.50, 999.99),
  stringsAsFactors = FALSE
)
det3 <- .pago_audit_detalle(multi_row, "USD", "CTA-02", as.Date("2026-08-20"))
.chk(length(det3), 3L, "three rows: three detail entries")
.chk(vapply(det3, function(x) x$parte, character(1)),
     c("ACME SA", "BETA SA DE CV", "GAMMA CORP"),
     "three rows: partes in order")
.chk(vapply(det3, function(x) x$importe, numeric(1)), c(100.00, 250.50, 999.99),
     "three rows: importes in order")
.chk(unique(vapply(det3, function(x) x$moneda, character(1))), "USD",
     "three rows: same moneda for every row in one confirm")
.chk(unique(vapply(det3, function(x) x$cuenta_id, character(1))), "CTA-02",
     "three rows: same cuenta_id for every row in one confirm")

na_row <- data.frame(Parte = NA_character_, Documento = "FAC-9", Importe = 50,
                     stringsAsFactors = FALSE)
det_na <- .pago_audit_detalle(na_row, NA_character_, NA_character_, NA)
.chk(is.na(det_na[[1]]$parte), TRUE, "NA parte stays NA, not a literal 'NA' string")
.chk(is.na(det_na[[1]]$moneda), TRUE, "NA moneda (cuenta unassigned) stays NA")
.chk(is.na(det_na[[1]]$cuenta_id), TRUE, "NA cuenta_id stays NA")

# JSON round-trip of a realistic multi-row payload -- this is the exact shape
# now attached to a "confirmar_pago"/"confirmar_cobro" log_action() call.
det_json <- jsonlite::toJSON(list(pagos = det3), auto_unbox = TRUE)
det_back <- jsonlite::fromJSON(det_json, simplifyVector = TRUE)
.chk(nrow(det_back$pagos), 3L, "JSON round-trip: 3 rows survive")
.chk(det_back$pagos$parte, c("ACME SA", "BETA SA DE CV", "GAMMA CORP"),
     "JSON round-trip: parte column intact")
.chk(as.numeric(det_back$pagos$importe), c(100.00, 250.50, 999.99),
     "JSON round-trip: importe column intact, numeric")

# =============================================================================
# Part B -- .policy_names_from_moves() (R/settings/settings_companies.R)
# =============================================================================
cat("\n-- Part B: .policy_names_from_moves() --\n")

catalog <- data.frame(
  id   = c("pol-1", "pol-2", "pol-3"),
  name = c("Neteo 30 días", "Fin de mes", "Saltar feriados"),
  stringsAsFactors = FALSE
)

.chk(.policy_names_from_moves(NULL, catalog), list(ids = character(0), names = character(0)),
     "NULL moves returns empty ids/names")

moves_no_col <- data.frame(Parte = "ACME", stringsAsFactors = FALSE)
.chk(.policy_names_from_moves(moves_no_col, catalog),
     list(ids = character(0), names = character(0)),
     "moves frame without policy_ids column returns empty")

moves_empty_ids <- data.frame(Parte = c("ACME", "BETA"), policy_ids = c(NA, NA),
                              stringsAsFactors = FALSE)
.chk(.policy_names_from_moves(moves_empty_ids, catalog),
     list(ids = character(0), names = character(0)),
     "moves frame with all-NA policy_ids returns empty")

moves_single <- data.frame(Parte = "ACME", policy_ids = "pol-1", stringsAsFactors = FALSE)
res_single <- .policy_names_from_moves(moves_single, catalog)
.chk(res_single$ids,   "pol-1",          "single policy id resolved")
.chk(res_single$names, "Neteo 30 días",  "single policy name resolved from catalog")

moves_stacked <- data.frame(
  Parte      = c("ACME", "BETA", "GAMMA"),
  policy_ids = c("pol-1,pol-2", "pol-2", "pol-3,pol-1"),
  stringsAsFactors = FALSE
)
res_stacked <- .policy_names_from_moves(moves_stacked, catalog)
.chk(sort(res_stacked$ids), sort(c("pol-1", "pol-2", "pol-3")),
     "stacked/duplicated policy_ids de-duplicated across all rows")
.chk(length(res_stacked$ids), length(res_stacked$names),
     "ids and names vectors stay the same length")
.chk_true(all(c("Neteo 30 días", "Fin de mes", "Saltar feriados") %in% res_stacked$names),
     "all three distinct policy names present exactly once each")

moves_unknown <- data.frame(Parte = "ACME", policy_ids = "pol-999", stringsAsFactors = FALSE)
res_unknown <- .policy_names_from_moves(moves_unknown, catalog)
.chk(res_unknown$ids,   "pol-999", "unresolvable policy id still returned")
.chk(res_unknown$names, "pol-999", "unresolvable policy id falls back to the raw id as its 'name'")

res_no_catalog <- .policy_names_from_moves(moves_single, NULL)
.chk(res_no_catalog$ids,   "pol-1", "NULL catalog: id still extracted")
.chk(res_no_catalog$names, "pol-1", "NULL catalog: name falls back to raw id, never errors")

res_empty_catalog <- .policy_names_from_moves(moves_single, catalog[0, ])
.chk(res_empty_catalog$names, "pol-1", "zero-row catalog: falls back to raw id, never errors")

# =============================================================================
# Part C -- format_session_duration() (R/app_audit.R)
# =============================================================================
cat("\n-- Part C: format_session_duration() --\n")

.chk(format_session_duration(0),    "0s",        "0 seconds")
.chk(format_session_duration(1),    "1s",        "1 second")
.chk(format_session_duration(59),   "59s",       "59 seconds: still seconds-only")
.chk(format_session_duration(60),   "1min 0s",   "exactly 60s rolls to 1 minute")
.chk(format_session_duration(61),   "1min 1s",   "61s: 1 minute 1 second")
.chk(format_session_duration(125),  "2min 5s",   "125s: 2 minutes 5 seconds")
.chk(format_session_duration(3599), "59min 59s", "3599s: just under an hour")
.chk(format_session_duration(3600), "1h 0min",   "exactly 3600s rolls to 1 hour")
.chk(format_session_duration(3661), "1h 1min",   "3661s: 1h 1min (seconds dropped once hours shown)")
.chk(format_session_duration(7325), "2h 2min",   "7325s: 2h 2min")
.chk(format_session_duration(-5),   "0s",        "negative input clamps to 0s, never a negative duration")
.chk(format_session_duration(NA),   "0s",        "NA input falls back to 0s, never errors")
.chk(format_session_duration(NULL), "0s",        "NULL input falls back to 0s via %||%, never errors")

# =============================================================================
# Part D -- log_action() metadata JSON round-trip for the new call-site shapes
# =============================================================================
cat("\n-- Part D: log_action() end-to-end for payment/policy/login/logout shapes --\n")

# Uses mock_s3readRDS()/mock_s3saveRDS() -- the SAME technique as
# tests/test_saas_audit.R, and the only one that actually works: log_action()
# resolves .audit_read_chunk()/.audit_write_chunk() via app_audit.R's own
# (global-env) closure, so redefining those names inside this file's sourcing
# environment has no effect on log_action()'s real writes. What actually
# routes log_action() to an isolated in-memory store is _run_saas.R's
# process-wide aws.s3::s3readRDS/s3saveRDS namespace patch, which the REAL
# (unmodified) .audit_read_chunk/.audit_write_chunk call into -- so seeding/
# reading through mock_s3readRDS/mock_s3saveRDS directly (defined by
# _run_saas.R, visible here since this file's env parents globalenv()) is
# what actually observes log_action()'s writes.
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "stage1test/app_audit.rds", "mock-bucket")
Sys.setenv(CLIENT_ID = "stage1test")

# -- D1: confirmar_pago-shaped call --
pago_detalle <- .pago_audit_detalle(multi_row, "USD", "CTA-02", as.Date("2026-08-20"))
log_action(
  user = "larivera", module = "pagar_hoy", action = "confirmar_pago",
  description = "3 pago(s) confirmado(s)", target_id = "Networks Realtors",
  metadata = list(empresa = "Networks Realtors", n_pago = 3L, n_abono = 0L,
                  moneda = "USD", cuenta_id = "CTA-02", fecha_pago = "2026-08-20",
                  pagos = pago_detalle, secondary_error = NA_character_),
  client_id = "stage1test", viewer_home_client_id = "stage1test"
)
.audit_flush_pending("stage1test")
log_df <- mock_s3readRDS("stage1test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df), 1L, "D1: confirmar_pago entry written")
.chk(log_df$action[1], "confirmar_pago", "D1: action column correct")
meta1 <- jsonlite::fromJSON(log_df$metadata[1], simplifyVector = TRUE)
.chk(meta1$cuenta_id, "CTA-02", "D1: metadata cuenta_id survives round-trip")
.chk(meta1$moneda,    "USD",    "D1: metadata moneda survives round-trip")
.chk(nrow(meta1$pagos), 3L,     "D1: metadata pagos array has 3 rows after round-trip")
.chk(meta1$pagos$documento, c("FAC-1", "FAC-2", "FAC-3"),
     "D1: metadata pagos documento values intact")

# -- D2: auto_aplicar_politicas-shaped call --
pol_res <- .policy_names_from_moves(moves_stacked, catalog)
log_action(
  user = "system", module = "settings_companies", action = "auto_aplicar_politicas",
  description = "Auto-aplicación de políticas: 3 documento(s) (3 socio(s))",
  metadata = list(n_moves = 3L, partes = c("ACME", "BETA", "GAMMA"),
                  policy_ids = pol_res$ids, policy_names = pol_res$names),
  client_id = "stage1test", viewer_home_client_id = "stage1test"
)
.audit_flush_pending("stage1test")
log_df2 <- mock_s3readRDS("stage1test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df2), 2L, "D2: two entries after policy log")
meta2 <- jsonlite::fromJSON(log_df2$metadata[2], simplifyVector = TRUE)
.chk_true(all(c("Neteo 30 días", "Fin de mes", "Saltar feriados") %in% meta2$policy_names),
     "D2: metadata policy_names round-trips with all three names")

# -- D3: login-shaped call --
log_action(
  user = "larivera", module = "auth", action = "login",
  description = "Inicio de sesión: larivera", target_id = "larivera",
  metadata = list(user = "larivera", tier = "dev", is_staff = TRUE),
  client_id = "stage1test", viewer_home_client_id = "stage1test"
)
.audit_flush_pending("stage1test")
log_df3 <- mock_s3readRDS("stage1test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df3), 3L, "D3: three entries after login log")
.chk(log_df3$module[3], "auth", "D3: module='auth' for login entry")
.chk(log_df3$action[3], "login", "D3: action='login'")
meta3 <- jsonlite::fromJSON(log_df3$metadata[3], simplifyVector = TRUE)
.chk_true(isTRUE(meta3$is_staff), "D3: metadata is_staff survives round-trip as TRUE")

# -- D4: logout-shaped call, with a realistic duration --
dur <- 7325  # 2h 2min
log_action(
  user = "larivera", module = "auth", action = "logout",
  description = sprintf("Cierre de sesión: larivera (%s activo)", format_session_duration(dur)),
  target_id = "larivera",
  metadata = list(user = "larivera", duration_seconds = dur,
                  login_ts = "2026-08-26 09:00:00", logout_ts = "2026-08-26 11:02:05"),
  client_id = "stage1test", viewer_home_client_id = "stage1test"
)
.audit_flush_pending("stage1test")
log_df4 <- mock_s3readRDS("stage1test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df4), 4L, "D4: four entries after logout log")
.chk(log_df4$action[4], "logout", "D4: action='logout'")
.chk_true(grepl("2h 2min", log_df4$description[4]),
     "D4: description embeds the human-readable duration")
meta4 <- jsonlite::fromJSON(log_df4$metadata[4], simplifyVector = TRUE)
.chk(as.numeric(meta4$duration_seconds), 7325, "D4: metadata duration_seconds round-trips numerically")
.chk(meta4$login_ts,  "2026-08-26 09:00:00", "D4: metadata login_ts round-trips")
.chk(meta4$logout_ts, "2026-08-26 11:02:05", "D4: metadata logout_ts round-trips")

# -- D5: logout with no known login_ts (e.g. pre-existing session at deploy time) --
log_action(
  user = "larivera", module = "auth", action = "logout",
  description = "Cierre de sesión: larivera",
  target_id = "larivera",
  metadata = list(user = "larivera", duration_seconds = NA_real_,
                  login_ts = NA_character_, logout_ts = "2026-08-26 12:00:00"),
  client_id = "stage1test", viewer_home_client_id = "stage1test"
)
.audit_flush_pending("stage1test")
log_df5 <- mock_s3readRDS("stage1test/app_audit.rds", "mock-bucket")
.chk(nrow(log_df5), 5L, "D5: five entries after no-duration logout log")
meta5 <- jsonlite::fromJSON(log_df5$metadata[5], simplifyVector = TRUE)
.chk_true(is.na(meta5$duration_seconds) || is.null(meta5$duration_seconds),
     "D5: unknown duration serializes as NA/null, not a fabricated number")
.chk_false(grepl("activo", log_df5$description[5]),
     "D5: description omits the duration parenthetical when unknown")

cat(sprintf("\n  Stage 1 subtotal: %d passed, %d failed\n", .pass, .fail))
