# =============================================================================
# tests/test_activity_stage2_detail_modal.R
#
# Stage 2 of the Actividad audit overhaul (2026-08-26): a click on any row's
# Descripción cell opens a detail modal, with bespoke rendering for payments
# (the priority), policy applies, and login/logout, and a generic key/value
# fallback for every other module/action so no log type is left without a
# popup. This file unit-tests the pure classification/parsing helpers, the
# tag-building renderers (via as.character() + grepl, since they return
# shiny.tag objects with no running browser to inspect), and a handful of
# testServer() smoke tests for the click -> modal wiring itself.
#
# Sourced by _run_saas.R (which already loads R/audit_log_viewer_module.R
# into globalenv() before any test file runs).
# =============================================================================

cat("── Activity audit Stage 2: clickable Descripción detail modal ────────────\n")

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
.html <- function(tag) paste(as.character(tag), collapse = "\n")

# =============================================================================
# Part A -- .audit_detail_kind()
# =============================================================================
cat("\n-- Part A: .audit_detail_kind() --\n")

.chk(.audit_detail_kind("pagar_hoy", "confirmar_pago"),            "payment_pago",  "confirmar_pago -> payment_pago")
.chk(.audit_detail_kind("pagar_hoy", "confirmar_pago_incompleto"), "payment_pago",  "confirmar_pago_incompleto -> payment_pago")
.chk(.audit_detail_kind("pagar_hoy", "confirmar_cobro"),            "payment_cobro", "confirmar_cobro -> payment_cobro")
.chk(.audit_detail_kind("pagar_hoy", "confirmar_cobro_incompleto"), "payment_cobro", "confirmar_cobro_incompleto -> payment_cobro")
.chk(.audit_detail_kind("settings_companies", "aplicar_politicas"),      "policy", "aplicar_politicas -> policy")
.chk(.audit_detail_kind("settings_companies", "auto_aplicar_politicas"), "policy", "auto_aplicar_politicas -> policy")
.chk(.audit_detail_kind("auth", "login"),  "login",  "auth/login -> login")
.chk(.audit_detail_kind("auth", "logout"), "logout", "auth/logout -> logout")
.chk(.audit_detail_kind("pagar_hoy", "quitar_pagos"), "generic", "an unrelated pagar_hoy action falls to generic")
.chk(.audit_detail_kind("bancos", "eliminar_confirmacion"), "generic",
     "a single-row bancos action (no bulk row detail) falls to generic")
.chk(.audit_detail_kind("bancos", "importar_movimientos"), "bancos_movimientos",
     "importar_movimientos gets the bespoke bancos row-table view (Stage 2 of the Bancos/Pasivos logging extension, 2026-09-03)")
.chk(.audit_detail_kind("settings_companies", "guardar_politicas_socio"), "generic",
     "a settings_companies action NOT in the policy-apply set falls to generic")
.chk(.audit_detail_kind(NA, NA), "generic", "NA module/action falls to generic, never errors")
.chk(.audit_detail_kind(NULL, NULL), "generic", "NULL module/action falls to generic, never errors")
.chk(.audit_detail_kind("clientes", "external_access"), "generic", "jump/bookkeeping rows fall to generic")

# =============================================================================
# Part B -- .audit_parse_metadata()
# =============================================================================
cat("\n-- Part B: .audit_parse_metadata() --\n")

.chk(.audit_parse_metadata(NA_character_), list(), "NA metadata returns empty list")
.chk(.audit_parse_metadata(""), list(), "empty string metadata returns empty list")
.chk(.audit_parse_metadata(NULL), list(), "NULL metadata returns empty list")
.chk(.audit_parse_metadata("not valid json{{{"), list(),
     "malformed JSON returns empty list, does not error")

valid <- .audit_parse_metadata('{"empresa":"ACME","n_pago":3}')
.chk(valid$empresa, "ACME", "valid JSON: empresa field parsed")
.chk(valid$n_pago,  3,      "valid JSON: numeric field parsed")

.chk(.audit_parse_metadata("{}"), list(), "empty JSON object returns an empty (but non-erroring) list")

# =============================================================================
# Part C -- .audit_normalize_pagos()
# =============================================================================
cat("\n-- Part C: .audit_normalize_pagos() --\n")

EMPTY_COLS <- c("parte", "documento", "importe", "moneda", "cuenta_id", "fecha")

.chk(names(.audit_normalize_pagos(NULL)), EMPTY_COLS, "NULL input: empty frame with the right columns")
.chk(nrow(.audit_normalize_pagos(NULL)), 0L, "NULL input: zero rows")
.chk(nrow(.audit_normalize_pagos(list())), 0L, "empty list input: zero rows")

# Real round-trip: build the exact JSON shape .pago_audit_detalle() +
# log_action() produce, then parse it back the way the modal actually would.
pago_rows <- data.frame(Parte = c("ACME SA", "BETA SA"), Documento = c("FAC-1", "FAC-2"),
                        Importe = c(100.5, 250), stringsAsFactors = FALSE)
det <- .pago_audit_detalle(pago_rows, "MXN", "CTA-01", as.Date("2026-08-26"))
meta_json <- jsonlite::toJSON(list(pagos = det), auto_unbox = TRUE, na = "null")
meta_back <- .audit_parse_metadata(as.character(meta_json))
norm <- .audit_normalize_pagos(meta_back$pagos)
.chk(nrow(norm), 2L, "real round-trip: 2 rows survive JSON round-trip + normalize")
.chk(norm$parte, c("ACME SA", "BETA SA"), "real round-trip: parte values intact")
.chk(as.numeric(norm$importe), c(100.5, 250), "real round-trip: importe values intact, numeric")

# Single-row round-trip specifically (a common real case: one invoice paid alone) --
# guards against auto_unbox collapsing a length-1 array of objects into a bare object.
det1 <- .pago_audit_detalle(pago_rows[1, , drop = FALSE], "MXN", "CTA-01", as.Date("2026-08-26"))
meta_json1 <- jsonlite::toJSON(list(pagos = det1), auto_unbox = TRUE, na = "null")
meta_back1 <- .audit_parse_metadata(as.character(meta_json1))
norm1 <- .audit_normalize_pagos(meta_back1$pagos)
.chk(nrow(norm1), 1L, "single-row round-trip: exactly 1 row (not collapsed/lost)")
.chk(norm1$parte[1], "ACME SA", "single-row round-trip: parte intact")

# Missing-column defensiveness
partial <- list(list(parte = "ACME"))
norm_partial <- .audit_normalize_pagos(partial)
.chk(nrow(norm_partial), 1L, "partial record (missing columns): still 1 row")
.chk(is.na(norm_partial$importe[1]), TRUE, "partial record: missing importe becomes NA, not an error")

# =============================================================================
# Part D -- .audit_detail_payment()
# =============================================================================
cat("\n-- Part D: .audit_detail_payment() --\n")

meta_pago <- list(empresa = "Networks Realtors", moneda = "USD", cuenta_id = "CTA-02",
                  fecha_pago = "2026-08-20",
                  pagos = list(list(parte = "ACME SA", documento = "FAC-1",
                                    importe = 1500.75, moneda = "USD",
                                    cuenta_id = "CTA-02", fecha = "2026-08-20")))
html_pago <- .html(.audit_detail_payment(meta_pago, "pagos", "Pago(s)"))
.chk_true(grepl("Networks Realtors", html_pago, fixed = TRUE), "payment detail: empresa shown")
.chk_true(grepl("USD", html_pago, fixed = TRUE), "payment detail: moneda shown")
.chk_true(grepl("CTA-02", html_pago, fixed = TRUE), "payment detail: cuenta_id shown")
.chk_true(grepl("ACME SA", html_pago, fixed = TRUE), "payment detail: partner name shown in the row table")
.chk_true(grepl("FAC-1", html_pago, fixed = TRUE), "payment detail: documento shown")
.chk_true(grepl("1,500.75", html_pago, fixed = TRUE), "payment detail: importe formatted with thousands separator")

meta_empty_pago <- list(empresa = "ACME", moneda = "MXN", cuenta_id = "CTA-01", pagos = list())
html_empty <- .html(.audit_detail_payment(meta_empty_pago, "pagos", "Pago(s)"))
.chk_true(grepl("Sin detalle de pago", html_empty, fixed = TRUE),
     "payment detail: empty pagos array shows the 'no detail' message, not an empty/broken table")

html_cobro <- .html(.audit_detail_payment(
  list(empresa = "X", moneda = "MXN", cuenta_id = "C1",
       cobros = list(list(parte = "CLIENTE Z", documento = "FAC-9", importe = 500,
                          moneda = "MXN", cuenta_id = "C1", fecha = "2026-08-01"))),
  "cobros", "Cobro(s)"))
.chk_true(grepl("CLIENTE Z", html_cobro, fixed = TRUE), "cobro detail: uses the 'cobros' key correctly")

# =============================================================================
# Part E -- .audit_detail_policy()
# =============================================================================
cat("\n-- Part E: .audit_detail_policy() --\n")

html_pol <- .html(.audit_detail_policy(list(
  policy_names = c("Neteo 30 días", "Fin de mes"), n_moves = 12L,
  partes = c("ACME", "BETA", "GAMMA")
)))
.chk_true(grepl("Neteo 30 días", html_pol, fixed = TRUE), "policy detail: first policy name shown")
.chk_true(grepl("Fin de mes", html_pol, fixed = TRUE), "policy detail: second policy name shown")
.chk_true(grepl("12", html_pol, fixed = TRUE), "policy detail: n_moves shown")
.chk_true(grepl("ACME", html_pol, fixed = TRUE) && grepl("GAMMA", html_pol, fixed = TRUE),
     "policy detail: affected partners shown")

html_pol_empty <- .html(.audit_detail_policy(list(n_moves = 0L)))
.chk_true(grepl("&#8212;|—|—", html_pol_empty),
     "policy detail: no policy_names/partes falls back to an em-dash placeholder, not blank/NA text")

# =============================================================================
# Part F -- .audit_detail_login()
# =============================================================================
cat("\n-- Part F: .audit_detail_login() --\n")

html_login_staff <- .html(.audit_detail_login(list(user = "bunny", tier = "hopdesk", is_staff = TRUE)))
.chk_true(grepl("bunny", html_login_staff, fixed = TRUE), "login detail: user shown")
.chk_true(grepl("hopdesk", html_login_staff, fixed = TRUE), "login detail: tier shown")
.chk_true(grepl("Sí", html_login_staff), "login detail: is_staff=TRUE renders as 'Sí'")

html_login_client <- .html(.audit_detail_login(list(user = "tesoreria", tier = "finance", is_staff = FALSE)))
.chk_true(grepl("No", html_login_client, fixed = TRUE) && !grepl("Sí", html_login_client, fixed = TRUE),
     "login detail: is_staff=FALSE renders as 'No', not 'Sí'")

html_login_missing <- .html(.audit_detail_login(list(user = "x")))
.chk_true(is.character(html_login_missing) && nzchar(html_login_missing),
     "login detail: missing tier/is_staff fields never error, still render")

# =============================================================================
# Part G -- .audit_detail_logout()
# =============================================================================
cat("\n-- Part G: .audit_detail_logout() --\n")

html_logout <- .html(.audit_detail_logout(list(
  user = "larivera", duration_seconds = 7325,
  login_ts = "2026-08-26 09:00:00", logout_ts = "2026-08-26 11:02:05"
)))
.chk_true(grepl("larivera", html_logout, fixed = TRUE), "logout detail: user shown")
.chk_true(grepl("2h 2min", html_logout, fixed = TRUE),
     "logout detail: duration formatted via format_session_duration()")
.chk_true(grepl("2026-08-26 09:00:00", html_logout, fixed = TRUE), "logout detail: login_ts shown")
.chk_true(grepl("2026-08-26 11:02:05", html_logout, fixed = TRUE), "logout detail: logout_ts shown")

html_logout_unknown <- .html(.audit_detail_logout(list(user = "x", duration_seconds = NA_real_)))
.chk_true(grepl("Desconocida", html_logout_unknown, fixed = TRUE),
     "logout detail: NA duration renders as 'Desconocida', not '0s' or an error")

html_logout_null <- .html(.audit_detail_logout(list(user = "x")))
.chk_true(grepl("Desconocida", html_logout_null, fixed = TRUE),
     "logout detail: entirely missing duration_seconds also renders as 'Desconocida'")

# =============================================================================
# Part H -- .audit_detail_generic()
# =============================================================================
cat("\n-- Part H: .audit_detail_generic() --\n")

html_generic_empty <- .html(.audit_detail_generic(list()))
.chk_true(grepl("Sin detalle adicional", html_generic_empty, fixed = TRUE),
     "generic detail: empty metadata shows the 'no detail' message")

html_generic <- .html(.audit_detail_generic(list(scope = "new", n_moves = 5L, ids = c("a", "b", "c"))))
.chk_true(grepl("scope", html_generic, fixed = TRUE), "generic detail: key 'scope' rendered")
.chk_true(grepl("new", html_generic, fixed = TRUE), "generic detail: value 'new' rendered")
.chk_true(grepl("a, b, c", html_generic, fixed = TRUE),
     "generic detail: a vector value is collapsed to a comma-separated string")

# =============================================================================
# Part I -- .audit_detail_modal_body() end-to-end (classification + parse + render)
# =============================================================================
cat("\n-- Part I: .audit_detail_modal_body() end-to-end --\n")

.mk_row <- function(module, action, description, metadata_list,
                    target_id = NA_character_, client_id = NA_character_,
                    user = "larivera") {
  data.frame(
    id = "row-1", ts = as.POSIXct("2026-08-26 12:00:00", tz = "UTC"),
    user = user, module = module, action = action, description = description,
    target_id = target_id, s3_key = NA_character_, client_id = client_id,
    metadata = as.character(jsonlite::toJSON(metadata_list, auto_unbox = TRUE, na = "null")),
    stringsAsFactors = FALSE
  )
}

row_pago <- .mk_row("pagar_hoy", "confirmar_pago", "1 pago(s) confirmado(s)",
  list(empresa = "ACME", moneda = "MXN", cuenta_id = "CTA-01",
       pagos = list(list(parte = "PROVEEDOR X", documento = "FAC-7",
                         importe = 300, moneda = "MXN", cuenta_id = "CTA-01", fecha = "2026-08-26"))),
  target_id = "ACME")
html_i1 <- .html(.audit_detail_modal_body(row_pago, "own_client"))
.chk_true(grepl("larivera", html_i1, fixed = TRUE), "end-to-end: common header shows user")
.chk_true(grepl("26/08/2026", html_i1, fixed = TRUE), "end-to-end: common header shows formatted date")
.chk_true(grepl("pagar_hoy", html_i1, fixed = TRUE), "end-to-end: common header shows module")
.chk_true(grepl("confirmar_pago", html_i1, fixed = TRUE), "end-to-end: common header shows action")
.chk_true(grepl("ACME", html_i1, fixed = TRUE), "end-to-end: ID destino shown when present")
.chk_true(grepl("PROVEEDOR X", html_i1, fixed = TRUE), "end-to-end: payment row detail present (bespoke renderer engaged)")

row_policy <- .mk_row("settings_companies", "auto_aplicar_politicas", "Auto-aplicación",
  list(n_moves = 4L, partes = c("ACME", "BETA"), policy_names = c("Fin de mes")),
  user = "system")
html_i2 <- .html(.audit_detail_modal_body(row_policy, "own_client"))
.chk_true(grepl("Fin de mes", html_i2, fixed = TRUE), "end-to-end: policy name present (bespoke renderer engaged)")
.chk_false(grepl("class=\"table table-sm table-hover", html_i2, fixed = TRUE),
     "end-to-end: policy view does NOT use the payment row-table markup (basic info only, per spec)")

row_login <- .mk_row("auth", "login", "Inicio de sesión: bunny",
  list(user = "bunny", tier = "hopdesk", is_staff = TRUE), user = "bunny")
html_i3 <- .html(.audit_detail_modal_body(row_login, "own_client"))
.chk_true(grepl("Sí", html_i3), "end-to-end: login view shows is_staff (bespoke renderer engaged)")

row_logout <- .mk_row("auth", "logout", "Cierre de sesión: bunny (5min 0s activo)",
  list(user = "bunny", duration_seconds = 300, login_ts = "2026-08-26 08:00:00",
       logout_ts = "2026-08-26 08:05:00"), user = "bunny")
html_i4 <- .html(.audit_detail_modal_body(row_logout, "own_client"))
.chk_true(grepl("5min 0s", html_i4, fixed = TRUE), "end-to-end: logout view shows formatted duration")

row_generic <- .mk_row("bancos", "eliminar_confirmacion", "Confirmación bancaria movida a papelera",
  list(empresa = "ACME", cuenta = "CTA-05"))
html_i5 <- .html(.audit_detail_modal_body(row_generic, "own_client"))
.chk_true(grepl("cuenta", html_i5, fixed = TRUE), "end-to-end: unmodeled module/action falls through to generic key/value view")
.chk_true(grepl("CTA-05", html_i5, fixed = TRUE), "end-to-end: generic view's value rendered")

# target_id / Cliente visibility
row_no_target <- .mk_row("bancos", "importar_movimientos", "x", list(), target_id = NA_character_)
html_i6 <- .html(.audit_detail_modal_body(row_no_target, "own_client"))
.chk_false(grepl("ID destino", html_i6, fixed = TRUE),
     "end-to-end: NA target_id -> 'ID destino' row omitted entirely, not shown blank")

row_multi <- .mk_row("bancos", "importar_movimientos", "x", list(), client_id = "networks")
html_i7 <- .html(.audit_detail_modal_body(row_multi, "multi_client"))
.chk_true(grepl("networks", html_i7, fixed = TRUE), "end-to-end: multi_client mode shows the Cliente field")
html_i8 <- .html(.audit_detail_modal_body(row_multi, "own_client"))
.chk_false(grepl("Cliente:", html_i8, fixed = TRUE), "end-to-end: own_client mode never shows a Cliente field")

# =============================================================================
# Part J -- testServer(): click -> modal wiring smoke tests
# =============================================================================
cat("\n-- Part J: click observer wiring (testServer) --\n")

.j_mock_shared <- function(user, tier, client_id, is_staff, home_cid, effective_cid) {
  list(
    current_user_info  = function() list(user = user, tier = tier, client_id = client_id, is_staff = is_staff),
    home_client_id      = function() home_cid,
    effective_client_id = function() effective_cid
  )
}

Sys.setenv(CLIENT_ID = "stage2test")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "stage2test/app_audit.rds", "mock-bucket")
log_action(user = "larivera", module = "pagar_hoy", action = "confirmar_pago",
          description = "1 pago(s) confirmado(s)", target_id = "ACME",
          metadata = list(empresa = "ACME", moneda = "MXN", cuenta_id = "CTA-01",
                          pagos = list(list(parte = "X", documento = "F-1", importe = 10,
                                            moneda = "MXN", cuenta_id = "CTA-01", fecha = "2026-08-26"))),
          client_id = "stage2test", viewer_home_client_id = "stage2test")
.audit_flush_pending("stage2test")

shiny::testServer(auditLogViewerServer, args = list(
  shared = .j_mock_shared("larivera", "finance", "stage2test", FALSE, "stage2test", "stage2test"),
  mode   = "own_client"
), {
  session$flushReact()
  .chk_true(nrow(visible_fetched()) >= 1L, "testServer setup: seeded row visible before click tests")

  ok1 <- tryCatch({
    session$setInputs(audit_tbl_cell_clicked = list(row = 1, col = .audit_desc_col0))
    session$flushReact()
    TRUE
  }, error = function(e) { message("  click on Descripción cell errored: ", e$message); FALSE })
  .chk_true(ok1, "clicking row 1's Descripción cell does not error (modal opens)")

  ok2 <- tryCatch({
    session$setInputs(audit_tbl_cell_clicked = list(row = 1, col = 0L))
    session$flushReact()
    TRUE
  }, error = function(e) { message("  click on a non-Descripción cell errored: ", e$message); FALSE })
  .chk_true(ok2, "clicking a DIFFERENT column (Hora, col=0) does not error (silent no-op)")

  ok3 <- tryCatch({
    session$setInputs(audit_tbl_cell_clicked = list(row = 9999L, col = .audit_desc_col0))
    session$flushReact()
    TRUE
  }, error = function(e) { message("  out-of-range row click errored: ", e$message); FALSE })
  .chk_true(ok3, "clicking an out-of-range row index does not error (guarded, silent no-op)")

  ok4 <- tryCatch({
    session$setInputs(audit_tbl_cell_clicked = NULL)
    session$flushReact()
    TRUE
  }, error = function(e) { message("  NULL cell_clicked errored: ", e$message); FALSE })
  .chk_true(ok4, "a NULL cell_clicked value (initial state) does not error")
})

cat(sprintf("\n  Stage 2 subtotal: %d passed, %d failed\n", .pass, .fail))
