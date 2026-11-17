# =============================================================================
# tests/test_t40_ic_sin_cambios.R  (T40)
# Leaving Intercompany for Business Partners before the IC editor's fields
# reached the server said "Cambios sin guardar" with nothing changed:
# .ic_current() read the not-yet-sent fields (NULL) as emptied, so they never
# matched the saved registry (NTS CxC 1027) and the BP panel did not open --
# Ajustes B in test_ajustes_hd_chromote, and any user with a quick click.
#
# Now a field not sent yet (NULL) counts as the saved value in its slot; one
# the user emptied ("") is still a change. Ajustes' real server (the test app
# tests/helpers/ajustes_tablas_fixture_app.R) under shiny::testServer:
#   1. Intercompany opened, no field sent, then Business Partners: the BP panel
#      opens (no "unsaved" stop);
#   2. the fields sent as drawn (1027): BP opens too;
#   3. the user emptied NTS's 1027: it stops ("Cambios sin guardar"), BP does
#      not open;
#   4. the user changed the CxC prefix: it stops too.
# Contraprueba on dd04eb0: 1 fails (the panel stays on Intercompany).
# Run from the project root:  Rscript tests/test_t40_ic_sin_cambios.R
# =============================================================================
cat("=== test_t40_ic_sin_cambios ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
app <- suppressMessages(source("tests/helpers/ajustes_tablas_fixture_app.R")$value)

# What the gear's panel shows after `pasos` (a function of the session).
panel_tras <- function(pasos) {
  out <- NULL
  shiny::testServer(app, {
    # The buttons bind at 0, as in the browser (their observers ignoreInit).
    session$setInputs(stg_btn_interco = 0, stg_btn_catalogo = 0)
    session$setInputs(stg_btn_interco = 1)
    ok("   (Intercompany is open)", es_ic(paste(as.character(output$settings_panel$html), collapse = "")))
    pasos(session)
    session$setInputs(stg_btn_catalogo = 1)
    out <<- paste(as.character(output$settings_panel$html), collapse = "")
  })
  out
}
es_bp <- function(h) grepl("bp_refresh_btn", h, fixed = TRUE)
es_ic <- function(h) grepl("ic_ar_prefix", h, fixed = TRUE) || grepl("ic_editor", h, fixed = TRUE)
# The editor's fields as drawn (the saved registry), `...` replacing some.
campos <- function(session, ...)
  do.call(session$setInputs, utils::modifyList(
    list(ic_ar_prefix = "C", ic_ap_prefix = "P", ic_ar_NTS_1 = "1027", ic_ar_NTS_2 = "", ic_ap_NTS_1 = "",
         ic_ar_NCS_1 = "", ic_ap_NCS_1 = ""), list(...)))

h1 <- panel_tras(function(session) NULL)
ok("1: Intercompany opened, no field sent yet, then Business Partners: the BP panel opens",
   es_bp(h1) && !es_ic(h1))
h2 <- panel_tras(function(session) campos(session))
ok("2: the fields sent as drawn (NTS 1027): BP opens", es_bp(h2))
h3 <- panel_tras(function(session) campos(session, ic_ar_NTS_1 = ""))
ok("3: the user emptied NTS's 1027: 'Cambios sin guardar' stops it, BP does not open",
   !es_bp(h3))
h4 <- panel_tras(function(session) campos(session, ic_ar_prefix = "X"))
ok("4: the user changed the CxC prefix: it stops too", !es_bp(h4))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
