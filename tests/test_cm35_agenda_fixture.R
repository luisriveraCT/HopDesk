# =============================================================================
# tests/test_cm35_agenda_fixture.R
# CM35 (integrator's A1). tests/helpers/agenda_fixture_app.R -- the Agenda's
# browser fixture (test_agenda_hd_chromote and the payment-file tests) --
# must let its Confirmar run end to end:
#   - it loads pagar_hoy_module.R piecewise, so autor_registrable() (CM35,
#     confirmado_por) has to be loaded with it, or Confirmar errors;
#   - its safe_load_pagar_hoy() stub returned NULL "to fall back to the
#     session's copy"; since CM33 a NULL read is a failed read and Confirmar
#     stops with nothing written, so the stub answers with the fixture's
#     shared Agenda.
# No browser: the fixture's own server under testServer().
#
# Run from project root:  Rscript tests/test_cm35_agenda_fixture.R
# =============================================================================
cat("=== test_cm35_agenda_fixture ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))

ok("the fixture loads autor_registrable() next to pagar_hoy_module.R", exists("autor_registrable", mode = "function"))
ok("its safe_load_pagar_hoy() answers with the shared Agenda, not NULL",
   is.data.frame(safe_load_pagar_hoy()) && nrow(safe_load_pagar_hoy()) > 0L)

AVISOS <- character()
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
showModal <- function(...) invisible(NULL); removeModal <- function(...) invisible(NULL)
res <- NULL   # testServer() does not return its expression's value: set from inside
err <- tryCatch({
  testServer(app, {
    session$flushReact()
    antes <- nrow(AGENDA())
    do.call(session$setInputs, setNames(list("MXN", as.character(Sys.Date())),
                                        c(paste0("ph-bal_cur_", EMP), paste0("ph-conf_fecha_ap_", EMP))))
    do.call(session$setInputs, setNames(list(list(nonce = 1)), paste0("ph-do_confirm_ap_", EMP)))
    session$flushReact()
    res <<- list(reg = registro(), antes = antes, despues = nrow(AGENDA()))
  })
  NULL
}, error = function(e) e)
if (inherits(err, "error")) res <- err
ok("Confirmar in the fixture runs without an error (no missing function)", !inherits(res, "error"))
if (inherits(res, "error")) message("  ", conditionMessage(res))
ok("…it confirms (the fixture records the confirmations) and nothing says 'try again'",
   is.list(res) && any(grepl("^confirmados AP: ", res$reg)) && !any(AVISOS %in% tr("comun.reintentar")))
ok("…and the confirmed rows leave the shared Agenda", is.list(res) && res$despues < res$antes)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
