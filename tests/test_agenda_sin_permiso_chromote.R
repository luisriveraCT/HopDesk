# =============================================================================
# tests/test_agenda_sin_permiso_chromote.R
# Sending invoices to the Agenda needs can_register_payments (Luis,
# 2026-11-03). Without it, Vencidos, the lupa and the day's tables don't show
# what sends to the Agenda -- "Agregar todo", "Agregar selección", and in the
# day the row's and the group's ＋ -- and the other buttons stay. The
# handlers check the permission again (tests/test_agenda_permiso.R checks
# that statically). In a real headless Chrome, against the Vencidos and day
# fixtures with HD_SIN_AGENDA=1 (their puede() denies that permission only).
#
# Run from project root:  Rscript tests/test_agenda_sin_permiso_chromote.R
# =============================================================================
cat("=== test_agenda_sin_permiso_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(cond, timeout_s = 20)
  poll_until(function() isTRUE(tryCatch(cond(), error = function(e) FALSE)), timeout_s = timeout_s, interval_s = 0.2)
arrancar <- function(fixture, puerto, extra = "") {
  try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(HD_SIN_AGENDA = "1"%s); setwd("%s"); source("tests/helpers/%s_fixture_app.R")$value',
                                        extra, repo, fixture), puerto, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(app)) return(FALSE)
  b <<- new_chromote_session()
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", puerto), wait_ = TRUE))
  espera(function() isTRUE(ev("!!window.Shiny && !!Shiny.setInputValue")), timeout_s = 60)
}
lista <- function(t) unlist(ev(sprintf("(HDT.estado('%s') || {}).visibles", t)))
botones <- function(t, sel) unique(unlist(ev(sprintf(
  "[].map.call(document.querySelectorAll('#%s %s'), function (b) { return b.dataset.accion; })", t, sel))))
AGENDA <- c("agregar", "agregar_todo", "agregar_seleccion")
marcar <- function(t) invisible(ev(sprintf(
  "(function(){ var tr = document.querySelector('#%s tbody tr:not(.hdt-banda):not(.hdt-grupo):not(.hdt-esp)'); tr.children[1].click(); return 0; })()", t)))

# ── Vencidos and the lupa ───────────────────────────────────────────────────
V <- "ven-tabla-t-hdt"; L <- "lupa-tabla-t-hdt"
ok("Vencidos fixture started without the permission", arrancar("vencidos", 39441L))
ok("Vencidos paints its invoices", espera(function() length(lista(V)) > 0, timeout_s = 60))
# Outside Editar its bar holds only the two Agregar; the rest (tags, move,
# Ver datos SAP) is the Editar mode's.
editar <- function(t) invisible(ev(sprintf("document.querySelector('#%s .hdt-editar').click(); 0", t)))
marcar(V)
Sys.sleep(1)
ok("Vencidos, with a row marked: no 'Agregar todo' / 'Agregar selección'",
   !any(botones(V, ".hdt-acciones button.hdt-btn") %in% AGENDA))
editar(V)
ok("Vencidos' Editar mode keeps its own buttons (tags, Ver datos SAP...)",
   espera(function() all(c("etiqueta_a", "crudos") %in% botones(V, ".hdt-acciones button.hdt-btn"))) &&
   !any(botones(V, ".hdt-acciones button.hdt-btn") %in% AGENDA))
invisible(ev("Shiny.setInputValue('btn_search', Math.random(), {priority: 'event'}); 0"))
ok("the lupa opens", espera(function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show') && (HDT.estado('%s') || {visibles: []}).visibles.length > 0", L)))))
marcar(L)
Sys.sleep(1)
editar(L)
ok("the lupa, with a row marked and in Editar: no Agregar buttons, its own ones stay",
   espera(function() "crudos" %in% botones(L, ".hdt-acciones button.hdt-btn")) &&
   !any(botones(L, ".hdt-acciones button.hdt-btn") %in% AGENDA))

# ── The day ─────────────────────────────────────────────────────────────────
R <- "ap-resumen-hdt"
ok("day fixture started without the permission", arrancar("dia", 39442L))
invisible(ev("Shiny.setInputValue('abrir', Math.random(), {priority: 'event'}); 0"))
ok("the day's modal opens on Resumen", espera(function() length(lista(R)) > 0, timeout_s = 30))
ok("the day: no ＋ on rows or groups, and ✎ stays",
   espera(function() "editar" %in% botones(R, "tbody button.hdt-accion")) &&
   !any(botones(R, "tbody button.hdt-accion") %in% AGENDA))
ok("the day's bar: no 'Agregar todo' / 'Agregar selección'",
   !any(botones(R, ".hdt-acciones button.hdt-btn") %in% AGENDA) &&
   { marcar(R); Sys.sleep(1); !any(botones(R, ".hdt-acciones button.hdt-btn") %in% AGENDA) })

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
