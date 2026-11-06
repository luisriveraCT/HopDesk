# =============================================================================
# tests/test_abono_caracterizacion_chromote.R
# Abono parcial in a real headless Chrome, as it works on master (T25 step
# 0b): what only real clicks and keys show. The fixture is
# tests/helpers/abono_fixture_app.R (the real modal and server over
# tests/helpers/abono_datos_prueba.R; no S3). The DOM is reached through the
# few functions under "Today's modal" so that, after the migration, the same
# cases (B1..) run on the engine. [CAMBIA] marks what reportes/T25_diseno.md
# changes on purpose.
# Sizes: Luis's window (1293 x 632) and 1772 x 866.
#
# Run from project root:  Rscript tests/test_abono_caracterizacion_chromote.R
# =============================================================================
cat("=== test_abono_caracterizacion_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/live_app_harness.R")

repo <- normalizePath(".", winslash = "/")
PORT <- 38994L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/abono_fixture_app.R")$value', repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
    var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
  for (t in c("mouseMoved", "mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[[1]], y = p[[2]], button = if (t == "mouseMoved") "none" else "left",
                               buttons = if (t == "mousePressed") 1L else 0L, clickCount = 1L)
  Sys.sleep(0.4)
}
escribe <- function(js_el, texto) {   # select what is there and type over it
  clic(js_el); ev(sprintf("(function(){ var e = %s; e.select && e.select(); })()", js_el))
  b$Input$insertText(text = texto); Sys.sleep(0.4)
}
espera <- function(f, s = 8) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}", simplifyVector = FALSE)

# ── Today's modal (the only part that changes with the migration) ───────────
abrir <- function() {
  ev("window.__alertas = []; window.alert = function(m){ window.__alertas.push(String(m)); };")
  clic("document.getElementById('pick_abono')")
  espera(function() ev("document.querySelectorAll('.ab-check').length") >= 6, 15)
}
abierto  <- function() isTRUE(ev("!!document.querySelector('.modal.show #ab_table')"))
casilla  <- function(doc) sprintf("document.querySelector('.ab-check[data-documento=\"%s\"]')", doc)
importe  <- function(doc) sprintf("document.querySelector('.ab-amt[data-idx=\"' + %s.dataset.idx + '\"]')", casilla(doc))
fila     <- function(doc) sprintf("%s.closest('tr')", casilla(doc))
grupo    <- "document.querySelector('.ab-group-row')"
visible  <- function(js_el) isTRUE(ev(sprintf("(function(){ var e = %s; return !!e && e.offsetParent !== null; })()", js_el)))
docs_visibles <- function() unlist(ev("Array.from(document.querySelectorAll('.ab-check')).filter(function(c){ return c.offsetParent !== null; }).map(function(c){ return c.dataset.documento; })"))
editable <- function(doc) isTRUE(ev(sprintf("!%s.disabled", importe(doc))))
valor    <- function(doc) ev(sprintf("%s.value", importe(doc)))
avisa    <- function(doc) isTRUE(ev(sprintf("%s.classList.contains('ab-warn')", importe(doc))))
# The value and its input event (typing "" sends no event, so clearing never filtered back).
buscar   <- function(q) { ev(sprintf("(function(){ var e = document.getElementById('ab_search'); e.value = %s;
                            e.dispatchEvent(new Event('input', {bubbles: true})); })()", jsonlite::toJSON(q, auto_unbox = TRUE)))
                          Sys.sleep(0.4) }
enviar   <- function() clic("document.getElementById('ab_stage')")
alertas  <- function() unlist(ev("window.__alertas"))

for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  pantalla(tam[1], tam[2])
  et <- sprintf("[%dx%d]", tam[1], tam[2])
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT)); b$Page$loadEventFired()
  espera(function() ev("!!document.getElementById('pick_abono') && !!window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()"), 20)

  ok(paste(et, "B1 the modal opens with the 6 invoices and one group"), {
    abrir() && abierto() && setequal(docs_visibles(), c("F-101", "F-102", "F-103", "F-201", "F-501", "F-403")) &&
      ev("document.querySelectorAll('.ab-group-row').length") == 1L
  })
  ok(paste(et, "[CAMBIA C1] B2 every amount starts disabled, with the balance in it"), {
    !any(vapply(c("F-101", "F-103", "F-201"), editable, logical(1))) &&
      identical(valor("F-103"), "250") && identical(valor("F-102"), "2500.5")
  })
  ok(paste(et, "[CAMBIA C1] B3 ticking a row enables its amount; unticking disables it again"), {
    clic(casilla("F-201")); a <- editable("F-201")
    clic(casilla("F-201")); a && !editable("F-201")
  })
  ok(paste(et, "B4 an amount over the balance warns (only a warning: 'Mayor al saldo (250)')"), {
    clic(casilla("F-103")); escribe(importe("F-103"), "300")
    r <- avisa("F-103") && grepl("Mayor al saldo (250)", ev(sprintf("%s.parentElement.innerText", importe("F-103"))), fixed = TRUE)
    clic(casilla("F-103"))                              # unticking drops the warning
    r && !avisa("F-103")
  })
  ok(paste(et, "B5 the group's box ticks its 3 invoices; one off leaves it indeterminate"), {
    clic(sprintf("%s.querySelector('.ab-group-check')", grupo))
    todas <- all(vapply(c("F-101", "F-102", "F-103"), function(d) isTRUE(ev(sprintf("%s.checked", casilla(d)))), logical(1)))
    clic(casilla("F-102"))
    ind <- isTRUE(ev(sprintf("%s.querySelector('.ab-group-check').indeterminate", grupo)))
    clic(sprintf("%s.querySelector('.ab-group-check')", grupo)); clic(sprintf("%s.querySelector('.ab-group-check')", grupo))
    todas && ind && !isTRUE(ev(sprintf("%s.checked", casilla("F-101"))))
  })
  ok(paste(et, "B6 the group's arrow and Colapsar hide its invoices; Expandir shows them"), {
    clic(sprintf("%s.querySelector('.ab-expand-btn')", grupo))
    a <- !visible(fila("F-101")) && visible(fila("F-201"))
    clic(sprintf("%s.querySelector('.ab-expand-btn')", grupo))
    clic("document.getElementById('ab_toggle_btn')")
    c1 <- !visible(fila("F-102")) && grepl("Expandir", ev("document.getElementById('ab_toggle_btn').textContent"))
    clic("document.getElementById('ab_toggle_btn')")
    a && c1 && visible(fila("F-102"))
  })
  ok(paste(et, "B7 search: by reference (A-77), by Parte (dos), and back"), {
    buscar("a-77"); s1 <- identical(docs_visibles(), "F-102") && visible(grupo)
    buscar("dos");  s2 <- identical(docs_visibles(), "F-201") && !visible(grupo)
    buscar("");     s1 && s2 && length(docs_visibles()) == 6L
  })
  ok(paste(et, "[CAMBIA C5] B8 Enviar with nothing ticked: a browser alert, nothing sent"), {
    enviar()
    identical(alertas(), "Selecciona al menos una factura.") && length(eco()$guardado) == 0L && abierto()
  })
  ok(paste(et, "B9 Enviar: an edited amount and an untouched one reach the Agenda; the modal closes"), {
    clic(casilla("F-101")); escribe(importe("F-101"), "400")
    clic(casilla("F-201"))
    enviar()
    espera(function() length(eco()$guardado) == 2L) && {
      g <- eco()$guardado
      identical(vapply(g, `[[`, "", "Documento"), c("F-101", "F-201")) &&
        identical(vapply(g, function(x) as.numeric(x$Importe), 0), c(400, 800)) &&
        espera(function() !abierto()) && eco()$cierres == 1L
    }
  })
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
