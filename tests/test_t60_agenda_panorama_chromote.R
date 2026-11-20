# =============================================================================
# tests/test_t60_agenda_panorama_chromote.R
# T60 in the browser, over ONE in-memory Agenda shown by the real Agenda de
# Hoy and the real Panorama (tests/helpers/agenda_panorama_fixture_app.R): no
# S3, nothing written to dev. At 1293x632:
#   - the Hoy card "Programado para pagar hoy" = the Agenda's "↓ Pagos",
#     currency by currency, and both = the known pending AP rows of every
#     origin (ERP, manual, Pasivos, Requisiciones), overdue, today and ahead;
#   - under the card, "Incluye 7 vencidos (…) · 4 anticipados (…)", by currency;
#   - the Cinta's "Programado para pagar …" and the Clima's "N pagos" agree.
# Run from project root:  Rscript tests/test_t60_agenda_panorama_chromote.R
# =============================================================================
cat("=== test_t60_agenda_panorama_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port: an earlier app still alive there would be the one tested
raiz <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
# Rscript never runs a top-level on.exit(): the cleanup is called by hand.
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 120) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 1); TRUE }, error = function(e) FALSE))
mostrar <- function(v) ev(sprintf("(function(){ var a = document.querySelector(\"nav.navbar a[data-value='%s']\"); if (!a) return false;
  if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()", v))
# "1,234.50 MXN" pieces of a text -> numbers named by currency.
montos <- function(txt) {
  m <- regmatches(txt, gregexpr("[−-]?[0-9][0-9,]*(\\.[0-9]+)? [A-Z]{3}", txt))[[1]]
  if (!length(m)) return(setNames(numeric(0), character(0)))
  v <- as.numeric(gsub("[^0-9.]", "", sub(" [A-Z]{3}$", "", m))) * ifelse(grepl("^[−-]", m), -1, 1)
  setNames(v, sub("^.* ", "", m))
}
mismo <- function(a, b) length(a) == length(b) && setequal(names(a), names(b)) &&
  isTRUE(all.equal(unname(a[sort(names(a))]), unname(b[sort(names(b))])))
# The fixture's pending AP rows (agenda0 + the T60 rows; the confirmed one out):
ESPERADO <- c(MXN = 15000 + 8200 + 3100 + 400 + 700 + 1000 + 250 + 640 + 2000, USD = 1200 + 300 + 75)

tryCatch({
  app <- start_bg_app(sprintf('setwd("%s"); source("tests/helpers/agenda_panorama_fixture_app.R")$value', raiz),
                      PORT, timeout_s = 240)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  # Navigate without blocking on the load event (a busy machine makes it slow); wait by condition.
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  ok("the fixture app opens (Agenda and Panorama tabs)", espera("!!document.querySelector(\"nav.navbar a[data-value='PH']\")", 180))

  mostrar("PH")
  espera("(function(){ var g = document.getElementById('ph-global_total_ui'); return !!g && g.textContent.indexOf('\\u2193') >= 0; })()")
  Sys.sleep(2)
  ag_txt <- ev("(function(){ var g = document.getElementById('ph-global_total_ui'); if (!g) return '';
    return [].filter.call(g.querySelectorAll('.badge'), function(x){ return x.textContent.indexOf('\\u2193') >= 0; })
      .map(function(x){ return x.textContent; }).join(' | '); })()")
  ag <- montos(ag_txt)
  cat(" Agenda ↓ Pagos:", ag_txt, "\n")
  ok("the Agenda's '↓ Pagos' = the pending AP rows of every origin and due date (31,290.00 MXN · 1,575.00 USD)", mismo(ag, ESPERADO))

  mostrar("PAN")
  espera("!!document.querySelector('.tab-pane.active .pan-hoy-tarjeta')")
  Sys.sleep(3)
  tarjeta <- "[].filter.call(document.querySelectorAll('.tab-pane.active .pan-hoy-tarjeta'), function(x){ return (x.getAttribute('onclick') || '').indexOf(\"'PH'\") >= 0; })[0]"
  card_txt <- ev(sprintf("(function(){ var c = %s; if (!c) return '';
    var t = [c.querySelector('.pan-cifra')].concat([].slice.call(c.querySelectorAll('.pan-cifra-otras div')));
    return t.filter(Boolean).map(function(x){ return x.textContent; }).join(' | '); })()", tarjeta))
  cat(" Panorama card:", card_txt, "\n")
  ok("Hoy card 'Programado para pagar hoy' = the Agenda's '↓ Pagos', currency by currency", mismo(montos(card_txt), ag))
  ok("…including Luis's case: the Requisiciones item due in two days is in it (2,000.00 of the MXN total)",
     isTRUE(all.equal(unname(montos(card_txt)["MXN"]), ESPERADO[["MXN"]])))
  extra <- ev(sprintf("(function(){ var c = %s; var e = c && c.querySelector('.pan-explica--extra'); return e ? e.textContent : ''; })()", tarjeta))
  cat(" its breakdown:", extra, "\n")
  ok("the breakdown under it: 'Incluye 7 vencidos (…) · 4 anticipados (…)'",
     grepl("^\\s*Incluye 7 vencidos \\(", extra) && grepl("· 4 anticipados \\(", extra))
  ok("…amounts by currency, never added up, no '$': anticipados = 2,890.00 MXN and 75.00 USD",
     grepl("4 anticipados (2,890.00 MXN · 75.00 USD)", extra, fixed = TRUE) && !grepl("$", extra, fixed = TRUE))
  clima <- ev("(function(){ var c = document.querySelector('.tab-pane.active .pan-clima'); return c ? c.textContent : ''; })()")
  ok("Clima: '12 pagos' (the Agenda's 12 pending AP rows)", grepl("12 pagos", clima, fixed = TRUE))
  cinta <- ev("(function(){ var c = document.querySelector('.tab-pane.active .pan-cinta'); return c ? c.textContent : ''; })()")
  # The Cinta repeats its track (the scrolling ribbon): only the first
  # "Programado para pagar <amount CODE>[, <amount CODE>]".
  m <- regmatches(cinta, regexpr("Programado para pagar [0-9][0-9,.]* [A-Z]{3}(, [0-9][0-9,.]* [A-Z]{3})?", cinta))
  cat(" Cinta:", substr(gsub("\\s+", " ", cinta), 1, 300), "\n")
  ok("Cinta 'Programado para pagar …' = the Agenda's figures", length(m) == 1L && mismo(montos(m), ag[names(montos(m))]) &&
       length(montos(m)) == 2L)
  if (nzchar(Sys.getenv("T60_FOTO"))) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), Sys.getenv("T60_FOTO"))
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
