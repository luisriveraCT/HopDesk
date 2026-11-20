# =============================================================================
# tests/test_t60_agenda_panorama_chromote.R
# T60 in the browser: the real app against dev, as a client, at 1293x632.
# Panorama's three figures from the Agenda equal their twin there, the
# Agenda's "↓ Pagos" (Posición neta), currency by currency:
#   - the Hoy card "Programado para pagar hoy" (its big figure + "+ …" lines);
#   - the Cinta's "Programado para pagar …" (its first two currencies);
#   - the Clima says payments exist exactly when the Agenda has some.
# Reads only: nothing is staged or written. Fails if the Agenda is empty (it
# would prove nothing). Credentials in .Renviron.local, never printed.
# Run from project root:  Rscript tests/test_t60_agenda_panorama_chromote.R
# =============================================================================
cat("=== test_t60_agenda_panorama_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
renv <- Sys.getenv("T60_RENVIRON", ".Renviron.local"); if (file.exists(renv)) readRenviron(renv)
if (!nzchar(Sys.getenv("HOPDESK_TEST_CLIENTE_USER"))) { cat("SKIP: no test credentials\n"); quit(status = 0L) }
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port: an earlier app still alive there would be the one tested
repo <- normalizePath(".", winslash = "/")
f <- file.path(repo, ".t60_app.R")
writeLines(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value', repo, repo), f)
proc <- callr::r_bg(function(app_file, port) shiny::runApp(app_file, port = port, host = "127.0.0.1", launch.browser = FALSE),
                    args = list(app_file = f, port = PORT), stdout = tempfile(), stderr = tempfile(),
                    env = c(callr::rcmd_safe_env(), Sys.getenv()))
b <- NULL
# Rscript never runs a top-level on.exit(): the cleanup is called by hand.
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(proc$kill(), silent = TRUE); unlink(f) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 120) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 1); TRUE }, error = function(e) FALSE))
mostrar <- function(v) ev(sprintf("(function(){ var a = document.querySelector(\"nav.navbar a[data-value='%s']\"); if (!a) return false;
  if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()", v))
# "1,234.50 MXN" pieces of a text -> named numbers by currency.
montos <- function(txt) {
  m <- regmatches(txt, gregexpr("[−-]?[0-9][0-9,]*(\\.[0-9]+)? [A-Z]{3}", txt))[[1]]
  if (!length(m)) return(setNames(numeric(0), character(0)))
  v <- as.numeric(gsub("[^0-9.]", "", sub(" [A-Z]{3}$", "", m))) * ifelse(grepl("^[−-]", m), -1, 1)
  setNames(v, sub("^.* ", "", m))
}
mismo <- function(a, b) length(a) == length(b) && setequal(names(a), names(b)) &&
  isTRUE(all.equal(unname(a[sort(names(a))]), unname(b[sort(names(b))])))

tryCatch({
  poll_until(function() !inherits(tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", PORT),
    handle = curl::new_handle(TIMEOUT = 2)), error = function(e) e), "error"), timeout_s = 300, interval_s = 1)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  # Navigate without blocking on the load event (a busy machine makes the first
  # load slow and Page.navigate times out); then wait for the login by condition.
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = FALSE))
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)),
             timeout_s = 180, interval_s = 1)
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), auto_unbox = TRUE), jsonlite::toJSON(Sys.getenv("HOPDESK_TEST_CLIENTE_PASS"), auto_unbox = TRUE)))
  Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(function() isTRUE(tryCatch(ev("(function(){ var u = document.getElementById('navbar_user_badge');
    return !!u && u.textContent.trim().length > 0; })()"), error = function(e) FALSE)), timeout_s = 300, interval_s = 2)
  Sys.sleep(5)

  # The Agenda's twin: the "↓" pills of its Posición neta.
  mostrar("PH")
  espera("(function(){ var g = document.getElementById('ph-global_total_ui'); return !!g && g.textContent.trim().length > 0; })()")
  Sys.sleep(4)
  ag_txt <- ev("(function(){ var g = document.getElementById('ph-global_total_ui'); if (!g) return '';
    return [].filter.call(g.querySelectorAll('.badge'), function(x){ return x.textContent.indexOf('\\u2193') >= 0; })
      .map(function(x){ return x.textContent; }).join(' | '); })()")
  ag <- montos(ag_txt)
  cat(" Agenda ↓ Pagos:", ag_txt, "\n")
  ok("the Agenda holds payments (otherwise this proves nothing)", length(ag) > 0L)

  mostrar("PAN")
  espera("!!document.querySelector('.tab-pane.active .hd-panorama .pan-hoy-tarjeta')")
  Sys.sleep(4)
  card_txt <- ev("(function(){ var c = [].filter.call(document.querySelectorAll('.tab-pane.active .pan-hoy-tarjeta'),
      function(x){ return (x.getAttribute('onclick') || '').indexOf(\"'PH'\") >= 0; })[0];
    if (!c) return '';
    var t = [c.querySelector('.pan-cifra')].concat([].slice.call(c.querySelectorAll('.pan-cifra-otras div')));
    return t.filter(Boolean).map(function(x){ return x.textContent; }).join(' | '); })()")
  cat(" Panorama card:", card_txt, "\n")
  ok("Hoy card 'Programado para pagar hoy' = the Agenda's '↓ Pagos', currency by currency", mismo(montos(card_txt), ag))
  extra <- ev("(function(){ var c = [].filter.call(document.querySelectorAll('.tab-pane.active .pan-hoy-tarjeta'),
      function(x){ return (x.getAttribute('onclick') || '').indexOf(\"'PH'\") >= 0; })[0];
    var e = c && c.querySelector('.pan-explica--extra'); return e ? e.textContent : ''; })()")
  cat(" its breakdown:", extra, "\n")
  ok("…its breakdown line, when there is one, reads 'Incluye …' with amounts by currency, no '$'",
     !nzchar(extra) || (startsWith(trimws(extra), "Incluye") && length(montos(extra)) > 0L && !grepl("$", extra, fixed = TRUE)))
  cinta <- ev("(function(){ var c = document.querySelector('.tab-pane.active .pan-cinta'); return c ? c.textContent : ''; })()")
  if (nzchar(cinta)) {
    m <- regmatches(cinta, regexpr("Programado para pagar [^·|]*", cinta))
    ci <- montos(m)
    ok("Cinta 'Programado para pagar …' = the Agenda's first currencies", length(ci) > 0L &&
         all(names(ci) %in% names(ag)) && isTRUE(all.equal(unname(ci), unname(ag[names(ci)]))))
  } else cat(" (no Cinta on dev now: not checked)\n")
  clima <- ev("(function(){ var c = document.querySelector('.tab-pane.active #pan-clima, .tab-pane.active .pan-clima');
    return c ? c.textContent : ''; })()")
  ok("Clima names payments exactly when the Agenda has some", identical(grepl("[0-9]+ pagos?\\b", clima), length(ag) > 0L))
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
