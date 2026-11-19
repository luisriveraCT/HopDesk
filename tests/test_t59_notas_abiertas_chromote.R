# =============================================================================
# tests/test_t59_notas_abiertas_chromote.R
# T59 (2): "Notas del mes" looks open while its panel is open, on both
# buttons (the floating one and the Calendar header's, T56), with CSS only:
# body:has(#notes_panel .notes-panel-wrapper). The old JS marked ids that no
# longer exist (#toggle_notes_ar/_ap), so nothing was ever marked; it is gone.
# Static part first; then the real app against dev, as staff, at 1293x632.
#
# Run from project root:  Rscript tests/test_t59_notas_abiertas_chromote.R
# =============================================================================
cat("=== test_t59_notas_abiertas_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
ui <- paste(readLines("R/ui_components.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("static: the dead JS for #toggle_notes_ar/_ap is gone", !grepl("toggle_notes_ar", ui, fixed = TRUE))
ok("static: one rule marks both buttons open while #notes_panel holds the panel",
   grepl("body:has(#notes_panel .notes-panel-wrapper) #toggle_notes,", ui, fixed = TRUE) &&
     grepl("body:has(#notes_panel .notes-panel-wrapper) .cal-notes-btn {", ui, fixed = TRUE))
ok("static: no class-based .notes-open rule is left in ui_components.R", !grepl(".notes-bar-btn.notes-open", ui, fixed = TRUE))

renv <- Sys.getenv("T59_RENVIRON", ".Renviron.local"); if (file.exists(renv)) readRenviron(renv)
if (!nzchar(Sys.getenv("HOPDESK_TEST_USER"))) {
  cat("SKIP (browser part): no test credentials in .Renviron.local\n")
  cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail)); quit(status = as.integer(.fail > 0L))
}
source("tests/helpers/live_app_harness.R")
PORT <- 39064L
repo <- normalizePath(".", winslash = "/")
f <- file.path(repo, ".t59b_app.R")
writeLines(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value', repo, repo), f)
proc <- callr::r_bg(function(app_file, port) shiny::runApp(app_file, port = port, host = "127.0.0.1", launch.browser = FALSE),
                    args = list(app_file = f, port = PORT), stdout = tempfile(), stderr = tempfile(),
                    env = c(callr::rcmd_safe_env(), Sys.getenv()))
b <- NULL
# Rscript never runs a top-level on.exit(): the cleanup is called by hand.
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(proc$kill(), silent = TRUE); unlink(f) }
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.5); TRUE }, error = function(e) FALSE))
clic <- function(sel) {   # a real mouse click at the element's centre
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased"))
    b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  TRUE
}
mostrar <- function(v) ev(sprintf("(function(){ var a = document.querySelector(\"nav.navbar a[data-value='%s']\"); if (!a) return false;
  if (window.jQuery) jQuery(a).tab('show'); a.click(); return true; })()", v))
fondo <- function(sel) ev(sprintf("(function(){ var e = document.querySelector(%s); return e ? getComputedStyle(e).backgroundColor : null; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
ABIERTO <- "rgb(74, 111, 165)"   # #4a6fa5
PANEL <- "!!document.querySelector('#notes_panel .notes-panel-wrapper')"
HEADER <- ".tab-pane.active .cal-title-bar .cal-notes-btn"

tryCatch({
  poll_until(function() !inherits(tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", PORT),
    handle = curl::new_handle(TIMEOUT = 2)), error = function(e) e), "error"), timeout_s = 300, interval_s = 1)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60)
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(Sys.getenv("HOPDESK_TEST_USER"), auto_unbox = TRUE), jsonlite::toJSON(Sys.getenv("HOPDESK_TEST_PASS"), auto_unbox = TRUE)))
  Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(function() isTRUE(tryCatch(ev("(function(){ var u = document.getElementById('navbar_user_badge');
    return !!u && u.textContent.trim().length > 0; })()"), error = function(e) FALSE)), timeout_s = 300, interval_s = 2)
  Sys.sleep(5)

  mostrar("VEN"); Sys.sleep(3)
  ok("Vencidos, panel closed: the floating button does not look open", !identical(fondo("#toggle_notes"), ABIERTO) && !isTRUE(ev(PANEL)))
  clic("#toggle_notes")
  ok("the floating button opens the panel…", espera(PANEL))
  ok("…and looks open", espera(sprintf("getComputedStyle(document.getElementById('toggle_notes')).backgroundColor === '%s'", ABIERTO)))
  mostrar("CAL")
  ok("Calendar, panel still open: the header's button looks open too",
     espera(sprintf("(function(){ var e = document.querySelector('%s'); return !!e && getComputedStyle(e).backgroundColor === '%s'; })()", HEADER, ABIERTO)))
  clic(HEADER)
  ok("the header's button closes the panel…", espera(sprintf("!%s", PANEL)))
  ok("…and neither looks open any more",
     espera(sprintf("(function(){ var e = document.querySelector('%s'); return !!e && getComputedStyle(e).backgroundColor !== '%s' &&
       getComputedStyle(document.getElementById('toggle_notes')).backgroundColor !== '%s'; })()", HEADER, ABIERTO, ABIERTO)))
  # Panorama with its Cinta (T49 gives the button an opaque face there): open still wins.
  mostrar("PAN"); Sys.sleep(4)
  if (isTRUE(ev("!!document.querySelector('.tab-pane.active .hd-panorama .pan-cinta')"))) {
    clic("#toggle_notes")
    ok("Panorama with its Cinta: open, the button looks open over T49's opaque face",
       espera(PANEL) && espera(sprintf("getComputedStyle(document.getElementById('toggle_notes')).backgroundColor === '%s'", ABIERTO)))
    clic("#toggle_notes"); espera(sprintf("!%s", PANEL))
  } else cat(" (Panorama shows no Cinta on dev now: that case is not checked)\n")
}, error = function(e) ok(paste("no error running the checks:", conditionMessage(e)), FALSE))
limpiar()

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
