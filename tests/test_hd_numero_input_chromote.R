# =============================================================================
# tests/test_hd_numero_input_chromote.R
# T66 in the browser: hd_numero_input() on a small in-memory app, at
# 1293x632 (Luis's window). No S3, no login.
#   A  the wheel over the field never changes it and the page scrolls; the
#      arrow keys change nothing; no <input type="number"> on the page.
#   B  what the server gets: "608,763.35" -> 608763.35, "32,000.50", "-5",
#      empty / "abc" / "1,5" -> NA; a monto shows its separator when left;
#      a tasa with too many decimals and an entero with decimals -> NA.
#   C  when it is sent: nothing while typing, on leaving the field, on
#      Enter; the same number is not sent twice; typing and clicking a button
#      straight away, the server already has the new number.
#   D  hd_numero_update(): the same number sends nothing (pagar_hoy's cycle),
#      a new one is sent once, NA empties.
#   E  JS that reads the field with hdNumero.valor(this) on focusout (as
#      cashflow_export does) gets the number, not parseFloat("1,234.5") = 1.
#   F  a field that arrives with dynamic UI (renderUI) is bound too.
# Run from the repo root:  Rscript tests/test_hd_numero_input_chromote.R
# =============================================================================
cat("=== test_hd_numero_input_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()   # never a fixed port
raiz <- normalizePath(".", winslash = "/")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }

APP <- sprintf('
setwd(%s)
library(shiny)
`%%||%%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/ui_components.R")) suppressMessages(source(f))
txt <- function(x) if (is.null(x)) "NULL" else if (length(x) != 1L || is.na(x)) "NA" else format(x, digits = 15)
ui <- fluidPage(
  div(style = "height:250px;"),
  hd_numero_input("m", "Monto", "monto", value = 1234.5, min = 0),
  hd_numero_input("t", "Sobretasa", "tasa", value = 200, decimales = 2),
  hd_numero_input("e", "Plazo", "entero", value = 12),
  actionButton("btn", "Siguiente"), actionButton("upd_igual", "igual"),
  actionButton("upd_nuevo", "nuevo"), actionButton("upd_na", "vaciar"), actionButton("dinamico", "dinamico"),
  uiOutput("zona"),
  pre(id = "eco", textOutput("eco_txt")),
  div(style = "height:3000px;"),
  tags$script(HTML("$(document).on(\'focusout\', \'#m\', function () { window.__leido = hdNumero.valor(this); window.__parse = parseFloat($(this).val()); });"))
)
server <- function(input, output, session) {
  n <- reactiveValues(m = 0L, t = 0L, e = 0L, d = 0L)
  observeEvent(input$m, n$m <- n$m + 1L, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$t, n$t <- n$t + 1L, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$e, n$e <- n$e + 1L, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$d, n$d <- n$d + 1L, ignoreInit = TRUE, ignoreNULL = FALSE)
  al_clic <- reactiveVal("")
  observeEvent(input$btn, al_clic(txt(input$m)))
  observeEvent(input$upd_igual, hd_numero_update(session, "m", value = input$m))
  observeEvent(input$upd_nuevo, hd_numero_update(session, "m", value = 99))
  observeEvent(input$upd_na, hd_numero_update(session, "m", value = NA))
  observeEvent(input$dinamico, output$zona <- renderUI(hd_numero_input("d", "Dinamico", "monto")))
  output$eco_txt <- renderText(jsonlite::toJSON(list(
    m = txt(input$m), t = txt(input$t), e = txt(input$e), d = txt(input$d),
    nm = n$m, nt = n$t, ne = n$e, nd = n$d, clic = al_clic(), clase_m = class(input$m)), auto_unbox = TRUE))
}
shinyApp(ui, server)
', deparse(raiz))

tryCatch({
  app <- start_bg_app(APP, PORT, timeout_s = 40)
  b <- new_chromote_session()
  b$Emulation$setDeviceMetricsOverride(width = 1293, height = 632, deviceScaleFactor = 1, mobile = FALSE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT)); Sys.sleep(1)
  ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
  espera <- function(js, s = 15) isTRUE(tryCatch({ poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
    timeout_s = s, interval_s = 0.25); TRUE }, error = function(e) FALSE))
  eco <- function() {   # the server's view; empty (or half drawn) -> list()
    t <- trimws(ev("(document.getElementById('eco_txt') || {}).textContent || ''"))
    if (nzchar(t)) tryCatch(jsonlite::fromJSON(t), error = function(e) list()) else list()
  }
  eco_es <- function(campo, valor, s = 8) isTRUE(poll_until(function() identical(eco()[[campo]], valor), timeout_s = s, interval_s = 0.2))
  centro <- function(sel) jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s);
    var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  clic <- function(sel) {
    p <- centro(sel)
    for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
    Sys.sleep(0.3)
  }
  enfoca <- function(sel) ev(sprintf("(function(){ var e = document.querySelector(%s); e.focus(); e.select(); return true; })()",
                                     jsonlite::toJSON(sel, auto_unbox = TRUE)))
  sal <- function(sel) ev(sprintf("(function(){ var e = document.querySelector(%s); e.blur(); return e.value; })()",
                                  jsonlite::toJSON(sel, auto_unbox = TRUE)))
  teclea <- function(sel, texto) { enfoca(sel); if (nzchar(texto)) b$Input$insertText(text = texto) else
    b$Input$dispatchKeyEvent(type = "keyDown", key = "Delete", code = "Delete", windowsVirtualKeyCode = 46); Sys.sleep(0.3); sal(sel) }
  tecla <- function(key, code, vk) for (t in c("keyDown", "keyUp")) b$Input$dispatchKeyEvent(type = t, key = key, code = code, windowsVirtualKeyCode = vk)

  ok("the app is up, with no login", espera("!!document.getElementById('eco') && document.getElementById('eco').textContent.length > 2", 30) &&
       !isTRUE(ev("!!document.querySelector('input[type=password]')")))
  ok("the initial values reach the server as numbers", eco_es("m", "1234.5") && identical(eco()$t, "200") && identical(eco()$e, "12") &&
       identical(eco()$clase_m, "numeric"))
  ok("the monto shows its separator from the start", identical(ev("document.getElementById('m').value"), "1,234.50"))

  # ── A: the wheel and the arrows ─────────────────────────────────────────────
  ok("no <input type=number> on the page", identical(ev("document.querySelectorAll('input[type=number]').length"), 0L))
  ev("window.scrollTo(0, 0)"); Sys.sleep(0.2)
  y0 <- ev("window.scrollY"); p <- centro("#t")
  enfoca("#t")   # focused, as when Luis has just typed in it
  b$Input$dispatchMouseEvent(type = "mouseWheel", x = p[1], y = p[2], deltaX = 0, deltaY = 300); Sys.sleep(0.6)
  ok("the wheel over the field: the value does not change", identical(ev("document.getElementById('t').value"), "200") && identical(eco()$t, "200"))
  ok("the wheel over the field: the page scrolls down", isTRUE(ev("window.scrollY") > y0))
  ev("window.scrollTo(0, 0)"); enfoca("#t")
  tecla("ArrowUp", "ArrowUp", 38); tecla("ArrowDown", "ArrowDown", 40); tecla("ArrowUp", "ArrowUp", 38); Sys.sleep(0.3); sal("#t")
  ok("the arrow keys change nothing", identical(ev("document.getElementById('t').value"), "200") && identical(eco()$nt, 0L))

  # ── B: what the server gets ─────────────────────────────────────────────────
  v <- teclea("#m", "608,763.35")
  ok("'608,763.35' -> 608763.35, and the field keeps '608,763.35'", eco_es("m", "608763.35") && identical(v, "608,763.35"))
  v <- teclea("#m", "608763.35")
  ok("'608763.35' -> shows '608,763.35' when left", identical(v, "608,763.35"))
  teclea("#m", "32,000.50"); ok("'32,000.50' -> 32000.5", eco_es("m", "32000.5"))
  teclea("#m", "-5");        ok("'-5' -> -5 (the server validates the range)", eco_es("m", "-5"))
  teclea("#m", "");          ok("empty -> NA", eco_es("m", "NA"))
  teclea("#m", "7");         eco_es("m", "7")
  v <- teclea("#m", "abc");  ok("'abc' -> NA, and the text stays for the person to see", eco_es("m", "NA") && identical(v, "abc"))
  teclea("#m", "7");         eco_es("m", "7")
  teclea("#m", "1,5");       ok("'1,5' -> NA (never 15)", eco_es("m", "NA"))
  teclea("#m", "1.005");     ok("a monto rounds to cents: '1.005' -> 1.01, shown '1.01'", eco_es("m", "1.01") && identical(ev("document.getElementById('m').value"), "1.01"))
  teclea("#t", "0.125");     ok("a tasa with more decimals than it takes -> NA", eco_es("t", "NA"))
  v <- teclea("#t", "12.5"); ok("a tasa keeps what was typed: '12.5'", eco_es("t", "12.5") && identical(v, "12.5"))
  teclea("#e", "5.5");       ok("an entero with decimals -> NA", eco_es("e", "NA"))
  teclea("#e", "45");        ok("'45' -> 45", eco_es("e", "45"))

  # ── C: when it is sent ──────────────────────────────────────────────────────
  teclea("#m", "100"); eco_es("m", "100"); n0 <- eco()$nm
  enfoca("#m"); b$Input$insertText(text = "250"); Sys.sleep(1.2)
  ok("nothing is sent while typing", identical(eco()$m, "100") && identical(eco()$nm, n0))
  tecla("Enter", "Enter", 13)
  ok("Enter sends it", eco_es("m", "250") && identical(eco()$nm, n0 + 1L))
  sal("#m"); Sys.sleep(0.6)
  ok("leaving after Enter does not send the same number again", identical(eco()$nm, n0 + 1L))
  teclea("#m", "250.00"); Sys.sleep(0.6)
  ok("retyping the same number sends nothing", identical(eco()$nm, n0 + 1L))
  enfoca("#m"); b$Input$insertText(text = "777"); Sys.sleep(0.2)
  clic("#btn")
  ok("typing and clicking a button straight away: the server has the new number", eco_es("clic", "777"))

  # ── D: hd_numero_update() ───────────────────────────────────────────────────
  n1 <- eco()$nm
  clic("#upd_igual"); Sys.sleep(1)
  ok("update with the same number: no change reaches the server", identical(eco()$nm, n1) && identical(eco()$m, "777"))
  clic("#upd_nuevo")
  ok("update with a new number: sent once, shown as a monto", eco_es("m", "99") && identical(eco()$nm, n1 + 1L) &&
       identical(ev("document.getElementById('m').value"), "99.00"))
  clic("#upd_na")
  ok("update with NA: empty field, NA on the server", eco_es("m", "NA") && identical(ev("document.getElementById('m').value"), ""))

  # ── E: JS that reads the field ──────────────────────────────────────────────
  teclea("#m", "1,234.5"); Sys.sleep(0.3)
  ok("hdNumero.valor(this) on focusout gets 1234.5 (parseFloat of the text would give 1)",
     isTRUE(ev("window.__leido") == 1234.5) && isTRUE(ev("window.__parse") == 1))

  # ── F: dynamic UI ───────────────────────────────────────────────────────────
  clic("#dinamico")
  ok("a field from renderUI appears", espera("!!document.getElementById('d')"))
  Sys.sleep(0.5); teclea("#d", "2,500")
  ok("and it is bound: '2,500' -> 2500", eco_es("d", "2500") && identical(ev("document.getElementById('d').value"), "2,500.00"))
}, error = function(e) { cat(" ERROR:", conditionMessage(e), "\n"); .fail <<- .fail + 1L }, finally = limpiar())

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
