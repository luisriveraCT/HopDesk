# =============================================================================
# tests/test_hd_table_filtros_chromote.R
# T03b pieces of the hd_table engine in a real headless Chrome, against the
# demo app (dev/hd_table_demo: Bootstrap 5 flatly like HopDesk, generated
# data, no S3), at 1400x1080. Every click and key is a real input event
# (Input.dispatchMouseEvent / dispatchKeyEvent).
#
# Run from project root:  Rscript tests/test_hd_table_filtros_chromote.R
# =============================================================================
cat("=== test_hd_table_filtros_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38962L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("dev/hd_table_demo/app.R")$value', repo),
                             PORT, timeout_s = 40),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("demo app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
pantalla(1400L, 1080L)
base <- sprintf("http://127.0.0.1:%d", PORT)
invisible(try(b$Browser$grantPermissions(permissions = list("clipboardReadWrite", "clipboardSanitizedWrite"), origin = base),
    silent = TRUE))

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "a1-hdt"
abrir <- function(query) {
  b$Page$navigate(paste0(base, "/", query), wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", A))), timeout_s = 30)
  Sys.sleep(0.6)   # auto height settles on the next frame
  ok
}
estado <- function(id = A) ev(sprintf("HDT.estado('%s')", id))
vis <- function() unlist(estado()$visibles)
# behavior 'instant': Bootstrap 5 sets scroll-behavior: smooth on :root.
centro <- function(js_el, ver = TRUE) ev(sprintf("(function(){ var e = %s; %s
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el,
  if (ver) "e.scrollIntoView({block:'center', behavior:'instant'});" else ""))
raton <- function(tipo, p, botones = 0L, n = 1L, shift = FALSE)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n, modifiers = if (shift) 8L else 0L)
clic <- function(js_el, ver = TRUE) {
  p <- centro(js_el, ver)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.25)
}
clic_en <- function(x, y) { p <- list(x, y); raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.25) }
tecla <- function(key, code = key, vk = 0L) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = key, code = code, windowsVirtualKeyCode = vk)
  b$Input$dispatchKeyEvent(type = "keyUp", key = key, code = code, windowsVirtualKeyCode = vk)
  Sys.sleep(0.2)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.4)
}
# Every visible row's cell text in column j (0-based, full mode).
columna <- function(j) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr:not(.hdt-esp)')).map(function(r){ return r.children[%d].lastChild.textContent; })", A, j)))
COL_EMPRESA <- 0L; COL_MONEDA <- 4L; COL_ESTADO <- 6L

boton_filtro <- function(k) sprintf("document.querySelectorAll('#%s .hdt-filtro')[%d]", A, k)
textos_filtros <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-filtro')).map(function(b){ return b.textContent; })", A)))
limpiar_visible <- function() isTRUE(ev(sprintf("(function(){ var b = document.querySelector('#%s .hdt-limpiar'); return !!b && !b.hidden && b.offsetParent !== null; })()", A)))
menu_abierto <- function() isTRUE(ev("!!document.querySelector('.hdt-menu')"))
# Options of the open menu: value -> count.
menu_ops <- function() {
  x <- unlist(ev("Array.from(document.querySelectorAll('.hdt-menu .hdt-menu-op')).map(function(l){ return l.querySelector('input').dataset.valor + '=' + l.querySelector('.hdt-menu-n').textContent; })"))
  setNames(as.integer(gsub(",", "", sub(".*=", "", x))), sub("=.*", "", x))
}
menu_op <- function(valor) sprintf("document.querySelector('.hdt-menu input[data-valor=\"%s\"]')", valor)
dentro <- function(js_el) isTRUE(ev(sprintf("(function(){ var r = %s.getBoundingClientRect(); return r.left >= 0 && r.top >= 0 && r.right <= innerWidth && r.bottom <= innerHeight; })()", js_el)))

ok("demo loads (300 rows, Bootstrap 5)", abrir("?n=300&seed=1"))
vis0 <- vis()
moneda0 <- columna(COL_MONEDA); empresa0 <- columna(COL_EMPRESA)

# ── Filters (filtros) ──────────────────────────────────────────────────────
ok("two filter buttons in the bar, in Spanish, all off; no 'Limpiar filtros'",
   identical(textos_filtros(), c("Moneda: Todas", "Empresa: Todas")) && !limpiar_visible())

clic(boton_filtro(0))
ops <- menu_ops()
cat(sprintf("  Moneda menu: %s\n", paste(names(ops), ops, collapse = ", ")))
ok("clicking a filter opens its menu: the values in the data with their counts",
   menu_abierto() && setequal(names(ops), unique(moneda0)) &&
     all(vapply(names(ops), function(v) ops[[v]] == sum(moneda0 == v), logical(1))))
ok("... and the menu is inside the window", dentro("document.querySelector('.hdt-menu')"))

clic(menu_op("USD"), ver = FALSE)
ok("one option filters to it: only USD rows, the button says so, 'Limpiar filtros' shows",
   length(vis()) == ops[["USD"]] && all(columna(COL_MONEDA) == "USD") &&
     identical(textos_filtros()[1], "Moneda: USD") && limpiar_visible())
ok("... and the menu stays open to pick more", menu_abierto())
clic(menu_op("MXN"), ver = FALSE)
ok("two options of one filter combine with OR (USD or MXN: every row)",
   length(vis()) == 300L && identical(textos_filtros()[1], "Moneda: 2 valores"))
clic(menu_op("MXN"), ver = FALSE)
ok("unchecking one goes back to USD only", length(vis()) == ops[["USD"]])

tecla("Escape", "Escape", 27L)
ok("Esc closes the menu and gives the focus back to its button",
   !menu_abierto() && isTRUE(ev(sprintf("document.activeElement === %s", boton_filtro(0)))))
ok("... and the filter stays on", length(vis()) == ops[["USD"]])

clic(boton_filtro(1))
ops_emp <- menu_ops()
usd <- moneda0 == "USD"
ok("the other filter's menu counts what the first one leaves (Empresa among USD rows)",
   all(vapply(names(ops_emp), function(v) ops_emp[[v]] == sum(usd & empresa0 == v), logical(1))))
clic(menu_op("NTS"), ver = FALSE)
esperado <- vis0[usd & empresa0 == "NTS"]
ok("two filters combine with AND (USD and NTS)",
   setequal(vis(), esperado) && all(columna(COL_MONEDA) == "USD") && all(columna(COL_EMPRESA) == "NTS"))

clic_en(1300, 1000)
ok("a click outside closes the menu", !menu_abierto())

# With the search and the chips.
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "aceros")
con_busq <- vis()
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
con_todo <- vis()
estados <- columna(COL_ESTADO)
cat(sprintf("  USD+NTS: %d rows; + search 'aceros': %d; + chip pendiente: %d\n", length(esperado), length(con_busq), length(con_todo)))
ok("filters combine with the search and the chip (every row passes all of them)",
   all(con_todo %in% con_busq) && all(con_busq %in% esperado) && all(estados == "Pendiente") &&
     all(columna(COL_MONEDA) == "USD") && all(columna(COL_EMPRESA) == "NTS"))
cifra_pend <- as.integer(ev(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente] .hdt-ficha-cifra').textContent", A)))
ok("the chips count what the search and the filters leave", cifra_pend == length(con_todo))
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
escribir(sprintf("document.querySelector('#%s .hdt-buscar')", A), "")

clic(sprintf("document.querySelector('#%s .hdt-limpiar')", A))
ok("'Limpiar filtros' turns every filter off and hides itself",
   length(vis()) == 300L && identical(textos_filtros(), c("Moneda: Todas", "Empresa: Todas")) && !limpiar_visible())

# New data: the filter stays; a chosen value that disappears drops out.
clic(boton_filtro(0)); clic(menu_op("USD"), ver = FALSE); tecla("Escape", "Escape", 27L)
clic(boton_filtro(1)); clic(menu_op("NCS"), ver = FALSE); tecla("Escape", "Escape", 27L)
antes <- vis()
clic("document.getElementById('cambiar')")
Sys.sleep(0.8)
ok("filters survive new data (same rows shown, same filters on)",
   identical(vis(), antes) && identical(unlist(estado()$filtros$Moneda), "USD") &&
     identical(textos_filtros(), c("Moneda: USD", "Empresa: NCS")))
clic("document.getElementById('solo_mxn')")
Sys.sleep(0.8)
f <- estado()$filtros
ok("a chosen value that's no longer in the data drops out of its filter (USD gone, NCS stays)",
   is.null(f$Moneda) && identical(unlist(f$Empresa), "NCS") &&
     identical(textos_filtros(), c("Moneda: Todas", "Empresa: NCS")) &&
     all(columna(COL_MONEDA) == "MXN") && all(columna(COL_EMPRESA) == "NCS"))
clic("document.getElementById('solo_mxn')")
Sys.sleep(0.8)
clic(sprintf("document.querySelector('#%s .hdt-limpiar')", A))

# The menu never leaves the window: a short window with the button near
# its bottom edge opens it above the button. (Room above the table, so the
# page can scroll the button down to that edge.)
pantalla(1400L, 420L)
invisible(ev("document.body.style.paddingTop = '1200px'"))
Sys.sleep(0.3)
invisible(ev(sprintf("(function(){ var r = %s.getBoundingClientRect(); window.scrollBy({top: r.bottom - innerHeight + 12, behavior: 'instant'}); })()", boton_filtro(1))))
Sys.sleep(0.2)
clic(boton_filtro(1), ver = FALSE)
abajo <- ev(sprintf("%s.getBoundingClientRect().top", boton_filtro(1)))
arriba_menu <- ev("(document.querySelector('.hdt-menu') || {getBoundingClientRect: function(){ return {bottom: NaN}; }}).getBoundingClientRect().bottom")
cat(sprintf("  window 420 px high: button top at %.0f, menu bottom at %.0f
", abajo, arriba_menu))
ok("near the bottom edge the menu opens above its button and stays inside the window",
   dentro("document.querySelector('.hdt-menu')") && arriba_menu <= abajo)
tecla("Escape", "Escape", 27L)
invisible(ev("document.body.style.paddingTop = ''"))
pantalla(1400L, 1080L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
