# =============================================================================
# tests/test_hd_edicion_celda_chromote.R
# The piece edicion_celda (T14) in a real headless Chrome, against
# tests/helpers/edicion_celda_fixture_app.R (a table shaped like Abono
# parcial; no S3). Every click and key is a real input event
# (Input.dispatchMouseEvent / dispatchKeyEvent / insertText). What the
# server got is read from the app's #eco (t$ediciones(), t$seleccion()).
# Sizes: Luis's real window (1293 x 632) and 1772 x 866; screenshots go to
# EC_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_hd_edicion_celda_chromote.R
# =============================================================================
cat("=== test_hd_edicion_celda_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38977L
CAPTURAS <- Sys.getenv("EC_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

arrancar <- function(n_extra = 0L) {
  stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(EC_N = "%d"); setwd("%s"); source("tests/helpers/edicion_celda_fixture_app.R")$value',
                                        n_extra, repo), PORT, timeout_s = 90),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
ok("fixture app started", arrancar())
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
base <- sprintf("http://127.0.0.1:%d", PORT)
ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "t-hdt"
abrir <- function(tabla = A) {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", tabla))), timeout_s = 60)
  Sys.sleep(0.6)
  ok
}
estado <- function(id = A) ev(sprintf("HDT.estado('%s')", id))
ediciones_js <- function(id = A) estado(id)$ediciones
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved" && !botones) "none" else "left",
                             buttons = botones, clickCount = 1L)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.25)
}
TECLAS <- list(Enter = list(code = "Enter", vk = 13L), Tab = list(code = "Tab", vk = 9L),
               Escape = list(code = "Escape", vk = 27L), Backspace = list(code = "Backspace", vk = 8L))
tecla <- function(k, shift = FALSE) {
  d <- TECLAS[[k]]; m <- if (shift) 8L else 0L
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = d$code, windowsVirtualKeyCode = d$vk, modifiers = m)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = d$code, windowsVirtualKeyCode = d$vk, modifiers = m)
  Sys.sleep(0.3)
}
# Select all in the focused input and type over it.
escribir <- function(texto) {
  ev("document.activeElement && document.activeElement.select && document.activeElement.select()")
  if (nzchar(texto)) b$Input$insertText(text = texto) else tecla("Backspace")
  Sys.sleep(0.15)
}
celda <- function(id, tabla = A) sprintf("HDT.fila('%s','%s').querySelector('td.hdt-editable, td.hdt-editando')", tabla, id)
td_importe <- function(id, tabla = A) sprintf("(function(){ var tr = HDT.fila('%s','%s'); return tr.children[tr.children.length - 1]; })()", tabla, id)
clases <- function(id, tabla = A) ev(sprintf("%s.className", td_importe(id, tabla)))
texto_celda <- function(id, tabla = A) ev(sprintf("%s.textContent", td_importe(id, tabla)))
input_valor <- function() ev("(document.activeElement && document.activeElement.classList.contains('hdt-ed-input')) ? document.activeElement.value : null")
eco <- function() {
  x <- ev("document.getElementById('eco').textContent")
  if (is.null(x) || !nzchar(x)) return(NULL)
  jsonlite::fromJSON(x, simplifyDataFrame = TRUE)
}
eco_ed <- function(campo = "ediciones") {
  e <- eco()[[campo]]
  if (is.null(e) || !length(e) || (is.data.frame(e) && !nrow(e))) return(data.frame(id = character(), valor = numeric(), valido = logical()))
  e[order(e$id), ]
}
esperar_eco <- function(f, s = 8) poll_until(function() isTRUE(tryCatch(f(eco_ed()), error = function(e) FALSE)), timeout_s = s)
pie <- function() ev(sprintf("document.querySelector('#%s .hdt-pie').textContent", A))
fila_grupo <- function(primera) sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", A, primera)
suma_grupo <- function(primera) ev(sprintf("(function(){ var tr = %s; return tr.children[tr.children.length - 1].textContent; })()", fila_grupo(primera)))
banda <- function(m) ev(sprintf("(function(){ var b = Array.from(document.querySelectorAll('#%s tr.hdt-banda')).filter(function(tr){ return tr.textContent.indexOf('%s') === 0; })[0]; return b ? b.querySelector('.hdt-banda-suma').textContent : null; })()", A, m))
tip_texto <- function() ev("(function(){ var t = document.querySelector('.hdt-tip'); return t && !t.hidden ? t.textContent : null; })()")
captura <- function(nombre) if (nzchar(CAPTURAS)) {
  dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
  b$screenshot(file.path(CAPTURAS, nombre), show = FALSE)
}

for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  cat(sprintf("\n--- %d x %d ---\n", tam[1], tam[2]))
  pantalla(tam[1], tam[2])
  ok("the table loads", abrir())
  clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", A))   # open every group: rows a..e show

  ok("E1: Importe is editable on every free row; the paid one (g) isn't",
     grepl("hdt-editable", clases("a")) && !grepl("hdt-editable", clases("g")))
  clic(td_importe("g"))
  ok("E1: a click on the paid row's amount opens nothing", is.null(estado()$editando))

  clic(celda("b"))
  ok("E2: a click on the cell types in it, starting from its value; the row isn't marked yet",
     identical(estado()$editando, "b") && identical(input_valor(), "500") && !length(unlist(estado()$seleccion)))
  captura(sprintf("T14_editando_%dx%d.png", tam[1], tam[2]))
  escribir("450"); tecla("Enter")
  ok("E3: Enter keeps it and goes down to the next row's cell (c)",
     identical(estado()$editando, "c") && isTRUE(ediciones_js()$b$valor == 450))
  ok("E3: the cell shows $450.00 in bold with its dot; typing marked the row (P1)",
     grepl("hdt-editada", clases("b")) && identical(texto_celda("b"), "$450.00") &&
       identical(unlist(estado()$seleccion), "b"))
  ok("E3: the server got it: t$ediciones() = b 450, valid; t$seleccion() = b",
     esperar_eco(function(e) identical(e$id, "b") && e$valor == 450 && isTRUE(e$valido)) &&
       identical(unlist(eco()$seleccion), "b"))

  escribir("1"); tecla("Escape")
  ok("E4: Esc leaves the cell as it was (c: $250.00, not edited, not marked)",
     is.null(estado()$editando) && identical(texto_celda("c"), "$250.00") && !grepl("hdt-editada", clases("c")) &&
       identical(unlist(estado()$seleccion), "b"))

  ok("E5: the footer adds what was typed ($450.00, not the $500.00 balance)", grepl("$450.00", pie(), fixed = TRUE) &&
       !grepl("$500.00", pie(), fixed = TRUE))
  ok("E5: the group's subtotal (NTS ACEROS: 1000 + 450 + 250) and the MXN band (2,620.00) too",
     identical(suma_grupo("a"), "$1,700.00") && grepl("$2,620.00", banda("MXN"), fixed = TRUE))

  clic(celda("a")); escribir("1500"); tecla("Enter"); tecla("Escape")
  ok("E6: over its maximum (Saldo 1,000): the cell in the danger tone, invalid here and on the server",
     grepl("hdt-ed-mal", clases("a")) && isFALSE(ediciones_js()$a$valido) &&
       esperar_eco(function(e) identical(e$id, c("a", "b")) && identical(e$valido, c(FALSE, TRUE))))
  ok("E6: an invalid amount doesn't add up: footer a + b = $450.00, group 450 + 250 = $700.00",
     grepl("$450.00", pie(), fixed = TRUE) && identical(suma_grupo("a"), "$700.00"))
  p <- centro(td_importe("a")); raton("mouseMoved", c(p[[1]] - 3, p[[2]])); raton("mouseMoved", p)
  poll_until(function() !is.null(tip_texto()), timeout_s = 3)
  ok("E6: the bubble says why: 'Mayor que el máximo ($1,000.00)'",
     grepl("Mayor que el máximo ($1,000.00)", tip_texto() %||% "", fixed = TRUE))
  captura(sprintf("T14_fuera_de_rango_%dx%d.png", tam[1], tam[2]))

  clic(celda("d")); escribir("700"); tecla("Tab")
  ok("E7: Tab keeps it and goes to the next editable cell (e)", identical(estado()$editando, "e") &&
       isTRUE(ediciones_js()$d$valor == 700))
  tecla("Tab", shift = TRUE)
  ok("E7: Shift+Tab goes back (d, showing what was typed)", identical(estado()$editando, "d") && identical(input_valor(), "700"))
  tecla("Escape")

  clic(celda("e")); escribir("abc"); tecla("Enter")
  ok("E8: something that isn't an amount isn't kept: red, the focus stays, the bubble says so",
     identical(estado()$editando, "e") && grepl("hdt-ed-mal", clases("e")) && is.null(ediciones_js()$e) &&
       grepl("Escribe un importe", tip_texto() %||% "", fixed = TRUE))
  tecla("Escape")
  ok("E8: Esc after it: e as it was", is.null(estado()$editando) && identical(texto_celda("e"), "$120.00"))

  clic(celda("d")); escribir(""); tecla("Enter"); tecla("Escape")
  ok("E9: empty + Enter goes back to the data's value (d $800.00, no edit); its mark stays",
     is.null(ediciones_js()$d) && identical(texto_celda("d"), "$800.00") && "d" %in% unlist(estado()$seleccion))
  clic(celda("f")); escribir("75.50"); tecla("Enter"); tecla("Escape")
  ok("E9: typing the data's own value is no edit", is.null(ediciones_js()$f))

  # Search, sort and clearing them keep what was typed.
  buscar <- sprintf("document.querySelector('#%s .hdt-buscar')", A)
  clic(buscar); b$Input$insertText(text = "F-101"); Sys.sleep(0.5)
  ok("E10: with a search on, the typed amounts are still there", identical(sort(names(ediciones_js())), c("a", "b")) &&
       identical(unlist(estado()$visibles), "a") && grepl("hdt-ed-mal", clases("a")))
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", buscar)); Sys.sleep(0.3)
  th <- sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.textContent.indexOf('Importe') === 0; })[0]", A)
  clic(th); clic(th); clic(th)
  ok("E10: after sorting and back, too (b still $450.00)", identical(texto_celda("b"), "$450.00") &&
       identical(sort(names(ediciones_js())), c("a", "b")))

  # Another session changes the data: b's Saldo drops to 300 (450 now over
  # it), c is gone, e gets paid.
  clic(celda("c")); escribir("200"); tecla("Enter"); tecla("Escape")
  clic(celda("e")); escribir("100"); tecla("Enter"); tecla("Escape")
  ok("E11: before: a, b, c, e typed", esperar_eco(function(e) identical(e$id, c("a", "b", "c", "e"))))
  clic("document.getElementById('otra')")
  ok("E11: new data: the typed amounts of rows that stay are kept; c (gone) and e (now paid) drop out",
     poll_until(function() identical(sort(names(ediciones_js())), c("a", "b")), timeout_s = 8) &&
       esperar_eco(function(e) identical(e$id, c("a", "b"))))
  ok("E11: and they're checked against the new bounds: b (450 > 300) is invalid now, here and on the server",
     isFALSE(ediciones_js()$b$valido) && grepl("hdt-ed-mal", clases("b")) &&
       esperar_eco(function(e) identical(e$valido, c(FALSE, FALSE))))
  ok("E11: the selection follows the same rule (e, now paid, drops out)", !"e" %in% unlist(estado()$seleccion))

  # New data while typing: the cell reopens with what was typed.
  clic(celda("f")); escribir("60")
  ev("Shiny.setInputValue('otra', 99, {priority: 'event'})")
  Sys.sleep(1.5)
  ok("E12: new data arriving while typing: the cell stays open with what was typed",
     identical(estado()$editando, "f") && identical(input_valor(), "60"))
  tecla("Enter"); tecla("Escape")

  clic("document.getElementById('limpiar')")
  ok("E13: t$limpiar_ediciones(): every cell back to the data, here and on the server",
     poll_until(function() !length(ediciones_js()), timeout_s = 6) && identical(texto_celda("a"), "$1,000.00") &&
       esperar_eco(function(e) !nrow(e)))

  # In a modal: Esc while typing must not close it.
  clic("document.getElementById('modal')")
  poll_until(function() isTRUE(ev("!!document.querySelector('.modal.show') && !!HDT.estado('m-hdt')")), timeout_s = 10)
  Sys.sleep(0.6)
  clic(celda("f", "m-hdt")); escribir("10"); tecla("Escape")
  ok("E14: Esc while typing in a modal leaves the cell and keeps the modal open",
     is.null(estado("m-hdt")$editando) && isTRUE(ev("!!document.querySelector('.modal.show')")))
  tecla("Escape")
  ok("E14: a second Esc (not typing) closes the modal, as always",
     poll_until(function() isFALSE(ev("!!document.querySelector('.modal.show')")), timeout_s = 5))
  Sys.sleep(0.5)
}

# Window mode (> 800 rows): what was typed survives scrolling away and back.
cat("\n--- modo ventana (1,007 filas) ---\n")
ok("fixture app with 1,000 extra rows", arrancar(1000L))
pantalla(1293L, 632L)
ok("the table loads", abrir())
ok("window mode is on", identical(estado()$modo, "ventana"))
clic(celda("x0001")); escribir("55"); tecla("Enter")
ok("W1: Enter goes to the next row even in window mode (x0002)", identical(estado()$editando, "x0002"))
tecla("Escape")
ev(sprintf("document.querySelector('#%s .hdt-scroll').scrollTop = 1e6", A)); Sys.sleep(0.6)
ok("W1: scrolled away, the row isn't painted", isTRUE(ev(sprintf("!HDT.fila('%s','x0001') || !HDT.fila('%s','x0001').parentNode", A, A))))
ev(sprintf("document.querySelector('#%s .hdt-scroll').scrollTop = 0", A)); Sys.sleep(0.6)
ok("W1: back, the amount typed is still there ($55.00, bold) and on the server",
   identical(texto_celda("x0001"), "$55.00") && grepl("hdt-editada", clases("x0001")) &&
     esperar_eco(function(e) identical(e$id, "x0001") && e$valor == 55))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
