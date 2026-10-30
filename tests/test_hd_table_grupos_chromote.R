# =============================================================================
# tests/test_hd_table_grupos_chromote.R
# T03c pieces of the hd_table engine in a real headless Chrome, against the
# demo app (dev/hd_table_demo: Bootstrap 5 flatly like HopDesk, generated
# data, no S3), at 1400x1080. Every click and key is a real input event
# (Input.dispatchMouseEvent / dispatchKeyEvent). Expected rows and amounts
# come from the demo's own generator.
#
# Run from project root:  Rscript tests/test_hd_table_grupos_chromote.R
# =============================================================================
cat("=== test_hd_table_grupos_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38963L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("dev/hd_table_demo/app.R")$value', repo),
                             PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("demo app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
pantalla(1400L, 1080L)
base <- sprintf("http://127.0.0.1:%d", PORT)

ev <- function(js, await = FALSE) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = await)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
A <- "a1-hdt"
abrir <- function(query) {
  b$Page$navigate(paste0(base, "/", query), wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s')", A))), timeout_s = 30)
  Sys.sleep(0.6)   # auto height and band offsets settle on the next frame
  ok
}
estado <- function(id = A) ev(sprintf("HDT.estado('%s')", id))
vis <- function() unlist(estado()$visibles)
lista <- function() unlist(estado()$lista)
# behavior 'instant': Bootstrap 5 sets scroll-behavior: smooth on :root.
centro <- function(js_el, ver = TRUE) ev(sprintf("(function(){ var e = %s; %s
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el,
  if (ver) "e.scrollIntoView({block:'center', behavior:'instant'});" else ""))
raton <- function(tipo, p, botones = 0L, n = 1L, shift = FALSE)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved") "none" else "left",
                             buttons = botones, clickCount = n, modifiers = if (shift) 8L else 0L)
clic <- function(js_el, ver = TRUE, shift = FALSE) {
  p <- centro(js_el, ver)
  raton("mouseMoved", p); raton("mousePressed", p, 1L, shift = shift); raton("mouseReleased", p, shift = shift)
  Sys.sleep(0.25)
}
escribir <- function(js_input, texto) {
  p <- centro(js_input); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  ev(sprintf("var i=%s; i.value=''; i.dispatchEvent(new Event('input'))", js_input))
  if (nzchar(texto)) b$Input$insertText(text = texto)
  Sys.sleep(0.4)
}
buscar <- sprintf("document.querySelector('#%s .hdt-buscar')", A)

# The demo's own data.
demo <- new.env()
invisible(capture.output(suppressMessages(sys.source("dev/hd_table_demo/app.R", envir = demo))))
datos_de <- function(n, periodo = FALSE, grupos = FALSE) demo$demo_facturas(n, 1, periodo, grupos)

# Bands as painted: level, title, count and amount read from the band.
bandas_dom <- function() {
  x <- ev(sprintf("Array.from(document.querySelectorAll('#%s tr.hdt-banda')).map(function(tr){
    var q = function(c){ var e = tr.querySelector(c); return e ? e.textContent : ''; };
    return { nivel: tr.classList.contains('hdt-banda-1') ? 1 : 2, titulo: q('.hdt-banda-titulo'),
             n: q('.hdt-banda-n'), suma: q('.hdt-banda-suma') }; })", A))
  data.frame(nivel = vapply(x, `[[`, 1, "nivel"), titulo = vapply(x, `[[`, "", "titulo"),
             n = as.integer(gsub("[^0-9]", "", vapply(x, `[[`, "", "n"))),
             suma = as.numeric(gsub("[^0-9.-]", "", vapply(x, `[[`, "", "suma"))), stringsAsFactors = FALSE)
}
# What each band should say, from the painted list and the data: its rows
# are the ones until the next band of its level or above; locked rows
# don't count.
bandas_esperadas <- function(datos) {
  l <- lista(); out <- list()
  for (p in which(startsWith(l, "banda:"))) {
    nivel <- as.integer(strsplit(l[p], ":")[[1]][2])
    q <- p + 1L
    while (q <= length(l) && !(startsWith(l[q], "banda:") && as.integer(strsplit(l[q], ":")[[1]][2]) <= nivel)) q <- q + 1L
    ids <- setdiff(l[seq_len(q - 1L)[-seq_len(p)]], l[startsWith(l, "banda:")])
    d <- datos[datos$id %in% ids & !datos$.bloqueada, ]
    out[[length(out) + 1L]] <- data.frame(nivel = nivel, n = nrow(d), suma = round(sum(d$Importe), 2))
  }
  do.call(rbind, out)
}
cuadran <- function(datos) {
  dom <- bandas_dom(); esp <- bandas_esperadas(datos)
  nrow(dom) == nrow(esp) && nrow(dom) > 0 && all(dom$nivel == esp$nivel) && all(dom$n == esp$n) &&
    all(abs(dom$suma - esp$suma) < 0.006)
}

# ── Sections and subtotals ─────────────────────────────────────────────────
ok("demo loads (300 rows, sections on, no month, no groups)", abrir("?n=300&seed=1&periodo=0&grupos=0"))
d300 <- datos_de(300)
l <- lista()
cabeceras <- l[startsWith(l, "banda:")]
cat("  bands:", paste(cabeceras, collapse = "  "), "\n")
ok("sections: currency first (MXN before USD), then Cobros before Pagos inside each",
   identical(cabeceras, c("banda:1:MXN", "banda:2:cobro", "banda:2:pago", "banda:1:USD", "banda:2:cobro", "banda:2:pago")))
ok("band titles: the currency, then 'Cobros' / 'Pagos'",
   identical(bandas_dom()$titulo, c("MXN", "Cobros", "Pagos", "USD", "Cobros", "Pagos")))
moneda <- setNames(d300$Moneda, d300$id); tipo <- setNames(d300$.tipo, d300$id)
en_su_banda <- TRUE; m <- NA; tp <- NA
for (x in l) {
  if (startsWith(x, "banda:1:")) m <- sub("banda:1:", "", x)
  else if (startsWith(x, "banda:2:")) tp <- sub("banda:2:", "", x)
  else if (moneda[[x]] != m || tipo[[x]] != tp) en_su_banda <- FALSE
}
ok("every row sits under the bands of its own currency and type", en_su_banda && length(vis()) == 300L)
ok("each band counts its rows and adds up their amounts (locked rows don't count)", cuadran(d300))

escribir(buscar, "aceros")
ok("subtotals follow the search", cuadran(d300) && length(vis()) < 300L)
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
ok("... the chips", cuadran(d300))
clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
escribir(buscar, "")
clic(sprintf("document.querySelectorAll('#%s .hdt-filtro')[1]", A))
clic("document.querySelector('.hdt-menu input[data-valor=\"NTS\"]')", ver = FALSE)
ok("... and the filters", cuadran(d300) && all(vapply(vis(), function(id) d300$Empresa[d300$id == id] == "NTS", logical(1))))
invisible(b$Input$dispatchKeyEvent(type = "rawKeyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L))
invisible(b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L))
clic(sprintf("document.querySelector('#%s .hdt-limpiar')", A))

# Sorting keeps the sections and sorts inside each one.
l_datos <- lista()
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))   # Importe
importe <- setNames(d300$Importe, d300$id)
l <- lista(); bloques <- split(l[!startsWith(l, "banda:")], cumsum(startsWith(l, "banda:"))[!startsWith(l, "banda:")])
ok("sorting by amount keeps the sections and sorts inside each one",
   identical(l[startsWith(l, "banda:")], cabeceras) && all(vapply(bloques, function(ids) !is.unsorted(importe[ids]), logical(1))))
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))   # descending
clic(sprintf("document.querySelectorAll('#%s thead th')[3]", A))   # third click
ok("a third click on the title goes back to the data's own order, with no arrow",
   identical(lista(), l_datos) && identical(ev(sprintf("document.querySelectorAll('#%s thead th')[3].querySelector('.hdt-flecha').textContent", A)), "") &&
     identical(estado()$orden$col, -1L))

# Bands stick under the header while scrolling.
# Sticky cells are the th / td (not the thead / tr): measure those. The
# stuck band of each level is the last one whose cell sits right under the
# header (level 1) or under that band (level 2).
pegadas <- function() ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'), th = s.querySelector('thead th').getBoundingClientRect();
  var top = s.getBoundingClientRect().top, b1 = null, b2 = null;
  s.querySelectorAll('tr.hdt-banda-1').forEach(function(tr){ var r = tr.firstChild.getBoundingClientRect(); if (Math.abs(r.top - th.bottom) < 1) b1 = tr; });
  s.querySelectorAll('tr.hdt-banda-2').forEach(function(tr){ var r = tr.firstChild.getBoundingClientRect(); if (b1 && Math.abs(r.top - b1.firstChild.getBoundingClientRect().bottom) < 1) b2 = tr; });
  return { b1: b1 ? b1.querySelector('.hdt-banda-titulo').textContent : null, b2: b2 ? b2.querySelector('.hdt-banda-titulo').textContent : null,
           cab: Math.abs(th.top - top) < 1.5 }; })()", A))
invisible(ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight * 0.55; })()", A)))
Sys.sleep(0.4)
p <- pegadas()
cat(sprintf("  scrolled to 55%%: stuck bands '%s' / '%s'\n", p$b1 %||% "-", p$b2 %||% "-"))
ok("scrolling: the header stays on top and the current section's two bands stick right under it",
   isTRUE(p$cab) && !is.null(p$b1) && !is.null(p$b2))

# ── The pinned block's line ─────────────────────────────────────────────────
ok("demo loads with the month and pinned rows", abrir("?n=300&seed=1&grupos=0"))
l <- lista(); nf <- length(unlist(estado()$fijas))
sep <- ev(sprintf("(function(){ var tr = document.querySelector('#%s tr.hdt-sep-fijas'); return tr ? tr.textContent : null; })()", A))
ok("a line with the caller's label goes right under the pinned block",
   identical(which(l == "sep"), nf + 1L) && identical(sep, sprintf("Siempre arriba: %d", nf)))
ok("... and the sections start below it", startsWith(l[nf + 2L], "banda:1:"))
escribir(buscar, "zzzz-nada")
ok("with the pinned block empty (search), no line is painted", !"sep" %in% lista())
escribir(buscar, "")
ok("demo loads with the pinned line but no label", abrir("?n=300&seed=1&rotulo=0&grupos=0"))
sep <- ev(sprintf("(function(){ var tr = document.querySelector('#%s tr.hdt-sep-fijas'); return tr ? { t: tr.textContent, h: tr.getBoundingClientRect().height } : null; })()", A))
ok("without fijar_titulo there is only the line: no text, and thinner than a row",
   !is.null(sep) && identical(sep$t, "") && sep$h < 15)

# ── Window mode: 5,000 rows, sticky bands that were never painted ─────────
ok("demo loads with 5,000 rows", abrir("?n=5000&seed=1&periodo=0&grupos=0"))
st <- estado()
ok("5,000 rows in sections paint in window mode (bands count as rows)",
   identical(st$modo, "ventana") && st$pintadas < 300L && length(unlist(st$lista)) == 5006L)
invisible(ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight * 0.62; })()", A)))
Sys.sleep(0.5)
p <- pegadas()
l <- lista(); vent <- unlist(estado()$ventana)
gobierna <- function(nivel) { prev <- l[seq_len(vent[1] + 1L)]; x <- tail(prev[startsWith(prev, paste0("banda:", nivel, ":"))], 1); sub("^banda:[12]:", "", x) }
esperado1 <- gobierna(1); esperado2 <- c(cobro = "Cobros", pago = "Pagos")[[gobierna(2)]]
cat(sprintf("  5,000 rows at 62%%: window %d-%d, stuck '%s' / '%s', expected '%s' / '%s'\n", vent[1], vent[2],
            p$b1 %||% "-", p$b2 %||% "-", esperado1, esperado2))
ok("in window mode the current section's bands are painted and stuck, even though they are far above",
   identical(p$b1, esperado1) && identical(p$b2, esperado2))
d5000 <- datos_de(5000)
escribir(buscar, "acero")
ok("subtotals follow the search in window mode too (back to full under 800)", cuadran(d5000))
escribir(buscar, "")

# ── Groups (expandir, sel_grupo) and Shift + click ─────────────────────────
ok("demo loads with groups (300 rows, Empresa + Vence + Parte)", abrir("?n=300&seed=1&periodo=0"))
dg <- datos_de(300, FALSE, TRUE)
clave <- paste(dg$Empresa, dg$Vence, dg$Parte, dg$Moneda, dg$.tipo, sep = "|")
esperados <- split(dg$id, clave); esperados <- esperados[lengths(esperados) >= 2]
grupos_lista <- function() { l <- lista(); l[startsWith(l, "grupo:")] }
miembros <- function(g) strsplit(sub("^grupo:[a-z]+:", "", g), ",")[[1]]
gl <- grupos_lista()
cat(sprintf("  %d groups expected, %d painted\n", length(esperados), length(gl)))
ok("one group per company + date + counterpart with 2 or more rows, all starting closed",
   length(gl) == length(esperados) && all(startsWith(gl, "grupo:cerrado:")) &&
     setequal(vapply(gl, function(g) paste(sort(miembros(g)), collapse = ","), ""),
              vapply(esperados, function(x) paste(sort(x), collapse = ","), "")))
ok("rows of closed groups aren't painted, but they are among the visible rows",
   !any(unlist(lapply(gl, miembros)) %in% lista()) && all(unlist(lapply(gl, miembros)) %in% vis()))

# A group with a locked row, to check counts, sums and marking.
bloq <- setNames(dg$.bloqueada, dg$id); imp <- setNames(dg$Importe, dg$id)
con_bloq <- Filter(function(g) any(bloq[miembros(g)]) && !all(bloq[miembros(g)]), gl)[[1]]
m1 <- miembros(con_bloq); libres <- m1[!bloq[m1]]
cab <- function(id) sprintf("document.querySelector('#%s tr.hdt-grupo[data-primera=\"%s\"]')", A, id)
celda_grupo <- ev(sprintf("(function(){ var tr = %s; return { n: tr.querySelector('.hdt-grupo-n').textContent,
  boton: tr.querySelector('.hdt-grupo-abrir').textContent, nombre: tr.querySelector('.hdt-col-grupo').lastChild.textContent,
  suma: tr.children[3].textContent }; })()", cab(m1[1])))
ok("the group's row shows ▶, how many rows without the locked ones, the counterpart and their total",
   identical(celda_grupo$boton, "▶") && identical(as.integer(celda_grupo$n), length(libres)) &&
     identical(celda_grupo$nombre, dg$Parte[dg$id == m1[1]]) &&
     abs(as.numeric(gsub("[^0-9.]", "", celda_grupo$suma)) - sum(imp[libres])) < 0.006)

# Open and close.
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", cab(m1[1])))
l <- lista(); p <- which(startsWith(l, "grupo:") & grepl(m1[1], l, fixed = TRUE))
ok("▶ opens the group: its rows follow it, ▼ shows, and nothing gets marked",
   startsWith(l[p], "grupo:abierto:") && identical(l[p + seq_along(m1)], miembros(l[p])) &&
     identical(ev(sprintf("%s.querySelector('.hdt-grupo-abrir').textContent", cab(m1[1]))), "▼") &&
     length(unlist(estado()$seleccion)) == 0L)
clic("document.getElementById('cambiar')"); Sys.sleep(0.8)
ok("an open group stays open with new data", any(startsWith(grupos_lista(), "grupo:abierto:") & grepl(m1[1], grupos_lista(), fixed = TRUE)))
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", cab(m1[1])))
ok("▼ closes it again", all(startsWith(grupos_lista(), "grupo:cerrado:")))
todos_btn <- sprintf("document.querySelector('#%s .hdt-grupos-todos')", A)
clic(todos_btn)
ok("▶▶ opens every group and turns into ▼▼",
   all(startsWith(grupos_lista(), "grupo:abierto:")) && identical(ev(sprintf("%s.textContent", todos_btn)), "▼▼"))
clic(todos_btn)
ok("▼▼ closes them all", all(startsWith(grupos_lista(), "grupo:cerrado:")) &&
     identical(ev(sprintf("%s.textContent", todos_btn)), "▶▶"))

# Marking a group.
sel <- function() unlist(estado()$seleccion)
clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(m1[1])))
ok("a click on a closed group marks all its rows, never the locked ones",
   setequal(sel(), libres) && isTRUE(ev(sprintf("%s.classList.contains('hdt-sel')", cab(m1[1])))))
clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(m1[1])))
ok("a second click unmarks them", length(sel()) == 0L)
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", cab(m1[1])))
fila_de <- function(id) sprintf("HDT.fila('%s', '%s').children[2]", A, id)
clic(fila_de(libres[1]))
ok("with only some of its rows marked, the group shows half-marked",
   identical(sel(), libres[1]) && isTRUE(ev(sprintf("%s.classList.contains('hdt-sel-parcial')", cab(m1[1])))))
clic(fila_de(libres[1]))
clic(sprintf("%s.querySelector('.hdt-grupo-abrir')", cab(m1[1])))

# Shift + click over groups: from one group to another, everything between.
l <- lista(); items <- which(!startsWith(l, "banda:") & l != "sep")
g_pos <- which(startsWith(l, "grupo:"))
a_pos <- g_pos[2]; b_pos <- g_pos[4]
if (any(startsWith(l[a_pos:b_pos], "banda:"))) { a_pos <- g_pos[1]; b_pos <- g_pos[2] }
filas_entre <- unique(unlist(lapply(l[a_pos:b_pos], function(x) if (startsWith(x, "grupo:")) miembros(x) else if (startsWith(x, "banda:")) character(0) else x)))
filas_entre <- filas_entre[!bloq[filas_entre]]
clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(miembros(l[a_pos])[1])))
clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(miembros(l[b_pos])[1])), shift = TRUE)
ok("Shift + click from one group to another marks every row of both, of the groups and of the rows between",
   setequal(sel(), filas_entre) && length(filas_entre) > 4L)
# P2: Shift + click on a marked one takes the opposite state: unmarks.
c_pos <- g_pos[which(g_pos == b_pos) - 1L]
if (c_pos <= a_pos) c_pos <- a_pos
quitar <- unique(unlist(lapply(l[c_pos:b_pos], function(x) if (startsWith(x, "grupo:")) miembros(x) else if (startsWith(x, "banda:")) character(0) else x)))
clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(miembros(l[c_pos])[1])), shift = TRUE)
ok("Shift + click on a marked group unmarks the range back to it (the clicked one's opposite state)",
   setequal(sel(), setdiff(filas_entre, quitar)) && length(quitar) > 0L)
deseleccionar <- function() { ev(sprintf("(document.querySelector('#%s .hdt-deseleccionar')||{click:function(){}}).click()", A)); Sys.sleep(0.3) }
deseleccionar()

# G7: a group click marks only the rows the filter leaves.
estado_de <- setNames(dg$Estado, dg$id)
cand <- Filter(function(g) { m <- miembros(g); k <- sum(estado_de[m] == "pendiente" & !bloq[m]); k >= 2 && k < length(m) }, gl)
if (length(cand)) {
  mg <- miembros(cand[[1]])
  clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
  clic(sprintf("%s.querySelector('.hdt-col-grupo')", cab(intersect(mg, vis())[1])))
  ok("with a chip on, a group click marks only its rows the chip leaves (never the hidden ones)",
     setequal(sel(), mg[estado_de[mg] == "pendiente" & !bloq[mg]]))
  clic(sprintf("document.querySelector('#%s .hdt-ficha[data-valor=pendiente]')", A))
  deseleccionar()
} else ok("(a group with 2+ pending rows and some that aren't: not in this data)", FALSE)

# Groups with 5,000 rows: window mode, open all, stuck bands still right.
ok("demo loads 5,000 rows with sections and groups", abrir("?n=5000&seed=1&periodo=0"))
st <- estado()
ng <- sum(startsWith(unlist(st$lista), "grupo:"))
cat(sprintf("  5,000 rows: %d groups, %d items painted out of %d, mode %s\n", ng, st$pintadas, length(unlist(st$lista)), st$modo))
ok("5,000 rows in groups paint in window mode", identical(st$modo, "ventana") && ng > 400L && st$pintadas < 300L)
clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", A))
ok("opening every group of 5,000 rows shows them all, still in window mode",
   all(startsWith(grupos_lista(), "grupo:abierto:")) && identical(estado()$modo, "ventana"))
invisible(ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'); s.scrollTop = s.scrollHeight * 0.4; })()", A)))
Sys.sleep(0.5)
p <- pegadas()
ok("... and the current section's bands stay stuck", !is.null(p$b1) && !is.null(p$b2))

# ── Optional columns (columnas_opc) ────────────────────────────────────────
descargas <- file.path(tempdir(), "hdt_descargas_t03c")
unlink(descargas, recursive = TRUE); dir.create(descargas)
invisible(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(descargas, winslash = "\\")))
invisible(try(b$Browser$grantPermissions(permissions = list("clipboardReadWrite", "clipboardSanitizedWrite"), origin = base),
              silent = TRUE))
titulos <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).filter(function(th){ return th.dataset.col != null; }).map(function(th){ return th.firstChild.textContent; })", A)))
ok("demo loads (no sections, to read plain rows)", abrir("?n=60&seed=1&periodo=0&secciones=0"))
invisible(ev("localStorage.clear()"))
ok("demo reloads with nothing remembered", abrir("?n=60&seed=1&periodo=0&secciones=0"))
t0 <- titulos()
ok("an optional column that starts hidden isn't shown; one that starts shown is",
   !"Origen" %in% t0 && "Moneda" %in% t0)
btn_col <- sprintf("document.querySelector('#%s .hdt-columnas')", A)
clic(btn_col)
ops <- unlist(ev("Array.from(document.querySelectorAll('.hdt-menu input')).map(function(i){ return i.dataset.campo + '=' + i.checked; })"))
ok("the 'Columnas' menu lists only the optional columns, checked as shown", identical(ops, c("Moneda=true", "Origen=false")))
clic("document.querySelector('.hdt-menu input[data-campo=\"Origen\"]')", ver = FALSE)
clic("document.querySelector('.hdt-menu input[data-campo=\"Moneda\"]')", ver = FALSE)
t1 <- titulos()
ok("checking Origen shows it, unchecking Moneda hides it (header and cells alike)",
   "Origen" %in% t1 && !"Moneda" %in% t1 &&
     isTRUE(ev(sprintf("Array.from(document.querySelectorAll('#%s tbody tr')).every(function(tr){ return tr.querySelectorAll('td:not(.hdt-acc):not(.hdt-cb)').length === %d; })", A, length(t1)))))
invisible(b$Input$dispatchKeyEvent(type = "rawKeyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L))
invisible(b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27L))
ok("the choice is remembered when the page reloads", abrir("?n=60&seed=1&periodo=0&secciones=0") && identical(titulos(), t1))

# Ctrl+C and Excel take only the columns shown.
v <- vis(); libres <- setdiff(v, unlist(estado()$bloqueadas))
clic(sprintf("HDT.fila('%s', '%s').children[1]", A, libres[1]))
clic(sprintf("HDT.fila('%s', '%s').children[1]", A, libres[2]))
invisible(tryCatch(ev("navigator.clipboard.writeText('')", await = TRUE), error = function(e) NULL))
invisible(b$Input$dispatchKeyEvent(type = "keyDown", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L,
                                   commands = list("copy")))
invisible(b$Input$dispatchKeyEvent(type = "keyUp", key = "c", code = "KeyC", windowsVirtualKeyCode = 67L, modifiers = 2L))
leer <- function() tryCatch(ev("navigator.clipboard.readText()", await = TRUE), error = function(e) "")
poll_until(function() nzchar(leer()), timeout_s = 8)
cabeza_tsv <- strsplit(strsplit(leer(), "\r\n", fixed = TRUE)[[1]][1], "\t", fixed = TRUE)[[1]]
ok("Ctrl+C copies only the columns shown, in their order", identical(cabeza_tsv, t1))
clic(sprintf("document.querySelector('#%s .hdt-excel')", A))
ok("Excel downloads", poll_until(function() length(list.files(descargas, pattern = "\\.xlsx$")) == 1L, timeout_s = 20))
xl <- openxlsx::read.xlsx(list.files(descargas, pattern = "\\.xlsx$", full.names = TRUE)[1])
ok("... with only the columns shown, in their order", identical(names(xl), make.names(t1)) || identical(names(xl), t1))
invisible(ev("localStorage.clear()"))

# ── Tags A / B / C and the edit mode ───────────────────────────────────────
ok("demo loads (60 rows, flat)", abrir("?n=60&seed=1&periodo=0&secciones=0"))
d60 <- datos_de(60)
COL_ET <- 7L
ETQ <- c(urgent = "A", both = "B", important = "C")
TONO <- c(urgent = "rojo", both = "naranja", important = "amarillo")
fila_info <- function(id) ev(sprintf("(function(){ var tr = HDT.fila('%s', '%s'); var p = tr.children[%d].querySelector('.hdt-ins');
  return { pill: p ? p.textContent : '', tono: p ? p.className : '', fila: tr.className }; })()", A, id, COL_ET))
bien <- vapply(d60$id, function(id) {
  e <- d60$Etiqueta[d60$id == id]; x <- fila_info(id)
  if (!nzchar(e)) return(!nzchar(x$pill) && !grepl("hdt-et-", x$fila))
  identical(x$pill, ETQ[[e]]) && grepl(paste0("hdt-tono-", TONO[[e]]), x$tono) && grepl(paste0("hdt-et-", TONO[[e]]), x$fila)
}, logical(1))
ok("tags show as pills A (red), B (orange), C (yellow) and color their row; untagged rows have neither", all(bien))
clic(sprintf("document.querySelectorAll('#%s thead th')[%d]", A, COL_ET))
peso <- c(urgent = 1, both = 2, important = 3)
p_vis <- vapply(vis(), function(id) { e <- d60$Etiqueta[d60$id == id]; if (nzchar(e)) peso[[e]] else 9 }, 1)
ok("sorting by the tag goes A, B, C, then untagged", !is.unsorted(p_vis))
clic(sprintf("document.querySelectorAll('#%s thead th')[%d]", A, COL_ET))
p_vis <- vapply(vis(), function(id) { e <- d60$Etiqueta[d60$id == id]; if (nzchar(e)) peso[[e]] else 9 }, 1)
ok("... and descending C, B, A, with untagged still last", !is.unsorted(rev(p_vis[p_vis < 9])) && all(tail(p_vis, sum(p_vis == 9)) == 9))
clic(sprintf("document.querySelectorAll('#%s thead th')[%d]", A, COL_ET))

# Edit mode.
acciones_vis <- function() unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s .hdt-acciones button')).map(function(b){ return b.dataset.accion + (b.disabled ? ':off' : ''); })", A)))
editar <- sprintf("document.querySelector('#%s .hdt-editar')", A)
ok("the edit actions don't show until Editar is on", !any(grepl("etiqueta_a", acciones_vis() %||% "")))
clic(editar)
a0 <- acciones_vis()
ok("Editar shows them, disabled while nothing is marked",
   all(c("etiqueta_a:off", "etiqueta_b:off", "etiqueta_c:off", "quitar_etiqueta:off", "mover:off", "restaurar:off") %in% a0) &&
     identical(ev(sprintf("%s.getAttribute('aria-pressed')", editar)), "true"))
sin_et <- setdiff(d60$id[!nzchar(d60$Etiqueta) & !d60$.bloqueada], character(0))[1]
clic(sprintf("HDT.fila('%s', '%s').children[1]", A, sin_et))
ok("with a row marked they can be used", "etiqueta_a" %in% acciones_vis() && "mover" %in% acciones_vis())
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=etiqueta_a]')", A))
ok("'Etiqueta A' reaches the server, and the row comes back with pill A and the red background",
   poll_until(function() { x <- fila_info(sin_et); identical(x$pill, "A") && grepl("hdt-et-rojo", x$fila) }, timeout_s = 8))
manana <- format(Sys.Date() + 1, "%Y-%m-%d")
fecha_in <- sprintf("document.querySelector('#%s .hdt-acciones input.hdt-fecha')", A)
ok("'Mover' has a date box, tomorrow by default", identical(ev(sprintf("%s.value", fecha_in)), manana))
invisible(ev(sprintf("var i = %s; i.value = ''; i.dispatchEvent(new Event('input'))", fecha_in)))
ok("an empty date disables 'Mover'", "mover:off" %in% acciones_vis())
invisible(ev(sprintf("var i = %s; i.value = '2026-12-15'; i.dispatchEvent(new Event('input'))", fecha_in)))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=mover]')", A))
ok("'Mover' sends the date as valor and the row moves to it",
   poll_until(function() grepl('valor *: chr "2026-12-15"', ev("document.getElementById('panel_a').innerText")), timeout_s = 8) &&
     poll_until(function() identical(ev(sprintf("HDT.fila('%s', '%s').children[5].textContent", A, sin_et)), "15 dic 2026"), timeout_s = 8))
clic(sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=eliminar_edicion]')", A))
ok("Eliminar in the edit bar asks for confirmation first",
   isTRUE(ev(sprintf("!document.querySelector('#%s .hdt-confirmar').hidden", A))))
clic(sprintf("document.querySelector('#%s .hdt-confirmar-no')", A))
clic(editar)
ok("Editar off hides the edit actions again", !any(grepl("etiqueta_a", acciones_vis() %||% "")))

# A group shows its strongest tag.
ok("demo loads with groups", abrir("?n=300&seed=1&periodo=0"))
dg <- datos_de(300, FALSE, TRUE)
gl <- grupos_lista()
fuerte_ok <- vapply(gl, function(g) {
  m <- miembros(g); e <- dg$Etiqueta[match(m, dg$id)]; e <- e[nzchar(e)]
  esperado <- if (length(e)) ETQ[[names(peso)[min(peso[e])]]] else ""
  x <- ev(sprintf("(function(){ var tr = %s, p = tr.children[%d].querySelector('.hdt-ins'); return { t: p ? p.textContent : '', c: tr.className }; })()",
                  cab(m[1]), COL_ET))
  identical(x$t, esperado) && (!nzchar(esperado) || grepl("hdt-et-", x$c))
}, logical(1))
ok("each group's row shows the strongest tag of its rows, with its background", all(fuerte_ok))

# ── crudos, agregado, adjuntos ─────────────────────────────────────────────
ok("demo loads with marks (60 rows, sections, no groups)", abrir("?n=60&seed=1&periodo=0&grupos=0"))
dm <- demo$demo_facturas(60, 1, FALSE, FALSE, marcas = TRUE)
ultima <- function() {
  linea <- strsplit(ev("document.getElementById('log_a').innerText") %||% "", "\n")[[1]][1]
  m <- regmatches(linea, regexec("^\\S+\\s+(\\S+)\\s+\\[(\\d+)\\]\\s*(.*)$", linea))[[1]]
  if (!length(m)) return(list(accion = NA, ids = character(0)))
  list(accion = m[2], ids = if (nzchar(trimws(m[4]))) strsplit(trimws(m[4]), " +")[[1]] else character(0))
}
marca <- function(id) ev(sprintf("(function(){ var tr = HDT.fila('%s', '%s'), td = tr.querySelector('td.hdt-acc');
  var v = td.querySelector('.hdt-agregado'), c = td.querySelector('.hdt-adjuntos');
  return { ag: !!v, clip: c ? c.textContent : '' }; })()", A, id))
con_ag <- dm$id[dm$.agregado]; con_clip <- dm$id[dm$.adjuntos > 0]
ok("agregado: a check in the actions column on exactly the rows already in Agenda de Hoy",
   all(vapply(dm$id, function(id) marca(id)$ag == (id %in% con_ag), logical(1))))
libre_ag <- setdiff(con_ag, dm$id[dm$.bloqueada])[1]
clic(sprintf("HDT.fila('%s', '%s').children[1]", A, libre_ag))
ok("... it doesn't lock the row: it can still be marked", identical(unlist(estado()$seleccion), libre_ag))
invisible(ev(sprintf("(document.querySelector('#%s .hdt-deseleccionar')||{click:function(){}}).click()", A))); Sys.sleep(0.3)
ok("adjuntos: a clip with the count on the rows that have attachments",
   all(vapply(dm$id, function(id) { x <- marca(id)$clip; n <- dm$.adjuntos[dm$id == id]
     if (n > 0) identical(x, sprintf("\U0001f4ce %d", n)) else identical(x, "") }, logical(1))))
clic(sprintf("HDT.fila('%s', '%s').querySelector('.hdt-adjuntos')", A, con_clip[1]))
ok("the clip sends accion 'adjuntos' with that row's id, without marking it",
   poll_until(function() { u <- ultima(); identical(u$accion, "adjuntos") && identical(u$ids, con_clip[1]) }, timeout_s = 8) &&
     length(unlist(estado()$seleccion)) == 0L)
clic(sprintf("document.querySelector('#%s .hdt-editar')", A))
crudos_btn <- sprintf("document.querySelector('#%s .hdt-acciones button[data-accion=crudos]')", A)
ok("crudos: 'Ver datos de origen' in the edit bar, usable with nothing marked",
   identical(ev(sprintf("%s.textContent", crudos_btn)), "Ver datos de origen") && !isTRUE(ev(sprintf("%s.disabled", crudos_btn))))
clic(crudos_btn)
ok("... it sends accion 'crudos' with no ids (the module decides what to show)",
   poll_until(function() { u <- ultima(); identical(u$accion, "crudos") && !length(u$ids) }, timeout_s = 8))
dos <- setdiff(dm$id, dm$id[dm$.bloqueada])[1:2]
clic(sprintf("HDT.fila('%s', '%s').children[1]", A, dos[1])); clic(sprintf("HDT.fila('%s', '%s').children[1]", A, dos[2]))
clic(crudos_btn)
ok("... and with rows marked, their ids",
   poll_until(function() { u <- ultima(); identical(u$accion, "crudos") && setequal(u$ids, dos) }, timeout_s = 8))
clic(sprintf("document.querySelector('#%s .hdt-editar')", A))

# ── The table's own empty message ──────────────────────────────────────────
ok("demo loads with no rows", abrir("?n=0&seed=1&periodo=0"))
vacio <- ev(sprintf("(function(){ var v = document.querySelector('#%s .hdt-vacio'); return { t: v.textContent, c: v.className, h: v.hidden,
  icono: getComputedStyle(v, '::before').content }; })()", A))
ok("no rows at all: the table's own message, with the green check of its tone",
   identical(vacio$t, "Sin vencimientos pendientes, todo al día") && grepl("hdt-vacio-exito", vacio$c) &&
     !isTRUE(vacio$h) && grepl("✓", vacio$icono))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
