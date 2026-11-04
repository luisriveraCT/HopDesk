# =============================================================================
# tests/test_hd_matriz_chromote.R
# The matrix base (T17a: columna_fija, tono_celda, dinero_compacto) in a real
# headless Chrome, against tests/helpers/matriz_fixture_app.R (300 rows x
# 180 periods, sections by currency; no S3). Scrolling is the real scroll
# container's; the bubble opens on a real mouse move.
# Sizes: Luis's real window (1293 x 632) and 1772 x 866, then window mode
# (MZ_N = 1000). Screenshots go to MZ_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_hd_matriz_chromote.R
# =============================================================================
cat("=== test_hd_matriz_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 38979L
CAPTURAS <- Sys.getenv("MZ_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

arrancar <- function(n = 300L) {
  stop_bg_app(app)
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(MZ_N = "%d"); setwd("%s"); source("tests/helpers/matriz_fixture_app.R")$value',
                                        n, repo), PORT, timeout_s = 90),
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
M <- "m-hdt"; P <- "p-hdt"
abrir <- function() {
  b$Page$navigate(base, wait_ = TRUE)
  ok <- poll_until(function() isTRUE(ev(sprintf("!!window.HDT && !!HDT.estado('%s') && !!HDT.estado('%s') && HDT.estado('%s').visibles.length > 0",
                                                M, P, M))), timeout_s = 90)
  Sys.sleep(0.8)
  ok
}
estado <- function(id = M) ev(sprintf("HDT.estado('%s')", id))
capturar <- function(nombre) if (nzchar(CAPTURAS)) {
  dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
  b$screenshot(file.path(CAPTURAS, paste0(nombre, ".png")), show = FALSE)
}
# Scroll the matrix's own container; returns after the browser has painted.
desplazar <- function(x = NULL, y = NULL) {
  ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll'); %s %s })()", M,
             if (is.null(x)) "" else sprintf("s.scrollLeft = %d;", as.integer(x)),
             if (is.null(y)) "" else sprintf("s.scrollTop = %d;", as.integer(y))))
  Sys.sleep(0.35)
}
# A cell's box relative to the scroll container's (left, top, width).
caja <- function(sel) ev(sprintf("(function(){ var s = document.querySelector('#%s .hdt-scroll').getBoundingClientRect();
  var e = document.querySelector(%s); if (!e) return null; var r = e.getBoundingClientRect();
  return {izq: r.left - s.left, arriba: r.top - s.top, ancho: r.width}; })()", M, jsonlite::toJSON(sel, auto_unbox = TRUE)))
# A row's cell by the column's title: text, classes, colour, background.
celda <- function(fila, titulo) ev(sprintf("(function(){ var t = HDT.fila('%s', '%s'); if (!t) return null;
  var ths = document.querySelectorAll('#%s thead th'), k = -1;
  for (var i = 0; i < ths.length; i++) if (ths[i].textContent === %s) { k = i; break; }
  if (k < 0) return null; var td = t.children[k], cs = getComputedStyle(td);
  return {texto: td.textContent, clases: td.className, color: cs.color, fondo: cs.backgroundColor, pos: cs.position}; })()",
  M, fila, M, jsonlite::toJSON(titulo, auto_unbox = TRUE)))
var_css <- function(nombre) ev(sprintf("getComputedStyle(document.querySelector('#%s')).getPropertyValue('%s').trim()", M, nombre))
raton <- function(p) b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[[1]], y = p[[2]], button = "none", buttons = 0L)

ronda <- function(w, h) {
  et <- sprintf("[%dx%d]", w, h)
  pantalla(w, h)
  ok(paste(et, "the page loads with both tables"), abrir())
  e <- estado()
  ok(sprintf("%s M1: all 300 rows painted (%s mode), with 2 + 2 + 180 columns; painted in %.0f ms",
             et, e$modo, e$medidas$pintado %||% NA), e$pintadas >= 300 && identical(e$modo, "completo") &&
       ev(sprintf("document.querySelectorAll('#%s thead th').length", M)) == 184)
  ok(paste(et, "M1b: a 300 x 184 paint stays under 4 s"), (e$medidas$pintado %||% Inf) < 4000)

  # ── columna_fija ──
  ok(paste(et, "M2: the root says it has fixed columns; the header's first two are fixed, the third isn't"),
     isTRUE(ev(sprintf("(function(){ var ths = document.querySelectorAll('#%s thead th');
       return document.querySelector('#%s').classList.contains('hdt-con-fijas') && ths[0].classList.contains('hdt-fija') &&
         ths[1].classList.contains('hdt-fija') && ths[1].classList.contains('hdt-fija-ultima') && !ths[2].classList.contains('hdt-fija'); })()", M, M))))
  ancho1 <- caja(sprintf("#%s thead th:nth-child(1)", M))$ancho
  ok(sprintf("%s M3: the second column's offset is the first one's width (%s)", et, var_css("--hdt-fija-1")),
     abs(as.numeric(sub("px", "", var_css("--hdt-fija-1"))) - ancho1) < 0.6 && var_css("--hdt-fija-0") == "0px")
  r0 <- celda("r0005", "Contraparte")
  desplazar(x = 3000)
  izq <- ev(sprintf("document.querySelector('#%s .hdt-scroll').scrollLeft", M))
  ok(sprintf("%s M4: the table scrolls sideways (scrollLeft %s)", et, izq), izq > 2000)
  c1 <- caja(sprintf("#%s tbody tr:not(.hdt-banda) td.hdt-fija", M))
  c2 <- caja(sprintf("#%s tbody tr:not(.hdt-banda) td.hdt-fija-ultima", M))
  ok(sprintf("%s M5: after scrolling 3000 px, the name stays at the left (%.1f) and the company right after it (%.1f)",
             et, c1$izq, c2$izq), abs(c1$izq) < 1 && abs(c2$izq - ancho1) < 1)
  th3 <- caja(sprintf("#%s thead th:nth-child(3)", M))
  ok(paste(et, "M6: a column that isn't fixed did scroll away"), th3$izq < -1000)
  ok(paste(et, "M7: fixed cells are opaque (what scrolls under them doesn't show)"),
     !identical(r0$fondo, "rgba(0, 0, 0, 0)") && identical(r0$pos, "sticky"))
  desplazar(y = 4000)
  h1 <- caja(sprintf("#%s thead th:nth-child(1)", M))
  ok(sprintf("%s M8: scrolled down and sideways, the fixed header cell stays at the top left (%.1f, %.1f)", et, h1$izq, h1$arriba),
     abs(h1$izq) < 1 && abs(h1$arriba) < 1)
  ok(paste(et, "M9: layers: fixed header (3) > bands (2) > fixed cells (1)"),
     isTRUE(ev(sprintf("(function(){ var z = function (s) { return getComputedStyle(document.querySelector(s)).zIndex; };
       return z('#%s thead th.hdt-fija') === '3' && z('#%s tbody tr.hdt-banda td') === '2' && z('#%s tbody td.hdt-fija') === '1'; })()", M, M, M))))
  desplazar(y = 0)
  bt <- caja(sprintf("#%s tbody tr.hdt-banda .hdt-banda-titulo", M))
  ok(sprintf("%s M10: a section band's title stays visible at the left while scrolled sideways (%.1f)", et, bt$izq),
     bt$izq >= 0 && bt$izq < 40)
  capturar(sprintf("matriz_%dx%d_desplazada", w, h))
  desplazar(x = 0, y = 0)

  # ── dinero_compacto ──
  ok(paste(et, "M11: 4,495,962 shows as $4.5M"), identical(celda("r0001", "d001")$texto, "$4.5M"))
  z <- celda("r0006", "d001")   # (6 + 1) %% 7 == 0: a zero
  ok(sprintf("%s M12: a zero paints nothing (\"%s\")", et, z$texto), identical(z$texto, ""))
  p <- ev(sprintf("(function(){ var t = HDT.fila('%s', 'r0001'), ths = document.querySelectorAll('#%s thead th'), k = -1;
    for (var i = 0; i < ths.length; i++) if (ths[i].textContent === 'd001') k = i;
    var td = t.children[k]; td.scrollIntoView({block: 'center', inline: 'center'});
    var r = td.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2]; })()", M, M))
  Sys.sleep(0.3)
  raton(c(p[[1]] - 30, p[[2]])); raton(p)
  ok(paste(et, "M13: the bubble on a compact cell shows the full amount"),
     poll_until(function() identical(ev("(function(){ var t = document.querySelector('.hdt-tip'); return t && !t.hidden ? t.querySelector('.hdt-tip-texto').textContent : null; })()"),
                                     "$4,495,962.00"), timeout_s = 5))
  ok(paste(et, "M14: and its copy button copies the raw number"),
     identical(ev("document.querySelector('.hdt-tip').copia"), "4495962"))
  ok(paste(et, "M14b: with the bubble, the compact cell has no title of its own"),
     !nzchar(ev(sprintf("HDT.fila('%s', 'r0001').querySelector('td.hdt-tc-signo').title", M))))
  raton(c(5, 5)); desplazar(x = 0, y = 0)
  ok(paste(et, "M14c: a compact table without the bubble (no copiar_celda) shows the full amount on hover (title)"),
     identical(ev("HDT.fila('c-hdt', 'r0001').children[1].title"), "$4,495,962.00") &&
       identical(ev("HDT.fila('c-hdt', 'r0001').children[1].textContent"), "$4.5M"))

  # ── tono_celda ──
  pos <- celda("r0001", "d001"); neg <- celda("r0004", "d001")   # (4 + 1) %% 5 == 0: negative
  ok(sprintf("%s M15: a positive amount is green (%s), a negative one red (%s)", et, pos$color, neg$color),
     grepl("hdt-tc-signo", pos$clases) && grepl("hdt-tc-exito", pos$clases) && pos$color == "rgb(21, 128, 74)" &&
       grepl("hdt-tc-peligro", neg$clases) && neg$color == "rgb(194, 43, 43)" && startsWith(neg$texto, "-$"))
  ok(sprintf("%s M16: the sign colours only the text (no tint: %s)", et, pos$fondo), identical(pos$fondo, "rgba(0, 0, 0, 0)"))
  est <- celda("r0001", "Estado"); est2 <- celda("r0005", "Estado")   # Estado: aviso, ..., "rojo" (not in the list)
  ok(sprintf("%s M17: a state tints the cell (aviso: %s on %s)", et, est$color, est$fondo),
     grepl("hdt-tc-estado", est$clases) && est$fondo == "rgb(255, 244, 220)" && est$color == "rgb(154, 86, 0)")
  ok(paste(et, "M18: a value outside the closed list paints no tone"), !grepl("hdt-tc-", est2$clases))
  hoy <- celda("r0001", "Hoy")
  ok(paste(et, "M19: the Hoy column is shaded, header and cells"),
     grepl("hdt-destacar", hoy$clases) && hoy$fondo == "rgb(230, 237, 255)" &&
       isTRUE(ev(sprintf("Array.prototype.some.call(document.querySelectorAll('#%s thead th'), function (th) { return th.textContent === 'Hoy' && th.classList.contains('hdt-destacar'); })", M))))
  ok(paste(et, "M20: hovering a row still wins over the shading (row rules first)"), {
    p <- ev(sprintf("(function(){ var td = HDT.fila('%s', 'r0002').children[0]; td.scrollIntoView({block: 'center'});
      var r = td.getBoundingClientRect(); return [r.left + 10, r.top + r.height / 2]; })()", M))
    Sys.sleep(0.2); raton(p); Sys.sleep(0.2)
    identical(celda("r0002", "Hoy")$fondo, "rgb(240, 244, 255)")
  })
  raton(c(5, 5))

  # ── reactive columns ──
  ev("document.getElementById('menos').click()")
  ok(paste(et, "M21: reactive columns: 90 periods now, the same two fixed and their offsets"),
     poll_until(function() ev(sprintf("document.querySelectorAll('#%s thead th').length", M)) == 94, timeout_s = 15) &&
       isTRUE(ev(sprintf("document.querySelectorAll('#%s thead th.hdt-fija').length === 2", M))) &&
       { Sys.sleep(0.4); abs(as.numeric(sub("px", "", var_css("--hdt-fija-1"))) - caja(sprintf("#%s thead th:nth-child(1)", M))$ancho) < 0.6 })

  # ── a table without the options ──
  ok(paste(et, "M22: the plain table has nothing new: no fixed, toned, shaded cells, and full amounts"),
     isTRUE(ev(sprintf("(function(){ var r = document.querySelector('#%s');
       return !r.classList.contains('hdt-con-fijas') && !r.querySelector('.hdt-fija, .hdt-fija-cb, .hdt-destacar, [class*=\"hdt-tc-\"]') &&
         !r.querySelector('td[style*=\"--hdt-fija\"], td[title]') && /^-?\\$[\\d,]+\\.\\d\\d$/.test(HDT.fila('%s', 'r0001').children[2].textContent); })()", P, P))))
}

ronda(1293, 632)
ronda(1772, 866)

# ── Window mode (more than 800 visible rows) ──
ok("fixture app restarted with 1,000 rows", arrancar(1000L))
pantalla(1293, 632)
ok("[ventana] the page loads", abrir())
e <- estado()
ok(sprintf("[ventana] M23: window mode (%s), %d rows painted of 1,000; painted in %.0f ms", e$modo, e$pintadas, e$medidas$pintado %||% NA),
   identical(e$modo, "ventana") && e$pintadas < 200)
desplazar(x = 2500, y = 15000)
Sys.sleep(0.4)
c1 <- caja(sprintf("#%s tbody tr:not(.hdt-banda):not(.hdt-esp) td.hdt-fija", M))
ok(sprintf("[ventana] M24: far down and sideways, the name is still at the left (%.1f)", c1$izq %||% NA),
   !is.null(c1) && abs(c1$izq) < 1)
capturar("matriz_ventana")

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
