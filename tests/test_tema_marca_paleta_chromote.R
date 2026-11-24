# =============================================================================
# tests/test_tema_marca_paleta_chromote.R
# TEMA 6 (T6-0): what a client in Marca REALLY sees that is not Marca.
# The guardian (tests/test_temas_guardian.R) counts hex written in the code;
# this test measures the painted page: the real app (tests/helpers/
# arranque_app.R: dev bucket, NOTHING written) with every client's
# group_config read as tema = "marca" in this process (ARR_FORZAR_TEMA), the
# client test user signed in, every tab shown in turn (nested tabs too), and
# the computed color / background-color / border colours / SVG fill and stroke
# of every visible element compared with Marca's palette:
#   - every hex of Marca's roles and the commons (R/tema_registro.R), the
#     brand sheet www/hd_marca.css and Panorama (www/panorama.css);
#   - a colour with alpha whose rgb is one of those (color-mix with
#     transparent), or an opaque colour on the line between two of them
#     (a tint "<rol>-pN" flattened over a surface);
#   - the data palettes, which are data, not the look: currencies (Bancos,
#     Pasivos, the currency pills), the tiers' colours, the folders'.
# Whatever is left is a colour Marca does not paint: listed per tab with its
# property and a few elements, and written to TEMA_SALIDA/fuera_de_marca.tsv.
# One check per tab ("0 colours outside Marca"); while TEMA 6 is open the
# failures ARE the inventory. Not covered: modals, menus and tooltips that
# only open on a click, gradients and background images, canvas charts.
# Run from project root (credentials in .Renviron / .Renviron.local):
#   TEMA_SALIDA=<folder> Rscript --vanilla tests/test_tema_marca_paleta_chromote.R
# =============================================================================
cat("=== test_tema_marca_paleta_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

usuario <- c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS"))
if (!all(nzchar(usuario))) { cat("SKIP: faltan las credenciales de prueba.\n"); quit(status = 2L) }
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "tema_marca_paleta"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))
TOL    <- 3     # ΔE (CIE76) to a palette colour, or to the line between two
MARINO <- "#13213A"

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

# ── Marca's palette, from the registry and the brand sheets ──────────────────
tema <- new.env(); sys.source("R/tema_registro.R", envir = tema)
RE_HEX6 <- "#[0-9A-Fa-f]{6}\\b"
hex_de <- function(x) toupper(unique(unlist(regmatches(x, gregexpr(RE_HEX6, x, perl = TRUE)))))
lineas <- function(f, patron) { x <- readLines(f, warn = FALSE, encoding = "UTF-8"); x[grepl(patron, x, perl = TRUE)] }
valores <- unlist(c(tema$HD_TEMAS$marca$tokens, tema$.HD_COMUNES), use.names = FALSE)
PALETA <- unique(c(hex_de(valores), "#FFFFFF",
                   hex_de(readLines("www/hd_marca.css", warn = FALSE)), hex_de(readLines("www/panorama.css", warn = FALSE))))
# rgba() inside the brand values (shadows aside, colours like rgba(19,33,58,.28))
rgba_txt <- unlist(regmatches(c(valores, readLines("www/panorama.css", warn = FALSE)),
                              gregexpr("rgba?\\(\\s*\\d+\\s*,\\s*\\d+\\s*,\\s*\\d+", c(valores, readLines("www/panorama.css", warn = FALSE)))))
if (length(rgba_txt)) PALETA <- unique(c(PALETA, vapply(regmatches(rgba_txt, gregexpr("\\d+", rgba_txt)), function(v)
  sprintf("#%02X%02X%02X", as.integer(v[1]), as.integer(v[2]), as.integer(v[3])), "")))
DATOS <- unique(c(
  hex_de(lineas("R/bancos_module.R", "^\\s*[A-Z]{3}\\s*=\\s*\"#|#5F5E5A")),
  hex_de(lineas("R/pasivos_table_styles.R", "^\\.PASIVOS_CUR_")),
  hex_de(lineas("R/ui_components.R", "ph-ctr|^\\s*background: #[0-9A-Fa-f]{6};$")),
  hex_de(lineas("R/tier_registry.R", "color\\s*=")),
  hex_de(readLines("R/requisiciones_persistence.R", warn = FALSE))))
cat(sprintf("      | Marca's palette: %d colours; data palettes: %d\n", length(PALETA), length(DATOS)))

lab <- function(hex) grDevices::convertColor(t(grDevices::col2rgb(hex)) / 255, from = "sRGB", to = "Lab")
P_LAB <- lab(PALETA); D_LAB <- lab(DATOS)
# A tint "<rol>-pN" is the role flattened over a surface: the line from each
# palette colour to one of Marca's surfaces (or its dark band, for the light
# tints over it). Never the line between any two colours: that would pass
# almost anything.
BASES <- lab(toupper(vapply(c("superficie", "fondo", "superficie-2", "primario", "oscuro-fondo"),
                            function(r) tema$HD_TEMAS$marca$tokens[[r]], "")))
dist_seg <- function(x, A) {
  best <- min(sqrt(colSums((t(A) - x)^2)))
  for (j in seq_len(nrow(BASES))) {
    d  <- t(A) - BASES[j, ]                                   # 3 x n: base -> colour
    w  <- x - BASES[j, ]
    tt <- pmin(1, pmax(0, colSums(d * w) / pmax(colSums(d^2), 1e-9)))
    p  <- BASES[j, ] + d * rep(tt, each = 3L)
    best <- min(best, sqrt(colSums((p - x)^2)))
  }
  best
}
clasifica <- function(hex, alfa) {
  x <- lab(hex)[1, ]
  if (min(sqrt(colSums((t(D_LAB) - x)^2))) <= TOL) return("dato")
  if (alfa < 1) return(if (min(sqrt(colSums((t(P_LAB) - x)^2))) <= TOL) "marca" else "fuera")
  if (dist_seg(x, P_LAB) <= TOL) "marca" else "fuera"
}

# ── The real app ─────────────────────────────────────────────────────────────
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39050L
app <- NULL; b <- NULL
fin <- function() {
  if (!is.null(app)) tryCatch({
    x <- readLines(app$proc$get_error_file(), warn = FALSE, encoding = "UTF-8")
    x <- x[grepl("tema forzado|Error|error|WARN", x) & !grepl(tolower(usuario[["user"]]), tolower(x), fixed = TRUE)]
    writeLines(x, file.path(SALIDA, "app_log.txt"), useBytes = TRUE)
  }, error = function(e) NULL)
  try(b$close(), silent = TRUE); stop_bg_app(app)
  cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
  quit(status = if (.fail > 0L) 1L else 0L)
}
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
# Every client's group_config reads as Marca: the client id is a pattern in
# arranque_app.R's regex ("(^|/)<cid>/group_config.rds").
codigo <- sprintf(paste0('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s", ARR_FORZAR_TEMA = "marca", ',
                         'ARR_FORZAR_TEMA_CLIENTE = "[^/]+"); setwd("%s"); source("tests/helpers/arranque_app.R")$value'),
                  repo, repo)
app <- tryCatch(start_bg_app(codigo, PORT, timeout_s = 300, www = file.path(repo, "www")),
                error = function(e) { message(conditionMessage(e)); NULL })
if (!ok("the real app starts on dev, every client read as Marca", !is.null(app))) fin()

b <- new_chromote_session()
invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
for (k in 1:2) if (!inherits(tryCatch(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE, timeout_ = 120),
                                      error = function(e) e), "error")) break
poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 120)
Sys.sleep(1)
ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
   u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
   jsonlite::toJSON(usuario[["user"]], auto_unbox = TRUE), jsonlite::toJSON(usuario[["pass"]], auto_unbox = TRUE)))
Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
cargada <- function() isTRUE(tryCatch(ev(
  "(function(){ var f = document.getElementById('auth-user_id'), o = document.getElementById('hopdesk-loading');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]') &&
            (!o || o.style.display === 'none' || o.classList.contains('hld-fadeout')); })()"), error = function(e) FALSE))
tema_de <- function() tryCatch(ev("document.documentElement.getAttribute('data-hd-tema') || ''"), error = function(e) "?")
primario <- function() tryCatch(toupper(ev("getComputedStyle(document.documentElement).getPropertyValue('--hd-primario').trim()")),
                                error = function(e) "?")
if (!ok("the client user signs in", poll_until(cargada, timeout_s = LIMITE, interval_s = 1))) fin()
if (!ok("the page turns Marca (attribute and primario)",
        poll_until(function() identical(tema_de(), "marca"), timeout_s = LIMITE, interval_s = 0.5) &&
          identical(primario(), MARINO))) fin()
if (!ok("no sign-in form left", !isTRUE(ev("(function(){ var f = document.getElementById('auth-user_id'); return !!f && f.offsetParent !== null; })()")))) fin()
Sys.sleep(3)

# Show a tab, its ancestors' tabs first (a nested tab inside a hidden pane
# draws nothing).
MOSTRAR_JS <- "function(v){
  var a = document.querySelector(\"a[data-bs-toggle='tab'][data-value='\" + v + \"'], a[data-toggle='tab'][data-value='\" + v + \"']\");
  if (!a) return false;
  var cadena = [], p = a;
  while (p) { var pane = p.closest && p.closest('.tab-pane'); if (!pane) break;
    var l = document.querySelector(\"a[data-bs-toggle='tab'][href='#\" + pane.id + \"'], a[data-bs-toggle='tab'][data-bs-target='#\" + pane.id + \"']\");
    if (!l) break; cadena.unshift(l); p = l; }
  cadena.push(a);
  cadena.forEach(function(x){ bootstrap.Tab.getOrCreateInstance(x).show(); });
  return true; }"
# Every visible element's painted colours, grouped by (property, colour).
MEDIR_JS <- "(function(){
  var out = {}, n = 0;
  function sel(e){ var s = e.tagName.toLowerCase(); if (e.id) s += '#' + e.id;
    if (typeof e.className === 'string' && e.className.trim()) s += '.' + e.className.trim().split(/\\s+/).slice(0, 3).join('.');
    return s; }
  function nota(prop, val, e){
    if (!val || val === 'none' || val === 'transparent' || /^rgba\\(0, 0, 0, 0\\)$/.test(val)) return;
    var k = prop + '|' + val; if (!out[k]) out[k] = {prop: prop, val: val, n: 0, ej: []};
    out[k].n++; if (out[k].ej.length < 3) out[k].ej.push(sel(e)); }
  var todos = document.body.querySelectorAll('*');
  for (var i = 0; i < todos.length; i++) {
    var e = todos[i];
    if (!e.getClientRects().length) continue;
    var cs = getComputedStyle(e);
    if (cs.visibility === 'hidden' || cs.display === 'none' || parseFloat(cs.opacity) === 0) continue;
    n++;
    var texto = false;
    for (var k = 0; k < e.childNodes.length; k++) if (e.childNodes[k].nodeType === 3 && e.childNodes[k].nodeValue.trim()) { texto = true; break; }
    if (texto) nota('color', cs.color, e);
    nota('background-color', cs.backgroundColor, e);
    ['Top', 'Right', 'Bottom', 'Left'].forEach(function(s){
      if (parseFloat(cs['border' + s + 'Width']) > 0 && cs['border' + s + 'Style'] !== 'none') nota('border-color', cs['border' + s + 'Color'], e); });
    if (e instanceof SVGElement) { nota('fill', cs.fill, e); nota('stroke', cs.stroke, e); }
  }
  return {n: n, colores: Object.keys(out).map(function(k){ return out[k]; })};
})()"
rgba <- function(val) {
  v <- as.numeric(regmatches(val, gregexpr("[0-9.]+", val))[[1]])
  if (length(v) < 3L) return(NULL)
  list(hex = sprintf("#%02X%02X%02X", round(v[1]), round(v[2]), round(v[3])), alfa = if (length(v) >= 4L) v[4] else 1)
}

pestanas <- unlist(tryCatch(ev("Array.from(document.querySelectorAll(\"a[data-bs-toggle='tab'][data-value]\"))
  .map(function(a){ return a.getAttribute('data-value'); })"), error = function(e) list()))
pestanas <- unique(pestanas[nzchar(pestanas)])
cat(sprintf("      | %d tabs (nested ones too): %s\n", length(pestanas), paste(pestanas, collapse = ", ")))
filas <- list()
for (v in pestanas) {
  if (!isTRUE(tryCatch(ev(sprintf("(%s)(%s)", MOSTRAR_JS, jsonlite::toJSON(v, auto_unbox = TRUE))), error = function(e) FALSE))) next
  Sys.sleep(3)
  poll_until(function() isTRUE(tryCatch(ev("document.querySelectorAll('.recalculating').length === 0"), error = function(e) TRUE)),
             timeout_s = 90, interval_s = 1)
  Sys.sleep(3); ev("window.scrollTo(0, 0); true")
  tryCatch({ s <- b$Page$captureScreenshot(format = "png")
             writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0("marca_", v, ".png"))) }, error = function(e) NULL)
  m <- tryCatch(ev(MEDIR_JS), error = function(e) NULL)
  if (is.null(m)) { ok(sprintf("%s: measured", v), FALSE); next }
  fuera <- list()
  for (c in m$colores) {
    x <- rgba(c$val); if (is.null(x) || x$alfa == 0) next
    if (identical(clasifica(x$hex, x$alfa), "fuera"))
      fuera[[length(fuera) + 1L]] <- data.frame(pestana = v, propiedad = c$prop, color = x$hex, alfa = x$alfa,
                                                n = c$n, ejemplos = paste(unlist(c$ej), collapse = " ; "))
  }
  f <- if (length(fuera)) do.call(rbind, fuera) else NULL
  if (!is.null(f)) { f <- f[order(-f$n), ]; filas[[v]] <- f }
  ok(sprintf("%s: %d elements, 0 colours outside Marca (%d left)", v, m$n, if (is.null(f)) 0L else nrow(f)), is.null(f))
  if (!is.null(f)) for (i in seq_len(min(nrow(f), 8L)))
    cat(sprintf("      | %-16s %s%s x%d  %s\n", f$propiedad[i], f$color[i],
                if (f$alfa[i] < 1) sprintf(" @%.2f", f$alfa[i]) else "", f$n[i], substr(f$ejemplos[i], 1, 110)))
}
todo <- if (length(filas)) do.call(rbind, filas) else
  data.frame(pestana = character(), propiedad = character(), color = character(), alfa = numeric(), n = integer(), ejemplos = character())
write.table(todo, file.path(SALIDA, "fuera_de_marca.tsv"), sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
cat(sprintf("      | %d (tab, property, colour) outside Marca; detail in %s\n", nrow(todo), file.path(SALIDA, "fuera_de_marca.tsv")))
ok("still Marca after every tab", identical(tema_de(), "marca"))
fin()
