# =============================================================================
# tests/test_tema_marca_paleta_chromote.R
# TEMA 6 (T6-0): what a client in Marca REALLY sees that is not Marca.
# The guardian (tests/test_temas_guardian.R) counts hex written in the code;
# this test measures the painted page: the real app (tests/helpers/
# arranque_app.R: dev bucket, NOTHING written) with every client's
# group_config read as tema = "marca" in this process (ARR_FORZAR_TEMA), the
# client test user signed in, every tab shown in turn (nested tabs too), and
# the computed color / background-color / border colours / SVG fill and stroke
# of every visible element compared with the EXACT set of colours Marca can
# paint (no "near enough" lines between colours: a cool Bootstrap grey would
# pass as a tint of something):
#   - Marca's block of the generated www/hd_temas.css (roles, the
#     legacy tokens' tints), the commons (.HD_COMUNES: the same in every
#     theme, e.g. the tags' etq-*; not acceso-*, only the sign-in pages paint
#     those), the Bootstrap bridge (www/hd_tema_bootstrap.css,
#     Marca only), the brand sheet www/hd_marca.css and www/panorama.css;
#   - every opaque color-mix(in srgb, var(--hd-a) N%, var(--hd-b) | white)
#     the code writes, worked out under Marca;
#   - a colour with alpha whose rgb is one of those (color-mix with
#     transparent);
#   - the currency palettes (Bancos, Pasivos, the currency pills) and the
#     folders' colours: data, not the look. The tiers' colours are NOT
#     excused (two of them are Bootstrap's own blue and grey): a tier badge
#     shows up in the list and the report says so.
# The classifier has a control at the start: Bootstrap and Clásico colours
# must come out as outside Marca, or the run stops.
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

# TEMA_MEDIR_COMO=prueba signs in the staff test user instead (TEMA 6-6: the
# new-request form, which the client test user cannot open in dev).
usuario <- if (identical(Sys.getenv("TEMA_MEDIR_COMO"), "prueba"))
  c(user = Sys.getenv("HOPDESK_TEST_USER"), pass = Sys.getenv("HOPDESK_TEST_PASS")) else
  c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS"))
if (!all(nzchar(usuario))) { cat("SKIP: faltan las credenciales de prueba.\n"); quit(status = 2L) }
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "tema_marca_paleta"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))
TOL    <- 2.5   # ΔE (CIE76) to a colour Marca paints
MARINO <- "#13213A"

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

# ── Marca's palette: the colours Marca can paint ─────────────────────────────
tema <- new.env(); sys.source("R/tema_registro.R", envir = tema)
leer <- function(f) readLines(f, warn = FALSE, encoding = "UTF-8")
RE_HEX6 <- "#[0-9A-Fa-f]{6}\\b"
RE_RGB  <- "rgba?\\(\\s*\\d+\\s*,\\s*\\d+\\s*,\\s*\\d+"
colores_de <- function(x) {
  h <- toupper(unlist(regmatches(x, gregexpr(RE_HEX6, x, perl = TRUE))))
  r <- unlist(regmatches(x, gregexpr(RE_RGB, x, perl = TRUE)))
  # "--x-rgb: 19, 33, 58" (Bootstrap's triplets)
  tr <- unlist(regmatches(x, gregexpr("-rgb\\s*:\\s*\\d+\\s*,\\s*\\d+\\s*,\\s*\\d+", x, perl = TRUE)))
  a_hex <- function(s) vapply(regmatches(s, gregexpr("\\d+", s)), function(v)
    sprintf("#%02X%02X%02X", as.integer(v[1]), as.integer(v[2]), as.integer(v[3])), "")
  unique(c(h, if (length(r)) a_hex(r), if (length(tr)) a_hex(sub("^-rgb\\s*:", "", tr))))
}
lineas <- function(f, patron) { x <- leer(f); x[grepl(patron, x, perl = TRUE)] }
# The Marca block of the generated sheet.
css <- leer("www/hd_temas.css")
i0 <- grep('^\\[data-hd-tema="marca"\\] \\{', css); i1 <- i0 + which(grepl("^\\}", css[(i0 + 1L):length(css)]))[1]
stopifnot(length(i0) == 1L, !is.na(i1))
# Opaque mixes the code writes, under Marca.
codigo <- unlist(lapply(c("app.R", list.files(c("R", "www"), pattern = "\\.(R|css|js)$", recursive = TRUE, full.names = TRUE)), leer))
mx <- unique(unlist(regmatches(codigo, gregexpr("color-mix\\(in srgb, var\\(--hd-[a-z0-9-]+\\) [0-9.]+%, (var\\(--hd-[a-z0-9-]+\\)|white)\\)", codigo))))
rol_de <- function(v) { v <- sub("^leg-", "", v); p <- tema$tema_legado_partes(v); if (!is.null(p)) p$rol else v }
mezclas <- vapply(mx, function(s) tryCatch({
  m <- regmatches(s, regexec("var\\(--hd-([a-z0-9-]+)\\) ([0-9.]+)%, (var\\(--hd-([a-z0-9-]+)\\)|white)", s))[[1]]
  a <- tema$tema_color(if (startsWith(m[2], "leg-")) rol_de(m[2]) else m[2], "marca")
  b <- if (m[4] == "white") "#FFFFFF" else tema$tema_color(if (startsWith(m[5], "leg-")) rol_de(m[5]) else m[5], "marca")
  ca <- grDevices::col2rgb(a)[, 1]; cb <- grDevices::col2rgb(b)[, 1]; w <- as.numeric(m[3]) / 100
  v <- round(ca * w + cb * (1 - w)); sprintf("#%02X%02X%02X", v[1], v[2], v[3])
}, error = function(e) NA_character_), "")
PALETA <- unique(c(colores_de(css[i0:i1]), colores_de(unlist(tema$.HD_COMUNES[!startsWith(names(tema$.HD_COMUNES), "acceso-")])), colores_de(leer("www/hd_tema_bootstrap.css")),
                   colores_de(leer("www/hd_marca.css")), colores_de(leer("www/panorama.css")),
                   na.omit(mezclas), "#FFFFFF",
                   # TEMA 6-6: a tier badge is its colour (data) mixed with
                   # Marca's encabezado (nivel_fondo()): that mix is Marca's
                   # paint. The bare tier colours stay NOT excused.
                   if (exists("nivel_fondo_hex", envir = tema, inherits = FALSE)) {
                     niv <- new.env(); sys.source("R/tier_registry.R", envir = niv)
                     toupper(vapply(names(niv$TIER_REGISTRY), function(n) tema$nivel_fondo_hex(niv$nivel_color(n), "marca", niv$.NIVEL_MEZCLA), ""))
                   }))
DATOS <- unique(c(
  colores_de(lineas("R/bancos_module.R", "^\\s*[A-Z]{3}\\s*=\\s*\"#|#5F5E5A")),
  colores_de(lineas("R/pasivos_table_styles.R", "^\\.PASIVOS_CUR_")),
  colores_de(lineas("R/ui_components.R", "ph-ctr|^\\s*background: #[0-9A-Fa-f]{6};$")),
  colores_de(leer("R/requisiciones_persistence.R"))))
cat(sprintf("      | Marca paints %d colours (%d of them opaque mixes from the code); data palettes: %d\n",
            length(PALETA), sum(!is.na(mezclas)), length(DATOS)))

lab <- function(hex) grDevices::convertColor(t(grDevices::col2rgb(hex)) / 255, from = "sRGB", to = "Lab")
P_LAB <- lab(PALETA); D_LAB <- lab(DATOS)
cerca <- function(x, A) min(sqrt(colSums((t(A) - x)^2)))
clasifica <- function(hex, alfa) {
  x <- lab(hex)[1, ]
  if (cerca(x, P_LAB) <= TOL) return("marca")
  if (cerca(x, D_LAB) <= TOL) return("dato")
  "fuera"
}
# Control: Bootstrap's and Clásico's own colours are outside Marca.
CONTROL <- c("#0D6EFD", "#0A58CA", "#6C757D", "#ECF0F1", "#DEE2E6", "#212529", "#198754", "#F0F4FF")
fuera_ctl <- vapply(CONTROL, function(h) identical(clasifica(h, 1), "fuera"), logical(1))
if (!ok(sprintf("control: Bootstrap / Clásico colours come out as outside Marca (%s)",
                paste(sprintf("%s %s", CONTROL, ifelse(fuera_ctl, "fuera", "PASA")), collapse = ", ")), all(fuera_ctl))) {
  cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail)); quit(status = 1L) }

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

# A new browser session, signed in as the client (also after a tab closed it).
sesion <- function() {
  try(b$close(), silent = TRUE)
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  for (k in 1:2) if (!inherits(tryCatch(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE, timeout_ = 120),
                                        error = function(e) e), "error")) break
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 120)
  Sys.sleep(1)
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(usuario[["user"]], auto_unbox = TRUE), jsonlite::toJSON(usuario[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(cargada, timeout_s = LIMITE, interval_s = 1) &&
    poll_until(function() identical(tema_de(), "marca"), timeout_s = LIMITE, interval_s = 0.5)
}
cargada <- function() isTRUE(tryCatch(ev(
  "(function(){ var f = document.getElementById('auth-user_id'), o = document.getElementById('hopdesk-loading');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]') &&
            (!o || o.style.display === 'none' || o.classList.contains('hld-fadeout')); })()"), error = function(e) FALSE))
tema_de <- function() tryCatch(ev("document.documentElement.getAttribute('data-hd-tema') || ''"), error = function(e) "?")
primario <- function() tryCatch(toupper(ev("getComputedStyle(document.documentElement).getPropertyValue('--hd-primario').trim()")),
                                error = function(e) "?")
if (!ok("the client user signs in and the page turns Marca (attribute and primario)",
        sesion() && identical(primario(), MARINO))) fin()
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
    out[k].n++; if (out[k].ej.length < 3) out[k].ej.push(e instanceof SVGElement ? svg_ej(e) : sel(e)); }
  // TEMA 6-10: an SVG says where it hangs, its size and how it starts (the
  // bare black-filled <svg> of every tab, not identified from the code).
  function svg_ej(e){ var r = e.getBoundingClientRect(), p = e.parentElement, a = p && p.parentElement;
    return (a ? sel(a) + '>' : '') + (p ? sel(p) + '>' : '') + sel(e) + ' ' + Math.round(r.width) + 'x' + Math.round(r.height) +
      ' ' + e.outerHTML.slice(0, 90).replace(/\\s+/g, ' '); }
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
    // Only shapes and text paint a fill or a stroke. The <svg> box itself
    // never does: its fill is only inherited (black by default). That was the
    // black #000000 of every tab (TEMA 6-10): bslib's 0x0 <svg> of icon
    // gradients (insertSvgGradient, components.js), the calendar arrow (its
    // path has fill none) and the empty Bancos sparkline.
    if (e instanceof SVGGeometryElement || e instanceof SVGTextContentElement) { nota('fill', cs.fill, e); nota('stroke', cs.stroke, e); }
  }
  return {n: n, colores: Object.keys(out).map(function(k){ return out[k]; })};
})()"
# rgb()/rgba() in 0-255, or color(srgb r g b / a) in 0-1: what Chrome returns
# for a color-mix (read as 0-255 it would turn every mix into black).
rgba <- function(val) {
  v <- as.numeric(regmatches(val, gregexpr("[0-9]*\\.?[0-9]+(e-?[0-9]+)?", val))[[1]])
  if (length(v) < 3L) return(NULL)
  if (startsWith(trimws(val), "color(srgb")) v[1:3] <- v[1:3] * 255
  else if (!startsWith(trimws(val), "rgb")) return(NULL)
  list(hex = sprintf("#%02X%02X%02X", as.integer(round(min(255, v[1]))), as.integer(round(min(255, v[2]))), as.integer(round(min(255, v[3])))),
       alfa = if (length(v) >= 4L) v[4] else 1)
}

pestanas <- unlist(tryCatch(ev("Array.from(document.querySelectorAll(\"a[data-bs-toggle='tab'][data-value]\"))
  .map(function(a){ return a.getAttribute('data-value'); })"), error = function(e) list()))
pestanas <- unique(pestanas[nzchar(pestanas)])
cat(sprintf("      | %d tabs (nested ones too): %s\n", length(pestanas), paste(pestanas, collapse = ", ")))
filas <- list()
SIN_CUBRIR <- character()
# One view as it is on screen now: a capture, its colours, the ones outside Marca.
medir_vista <- function(v) {
  tryCatch({ s <- b$Page$captureScreenshot(format = "png")
             writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0("marca_", v, ".png"))) }, error = function(e) NULL)
  m <- tryCatch(ev(MEDIR_JS), error = function(e) NULL)
  if (is.null(m)) { ok(sprintf("%s: measured", v), FALSE); return(invisible(NULL)) }
  fuera <- list()
  for (c in m$colores) {
    x <- rgba(c$val); if (is.null(x) || x$alfa == 0) next
    if (identical(clasifica(x$hex, x$alfa), "fuera"))
      fuera[[length(fuera) + 1L]] <- data.frame(pestana = v, propiedad = c$prop, color = x$hex, alfa = x$alfa,
                                                n = c$n, ejemplos = paste(unlist(c$ej), collapse = " ; "))
  }
  f <- if (length(fuera)) do.call(rbind, fuera) else NULL
  if (!is.null(f)) { f <- f[order(-f$n), ]; filas[[v]] <<- f }
  ok(sprintf("%s: %d elements, 0 colours outside Marca (%d left)", v, m$n, if (is.null(f)) 0L else nrow(f)), is.null(f))
  if (!is.null(f)) for (i in seq_len(min(nrow(f), 8L)))
    cat(sprintf("      | %-16s %s%s x%d  %s\n", f$propiedad[i], f$color[i],
                if (f$alfa[i] < 1) sprintf(" @%.2f", f$alfa[i]) else "", f$n[i], substr(f$ejemplos[i], 1, 110)))
}
for (v in pestanas) {
  if (!isTRUE(tryCatch(ev(sprintf("(%s)(%s)", MOSTRAR_JS, jsonlite::toJSON(v, auto_unbox = TRUE))), error = function(e) FALSE))) next
  Sys.sleep(3)
  poll_until(function() isTRUE(tryCatch(ev("document.querySelectorAll('.recalculating').length === 0"), error = function(e) TRUE)),
             timeout_s = 90, interval_s = 1)
  Sys.sleep(3)
  # A tab whose page closes the browser session: NOT COVERED, then a new session.
  if (inherits(tryCatch(ev("window.scrollTo(0, 0); true"), error = function(e) e), "error")) {
    cat(sprintf("      | NOT COVERED %s: the browser session closed on this tab; a new one goes on\n", v))
    SIN_CUBRIR <- c(SIN_CUBRIR, v)
    if (!ok(sprintf("a new session after %s signs in again in Marca", v), sesion())) fin()
    next
  }
  medir_vista(v)
}
# TEMA 6-10: the other .hd-marca views. The ☰ menu panel open
# (R/ui_components.R) and the new-request form of Requisiciones
# (.hdreq-form.hd-marca; the folders panel is in the REQ tab above).
# Each one that does not open is NOT COVERED, never "0 outside".
abre <- function(js, espera) isTRUE(tryCatch(ev(js), error = function(e) FALSE)) &&
  poll_until(function() isTRUE(tryCatch(ev(espera), error = function(e) FALSE)), timeout_s = 30, interval_s = 1)
if (abre("(function(){ var b = document.getElementById('hd-menu-btn'); if (b && b.offsetParent !== null) { b.click(); return true; }
           var t = document.querySelector('.navbar-toggler'); if (t && t.offsetParent !== null) { t.click(); return true; } return false; })()",
         "(function(){ var p = document.querySelector('nav.navbar .navbar-collapse.hd-marca');
           return !!p && p.getClientRects().length > 0 && getComputedStyle(p).display !== 'none'; })()")) {
  Sys.sleep(1.5); medir_vista("MENU")
  tryCatch(ev("(function(){ var b = document.getElementById('hd-menu-btn'); var t = document.querySelector('.navbar-toggler');
    (b && b.offsetParent !== null ? b : t).click(); return true; })()"), error = function(e) FALSE)
  Sys.sleep(1)
} else { cat("      | NOT COVERED MENU: the .hd-marca menu panel did not open\n"); SIN_CUBRIR <- c(SIN_CUBRIR, "MENU") }
# Requisiciones' two forms (TEMA 6-6). The new-request form is NOT .hd-marca
# (its evidence zone .hdreq-zona says it opened; waiting for .hd-marca there
# left it NOT COVERED in T6-10); the folder form (the panel's "+") is
# .hdreq-form.hd-marca. Nothing is written: both close without Guardar.
cerrar_req <- function() {
  tryCatch(ev("(function(){ var b = document.querySelector('.modal.show .hd-modal-cerrar, .modal.show .btn-close'); if (b) b.click(); return true; })()"),
           error = function(e) FALSE)
  poll_until(function() identical(tryCatch(as.integer(ev("document.querySelectorAll('.modal.show').length")), error = function(e) NA_integer_), 0L),
             timeout_s = 15, interval_s = 0.5)
  Sys.sleep(1)
}
req_abierta <- "REQ" %in% pestanas &&
  isTRUE(tryCatch(ev(sprintf("(%s)(%s)", MOSTRAR_JS, jsonlite::toJSON("REQ", auto_unbox = TRUE))), error = function(e) FALSE)) &&
  { Sys.sleep(4); TRUE }
if (req_abierta &&
    abre("(function(){ var b = document.getElementById('req-nueva'); if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()",
         "(function(){ var z = document.querySelector('.modal.show .hdreq-zona'); return !!z && z.getClientRects().length > 0; })()")) {
  Sys.sleep(2); medir_vista("REQ_NUEVA"); cerrar_req()
} else { cat("      | NOT COVERED REQ_NUEVA: the new-request form did not open\n"); SIN_CUBRIR <- c(SIN_CUBRIR, "REQ_NUEVA") }
if (req_abierta &&
    abre("(function(){ var b = document.getElementById('req-carpeta_nueva'); if (!b || b.getClientRects().length === 0) return false; b.click(); return true; })()",
         "(function(){ var f = document.querySelector('.modal.show .hdreq-form.hd-marca'); return !!f && f.getClientRects().length > 0; })()")) {
  Sys.sleep(2); medir_vista("REQ_CARPETA"); cerrar_req()
} else { cat("      | NOT COVERED REQ_CARPETA: the folder form did not open\n"); SIN_CUBRIR <- c(SIN_CUBRIR, "REQ_CARPETA") }
todo <- if (length(filas)) do.call(rbind, filas) else
  data.frame(pestana = character(), propiedad = character(), color = character(), alfa = numeric(), n = integer(), ejemplos = character())
write.table(todo, file.path(SALIDA, "fuera_de_marca.tsv"), sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
cat(sprintf("      | %d (tab, property, colour) outside Marca; detail in %s\n", nrow(todo), file.path(SALIDA, "fuera_de_marca.tsv")))
if (length(SIN_CUBRIR)) cat(sprintf("Vistas NO CUBIERTAS: %d (%s)\n", length(SIN_CUBRIR), paste(SIN_CUBRIR, collapse = ", ")))
ok("still Marca after every tab", identical(tema_de(), "marca"))
fin()
