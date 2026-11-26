# =============================================================================
# tests/test_t107_botones.R
#
# T107 (2026-11-25): every button answers the instant it is clicked
# (www/hd_botones.js; hd_boton(), hd_espera_attr(), hd_boton_listo() and
# HD_BOTONES_SIN_ESPERA in R/ui_components.R).
#   J. the JS, in node with a fake page: held in the click's own frame
#      (pressed, inert, aria-busy; never .disabled), released on the idle after the
#      server's busy -- to NEUTRAL; a check or a cross only when the server
#      says so (hd-boton message, or an error notification); the 60 s cap;
#      "servidor" and "no"; closing buttons never held; a link; a double click.
#   G. the guardian: the JS and CSS load on the app page only; every
#      exception has its reason and exists; a tags$button that sets an input
#      says how it waits (ratchet per file, to migrate by batches); the old
#      server-side guard (shinyjs::disable + on.exit(enable)) does not grow.
# Run from project root.
# =============================================================================
cat("=== test_t107_botones ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

# ── J. the JS ─────────────────────────────────────────────────────────────────
ruta_js <- normalizePath("www/hd_botones.js", winslash = "/")
node <- Sys.which("node")
correr_node <- function(lineas) {
  js <- tempfile(fileext = ".js")
  writeLines(c(
    "var vm = require('vm'), fs = require('fs');",
    sprintf("var CODIGO = fs.readFileSync(%s, 'utf8');", jsonlite::toJSON(ruta_js, auto_unbox = TRUE)),
    # a fake page: elements with classList / attributes / closest / parent,
    # a click listener, jQuery's $(document).on, a MutationObserver, Shiny's
    # message handlers, and timers that only run when the test says so.
    "function pagina() {",
    "  var P = { ahora: 1000, timers: [], on: {}, clic: null, mo: null, handlers: {}, porId: {} };",
    "  function el(tag, clases, attrs, padre) {",
    "    var e = { tagName: tag, parentElement: padre || null, attrs: attrs || {}, cls: new Set((clases || '').split(' ').filter(Boolean)) };",
    "    e.classList = { add: function () { for (var i = 0; i < arguments.length; i++) e.cls.add(arguments[i]); },",
    "      remove: function () { for (var i = 0; i < arguments.length; i++) e.cls.delete(arguments[i]); },",
    "      contains: function (c) { return e.cls.has(c); } };",
    "    e.getAttribute = function (a) { return a in e.attrs ? e.attrs[a] : null; };",
    "    e.setAttribute = function (a, v) { e.attrs[a] = String(v); };",
    "    e.removeAttribute = function (a) { delete e.attrs[a]; };",
    "    e.hasAttribute = function (a) { return a in e.attrs; };",
    "    if (tag === 'BUTTON') e.disabled = false;",
    "    e.closest = function (sel) {",   # only the selector hd_botones.js uses
    "      for (var x = e; x; x = x.parentElement) if (x.cls.has('action-button') || x.hasAttribute('data-hd-espera')) return x;",
    "      return null; };",
    "    if (attrs && attrs.id) P.porId[attrs.id] = e;",
    "    return e; }",
    "  P.el = el;",
    "  var doc = { body: {}, addEventListener: function (t, f, cap) { if (t === 'click') { if (cap) P.clicCaptura = f; else P.clic = f; } },",
    "    getElementById: function (id) { return P.porId[id] || null; } };",
    "  var $ = function () { return { on: function (ev, f) { P.on[ev] = f; } }; };",
    "  var sb = { window: {}, document: doc, console: { error: function () {} }, JSON: JSON,",
    "    Date: { now: function () { return P.ahora; } },",
    "    setTimeout: function (f, ms) { var t = { f: f, en: P.ahora + ms, hecho: false }; P.timers.push(t); return t; },",
    "    clearTimeout: function (t) { if (t) t.hecho = true; }, setInterval: function () { return 0; }, clearInterval: function () {},",
    "    MutationObserver: function (cb) { P.mo = cb; this.observe = function () {}; } };",
    "  sb.window.jQuery = $;",
    "  sb.window.Shiny = { addCustomMessageHandler: function (n, f) { P.handlers[n] = f; } };",
    "  sb.Shiny = sb.window.Shiny;",
    "  vm.runInNewContext(CODIGO, sb);",
    "  P.B = sb.window.hdBotones;",
    "  P.avanzar = function (ms) { P.ahora += ms; P.timers.forEach(function (t) { if (!t.hecho && t.en <= P.ahora) { t.hecho = true; t.f(); } }); };",
    # a click as the browser runs it: document capture, then the target (Shiny's
    # binding listens on the button: counted in e.llegaron), then document bubble.
    # A disabled <button> gets no click at all.
    "  P.clicar = function (e) {",
    "    if (e.disabled) return;",
    "    var parado = false, ev = { target: e, preventDefault: function () {},",
    "      stopPropagation: function () { parado = true; }, stopImmediatePropagation: function () { parado = true; } };",
    "    if (P.clicCaptura) P.clicCaptura(ev);",
    "    if (parado) return;",
    "    for (var x = e; x; x = x.parentElement) x.llegaron = (x.llegaron || 0) + 1;",
    "    P.clic(ev); };",
    "  P.busy = function () { P.on['shiny:busy'](); }; P.idle = function () { P.on['shiny:idle'](); };",
    "  P.errorAviso = function () { P.mo([{ addedNodes: [{ classList: { contains: function (c) { return c === 'shiny-notification-error'; } } }] }]); };",
    "  P.estado = function (e) { return { ocupado: e.cls.has('hd-btn-ocupado'), aria: e.getAttribute('aria-busy'), disabled: !!e.disabled,",
    "    ok: e.cls.has('hd-btn-ok'), error: e.cls.has('hd-btn-error'), inerte: e.cls.has('hd-btn-inerte'),",
    "    aria_dis: e.getAttribute('aria-disabled'), llegaron: e.llegaron || 0 }; };",
    "  return P; }",
    lineas), js)
  jsonlite::fromJSON(paste(system2(node, js, stdout = TRUE), collapse = ""), simplifyVector = FALSE)
}

if (!nzchar(node)) {
  cat("SKIP (J): node no está instalado\n")
} else {
  r <- correr_node(c(
    "var r = {};",
    # 1. the plain cycle: click -> held -> busy -> idle -> neutral
    "var P = pagina(), b = P.el('BUTTON', 'btn action-button', { id: 'guardar' });",
    "P.clicar(b); r.al_clic = P.estado(b);",
    "P.avanzar(200); P.busy(); P.avanzar(800); r.trabajando = P.estado(b);",
    "P.idle(); r.al_idle = P.estado(b);",
    # 2. an idle from earlier work (no busy since the click) does not release it
    "P = pagina(); b = P.el('BUTTON', 'btn action-button'); P.clicar(b); P.idle(); r.idle_viejo = P.estado(b);",
    "P.busy(); P.idle(); r.idle_suyo = P.estado(b);",
    # 3. clicked while the server is already busy: the first idle releases it
    "P = pagina(); b = P.el('BUTTON', 'btn action-button'); P.busy(); P.avanzar(100); P.clicar(b); P.avanzar(3000);",
    "r.ocupado_largo = P.estado(b); P.idle(); r.tras_idle_ocupado = P.estado(b);",
    # 4. the server never got busy on it (an input nobody observes): 1.5 s
    "P = pagina(); b = P.el('BUTTON', 'btn action-button'); P.clicar(b); P.avanzar(1400); r.antes_15 = P.estado(b); P.avanzar(200); r.tras_15 = P.estado(b);",
    # 5. an error notification while it works: a cross
    "P = pagina(); b = P.el('BUTTON', 'btn action-button'); P.clicar(b); P.busy(); P.errorAviso(); P.idle(); r.con_error = P.estado(b);",
    "P.avanzar(2000); r.error_pasa = P.estado(b);",
    # 6. the server's own word on an auto button (before the idle): a check
    "P = pagina(); b = P.el('BUTTON', 'btn action-button', { id: 'ok1' }); P.clicar(b); P.busy();",
    "P.handlers['hd-boton']({ id: 'ok1', ok: true }); r.listo_ok = P.estado(b); P.idle(); r.listo_ok_idle = P.estado(b);",
    # 7. 'servidor': the idle does not release it; hd_boton_listo does, ok=false -> cross; the 60 s cap
    "P = pagina(); b = P.el('BUTTON', 'btn action-button', { id: 'xl', 'data-hd-espera': 'servidor' });",
    "P.clicar(b); P.busy(); P.idle(); P.avanzar(5000); r.servidor_idle = P.estado(b);",
    "P.handlers['hd-boton']({ id: 'xl', ok: false }); r.servidor_listo = P.estado(b);",
    "P = pagina(); b = P.el('BUTTON', 'btn action-button', { 'data-hd-espera': 'servidor' }); P.clicar(b);",
    "P.avanzar(59000); r.antes_tope = P.estado(b); P.avanzar(1500); r.tras_tope = P.estado(b);",
    # 8. never held: 'no', a closing button, a modal dismiss; a click on a child; a plain element
    "P = pagina(); var no = P.el('BUTTON', 'btn action-button', { 'data-hd-espera': 'no' }); P.clicar(no); r.no = P.estado(no);",
    "var x = P.el('BUTTON', 'btn-close action-button'); P.clicar(x); r.cerrar = P.estado(x);",
    "var d = P.el('BUTTON', 'btn action-button', { 'data-bs-dismiss': 'modal' }); P.clicar(d); r.dismiss = P.estado(d);",
    "var b2 = P.el('BUTTON', 'btn action-button'), ic = P.el('I', 'fa', {}, b2); P.clicar(ic); r.hijo = P.estado(b2);",
    "var n0 = P.B.pendientes.length, div = P.el('DIV', 'cualquier'); P.clicar(div); r.nada = P.B.pendientes.length - n0;",
    # 9. a disabled button stays disabled after; a link takes no pointer events; a double click holds once
    "P = pagina(); var dis = P.el('BUTTON', 'btn action-button'); dis.disabled = true; P.clicar(dis); r.ya_deshabilitado = P.estado(dis);",
    "var a = P.el('A', 'action-button'); P.clicar(a); r.enlace = P.estado(a); P.busy(); P.idle(); r.enlace_suelto = P.estado(a);",
    "var dd = P.el('BUTTON', 'btn action-button'); P.clicar(dd); P.clicar(dd); r.doble = P.B.pendientes.length; r.doble_llegaron = dd.llegaron;",
    # 11. the server disables it while it waits (shinyjs::toggleState, ok = FALSE):
    # still disabled after the idle; and one it enables while it waits stays enabled
    "P = pagina(); var sv = P.el('BUTTON', 'btn action-button'); P.clicar(sv); P.busy(); sv.disabled = true; P.idle(); r.servidor_deshabilita = P.estado(sv);",
    "P = pagina(); var sv2 = P.el('BUTTON', 'btn action-button', { id: 'sv2' }); P.busy(); P.clicar(sv2); sv2.disabled = true; P.handlers['hd-boton']({ id: 'sv2', ok: true }); r.servidor_deshabilita_listo = P.estado(sv2);",
    "P = pagina(); var sv3 = P.el('BUTTON', 'btn action-button'); P.clicar(sv3); sv3.disabled = true; P.avanzar(1600); r.sin_espera_deshabilitado = P.estado(sv3);",
    # 12. aria-disabled that was there before is put back; one held is never .disabled
    "P = pagina(); var ar = P.el('BUTTON', 'btn action-button', { 'aria-disabled': 'false' }); P.clicar(ar); r.aria_retenido = P.estado(ar); P.busy(); P.idle(); r.aria_suelto = P.estado(ar);",
    # 10. a tags$button of our own with data-hd-espera="auto"
    "var pb = P.el('BUTTON', 'pan-boton', { 'data-hd-espera': 'auto' }); P.clicar(pb); r.propio = P.estado(pb);",
    "console.log(JSON.stringify(r));"))
  # held = pressed + inert (no pointer events) + aria-disabled; .disabled is
  # never the JS's (T107a): it stays whatever it was unless the test says so
  e <- function(x, ocupado, disabled = FALSE, ok = FALSE, error = FALSE)
    identical(x$ocupado, ocupado) && identical(x$inerte, ocupado) &&
      identical(identical(x$aria_dis, "true"), ocupado) &&
      identical(x$disabled, disabled) && identical(x$ok, ok) && identical(x$error, error)
  ok("J1 the click itself holds the button: pressed, inert, aria-busy and aria-disabled (never .disabled)",
     e(r$al_clic, TRUE) && identical(r$al_clic$aria, "true"))
  ok("J2 it stays held while the server works, and the idle releases it to NEUTRAL (no check by inference)",
     e(r$trabajando, TRUE) && e(r$al_idle, FALSE) && is.null(r$al_idle$aria))
  ok("J3 an idle from earlier work does not release it; the idle after its own busy does",
     e(r$idle_viejo, TRUE) && e(r$idle_suyo, FALSE))
  ok("J4 clicked while the server is already busy: held for as long as it works, released on the first idle",
     e(r$ocupado_largo, TRUE) && e(r$tras_idle_ocupado, FALSE))
  ok("J5 a click the server never got busy on is released after 1.5 s, neutral",
     e(r$antes_15, TRUE) && e(r$tras_15, FALSE))
  ok("J6 an error notification while it works: a cross on release, gone after a moment",
     e(r$con_error, FALSE, error = TRUE) && e(r$error_pasa, FALSE))
  ok("J7 the server's own word (hd_boton_listo ok): a check, and the idle after changes nothing",
     e(r$listo_ok, FALSE, ok = TRUE) && e(r$listo_ok_idle, FALSE, ok = TRUE))
  ok("J8 'servidor': the idle does not release it; hd_boton_listo(ok = FALSE) does, with a cross",
     e(r$servidor_idle, TRUE) && e(r$servidor_listo, FALSE, error = TRUE))
  ok("J9 never stuck: released at 60 s, neutral", e(r$antes_tope, TRUE) && e(r$tras_tope, FALSE))
  ok("J10 never held: data-hd-espera='no', a closing button (.btn-close), a modal dismiss",
     e(r$no, FALSE) && e(r$cerrar, FALSE) && e(r$dismiss, FALSE))
  ok("J11 a click on the button's icon holds the button; a click on anything else holds nothing",
     e(r$hijo, TRUE) && identical(r$nada, 0L))
  ok("J12 a disabled button is left alone; a link is made inert (no 'disabled' on an <a>) and released",
     e(r$ya_deshabilitado, FALSE, disabled = TRUE) && isTRUE(r$enlace$ocupado) && isTRUE(r$enlace$inerte) &&
       !isTRUE(r$enlace_suelto$ocupado) && !isTRUE(r$enlace_suelto$inerte))
  ok("J13 a double click holds it once, and the second click never reaches the button (stopped in the capture phase)",
     identical(r$doble, 1L) && identical(r$doble_llegaron, 1L))
  ok("J14 a tags$button of our own with data-hd-espera is held too", e(r$propio, TRUE))
  ok("J15 the server disables it while it waits (toggleState, ok = FALSE): still disabled after the idle",
     e(r$servidor_deshabilita, FALSE, disabled = TRUE))
  ok("J16 the same when released by hd_boton_listo or by the 1.5 s without busy",
     e(r$servidor_deshabilita_listo, FALSE, disabled = TRUE, ok = TRUE) && e(r$sin_espera_deshabilitado, FALSE, disabled = TRUE))
  ok("J17 an aria-disabled set before the click is put back on release",
     isTRUE(r$aria_retenido$ocupado) && identical(r$aria_retenido$aria_dis, "true") &&
       identical(r$aria_suelto$aria_dis, "false") && !isTRUE(r$aria_suelto$ocupado))
}

# ── G. the guardian ───────────────────────────────────────────────────────────
cat("\nG. the guardian\n")
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
i_js <- grep('src = "hd_botones.js', app, fixed = TRUE); i_css <- grep('href = "hd_botones.css', app, fixed = TRUE)
i_ov <- grep('overlay_init\\.js\\?v=[0-9]+", `data-pagina` = "app"', app)
# once, and next to the app page's overlay (the sign-in page's tags_top loads
# the overlay without data-pagina, and never hd_botones)
ok("G1 hd_botones.js and .css load once, on the app page (next to the overlay's data-pagina=\"app\"), never on the sign-in page",
   length(i_js) == 1L && length(i_css) == 1L && length(i_ov) == 1L && abs(i_js - i_ov) <= 6L && abs(i_css - i_ov) <= 6L)
css <- paste(readLines("www/hd_botones.css", warn = FALSE), collapse = "\n")
ok("G2 the CSS takes its colours from the theme's tokens only (no hex, rgb or named colour)",
   !grepl("#[0-9a-fA-F]{3,8}\\b|rgba?\\(", css) && grepl("var(--hd-", css, fixed = TRUE))

suppressMessages({ library(shiny) })
src_ui <- new.env()
for (e in parse("R/ui_components.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      as.character(e[[2]]) %in% c("hd_boton", "hd_espera_attr", "hd_boton_listo", "HD_BOTONES_SIN_ESPERA"))
    eval(e, src_ui)
ok("G3 hd_boton(): 'auto' is a plain actionButton; 'servidor' and 'no' carry data-hd-espera",
   !grepl("data-hd-espera", as.character(src_ui$hd_boton("a", "A"))) &&
     grepl('data-hd-espera="servidor"', as.character(src_ui$hd_boton("b", "B", espera = "servidor")), fixed = TRUE) &&
     grepl('data-hd-espera="no"', as.character(src_ui$hd_boton("c", "C", espera = "no")), fixed = TRUE))
enviado <- NULL
ses <- list(ns = function(x) paste0("m-", x), sendCustomMessage = function(type, msg) enviado <<- list(type = type, msg = msg))
src_ui$hd_boton_listo(ses, "xl", ok = TRUE); m1 <- enviado
src_ui$hd_boton_listo(ses, "xl", ok = FALSE); m2 <- enviado
src_ui$hd_boton_listo(ses, "xl", ok = NA); m3 <- enviado
ok("G4 hd_boton_listo(): the 'hd-boton' message with the namespaced id, ok TRUE / FALSE / none",
   identical(m1$type, "hd-boton") && identical(m1$msg$id, "m-xl") && isTRUE(m1$msg$ok) &&
     identical(m2$msg$ok, FALSE) && is.null(m3$msg$ok))

archivos <- c("app.R", list.files("R", "\\.R$", full.names = TRUE))
textos <- setNames(lapply(archivos, function(f) readLines(f, warn = FALSE, encoding = "UTF-8")), archivos)
lista <- src_ui$HD_BOTONES_SIN_ESPERA
ok(sprintf("G5 every exception (HD_BOTONES_SIN_ESPERA, %d) has its reason and its id exists in its file", nrow(lista)),
   all(c("id", "archivo", "pantalla", "razon") %in% names(lista)) && all(nzchar(trimws(lista$razon))) &&
     all(vapply(seq_len(nrow(lista)), function(i) any(grepl(lista$id[i], textos[[lista$archivo[i]]] %||% "", fixed = TRUE)), logical(1))))
# every espera = "no" (hd_boton or hd_espera_attr) is in the list, by its id
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
# on the parsed code (a call spans lines): hd_boton(<id>, ..., espera = "no"),
# and a tags$button(id = <id>, ..., hd_espera_attr("no")); <id> a literal or ns("<literal>")
.recorrer <- function(e, f) {
  if (is.call(e)) {
    f(e)
    for (i in seq_along(e)[-1]) { if (is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")) next; .recorrer(e[[i]], f) }
  } else if (is.expression(e) || is.list(e) || is.pairlist(e)) {
    for (i in seq_along(e)) { if (is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")) next; .recorrer(e[[i]], f) }
  }
}
.literal <- function(x) if (is.character(x)) x[1] else if (is.call(x) && length(x) == 2L && is.character(x[[2]])) x[[2]][1] else NA_character_
nos <- unlist(lapply(archivos, function(f) {
  enc <- character(0)
  for (e in tryCatch(parse(f, keep.source = FALSE), error = function(err) NULL)) .recorrer(e, function(cl) {
    cab <- paste(deparse(cl[[1]]), collapse = "")
    a <- as.list(cl)[-1]; nm <- names(a) %||% rep("", length(a))
    if (cab %in% c("hd_boton") && identical(a[["espera"]], "no"))
      enc <<- c(enc, .literal(if ("inputId" %in% nm) a[["inputId"]] else a[[which(!nzchar(nm))[1]]]))
    if (cab %in% c("tags$button", "shiny::tags$button", "htmltools::tags$button") &&
        grepl('hd_espera_attr\\("no"\\)', paste(deparse(cl, width.cutoff = 500L), collapse = " ")))
      enc <<- c(enc, .literal(a[["id"]]))
  })
  if (length(enc)) sprintf("%s:%s", f, enc) else character(0)
}))
ok(sprintf("G6 every espera = \"no\" in the code is in HD_BOTONES_SIN_ESPERA, with its file (%s)", paste(nos, collapse = ", ")),
   length(nos) == nrow(lista) && all(nos %in% sprintf("%s:%s", lista$archivo, lista$id)))

# The ratchets: what is still to migrate, per file. A file may go down (then
# lower its number here); it may never go up, and a new file is a failure.
TAGS_BUTTON_PENDIENTES <- c("app.R" = 1L, "R/empresas_module.R" = 4L, "R/pagar_hoy_module.R" = 2L,
                            "R/panorama_module.R" = 6L, "R/panorama_publicar_module.R" = 3L,
                            "R/requisiciones_module.R" = 4L, "R/tiers_module.R" = 4L)
GUARDA_VIEJA_PENDIENTE <- c("R/bancos_module.R" = 5L, "R/pasivos_module.R" = 1L,
                            "R/pasivos_table_module.R" = 1L, "R/pasivos_wizard_module.R" = 1L)
sin_declarar <- function(f) {
  n <- 0L; donde <- integer(0)
  buscar <- function(e) {
    if (is.call(e)) {
      cab <- paste(deparse(e[[1]]), collapse = "")
      if (cab %in% c("tags$button", "shiny::tags$button", "htmltools::tags$button")) {
        txt <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
        if (grepl("Shiny.setInputValue", txt, fixed = TRUE) && !grepl("data-hd-espera|hd_espera_attr", txt)) n <<- n + 1L
      }
      for (i in seq_along(e)[-1]) {
        if (is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")) next
        buscar(e[[i]])
      }
    } else if (is.expression(e) || is.list(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        if (is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")) next
        buscar(e[[i]])
      }
    }
  }
  for (e in tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)) buscar(e)
  n
}
n_tb <- vapply(archivos, sin_declarar, integer(1)); n_tb <- n_tb[n_tb > 0]
de_mas <- n_tb[!names(n_tb) %in% names(TAGS_BUTTON_PENDIENTES) | n_tb > TAGS_BUTTON_PENDIENTES[names(n_tb)]]
ok(sprintf("G7 a tags$button that sets an input says how it waits (hd_espera_attr()) -- %d still to migrate%s",
           sum(n_tb), if (length(de_mas)) paste0("; NEW in: ", paste(names(de_mas), de_mas, sep = " ", collapse = ", "),
                                                  " -- add hd_espera_attr() to the tags$button") else ""),
   !length(de_mas))
n_gv <- vapply(archivos, function(f) sum(grepl("on.exit\\(shinyjs::enable\\(", textos[[f]])), integer(1)); n_gv <- n_gv[n_gv > 0]
de_mas_gv <- n_gv[!names(n_gv) %in% names(GUARDA_VIEJA_PENDIENTE) | n_gv > GUARDA_VIEJA_PENDIENTE[names(n_gv)]]
ok(sprintf("G8 no new server-side guard (shinyjs::disable + on.exit(enable)): the click already holds the button -- %d left%s",
           sum(n_gv), if (length(de_mas_gv)) paste0("; NEW in: ", paste(names(de_mas_gv), collapse = ", ")) else ""),
   !length(de_mas_gv))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
