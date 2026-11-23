# =============================================================================
# tests/test_tema_cliente_chromote.R
# TEMA stage 5a in the real app (tests/helpers/arranque_app.R: dev bucket,
# and NOTHING is written -- its write functions are no-ops), end to end:
#   Run A (the client has no theme)
#     1  the client user signs in: Clásico;
#     2  HopDesk staff (the test staff account read as principal in this
#        process, ARR_FORZAR_NIVEL, so it may jump) jumps into that client:
#        still Clásico; Ajustes › Grupo › Empresas offers Marca to staff
#        (marked "HopDesk only"); Guardar with Marca turns the page Marca at
#        once (the write itself is omitted by the harness); back home: Clásico.
#   Run B (the client's group_config read with tema = "marca" in this process,
#        ARR_FORZAR_TEMA: what a saved Marca looks like, nothing written)
#     3  the client signs in: Marca; at the SECOND sign-in Marca from the first
#        paint (no Clásico in between); a screenshot of every tab in Marca;
#     4  two sessions at once: staff at home is Clásico while the client's
#        session is Marca; staff jumping into the client sees Marca, and
#        Clásico again back home.
# Screenshots to TEMA_SALIDA.
# Run from project root (credentials in .Renviron / .Renviron.local):
#   TEMA_SALIDA=<folder> Rscript --vanilla tests/test_tema_cliente_chromote.R
# =============================================================================
cat("=== test_tema_cliente_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

usuarios <- list(
  prueba  = c(user = Sys.getenv("HOPDESK_TEST_USER"),         pass = Sys.getenv("HOPDESK_TEST_PASS")),
  cliente = c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS")))
if (!all(nzchar(unlist(usuarios)))) { cat("SKIP: faltan las credenciales de prueba.\n"); quit(status = 2L) }
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "tema_cliente"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))
MARINO <- "#13213A"; AZUL <- "#0A58CA"   # Marca's and Clásico's primario (R/tema_registro.R)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
fin <- function() {
  cerrar_todo()
  cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
  quit(status = if (.fail > 0L) 1L else 0L)
}
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39040L
app <- NULL; pes <- list()
CORRIDA <- 0L
guardar_log <- function() if (!is.null(app)) tryCatch({
  x <- readLines(app$proc$get_error_file(), warn = FALSE, encoding = "UTF-8")
  quien <- tolower(unlist(lapply(usuarios, `[[`, "user")))
  x <- x[grepl("group_config|tema forzado|\\[CTX\\]|escritura omitida|S3_WRITE|Error|error|WARN", x) &
         !Reduce(`|`, lapply(quien, function(u) grepl(u, tolower(x), fixed = TRUE)))]
  writeLines(x, file.path(SALIDA, sprintf("app_log_corrida_%d.txt", CORRIDA)), useBytes = TRUE)
}, error = function(e) message("  (log: ", conditionMessage(e), ")"))
cerrar_todo <- function() {
  guardar_log()
  for (k in names(pes)) try(pes[[k]]$close(), silent = TRUE)
  pes <<- list(); stop_bg_app(app); app <<- NULL
}
# group_config loads late (Phase 2 at sign-in, last of a jump's reads): wait
# for the theme, and say how long it took.
espera_tema <- function(b, valor, etiqueta, tope = 120) {
  t0 <- Sys.time()
  llego <- poll_until(function() identical(tema_de(b), valor), timeout_s = tope, interval_s = 0.5)
  cat(sprintf("      | %s: %s after %.1f s\n", etiqueta, if (llego) valor else paste("still", tema_de(b)),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  llego
}

ev <- function(b, js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
# Every value <html data-hd-tema> takes on a document, from its very start.
TRAZA_JS <- "window.__temas = [];
  new MutationObserver(function(){ var h = document.documentElement, t = h && h.getAttribute('data-hd-tema');
    if (t && window.__temas[window.__temas.length - 1] !== t) window.__temas.push(t); })
  .observe(document, {attributes: true, subtree: true, attributeFilter: ['data-hd-tema']});
  window.__hdTemaMsgs = [];
  document.addEventListener('DOMContentLoaded', function(){ if (window.jQuery) jQuery(document).on('shiny:message', function(e){
    var c = e.message && e.message.custom; if (c && c.hdTema) window.__hdTemaMsgs.push(c.hdTema); }); });"
tema_de <- function(b) tryCatch(ev(b, "document.documentElement.getAttribute('data-hd-tema') || ''"), error = function(e) "?")
primario <- function(b) tryCatch(toupper(ev(b, "getComputedStyle(document.documentElement).getPropertyValue('--hd-primario').trim()")),
                                 error = function(e) "?")
traza <- function(b) unlist(tryCatch(ev(b, "window.__temas || []"), error = function(e) list()))
cliente_visto <- function(b) tryCatch(ev(b, "(function(){ try { return localStorage.getItem('hd-tema-cliente') || ''; } catch(e) { return ''; } })()"),
                                      error = function(e) "")
cargada <- function(b) isTRUE(tryCatch(ev(b,
  "(function(){ var f = document.getElementById('auth-user_id'), o = document.getElementById('hopdesk-loading');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]') &&
            (!o || o.style.display === 'none' || o.classList.contains('hld-fadeout')); })()"), error = function(e) FALSE))
captura <- function(b, nombre) tryCatch({
  s <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0(nombre, ".png")))
}, error = function(e) message("  (captura ", nombre, ": ", conditionMessage(e), ")"))

# The test staff account reads as principal (it may jump to any client); the
# username goes to the app process through the environment, never printed.
arrancar <- function(tema_forzado = "", cliente = "") {
  cerrar_todo(); CORRIDA <<- CORRIDA + 1L
  codigo <- sprintf(paste0('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s", ARR_FORZAR_NIVEL = "principal", ',
                           'ARR_FORZAR_USUARIO = Sys.getenv("HOPDESK_TEST_USER"), ',
                           'ARR_FORZAR_TEMA = "%s", ARR_FORZAR_TEMA_CLIENTE = "%s"); ',
                           'setwd("%s"); source("tests/helpers/arranque_app.R")$value'),
                    repo, tema_forzado, cliente, repo)
  app <<- tryCatch(start_bg_app(codigo, PORT, timeout_s = 300, www = file.path(repo, "www")),
                   error = function(e) { message(conditionMessage(e)); NULL })
  !is.null(app)
}
nueva <- function(nombre) {
  try(pes[[nombre]]$close(), silent = TRUE)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$addScriptToEvaluateOnNewDocument(source = TRAZA_JS))
  pes[[nombre]] <<- b
  b
}
entrar <- function(b, quien) {
  # dev's R may still be busy with the previous session: a long wait, one retry.
  for (k in 1:2) if (!inherits(tryCatch(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE, timeout_ = 120),
                                        error = function(e) e), "error")) break
  if (!poll_until(function() isTRUE(tryCatch(ev(b, "!!document.getElementById('auth-user_id')"), error = function(e) FALSE)),
                  timeout_s = 120)) return(FALSE)
  Sys.sleep(1)
  u <- usuarios[[quien]]
  ev(b, sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5)
  ev(b, "document.getElementById('auth-go_auth').click(); true")
  if (!poll_until(function() cargada(b), timeout_s = LIMITE, interval_s = 1)) return(FALSE)
  # The server's theme message comes once the client's group_config is read
  # (Phase 2, after the overlay is gone).
  t0 <- Sys.time()
  llego <- poll_until(function() mensajes(b) > 0L, timeout_s = LIMITE, interval_s = 0.5)
  cat(sprintf("      | %s: the theme message came %.1f s after the page loaded\n", quien,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  Sys.sleep(2)
  llego
}
mensajes <- function(b) as.integer(tryCatch(ev(b, "(window.__hdTemaMsgs || []).length"), error = function(e) 0L))
saltar <- function(b, cid) {
  n0 <- mensajes(b)
  ev(b, sprintf("Shiny.setInputValue('trs-modal_ctx_select', %s, {priority: 'event'});
                 Shiny.setInputValue('trs-modal_ctx_confirm', Math.random(), {priority: 'event'}); true",
                jsonlite::toJSON(cid, auto_unbox = TRUE)))
  nzchar(cid) && poll_until(function() mensajes(b) > n0 && identical(cliente_visto(b), cid), timeout_s = LIMITE, interval_s = 1) &&
    { Sys.sleep(3); TRUE }
}
# Ajustes › Grupo › Empresas, until the theme selector is drawn.
abrir_empresas <- function(b) {
  ev(b, "(function(){ var a = document.querySelector(\"[data-value='EMP']\"); if (!a) return false;
          bootstrap.Tab.getOrCreateInstance(a).show(); return true; })()")
  poll_until(function() isTRUE(tryCatch(ev(b, "!!document.querySelector(\"#emp-tema_input input[type=radio]\")"),
                                        error = function(e) FALSE)), timeout_s = 120, interval_s = 1)
}
guardar_tema <- function(b, valor) {
  ev(b, "(function(){ var i = document.getElementById('emp-group_name_input');
    if (i && !i.value.trim()) { i.value = 'Grupo de prueba'; $(i).trigger('change'); } return true; })()")
  ev(b, sprintf("(function(){ var r = document.querySelector(\"#emp-tema_input input[type=radio][value='%s']\");
    if (!r) return false; r.click(); $(r).trigger('change'); return true; })()", valor)) &&
    { Sys.sleep(1)
      cat("      | radio checked before Guardar:", tryCatch(ev(b, "(function(){ var r = document.querySelector(\"#emp-tema_input input[type=radio]:checked\");
        return r ? (r.value === '' ? '(automático)' : r.value) : '(ninguno)'; })()"), error = function(e) "?"), "\n")
      ev(b, "document.getElementById('emp-btn_save_group_config').click(); true")
      # What the app answered: its notification (the save path's only voice).
      aviso <- ""
      poll_until(function() {
        aviso <<- tryCatch(ev(b, "Array.from(document.querySelectorAll('.shiny-notification-content'))
          .map(function(n){ return n.textContent.trim(); }).join(' | ')"), error = function(e) "")
        nzchar(aviso) }, timeout_s = 20, interval_s = 0.5)
      cat("      | notification after Guardar:", if (nzchar(aviso)) aviso else "(none in 20 s)", "\n")
      captura(b, "staff_tras_guardar")
      TRUE }
}

# ── Run A: the client has no theme ───────────────────────────────────────────
if (!ok("A the real app starts on dev (staff read as principal)", arrancar())) fin()
bc <- nueva("cliente")
if (!ok("1 the client user signs in", entrar(bc, "cliente"))) fin()
CID <- cliente_visto(bc)
ok("1 the server told the page its client and the theme: Clásico",
   nzchar(CID) && identical(tema_de(bc), "clasico") && identical(primario(bc), AZUL))
try(bc$close(), silent = TRUE); pes$cliente <- NULL

bs <- nueva("staff")
if (ok("2 staff signs in", entrar(bs, "prueba"))) {
  CASA <- cliente_visto(bs)
  ok("2 staff at home: Clásico", identical(tema_de(bs), "clasico") && nzchar(CASA) && !identical(CASA, CID))
  if (ok("2 staff jumps into the client: still Clásico", saltar(bs, CID) && identical(tema_de(bs), "clasico")) &&
      ok("2 Empresas shows the theme selector", abrir_empresas(bs))) {
    ok("2 nothing chosen yet: 'Automático' is selected",
       identical(ev(bs, "(function(){ var r = document.querySelector(\"#emp-tema_input input[type=radio]:checked\");
         return r ? r.value : null; })()"), ""))
    ok("2 staff sees Marca in the selector, marked as HopDesk only",
       isTRUE(ev(bs, "(function(){ var r = document.querySelector(\"#emp-tema_input input[value='marca']\");
         return !!r && !!r.closest('label').parentElement.querySelector('.badge'); })()")))
    captura(bs, "staff_empresas_selector")
    ok("2 Guardar with Marca: the staff page turns Marca at once (attribute and tokens)",
       guardar_tema(bs, "marca") && espera_tema(bs, "marca", "after Guardar, staff") &&
         identical(primario(bs), MARINO))
    captura(bs, "staff_en_cliente_marca")
    ok("2 back home: Clásico", saltar(bs, CASA) && identical(tema_de(bs), "clasico"))
  }
}

# ── Run B: the client's group_config reads with tema = "marca" ──────────────
if (!nzchar(CID) || !ok("B the real app starts again, the client's theme read as Marca", arrancar("marca", CID))) fin()
bc <- nueva("cliente")
ok("3 the client signs in: Marca once loaded", entrar(bc, "cliente") && espera_tema(bc, "marca", "first sign-in, client") &&
     identical(primario(bc), MARINO))
cat("      | first sign-in, every value of data-hd-tema:", paste(traza(bc), collapse = " → "), "\n")
ok("3 second sign-in: Marca from the first paint (no Clásico in between)",
   entrar(bc, "cliente") && espera_tema(bc, "marca", "second sign-in, client") &&
     length(traza(bc)) > 0L && all(traza(bc) == "marca"))
cat("      | second sign-in:", paste(traza(bc), collapse = " → "), "\n")
pestanas <- unlist(tryCatch(ev(bc, "Array.from(document.querySelectorAll('.nav-link[data-value]'))
  .map(function(a){ return a.getAttribute('data-value'); })"), error = function(e) list()))
for (v in setdiff(pestanas, "GRUPO")) {
  hecho <- isTRUE(tryCatch(ev(bc, sprintf("(function(){ var a = document.querySelector(\".nav-link[data-value='%s']\");
    if (!a) return false; bootstrap.Tab.getOrCreateInstance(a).show(); return true; })()", v)), error = function(e) FALSE))
  if (!hecho) next
  Sys.sleep(8); ev(bc, "window.scrollTo(0, 0); true")
  captura(bc, paste0("cliente_marca_", v))
}
ok("3 still Marca after visiting every tab", identical(tema_de(bc), "marca"))

bs <- nueva("staff")
if (ok("4 staff signs in while the client's session is open", entrar(bs, "prueba"))) {
  CASA <- cliente_visto(bs)
  ok("4 at the same time: staff at home is Clásico, the client's session stays Marca",
     identical(tema_de(bs), "clasico") && identical(tema_de(bc), "marca"))
  ok("4 staff jumping into the client sees Marca", saltar(bs, CID) && espera_tema(bs, "marca", "jump, staff") &&
       identical(primario(bs), MARINO))
  captura(bs, "staff_salto_cliente_marca")
  ok("4 back home: Clásico again, the client's session still Marca",
     saltar(bs, CASA) && identical(tema_de(bs), "clasico") && identical(tema_de(bc), "marca"))
}
fin()
