# =============================================================================
# tests/test_tema_cliente_chromote.R
# TEMA stage 5a in the real app (dev bucket), end to end, through the real
# screens only:
#   1  the client user signs in: its client has no theme -> Clásico;
#   2  HopDesk staff jumps into that client: still Clásico; in Ajustes ›
#      Grupo › Empresas the selector offers Marca to staff (marked "HopDesk
#      only"); staff picks Marca and saves -> the page turns Marca at once;
#   3  the client user signs in again: Marca after loading, and at the
#      SECOND sign-in Marca from the first paint (no Clásico flash: the head
#      script read localStorage); a screenshot of every tab in Marca;
#   4  two sessions at once: staff back home (its own client: Clásico) while
#      the client's session stays Marca -- nothing shared between sessions;
#   5  staff jumps in again and puts back the theme the client had (always,
#      even after a failure: a client left in Marca in dev is reported).
# Writes ONE thing, in dev: group_config.rds's `tema` of the test client,
# and puts back what it found. Screenshots to TEMA_SALIDA.
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
MARINO <- "#13213A"   # Marca's primario (R/tema_registro.R)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39040L
app <- NULL; pes <- list()
cerrar_todo <- function() { for (k in names(pes)) try(pes[[k]]$close(), silent = TRUE); stop_bg_app(app) }

ev <- function(b, js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
# Every value <html data-hd-tema> takes on a document, from its very start.
TRAZA_JS <- "window.__temas = [];
  new MutationObserver(function(){ var t = document.documentElement.getAttribute('data-hd-tema');
    if (t && window.__temas[window.__temas.length - 1] !== t) window.__temas.push(t); })
  .observe(document.documentElement, {attributes: true, attributeFilter: ['data-hd-tema']});"
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

nueva <- function(nombre) {
  try(pes[[nombre]]$close(), silent = TRUE)
  b <- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$addScriptToEvaluateOnNewDocument(source = TRAZA_JS))
  pes[[nombre]] <<- b
  b
}
entrar <- function(b, quien) {
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  if (!poll_until(function() isTRUE(tryCatch(ev(b, "!!document.getElementById('auth-user_id')"), error = function(e) FALSE)),
                  timeout_s = 60)) return(FALSE)
  Sys.sleep(1)
  u <- usuarios[[quien]]
  ev(b, sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5)
  ev(b, "document.getElementById('auth-go_auth').click(); true")
  poll_until(function() cargada(b), timeout_s = LIMITE, interval_s = 1) && { Sys.sleep(5); TRUE }
}
saltar <- function(b, cid) {
  ev(b, sprintf("Shiny.setInputValue('trs-modal_ctx_select', %s, {priority: 'event'});
                 Shiny.setInputValue('trs-modal_ctx_confirm', Math.random(), {priority: 'event'}); true",
                jsonlite::toJSON(cid, auto_unbox = TRUE)))
  poll_until(function() identical(cliente_visto(b), cid), timeout_s = LIMITE, interval_s = 1) && { Sys.sleep(8); TRUE }
}
# Ajustes › Grupo › Empresas, until the theme selector is drawn.
abrir_empresas <- function(b) {
  ev(b, "(function(){ var a = document.querySelector(\"[data-value='EMP']\"); if (!a) return false;
          bootstrap.Tab.getOrCreateInstance(a).show(); return true; })()")
  poll_until(function() isTRUE(tryCatch(ev(b, "!!document.querySelector(\"#emp-tema_input input[type=radio]\")"),
                                        error = function(e) FALSE)), timeout_s = 120, interval_s = 1)
}
tema_elegido <- function(b) ev(b, "(function(){ var r = document.querySelector(\"#emp-tema_input input[type=radio]:checked\");
  return r ? r.value : null; })()")
guardar_tema <- function(b, valor) {
  ev(b, sprintf("(function(){ var r = document.querySelector(\"#emp-tema_input input[type=radio][value='%s']\");
    if (!r) return false; r.click(); $(r).trigger('change'); return true; })()", valor)) &&
    { Sys.sleep(1); ev(b, "document.getElementById('emp-btn_save_group_config').click(); true"); Sys.sleep(4); TRUE }
}

# ── Start ────────────────────────────────────────────────────────────────────
app <- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("tests/helpers/arranque_app.R")$value',
                                     repo, repo), PORT, timeout_s = 300, www = file.path(repo, "www")),
                error = function(e) { message(conditionMessage(e)); NULL })
if (!ok("the real app starts on dev", !is.null(app))) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

# ── 1. The client, no theme yet ──────────────────────────────────────────────
bc <- nueva("cliente")
if (!ok("1 the client user signs in", entrar(bc, "cliente"))) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }
CID <- cliente_visto(bc)
ok(sprintf("1 the server told the page its client (%s) and the theme: Clásico", if (nzchar(CID)) "leído" else "vacío"),
   nzchar(CID) && identical(tema_de(bc), "clasico") && identical(primario(bc), "#0A58CA"))
try(bc$close(), silent = TRUE); pes$cliente <- NULL

# ── 2. Staff puts Marca on it ────────────────────────────────────────────────
bs <- nueva("staff")
ORIGINAL <- NULL; PUESTO <- FALSE
restaurar <- function() {
  if (!PUESTO) return(invisible(TRUE))
  b <- nueva("restaura")
  hecho <- entrar(b, "prueba") && saltar(b, CID) && abrir_empresas(b) && guardar_tema(b, ORIGINAL %||% "") &&
    poll_until(function() identical(tema_de(b), if (nzchar(ORIGINAL %||% "")) ORIGINAL else "clasico"), timeout_s = 30)
  ok(sprintf("5 the client's theme is back to what it had (%s)", if (nzchar(ORIGINAL %||% "")) ORIGINAL else "automático"), hecho)
  if (!hecho) cat("      | ATENCIÓN: el cliente de prueba quedó con Marca en dev; ponerlo en Automático desde Empresas.\n")
  PUESTO <<- !hecho
  invisible(hecho)
}
# on.exit does not run in Rscript: steps 2-4 in a tryCatch, restaurar() always after.
tryCatch({
if (ok("2 staff signs in", entrar(bs, "prueba"))) {
  CASA <- cliente_visto(bs)
  ok("2 staff at home: Clásico", identical(tema_de(bs), "clasico") && nzchar(CASA) && !identical(CASA, CID))
  if (ok("2 staff jumps into the client: still Clásico", saltar(bs, CID) && identical(tema_de(bs), "clasico"))) {
    if (ok("2 Empresas shows the theme selector", abrir_empresas(bs))) {
      ORIGINAL <- tema_elegido(bs) %||% ""
      ok("2 staff sees Marca in the selector, marked as HopDesk only",
         isTRUE(ev(bs, "(function(){ var r = document.querySelector(\"#emp-tema_input input[value='marca']\");
           return !!r && !!r.closest('.radio, .form-check, label').parentElement.querySelector('.badge'); })()")))
      captura(bs, "staff_empresas_selector")
      PUESTO <- guardar_tema(bs, "marca")
      ok("2 saved: the staff page turns Marca at once (attribute and tokens)",
         PUESTO && poll_until(function() identical(tema_de(bs), "marca"), timeout_s = 30) && identical(primario(bs), MARINO))
      captura(bs, "staff_en_cliente_marca")
    }
  }
}

# ── 3. The client sees Marca; the second sign-in paints Marca first ─────────
if (PUESTO) {
  bc <- nueva("cliente")
  ok("3 the client signs in: Marca once loaded", entrar(bc, "cliente") && identical(tema_de(bc), "marca") &&
       identical(primario(bc), MARINO))
  cat("      | first sign-in, every value of data-hd-tema:", paste(traza(bc), collapse = " → "), "\n")
  invisible(bc$Page$reload(wait_ = TRUE))
  ok("3 second sign-in: Marca from the first paint (no Clásico in between)",
     entrar(bc, "cliente") && length(traza(bc)) > 0L && all(traza(bc) == "marca"))
  cat("      | second sign-in:", paste(traza(bc), collapse = " → "), "\n")
  # Every tab in Marca, for Luis.
  pestanas <- unlist(tryCatch(ev(bc, "Array.from(document.querySelectorAll('.nav-link[data-value]'))
    .map(function(a){ return a.getAttribute('data-value'); })"), error = function(e) list()))
  for (v in setdiff(pestanas, c("GRUPO"))) {
    hecho <- isTRUE(tryCatch(ev(bc, sprintf("(function(){ var a = document.querySelector(\".nav-link[data-value='%s']\");
      if (!a) return false; bootstrap.Tab.getOrCreateInstance(a).show(); return true; })()", v)), error = function(e) FALSE))
    if (!hecho) next
    Sys.sleep(8); ev(bc, "window.scrollTo(0, 0); true")
    captura(bc, paste0("cliente_marca_", v))
  }
  ok("3 still Marca after visiting every tab", identical(tema_de(bc), "marca"))

  # ── 4. Two sessions at once ────────────────────────────────────────────────
  if (!is.null(pes$staff) && saltar(bs, CASA))
    ok("4 at the same time: staff back home is Clásico, the client's session stays Marca",
       identical(tema_de(bs), "clasico") && identical(tema_de(bc), "marca"))
  else ok("4 staff goes back home", FALSE)
}
}, error = function(e) ok(paste("steps 2-4 ran to the end:", conditionMessage(e)), FALSE))

restaurar()
cerrar_todo(); app <- NULL; pes <- list()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
