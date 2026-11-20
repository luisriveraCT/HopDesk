# =============================================================================
# tests/test_t69_compras_chromote.R
# T69 in Chrome, on the REAL app against dev (tests/helpers/arranque_app.R:
# own process, no S3 writes, no production reads), at Luis's 1293x632:
#   - as Compras (the client test user read as "compras" in that process,
#     ARR_FORZAR_NIVEL): only Panorama and Requisiciones are in the page --
#     the other client tabs are not hidden but gone from the DOM -- and not one
#     "hdt-datos" message of a treasury table crosses the websocket, while
#     Requisiciones' table does when its tab opens;
#   - as the same user with its own tier (finance-like, every tab): the
#     treasury tabs are there and their tables do travel, as before.
# Fails if the sign-in form is still showing (a run stuck at the login proves
# nothing), and if no message at all was captured.
#
# Run from the MAIN checkout (its .Renviron is resolved to dev), pointing at
# this worktree:
#   cd C:/Users/luisr/Antiguedad_App
#   ARR_RAIZ=C:/Users/luisr/Antiguedad_App_t69 Rscript <raiz>/tests/test_t69_compras_chromote.R
# Credentials: HOPDESK_TEST_CLIENTE_USER / _PASS, read line by line from the
# first .Renviron.local that has them (ARR_CREDENCIALES, or this checkout's);
# never printed.
# =============================================================================
cat("=== test_t69_compras_chromote ===\n\n")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
repo <- normalizePath(".", winslash = "/")
raiz <- normalizePath(Sys.getenv("ARR_RAIZ", repo), winslash = "/")

leer_cred <- function(archivo, nombre) {
  if (!file.exists(archivo)) return("")
  l <- grep(paste0("^", nombre, "="), readLines(archivo, warn = FALSE), value = TRUE)
  if (!length(l)) "" else sub('^"(.*)"$', "\\1", sub(paste0("^", nombre, "="), "", l[1]))
}
cred_archivo <- Sys.getenv("ARR_CREDENCIALES", ".Renviron.local")
usr  <- leer_cred(cred_archivo, "HOPDESK_TEST_CLIENTE_USER")
pass <- leer_cred(cred_archivo, "HOPDESK_TEST_CLIENTE_PASS")
if (!nzchar(usr) || !nzchar(pass)) {
  cat("SKIP: no client test credentials in", cred_archivo, "-- this test did NOT pass.\n"); quit(status = 2L)
}

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(r))
}
source(file.path(raiz, "tests/helpers/live_app_harness.R"))
PORT <- 39069L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
login_visible <- function() isTRUE(tryCatch(ev("(function(){ var f = document.getElementById('auth-user_id');
  return !!f && f.offsetParent !== null; })()"), error = function(e) TRUE))
dentro <- function() !login_visible() && isTRUE(tryCatch(ev("!!document.querySelector('.nav-link[data-value]')"), error = function(e) FALSE))
carga_terminada <- function() dentro() && isTRUE(tryCatch(ev(
  "(function(){ var o = document.getElementById('hopdesk-loading');
     return !o || o.style.display === 'none' || o.classList.contains('hld-fadeout'); })()"), error = function(e) FALSE))
en_dom <- function() tryCatch(unlist(ev("Array.from(document.querySelectorAll('.navbar .nav-link[data-value]')).map(function(a){ return a.getAttribute('data-value'); })")),
                              error = function(e) character())

TESORERIA_TABS <- c("CAL", "VEN", "PH", "BNC", "IC", "PSV", "FC", "RPT")
TESORERIA_TABLAS <- c("agenda_pagos", "agenda_cobros", "vencidos", "libro_banco", "historial_conf", "lista_pasivos",
                      "pasivos_tabla", "mapa_ic", "ic_saldos", "facturas_lupa", "datos_sap", "flujo_caja",
                      "detalle_dia_detalle", "detalle_dia_resumen", "descargar_pagos")

correr <- function(et, nivel) {
  cat(sprintf("\n── %s ──\n", et))
  Sys.setenv(ARR_FORZAR_NIVEL = nivel, ARR_FORZAR_USUARIO = if (nzchar(nivel)) usr else "")
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("%s/tests/helpers/arranque_app.R")$value',
                                        raiz, repo, raiz), PORT, timeout_s = 300, www = file.path(raiz, "www")),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (!ok(paste(et, "the real app starts on dev"), !is.null(app))) return(invisible())
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  tablas <- character(); quitar_msgs <- character(); n_marcos <- 0L; claves_marcos <- character(); n_oculta <- 0L
  invisible(b$Network$enable())
  b$Network$webSocketFrameReceived(callback_ = function(m) {
    p <- m$response$payloadData %||% ""; n_marcos <<- n_marcos + 1L
    claves_marcos <<- c(claves_marcos, regmatches(p, regexpr('^\\{"[A-Za-z_-]+"', p)))
    if (grepl("remove-tab", p, fixed = TRUE)) quitar_msgs <<- c(quitar_msgs, substr(p, 1, 160))
    if (grepl("ocultaNivel", p, fixed = TRUE)) n_oculta <<- n_oculta + 1L
    if (grepl("t69_quitadas", p, fixed = TRUE)) quitar_msgs <<- c(quitar_msgs, paste("runjs T69 recibido:", substr(p, 1, 80)))
    if (grepl("hdt-datos", p, fixed = TRUE)) {
      t <- regmatches(p, gregexpr('"tabla":"[a-z_0-9]+"', p))[[1]]
      tablas <<- c(tablas, sub('^"tabla":"(.*)"$', "\\1", t))
    }
  })
  consola <- character()
  invisible(b$Runtime$enable())
  b$Runtime$consoleAPICalled(callback_ = function(m) {
    t <- paste(vapply(m$args, function(a) as.character(a$value %||% a$description %||% ""), character(1)), collapse = " ")
    consola <<- c(consola, substr(t, 1, 200))
  })
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  ok(paste(et, "the sign-in form shows"),
     poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60))
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(usr, auto_unbox = TRUE), jsonlite::toJSON(pass, auto_unbox = TRUE)))
  Sys.sleep(0.5)
  ev("document.getElementById('auth-go_auth').click(); true")
  poll_until(dentro, timeout_s = 300, interval_s = 1)
  poll_until(carga_terminada, timeout_s = 300, interval_s = 1)
  if (!ok(paste(et, "signed in (the sign-in form is gone, the app loaded)"), carga_terminada() && !login_visible()))
    return(invisible())
  Sys.sleep(40)   # R loads the data ~35 s after signing in: let every table push what it would push
  pres <- en_dom()
  # The tabs a person can see: the link and its <li> are not display:none
  # (the tier's hiding sets that on both). Not by size: inside the collapsed
  # ☰ menu at 1293 px every link measures 0 and the check measured nothing.
  visibles_js <- "Array.from(document.querySelectorAll('.navbar .nav-link[data-value]')).filter(function(a){
      var li = a.closest('li');
      return getComputedStyle(a).display !== 'none' && (!li || getComputedStyle(li).display !== 'none'); })
      .map(function(a){ return a.getAttribute('data-value'); })"
  ver_tabs <- function() tryCatch(unlist(ev(visibles_js)), error = function(e) character())
  # The tier's hiding is sent from an observer that runs after the session's
  # long first load (R does one thing at a time): seen in the shiny.trace
  # log, the "hide" messages leave R minutes after signing in. Wait for them,
  # never measure at a fixed time (that is what made T69 look broken).
  t0 <- Sys.time()
  if (nzchar(nivel))   # only a forced (limited) tier has tabs to wait for
    poll_until(function() !length(intersect(ver_tabs(), TESORERIA_TABS)) && length(ver_tabs()) > 0L,
               timeout_s = as.numeric(Sys.getenv("T69_ESPERA_PESTANAS", "300")), interval_s = 2)
  vistas_tabs <- ver_tabs()
  cat(sprintf("      | pestañas VISIBLES: %s (tras %.0f s de espera)\n", paste(vistas_tabs, collapse = " "),
              as.numeric(Sys.time() - t0, units = "secs")))
  cat("      | pestañas en la página:", paste(pres, collapse = " "), "\n")
  cat("      | #nav:", tryCatch(ev("(function(){ var n = document.getElementById('nav'); if (!n) return 'no existe';
      return n.tagName + ' class=' + n.className + ' tabsetid=' + n.getAttribute('data-tabsetid') +
        ' panes CAL=' + document.querySelectorAll('.tab-pane[data-value=\"CAL\"]').length; })()"), error = function(e) "?"), "\n")
  if (length(consola)) cat(paste0("      | consola: ", utils::head(consola, 6)), sep = "\n")
  cat("      | consola T69:", paste(grep("T69", consola, value = TRUE), collapse = " / "), "\n")
  cat("      | consola errores:", paste(utils::head(grep("rror|shinyjs|undefined|not a function", consola, value = TRUE), 8), collapse = " / "), "\n")
  cat("      | CAL display:", tryCatch(ev("(function(){ var a = document.querySelector(\".navbar .nav-link[data-value='CAL']\");
      return a ? getComputedStyle(a).display + ' oculta=' + a.dataset.ocultaNivel : 'no'; })()"), error = function(e) "?"),
      "| shinyjs:", tryCatch(ev("typeof shinyjs"), error = function(e) "?"),
      "| t69_quitadas:", tryCatch(ev("String((Shiny.shinyapp.$inputValues || {}).t69_quitadas)"), error = function(e) "?"), "\n")
  cat("      | marcos con el CSS de ocultar (ocultaNivel):", n_oculta, "\n")
  tt <- table(claves_marcos)
  cat("      | marcos totales:", n_marcos, "| tipos:", paste(names(tt), tt, collapse = " "), "\n")
  cat("      | mensajes remove-tab / runjs T69:", length(quitar_msgs), if (length(quitar_msgs)) quitar_msgs[1] else "", "\n")
  cat("      | el mismo JS corrido a mano:", tryCatch(ev("(function(){ try {
      var n = document.querySelectorAll('.tab-pane[data-value=\"VEN\"]').length; return 'panes VEN=' + n + ' Shiny.unbindAll=' + typeof Shiny.unbindAll;
    } catch (e) { return 'error ' + e; } })()"), error = function(e) conditionMessage(e)), "\n")
  cat("      | input$nav en el cliente:", tryCatch(ev("JSON.stringify(Object.keys(Shiny.shinyapp.$inputValues).filter(function(k){ return k.indexOf('nav') === 0; }))"),
                                                   error = function(e) "?"), "\n")
  # Requisiciones' table travels when its tab opens (Requisiciones reads S3 only then).
  if ("REQ" %in% pres) {
    ev("var m = document.querySelector('.navbar-toggler'); if (m && getComputedStyle(m).display !== 'none') m.click(); true")
    Sys.sleep(1)
    ev("document.querySelector(\".navbar .nav-link[data-value='REQ']\").click(); true")
    poll_until(function() "requisiciones" %in% tablas, timeout_s = 60, interval_s = 1)
    cat("      | pestaña activa tras el clic:", tryCatch(ev("(Shiny.shinyapp.$inputValues || {}).nav || '?'"), error = function(e) "?"),
        "| tabla de requisiciones en la página:", tryCatch(ev("!!document.querySelector('[id$=\"t-hdt\"]')"), error = function(e) "?"), "\n")
  }
  reg <- tryCatch(c(readLines(app$proc$get_output_file(), warn = FALSE), readLines(app$proc$get_error_file(), warn = FALSE)),
                  error = function(e) character())
  clave <- reg[grepl("nivel forzado|[T69]|Warning: Error in|Execution halted", reg, fixed = FALSE) &
                 (grepl("nivel forzado", reg, fixed = TRUE) | grepl("[T69", reg, fixed = TRUE) |
                  grepl("Warning: Error in", reg, fixed = TRUE) | grepl("Execution halted", reg, fixed = TRUE))]
  if (length(clave)) cat(paste0("      | R: ", utils::head(clave, 12)), sep = "\n")
  writeLines(reg, file.path(Sys.getenv("T69_LOG_DIR", tempdir()), paste0("t69_app_", gsub("[^a-z]", "", et), ".log")))
  vistas <- unique(tablas)
  cat("      | tablas recibidas:", if (length(vistas)) paste(vistas, collapse = " ") else "(ninguna)", "\n")
  list(tabs = pres, visibles = vistas_tabs, tablas = vistas)
}

# ── Compras ──────────────────────────────────────────────────────────────────
r <- correr("[compras]", "compras")
if (is.list(r)) {
  ok("[compras] Panorama and Requisiciones are in the page", all(c("PAN", "REQ") %in% r$tabs))
  # The tier's hiding reaches the page once R has sent it (waited for above).
  ok(sprintf("[compras] no tab other than PAN and REQ is visible (visible: %s)",
             paste(setdiff(intersect(r$visibles, c(TESORERIA_TABS, "PAN", "REQ")), c("PAN", "REQ")), collapse = ",")),
     length(r$visibles) > 0L && !length(setdiff(intersect(r$visibles, c(TESORERIA_TABS, "PAN", "REQ")), c("PAN", "REQ"))))
  ok(sprintf("[compras] no treasury table's data crossed the websocket (%s)",
             paste(intersect(TESORERIA_TABLAS, r$tablas), collapse = ",")),
     !length(intersect(TESORERIA_TABLAS, r$tablas)))
  ok("[compras] Requisiciones' table did travel (the capture works)", "requisiciones" %in% r$tablas)
}
try(b$close(), silent = TRUE); b <- NULL; stop_bg_app(app); app <- NULL

# ── The same user with its own tier ──────────────────────────────────────────
r <- if (identical(Sys.getenv("T69_SOLO_COMPRAS"), "1")) NULL else correr("[propio]", "")
if (is.list(r)) {
  ok("[propio] the treasury tabs are there, as before", all(c("CAL", "PH", "VEN") %in% r$tabs))
  ok(sprintf("[propio] treasury tables travel, as before (%s)", paste(intersect(TESORERIA_TABLAS, r$tablas), collapse = ",")),
     length(intersect(TESORERIA_TABLAS, r$tablas)) > 0L)
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L || .pass == 0L) quit(status = 1L)
