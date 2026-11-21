# =============================================================================
# tests/test_t69c_barra_inicial_chromote.R
# T69c in Chrome, on the REAL app against dev (tests/helpers/arranque_app.R:
# own process, no S3 writes, no production reads), at Luis's 1293x632.
# The navbar is measured from the FIRST frame after signing in -- the moment
# a tab link exists in the page -- and then every ~150 ms until the first
# load ends and the server's observer has taken over (hd-nivel-aplicado on
# <html>). Never after a fixed wait.
#   - as Compras (the client test user read as "compras" in that process,
#     ARR_FORZAR_NIVEL): not one sample, the first one included, shows a tab
#     other than PAN and REQ; the first frame already has the CSS; samples
#     were taken while the load was still running (otherwise nothing was
#     measured); after the take-over it is still PAN and REQ only;
#   - as the same user with its own tier (every tab): the first frame has no
#     first-paint CSS and, after the take-over, the treasury tabs show, as
#     before (the counter-proof).
# Visible = the link and its <li> are not display:none. Not by size: inside
# the collapsed ☰ menu at 1293 px every link measures 0.
# Fails if the sign-in form is still showing.
#
# Run from the MAIN checkout (its .Renviron is resolved to dev), pointing at
# this worktree:
#   cd C:/Users/luisr/Antiguedad_App
#   ARR_RAIZ=C:/Users/luisr/Antiguedad_App_t69c Rscript <raiz>/tests/test_t69c_barra_inicial_chromote.R
# Credentials: HOPDESK_TEST_CLIENTE_USER / _PASS, read line by line from the
# first .Renviron.local that has them (ARR_CREDENCIALES, or this checkout's);
# never printed.
# =============================================================================
cat("=== test_t69c_barra_inicial_chromote ===\n\n")
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
PORT <- 39070L
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
login_visible <- function() isTRUE(tryCatch(ev("(function(){ var f = document.getElementById('auth-user_id');
  return !!f && f.offsetParent !== null; })()"), error = function(e) TRUE))

CLIENTE <- c("PAN", "CAL", "VEN", "PH", "BNC", "IC", "PSV", "REQ", "FC", "RPT")
TESORERIA_TABS <- setdiff(CLIENTE, c("PAN", "REQ"))

# One sample: null while no tab link exists yet; otherwise the visible client
# tabs, whether the first paint's CSS is in the page, whether the server has
# taken over, and whether the loading overlay is still up.
MUESTRA_JS <- "(function(){
  var links = document.querySelectorAll('.navbar .nav-link[data-value]');
  if (!links.length || document.getElementById('auth-user_id')) return null;
  var vis = Array.from(links).filter(function(a){
      var li = a.closest('li');
      return getComputedStyle(a).display !== 'none' && (!li || getComputedStyle(li).display !== 'none'); })
    .map(function(a){ return a.getAttribute('data-value'); });
  var o = document.getElementById('hopdesk-loading');
  return { vis: vis.join(' '),
           css: !!document.getElementById('hd-t69c-pestanas'),
           aplicado: document.documentElement.classList.contains('hd-nivel-aplicado'),
           cargando: !!o && o.style.display !== 'none' && !o.classList.contains('hld-fadeout') };
})()"

correr <- function(et, nivel) {
  cat(sprintf("\n── %s ──\n", et))
  Sys.setenv(ARR_FORZAR_NIVEL = nivel, ARR_FORZAR_USUARIO = if (nzchar(nivel)) usr else "")
  app <<- tryCatch(start_bg_app(sprintf('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s"); setwd("%s"); source("%s/tests/helpers/arranque_app.R")$value',
                                        raiz, repo, raiz), PORT, timeout_s = 300, www = file.path(raiz, "www")),
                   error = function(e) { message(conditionMessage(e)); NULL })
  if (!ok(paste(et, "the real app starts on dev"), !is.null(app))) return(invisible())
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE))
  ok(paste(et, "the sign-in form shows"),
     poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 60))
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(usr, auto_unbox = TRUE), jsonlite::toJSON(pass, auto_unbox = TRUE)))
  Sys.sleep(0.5)
  ev("document.getElementById('auth-go_auth').click(); true")

  # Sample from the first frame with a tab link until the server has taken
  # over and the load has ended (or the limit).
  muestras <- list(); t0 <- Sys.time()
  limite <- as.numeric(Sys.getenv("T69C_LIMITE", "420"))
  repeat {
    m <- tryCatch(ev(MUESTRA_JS), error = function(e) NULL)
    if (is.list(m)) {
      m$t <- as.numeric(Sys.time() - t0, units = "secs"); muestras[[length(muestras) + 1L]] <- m
      if (isTRUE(m$aplicado) && !isTRUE(m$cargando)) break
    }
    if (as.numeric(Sys.time() - t0, units = "secs") > limite) break
    Sys.sleep(0.15)
  }
  if (!ok(paste(et, "signed in: a tab link appeared and the sign-in form is gone"), length(muestras) > 0L && !login_visible()))
    return(invisible())
  Sys.sleep(3)   # one more look after the take-over settles
  fin <- tryCatch(ev(MUESTRA_JS), error = function(e) NULL)
  vis <- lapply(muestras, function(m) strsplit(m$vis %||% "", " ", fixed = TRUE)[[1]])
  prim <- muestras[[1]]
  n_cargando <- sum(vapply(muestras, function(m) isTRUE(m$cargando) && !isTRUE(m$aplicado), logical(1)))
  t_aplic <- Filter(function(m) isTRUE(m$aplicado), muestras)
  cat(sprintf("      | muestras: %d en %.0f s | durante la carga y antes del relevo: %d | relevo a los %s s\n",
              length(muestras), muestras[[length(muestras)]]$t, n_cargando,
              if (length(t_aplic)) sprintf("%.0f", t_aplic[[1]]$t) else "(nunca)"))
  cat("      | primer marco: visibles =", prim$vis, "| CSS inicial =", prim$css, "\n")
  cat("      | todas las pestañas vistas alguna vez:", paste(unique(unlist(vis)), collapse = " "), "\n")
  cat("      | al final:", if (is.list(fin)) fin$vis else "?", "\n")
  list(vis = vis, prim = prim, fin = fin, n_cargando = n_cargando, aplicado = length(t_aplic) > 0L)
}

# ── Compras ──────────────────────────────────────────────────────────────────
r <- correr("[compras]", "compras")
if (is.list(r)) {
  ok("[compras] the first frame already carries the first paint's CSS", isTRUE(r$prim$css))
  primeras <- strsplit(r$prim$vis %||% "", " ", fixed = TRUE)[[1]]
  # (In an admin deployment -- CLIENT_ID hd-admin -- the static CSS hides
  # every client tab, PAN and REQ included, until the server shows them; so
  # the first frame is only required to show no treasury tab.)
  ok(sprintf("[compras] first frame: no treasury tab visible (%s)", paste(primeras, collapse = " ")),
     !length(intersect(primeras, TESORERIA_TABS)))
  malas <- unique(intersect(unlist(r$vis), TESORERIA_TABS))
  ok(sprintf("[compras] no sample, from the first frame to the take-over, shows a treasury tab (%s)",
             if (length(malas)) paste(malas, collapse = ",") else "none"), !length(malas))
  ok(sprintf("[compras] samples were taken while the first load ran (%d): the check measured the window", r$n_cargando),
     r$n_cargando >= 3L)
  ok("[compras] the server took over (hd-nivel-aplicado)", isTRUE(r$aplicado))
  fin <- strsplit(r$fin$vis %||% "", " ", fixed = TRUE)[[1]]
  ok(sprintf("[compras] after the take-over: PAN and REQ, and no treasury tab (%s)", paste(fin, collapse = " ")),
     all(c("PAN", "REQ") %in% fin) && !length(intersect(fin, TESORERIA_TABS)))
}
try(b$close(), silent = TRUE); b <- NULL; stop_bg_app(app); app <- NULL

# ── The same user with its own tier (counter-proof) ──────────────────────────
r <- if (identical(Sys.getenv("T69C_SOLO_COMPRAS"), "1")) NULL else correr("[propio]", "")
if (is.list(r)) {
  ok("[propio] the first frame has no first-paint CSS", !isTRUE(r$prim$css))
  fin <- strsplit(r$fin$vis %||% "", " ", fixed = TRUE)[[1]]
  ok("[propio] after the take-over: the treasury tabs still show", all(c("CAL", "PH", "VEN") %in% fin))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L || .pass == 0L) quit(status = 1L)
