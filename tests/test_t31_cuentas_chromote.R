# =============================================================================
# tests/test_t31_cuentas_chromote.R
# T31 in a real headless Chrome, at Luis's window (1293 x 632), on
# tests/helpers/cuentas_fixture_app.R (app.R's real account page over S3 in
# memory, simulated mail, no network):
#   A  ?invite=: name, job title, user name, password; an injected name is
#      refused under its field and never runs; a good activation shows the
#      user name and drops the token from the address bar;
#   B  ?olvide=: the same answer for an account's e-mail, an unknown one and
#      an injected one; the e-mail never reaches the page; one mail arrives
#      (simulated), with the user name almost hidden;
#   C  ?reset=<token>: no token in the page's HTML; a weak password shows
#      its message; a good one changes it, the token leaves the bar, the same
#      link then reads invalid; a forged token reads the same;
#   D  every page: no third-party request, no Referer (meta), no horizontal
#      scroll at 1293 x 632, no user name / e-mail / token in the HTML of
#      ?olvide= and ?reset=.
# Run from project root:  Rscript tests/test_t31_cuentas_chromote.R
# Only on your Chrome turn (rule 28).
# =============================================================================
cat("=== test_t31_cuentas_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39131L
BASE <- sprintf("http://127.0.0.1:%d", PORT)
DIR <- normalizePath(file.path(tempdir(), "cuentas"), winslash = "/", mustWork = FALSE)
unlink(DIR, recursive = TRUE); dir.create(DIR, recursive = TRUE)
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

app <- start_bg_app(sprintf('Sys.setenv(CUENTAS_DIR = "%s", HOPDESK_APP_URL = "%s"); setwd("%s"); source("tests/helpers/cuentas_fixture_app.R")$value',
                            DIR, BASE, repo), PORT, timeout_s = 120)
b <- new_chromote_session()
b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(f, s = 20) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
ir <- function(q) { b$Page$navigate(paste0(BASE, "/", q)); b$Page$loadEventFired(wait_ = TRUE)
  espera(function() isTRUE(ev("!!document.querySelector('.hd-activate-card') && document.querySelector('.hd-activate-card').innerText.length > 20"))) }
html <- function() ev("document.documentElement.outerHTML")
tarjeta <- function() ev("document.querySelector('.hd-activate-card').innerText")
escribir <- function(id, txt) ev(sprintf("(function(){ var e = document.getElementById('%s'); e.value = %s;
  e.dispatchEvent(new Event('input', {bubbles:true})); e.dispatchEvent(new Event('change', {bubbles:true})); return true; })()",
  id, jsonlite::toJSON(txt, auto_unbox = TRUE)))
pulsar <- function(id) { ev(sprintf("document.getElementById('%s').click()", id)); Sys.sleep(0.8) }
recursos_fuera <- function() unlist(ev(sprintf(
  "performance.getEntriesByType('resource').map(function(r){ return r.name; }).filter(function(n){ return n.indexOf('%s') !== 0 && n.indexOf('data:') !== 0; })", BASE)))
sin_scroll <- function() isTRUE(ev("document.documentElement.scrollWidth <= window.innerWidth"))
referrer <- function() isTRUE(ev("!!document.querySelector('meta[name=\"referrer\"][content=\"no-referrer\"]')"))
es <- jsonlite::fromJSON("www/i18n/es.json")
# The reset mails only (A's activation also mails a welcome): line 2 is the subject.
correos <- function() Filter(function(f) identical(readLines(f, n = 2L, warn = FALSE, encoding = "UTF-8")[2],
                                                   es[["cuentas.olvide.correo.asunto"]]),
                             sort(list.files(file.path(DIR, "correos"), full.names = TRUE)))
es <- jsonlite::fromJSON("www/i18n/es.json")

# ── A. ?invite= ──────────────────────────────────────────────────────────────
tok_inv <- readLines(file.path(DIR, "invite_token.txt"))
ok("A: la página de activación abre con nombre, puesto, usuario y contraseña",
   { ir(paste0("?invite=", tok_inv))
     all(unlist(ev("['invite_nombre','invite_puesto','invite_usuario','invite_pw_new','invite_pw_conf'].map(function(i){ return !!document.getElementById(i); })"))) })
ok("A: el nombre de la invitación con marcado se muestra como texto",
   grepl("María <b>Núñez</b>", tarjeta(), fixed = TRUE) && !isTRUE(ev("!!document.querySelector('.hd-activate-lead b')")))
ok("A: sin Referer, sin recursos de terceros, sin scroll lateral", referrer() && !length(recursos_fuera()) && sin_scroll())
escribir("invite_nombre", "<img src=x onerror=\"window.__x=1\">"); escribir("invite_puesto", "Analista")
escribir("invite_usuario", "mjnunez"); escribir("invite_pw_new", "Segura#2026"); escribir("invite_pw_conf", "Segura#2026")
pulsar("invite_pw_submit")
ok("A: un nombre con marcado se rechaza bajo su campo y no se ejecuta",
   espera(function() grepl(es[["cuentas.error.caracteres"]], tarjeta(), fixed = TRUE), 10) &&
     !isTRUE(ev("window.__x === 1")) && !isTRUE(ev("!!document.querySelector('.hd-activate-card img')")))
escribir("invite_nombre", "María José Núñez"); pulsar("invite_pw_submit")
ok("A: la activación buena muestra el usuario elegido",
   espera(function() grepl(es[["cuentas.activacion.titulo_hecho"]], tarjeta(), fixed = TRUE), 15) &&
     grepl("mjnunez", tarjeta(), fixed = TRUE))
ok("A: el token de la invitación salió de la barra", espera(function() !grepl(tok_inv, ev("location.href"), fixed = TRUE), 5))

# ── B. ?olvide= ──────────────────────────────────────────────────────────────
respuesta <- function(correo) {
  ir("?olvide=1"); escribir("olvide_correo", correo); pulsar("olvide_enviar")
  espera(function() grepl(es[["cuentas.olvide.respuesta"]], tarjeta(), fixed = TRUE), 10)
  list(texto = tarjeta(), html = html())
}
ok("B: la página de 'olvidé' abre, sin Referer, sin terceros, sin scroll",
   { ir("?olvide=1"); referrer() && !length(recursos_fuera()) && sin_scroll() &&
       !grepl("ana@x.mx|ana\\b", html()) })
r1 <- respuesta("ana@x.mx")
espera(function() length(correos()) >= 1L, 15)
r2 <- respuesta("nadie@x.mx"); r3 <- respuesta("<img src=x onerror=\"window.__x=2\">@x.mx")
ok("B: la respuesta es idéntica para una cuenta, un correo desconocido y uno inyectado",
   identical(r1$texto, r2$texto) && identical(r2$texto, r3$texto))
ok("B: el correo escrito no vuelve a la página y nada inyectado corre",
   !grepl("ana@x.mx", r1$html, fixed = TRUE) && !grepl("nadie@x.mx", r2$html, fixed = TRUE) &&
     !grepl("onerror=\"window.__x=2", r3$html, fixed = TRUE) && !isTRUE(ev("window.__x === 2")))
Sys.sleep(2)
mails <- correos()
ok("B: llegó un solo correo (simulado), a ana, con el usuario casi oculto",
   length(mails) == 1L && identical(readLines(mails[1], n = 1L), "ana@x.mx") &&
     grepl("a…a|a…", paste(readLines(mails[1], warn = FALSE), collapse = "\n")))

# ── C. ?reset= ───────────────────────────────────────────────────────────────
cuerpo <- paste(readLines(mails[1], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
enlace <- regmatches(cuerpo, regexpr(paste0(BASE, "/\\?reset=[0-9a-f]{64}"), cuerpo))
tk <- sub(".*=", "", enlace)
ok("C: el correo trae el enlace con la dirección de esta app", length(enlace) == 1L)
ok("C: la página del enlace pide la contraseña y su HTML no trae el token, el usuario ni el correo",
   { b$Page$navigate(enlace); b$Page$loadEventFired(wait_ = TRUE)
     espera(function() isTRUE(ev("!!document.getElementById('reset_pw_new')")))
     h <- html(); !grepl(tk, h, fixed = TRUE) && !grepl("ana@x.mx", h, fixed = TRUE) && !grepl(">ana<", h, fixed = TRUE) &&
       !grepl("type=\"hidden\"", h, fixed = TRUE) && referrer() && !length(recursos_fuera()) && sin_scroll() })
escribir("reset_pw_new", "debil"); escribir("reset_pw_conf", "debil"); pulsar("reset_enviar")
ok("C: una contraseña débil muestra su mensaje",
   espera(function() grepl(es[["cuentas.error.contrasena_regla"]], tarjeta(), fixed = TRUE), 10))
escribir("reset_pw_new", "Nueva#2026x"); escribir("reset_pw_conf", "Nueva#2026x"); pulsar("reset_enviar")
ok("C: la buena cambia la contraseña y el token sale de la barra",
   espera(function() grepl(es[["cuentas.reset.titulo_hecho"]], tarjeta(), fixed = TRUE), 15) &&
     espera(function() !grepl(tk, ev("location.href"), fixed = TRUE), 5) && !grepl(tk, html(), fixed = TRUE))
ok("C: el mismo enlace ya no sirve", { ir(paste0("?reset=", tk))
  grepl(es[["cuentas.reset.enlace_invalido"]], tarjeta(), fixed = TRUE) && !grepl(tk, html(), fixed = TRUE) })
ok("C: un token forjado se ve igual y no se repite", { ir("?reset=%3Cscript%3Ewindow.__x%3D3%3C%2Fscript%3E")
  grepl(es[["cuentas.reset.enlace_invalido"]], tarjeta(), fixed = TRUE) && !isTRUE(ev("window.__x === 3")) &&
    !grepl("window.__x=3", html(), fixed = TRUE) })
ok("D: el log de la prueba no trae el token, la contraseña ni el correo",
   { l <- paste(readLines(file.path(DIR, "log.txt"), warn = FALSE), collapse = "\n")
     nzchar(l) && !grepl(tk, l, fixed = TRUE) && !grepl("Nueva#2026x", l, fixed = TRUE) && !grepl("ana@x.mx", l, fixed = TRUE) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
