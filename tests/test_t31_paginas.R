# =============================================================================
# tests/test_t31_paginas.R
# T31: the account page above secure_app (app.R) -- ?invite=, ?olvide= and
# ?reset= -- run with testServer on the very block of app.R's server() that
# fills it (from "Invitation activation" to its early return), with the
# account functions replaced by recorders. What it checks:
#   1. ?olvide=: the same answer for every e-mail, the e-mail never shown
#      back, the request runs after the answer (later), only once;
#   2. ?reset=: no token anywhere in the page (no hidden field), an invalid
#      link reads the same as any other, the token leaves the address bar,
#      the password goes to reset_aplicar() and an error shows its key's text;
#   3. ?invite=: a name or client with markup is shown escaped;
#   4. injection in the 3 forms (HTML, JS, quotes, regex, a huge value):
#      never echoed as markup, never breaks the page;
#   5. the login links to ?olvide=1; the shell sends no Referer and loads
#      nothing from a third party; ui() routes the 3 parameters.
# No S3 and no network. Run from project root:  Rscript tests/test_t31_paginas.R
# APP_R = another copy of app.R (contraprueba).
# =============================================================================
cat("=== test_t31_paginas ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressMessages(source("R/i18n.R"))
source("R/usuarios_cuentas.R")
F_APP <- Sys.getenv("APP_R", "app.R")
app <- readLines(F_APP, warn = FALSE, encoding = "UTF-8")

# The block of server() that fills the page, as a server function of its own.
ini <- grep("^  # ── Invitation activation", app)
fin <- grep("^    message\\(\"\\[INVITE\\] (account|activation) page", app) + 2L
bloque <- gsub("session$clientData$url_search", "URL_SEARCH", app[ini:fin], fixed = TRUE)
servidor <- eval(parse(text = c("function(input, output, session) {", ".sn <- 1L", bloque, "}")))
# The shell and ui(), for section 5.
for (e in parse(text = app, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".invite_page_shell", ".hopdesk_brandmark", "ui") &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e, globalenv())

# Recorders in place of the account functions.
URL_SEARCH <- ""
pedidos <- list(); aplicados <- list(); resolver_ok <- TRUE
reset_solicitar <- function(correo, ...) { pedidos[[length(pedidos) + 1L]] <<- correo; list(motivo = "x") }
resoluciones <- 0L   # each call logs (and counts toward the alert) in the real one
reset_resolver <- function(token, ...) { resoluciones <<- resoluciones + 1L
  if (isTRUE(resolver_ok) && identical(token, TOKEN)) data.frame(username = "ana") else NULL }
respuesta_aplicar <- list(ok = TRUE)
reset_aplicar <- function(token, contrasena, confirmar, ...) {
  aplicados[[length(aplicados) + 1L]] <<- list(token = token, pw = contrasena); respuesta_aplicar }
INVITE <- list(token = "t", display_name = "<script>alert(1)</script>", client_id = "acme", email = "m@x.mx", tier = "admin")
resolve_invite <- function(token) if (identical(token, "tok_invite")) INVITE else NULL
read_client_registry <- function() data.frame(client_id = "acme", display_name = "<img src=x onerror=alert(2)>")
app_public_url <- function(session = NULL) "https://hopdesk.app"
derive_username <- function(x) "maria"
TOKEN <- strrep("ab", 32)

html_de <- function(session) { h <- session$getOutput("invite_body"); paste(h$html %||% h, collapse = "") }
correr <- function(url, cuerpo) { URL_SEARCH <<- url; pedidos <<- list(); aplicados <<- list()
  testServer(servidor, { session$flushReact(); cuerpo(session, input) }) }
ataques <- list("<script>alert(1)</script>", "\"><img src=x onerror=alert(1)>", "' OR '1'='1", ".*(a+)+$",
                "{cliente}", strrep("x", 1e5))

# ── 1. ?olvide= ──────────────────────────────────────────────────────────────
respuestas <- character()
for (correo in c("ana@x.mx", "nadie@x.mx", ataques)) correr("?olvide=1", function(session, input) {
  session$setInputs(olvide_correo = correo, olvide_enviar = 1); session$flushReact()
  h <- html_de(session)
  respuestas <<- c(respuestas, h)
  ok(sprintf("olvidé (%s…): el correo escrito no vuelve a la página", substr(correo, 1, 12)),
     !grepl(correo, h, fixed = TRUE) && grepl(tr("cuentas.olvide.respuesta"), h, fixed = TRUE))
  # testServer runs later() callbacks inside its own flush, so the order is
  # checked in the code below; here: once, with what was typed.
  ok(sprintf("olvidé (%s…): la solicitud corre una sola vez, con lo escrito", substr(correo, 1, 12)),
     { session$setInputs(olvide_enviar = 2); session$flushReact(); later::run_now()
       length(pedidos) == 1L && identical(pedidos[[1]], correo) })
})
ok("olvidé: la página dice exactamente lo mismo para cualquier correo", length(unique(respuestas)) == 1L)
obs <- paste(bloque[grep("observeEvent\\(input\\$olvide_enviar", bloque):(grep("observeEvent\\(input\\$olvide_enviar", bloque) + 8L)],
             collapse = "\n")
ok("olvidé: la página responde primero y la búsqueda va dentro de later::later()",
   grepl("olvide_hecho(TRUE)\n    later::later(function() tryCatch(reset_solicitar(correo)", obs, fixed = TRUE))

# ── 2. ?reset= ───────────────────────────────────────────────────────────────
correr(paste0("?reset=", TOKEN), function(session, input) {
  h <- html_de(session)
  ok("reset: el formulario no lleva el token (ni campo oculto ni texto)",
     grepl("reset_pw_new", h, fixed = TRUE) && !grepl(TOKEN, h, fixed = TRUE) && !grepl("type=\"hidden\"", h, fixed = TRUE))
  respuesta_aplicar <<- list(ok = FALSE, error = "cuentas.error.contrasena_regla")
  session$setInputs(reset_pw_new = "<script>x</script>", reset_pw_conf = "<script>x</script>", reset_enviar = 1)
  session$flushReact()
  e <- session$getOutput("reset_error"); e <- paste(e$html %||% e, collapse = "")
  ok("reset: el error muestra el texto de su clave y no repite lo escrito",
     grepl(tr("cuentas.error.contrasena_regla"), e, fixed = TRUE) && !grepl("<script>x", e, fixed = TRUE))
  ok("reset: el token y la contraseña llegan a reset_aplicar() desde la barra, no de un campo",
     identical(aplicados[[1]]$token, TOKEN) && identical(aplicados[[1]]$pw, "<script>x</script>"))
  respuesta_aplicar <<- list(ok = TRUE, usuario_oculto = "a…")
  session$setInputs(reset_pw_new = "Nueva#2026x", reset_pw_conf = "Nueva#2026x", reset_enviar = 2); session$flushReact()
  h <- html_de(session)
  ok("reset hecho: sin usuario ni token en la página, y el token sale de la barra",
     grepl(tr("cuentas.reset.titulo_hecho"), h, fixed = TRUE) && !grepl("ana", h, fixed = TRUE) &&
       !grepl(TOKEN, h, fixed = TRUE) && grepl("history.replaceState", h, fixed = TRUE))
})
invalidos <- character()
for (t in c(strrep("cd", 32), "<script>alert(1)</script>", "' OR 1=1 --", strrep("0", 1e5)))
  correr(paste0("?reset=", utils::URLencode(t, reserved = TRUE)), function(session, input) {
    h <- html_de(session); invalidos <<- c(invalidos, h)
    ok(sprintf("reset inválido (%s…): mensaje de siempre, sin repetir el token, y sale de la barra", substr(t, 1, 10)),
       grepl(tr("cuentas.reset.enlace_invalido"), h, fixed = TRUE) && !grepl(substr(t, 1, 40), h, fixed = TRUE) &&
         grepl("history.replaceState", h, fixed = TRUE) && !grepl("reset_pw_new", h, fixed = TRUE))
  })
ok("reset: todos los enlaces inválidos se ven igual", length(unique(invalidos)) == 1L)
# The integrator's review: the page re-renders (here: reset_hecho flips and
# back), and each render used to resolve -- and log -- the link again.
URL_SEARCH <- paste0("?reset=", strrep("cd", 32)); resoluciones <- 0L
testServer(servidor, { session$flushReact(); html_de(session)
  for (k in 1:3) { reset_hecho(TRUE); session$flushReact(); reset_hecho(FALSE); session$flushReact(); html_de(session) }
  ok("reset: el enlace se resuelve (y se registra) una sola vez por sesión, aunque la página se pinte de nuevo",
     resoluciones <= 1L && grepl(tr("cuentas.reset.enlace_invalido"), html_de(session), fixed = TRUE))
})

# ── 3. ?invite= ──────────────────────────────────────────────────────────────
correr("?invite=tok_invite", function(session, input) {
  h <- html_de(session)
  ok("invitación: nombre y cliente con marcado se muestran escapados",
     grepl("&lt;script&gt;alert(1)&lt;/script&gt;", h, fixed = TRUE) && !grepl("<script>alert(1)", h, fixed = TRUE) &&
       !grepl("<img src=x", h, fixed = TRUE))
  ok("invitación: pide nombre, puesto, usuario y contraseña",
     all(vapply(c("invite_nombre", "invite_puesto", "invite_usuario", "invite_pw_new", "invite_pw_conf"),
                function(i) grepl(i, h, fixed = TRUE), logical(1))))
})

# ── 5. Login, shell, ui() ────────────────────────────────────────────────────
codigo <- paste(app, collapse = "\n")
ok("el login enlaza a ?olvide=1 con su texto del diccionario",
   grepl('tags$a(href = "?olvide=1"', codigo, fixed = TRUE) && grepl('tr("cuentas.olvide.enlace")', codigo, fixed = TRUE))
pag <- htmltools::renderTags(.invite_page_shell("T"))
shell <- paste(pag$head, pag$html)
ok("la página no manda Referer", grepl('<meta name="referrer" content="no-referrer"/>', shell, fixed = TRUE))
ok("la página no carga nada de terceros (ni fuentes ni scripts de fuera)",
   !grepl("https?://", gsub("http://www.w3.org/2000/svg", "", shell, fixed = TRUE)))
pide <- function(q) as.character(tryCatch(ui(list(QUERY_STRING = q)), error = function(e) "secure"))
ok("ui() sirve la página de cuentas para ?invite=, ?olvide= y ?reset=",
   all(vapply(c("?invite=x", "?olvide=1", paste0("?reset=", TOKEN)), function(q)
     grepl("hd-activate-card", pide(q), fixed = TRUE), logical(1))))
ok("ningún message() del bloque imprime la dirección o el token",
   !any(grepl("message\\(.*(QUERY_STRING|url_search|URL_SEARCH|token|correo)", sub("#.*$", "", bloque))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
