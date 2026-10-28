# =============================================================================
# tests/test_invite_activation_page.R
# Covers the 2026-10-27 report: "la url me lleva a la página principal de
# login, necesitamos una de creación de usuario, ¿qué no había una ya?"
#
# There WAS one -- a modal fired by ?invite=<token> inside server(). It was
# unreachable by the only person who ever needs it. shinymanager::secure_app()
# returns the auth page, and never the application's own `ui`, whenever the
# request carries no valid token; an invitee has no account yet, so that is
# the only branch they can hit. Two separate defects fed the same symptom:
#
#   A. The activation flow lived behind the login gate.
#   B. The mailed link was built from the inviter's own address bar, which on
#      shinyapps.io carries a per-worker segment (/Hopdesk/_w_<hex>/) and
#      shinymanager's ?token= -- a link into somebody else's worker.
#
# Pure-logic tests for the URL scrubbing, plus source-level assertions for the
# page interception (a Shiny UI function can't be exercised headlessly here).
# Sourced by _run_saas.R.
# =============================================================================

cat("── Invitaciones: página de activación (2026-10-27) ────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. .normalize_app_url(): lo que de verdad rompía el enlace ───────────────

.chk(exists(".normalize_app_url"), TRUE, ".normalize_app_url() existe")
.chk(exists("app_public_url"),     TRUE, "app_public_url() existe")
.chk(exists("invite_activation_url"), TRUE, "invite_activation_url() existe")

# El caso real reportado: la URL del invitador en shinyapps.io.
.chk(.normalize_app_url("https://1weh8r-luis-rivera.shinyapps.io/Hopdesk/_w_198b75b62e7441a6824a070f2769abcd/"),
     "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk",
     "quita el segmento /_w_<hex>/ de shinyapps.io")

.chk(.normalize_app_url("https://x.shinyapps.io/App/_w_deadbeef"),
     "https://x.shinyapps.io/App",
     "quita /_w_<hex> también sin barra final")

# shinymanager deja su token en la barra de direcciones (keep_token = TRUE).
# Mandarlo por correo entregaría una sesión ajena, no sólo un enlace feo.
.chk(.normalize_app_url('https://x.shinyapps.io/App/?token="abc123"&language="es"'),
     "https://x.shinyapps.io/App",
     "descarta el ?token= de shinymanager")
.chk(grepl("token", .normalize_app_url('https://x.shinyapps.io/App/?token="abc"')),
     FALSE, "ningún token de sesión sobrevive a la normalización")

.chk(.normalize_app_url("https://x.shinyapps.io/App#seccion"),
     "https://x.shinyapps.io/App", "descarta el fragmento #")
.chk(.normalize_app_url("https://x.shinyapps.io/App///"),
     "https://x.shinyapps.io/App", "colapsa barras finales")
.chk(.normalize_app_url("  https://x.shinyapps.io/App/  "),
     "https://x.shinyapps.io/App", "tolera espacios alrededor")
.chk(.normalize_app_url(""),   "", "cadena vacía se queda vacía")
.chk(.normalize_app_url(NULL), "", "NULL no revienta")

# Falso positivo que hay que evitar: una ruta que legítimamente contiene "_w_"
# pero no es el segmento de worker.
.chk(.normalize_app_url("https://x.com/_workspace/App"),
     "https://x.com/_workspace/App",
     "no confunde /_workspace/ con /_w_<hex>/")

# ── B. app_public_url(): HOPDESK_APP_URL manda ───────────────────────────────

.old_env <- Sys.getenv("HOPDESK_APP_URL", unset = NA_character_)
on.exit({
  if (is.na(.old_env)) Sys.unsetenv("HOPDESK_APP_URL")
  else Sys.setenv(HOPDESK_APP_URL = .old_env)
}, add = TRUE)

Sys.setenv(HOPDESK_APP_URL = "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk/")
.chk(app_public_url(NULL), "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk",
     "HOPDESK_APP_URL gana y se normaliza")

# "auto" = derivar de la sesión. Tiene que ser una palabra y no un valor
# vacío: readRenviron() IGNORA una línea `NOMBRE=` sin valor, así que
# .Renviron.local no puede borrar lo que puso .Renviron. Verificado en
# 2026-10-27 — la URL de producción se coló en un run local justo así.
Sys.setenv(HOPDESK_APP_URL = "auto")
.chk(app_public_url(NULL), "", "'auto' desactiva el pin")
.chk(app_public_url(list(clientData = list(
       url_protocol = "http:", url_hostname = "127.0.0.1",
       url_port = "7845", url_pathname = "/"))),
     "http://127.0.0.1:7845", "'auto' deriva de la sesión")
.chk(app_public_url(list(clientData = list(
       url_protocol = "http:", url_hostname = "127.0.0.1",
       url_port = "7845", url_pathname = "/"))) == "auto",
     FALSE, "'auto' nunca se usa como si fuera una URL")

# El centinela vive en los dos archivos, o el override local no hace nada.
for (f in c(".Renviron.local", ".Renviron.local.example")) {
  if (file.exists(f)) {
    .lines <- readLines(f, warn = FALSE)
    .chk(any(grepl("^HOPDESK_APP_URL=auto$", trimws(.lines))), TRUE,
         sprintf("%s fija HOPDESK_APP_URL=auto (no vacío)", f))
    .chk(any(grepl("^HOPDESK_APP_URL=$", trimws(.lines))), FALSE,
         sprintf("%s no usa un vacío, que no sobreescribiría", f))
  }
}

# Sin sesión y sin variable no se inventa una URL: mejor vacío que un enlace
# a "your-app.shinyapps.io", que es lo que el código hacía antes.
Sys.setenv(HOPDESK_APP_URL = "")
.chk(app_public_url(NULL), "", "sin pin y sin sesión: vacío, no un placeholder")
.chk(grepl("your-app", paste(readLines("R/tiers_module.R", warn = FALSE),
                             collapse = "\n")),
     FALSE, "ya no queda el fallback 'your-app.shinyapps.io'")

# Una sesión falsa basta: app_public_url() sólo lee clientData.
.fake_session <- list(clientData = list(
  url_protocol = "https:", url_hostname = "1weh8r-luis-rivera.shinyapps.io",
  url_port = "", url_pathname = "/Hopdesk/_w_198b75b6/"))
.chk(app_public_url(.fake_session),
     "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk",
     "fallback de sesión: también limpia el worker")

.local_session <- list(clientData = list(
  url_protocol = "http:", url_hostname = "127.0.0.1",
  url_port = "7845", url_pathname = "/"))
.chk(app_public_url(.local_session), "http://127.0.0.1:7845",
     "run local conserva el puerto")

# 80/443 son implícitos; repetirlos rompe la coincidencia con el dominio
# verificado y se ve mal en el correo.
.chk(app_public_url(list(clientData = list(
       url_protocol = "https:", url_hostname = "hopdesk.app",
       url_port = "443", url_pathname = "/"))),
     "https://hopdesk.app", "omite el puerto 443")

# ── C. invite_activation_url(): la forma exacta del enlace ───────────────────

Sys.setenv(HOPDESK_APP_URL = "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk")
.url <- invite_activation_url("Ab3xYz")
.chk(.url, "https://1weh8r-luis-rivera.shinyapps.io/Hopdesk/?invite=Ab3xYz",
     "enlace de activación con la forma esperada")
# Una sola barra: "…/Hopdesk//?invite=" sería otra ruta para shinyapps.io.
.chk(grepl("//\\?invite=", .url), FALSE, "sin barra doble antes de ?invite=")
.chk(grepl("^https://[^/]+/Hopdesk/\\?invite=[A-Za-z0-9]+$", .url), TRUE,
     "nada más viaja en el enlace")

Sys.setenv(HOPDESK_APP_URL = "")
.chk(invite_activation_url("Ab3xYz", NULL), "",
     "sin URL base no se fabrica un enlace roto")

# ── D. La página existe y se sirve ANTES del gate de shinymanager ────────────

app_src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

.chk(grepl("secured_ui <- shinymanager::secure_app\\(", app_src), TRUE,
     "secure_app() ya no se asigna directo a `ui`")
.chk(grepl("\\.invite_page_shell <- function\\(\\)", app_src), TRUE,
     "existe la página de activación")
.chk(grepl('uiOutput\\("invite_body"\\)', app_src), TRUE,
     "la página monta invite_body")
.chk(grepl('output\\$invite_body <- renderUI', app_src), TRUE,
     "el servidor rellena invite_body")

# El orden es lo único que hace alcanzable la página: el interceptor de
# ?invite= tiene que correr ANTES de delegar en secured_ui(request).
.pos_shell  <- regexpr('return\\(\\.invite_page_shell\\(\\)\\)', app_src)
.pos_secure <- regexpr('secured_ui\\(request\\)', app_src)
.chk(.pos_shell > 0 && .pos_secure > 0 && .pos_shell < .pos_secure, TRUE,
     "?invite= se intercepta ANTES de delegar en shinymanager")

# La regresión concreta que cerramos: ya no hay un modal de activación, que
# era inalcanzable por vivir detrás del login.
.chk(grepl('title = "Enlace no válido"', app_src), FALSE,
     "el modal de activación (inalcanzable) ya no existe")

# El formulario y el handler deben compartir ids, o el botón vuelve a no hacer nada.
for (id in c("invite_pw_new", "invite_pw_conf", "invite_pw_submit")) {
  .chk(length(gregexpr(id, app_src, fixed = TRUE)[[1]]) >= 2, TRUE,
       sprintf("'%s' aparece en la página y en el handler", id))
}

# La página NO debe montar la app: un invitado sin cuenta no tiene tenant.
.shell <- sub("(?s).*\\.invite_page_shell <- function\\(\\) \\{(.*?)\\n\\}\\n(?s).*",
              "\\1", app_src, perl = TRUE)
.chk(grepl("page_navbar|mod_.*_ui\\(", .shell), FALSE,
     "la página de activación no monta la UI de la aplicación")

# La página de activación no debe arrancar la aplicación: un invitado no tiene
# cuenta, ni cliente, ni token. Antes del early return tardaba ~15s en pintar
# porque detrás se registraban los módulos y se disparaba el fetch de SAP.
.pos_block  <- regexpr("Invitation activation", app_src, fixed = TRUE)
.pos_return <- regexpr("application boot skipped", app_src, fixed = TRUE)
.pos_mods   <- regexpr("Shared state", app_src, fixed = TRUE)
.chk(.pos_return > 0, TRUE, "server() corta temprano en la página de activación")
.chk(.pos_block > 0 && .pos_block < .pos_return, TRUE,
     "el bloque de invitación se registra ANTES del corte")
.chk(.pos_return < .pos_mods, TRUE,
     "el corte ocurre antes de registrar los módulos")
# El corte depende de que el token exista; sin token la app arranca normal.
.chk(grepl("if \\(nzchar\\(isolate\\(invite_token_rv\\(\\)\\) %\\|\\|% \"\"\\)\\)", app_src),
     TRUE, "el corte sólo aplica cuando hay token en la URL")

# ── E. El parser del token tolera la barra de direcciones real ───────────────

# Antes: strsplit(pairs, "=") + [[2]]. Con token="abc" y language="es" en la
# URL (keep_token = TRUE los deja ahí), cualquier par raro lanzaba error y el
# tryCatch lo convertía en "no hay invitación".
.chk(grepl("shiny::parseQueryString", app_src), TRUE,
     "el token se lee con parseQueryString, no con strsplit")
.qs <- shiny::parseQueryString('?invite=Ab3xYz&token="t=x"&language="es"')
.chk(.qs[["invite"]], "Ab3xYz",
     "el token sobrevive junto al ?token= de shinymanager")

# ── F. derive_username(): el bug que apareció corriendo el flujo completo ────
# El original era tolower(gsub("[^a-z0-9]", "", display_name)): quitaba antes
# de pasar a minúsculas, así que BORRABA cada mayúscula. Visto en vivo el
# 2026-10-27: "Prueba Activacion" -> "ruebactivacion".

.chk(exists("derive_username"), TRUE, "derive_username() existe")
.chk(derive_username("Prueba Activacion"), "pruebaactivacion",
     "no borra las mayúsculas (el caso visto en vivo)")
.chk(derive_username("Luis Rivera"), "luisrivera",
     "'Luis Rivera' -> luisrivera, no 'uisivera'")
.chk(derive_username("ANA GOMEZ"), "anagomez",
     "un nombre todo en mayúsculas no se queda vacío")
.chk(derive_username("José Pérez"), "joseperez",
     "los acentos se doblan, no se tiran")
.chk(derive_username("María-José Núñez"), "mariajosenunez",
     "ñ y guiones")
.chk(derive_username("  Ana  "), "ana", "recorta espacios")
.chk(derive_username("Ana2"), "ana2", "conserva dígitos")
.chk(derive_username(""), "", "vacío entra, vacío sale")
.chk(derive_username(NULL), "", "NULL no revienta")
.chk(grepl("[^a-z0-9]", derive_username("J. O'Brien-Smíth Jr.")), FALSE,
     "el resultado siempre es [a-z0-9]")

# Y el fallback para un nombre que se reduce a nada.
.chk(grepl('if \\(!nzchar\\(username\\)\\) username <- paste0\\("usuario"',
           paste(readLines("app.R", warn = FALSE), collapse = "\n")), TRUE,
     "un nombre que se reduce a nada recibe un usuario de respaldo")

# ── G. El enlace se arma en un solo lugar ────────────────────────────────────

tiers_src <- paste(readLines("R/tiers_module.R", warn = FALSE), collapse = "\n")
.chk(grepl("invite_activation_url\\(token, session\\)", tiers_src), TRUE,
     "tiers_module usa invite_activation_url()")
.chk(grepl('paste0\\(base_url, "\\?invite="', tiers_src), FALSE,
     "ya no se concatena la URL a mano")
.chk(grepl("url_pathname", tiers_src), FALSE,
     "tiers_module ya no lee url_pathname por su cuenta")
.chk(grepl("url_pathname", app_src), FALSE,
     "app.R ya no lee url_pathname por su cuenta")
