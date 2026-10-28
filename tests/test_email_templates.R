# =============================================================================
# tests/test_email_templates.R
# Cubre el reporte del 2026-10-27: "lo de 'reply to' se me hace raro y es un
# correo que no me gustaría que apareciera" + "me gustaría que hubiera al menos
# algo de introducción en la invitación".
#
# Lo que se prueba no es "¿se ve bonito?" sino las cuatro cosas que, si se
# rompen, rompen el correo en silencio y sólo se descubren cuando un invitado
# dice "no me llegó" o "esto parece phishing":
#
#   A. El HTML no se puede inyectar desde el formulario de invitación.
#   B. El texto plano conserva el enlace (hay clientes que sólo lo renderizan).
#   C. El Reply-To sale de quien invita, y uno basura NO tumba el envío.
#   D. Nada de <style>, <svg> ni <img>: justo lo que Gmail y compañía descartan.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Correos: plantillas y Reply-To (2026-10-27) ────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

local({
  URL <- "https://ejemplo.shinyapps.io/Hopdesk/?invite=AbC123xyz"

  # ── A. Escape ──────────────────────────────────────────────────────────────
  # El nombre del invitado sale de un formulario y viaja en un correo firmado
  # por un dominio en el que el proveedor del destinatario ya aprendió a
  # confiar: un <a> inyectado ahí es phishing con remitente verificado.
  h <- email_invite_html('<img src=x onerror=alert(1)>', "ACME & Co", URL,
                         inviter_name = "<script>x</script>")

  .chk(grepl("<img",    h, fixed = TRUE), FALSE, "invitación: el <img> del nombre queda escapado")
  .chk(grepl("<script", h, fixed = TRUE), FALSE, "invitación: el <script> del invitador queda escapado")
  .chk(grepl("&lt;img src=x onerror=alert(1)&gt;", h, fixed = TRUE), TRUE,
       "invitación: el nombre aparece como texto, no como etiqueta")
  .chk(grepl("ACME &amp; Co", h, fixed = TRUE), TRUE,
       "invitación: el & del nombre del cliente va escapado")

  hw <- email_welcome_html("<b>X</b>", "<i>u</i>", "<em>C</em>", URL)
  .chk(grepl("<b>", hw, fixed = TRUE) || grepl("<i>", hw, fixed = TRUE) ||
       grepl("<em>", hw, fixed = TRUE), FALSE,
       "bienvenida: escapa nombre, usuario y cliente")
  .chk(grepl("<b>", email_password_reset_html("<b>X</b>", URL), fixed = TRUE), FALSE,
       "restablecer: escapa el nombre")

  # ── B. Texto plano ─────────────────────────────────────────────────────────
  # Una invitación cuyo texto plano perdiera la URL no sirve de nada.
  h_ok <- email_invite_html("María José Núñez", "Networks Group", URL,
                            inviter_name = "Luis Rivera")
  txt  <- .html_to_text(h_ok)
  .chk(grepl(URL, txt, fixed = TRUE),        TRUE,  "texto plano: conserva el enlace de activación")
  .chk(grepl("<",  txt, fixed = TRUE),       FALSE, "texto plano: no quedan etiquetas")
  .chk(grepl("Luis Rivera", txt, fixed = TRUE), TRUE, "texto plano: dice quién invitó")
  .chk(grepl("tesorería", txt, fixed = TRUE), TRUE, "texto plano: incluye la introducción pedida")

  txt_w <- .html_to_text(email_welcome_html("Ana", "ana", "Networks Group", "https://x.app"))
  .chk(grepl("ana", txt_w, fixed = TRUE), TRUE, "texto plano: la bienvenida conserva el usuario")

  # ── Quién invita ───────────────────────────────────────────────────────────
  .chk(grepl("Luis Rivera", h_ok, fixed = TRUE), TRUE,
       "invitación: nombra a quien la envió")
  sin <- email_invite_html("Ana", "Networks Group", URL)
  .chk(grepl("Te invitaron a unirte", sin, fixed = TRUE), TRUE,
       "invitación sin invitador resuelto: cae a una frase impersonal")
  .chk(grepl("NA", sin, fixed = TRUE), FALSE,
       "invitación sin invitador: no filtra un 'NA' al cuerpo")

  # El enlace va DOS veces: en el botón y suelto. Las pasarelas corporativas
  # reescriben o quitan los <a>; sin la URL copiable, un invitado detrás de una
  # de ellas se queda sin forma de activar la cuenta.
  .chk(length(gregexpr(URL, h_ok, fixed = TRUE)[[1]]), 2L,
       "invitación: el enlace va en el botón y también suelto para copiar")

  # ── D. Compatibilidad con clientes de correo ───────────────────────────────
  plantillas <- list(
    "invitación"  = h_ok,
    "bienvenida"  = email_welcome_html("Ana", "ana", "C", "https://x.app"),
    "restablecer" = email_password_reset_html("Ana", "https://x.app"))
  for (nm in names(plantillas)) {
    x <- plantillas[[nm]]
    .chk(grepl("<style", x, fixed = TRUE), FALSE,
         paste0(nm, ": sin bloque <style> (varios clientes borran el <head>)"))
    .chk(grepl("<svg", x, fixed = TRUE), FALSE,
         paste0(nm, ": sin <svg> (Gmail lo elimina)"))
    .chk(grepl("<img", x, fixed = TRUE), FALSE,
         paste0(nm, ": sin <img> (las imágenes vienen bloqueadas por defecto)"))
    .chk(grepl("#b49444", x, fixed = TRUE) && grepl("#07090d", x, fixed = TRUE), TRUE,
         paste0(nm, ": usa los colores de la app"))
    .chk(grepl("#c2185b", x, fixed = TRUE), FALSE,
         paste0(nm, ": ya no usa el rosa que no casaba con la página de activación"))
  }

  # ── C. Sin Reply-To ────────────────────────────────────────────────────────
  # Decisión del 2026-10-28, no un valor por omisión: hopdesk.app no tiene MX
  # (nada del dominio recibe), un buzón corporativo quedaba expuesto a cada
  # invitado, y el correo de quien invita es una dirección del staff que no
  # tiene por qué llegarle a alguien que todavía no es usuario.
  #
  # Estas aserciones existen para que nadie lo "restaure" sin querer.
  cuerpo <- .resend_body("HopDesk <n@m.app>", "a@b.com", "asunto", "<p>h</p>")
  .chk(grepl("reply_to", cuerpo, fixed = TRUE), FALSE,
       "payload: el JSON a Resend no lleva la clave reply_to")
  .chk(grepl('"from"', cuerpo, fixed = TRUE) && grepl('"text"', cuerpo, fixed = TRUE), TRUE,
       "payload: sigue llevando from y la parte de texto plano")

  .chk("reply_to" %in% names(formals(send_email)), FALSE,
       "send_email() ya no acepta una dirección de respuesta")
  .chk("reply_to" %in% names(formals(.resend_body)), FALSE,
       ".resend_body() ya no acepta una dirección de respuesta")
  .chk("reply_to" %in% names(email_service_status()), FALSE,
       "email_service_status() ya no publica un reply_to")

  # La variable de entorno quedó muerta: ponerla NO debe revivir la cabecera.
  # Es el modo realista de que esto se rompa — alguien la hereda de un
  # .Renviron viejo y asume que sigue funcionando.
  old <- Sys.getenv("RESEND_REPLY_TO", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RESEND_REPLY_TO") else Sys.setenv(RESEND_REPLY_TO = old)
  }, add = TRUE)
  Sys.setenv(RESEND_REPLY_TO = "nodeberia@aparecer.com")
  .chk(grepl("nodeberia@aparecer.com",
             .resend_body("H <n@m.app>", "a@b.com", "s", "<p>h</p>"), fixed = TRUE), FALSE,
       "RESEND_REPLY_TO quedó muerta: ponerla no revive la cabecera")

  src <- readLines("R/email_service.R", warn = FALSE, encoding = "UTF-8")
  .chk(any(grepl("Sys.getenv\\(\"RESEND_REPLY_TO\"\\)", src)), FALSE,
       "email_service: ya nadie lee RESEND_REPLY_TO")

  # ── El sitio de llamada nombra al invitador, pero NO lee su correo ──────────
  tm <- readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8")
  j <- grep("inviter_name <- tryCatch", tm)
  .chk(length(j), 1L, "tiers_module: resuelve el nombre de quien invita")
  blk <- paste(tm[j:(j + 10)], collapse = "\n")
  # El staff invita a inquilinos de los que NO es miembro: su cuenta vive en su
  # propio cliente, no en el de destino. Buscarlo en client_in daría vacío.
  .chk(grepl("current_user_info\\(\\)\\$client_id", blk), TRUE,
       "tiers_module: busca al invitador en SU cliente, no en el de destino")
  .chk(grepl("client_in", blk), FALSE,
       "tiers_module: no usa el cliente destino para resolver al invitador")
  .chk(grepl("\\$email", blk), FALSE,
       "tiers_module: NO lee el correo del invitador (no hay a dónde mandarlo)")
  .chk(any(grepl("reply_to", tm)), FALSE,
       "tiers_module: ninguna llamada pasa una dirección de respuesta")
  .chk(any(grepl("inviter_name = inviter_name", tm)), TRUE,
       "tiers_module: la plantilla sí recibe el nombre de quien invita")
})
