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

  # ── C. Reply-To ────────────────────────────────────────────────────────────
  # El default global va VACÍO a propósito: hopdesk.app no tiene MX, así que
  # ninguna dirección del dominio recibe, y apuntar a un buzón ajeno lo expone
  # a cada invitado.
  old <- Sys.getenv("RESEND_REPLY_TO", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RESEND_REPLY_TO") else Sys.setenv(RESEND_REPLY_TO = old)
  }, add = TRUE)

  Sys.unsetenv("RESEND_REPLY_TO")
  .chk(email_service_status()$reply_to, "", "Reply-To global: vacío cuando no hay variable")

  # "none" y no "" porque readRenviron() SALTA las líneas `NOMBRE=` sin valor:
  # un vacío en .Renviron.local NO sobreescribe lo que puso .Renviron.
  Sys.setenv(RESEND_REPLY_TO = "none")
  .chk(email_service_status()$reply_to, "", "Reply-To global: el centinela 'none' lo apaga")
  Sys.setenv(RESEND_REPLY_TO = "NONE")
  .chk(email_service_status()$reply_to, "", "Reply-To global: el centinela ignora mayúsculas")
  Sys.setenv(RESEND_REPLY_TO = "soporte@ejemplo.com")
  .chk(email_service_status()$reply_to, "soporte@ejemplo.com",
       "Reply-To global: una dirección real sí pasa")

  # El cuerpo JSON sólo lleva la cabecera cuando hay dirección.
  .chk(grepl("reply_to", .resend_body("H <n@m.app>", "a@b.com", "s", "<p>h</p>",
                                      reply_to = ""), fixed = TRUE), FALSE,
       "payload: sin dirección no se manda la cabecera reply_to")
  .chk(grepl('"reply_to":"x@y.com"',
             .resend_body("H <n@m.app>", "a@b.com", "s", "<p>h</p>",
                          reply_to = "x@y.com"), fixed = TRUE), TRUE,
       "payload: con dirección se manda tal cual")

  .chk("reply_to" %in% names(formals(send_email)), TRUE,
       "send_email() acepta un Reply-To por mensaje")

  # Una dirección mal formada NO debe tumbar la invitación con un 422 de
  # Resend: degrada al default global.
  src <- readLines("R/email_service.R", warn = FALSE, encoding = "UTF-8")
  i <- grep("rt <- tolower\\(trimws\\(reply_to", src)
  .chk(length(i), 1L, "send_email(): normaliza el reply_to recibido")
  .chk(any(grepl("rt <- status\\$reply_to", src[i:(i + 6)])), TRUE,
       "send_email(): un reply_to inválido cae al default, no falla el envío")

  # ── El sitio de llamada resuelve al invitador ──────────────────────────────
  tm <- readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8")
  j <- grep("inviter <- tryCatch", tm)
  .chk(length(j), 1L, "tiers_module: resuelve quién invita antes de mandar")
  blk <- paste(tm[j:(j + 16)], collapse = "\n")
  # El staff invita a inquilinos de los que NO es miembro: su cuenta vive en su
  # propio cliente, no en el de destino. Buscarlo en client_in daría vacío.
  .chk(grepl("current_user_info\\(\\)\\$client_id", blk), TRUE,
       "tiers_module: busca al invitador en SU cliente, no en el de destino")
  .chk(grepl("client_in", blk), FALSE,
       "tiers_module: no usa el cliente destino para resolver al invitador")
  .chk(any(grepl("reply_to = inviter\\$email", tm)), TRUE,
       "tiers_module: el Reply-To de la invitación es quien la envió")
  .chk(any(grepl("inviter_name = inviter\\$name", tm)), TRUE,
       "tiers_module: la plantilla recibe el nombre de quien invita")
})
