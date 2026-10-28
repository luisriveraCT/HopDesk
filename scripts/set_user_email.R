# =============================================================================
# scripts/set_user_email.R
# Cambia el correo registrado de una cuenta. No hay UI para este campo.
#
# POR QUÉ IMPORTA ESE CAMPO (no es decorativo):
#   1. Es el Reply-To de las invitaciones que manda esa cuenta
#      (R/tiers_module.R → send_email(reply_to = )).
#   2. Es a donde llegan los avisos de límite de usuarios del staff
#      (get_limit_alert_recipients()).
#   3. Es la dirección de recuperación si se pierde la contraseña.
#
# USO (desde la raíz del repo):
#   Rscript --no-init-file scripts/set_user_email.R <usuario> <correo> [cliente]
#   Rscript --no-init-file scripts/set_user_email.R mouse a@b.com hd-admin --prod
#
# Sin --prod escribe en el bucket DEV, que es lo que quieres para probar.
#
# ANTES DE CORRERLO CONTRA PRODUCCIÓN: cierra la app de HopDesk.
# auth_save_usuarios() sobreescribe usuarios.rds completo, sin fusionar, así
# que dos procesos escribiendo a la vez se pisan — y el candado de instancia
# de R/global.R existe justo para impedir eso. Si el candado salta, NO lo
# rodees: cierra la app.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
prod <- "--prod" %in% args
args <- args[args != "--prod"]

if (length(args) < 2L) {
  cat("uso: Rscript --no-init-file scripts/set_user_email.R <usuario> <correo> [cliente] [--prod]\n")
  quit(status = 1L)
}
usuario <- tolower(trimws(args[1]))
correo  <- tolower(trimws(args[2]))
cliente <- tolower(trimws(if (length(args) >= 3L) args[3] else "hd-admin"))

if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", correo)) {
  cat("[ABORTA] '", correo, "' no parece una dirección de correo\n", sep = "")
  quit(status = 1L)
}

if (prod) Sys.setenv(HOPDESK_PRODUCTION = "1")
source("R/global.R")

esperado <- if (prod) "antiguedad-rds-prod" else "antiguedad-rds-dev"
if (!identical(Sys.getenv("S3_BUCKET"), esperado)) {
  cat("[ABORTA] bucket inesperado:", Sys.getenv("S3_BUCKET"), "\n")
  quit(status = 1L)
}
cat("[BUCKET]", Sys.getenv("S3_BUCKET"), "| cliente:", cliente, "\n")

u <- auth_load_usuarios(client_id = cliente)
i <- which(tolower(u$username) == usuario & !(u$deleted %in% TRUE))
if (length(i) != 1L) {
  cat("[ABORTA] se esperaba 1 cuenta activa llamada '", usuario, "', hay ",
      length(i), "\n", sep = "")
  quit(status = 1L)
}

# Sólo el dominio al log: la dirección completa no tiene por qué quedar en una
# terminal o en un archivo de salida.
dom <- function(e) if (is.na(e) || !nzchar(trimws(e %||% ""))) "(vacío)" else sub("^[^@]*", "***", e)
cat("[ANTES ]", u$username[i], "|", u$display_name[i], "|", dom(u$email[i]), "\n")

if (identical(tolower(trimws(u$email[i] %||% "")), correo)) {
  cat("[SIN CAMBIO] ya estaba puesto\n")
} else {
  antes <- nrow(u)
  u$email[i] <- correo
  auth_save_usuarios(u, client_id = cliente)
  # Relectura desde S3, no del objeto en memoria: confirma que la escritura
  # llegó y que no se perdieron filas por el camino.
  v <- auth_load_usuarios(client_id = cliente)
  j <- which(tolower(v$username) == usuario & !(v$deleted %in% TRUE))
  ok <- length(j) == 1L && identical(tolower(trimws(v$email[j])), correo) && nrow(v) == antes
  cat("[DESPUÉS]", dom(v$email[j]), "| filas:", nrow(v), "de", antes,
      "| verificado:", ok, "\n")
  if (!ok) { cat("[FALLÓ] la relectura no coincide\n"); quit(status = 1L) }
}
cat("[LISTO]\n")
