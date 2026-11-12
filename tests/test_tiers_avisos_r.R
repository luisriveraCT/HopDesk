# =============================================================================
# tests/test_tiers_avisos_r.R
# The notices of R/tiers_module.R (Administración: users, blocks, jump access,
# client requests, clients and their quota, contacts, invitations, the mail
# queue, level settings) come from the dictionary, not fixed text
# (avisos-integrador, with the integrator's permission: text only).
#   1. no showNotification() in tiers_module.R carries fixed text (if/else
#      included);
#   2. every niveles.<sección>.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built
#      it (paste0/sprintf of the same pieces), zero-width spaces included;
#   4. the two contact notices take the name as a tr() parameter, not a gsub
#      around tr().
#
# Run from project root:  Rscript tests/test_tiers_avisos_r.R
# =============================================================================
cat("=== test_tiers_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/tiers_module.R"
SECC <- c("usuarios", "bloqueadas", "accesos", "solicitudes", "clientes", "contactos", "invitaciones", "cola", "config")

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de tiers_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- unlist(lapply(SECC, function(s) avisos_claves(F, paste0("niveles.", s, ".aviso."))))
falta <- avisos_claves_faltan(claves)
ok(sprintf("las %d claves niveles.<sección>.aviso.* existen en es y en", length(claves)),
   { if (length(falta)) message("  faltan: ", paste(falta, collapse = ", ")); length(claves) == 69L && !length(falta) })

es <- function(k, ...) tr(k, ..., idioma = "es")
u <- function(k, ...) es(paste0("niveles.usuarios.aviso.", k), ...)
b <- function(k, ...) es(paste0("niveles.bloqueadas.aviso.", k), ...)
a <- function(k, ...) es(paste0("niveles.accesos.aviso.", k), ...)
s <- function(k, ...) es(paste0("niveles.solicitudes.aviso.", k), ...)
cl <- function(k, ...) es(paste0("niveles.clientes.aviso.", k), ...)
co <- function(k, ...) es(paste0("niveles.contactos.aviso.", k), ...)
q <- function(k, ...) es(paste0("niveles.cola.aviso.", k), ...)
g <- function(k, ...) es(paste0("niveles.config.aviso.", k), ...)
username <- "ana.ruiz"; codigo <- "U0012"; nivel <- "Administrador"; err <- "sin red"; cid <- "acme"
quien <- "Luis R."; dur <- "2 h"; n <- 1234L; m <- 3L; name <- "María López"; tier_label <- "Finanzas"
iguales <- list(
  # Usuarios
  list(u("nombre_mostrar_obligatorio"), "El nombre para mostrar es obligatorio."),
  list(u("usuario_invalido"), "Usuario inválido (solo letras minúsculas, números y _)."),
  # contrasena_corta left with T31: one password rule for every screen, from
  # contrasena_error() (cuentas.error.*, tests/test_t31_validaciones.R).
  list(u("contrasenas_distintas"), "Las contraseñas no coinciden."),
  list(u("nivel_no_permitido"), "No puedes asignar ese nivel."),
  list(u("usuario_existe"), "Ya existe un usuario con ese nombre."),
  list(u("usuario_registrado"), "Este nombre de usuario ya está registrado en el sistema."),
  list(u("no_guardada"), "No se pudo guardar la cuenta. Verifica la conexión e intenta de nuevo."),
  list(u("creada", usuario = username, codigo = codigo), paste0("Cuenta '​", username, "' (", codigo, ") creada.")),
  list(u("no_eliminar_ultima", usuario = username, nivel = nivel),
       paste0("No se puede eliminar '​", username, "': debe existir al menos una cuenta ", nivel, " en el sistema.")),
  list(u("eliminacion_cancelada", nivel = nivel), paste0("Eliminación cancelada: debe existir al menos una cuenta ", nivel, ".")),
  list(u("cambio_no_guardado"), "No se pudo guardar el cambio. Verifica la conexión e intenta de nuevo."),
  list(u("desactivada", usuario = username), paste0("Cuenta '​", username, "' desactivada.")),
  list(u("falta_nivel_activo", nivel = nivel),
       paste0("No se puede guardar: debe existir al menos una cuenta ", nivel, " activa en este cliente.")),
  list(u("permisos_no_guardados"), "No se pudieron guardar los permisos. Verifica la conexión e intenta de nuevo."),
  list(u("cambios_guardados"), "Cambios guardados."),
  # Bloqueos
  list(b("desbloqueada", usuario = username), paste0("Cuenta '", username, "' desbloqueada.")),
  list(b("error_desbloquear", error = err), paste0("Error al desbloquear: ", err)),
  list(b("falta_usuario"), "Especifica el nombre de usuario a bloquear."),
  list(b("falta_motivo"), "Especifica el motivo del bloqueo."),
  list(b("ya_bloqueado", usuario = username), paste0("'", username, "' ya está bloqueado.")),
  list(b("bloqueada", usuario = username), paste0("Cuenta '", username, "' bloqueada.")),
  list(b("error_bloquear", error = err), paste0("Error al bloquear: ", err)),
  # Accesos de salto
  list(a("sin_acceso", cliente = cid), sprintf("Sin acceso a '%s'. Solicita un permiso en la pestaña 'Accesos de Salto'.", cid)),
  list(a("revocado", cliente = cid), sprintf("Tu acceso a '%s' fue revocado. Regresando al contexto de administración.", cid)),
  list(a("expirado", cliente = cid),
       sprintf("Tu acceso a '%s' ha expirado. Tienes 30 minutos adicionales antes de ser regresado al área de administración.", cid)),
  list(a("gracia_terminada", cliente = cid), sprintf("Período de gracia terminado. Regresando desde '%s' al contexto de administración.", cid)),
  list(a("solicitud_enviada"), "Solicitud enviada."),
  list(a("error", error = err), paste("Error:", err)),
  list(a("aprobado", usuario = quien, cliente = cid, duracion = dur), sprintf("Aprobado: %s → %s (%s).", quien, cid, dur)),
  list(a("error_aprobar", error = err), paste("Error al aprobar:", err)),
  list(a("denegado", usuario = quien, cliente = cid), sprintf("Denegado: %s → %s.", quien, cid)),
  list(a("error_denegar", error = err), paste("Error al denegar:", err)),
  list(a("acceso_revocado"), "Acceso revocado."),
  list(a("error_revocar", error = err), paste("Error al revocar:", err)),
  list(a("sin_clientes"), "Sin clientes para conceder."),
  list(a("concedido", usuario = quien, n = as.character(n)), sprintf("Acceso concedido a %s (%d cliente(s)).", quien, n)),
  list(a("error_conceder", error = err), paste("Error al conceder:", err)),
  # Solicitudes de cliente
  list(s("enviada_equipo"), "Tu solicitud fue enviada al equipo HopDesk. Te contactarán a la brevedad."),
  list(s("id_invalido"), "El ID solo puede contener letras minúsculas, números, _ y -."),
  list(s("nombre_obligatorio"), "El nombre del cliente es obligatorio."),
  list(s("id_existe", id = cid), paste0("El ID '", cid, "' ya existe en el registro.")),
  list(s("pendiente", id = cid), paste0("Ya hay una solicitud pendiente para '", cid, "'.")),
  list(s("error_guardar"), "Error al guardar la solicitud. Intenta de nuevo."),
  list(s("enviada", id = cid), paste0("Solicitud para '", cid, "' enviada.")),
  # "S3" -> "el servidor": the wording Luis approved in textos-visibles (d321e67), not the old text
  list(s("error_folder"), "Error al inicializar la carpeta del cliente en el servidor."),   # and "folder" -> "carpeta"
  list(s("error_registro"), "Error al guardar el registro del cliente."),
  list(s("aprobado", id = cid), paste0("Cliente '", cid, "' aprobado y activado.")),
  list(s("error_rechazo"), "Error al guardar el rechazo."),
  list(s("rechazada", id = cid), paste0("Solicitud para '", cid, "' rechazada.")),
  # Clientes y cupo
  list(cl("nombre_vacio"), "El nombre no puede estar vacío."),
  list(cl("limite_minimo"), "El límite debe ser al menos 1."),
  list(cl("maximo_cuentas", n = as.character(n)), sprintf("Selecciona como máximo %d cuentas.", n)),
  list(cl("error_leer_usuarios"), "No se pudo leer usuarios del cliente."),
  list(cl("falta_nivel", nivel = nivel),
       paste0("Mantén activa al menos una cuenta ", nivel, " del cliente. Ningún cambio guardado.")),
  list(cl("error_desactivar"), "Error al desactivar usuarios. Ningún cambio guardado."),
  list(cl("no_encontrado"), "Cliente no encontrado en el registro."),
  list(cl("error_revertido"), "Error al guardar el registro. Los cambios fueron revertidos."),
  list(cl("limite_actualizado", cliente = "ACME SA", n = as.character(n)),
       sprintf("Límite de '%s' actualizado a %d usuarios.", "ACME SA", n)),
  # Contactos
  list(co("nombre_obligatorio"), "El nombre es obligatorio."),
  # "email" -> "correo": the rule Luis approved in textos-visibles ("Email" -> "Correo"), not the old text
  list(co("email_obligatorio"), "El correo es obligatorio."),
  list(co("email_invalido"), "El correo no parece válido."),
  list(co("error_guardar"), "Error al guardar el contacto."),
  list(co("agregado", nombre = name), paste0("Contacto '", name, "' agregado.")),
  # Invitaciones
  list(es("niveles.invitaciones.aviso.otra_empresa"), "Esa invitación no pertenece a tu empresa."),
  # Cola de correo
  list(q("reintento", enviados = as.character(n), fallidos = as.character(m)),
       sprintf("Reintento de cola: %d enviado(s), %d fallido(s).", n, m)),
  list(q("enviado"), "Correo enviado."),   # "Email" -> "Correo" (textos-visibles)
  list(q("reintento_fallido"), "Reintento fallido — revisa la clave API."),
  # Configuración de niveles
  list(g("guardada", nivel = tier_label), paste0("Configuración de «", tier_label, "» guardada.")),
  list(g("error_guardar", error = err), paste0("Error al guardar: ", err)))
dif <- avisos_distintos(iguales)
ok(sprintf("el español de los %d textos es idéntico al de antes (con sus espacios de ancho cero)", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

# The English is translated and keeps the same {variables}; "Error: {error}"
# reads the same in both languages.
IGUAL_EN_INGLES <- "niveles.accesos.aviso.error"
vars <- function(x) sort(regmatches(x, gregexpr("\\{[a-z_]+\\}", x))[[1]])
ok("el inglés está traducido (salvo 'Error: {error}') y lleva las mismas {variables}",
   all(vapply(claves, function(k) { e1 <- i18n_dict("es")[[k]]; e2 <- i18n_dict("en")[[k]]
     (k %in% IGUAL_EN_INGLES || !identical(e1, e2)) && identical(vars(e1), vars(e2)) }, logical(1))))

codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("los números van como texto (sin separador de miles)",
   !grepl('tr\\("niveles\\.[a-z_]+\\.aviso\\.[a-z_]+",[^)]*\\b(n|enviados|fallidos) = (?!as\\.character\\()', codigo, perl = TRUE))
ok("contactos: el nombre va como parámetro de tr(), no con gsub alrededor",
   !grepl('gsub("{nombre}"', codigo, fixed = TRUE) &&
   grepl('tr("niveles.contactos.ya_inactivo", nombre = nombre)', codigo, fixed = TRUE) &&
   grepl('tr("niveles.contactos.desactivado", nombre = nombre)', codigo, fixed = TRUE))
ok("contactos: el texto sale igual que con el gsub de antes",
   identical(es("niveles.contactos.ya_inactivo", nombre = name),
             gsub("{nombre}", name, tr("niveles.contactos.ya_inactivo", idioma = "es"), fixed = TRUE)) &&
   identical(es("niveles.contactos.desactivado", nombre = name),
             gsub("{nombre}", name, tr("niveles.contactos.desactivado", idioma = "es"), fixed = TRUE)))
ok("la pantalla de contactos conserva su texto fijo (no es un aviso, fuera de alcance)",
   grepl('tags$p(class = "text-danger small mt-1", "El nombre es obligatorio.")', codigo, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
