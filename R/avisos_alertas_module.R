# =============================================================================
# R/avisos_alertas_module.R
# Alertas del equipo (Panorama › Publicar, front C). Per client, a matrix of
# users × 4 actions says whose actions the team wants to hear about. When a
# marked user does one of them, avisos_actividad() (R/avisos.R) asks
# alertas_preparar() here and, only if the user is marked, publishes an
# automatic aviso that only users allowed to do that action can see.
#
#   avisos_alertas.rds (per client): one row per marked user
#     usuario          username, lower case
#     crear            creates an item, a provision, a liability or a requisition
#     archivo_pago     generates a payment file
#     confirmar_pago   confirms a payment (CxP only, not a collection)
#     bancos           imports a bank statement or adds a manual movement
#     actualizado_por, actualizado
#
# T113: the screen (alertas_equipo_ui/server) and its writer (alertas_guardar)
# are gone. T68 (798990e1) unmounted them on purpose: each reader's "De quién
# recibo avisos" decides now, and publishing depends on permissions only.
# What stays: the publishing rule (ALERTAS_ACCIONES, alertas_preparar) and
# avisos_alertas.rds, READ ONLY (alertas_leer): Panorama works out from it the
# preference of whoever never saved one. Nothing writes it any more.
#
# Only client users are listed or stored: never a HopDesk staff account, for
# a client or for staff inside a client.
# =============================================================================

.ALERTAS_KEY <- "avisos_alertas.rds"
ALERTAS_COLUMNAS <- c("crear", "archivo_pago", "confirmar_pago", "bancos")
ALERTAS_VENTANA_MIN <- 10L

# Each action a hook names -> the matrix column it belongs to, and the
# permission a reader needs to see its aviso (the one needed to do it).
ALERTAS_ACCIONES <- list(
  crear_item           = list(columna = "crear",          permiso = "can_register_payments"),
  crear_provision      = list(columna = "crear",          permiso = "can_edit_provisiones"),
  crear_pasivo         = list(columna = "crear",          permiso = "can_manage_pasivos"),
  generar_archivo_pago = list(columna = "archivo_pago",   permiso = "can_register_payments"),
  confirmar_pago       = list(columna = "confirmar_pago", permiso = "can_confirm_payments"),
  importar_bancos      = list(columna = "bancos",         permiso = "can_manage_bancos"),
  movimiento_manual    = list(columna = "bancos",         permiso = "can_manage_bancos")
)

.schema_avisos_alertas <- function() tibble::tibble(
  usuario         = character(),
  crear           = logical(),
  archivo_pago    = logical(),
  confirmar_pago  = logical(),
  bancos          = logical(),
  actualizado_por = character(),
  actualizado     = as.POSIXct(character(), tz = "UTC")
)

.alertas_cid <- function(client_id) {
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid)) stop("[alertas] client_id es obligatorio", call. = FALSE)
  cid
}

# The matrix as S3 has it now. Strict: a failed read stop()s (it is never
# "nobody is marked"); no file yet is the empty matrix.
alertas_leer <- function(client_id) {
  leer_tabla_para_escribir(.ALERTAS_KEY, .alertas_cid(client_id), .schema_avisos_alertas)
}

# The client's live users with a client tier: the matrix's rows. Staff
# accounts never appear, whoever is looking.
alertas_usuarios <- function(client_id) {
  cid <- .alertas_cid(client_id)
  u <- auth_load_usuarios(client_id = cid)
  vacio <- data.frame(usuario = character(), nombre = character(), nivel = character(),
                      stringsAsFactors = FALSE)
  if (!is.data.frame(u) || !nrow(u)) return(vacio)
  vivo <- !(u$deleted %in% TRUE) & !(u$activo %in% FALSE) & u$tier %in% niveles_cliente()
  u <- u[vivo, , drop = FALSE]
  if (!nrow(u)) return(vacio)
  nombre <- if ("display_name" %in% names(u)) u$display_name else rep(NA_character_, nrow(u))
  data.frame(usuario = tolower(trimws(u$username)),
             nombre  = ifelse(is.na(nombre) | !nzchar(nombre %||% ""), u$username, nombre),
             nivel   = u$tier, stringsAsFactors = FALSE)
}

# Called by avisos_actividad() before it publishes anything. NULL means "do
# not publish": an unknown action, no user, the user not marked for that
# action, or a matrix that could not be read (fail closed, with a message).
# Otherwise `params` completed with the action's permiso, a clave per action
# and user, and the 10-minute window that groups repeats into one aviso.
# T68: actions that no longer go out as "act". Creating a requisition goes
# only through the Requisiciones channel (req_avisos_destinatarios()): it
# used to arrive twice. Its old "act" avisos still show.
ALERTAS_RETIRADAS <- c("crear_requisicion")

alertas_preparar <- function(client_id, accion, usuario, params = list()) {
  if (isTRUE(accion %in% ALERTAS_RETIRADAS)) return(NULL)
  def <- ALERTAS_ACCIONES[[accion %||% ""]]
  if (is.null(def)) {
    message("[alertas] acción desconocida: ", accion)
    return(NULL)
  }
  u <- tolower(trimws(usuario %||% ""))
  if (!nzchar(u)) return(NULL)
  # T68: published when at least one other user of the client holds this
  # action's permission (permissions only; each reader's "De quién recibo
  # avisos" filters when shown). A failed read of the users publishes all the
  # same: the delivery still filters by permission, and not publishing would
  # lose notices without a word.
  hay <- tryCatch(alertas_alguien_puede(client_id, accion, u), error = function(e) {
    message("[alertas] T68: users unreadable, publishing anyway (delivery filters by permission): ", accion)
    TRUE
  })
  if (!isTRUE(hay)) return(NULL)
  params$permiso     <- params$permiso %||% def$permiso
  params$clave       <- params$clave %||% paste("act", accion, u, sep = "|")
  params$ventana_min <- params$ventana_min %||% ALERTAS_VENTANA_MIN
  params
}

# T68: strict reads of this client's users and tier settings (a failure
# stops; the caller publishes anyway). Nothing of another client is read.
alertas_alguien_puede <- function(client_id, accion, autor) {
  cid <- .alertas_cid(client_id)
  usuarios <- .s3_leer_para_escribir(S3_KEYS$usuarios, cid)
  config   <- .s3_leer_para_escribir(S3_KEYS$tiers_config, cid)
  avisos_de_alguien_puede(usuarios, if (is.list(config)) config else NULL, accion, autor)
}
