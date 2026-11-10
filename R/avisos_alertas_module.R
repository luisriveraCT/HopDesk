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
# Only client users are listed or stored: never a HopDesk staff account, for
# a client or for staff inside a client. Changing the matrix needs
# can_publish_panorama, checked on the server.
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
  crear_requisicion    = list(columna = "crear",          permiso = "can_view_requisiciones"),
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

# Saves the marks of one or more users (`cambios`: data.frame with usuario
# and the 4 logical columns) on S3's matrix as it is now: other users' rows,
# saved meanwhile by another session, are kept. A user left with nothing
# marked is removed. Only the client's live client-tier users are accepted.
# A failed read or write stop()s: the caller says so and nothing is
# announced as saved. Returns the matrix written.
alertas_guardar <- function(client_id, cambios, por) {
  cid <- .alertas_cid(client_id)
  validos <- alertas_usuarios(cid)$usuario
  cambios <- as.data.frame(cambios, stringsAsFactors = FALSE)
  cambios$usuario <- tolower(trimws(cambios$usuario))
  ajenos <- setdiff(cambios$usuario, validos)
  if (length(ajenos))
    stop("[alertas] usuario que no es de este cliente: ", paste(ajenos, collapse = ", "), call. = FALSE)
  ahora <- as.POSIXct(Sys.time(), tz = "UTC")
  modificar_tabla(.ALERTAS_KEY, cid, .schema_avisos_alertas, function(m) {
    m <- m[!(m$usuario %in% cambios$usuario), , drop = FALSE]
    filas <- tibble::tibble(usuario = cambios$usuario)
    for (col in ALERTAS_COLUMNAS) filas[[col]] <- cambios[[col]] %in% TRUE
    algo <- Reduce(`|`, lapply(ALERTAS_COLUMNAS, function(col) filas[[col]]))
    filas <- filas[algo, , drop = FALSE]
    filas$actualizado_por <- rep(por %||% "", nrow(filas))
    filas$actualizado     <- rep(ahora, nrow(filas))
    dplyr::bind_rows(m, filas)
  }, permitir_reduccion = TRUE)
}

# Called by avisos_actividad() before it publishes anything. NULL means "do
# not publish": an unknown action, no user, the user not marked for that
# action, or a matrix that could not be read (fail closed, with a message).
# Otherwise `params` completed with the action's permiso, a clave per action
# and user, and the 10-minute window that groups repeats into one aviso.
alertas_preparar <- function(client_id, accion, usuario, params = list()) {
  def <- ALERTAS_ACCIONES[[accion %||% ""]]
  if (is.null(def)) {
    message("[alertas] acción desconocida: ", accion)
    return(NULL)
  }
  u <- tolower(trimws(usuario %||% ""))
  if (!nzchar(u)) return(NULL)
  m <- tryCatch(alertas_leer(client_id), error = function(e) {
    message("[alertas] no se pudo leer la matriz: ", conditionMessage(e))
    NULL
  })
  if (is.null(m)) return(NULL)
  fila <- m[m$usuario %in% u, , drop = FALSE]
  if (!nrow(fila) || !isTRUE(fila[[def$columna]][1])) return(NULL)
  params$permiso     <- params$permiso %||% def$permiso
  params$clave       <- params$clave %||% paste("act", accion, u, sep = "|")
  params$ventana_min <- params$ventana_min %||% ALERTAS_VENTANA_MIN
  params
}
