# =============================================================================
# R/adjuntos_registro.R -- T106: what can have documents, and which kinds
#
# The attachments layer (R/adjuntos_documentos.R) is not Pasivos' nor
# Requisiciones': any entity links to N documents. Each entity is one entry
# here -- adding one (a payment order, a bank movement, an invoice) is an
# entry, not new code in the layer. Functions by NAME (as the other
# registries), so this file loads before the ones that define them.
#   existe(client_id, id)   TRUE when the entity exists: a document is only
#                           linked to something real. A failed read stop()s.
#   reactivo_ver            the session reactive whose tier gate (T69b/M6)
#                           decides who may SEE the entity's documents
#   permiso_editar          R/permisos.R key to add, trash or restore
#   max_vivos / max_tablas  live documents (files / tables) per entity
#   tipos_doc               document kinds the entity takes (ADJ_TIPOS_DOC)
# Phase 1 (T106): pasivo. Next: CFDI by its fiscal UUID (refs), requisicion
# (migrating requisiciones_evidencia.rds), orden de pago, movimiento.
# =============================================================================

# Document kinds: code -> its dictionary key (adjuntos.tipo_doc.<code>).
ADJ_TIPOS_DOC <- c("contrato", "estado_cuenta", "cfdi", "comprobante_pago", "cotizacion",
                   "orden_compra", "tabla_amortizacion", "otro")

ADJ_ENTIDADES <- list(
  pasivo = list(
    etiqueta       = "adjuntos.entidad.pasivo",
    existe         = ".adj_pasivo_existe",
    reactivo_ver   = "pasivos_liabilities_db",
    permiso_editar = "can_manage_pasivos",
    max_vivos      = 20L,
    max_tablas     = 10L,
    tipos_doc      = c("contrato", "estado_cuenta", "cfdi", "comprobante_pago",
                       "tabla_amortizacion", "otro")
  )
)

# A liability of this client with this id (load_pasivos_liabilities() reads
# S3 strictly through .pasivos_load_with_list_cols).
.adj_pasivo_existe <- function(client_id, id) {
  df <- load_pasivos_liabilities(client_id = client_id)
  isTRUE(id %in% df$id)
}

adj_entidad <- function(entidad) {
  e <- if (is.character(entidad) && length(entidad) == 1L) ADJ_ENTIDADES[[entidad]] else NULL
  if (is.null(e)) stop("[adjuntos] entidad desconocida", call. = FALSE)
  e
}

# Who may see / change an entity's documents. `usuario`: usuario_sesion()'s
# list (tier, permisos). Both fail closed: an unknown tier sees nothing.
adj_puede_ver <- function(usuario, entidad) {
  e <- adj_entidad(entidad)
  isTRUE(tryCatch(nivel_reactivo_permitido(usuario$tier, e$reactivo_ver), error = function(x) FALSE))
}
adj_puede_editar <- function(usuario, entidad) {
  adj_puede_ver(usuario, entidad) && isTRUE(puede(usuario, adj_entidad(entidad)$permiso_editar))
}
