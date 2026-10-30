# ── hd_table pieces: which features a table turns on ─────────────────────────
# A table's pieces come from the registry (R/hd_registro.R, generated from the
# design page): everything its class always has, plus the table's `agregar`,
# minus its `quitar`, plus what the caller asks for in `piezas` -- which may
# only be optional pieces of the class or pieces the table already adds.
# Asking for a piece that doesn't exist or that the table can't have is a
# programming error and stops with a clear message.
#
# The core rules (HD_REGLAS_NUCLEO) are not pieces: they can't be turned off.
#
# No S3 here yet: per-client overrides of the registry are Phase 7.

# Every piece the design knows, in the design page's order.
HD_PIEZAS_VALIDAS <- unique(unlist(lapply(hd_clases, function(cl) c(cl$siempre, cl$opcional)),
                                   use.names = FALSE))

# Pieces the engine implements so far (T02). The rest are T03+: a table that
# has them still works, the engine just doesn't draw them yet.
HD_PIEZAS_MOTOR <- c(
  "texto", "copiar_celda", "recorte", "encabezado_fijo", "densidad",
  "sel_fila", "sel_rango", "casillas", "sel_todos", "sel_persistente", "no_seleccionable",
  "contador", "sumador_sel", "sumador_vis",
  "ordenar", "buscar",
  # T03a
  "insignias", "tachado", "provision", "fichas"
)

HD_REGLAS_NUCLEO <- c(
  identidad_id     = "Rows are identified by id, never by position.",
  escape           = "All text is painted with textContent, never innerHTML.",
  orden_real       = "Sorting uses each cell's real value, not its formatted text.",
  mensajes         = "Empty, empty-with-search and loading messages are always shown.",
  conservar_estado = "New data keeps scroll, sort, search and selection (minus ids that no longer exist)."
)

# Noun for the row counter ("594 facturas"), by class, with per-table
# exceptions where the class default would be wrong. Values are the
# dictionary base keys used with tr_n() / hdTn().
HD_CONTADOR_CLASE <- c(
  A = "tablas.contador.facturas", B = "tablas.contador.facturas",
  C = "tablas.contador.registros", D = "tablas.contador.registros",
  E = "tablas.contador.registros", F = "tablas.contador.registros"
)
HD_CONTADOR_TABLA <- c(
  libro_banco   = "tablas.contador.movimientos",
  requisiciones = "tablas.contador.registros"
)

hd_tabla_def <- function(tabla) {
  def <- hd_registro[[tabla]]
  if (is.null(def))
    stop(sprintf("hd_table: unknown table '%s' (not in hd_registro).", tabla), call. = FALSE)
  def
}

hd_piezas_de <- function(tabla, piezas = NULL) {
  def <- hd_tabla_def(tabla)
  cl  <- hd_clases[[def$clase]]
  activas <- union(setdiff(cl$siempre, def$quitar), def$agregar)
  if (length(piezas)) {
    piezas <- as.character(piezas)
    desconocidas <- setdiff(piezas, HD_PIEZAS_VALIDAS)
    if (length(desconocidas))
      stop(sprintf("hd_table '%s': unknown piece(s): %s. Valid pieces are in HD_PIEZAS_VALIDAS.",
                   tabla, paste(desconocidas, collapse = ", ")), call. = FALSE)
    # What the registry removes from a table is a product decision; the
    # caller can't turn it back on even if the class lists it as optional.
    permitidas <- setdiff(union(activas, union(cl$opcional, def$agregar)), def$quitar)
    no_tocan <- setdiff(piezas, permitidas)
    if (length(no_tocan))
      stop(sprintf(paste0("hd_table '%s' (class %s): piece(s) %s are not optional for this class",
                          " and the table doesn't add them (or removes them). Optional: %s."),
                   tabla, def$clase, paste(no_tocan, collapse = ", "),
                   paste(cl$opcional, collapse = ", ")), call. = FALSE)
    activas <- union(activas, piezas)
  }
  HD_PIEZAS_VALIDAS[HD_PIEZAS_VALIDAS %in% activas]
}

# The subset of a table's pieces the engine draws today. Pieces not built yet
# are dropped with one message per table and session of R, never an error.
.hd_avisadas <- new.env(parent = emptyenv())
hd_piezas_motor <- function(tabla, piezas) {
  pendientes <- setdiff(piezas, HD_PIEZAS_MOTOR)
  if (length(pendientes) && is.null(.hd_avisadas[[tabla]])) {
    .hd_avisadas[[tabla]] <- TRUE
    message(sprintf("hd_table '%s': pieces not built yet, ignored: %s",
                    tabla, paste(pendientes, collapse = ", ")))
  }
  intersect(piezas, HD_PIEZAS_MOTOR)
}

hd_contador_clave <- function(tabla) {
  def <- hd_tabla_def(tabla)
  if (tabla %in% names(HD_CONTADOR_TABLA)) HD_CONTADOR_TABLA[[tabla]]
  else HD_CONTADOR_CLASE[[def$clase]]
}
