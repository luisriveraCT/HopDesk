# ── hd_table: one table engine for HopDesk ───────────────────────────────────
# The server formats the rows once (R/hd_columnas.R) and sends them to the
# browser; www/hdtable.js paints, sorts, searches, selects and sums there.
# The browser only ever sends back ids and actions -- nothing by position.
#
#   t <- hd_table_server("facturas", tabla = "vencidos", datos = reactive(df),
#                        columnas = list(col_empresa("Empresa"), ...),
#                        piezas = c("densidad"), id_col = "id")
#   t$seleccion()   # ids marked, in the order they're shown
#   t$accion()      # list(accion, ids, nonce) or NULL
#
# Server -> browser: one "hdt-datos" custom message each time `datos`
# changes. Browser -> server: ns("sel") and ns("accion"), priority "event".
#
# accion() values: "detalle" (double click on a row, locked rows too), and
# the names given in acciones_fila (one row's id) and acciones_barra.
#
# PERMISSIONS: the .acciones control column only HIDES row buttons the user
# may not use. A hidden button is not a permission check -- the module that
# uses the table must check the permission again when the action arrives.

# Relative paths (like vencidos.js), so the app also works under a sub-path.
hd_table_deps <- function() {
  htmltools::singleton(htmltools::tags$head(
    i18n_head(),
    htmltools::tags$link(rel = "stylesheet", href = "hdtable.css?v=5"),
    htmltools::tags$script(src = "hdtable.js?v=5")
  ))
}

# alto: CSS height of the scrolling area (the totals footer sits below it).
hd_table_ui <- function(id, alto = NULL) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    hd_table_deps(),
    htmltools::div(
      id = ns("hdt"), class = "hdt", `data-ns` = ns(""),
      style = if (!is.null(alto)) paste0("--hdt-alto:", alto, ";"),
      htmltools::div(class = "hdt-cargando", tr("comun.cargando"))
    )
  )
}

# An optional argument whose piece the table doesn't have is ignored, with
# one message per table and argument (like pieces not built yet).
.hd_sin_pieza <- function(tabla, arg, pieza, valor, piezas) {
  if (is.null(valor) || pieza %in% piezas) return(valor)
  clave <- paste(tabla, arg, sep = "|")
  if (is.null(.hd_avisadas[[clave]])) {
    .hd_avisadas[[clave]] <- TRUE
    message(sprintf("hd_table '%s': `%s` ignored, the table doesn't have the '%s' piece.", tabla, arg, pieza))
  }
  NULL
}

# ── fichas ──────────────────────────────────────────────────────────────────
# fichas = list(campo = "etapa", orden = c(...), mostrar = "conteo" | "monto")
# One "Todas" chip plus one per value: first the values in `orden`, then any
# other value in the data, in order of appearance.
hd_fichas_validar <- function(fichas) {
  if (is.null(fichas)) return(NULL)
  if (!is.list(fichas) || !is.character(fichas$campo) || length(fichas$campo) != 1L || !nzchar(fichas$campo))
    stop("hd_table: `fichas` must be list(campo = <column name>, orden = <values>, mostrar = \"conteo\" | \"monto\").",
         call. = FALSE)
  desconocidos <- setdiff(names(fichas), c("campo", "orden", "mostrar"))
  if (length(desconocidos))
    stop(sprintf("hd_table: unknown `fichas` field(s): %s.", paste(desconocidos, collapse = ", ")), call. = FALSE)
  fichas$mostrar <- fichas$mostrar %||% "conteo"
  if (!fichas$mostrar %in% c("conteo", "monto"))
    stop(sprintf("hd_table: fichas$mostrar must be \"conteo\" or \"monto\", not \"%s\".", fichas$mostrar), call. = FALSE)
  fichas$orden <- as.character(fichas$orden %||% character(0))
  fichas
}

hd_fichas_payload <- function(fichas, datos, columnas) {
  if (is.null(fichas)) return(NULL)
  if (!fichas$campo %in% names(datos))
    stop(sprintf("hd_table: fichas$campo '%s' is not in the data.", fichas$campo), call. = FALSE)
  if (identical(fichas$mostrar, "monto") &&
      !any(vapply(columnas, function(c) identical(c$tipo, "dinero") && isTRUE(c$sumar), logical(1))))
    stop("hd_table: fichas$mostrar = \"monto\" needs a col_dinero() with sumar = TRUE.", call. = FALSE)
  valores <- as.character(datos[[fichas$campo]])
  valores[is.na(valores)] <- ""
  presentes <- unique(valores[nzchar(valores)])
  orden <- c(fichas$orden, setdiff(presentes, fichas$orden))
  # Labels from the col_insignia() of the same field, if there is one.
  ins <- Filter(function(c) identical(c$tipo, "insignia") && identical(c$campo, fichas$campo), columnas)
  mapa <- if (length(ins)) ins[[1]]$mapa else list()
  list(
    valores  = I(valores),
    opciones = lapply(orden, function(v) list(valor = v, texto = .hd_texto(mapa[[v]]$texto %||% v))),
    mostrar  = fichas$mostrar
  )
}

# ── Row actions (acc_fila) ──────────────────────────────────────────────────
# Fixed catalog: icon, dictionary key for the title, and the order the
# buttons always appear in, right-aligned. "provision" only shows on rows
# with .provision = TRUE.
HD_ACCIONES_FILA <- list(
  agregar   = list(icono = "＋",  clave = "tablas.accion.agregar"),
  editar    = list(icono = "✎",  clave = "tablas.accion.editar"),
  provision = list(icono = "⚡",  clave = "tablas.accion.provision"),
  deshacer  = list(icono = "↶",  clave = "tablas.accion.deshacer"),
  detalle   = list(icono = "ℹ",  clave = "tablas.accion.detalle"),
  eliminar  = list(icono = "✕",  clave = "tablas.accion.eliminar")
)

hd_acciones_fila_validar <- function(acciones_fila) {
  if (is.null(acciones_fila)) return(NULL)
  if (!is.character(acciones_fila) || !length(acciones_fila))
    stop("hd_table: `acciones_fila` must be a character vector of action names.", call. = FALSE)
  desconocidas <- setdiff(acciones_fila, names(HD_ACCIONES_FILA))
  if (length(desconocidas))
    stop(sprintf("hd_table: unknown row action(s): %s. Valid: %s.",
                 paste(desconocidas, collapse = ", "), paste(names(HD_ACCIONES_FILA), collapse = ", ")),
         call. = FALSE)
  names(HD_ACCIONES_FILA)[names(HD_ACCIONES_FILA) %in% acciones_fila]   # catalog order
}

hd_acciones_fila_meta <- function(acciones) {
  lapply(acciones, function(a) list(accion = a, icono = HD_ACCIONES_FILA[[a]]$icono,
                                    titulo = tr(HD_ACCIONES_FILA[[a]]$clave)))
}

hd_table_server <- function(id, tabla, datos, columnas, piezas = NULL, id_col = "id",
                            fichas = NULL, acciones_fila = NULL) {
  piezas_tabla <- hd_piezas_de(tabla, piezas)
  piezas_motor <- hd_piezas_motor(tabla, piezas_tabla)
  meta         <- hd_columnas_meta(columnas)
  contador     <- hd_contador_clave(tabla)
  fichas       <- .hd_sin_pieza(tabla, "fichas", "fichas", hd_fichas_validar(fichas), piezas_tabla)
  acc_fila     <- .hd_sin_pieza(tabla, "acciones_fila", "acc_fila",
                                hd_acciones_fila_validar(acciones_fila), piezas_tabla)
  acc_fila_meta <- if (length(acc_fila)) hd_acciones_fila_meta(acc_fila) else NULL

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      df <- datos()
      shiny::req(!is.null(df))
      session$sendCustomMessage("hdt-datos", list(
        id       = ns("hdt"),
        tabla    = tabla,
        filas    = hd_filas(df, columnas, id_col),
        columnas = meta,
        piezas   = I(piezas_motor),
        textos   = list(contador = contador),
        fichas   = hd_fichas_payload(fichas, df, columnas),
        acciones_fila = acc_fila_meta
      ))
    })

    list(
      seleccion = shiny::reactive(as.character(unlist(input$sel)) %||% character(0)),
      accion    = shiny::reactive({
        a <- input$accion
        if (is.null(a)) return(NULL)
        list(accion = as.character(a$accion), ids = as.character(unlist(a$ids)), nonce = a$nonce)
      })
    )
  })
}
