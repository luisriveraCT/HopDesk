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

# Relative paths (like vencidos.js), so the app also works under a sub-path.
hd_table_deps <- function() {
  htmltools::singleton(htmltools::tags$head(
    i18n_head(),
    htmltools::tags$link(rel = "stylesheet", href = "hdtable.css?v=4"),
    htmltools::tags$script(src = "hdtable.js?v=4")
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

hd_table_server <- function(id, tabla, datos, columnas, piezas = NULL, id_col = "id") {
  piezas_tabla <- hd_piezas_de(tabla, piezas)
  piezas_motor <- hd_piezas_motor(tabla, piezas_tabla)
  meta         <- hd_columnas_meta(columnas)
  contador     <- hd_contador_clave(tabla)

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
        textos   = list(contador = contador)
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
