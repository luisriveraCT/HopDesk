# =============================================================================
# R/settings/settings_mercado.R -- T116 F2: «Calendario de mercado», the
# HopDesk staff's capture of the official non-working days (R/mercado.R).
#
# STAFF ONLY, by an internal permission (can_capture_market_calendar,
# R/permisos.R): a client session never has it (permisos_resueltos() turns
# internal keys off for a client tier), so the hub never draws the button
# (R/settings/settings_hub.R), this panel never renders, and every observer
# checks puede() again before writing -- an input sent by hand does nothing.
# The calendar is ONE for every client (hd-admin/mercado/inhabiles.rds).
# =============================================================================


# Fail-closed: a user whose permisos cannot be read (an atomic vector without
# this key makes puede() stop) cannot capture.
.mercado_puede_usuario <- function(usuario)
  isTRUE(tryCatch(puede(usuario, "can_capture_market_calendar"), error = function(e) FALSE))
.mercado_puede <- function(shared)
  .mercado_puede_usuario(tryCatch(shared$usuario(), error = function(e) NULL))

# The hub's button (NULL without the permission: not in the DOM at all).
mercado_boton_hub <- function(usuario) {
  if (!.mercado_puede_usuario(usuario)) return(NULL)
  actionButton("stg_btn_mercado",
    tagList(icon("calendar-xmark"), " ", tr("tablas.ajustes.mercado.boton")),
    class = "btn btn-sm btn-outline-secondary text-start w-100")
}

.mercado_panel_ui <- function() {
  anio_hoy <- as.integer(format(hoy_en(), "%Y"))
  paises <- stats::setNames(names(MERCADO_PAISES), vapply(names(MERCADO_PAISES), mercado_pais_nombre, character(1)))
  tagList(
    tags$h6(class = "fw-semibold mb-1", tr("tablas.ajustes.mercado.titulo")),
    tags$p(class = "small text-muted mb-2", tr("tablas.ajustes.mercado.ayuda")),
    div(class = "d-flex flex-wrap gap-2 align-items-end mb-2",
      div(selectInput("mer_pais", tr("tablas.ajustes.mercado.pais"), choices = paises, selected = "MX", width = "180px")),
      # hd_numero_input() (T107): the browser does not enforce min/max -- the
      # writers (modificar_mercado_*, R/mercado.R) refuse a year outside .FESTIVOS_ANIOS_TOPE.
      div(hd_numero_input("mer_anio", tr("tablas.ajustes.mercado.anio"), formato = "entero", value = anio_hoy + 1L,
                          min = .FESTIVOS_ANIOS_TOPE[1], max = .FESTIVOS_ANIOS_TOPE[2], step = 1, width = "110px")),
      uiOutput("mer_estado", inline = TRUE)),
    hd_table_ui("mercado_t", alto = "40vh"),
    div(class = "d-flex flex-wrap gap-2 align-items-end mt-2",
      div(dateInput("mer_fecha", tr("tablas.ajustes.mercado.fecha"), value = NULL, width = "150px")),
      div(textInput("mer_motivo", tr("tablas.ajustes.mercado.motivo"), width = "220px")),
      div(textInput("mer_fuente", tr("tablas.ajustes.mercado.fuente"),
                    placeholder = tr("tablas.ajustes.mercado.fuente_ejemplo"), width = "220px")),
      actionButton("mer_agregar", tagList(icon("plus"), " ", tr("tablas.ajustes.mercado.agregar")),
                   class = "btn btn-sm btn-primary")),
    div(class = "d-flex flex-wrap gap-2 mt-2",
      actionButton("mer_anular", tagList(icon("ban"), " ", tr("tablas.ajustes.mercado.anular")),
                   class = "btn btn-sm btn-outline-danger"),
      actionButton("mer_confirmar", tagList(icon("circle-check"), " ", tr("tablas.ajustes.mercado.confirmar")),
                   class = "btn btn-sm btn-outline-success"),
      actionButton("mer_desconfirmar", tagList(icon("rotate-left"), " ", tr("tablas.ajustes.mercado.desconfirmar")),
                   class = "btn btn-sm btn-outline-secondary"))
  )
}

.mercado_columnas <- function() list(
  col_fecha("fecha", "tablas.ajustes.mercado.col_fecha"),
  col_texto("motivo", "tablas.ajustes.mercado.col_motivo"),
  col_insignia("estado", "tablas.ajustes.mercado.col_estado", mapa = list(
    coincide     = list(texto = "tablas.ajustes.mercado.estado_coincide",     tono = "exito"),
    solo_oficial = list(texto = "tablas.ajustes.mercado.estado_solo_oficial", tono = "aviso"),
    solo_regla   = list(texto = "tablas.ajustes.mercado.estado_solo_regla",   tono = "neutro")))
)

settings_mercado_observer <- function(input, output, session, shared) {
  version <- reactiveVal(0L)
  usuario_actual <- function() tryCatch(shared$current_user(), error = function(e) "system")
  evento <- function(accion, descripcion, metadata = list())
    list(usuario = usuario_actual(), modulo = "ajustes", accion = accion, descripcion = descripcion,
         metadata = metadata, home = tryCatch(shared$home_client_id(), error = function(e) NULL))
  anio_sel <- function() { a <- suppressWarnings(as.integer(input$mer_anio)); if (length(a) == 1L && !is.na(a)) a else NA_integer_ }
  pais_sel <- function() { p <- toupper(input$mer_pais %||% ""); if (mercado_pais_valido(p)) p else NA_character_ }
  avisar_error <- function(e)
    showNotification(if (inherits(e, "mercado_error")) tr(e$clave)
                     else tr("tablas.ajustes.mercado.error_guardar", error = conditionMessage(e)),
                     type = "error", duration = 6)

  filas <- reactive({
    version()
    if (!.mercado_puede(shared)) return(mercado_comparar_vacio())
    mercado_comparar(pais_sel(), anio_sel())
  })
  t_mer <- hd_table_server("mercado_t", tabla = "mercado_inhabiles", datos = filas,
                           columnas = .mercado_columnas(), piezas = c("sel_fila", "sel_rango", "sel_todos"),
                           vacio = "tablas.ajustes.mercado.vacio")

  observeEvent(input$stg_btn_mercado, {
    if (!.mercado_puede(shared)) return()
    mercado_inhabiles_refrescar(forzar = TRUE)
    version(version() + 1L)
    output$settings_panel <- renderUI({ if (.mercado_puede(shared)) .mercado_panel_ui() })
  }, ignoreInit = TRUE)

  output$mer_estado <- renderUI({
    version()
    if (!.mercado_puede(shared)) return(NULL)
    p <- pais_sel(); a <- anio_sel()
    if (is.na(p) || is.na(a)) return(NULL)
    conf <- mercado_anio_confirmado(p, a)
    if (is.null(conf)) tags$span(class = "badge text-bg-light border", tr("tablas.ajustes.mercado.sin_confirmar"))
    else tags$span(class = "badge text-bg-success", tr("tablas.ajustes.mercado.confirmado", por = conf$capturado_por))
  })

  observeEvent(input$mer_agregar, {
    if (!.mercado_puede(shared)) return()   # silent: a client never learns the permission's name
    p <- pais_sel(); f <- input$mer_fecha
    tryCatch({
      modificar_mercado_agregar_dia(p, f, input$mer_motivo, input$mer_fuente, por = usuario_actual(),
        evento = evento("mercado_inhabil_agregar",
                        sprintf("Día inhábil oficial agregado: %s %s", p, format(as.Date(f)))))
      updateTextInput(session, "mer_motivo", value = "")
      version(version() + 1L)
      showNotification(tr("tablas.ajustes.mercado.agregado"), type = "message", duration = 4)
    }, error = avisar_error)
  }, ignoreInit = TRUE)

  observeEvent(input$mer_anular, {
    if (!.mercado_puede(shared)) return()   # silent: a client never learns the permission's name
    marcadas <- filas()[filas()$id %in% t_mer$seleccion(), , drop = FALSE]
    ids <- marcadas$fila_id[!is.na(marcadas$fila_id)]
    if (!length(ids)) { showNotification(tr("tablas.ajustes.mercado.anular_nada"), type = "warning"); return() }
    tryCatch({
      for (i in ids) modificar_mercado_anular(i, por = usuario_actual(),
        evento = evento("mercado_anular", sprintf("Día inhábil oficial anulado: %s", i), list(id = i)))
      version(version() + 1L)
      showNotification(tr("tablas.ajustes.mercado.anulados", n = length(ids)), type = "message", duration = 4)
    }, error = function(e) { version(version() + 1L); avisar_error(e) })
  }, ignoreInit = TRUE)

  observeEvent(input$mer_confirmar, {
    if (!.mercado_puede(shared)) return()   # silent: a client never learns the permission's name
    p <- pais_sel(); a <- anio_sel()
    tryCatch({
      modificar_mercado_confirmar_anio(p, a, input$mer_fuente, por = usuario_actual(),
        evento = evento("mercado_anio_confirmar", sprintf("Año confirmado en el calendario oficial: %s %s", p, a)))
      version(version() + 1L)
      showNotification(tr("tablas.ajustes.mercado.confirmado_ok"), type = "message", duration = 4)
    }, error = avisar_error)
  }, ignoreInit = TRUE)

  # Undo a year's confirmation (voided, never deleted): the rule applies again.
  observeEvent(input$mer_desconfirmar, {
    if (!.mercado_puede(shared)) return()   # silent: a client never learns the permission's name
    p <- pais_sel(); a <- anio_sel()
    conf <- if (!is.na(p) && !is.na(a)) mercado_anio_confirmado(p, a) else NULL
    if (is.null(conf)) { showNotification(tr("tablas.ajustes.mercado.sin_confirmar"), type = "warning"); return() }
    tryCatch({
      modificar_mercado_anular(conf$id, por = usuario_actual(),
        evento = evento("mercado_anular", sprintf("Confirmación del año anulada: %s %s", p, a), list(id = conf$id)))
      version(version() + 1L)
      showNotification(tr("tablas.ajustes.mercado.desconfirmado"), type = "message", duration = 4)
    }, error = avisar_error)
  }, ignoreInit = TRUE)

  invisible(NULL)
}
