# =============================================================================
# R/adjuntos_pasivo_ui.R -- T106 phase 1: "Documentos" in a liability's detail
#
# The detail of a liability is the wizard's Revisión step (T77: opening one
# with ✎ lands there). This section sits at the end of that step
# (pasivos_step_ui_revision(), one line) and its server starts once per
# session (setup_pasivos_wizard(), one line). Everything else is here.
#
# Lazy (the Diseñador's ask, T108: ✎ must open fast): the section starts
# COLLAPSED and reads nothing from S3 until it is expanded -- no count either,
# that would be a read. It reads the list when opened, and again only after
# this session adds or trashes a document. A new liability (not saved yet)
# has no id: the section says to save it first.
#
# Reuses Requisiciones' pieces as they are (front E's files, untouched): the
# paste zone and download (www/requisiciones_paste.js: hdreq_bind,
# hdreq_descargar), its look (www/requisiciones.css, hdreq-* classes) and the
# pasted-table preview's table class (evidencia_excel, hd_table).
# Permissions in the server: adj_puede_ver() to read, adj_puede_editar() to
# add or trash (R/adjuntos_registro.R).
# =============================================================================

# The section, for the Revisión step's tagList. Every render starts closed and
# tells the server so (pdoc_abierto = FALSE): reopening the wizard never
# reads S3 by itself.
adjuntos_pasivo_ui <- function() {
  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "requisiciones.css?v=5"),
      htmltools::tags$script(src = "requisiciones_paste.js?v=2")),
    htmltools::tags$details(class = "mt-3 pdoc",
      # 'shown': Shiny rechecks what is visible on that event, never on a
      # <details> opening (found in Chrome, T106: the body stayed suspended).
      ontoggle = "Shiny.setInputValue('pdoc_abierto', this.open, {priority: 'event'}); if (this.open && window.jQuery) jQuery(this).trigger('shown');",
      htmltools::tags$summary(class = "fw-semibold", tr("comun.adjuntos.titulo")),
      shiny::uiOutput("pdoc_cuerpo")),
    # Only once Shiny is connected: called while the page is still loading,
    # setInputValue would race Shiny's own start. HTML(): tags$script() escapes
    # its text, and "&&" became "&amp;&amp;" -- a syntax error (found in Chrome, T106).
    htmltools::tags$script(htmltools::HTML("if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) Shiny.setInputValue('pdoc_abierto', false, {priority: 'event'});")))
}

# T106 2a: a CFDI's card says which one it is -- its UUID's first block, the
# issuer's RFC and the total in its currency (monto_moneda, never "$").
.adjdoc_cfdi_resumen <- function(r) {
  if (is.null(r$ref_uuid) || is.na(r$ref_uuid)) return(NULL)
  x <- tryCatch(jsonlite::fromJSON(r$refs %||% "{}"), error = function(e) list())
  total <- if (is.numeric(x$total) && !is.na(x$total)) monto_moneda(x$total, x$moneda %||% NA_character_) else ""
  htmltools::tags$span(class = "hdreq-ev-tam d-block",
    tr("comun.adjuntos.cfdi_resumen", uuid = substr(r$ref_uuid, 1L, 8L), emisor = x$rfc_emisor %||% "", total = total))
}

setup_adjuntos_pasivo <- function(input, output, session, shared, entidad_id) {
  ENT <- "pasivo"
  abierto  <- shiny::reactiveVal(FALSE)
  version  <- shiny::reactiveVal(0L)
  pegado   <- shiny::reactiveVal(NULL)    # adjdoc_preparar_pegado() / CSV preview awaiting confirmation
  pegado_enc <- shiny::reactiveVal(TRUE)
  descarga <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$pdoc_abierto, abierto(isTRUE(input$pdoc_abierto)), ignoreNULL = TRUE)
  # usr(): for observers (no dependency). usr_vivo(): for what is drawn and
  # read, so a change of tier or permissions (staff "ver como") redraws it.
  usr <- function() tryCatch(shiny::isolate(shared$usuario()), error = function(e) NULL)
  usr_vivo <- function() tryCatch(shared$usuario(), error = function(e) NULL)
  cid <- function() tryCatch(shiny::isolate(shared$effective_client_id()), error = function(e) NULL)
  yo  <- function() tryCatch(shiny::isolate(shared$current_user()), error = function(e) NULL) %||% "unknown"
  casa <- function() tryCatch(shiny::isolate(shared$home_client_id()), error = function(e) NULL)
  id_actual <- function() { x <- tryCatch(entidad_id(), error = function(e) NULL); if (is.null(x) || !nzchar(x)) NULL else x }

  # The live documents: read ONLY while the section is open.
  docs <- shiny::reactive({
    if (!abierto()) return(NULL)
    version()
    id <- id_actual(); c <- cid()
    if (is.null(id) || is.null(c) || !adj_puede_ver(usr_vivo(), ENT)) return(NULL)
    tryCatch(documentos_de(c, ENT, id), error = function(e) "error")
  })

  editar_o_avisa <- function() {
    if (adj_puede_editar(usr(), ENT)) return(TRUE)
    shiny::showNotification(permiso_denegado_texto(usr(), adj_entidad(ENT)$permiso_editar), type = "error")
    FALSE
  }
  # One write: refusals are shown with their own words, anything else as a
  # failed save (T74: never silent, never "done").
  escribir <- function(f, ok_msg) {
    r <- tryCatch(f(), error = function(e) e)
    if (inherits(r, "error")) {
      msg <- if (inherits(r, "adjdoc_error")) conditionMessage(r) else tr("comun.adjuntos.error_guardar", error = conditionMessage(r))
      shiny::showNotification(msg, type = "error", duration = NULL)
      return(NULL)
    }
    version(version() + 1L)
    if (!is.null(ok_msg)) shiny::showNotification(ok_msg, type = "message", duration = 4)
    r
  }

  output$pdoc_cuerpo <- shiny::renderUI({
    if (!abierto()) return(NULL)
    id <- id_actual()
    if (is.null(id)) return(htmltools::div(class = "text-muted small", tr("comun.adjuntos.guardar_primero")))
    if (!adj_puede_ver(usr_vivo(), ENT)) return(NULL)
    e <- adj_entidad(ENT)
    htmltools::tagList(
      if (adj_puede_editar(usr_vivo(), ENT)) htmltools::div(class = "hdreq-evidencia",
        shiny::selectInput("pdoc_tipo", tr("comun.adjuntos.tipo_doc_campo"), width = "260px",
          choices = stats::setNames(e$tipos_doc, vapply(e$tipos_doc, function(t) tr(paste0("comun.adjuntos.tipo_doc.", t)), ""))),
        htmltools::div(id = "pdoc_zona", class = "hdreq-zona", tabindex = "0",
          htmltools::div(class = "hdreq-zona-texto", tr("comun.adjuntos.zona")),
          shiny::fileInput("pdoc_archivo", NULL, multiple = TRUE, width = "100%",
                           buttonLabel = tr("comun.adjuntos.elegir"), placeholder = "",
                           accept = c(".pdf", ".png", ".jpg", ".jpeg", ".webp", ".xlsx", ".csv", ".xml"))),
        shiny::uiOutput("pdoc_pegado")),
      shiny::uiOutput("pdoc_lista"),
      htmltools::div(style = "display:none;", shiny::downloadLink("pdoc_descargar", "")))
  })
  # Born inside a closed <details>, these outputs would be suspended as hidden
  # and never resumed (opening a <details> is not an event Shiny watches).
  # Not suspending them reads nothing: closed, the body is NULL and docs()
  # does not read S3 (abierto() is FALSE) -- the laziness is abierto()'s.
  # (Set at the end, once the three outputs exist.)

  # The paste zone is bound once its elements are in the page.
  shiny::observe({
    shiny::req(abierto(), id_actual())
    if (!isTRUE(adj_puede_editar(usr(), ENT))) return()
    session$onFlushed(function() session$sendCustomMessage("hdreq_bind",
      list(zona = "pdoc_zona", archivo = "pdoc_archivo", texto = "pdoc_pegado_texto",
           origen = "pdoc_archivo_origen", error = "pdoc_pegado_error")), once = TRUE)
  })
  shiny::observeEvent(input$pdoc_pegado_error, {
    shiny::showNotification(input$pdoc_pegado_error$mensaje %||% "", type = "warning", duration = 6)
  })

  tipo_sel <- function() { t <- input$pdoc_tipo %||% "otro"; if (t %in% adj_entidad(ENT)$tipos_doc) t else "otro" }

  shiny::observeEvent(input$pdoc_archivo, {
    if (!editar_o_avisa()) return()
    id <- id_actual(); c <- cid(); if (is.null(id) || is.null(c)) return()
    f <- input$pdoc_archivo
    origen <- if (identical(input$pdoc_archivo_origen, "pegado")) "pegado" else "archivo"
    for (i in seq_len(nrow(f))) {
      p <- tryCatch(adjdoc_preparar(f$datapath[i], f$name[i], origen), error = function(e) e)
      if (inherits(p, "error")) {
        msg <- if (inherits(p, "adjdoc_error")) conditionMessage(p) else tr("comun.adjuntos.error_leer")
        shiny::showNotification(tr("comun.adjuntos.no_agregado", nombre = .adj_nombre_limpio(f$name[i]), motivo = msg),
                                type = "error", duration = 8)
        next
      }
      if (identical(p$clase, "tabla")) {   # a CSV: preview first, like a paste
        pegado_enc(isTRUE(p$encabezado_sugerido)); pegado(p); next
      }
      escribir(function() documento_adjuntar(c, ENT, id, p, tipo_sel(), yo(), home = casa()),
               tr("comun.adjuntos.agregado", nombre = p$nombre))
    }
    unlink(f$datapath)
  })

  shiny::observeEvent(input$pdoc_pegado_texto, {
    if (!editar_o_avisa()) return()
    p <- tryCatch(adjdoc_preparar_pegado(input$pdoc_pegado_texto$texto %||% ""), error = function(e) e)
    if (inherits(p, "error")) {
      shiny::showNotification(if (inherits(p, "adjdoc_error")) conditionMessage(p) else tr("comun.adjuntos.tabla.error_vacia"), type = "warning")
      return()
    }
    pegado_enc(isTRUE(p$encabezado_sugerido)); pegado(p)
  })

  t_pegada <- hd_table_server("pdoc_tabla", tabla = "evidencia_excel",
    datos = shiny::reactive({ p <- pegado(); if (is.null(p)) NULL else .reqm_pegada(p$celdas, pegado_enc(), p$numericas)$datos }),
    columnas = shiny::reactive({ p <- pegado(); shiny::req(p); .reqm_pegada(p$celdas, pegado_enc(), p$numericas)$columnas }),
    encabezado = pegado_enc,
    aviso = shiny::reactive({ p <- pegado(); if (is.null(p)) NULL else p$aviso }))
  shiny::observeEvent(t_pegada$accion(), {
    a <- t_pegada$accion()
    if (identical(a$accion, "encabezado")) pegado_enc(identical(a$ids, "si"))
  })
  output$pdoc_pegado <- shiny::renderUI({
    p <- pegado(); if (is.null(p)) return(NULL)
    htmltools::div(class = "hdreq-pegado",
      htmltools::div(class = "hdreq-pegado-cab",
        shiny::textInput("pdoc_pegado_titulo", NULL, value = p$nombre %||% "", placeholder = tr("comun.adjuntos.tabla.titulo"), width = "100%"),
        shiny::actionButton("pdoc_pegado_agregar", tr("comun.adjuntos.tabla.agregar"), class = "btn btn-primary btn-sm"),
        shiny::actionButton("pdoc_pegado_descartar", tr("comun.adjuntos.tabla.descartar"), class = "btn btn-outline-secondary btn-sm")),
      hd_table_ui("pdoc_tabla", alto = "220px"))
  })
  shiny::observeEvent(input$pdoc_pegado_descartar, pegado(NULL))
  shiny::observeEvent(input$pdoc_pegado_agregar, {
    if (!editar_o_avisa()) return()
    p <- pegado(); id <- id_actual(); c <- cid(); if (is.null(p) || is.null(id) || is.null(c)) return()
    acc <- if (is.null(p$nombre)) "pegar_tabla_documento" else "adjuntar_documento"
    ok <- escribir(function() documento_tabla_agregar(c, ENT, id, p, input$pdoc_pegado_titulo %||% "", pegado_enc(),
                                                       tipo_sel(), yo(), home = casa(), accion = acc),
                   tr("comun.adjuntos.tabla.agregada"))
    if (!is.null(ok)) pegado(NULL)
  })

  output$pdoc_lista <- shiny::renderUI({
    d <- docs()
    if (identical(d, "error")) return(htmltools::div(class = "alert alert-warning py-1 small", tr("comun.adjuntos.error_leer_lista")))
    if (is.null(d)) return(NULL)
    if (!nrow(d)) return(htmltools::div(class = "text-muted small", tr("comun.adjuntos.ninguno")))
    quitar <- adj_puede_editar(usr_vivo(), ENT)
    htmltools::div(class = "hdreq-ev-galeria",
      lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        nombre <- if (identical(r$clase, "tabla")) (if (!is.na(r$titulo)) r$titulo else tr("comun.adjuntos.tabla.sin_titulo")) else r$nombre
        htmltools::div(class = "hdreq-ev-item",
          htmltools::tags$button(type = "button", class = "hdreq-doc", title = tr("comun.adjuntos.descargar"),
            onclick = sprintf("Shiny.setInputValue('pdoc_descargar_id', %s, {priority: 'event'})", jsonlite::toJSON(r$id, auto_unbox = TRUE)),
            htmltools::tags$span(class = "hdreq-ev-tipo", if (identical(r$clase, "tabla")) tr("comun.adjuntos.tabla.tipo") else toupper(r$ext)),
            htmltools::tags$span(class = "hdreq-ev-nombre", nombre),
            htmltools::tags$span(class = "hdreq-ev-tam", tr(paste0("comun.adjuntos.tipo_doc.", r$tipo_doc))),
            .adjdoc_cfdi_resumen(r)),
          if (quitar) htmltools::tags$button(type = "button", class = "btn btn-sm btn-link text-danger p-0 hdreq-quitar",
            title = tr("comun.adjuntos.quitar"),
            onclick = sprintf("Shiny.setInputValue('pdoc_quitar', %s, {priority: 'event'})", jsonlite::toJSON(r$id, auto_unbox = TRUE)),
            "✕"))
      }))
  })

  shiny::observeEvent(input$pdoc_quitar, {
    if (!editar_o_avisa()) return()
    c <- cid(); if (is.null(c)) return()
    escribir(function() documento_papelera(c, as.character(input$pdoc_quitar), yo(), home = casa()), tr("comun.adjuntos.quitado"))
  })

  # Downloads: always an attachment (downloadHandler), with the stored type.
  shiny::observeEvent(input$pdoc_descargar_id, {
    if (!adj_puede_ver(usr(), ENT)) return()
    c <- cid(); if (is.null(c)) return()
    b <- tryCatch(documento_bytes(c, as.character(input$pdoc_descargar_id)), error = function(e) NULL)
    if (is.null(b)) { shiny::showNotification(tr("comun.adjuntos.error_leer"), type = "error"); return() }
    descarga(b)
    session$sendCustomMessage("hdreq_descargar", list(id = "pdoc_descargar"))
  })
  output$pdoc_descargar <- shiny::downloadHandler(
    filename = function() {
      b <- descarga()
      n <- sub("\\.[A-Za-z0-9]+$", "", b$nombre %||% "documento")
      paste0(gsub("[^[:alnum:] ._-]", "_", n), ".", b$ext %||% "bin")
    },
    content = function(file) writeBin(descarga()$bytes, file),
    contentType = NULL)
  shiny::outputOptions(output, "pdoc_descargar", suspendWhenHidden = FALSE)
  for (o in c("pdoc_cuerpo", "pdoc_lista", "pdoc_pegado")) shiny::outputOptions(output, o, suspendWhenHidden = FALSE)

  invisible(list(docs = docs, abierto = abierto))
}
