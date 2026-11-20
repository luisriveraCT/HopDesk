# =============================================================================
# R/requisiciones_module.R
# Requisiciones tab (front E, stage E2): the screen over
# R/requisiciones_persistence.R and R/adjuntos_s3.R.
#
#   requisicionesUI("req")
#   requisicionesServer("req", shared, activa = reactive(input$nav == "REQ"),
#                       abrir_provision = NULL)
#
# Liga con Pasivos (E3): abrir_provision(prefill, al_guardar) is how the app
# opens Pasivos' provision form. prefill is requisicion_prefill_provision();
# the form calls al_guardar(provision_id) once the provision is saved, and
# that links it to the requisition. Without abrir_provision the row action
# "Crear provisión" is not offered at all.
# The returned abrir_por_provision(provision_id) opens a requisition's
# detail read-only (paid ones too) for Calendario, Pasivos and the
# confirmation history.
#
# Tables only through hd_table(): "requisiciones" (the list), "provisiones"
# (the "Agregar evidencia a una provisión" picker) and "evidencia_excel" (a
# pasted table, twice: the preview of a new paste and a stored one).
#
# Rules this file keeps:
#   - every observer that writes starts with puede_o_avisa(); hidden buttons
#     are not a permission check;
#   - rows are acted on by id (t$seleccion(), accion ids), never by position;
#   - a failed read shows a notice, never an empty list; a failed save says
#     so and nothing is announced as done;
#   - shinyapps.io loads R/ alphabetically before app.R: nothing here calls
#     another file's function at the top level (only inside functions).
#   - all user text reaches the page through htmltools (escaped), and only
#     server-re-encoded images are shown; PDF and XLSX are only downloaded,
#     as attachments with their stored type.
# =============================================================================

# Writes made by any session of this process, per client: other sessions of
# the same process see them on their next poll (cheap: an in-memory number).
.reqm_versiones <- new.env(parent = emptyenv())
.reqm_version <- function(cid) if (is.null(cid)) 0L else (.reqm_versiones[[cid]] %||% 0L)
.reqm_tocar   <- function(cid) { .reqm_versiones[[cid]] <- .reqm_version(cid) + 1L; invisible(NULL) }

.REQM_POLL_MS <- 4000

# Prioridad marker: 1-10, neutral; 8-10 with a discreet accent (never red).
# Empty sorts last in both directions (NA order).
.reqm_col_prioridad <- function() {
  mapa <- stats::setNames(lapply(10:1, function(p) list(texto = as.character(p), tono = if (p >= 8) "info" else "neutro")),
                          as.character(10:1))
  col <- col_insignia("prioridad_txt", "requisiciones.col.prioridad", mapa = mapa)
  base <- col$valores
  col$valores <- function(x) {
    v <- base(x)
    raw <- as.character(x); raw[is.na(raw)] <- ""
    v$orden[!nzchar(raw)] <- NA
    v
  }
  col
}

.reqm_mapa_etapas <- function() list(
  sin_provision = list(texto = "requisiciones.etapa.sin_provision", tono = "neutro"),
  provisionada  = list(texto = "requisiciones.etapa.provisionada",  tono = "info"),
  por_pagar     = list(texto = "requisiciones.etapa.por_pagar",     tono = "morado"),   # CM35: en_pago splits
  en_agenda     = list(texto = "requisiciones.etapa.en_agenda",     tono = "aviso"),
  pagada        = list(texto = "requisiciones.etapa.pagada",        tono = "exito"),
  papelera      = list(texto = "requisiciones.etapa.papelera",      tono = "neutro"))

# aviso: add "Aviso de empresa" (T61) -- only while some requisition has one,
# so the list keeps its width the rest of the time (Luis's 1293 px screen).
.reqm_columnas <- function(iniciales, aviso = FALSE) Filter(Negate(is.null), list(
  col_texto("folio", "requisiciones.col.folio", recorte = FALSE, ancho = 124),
  col_texto("titulo", "requisiciones.col.titulo"),
  .reqm_col_prioridad(),
  col_texto("carpeta_ruta", "requisiciones.col.carpeta", opcional = TRUE, visible = FALSE),
  col_texto("proveedor", "requisiciones.col.proveedor"),
  col_empresa("empresa", "requisiciones.col.empresa", iniciales = iniciales),
  if (isTRUE(aviso)) col_insignia("empresa_aviso", "requisiciones.col.empresa_aviso", opcional = TRUE, mapa = list(
    fuera_carpeta = list(texto = "requisiciones.empresa_aviso.fuera_carpeta", tono = "aviso"),
    no_reconocida = list(texto = "requisiciones.empresa_aviso.no_reconocida", tono = "aviso"))),
  col_dinero("monto_mostrado", "requisiciones.col.monto", moneda = "moneda_mostrada"),
  col_insignia("etapa", "requisiciones.col.etapa", mapa = .reqm_mapa_etapas()),
  col_fecha("fecha_mes", "requisiciones.col.fecha"),
  col_numero("dias_sin_provision", "requisiciones.col.dias", opcional = TRUE, visible = FALSE),
  col_texto("comentarios_txt", "requisiciones.col.comentarios", recorte = FALSE, opcional = TRUE),
  col_texto("creado_por", "requisiciones.col.creado_por", opcional = TRUE, visible = FALSE)
))

# Rows for the list: the vista plus the engine's control columns.
# crear: offer "Crear provisión" on Sin provisión rows (the app wired a
# provision form and the user may create provisions).
.reqm_filas <- function(v, crear = FALSE) {
  if (!nrow(v)) {
    v$fecha_mes <- as.Date(character()); v$prioridad_txt <- character(); v$comentarios_txt <- character()
    v$.fijar <- logical(); v$.adjuntos <- integer(); v$.acciones <- character()
    return(v)
  }
  v$fecha_mes <- dplyr::coalesce(v$fecha_provision, as.Date(format(v$creado, tz = "America/Mexico_City")))
  v$prioridad_txt <- ifelse(is.na(v$prioridad), "", as.character(v$prioridad))
  v$comentarios_txt <- ifelse(v$n_comentarios == 0L, "",
    ifelse(v$comentarios_sin_leer > 0L,
           vapply(seq_len(nrow(v)), function(i) tr("requisiciones.comentarios.con_nuevos",
                  n = v$n_comentarios[i], nuevos = v$comentarios_sin_leer[i]), character(1)),
           as.character(v$n_comentarios)))
  v$.fijar <- v$etapa == "sin_provision"      # Sin provisión: always shown, on top, whatever the month
  v$.adjuntos <- as.integer(v$n_adjuntos + v$n_tablas)
  v$.acciones <- ifelse(isTRUE(crear) & v$etapa == "sin_provision", "detalle,crear_provision", "detalle")
  v
}

# A pasted table as engine rows and columns: text columns c1..cn, titles from
# the first row (encabezado) or "Columna n"; right-aligned only when the
# whole column is numeric. Nothing is reinterpreted.
.reqm_pegada <- function(celdas, encabezado, numericas = NULL) {
  if (is.null(celdas) || !length(celdas)) return(NULL)
  nc <- ncol(celdas)
  cuerpo <- if (encabezado && nrow(celdas) > 1L) celdas[-1, , drop = FALSE] else celdas
  df <- as.data.frame(cuerpo, stringsAsFactors = FALSE)
  names(df) <- paste0("c", seq_len(nc))
  df$id <- as.character(seq_len(nrow(df)))
  titulos <- if (encabezado && nrow(celdas) > 1L) celdas[1, ] else
    vapply(seq_len(nc), function(j) tr("requisiciones.tabla.columna", n = j), character(1))
  titulos[!nzchar(trimws(titulos))] <- vapply(which(!nzchar(trimws(titulos))),
    function(j) tr("requisiciones.tabla.columna", n = j), character(1))
  num <- numericas %||% rep(FALSE, nc)
  cols <- lapply(seq_len(nc), function(j) {
    col <- col_texto(paste0("c", j), titulos[j], recorte = TRUE)
    if (isTRUE(num[j])) col$alinear <- "derecha"
    col
  })
  list(datos = df, columnas = cols)
}

# T61: folder select options -- General (value "") first, then the live
# folders in tree order, each labelled with its path ("Obra A › Material").
# `excluir`: ids left out (a folder and its branch when choosing where to move it).
.reqm_opciones_carpeta <- function(cs, primera = "requisiciones.carpeta.general", excluir = character()) {
  op <- stats::setNames("", tr(primera))
  if (is.null(cs) || !nrow(cs)) return(op)
  cs <- cs[!(cs$id %in% excluir), , drop = FALSE]
  c(op, stats::setNames(cs$id, cs$ruta))
}

# T61: the companies a requisition in folder `carpeta_id` can be of, as
# select choices (initials = "NCS — short name"). `cmap`: initials -> short
# name of the active companies (shared$company_map()); `empresas`: the
# catalog (ids). General, a folder for every company, or an unreadable list:
# all of them. `actual`: a saved requisition's company; when it is not among
# them it stays, marked, so opening the form never erases it.
.reqm_empresas_carpeta <- function(cs, carpeta_id, cmap, empresas, actual = NA_character_) {
  inis <- names(cmap)
  if (!is.null(carpeta_id) && !is.na(carpeta_id) && nzchar(carpeta_id) && !is.null(cs) &&
      is.data.frame(empresas) && nrow(empresas)) {
    j <- match(carpeta_id, cs$id)
    ids <- if (is.na(j)) NULL else .req_carpeta_empresas(cs$empresas[j], existentes = empresas$id)
    if (!is.null(ids)) inis <- intersect(inis, empresas$initials[empresas$id %in% ids])
  }
  op <- if (length(inis)) stats::setNames(inis, paste0(inis, " — ", unname(cmap[inis]))) else character()
  if (!is.null(actual) && !is.na(actual) && nzchar(actual) && !actual %in% op)
    op <- c(op, stats::setNames(actual, tr("requisiciones.empresa_fuera_opcion", empresa = actual)))
  op
}

# T61: the company bubbles as company ids, for requisiciones_carpetas_visibles().
# empresa_sel holds the active companies' short names (app.R). activas NULL =
# no filter: no catalog yet, or no bubbles at all.
.reqm_burbujas <- function(empresas, empresa_sel) {
  if (!is.data.frame(empresas) || !nrow(empresas) || is.null(empresa_sel))
    return(list(activas = NULL, existentes = NULL))
  e <- empresas[!(empresas$deleted %in% TRUE) & !is.na(empresas$id), , drop = FALSE]
  if (!nrow(e)) return(list(activas = NULL, existentes = NULL))
  list(activas = e$id[e$nombre_corto %in% empresa_sel | e$initials %in% empresa_sel], existentes = e$id)
}

.reqm_tamano <- function(bytes) {
  if (is.na(bytes)) return("")
  if (bytes >= 1024^2) sprintf("%.1f MB", bytes / 1024^2) else sprintf("%.0f KB", max(1, bytes / 1024))
}

.reqm_fecha_hora <- function(t) {
  if (is.null(t) || is.na(t)) return("")
  format(t, "%d/%m/%Y %H:%M", tz = "America/Mexico_City")
}

# ── UI ───────────────────────────────────────────────────────────────────────

requisicionesUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "requisiciones.css?v=4"),
      htmltools::tags$script(src = "requisiciones_paste.js?v=2"),
      htmltools::tags$script(src = "requisiciones_carpetas.js?v=1")),
    htmltools::div(class = "hdreq",
      htmltools::div(class = "hdreq-lado hd-marca", shiny::uiOutput(ns("carpetas"))),
      htmltools::div(class = "hdreq-principal",
        htmltools::div(class = "hdreq-barra",
          shiny::uiOutput(ns("botones"), inline = TRUE),
          htmltools::div(class = "hdreq-barra-der",
            bslib::input_switch(ns("solo_mias"), tr("requisiciones.solo_mias"), value = FALSE),
            # CM35: paid ones stay listed (Luis); this hides them.
            bslib::input_switch(ns("ocultar_pagadas"), tr("requisiciones.ocultar_pagadas"), value = FALSE),
            shiny::uiOutput(ns("boton_papelera"), inline = TRUE))),
        shiny::uiOutput(ns("aviso_lectura")),
        hd_table_ui(ns("t"), alto = "ventana")),
      # Hidden download link driven by the evidence buttons (attachment, stored type).
      htmltools::div(style = "display:none;", shiny::downloadLink(ns("descargar"), "")))
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

requisicionesServer <- function(id, shared, activa = NULL, abrir_provision = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The currencies a folder or a requisition can be captured in (monedas
    # fase 3, T39, rule 1): the AP ledger's, the base first, then the whole
    # catalog. The first is the default.
    .req_monedas <- function() {
      base <- tryCatch(shared$moneda_base_cliente()$moneda, error = function(e) NULL) %||% MONEDA_BASE_OMISION
      monedas_para_capturar(tryCatch(shared$currency_choices$AP(), error = function(e) NULL) %||% character(), base)
    }

    cid <- shiny::reactive({
      x <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
      if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) NULL else x[1]
    })
    usr <- shiny::reactive(tryCatch(shared$usuario(), error = function(e) NULL))
    yo  <- function() shiny::isolate(usr()$user %||% "")
    activa_ok <- shiny::reactive(is.null(activa) || isTRUE(tryCatch(activa(), error = function(e) FALSE)))
    puede_ver <- shiny::reactive(puede(usr(), "can_view_requisiciones"))
    # "Crear provisión" exists only with a provision form wired in and the
    # permission to create provisions (both checked again on the server).
    puede_crear_prov <- shiny::reactive(is.function(abrir_provision) && puede(usr(), "can_edit_provisiones"))

    # Reload: after this session writes (version) and when another session of
    # this process wrote (poll of the in-memory counter, only while the tab
    # is open).
    version <- shiny::reactiveVal(0L)
    otro <- shiny::reactiveVal(0L)
    shiny::observe({
      if (!activa_ok()) return()
      shiny::invalidateLater(.REQM_POLL_MS)
      v <- .reqm_version(cid())
      if (!identical(v, shiny::isolate(otro()))) otro(v)
    })
    recargar <- function() {
      c <- shiny::isolate(cid())
      if (!is.null(c)) .reqm_tocar(c)
      shiny::isolate(otro(.reqm_version(c)))
      version(shiny::isolate(version()) + 1L)
    }

    # Runs a write; a req_error shows its own message, anything else the
    # generic one (and the log). Returns the value, or NULL on failure.
    escribir <- function(f, exito = NULL) {
      r <- tryCatch(list(ok = TRUE, v = f()), error = function(e) list(ok = FALSE, e = e))
      if (!r$ok) {
        msg <- if (inherits(r$e, "req_error")) conditionMessage(r$e) else {
          message("[requisiciones] escritura falló: ", conditionMessage(r$e))
          tr("requisiciones.error.guardar")
        }
        shiny::showNotification(msg, type = "error", duration = 8)
        recargar()
        return(NULL)
      }
      recargar()
      if (!is.null(exito)) shiny::showNotification(exito, type = "message", duration = 3)
      r$v %||% TRUE
    }

    # ── Data ────────────────────────────────────────────────────────────────
    vista <- shiny::reactive({
      version(); otro()
      c <- cid(); u <- usr()
      if (is.null(c) || is.null(u) || !activa_ok() || !puede_ver()) return(NULL)
      requisiciones_vista(c, usuario = u$user, incluir_pagadas = TRUE)   # CM35: they stay, read-only
    })
    carpetas <- shiny::reactive({
      version(); otro()
      c <- cid()
      if (is.null(c) || !activa_ok() || !puede_ver()) return(NULL)
      requisiciones_carpetas(c)
    })
    vista_segura    <- shiny::reactive(tryCatch(vista(), error = function(e) NULL))
    carpetas_segura <- shiny::reactive(tryCatch(carpetas(), error = function(e) NULL))

    # T61 (Luis): the company bubbles filter folders only. A folder shows when
    # it is for every company or shares one with the bubbles on, and its
    # mother shows; what is inside a hidden folder hides with it, also under
    # Todas. General never hides. Only this session's view: nothing written.
    burbujas <- shiny::reactive(.reqm_burbujas(tryCatch(shared$empresas_db(), error = function(e) NULL),
                                               tryCatch(shared$empresa_sel(), error = function(e) NULL)))
    carpetas_vis <- shiny::reactive({
      cs <- carpetas_segura()
      if (is.null(cs)) return(NULL)
      b <- burbujas()
      cs[requisiciones_carpetas_visibles(cs, b$activas, b$existentes), , drop = FALSE]
    })
    # Visible requisitions whose folder the bubbles hide.
    ocultas_burbujas <- function(v) {
      cs <- carpetas_segura(); cv <- carpetas_vis()
      if (is.null(cs) || is.null(cv) || is.null(v)) return(rep(FALSE, NROW(v)))
      v$carpeta_id %in% setdiff(cs$id, cv$id)
    }
    carpeta_sel  <- shiny::reactiveVal("todas")     # "todas" | "general" | <carpeta id> (T61)
    ver_papelera <- shiny::reactiveVal(FALSE)

    datos_lista <- shiny::reactive({
      v <- vista()
      if (is.null(v)) return(NULL)
      v <- v[if (ver_papelera()) v$etapa == "papelera" else v$etapa != "papelera", , drop = FALSE]
      if (isTRUE(input$solo_mias)) v <- v[tolower(v$creado_por) %in% tolower(yo()), , drop = FALSE]
      if (isTRUE(input$ocultar_pagadas)) v <- v[v$etapa != "pagada", , drop = FALSE]
      v <- v[!ocultas_burbujas(v), , drop = FALSE]
      # T61: General = no live folder; a folder shows its whole branch.
      sel <- carpeta_sel()
      if (identical(sel, "general")) v <- v[is.na(v$carpeta_ruta), , drop = FALSE]
      else if (!identical(sel, "todas")) {
        cs <- carpetas_segura()
        v <- v[v$carpeta_id %in% (if (is.null(cs)) sel else .req_carpeta_subarbol(cs, sel)), , drop = FALSE]
      }
      .reqm_filas(v, crear = puede_crear_prov())
    })

    iniciales <- shiny::reactive(tryCatch(shared$company_map(), error = function(e) c()) %||% c())
    empresas_cat <- shiny::reactive(tryCatch(shared$empresas_db(), error = function(e) NULL))   # T61: company ids
    puede_papelera <- function(u) puede(u, "can_trash_own_requisiciones") || puede(u, "can_trash_others_requisiciones")

    t <- hd_table_server("t", tabla = "requisiciones", datos = datos_lista,
      columnas = shiny::reactive({
        v <- vista_segura()
        .reqm_columnas(iniciales(), aviso = !is.null(v) && any(!is.na(v$empresa_aviso)))
      }),
      piezas = c("fichas", "detalle", "nav_periodo"),
      fichas = list(campo = "etapa", orden = REQ_ETAPAS_VISIBLES,
                    mostrar = "conteo"),
      periodo = list(campo = "fecha_mes", inicial = Sys.Date(), fijar_titulo = "requisiciones.fijadas"),
      acciones_fila = c("detalle", "crear_provision"),
      acciones_barra = list(
        list(accion = "mover", texto = "requisiciones.accion.mover"),
        list(accion = "papelera", texto = "requisiciones.accion.papelera", confirmar = TRUE, peligro = TRUE,
             confirmar_texto = "requisiciones.confirmar_papelera"),
        list(accion = "restaurar", texto = "requisiciones.accion.restaurar")),
      acciones_ocultas = shiny::reactive({
        u <- usr()
        c(if (ver_papelera()) c("mover", "papelera") else "restaurar",
          if (!puede(u, "can_edit_requisiciones")) "mover",
          if (!puede_papelera(u)) c("papelera", "restaurar"),
          if (!puede_crear_prov()) "crear_provision")
      }),
      textos = list(contador = "requisiciones.contador"),
      vacio = "requisiciones.vacio")

    # ── Top bar ─────────────────────────────────────────────────────────────
    output$botones <- shiny::renderUI({
      u <- usr()
      if (!puede(u, "can_view_requisiciones")) return(NULL)
      htmltools::tagList(
        if (puede(u, "can_create_requisiciones"))
          shiny::actionButton(ns("nueva"), tr("requisiciones.nueva"), class = "btn btn-primary btn-sm"),
        if (puede(u, "can_create_requisiciones"))
          shiny::actionButton(ns("desde_provision"), tr("requisiciones.desde_provision"),
                              class = "btn btn-outline-primary btn-sm"))
    })
    output$boton_papelera <- shiny::renderUI({
      if (!puede_ver()) return(NULL)
      shiny::actionButton(ns("papelera_toggle"),
        if (ver_papelera()) tr("requisiciones.salir_papelera") else tr("requisiciones.ver_papelera"),
        class = paste("btn btn-sm", if (ver_papelera()) "btn-secondary" else "btn-outline-secondary"))
    })
    shiny::observeEvent(input$papelera_toggle, ver_papelera(!ver_papelera()))

    output$aviso_lectura <- shiny::renderUI({
      if (is.null(usr())) return(NULL)
      if (!puede_ver()) return(htmltools::div(class = "alert alert-secondary py-2 small", tr("requisiciones.sin_permiso")))
      err <- tryCatch({ vista(); carpetas(); NULL }, error = function(e) {
        if (inherits(e, "shiny.silent.error")) return(NULL)
        conditionMessage(e) })
      if (is.null(err)) return(NULL)
      message("[requisiciones] lectura falló: ", err)
      htmltools::div(class = "alert alert-warning py-2 small", tr("requisiciones.error.leer"))
    })

    # ── Folders (left panel) ────────────────────────────────────────────────
    # T61: Todas, General (the virtual folder: carpeta_id NA, every company,
    # never hidden, never written) and the folder tree. A folder counts its
    # whole branch, and its committed amount is only its own currency (the
    # rest is counted apart, never summed). Folding is the browser's
    # (www/requisiciones_carpetas.js: localStorage per client and folder id).
    # With "Ver papelera" on, the trashed folders follow, to restore.
    carpetas_todas <- shiny::reactive({
      version(); otro()
      c <- cid()
      if (is.null(c) || !activa_ok() || !puede_ver() || !ver_papelera()) return(NULL)
      tryCatch(requisiciones_carpetas(c, incluir_papelera = TRUE), error = function(e) NULL)
    })
    output$carpetas <- shiny::renderUI({
      if (!puede_ver()) return(NULL)
      cs <- carpetas_vis(); v <- vista_segura()
      if (is.null(cs) || is.null(v)) return(NULL)
      res <- requisiciones_carpetas_resumen(cs, v)
      vivas <- v[!(v$etapa %in% c("pagada", "papelera")), , drop = FALSE]
      ocultas <- ocultas_burbujas(vivas)
      n_ocultas <- sum(ocultas); vivas <- vivas[!ocultas, , drop = FALSE]
      sel <- carpeta_sel()
      editar <- puede(usr(), "can_edit_requisiciones")
      clic <- function(input_id, valor) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})",
                                               ns(input_id), jsonlite::toJSON(valor, auto_unbox = TRUE))
      boton <- function(valor, nombre, n, color = NULL)
        htmltools::tags$button(type = "button", class = "hdreq-carpeta-btn", onclick = clic("carpeta_click", valor),
          if (!is.null(color)) htmltools::tags$span(class = "hdreq-punto", style = paste0("background:", color, ";")),
          htmltools::tags$span(class = "hdreq-carpeta-nombre", title = nombre, nombre),
          htmltools::tags$span(class = "hdreq-carpeta-n", n))
      fijo <- function(valor, nombre, n, clase = NULL)
        htmltools::tags$li(class = paste("hdreq-carpeta", clase, if (identical(sel, valor)) "activa"),
          htmltools::div(class = "hdreq-carpeta-fila", htmltools::tags$span(class = "hdreq-plegar-hueco"),
                         boton(valor, nombre, n)))
      presupuesto <- function(i) {
        p <- res$presupuesto[i]
        otra <- if (res$n_otra_moneda[i] > 0L)
          htmltools::div(class = "hdreq-otra-moneda", tr_n("requisiciones.carpeta.otra_moneda_n", res$n_otra_moneda[i]))
        if (is.na(p) || p <= 0) return(NULL)
        pct <- min(100, round(100 * res$comprometido[i] / p))
        htmltools::div(class = "hdreq-presupuesto", title = tr("requisiciones.carpeta.presupuesto_de",
            usado = monto_moneda(res$comprometido[i], res$moneda[i] %||% NA_character_, decimales = 0L, codigo = FALSE),
            total = monto_moneda(p, res$moneda[i] %||% NA_character_, decimales = 0L)),   # "1,234 de 5,000 MXN" (T55c)
          htmltools::div(class = "hdreq-barra-p", htmltools::div(style = paste0("width:", pct, "%;"))),
          otra)
      }
      hijas <- function(p) which(if (is.na(p)) is.na(res$padre_ef) else res$padre_ef %in% p)
      rama <- function(i) {
        h <- hijas(res$id[i])
        htmltools::tags$li(class = paste("hdreq-carpeta", if (identical(sel, res$id[i])) "activa", if (length(h)) "con-hijas"),
          `data-carpeta` = res$id[i],
          htmltools::div(class = "hdreq-carpeta-fila",
            if (length(h)) htmltools::tags$button(type = "button", class = "hdreq-plegar", `aria-expanded` = "true",
                                                  title = tr("requisiciones.carpeta.plegar"), `aria-label` = tr("requisiciones.carpeta.plegar"),
                                                  onclick = "hdreqPlegar(this)", "▾")
            else htmltools::tags$span(class = "hdreq-plegar-hueco"),
            boton(res$id[i], res$nombre[i], res$n_requisiciones[i], color = res$color[i]),
            if (editar) htmltools::tags$button(type = "button", class = "hdreq-carpeta-editar",
              title = tr("requisiciones.carpeta.editar"), `aria-label` = tr("requisiciones.carpeta.editar"),
              onclick = clic("carpeta_editar", res$id[i]), "✎")),
          presupuesto(i),
          if (length(h)) htmltools::tags$ul(class = "hdreq-carpetas hdreq-sub", lapply(h, rama)))
      }
      papelera <- NULL
      if (ver_papelera()) {
        tp <- carpetas_todas()
        tp <- if (is.null(tp)) NULL else tp[!is.na(tp$papelera_en), , drop = FALSE]
        if (!is.null(tp) && nrow(tp))
          papelera <- htmltools::tagList(
            htmltools::div(class = "hdreq-lado-titulo hdreq-lado-sub", tr("requisiciones.carpeta.en_la_papelera")),
            htmltools::tags$ul(class = "hdreq-carpetas hdreq-papelera-carpetas",
              lapply(seq_len(nrow(tp)), function(i) htmltools::tags$li(class = "hdreq-carpeta",
                htmltools::div(class = "hdreq-carpeta-fila",
                  htmltools::tags$span(class = "hdreq-carpeta-nombre", title = tp$ruta[i], tp$ruta[i]),
                  if (editar) htmltools::tags$button(type = "button", class = "btn btn-link btn-sm hdreq-restaurar",
                    onclick = clic("carpeta_restaurar", tp$id[i]), tr("requisiciones.carpeta.restaurar")))))))
      }
      htmltools::tagList(
        htmltools::div(class = "hdreq-lado-titulo", tr("requisiciones.carpetas"),
          if (editar) shiny::actionLink(ns("carpeta_nueva"), tr("requisiciones.carpeta.nueva"), class = "hdreq-mas")),
        htmltools::tags$ul(class = "hdreq-carpetas hdreq-arbol", `data-cliente` = cid() %||% "",
          fijo("todas", tr("requisiciones.carpeta.todas"), nrow(vivas)),
          if (n_ocultas > 0L) htmltools::tags$li(class = "hdreq-ocultas",
            tr_n("requisiciones.carpeta.ocultas_n", n_ocultas)),
          fijo("general", tr("requisiciones.carpeta.general"), sum(is.na(vivas$carpeta_ruta)), clase = "hdreq-general"),
          lapply(hijas(NA_character_), rama)),
        papelera)
    })
    shiny::observeEvent(input$carpeta_click, carpeta_sel(as.character(input$carpeta_click)))
    # A folder that disappears (trashed in another session) stops filtering.
    shiny::observe({
      cs <- carpetas_vis(); sel <- carpeta_sel()      # trashed elsewhere, or hidden by the bubbles
      if (!is.null(cs) && !sel %in% c("todas", "general") && !sel %in% cs$id) carpeta_sel("todas")
    })

    carpeta_editando <- shiny::reactiveVal(NULL)
    # T61: where a folder goes ("Dentro de": any live folder but itself and its
    # branch, and only where the branch still fits in REQ_CARPETA_MAX_NIVEL;
    # the server checks it all again) and its companies. Every company comes
    # ticked; a new subfolder starts with its mother's. All ticked is saved as
    # "every company" (NA), so a company added later is in too.
    carp_destinos <- function(cs, fila = NULL) {
      if (is.null(cs) || !nrow(cs)) return(.reqm_opciones_carpeta(cs, "requisiciones.carpeta.raiz"))
      excluir <- character(); alto <- 1L
      if (!is.null(fila)) {
        excluir <- .req_carpeta_subarbol(cs, fila$id)
        alto <- max(cs$nivel[cs$id %in% excluir]) - cs$nivel[match(fila$id, cs$id)] + 1L
      }
      excluir <- union(excluir, cs$id[cs$nivel + alto > REQ_CARPETA_MAX_NIVEL])
      .reqm_opciones_carpeta(cs, "requisiciones.carpeta.raiz", excluir = excluir)
    }
    # The active companies, with their ids (NULL without a catalog).
    carp_empresas <- function() {
      e <- empresas_cat(); cmap <- iniciales()
      if (!is.data.frame(e) || !nrow(e) || !length(cmap)) return(NULL)
      e <- e[!(e$deleted %in% TRUE) & !is.na(e$id) & e$initials %in% names(cmap), , drop = FALSE]
      if (nrow(e)) e[order(e$initials), , drop = FALSE] else NULL
    }
    # Ids ticked for folder `carpeta_id` ("" / NA = every company).
    carp_marcadas <- function(cs, carpeta_id, emp) {
      if (is.null(carpeta_id) || is.na(carpeta_id) || !nzchar(carpeta_id) || is.null(cs)) return(emp$id)
      j <- match(carpeta_id, cs$id)
      ids <- if (is.na(j)) NULL else .req_carpeta_empresas(cs$empresas[j], existentes = emp$id)
      if (is.null(ids)) emp$id else intersect(emp$id, ids)
    }
    modal_carpeta <- function(fila = NULL, padre = NA_character_) {
      monedas <- .req_monedas()
      cs <- carpetas_segura(); emp <- carp_empresas()
      destinos <- carp_destinos(cs, fila)
      padre_sel <- if (!is.null(fila)) fila$padre_ef else padre
      if (is.null(padre_sel) || is.na(padre_sel) || !padre_sel %in% destinos) padre_sel <- ""
      hd_modal(titulo = if (is.null(fila)) tr("requisiciones.carpeta.nueva") else tr("requisiciones.carpeta.editar"),
        htmltools::div(class = "hdreq-form hd-marca",
          shiny::textInput(ns("carp_nombre"), tr("requisiciones.carpeta.nombre"), value = fila$nombre %||% "", width = "100%"),
          shiny::selectInput(ns("carp_padre"), tr("requisiciones.carpeta.dentro_de"), choices = destinos,
                             selected = padre_sel, width = "100%"),
          if (!is.null(emp)) htmltools::div(class = "hdreq-carp-empresas",
            shiny::checkboxGroupInput(ns("carp_empresas"), tr("requisiciones.carpeta.empresas"), inline = TRUE,
              choiceNames = lapply(seq_len(nrow(emp)), function(i)
                htmltools::tags$span(title = emp$nombre_corto[i] %||% emp$initials[i], emp$initials[i])),
              choiceValues = emp$id,
              selected = carp_marcadas(cs, if (is.null(fila)) padre_sel else fila$id, emp)),
            htmltools::div(class = "form-text", tr("requisiciones.carpeta.empresas_ayuda"))),
          htmltools::div(class = "hdreq-fila2",
            htmltools::tags$label(class = "form-label", `for` = ns("carp_color"), tr("requisiciones.carpeta.color"),
              htmltools::tags$input(id = ns("carp_color"), type = "color", class = "form-control form-control-color",
                                    value = fila$color %||% REQ_COLOR_DEFECTO,
                                    oninput = sprintf("Shiny.setInputValue('%s', this.value)", ns("carp_color"))))),
          htmltools::div(class = "hdreq-fila2",
            hd_numero_input(ns("carp_presupuesto"), tr("requisiciones.carpeta.presupuesto"), formato = "monto",
                                value = if (is.null(fila) || is.na(fila$presupuesto)) NA else fila$presupuesto, min = 0),
            shiny::selectInput(ns("carp_moneda"), tr("requisiciones.campo.moneda"), choices = monedas,
                               selected = if (is.null(fila) || is.na(fila$moneda)) monedas[1] else fila$moneda))),
        pie = htmltools::tagList(
          if (!is.null(fila)) shiny::actionButton(ns("carp_papelera"), tr("requisiciones.carpeta.papelera"),
                                                  class = "btn btn-outline-danger btn-sm me-auto"),
          shiny::actionButton(ns("carp_guardar"), tr("comun.guardar"), class = "btn btn-primary btn-sm")))
    }
    shiny::observeEvent(input$carpeta_nueva, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      carpeta_editando(NULL)
      sel <- carpeta_sel()
      shiny::showModal(modal_carpeta(padre = if (sel %in% c("todas", "general")) NA_character_ else sel))
    })
    # A new folder takes the companies of the folder it goes into.
    shiny::observeEvent(input$carp_padre, {
      if (!is.null(carpeta_editando())) return()
      emp <- carp_empresas(); if (is.null(emp)) return()
      shiny::updateCheckboxGroupInput(session, "carp_empresas", selected = carp_marcadas(carpetas_segura(), input$carp_padre, emp))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$carpeta_editar, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      cs <- carpetas_segura()
      fila <- if (!is.null(cs)) cs[cs$id == input$carpeta_editar, , drop = FALSE]
      if (is.null(fila) || !nrow(fila)) return()
      carpeta_editando(fila$id)
      shiny::showModal(modal_carpeta(as.list(fila)))
    })
    shiny::observeEvent(input$carp_guardar, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      id <- carpeta_editando()
      datos <- list(nombre = input$carp_nombre, color = input$carp_color %||% REQ_COLOR_DEFECTO,
                    presupuesto = input$carp_presupuesto, moneda = input$carp_moneda,
                    padre_id = if (nzchar(input$carp_padre %||% "")) input$carp_padre else NA_character_)
      emp <- carp_empresas()
      if (!is.null(emp)) {
        m <- as.character(input$carp_empresas %||% character())
        # Companies the folder had that are not offered (inactive) are kept.
        cs <- carpetas_segura(); j <- if (is.null(id) || is.null(cs)) NA else match(id, cs$id)
        extra <- setdiff(if (is.na(j)) character() else (.req_carpeta_empresas(cs$empresas[j]) %||% character()), emp$id)
        datos$empresas <- if (setequal(m, emp$id) && !length(extra)) NA_character_ else c(m, extra)
      }
      ok <- escribir(function() if (is.null(id)) requisiciones_carpeta_crear(cid(), datos, yo())
                                else requisiciones_carpeta_editar(cid(), id, datos, yo()),
                     tr("requisiciones.carpeta.guardada"))
      if (!is.null(ok)) shiny::removeModal()
    })
    shiny::observeEvent(input$carp_papelera, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      id <- carpeta_editando()
      if (is.null(id)) return()
      ok <- escribir(function() requisiciones_carpeta_papelera(cid(), id, yo()), tr("requisiciones.carpeta.en_papelera"))
      if (!is.null(ok)) { shiny::removeModal(); if (identical(carpeta_sel(), id)) carpeta_sel("todas") }
    })
    shiny::observeEvent(input$carpeta_restaurar, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      escribir(function() requisiciones_carpeta_restaurar(cid(), as.character(input$carpeta_restaurar), yo()),
               tr("requisiciones.carpeta.restaurada"))
    })

    # ── Evidence (shared by "Nueva requisición" and the detail) ─────────────
    # destino NULL = a new requisition: evidence is staged in the session and
    # saved right after the requisition. destino = an id: saved at once.
    ev_destino   <- shiny::reactiveVal(NULL)
    ev_archivos  <- shiny::reactiveVal(list())   # staged: list of adjunto_preparar() results
    ev_tablas    <- shiny::reactiveVal(list())   # staged: list(preparada, titulo, encabezado)
    pegado       <- shiny::reactiveVal(NULL)     # tabla_pegada_preparar() awaiting confirmation
    pegado_enc   <- shiny::reactiveVal(TRUE)

    ui_evidencia <- function() {
      htmltools::div(class = "hdreq-evidencia",
        htmltools::div(id = ns("zona"), class = "hdreq-zona", tabindex = "0",
          htmltools::div(class = "hdreq-zona-texto", tr("requisiciones.evidencia.zona")),
          shiny::fileInput(ns("archivo"), NULL, multiple = TRUE, width = "100%",
                           buttonLabel = tr("requisiciones.evidencia.elegir"), placeholder = "",
                           accept = c(".png", ".jpg", ".jpeg", ".webp", ".pdf", ".xlsx"))),
        shiny::uiOutput(ns("ev_pegado")),
        shiny::uiOutput(ns("ev_lista")))
    }
    enlazar_zona <- function() {
      session$sendCustomMessage("hdreq_bind", list(zona = ns("zona"), archivo = ns("archivo"),
        texto = ns("pegado_texto"), origen = ns("archivo_origen"), error = ns("pegado_error")))
    }
    shiny::observeEvent(input$pegado_error, {
      shiny::showNotification(input$pegado_error$mensaje %||% "", type = "warning", duration = 6)
    })

    shiny::observeEvent(input$archivo, {
      dest <- ev_destino()
      if (!puede_o_avisa(shared, if (is.null(dest)) "can_create_requisiciones" else "can_edit_requisiciones")) return()
      f <- input$archivo
      origen <- if (identical(input$archivo_origen, "pegado")) "pegado" else "archivo"
      for (i in seq_len(nrow(f))) {
        p <- tryCatch(adjunto_preparar(f$datapath[i], f$name[i], origen), error = function(e) e)
        if (inherits(p, "error")) {
          msg <- if (inherits(p, "req_error")) conditionMessage(p) else tr("requisiciones.evidencia.error_leer")
          shiny::showNotification(tr("requisiciones.evidencia.no_agregado", nombre = .adj_nombre_limpio(f$name[i]), motivo = msg),
                                  type = "error", duration = 8)
          next
        }
        if (is.null(dest)) {
          if (length(ev_archivos()) >= ADJ_MAX_POR_REQ) {
            shiny::showNotification(tr("requisiciones.evidencia.error_limite_adjuntos", n = ADJ_MAX_POR_REQ), type = "error")
            break
          }
          ev_archivos(c(ev_archivos(), list(p)))
        } else {
          escribir(function() evidencia_adjuntar(cid(), dest, p, yo()),
                   tr("requisiciones.evidencia.agregado", nombre = p$nombre))
        }
      }
      unlink(f$datapath)
    })

    shiny::observeEvent(input$pegado_texto, {
      dest <- ev_destino()
      if (!puede_o_avisa(shared, if (is.null(dest)) "can_create_requisiciones" else "can_edit_requisiciones")) return()
      p <- tryCatch(tabla_pegada_preparar(input$pegado_texto$texto %||% ""), error = function(e) e)
      if (inherits(p, "error")) {
        shiny::showNotification(if (inherits(p, "req_error")) conditionMessage(p) else tr("requisiciones.tabla.error_vacia"),
                                type = "warning")
        return()
      }
      pegado_enc(isTRUE(p$encabezado_sugerido))
      pegado(p)
    })

    t_pegada <- hd_table_server("pegada", tabla = "evidencia_excel",
      datos = shiny::reactive({ p <- pegado(); if (is.null(p)) NULL else .reqm_pegada(p$celdas, pegado_enc(), p$numericas)$datos }),
      columnas = shiny::reactive({ p <- pegado(); shiny::req(p); .reqm_pegada(p$celdas, pegado_enc(), p$numericas)$columnas }),
      encabezado = pegado_enc,
      aviso = shiny::reactive({ p <- pegado(); if (is.null(p)) NULL else p$aviso }))
    shiny::observeEvent(t_pegada$accion(), {
      a <- t_pegada$accion()
      if (identical(a$accion, "encabezado")) pegado_enc(identical(a$ids, "si"))
    })

    output$ev_pegado <- shiny::renderUI({
      p <- pegado()
      if (is.null(p)) return(NULL)
      htmltools::div(class = "hdreq-pegado",
        htmltools::div(class = "hdreq-pegado-cab",
          shiny::textInput(ns("pegado_titulo"), NULL, placeholder = tr("requisiciones.tabla.titulo"), width = "100%"),
          shiny::actionButton(ns("pegado_agregar"), tr("requisiciones.tabla.agregar"), class = "btn btn-primary btn-sm"),
          shiny::actionButton(ns("pegado_descartar"), tr("requisiciones.tabla.descartar"), class = "btn btn-outline-secondary btn-sm")),
        hd_table_ui(ns("pegada"), alto = "220px"))
    })
    shiny::observeEvent(input$pegado_descartar, pegado(NULL))
    shiny::observeEvent(input$pegado_agregar, {
      dest <- ev_destino()
      if (!puede_o_avisa(shared, if (is.null(dest)) "can_create_requisiciones" else "can_edit_requisiciones")) return()
      p <- pegado(); if (is.null(p)) return()
      titulo <- input$pegado_titulo %||% ""
      if (is.null(dest)) {
        if (length(ev_tablas()) >= TABLA_MAX_POR_REQ) {
          shiny::showNotification(tr("requisiciones.evidencia.error_limite_tablas", n = TABLA_MAX_POR_REQ), type = "error")
          return()
        }
        ev_tablas(c(ev_tablas(), list(list(preparada = p, titulo = titulo, encabezado = pegado_enc()))))
        pegado(NULL)
      } else {
        ok <- escribir(function() evidencia_tabla_agregar(cid(), dest, p, titulo, pegado_enc(), yo()),
                       tr("requisiciones.tabla.agregada"))
        if (!is.null(ok)) pegado(NULL)
      }
    })

    # Staged list (new requisition) or the saved evidence (detail).
    detalle_id <- shiny::reactiveVal(NULL)
    evidencia_det <- shiny::reactive({
      version(); otro()
      id <- detalle_id(); c <- cid()
      if (is.null(id) || is.null(c)) return(NULL)
      requisicion_evidencia(c, id)
    })
    miniaturas <- new.env(parent = emptyenv())   # evid id -> data URI (server re-encoded image, scaled)
    miniatura <- function(evid_id, ancho = 160) {
      k <- paste0(evid_id, "_", ancho)
      if (!is.null(miniaturas[[k]])) return(miniaturas[[k]])
      b <- tryCatch(evidencia_bytes(cid(), evid_id), error = function(e) NULL)
      if (is.null(b)) return(NULL)
      out <- tryCatch({
        img <- magick::image_read(b$bytes)
        img <- magick::image_resize(img, paste0(ancho, "x", ancho, ">"))
        bytes <- magick::image_write(img, format = "png")
        paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
      }, error = function(e) NULL)
      miniaturas[[k]] <- out
      out
    }
    lectura <- shiny::reactiveVal(FALSE)    # detail of a paid or trashed requisition: read only

    output$ev_lista <- shiny::renderUI({
      dest <- ev_destino()
      boton_quitar <- function(input_id, valor) htmltools::tags$button(type = "button",
        class = "btn btn-sm btn-link text-danger p-0 hdreq-quitar", title = tr("requisiciones.evidencia.quitar"),
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns(input_id),
                          jsonlite::toJSON(valor, auto_unbox = TRUE)), "✕")
      if (is.null(dest)) {
        a <- ev_archivos(); tb <- ev_tablas()
        if (!length(a) && !length(tb)) return(NULL)
        return(htmltools::tags$ul(class = "hdreq-ev-lista",
          lapply(seq_along(a), function(i) htmltools::tags$li(
            htmltools::tags$span(class = "hdreq-ev-tipo", toupper(a[[i]]$ext)),
            htmltools::tags$span(class = "hdreq-ev-nombre", a[[i]]$nombre),
            htmltools::tags$span(class = "hdreq-ev-tam", .reqm_tamano(a[[i]]$tamano)),
            boton_quitar("staged_quitar", paste0("a", i)))),
          lapply(seq_along(tb), function(i) htmltools::tags$li(
            htmltools::tags$span(class = "hdreq-ev-tipo", tr("requisiciones.tabla.tipo")),
            htmltools::tags$span(class = "hdreq-ev-nombre", if (nzchar(tb[[i]]$titulo)) tb[[i]]$titulo else tr("requisiciones.tabla.sin_titulo")),
            htmltools::tags$span(class = "hdreq-ev-tam", tr_n("tablas.contador.tamano", tb[[i]]$preparada$filas - (tb[[i]]$encabezado * 1L),
                                                           columnas = tr_n("tablas.contador.columnas", tb[[i]]$preparada$columnas))),
            boton_quitar("staged_quitar", paste0("t", i))))))
      }
      ev <- tryCatch(evidencia_det(), error = function(e) "error")
      if (identical(ev, "error")) return(htmltools::div(class = "alert alert-warning py-1 small", tr("requisiciones.error.leer")))
      if (is.null(ev) || !nrow(ev)) return(htmltools::div(class = "text-muted small", tr("requisiciones.evidencia.ninguna")))
      quitar <- !lectura() && puede(usr(), "can_edit_requisiciones")
      htmltools::div(class = "hdreq-ev-galeria",
        lapply(seq_len(nrow(ev)), function(i) {
          e <- ev[i, ]
          cuerpo <- if (isTRUE(e$es_imagen)) {
            src <- miniatura(e$id)
            htmltools::tags$button(type = "button", class = "hdreq-mini",
              title = e$nombre,
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("ver_imagen"),
                                jsonlite::toJSON(e$id, auto_unbox = TRUE)),
              if (!is.null(src)) htmltools::tags$img(src = src, alt = e$nombre) else htmltools::span("⚠"))
          } else if (identical(e$tipo, "tabla")) {
            htmltools::tags$button(type = "button", class = "hdreq-doc",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("ver_tabla"),
                                jsonlite::toJSON(e$id, auto_unbox = TRUE)),
              htmltools::tags$span(class = "hdreq-ev-tipo", tr("requisiciones.tabla.tipo")),
              htmltools::tags$span(class = "hdreq-ev-nombre", if (!is.na(e$titulo)) e$titulo else tr("requisiciones.tabla.sin_titulo")),
              htmltools::tags$span(class = "hdreq-ev-tam", tr_n("tablas.contador.tamano", e$filas - (isTRUE(e$encabezado) * 1L),
                                                             columnas = tr_n("tablas.contador.columnas", e$columnas))))
          } else {
            htmltools::tags$button(type = "button", class = "hdreq-doc", title = tr("requisiciones.evidencia.descargar"),
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("descargar_id"),
                                jsonlite::toJSON(e$id, auto_unbox = TRUE)),
              htmltools::tags$span(class = "hdreq-ev-tipo", toupper(e$ext)),
              htmltools::tags$span(class = "hdreq-ev-nombre", e$nombre),
              htmltools::tags$span(class = "hdreq-ev-tam", .reqm_tamano(e$tamano)))
          }
          htmltools::div(class = "hdreq-ev-item", cuerpo, if (quitar) boton_quitar("ev_quitar", e$id))
        }))
    })
    shiny::observeEvent(input$staged_quitar, {
      k <- as.character(input$staged_quitar); i <- suppressWarnings(as.integer(substring(k, 2)))
      if (is.na(i)) return()
      if (startsWith(k, "a")) { a <- ev_archivos(); if (i <= length(a)) ev_archivos(a[-i]) }
      else { tb <- ev_tablas(); if (i <= length(tb)) ev_tablas(tb[-i]) }
    })
    shiny::observeEvent(input$ev_quitar, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      escribir(function() evidencia_papelera(cid(), as.character(input$ev_quitar), yo()),
               tr("requisiciones.evidencia.quitada"))
    })

    # Downloads: always an attachment, with the stored type.
    descarga <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$descargar_id, {
      # Read-only openers (Calendario, Pasivos, history) may download too.
      if (!requisiciones_puede_ver_evidencia(usr())) { puede_o_avisa(shared, "can_view_requisiciones"); return() }
      b <- tryCatch(evidencia_bytes(cid(), as.character(input$descargar_id)), error = function(e) NULL)
      if (is.null(b)) { shiny::showNotification(tr("requisiciones.error.leer"), type = "error"); return() }
      descarga(b)
      session$sendCustomMessage("hdreq_descargar", list(id = ns("descargar")))
    })
    output$descargar <- shiny::downloadHandler(
      filename = function() {
        b <- descarga()
        n <- sub("\\.[A-Za-z0-9]+$", "", b$nombre %||% "archivo")
        n <- gsub("[^[:alnum:] ._-]", "_", n)
        paste0(n, ".", b$ext %||% "bin")
      },
      content = function(file) writeBin(descarga()$bytes, file),
      contentType = NULL)
    shiny::outputOptions(output, "descargar", suspendWhenHidden = FALSE)

    imagen_ver <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$ver_imagen, imagen_ver(as.character(input$ver_imagen)))
    tabla_ver_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$ver_tabla, tabla_ver_id(as.character(input$ver_tabla)))
    tabla_ver <- shiny::reactive({
      id <- tabla_ver_id(); if (is.null(id)) return(NULL)
      evidencia_tabla(cid(), id)
    })
    t_ver <- hd_table_server("tabla_ver", tabla = "evidencia_excel",
      datos = shiny::reactive({ x <- tabla_ver(); if (is.null(x)) NULL else .reqm_pegada(x$celdas, x$encabezado)$datos }),
      columnas = shiny::reactive({ x <- tabla_ver(); shiny::req(x); .reqm_pegada(x$celdas, x$encabezado)$columnas }))

    output$visor <- shiny::renderUI({
      img <- imagen_ver(); tb <- tabla_ver_id()
      if (!is.null(img)) {
        src <- miniatura(img, ancho = 1400)
        return(htmltools::div(class = "hdreq-visor",
          htmltools::div(class = "hdreq-visor-cab",
            shiny::actionLink(ns("visor_cerrar"), paste("←", tr("requisiciones.evidencia.volver")))),
          if (!is.null(src)) htmltools::tags$img(src = src, class = "hdreq-visor-img", alt = "")))
      }
      if (!is.null(tb)) return(htmltools::div(class = "hdreq-visor",
        htmltools::div(class = "hdreq-visor-cab",
          shiny::actionLink(ns("visor_cerrar"), paste("←", tr("requisiciones.evidencia.volver")))),
        hd_table_ui(ns("tabla_ver"), alto = "300px")))
      NULL
    })
    shiny::observeEvent(input$visor_cerrar, { imagen_ver(NULL); tabla_ver_id(NULL) })

    # ── Nueva requisición ───────────────────────────────────────────────────
    campos_form <- function(fila = NULL, lectura = FALSE) {
      monedas <- .req_monedas()
      cs <- carpetas_segura()
      carp <- .reqm_opciones_carpeta(cs)
      carpeta_ini <- if (!is.null(fila)) (fila$carpeta_id %||% "") else if (carpeta_sel() %in% c("todas", "general")) "" else carpeta_sel()
      if (is.na(carpeta_ini) || !carpeta_ini %in% carp) carpeta_ini <- ""
      empresas <- .reqm_empresas_carpeta(cs, carpeta_ini, iniciales(), empresas_cat(), actual = fila$empresa %||% NA_character_)
      prio <- c(stats::setNames("", tr("requisiciones.campo.sin_prioridad")), stats::setNames(as.character(10:1), as.character(10:1)))
      v <- function(k, def = "") { x <- fila[[k]]; if (is.null(x) || is.na(x)) def else x }
      provs <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
      nom_provs <- if (is.data.frame(provs) && "nombre" %in% names(provs)) sort(unique(stats::na.omit(provs$nombre))) else character()
      htmltools::div(class = "hdreq-form", htmltools::tags$fieldset(class = "hdreq-fs", disabled = if (lectura) NA,
        shiny::textInput(ns("f_titulo"), tr("requisiciones.campo.titulo"), value = v("titulo"), width = "100%"),
        # T61 (Luis): requisition -> folder -> companies. The folder comes
        # first (General already chosen) and the company is one of its own.
        shiny::selectInput(ns("f_carpeta"), tr("requisiciones.campo.carpeta"), choices = carp,
                           selected = carpeta_ini, width = "100%"),
        htmltools::div(class = "hdreq-fila2",
          shiny::selectInput(ns("f_empresa"), tr("requisiciones.campo.empresa"), choices = c(stats::setNames("", ""), empresas),
                             selected = v("empresa"), width = "100%"),
          shiny::selectizeInput(ns("f_proveedor"), tr("requisiciones.campo.proveedor"),
                                choices = unique(c(v("proveedor"), nom_provs)), selected = v("proveedor"), width = "100%",
                                options = list(create = TRUE, placeholder = tr("requisiciones.campo.opcional")))),
        htmltools::div(class = "hdreq-fila3",
          hd_numero_input(ns("f_monto"), tr("requisiciones.campo.monto"), formato = "monto", value = if (is.null(fila) || is.na(fila$monto)) NA else fila$monto,
                              min = 0, width = "100%"),
          shiny::selectInput(ns("f_moneda"), tr("requisiciones.campo.moneda"), choices = monedas,
                             selected = v("moneda", monedas[1]), width = "100%"),
          shiny::selectInput(ns("f_prioridad"), tr("requisiciones.campo.prioridad"), choices = prio,
                             selected = if (is.null(fila) || is.na(fila$prioridad)) "" else as.character(fila$prioridad), width = "100%")),
        shiny::textAreaInput(ns("f_notas"), tr("requisiciones.campo.notas"), value = v("notas"), rows = 3, width = "100%",
                             placeholder = tr("requisiciones.campo.notas_ayuda")) |>
          htmltools::tagAppendAttributes(maxlength = REQ_MAX_NOTAS, .cssSelector = "textarea")))
    }
    # The company choices follow the folder chosen in the form.
    shiny::observeEvent(input$f_carpeta, {
      actual <- input$f_empresa %||% ""
      f <- if (is.null(detalle_id())) NULL else { v <- vista_segura(); v[v$id %in% detalle_id(), , drop = FALSE] }
      guardada <- if (!is.null(f) && nrow(f)) f$empresa[1] else NA_character_
      op <- .reqm_empresas_carpeta(carpetas_segura(), input$f_carpeta, iniciales(), empresas_cat(), actual = guardada)
      shiny::updateSelectInput(session, "f_empresa", choices = c(stats::setNames("", ""), op),
                               selected = if (actual %in% op) actual else "")
    }, ignoreInit = TRUE)
    datos_form <- function() list(
      titulo = input$f_titulo, empresa = input$f_empresa, proveedor = input$f_proveedor,
      monto = input$f_monto, moneda = input$f_moneda,
      prioridad = if (nzchar(input$f_prioridad %||% "")) input$f_prioridad else NA,
      carpeta_id = if (nzchar(input$f_carpeta %||% "")) input$f_carpeta else NA,
      notas = input$f_notas)

    shiny::observeEvent(input$nueva, {
      if (!puede_o_avisa(shared, "can_create_requisiciones")) return()
      ev_destino(NULL); ev_archivos(list()); ev_tablas(list()); pegado(NULL); detalle_id(NULL); lectura(FALSE)
      shiny::showModal(hd_modal(titulo = tr("requisiciones.nueva"), easyClose = FALSE,
        htmltools::div(class = "hdreq-modal-dos",
          campos_form(),
          htmltools::div(htmltools::tags$h6(tr("requisiciones.evidencia.titulo")), ui_evidencia())),
        pie = shiny::actionButton(ns("crear"), tr("requisiciones.crear"), class = "btn btn-primary btn-sm")))
      enlazar_zona()
    })

    shiny::observeEvent(input$crear, {
      if (!puede_o_avisa(shared, "can_create_requisiciones")) return()
      r <- escribir(function() requisicion_crear(cid(), datos_form(), yo()))
      if (is.null(r)) return()
      fallas <- character()
      for (p in ev_archivos()) {
        ok <- tryCatch({ evidencia_adjuntar(cid(), r$id, p, yo()); TRUE }, error = function(e) {
          message("[requisiciones] adjunto falló: ", conditionMessage(e)); FALSE })
        if (!ok) fallas <- c(fallas, p$nombre)
      }
      for (tb in ev_tablas()) {
        ok <- tryCatch({ evidencia_tabla_agregar(cid(), r$id, tb$preparada, tb$titulo, tb$encabezado, yo()); TRUE },
                       error = function(e) { message("[requisiciones] tabla falló: ", conditionMessage(e)); FALSE })
        if (!ok) fallas <- c(fallas, if (nzchar(tb$titulo)) tb$titulo else tr("requisiciones.tabla.sin_titulo"))
      }
      recargar()
      ev_archivos(list()); ev_tablas(list()); pegado(NULL)
      shiny::removeModal()
      if (length(fallas))
        shiny::showNotification(tr("requisiciones.creada_con_fallas", folio = r$folio, lista = paste(fallas, collapse = ", ")),
                                type = "warning", duration = 12)
      else shiny::showNotification(tr("requisiciones.creada", folio = r$folio), type = "message", duration = 4)
    })

    # ── Detalle ─────────────────────────────────────────────────────────────
    fila_det <- shiny::reactive({
      id <- detalle_id(); v <- vista_segura()
      if (is.null(id) || is.null(v)) return(NULL)
      f <- v[v$id == id, , drop = FALSE]
      if (nrow(f)) f else NULL
    })
    # externa: a row given by abrir_por_provision() (Calendario, Pasivos, the
    # confirmation history), shown read-only whatever its stage.
    abrir_detalle <- function(id, pestana = "datos", externa = NULL) {
      if (is.null(externa)) {
        if (!puede_o_avisa(shared, "can_view_requisiciones")) return()
        v <- vista_segura()
        f <- if (!is.null(v)) v[v$id == id, , drop = FALSE]
      } else f <- externa
      if (is.null(f) || !nrow(f)) { shiny::showNotification(tr("requisiciones.error.no_existe"), type = "warning"); return() }
      f <- f[1, , drop = FALSE]
      detalle_id(id); ev_destino(id); pegado(NULL); imagen_ver(NULL); tabla_ver_id(NULL)
      solo_lectura <- !is.null(externa) || f$etapa %in% c("pagada", "papelera")
      solo_ver <- solo_lectura || !puede(usr(), "can_edit_requisiciones")
      lectura(solo_lectura)
      if (puede(usr(), "can_view_requisiciones"))   # a write: only for whoever works the list
        tryCatch(requisiciones_marcar_leido(cid(), id, yo()), error = function(e)
          message("[requisiciones] marca de leído falló: ", conditionMessage(e)))
      ins <- .reqm_mapa_etapas()[[f$etapa]]
      shiny::showModal(hd_modal(
        titulo = htmltools::tagList(htmltools::tags$span(class = "hdreq-folio", f$folio), " ", f$titulo),
        encabezado = htmltools::tags$span(class = paste0("hdt-ins hdt-tono-", ins$tono), tr(ins$texto)),
        bslib::navset_underline(id = ns("det_pestana"), selected = pestana,
          bslib::nav_panel(tr("requisiciones.det.datos"), value = "datos",
            htmltools::div(class = "hdreq-det-cuerpo",
              campos_form(as.list(f), lectura = solo_ver),
              htmltools::div(class = "hdreq-det-pie",
                htmltools::tags$span(class = "text-muted small",
                  tr("requisiciones.det.creada_por", usuario = f$creado_por, fecha = .reqm_fecha_hora(f$creado))),
                if (!solo_ver) shiny::actionButton(ns("guardar_det"), tr("comun.guardar"), class = "btn btn-primary btn-sm")))),
          bslib::nav_panel(tr("requisiciones.det.evidencia"), value = "evidencia",
            htmltools::div(class = "hdreq-det-cuerpo",
              shiny::uiOutput(ns("visor")),
              if (!lectura() && puede(usr(), "can_edit_requisiciones")) ui_evidencia() else shiny::uiOutput(ns("ev_lista")))),
          bslib::nav_panel(tr("requisiciones.det.comentarios"), value = "comentarios",
            htmltools::div(class = "hdreq-det-cuerpo", shiny::uiOutput(ns("comentarios")),
              if (!lectura()) htmltools::div(class = "hdreq-comentar",
                shiny::textAreaInput(ns("comentario"), NULL, rows = 2, width = "100%",
                                     placeholder = tr("requisiciones.comentarios.escribe")) |>
                  htmltools::tagAppendAttributes(maxlength = REQ_MAX_COMENTARIO, .cssSelector = "textarea"),
                shiny::actionButton(ns("comentar"), tr("requisiciones.comentarios.enviar"), class = "btn btn-primary btn-sm")))),
          bslib::nav_panel(tr("requisiciones.det.historial"), value = "historial",
            htmltools::div(class = "hdreq-det-cuerpo", shiny::uiOutput(ns("historial")))))))
      if (!lectura() && puede(usr(), "can_edit_requisiciones")) enlazar_zona()
    }

    shiny::observeEvent(input$guardar_det, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      id <- detalle_id(); if (is.null(id)) return()
      escribir(function() requisicion_editar(cid(), id, datos_form(), yo()), tr("requisiciones.guardada"))
    })

    comentarios_det <- shiny::reactive({
      version(); otro()
      id <- detalle_id(); if (is.null(id) || is.null(cid())) return(NULL)
      requisicion_comentarios(cid(), id)
    })
    output$comentarios <- shiny::renderUI({
      cm <- tryCatch(comentarios_det(), error = function(e) "error")
      if (identical(cm, "error")) return(htmltools::div(class = "alert alert-warning py-1 small", tr("requisiciones.error.leer")))
      if (is.null(cm) || !nrow(cm)) return(htmltools::div(class = "text-muted small", tr("requisiciones.comentarios.ninguno")))
      htmltools::tags$ul(class = "hdreq-comentarios", lapply(seq_len(nrow(cm)), function(i) htmltools::tags$li(
        htmltools::div(class = "hdreq-com-cab", htmltools::tags$strong(cm$autor[i]), " ",
                       htmltools::tags$span(class = "text-muted", .reqm_fecha_hora(cm$creado[i]))),
        htmltools::div(class = "hdreq-com-texto", cm$texto[i]))))
    })
    shiny::observeEvent(input$comentar, {
      if (!puede_o_avisa(shared, "can_view_requisiciones")) return()
      id <- detalle_id(); if (is.null(id)) return()
      r <- escribir(function() requisicion_comentar(cid(), id, input$comentario, yo()))
      if (is.null(r)) return()
      shiny::updateTextAreaInput(session, "comentario", value = "")
      if (!isTRUE(r$aviso_ok)) shiny::showNotification(tr("requisiciones.comentarios.sin_aviso"), type = "warning")
    })

    output$historial <- shiny::renderUI({
      version(); otro()
      id <- detalle_id(); if (is.null(id) || is.null(cid())) return(NULL)
      # CM35: the requisition's events -- its own history plus the status
      # changes deduced from Pasivos, the Agenda and Bancos. Only drawn when
      # this tab is open. A source that cannot be read says so: a timeline is
      # never shown incomplete as if it were complete.
      h <- tryCatch(requisiciones_eventos(cid(), usuario = yo()), error = function(e) "error")
      if (identical(h, "error")) return(htmltools::div(class = "alert alert-warning py-1 small", tr("requisiciones.error.leer")))
      h <- h[h$requisicion_id %in% id, , drop = FALSE]
      if (!nrow(h)) return(NULL)
      h <- h[order(h$en, decreasing = TRUE, method = "radix"), , drop = FALSE]
      texto <- function(i) {
        if (h$tipo[i] == "estatus")
          return(tr("requisiciones.evento.estatus", etapa = requisiciones_etapa_nombre(h$a[i])))
        tr(paste0("requisiciones.historial.", h$detalle[i]))
      }
      htmltools::tags$ul(class = "hdreq-historial", lapply(seq_len(nrow(h)), function(i) htmltools::tags$li(
        class = if (h$tipo[i] == "estatus") "hdreq-evento-estatus",
        htmltools::tags$span(class = "text-muted", .reqm_fecha_hora(h$en[i])), " · ",
        htmltools::tags$strong(if (is.na(h$por[i])) "—" else h$por[i]), " ",
        texto(i))))
    })

    # ── List actions ────────────────────────────────────────────────────────
    pend_mover <- shiny::reactiveVal(NULL)
    shiny::observeEvent(t$accion(), {
      a <- t$accion()
      if (is.null(a) || !length(a$ids)) return()
      if (a$accion %in% c("detalle", "adjuntos")) {
        abrir_detalle(a$ids[1], if (identical(a$accion, "adjuntos")) "evidencia" else "datos")
        return()
      }
      if (identical(a$accion, "crear_provision")) {
        crear_provision(a$ids[1])
        return()
      }
      if (identical(a$accion, "mover")) {
        if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
        pend_mover(a$ids)
        carp <- .reqm_opciones_carpeta(carpetas_segura())
        shiny::showModal(hd_modal(titulo = tr_n("requisiciones.mover_n", length(a$ids)),
          shiny::selectInput(ns("mover_a"), tr("requisiciones.campo.carpeta"), choices = carp, width = "100%"),
          pie = shiny::actionButton(ns("mover_ok"), tr("requisiciones.accion.mover"), class = "btn btn-primary btn-sm")))
        return()
      }
      if (a$accion %in% c("papelera", "restaurar")) {
        u <- usr()
        if (!puede_papelera(u)) { puede_o_avisa(shared, "can_trash_own_requisiciones"); return() }
        v <- vista_segura(); if (is.null(v)) return()
        filas <- v[v$id %in% a$ids, , drop = FALSE]
        permitidas <- filas$id[requisiciones_puede_papelera(u, filas)]
        negadas <- setdiff(a$ids, permitidas)
        if (length(permitidas)) {
          if (a$accion == "papelera")
            escribir(function() requisiciones_papelera(cid(), permitidas, yo()),
                     tr_n("requisiciones.en_papelera_n", length(permitidas)))
          else
            escribir(function() requisiciones_restaurar(cid(), permitidas, yo()),
                     tr_n("requisiciones.restauradas_n", length(permitidas)))
        }
        if (length(negadas))
          shiny::showNotification(tr_n("requisiciones.sin_permiso_otros_n", length(negadas)), type = "warning", duration = 8)
      }
    })
    # ── Liga con Pasivos (E3) ───────────────────────────────────────────────
    # "Crear provisión": checked on the server again (the form wired, the
    # permission, and on S3's current data that the requisition still has no
    # provision); then Pasivos' form opens prefilled. Its al_guardar links the
    # provision it saved -- to the client of the click, never another one a
    # staff session jumped to in between.
    crear_provision <- function(req_id) {
      if (!is.function(abrir_provision)) return()
      if (!puede_o_avisa(shared, "can_edit_provisiones")) return()
      if (!puede_o_avisa(shared, "can_view_requisiciones")) return()
      c0 <- cid(); if (is.null(c0)) return()
      v <- tryCatch(requisiciones_vista(c0), error = function(e) NULL)
      if (is.null(v)) { shiny::showNotification(tr("requisiciones.error.leer"), type = "error"); return() }
      f <- v[v$id %in% req_id, , drop = FALSE]
      if (!nrow(f)) { shiny::showNotification(tr("requisiciones.error.no_existe"), type = "warning"); return() }
      if (!identical(f$etapa[1], "sin_provision")) {
        shiny::showNotification(tr("requisiciones.error.ya_ligada"), type = "warning"); recargar(); return()
      }
      folio <- f$folio[1]
      al_guardar <- function(provision_id) {
        if (!identical(shiny::isolate(cid()), c0)) {
          shiny::showNotification(tr("requisiciones.error.otro_cliente", folio = folio), type = "error", duration = 10)
          return(invisible(FALSE))
        }
        if (!puede_o_avisa(shared, "can_edit_provisiones")) return(invisible(FALSE))
        ok <- escribir(function() requisicion_ligar_provision(c0, req_id, provision_id, yo()),
                       tr("requisiciones.ligada", folio = folio))
        invisible(!is.null(ok))
      }
      abrir_provision(requisicion_prefill_provision(f[1, , drop = FALSE]), al_guardar)
    }

    # Read-only detail from Calendario, Pasivos or the confirmation history:
    # the requisition behind a provision, paid or trashed ones included.
    # Returns TRUE when it opened.
    abrir_por_provision <- function(provision_id) {
      if (!requisiciones_puede_ver_evidencia(usr())) { puede_o_avisa(shared, "can_view_requisiciones"); return(invisible(FALSE)) }
      c0 <- cid(); if (is.null(c0)) return(invisible(FALSE))
      f <- tryCatch(requisicion_de_provision(c0, provision_id), error = function(e) NULL)
      if (is.null(f)) { shiny::showNotification(tr("requisiciones.error.leer"), type = "error"); return(invisible(FALSE)) }
      if (!nrow(f)) { shiny::showNotification(tr("requisiciones.error.sin_requisicion"), type = "warning"); return(invisible(FALSE)) }
      abrir_detalle(f$id[1], "evidencia", externa = f[1, , drop = FALSE])
      invisible(TRUE)
    }

    shiny::observeEvent(input$mover_ok, {
      if (!puede_o_avisa(shared, "can_edit_requisiciones")) return()
      ids <- pend_mover(); if (!length(ids)) return()
      destino <- if (nzchar(input$mover_a %||% "")) input$mover_a else NA
      ok <- escribir(function() requisiciones_mover(cid(), ids, destino, yo()), tr_n("requisiciones.movidas_n", length(ids)))
      if (!is.null(ok)) { pend_mover(NULL); shiny::removeModal() }
    })

    # ── "Agregar evidencia a una provisión" (the provisions picker) ─────────
    selector_abierto <- shiny::reactiveVal(FALSE)
    datos_selector <- shiny::reactive({
      version(); otro()
      if (!selector_abierto() || is.null(cid())) return(NULL)
      provisiones_para_evidencia(cid())
    })
    t_sel <- hd_table_server("selector", tabla = "provisiones", datos = datos_selector,
      columnas = shiny::reactive(list(
        col_fecha("fecha", "requisiciones.col.fecha"),
        col_empresa("empresa", "requisiciones.col.empresa", iniciales = iniciales()),
        col_texto("parte", "requisiciones.col.proveedor"),
        col_texto("referencia", "requisiciones.col.referencia"),
        col_dinero("monto", "requisiciones.col.monto", moneda = "moneda"),
        col_insignia("etapa", "requisiciones.col.etapa", mapa = .reqm_mapa_etapas()))),
      piezas = c("casillas", "nav_periodo"),
      periodo = list(campo = "fecha", inicial = Sys.Date()),
      acciones_barra = list(list(accion = "convertir", texto = "requisiciones.selector.convertir", confirmar = TRUE,
                                 confirmar_texto = "requisiciones.selector.confirmar")),
      textos = list(contador = "requisiciones.selector.contador"),
      vacio = "requisiciones.selector.vacio")
    shiny::observeEvent(input$desde_provision, {
      if (!puede_o_avisa(shared, "can_create_requisiciones")) return()
      selector_abierto(TRUE)
      carp <- .reqm_opciones_carpeta(carpetas_segura())
      shiny::showModal(hd_modal(titulo = tr("requisiciones.desde_provision"),
        encabezado = htmltools::div(class = "hdreq-sel-carpeta",
          shiny::selectInput(ns("sel_carpeta"), NULL, choices = carp, width = "220px")),
        htmltools::p(class = "text-muted small mb-2", tr("requisiciones.selector.ayuda")),
        hd_table_ui(ns("selector"), alto = "ventana")))
    })
    shiny::observeEvent(t_sel$accion(), {
      a <- t_sel$accion()
      if (!identical(a$accion, "convertir") || !length(a$ids)) return()
      if (!puede_o_avisa(shared, "can_create_requisiciones")) return()
      carpeta <- if (nzchar(input$sel_carpeta %||% "")) input$sel_carpeta else NA
      n0 <- length(a$ids); creadas <- character()
      r <- escribir(function() creadas <<- requisiciones_desde_provisiones(cid(), a$ids, carpeta, yo()))
      if (!is.null(r) && length(creadas)) {
        shiny::removeModal(); selector_abierto(FALSE)
        shiny::showNotification(tr_n("requisiciones.selector.creadas_n", length(creadas)), type = "message", duration = 4)
      }
      if (!is.null(r) && length(creadas) < n0)
        shiny::showNotification(tr_n("requisiciones.selector.omitidas_n", n0 - length(creadas)), type = "warning", duration = 8)
    })
    # For tests and for the integrator's wiring.
    invisible(list(tabla = t, detalle_id = detalle_id, vista = vista, datos = datos_lista,
                   carpeta_sel = carpeta_sel, ver_papelera = ver_papelera, pegado = pegado,
                   lectura = lectura, abrir_por_provision = abrir_por_provision))
  })
}
