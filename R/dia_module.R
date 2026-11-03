# =============================================================================
# R/dia_module.R
# The Calendario's day modal on the hd_table engine (T05): two tables of the
# registry over the same invoices of one day, one currency and one ledger --
# "detalle_dia_resumen" (grouped by company + counterpart) and
# "detalle_dia_detalle" (one row per invoice) -- with a switch between them
# inside the same window.
#
# What is the day's own stays with ledgerModuleServer (R/ledger_module.R):
# which rows go in (ctx$detail), and the handlers that act on them (Agenda
# with its Importe key, moves with .sync_staged, the papelera, tags from S3,
# the edit modals, the raw-data viewer). This file only turns the table's
# ids into those handlers' keys -- never positions.
# =============================================================================

# Each row's id: the invoice's own key plus what tells apart two rows that
# share it (a provision, a manual entry's uuid), unique in the day.
dia_ids <- function(detail) {
  col <- function(n) { v <- detail[[n]]; if (is.null(v)) rep("", nrow(detail)) else { v <- as.character(v); v[is.na(v)] <- ""; v } }
  make.unique(paste(col("Empresa"), col("Moneda"), col("Documento"), format(as.numeric(detail$Importe), nsmall = 2),
                    col("provision_id"), col("id"), sep = "|"), sep = "#")
}

# The day's rows of these ids, as the handlers' keys: Empresa, Moneda,
# Documento, Importe, Parte, provision_id (and source, for deletes).
dia_llaves <- function(detail, ids) {
  d <- as.data.frame(detail, stringsAsFactors = FALSE)
  filas <- match(ids, dia_ids(d))
  filas <- filas[!is.na(filas)]
  campos <- intersect(c("Empresa", "Moneda", "Documento", "Importe", "Parte", "provision_id", "source"), names(d))
  unique(d[filas, campos, drop = FALSE])
}

# The tables' data: one row per invoice of the day, in the factory order
# (largest amount first; Resumen's groups follow it), with the control
# columns. agenda_df: the Agenda's pending queue (the same one its ✓ reads,
# keyed with Importe as the day always did).
dia_datos <- function(detail, ledger, tags_df, agenda_df = NULL, puede_convertir = FALSE) {
  if (is.null(detail) || !nrow(detail)) return(NULL)
  d <- as.data.frame(detail, stringsAsFactors = FALSE)
  col <- function(n, def = "") { v <- d[[n]]; if (is.null(v)) rep(def, nrow(d)) else { v <- as.character(v); v[is.na(v)] <- def; v } }
  source    <- col("source", SAP_B1_ERP_TYPE)
  prov_id   <- col("provision_id")
  provision <- source == "provision"
  confirmed <- if ("confirmed" %in% names(d)) d$confirmed %in% TRUE else rep(FALSE, nrow(d))
  override  <- if ("has_sap_override" %in% names(d)) d$has_sap_override %in% TRUE else rep(FALSE, nrow(d))
  documento <- col("Documento")
  importe   <- abs(as.numeric(d$Importe))
  agregado <- rep(FALSE, nrow(d))
  if (!is.null(agenda_df) && nrow(agenda_df) && all(c("ledger", "status") %in% names(agenda_df))) {
    pend <- agenda_df[agenda_df$ledger == ledger & !is.na(agenda_df$status) & agenda_df$status == "pending", , drop = FALSE]
    llave <- function(e, m, doc, imp) paste(e, m, doc, format(round(as.numeric(imp), 2), nsmall = 2), sep = "\u001f")
    agregado <- llave(col("Empresa"), col("Moneda"), documento, importe) %in%
      llave(pend$Empresa, pend$Moneda, pend$Documento, pend$Importe)
  }
  movida <- col("Movida", "Falso")
  out <- data.frame(
    id            = dia_ids(d),
    Empresa       = col("Empresa"),
    Parte         = col("Parte"),
    Referencia    = col("Factura"),
    # Synthetic PROV_ keys are internal: shown as nothing, as the day always did.
    Documento     = ifelse(startsWith(documento, "PROV_"), "", documento),
    Importe       = importe,
    Moneda        = col("Moneda"),
    FechaOriginal = if ("FechaVenc_Original" %in% names(d)) as.Date(d$FechaVenc_Original) else as.Date(NA),
    Movida        = ifelse(movida == "Manual", "manual", ifelse(movida == "Políticas", "politicas", "")),
    Etiqueta      = facturas_etiquetas(data.frame(Ledger = ledger, Empresa = col("Empresa"), Moneda = col("Moneda"),
                                                  Documento = documento), tags_df),
    Origen        = ifelse(provision, "provision", ifelse(source == "manual", "manual", ifelse(override, "sap_editada", "sap"))),
    .tipo         = if (identical(ledger, "AR")) "cobro" else "pago",
    .bloqueada    = confirmed,
    .provision    = provision & nzchar(prov_id),
    # A provision converts (⚡, for whoever may) and is neither staged nor
    # edited here; a paid row takes nothing (it never could); every other row
    # stages and edits.
    .acciones     = ifelse(confirmed, "", ifelse(provision, ifelse(nzchar(prov_id) & puede_convertir, "provision", ""),
                                                 "agregar,editar")),
    .agregado     = agregado,
    stringsAsFactors = FALSE
  )
  out[order(-out$Importe), , drop = FALSE]
}

# vista: "resumen" or "detalle". Both have the same columns; what shows by
# default differs (T05, P7).
# iniciales: the company map; the day's rows carry company names, shown as
# initials with the name in the bubble (T09).
dia_columnas <- function(vista, iniciales = NULL) {
  detalle <- identical(vista, "detalle")
  list(
    col_empresa("Empresa", "tablas.facturas.empresa", iniciales = iniciales),
    col_texto("Parte", "tablas.facturas.parte"),
    col_texto("Referencia", "tablas.facturas.referencia"),
    col_texto("Documento", "tablas.facturas.documento", recorte = FALSE, opcional = TRUE, visible = detalle),
    col_fecha("FechaOriginal", "tablas.facturas.venc_original", opcional = TRUE, visible = detalle),
    col_insignia("Movida", "tablas.dia.movida", opcional = TRUE, visible = detalle, mapa = list(
      manual    = list(texto = "tablas.dia.movida_manual",    tono = "info"),
      politicas = list(texto = "tablas.dia.movida_politicas", tono = "exito"))),
    col_dinero("Importe", "tablas.facturas.importe", moneda = "Moneda"),
    col_etiqueta("Etiqueta", "tablas.facturas.etiqueta"),
    col_insignia("Origen", "tablas.facturas.origen", opcional = TRUE, visible = TRUE, mapa = list(
      sap         = list(texto = "tablas.dia.origen_sap",         tono = "neutro"),
      sap_editada = list(texto = "tablas.dia.origen_sap_editada", tono = "info"),
      manual      = list(texto = "tablas.facturas.manual",        tono = "aviso"),
      provision   = list(texto = "tablas.provision",              tono = "morado")))
  )
}

# The bar: the invoices' actions (Vencidos and the lupa), with "Mover"
# opening on the day after the one shown.
dia_acciones <- function(dia_siguiente) {
  lapply(facturas_acciones(), function(a) {
    if (identical(a$accion, "mover")) a$campo_inicial <- dia_siguiente
    a
  })
}

# The payment-policy hint for the date chosen in "Mover" (Detalle's old
# move_to_hint_ui), as the table's notice line: which counterparts' policies
# that date breaks, and where the policy would put it. NULL: nothing to say.
dia_aviso_politicas <- function(fecha, partes, pp, catalog) {
  fecha <- tryCatch(as.Date(fecha), error = function(e) as.Date(NA))
  if (!length(fecha) || is.na(fecha) || is.null(pp) || !nrow(pp) || is.null(catalog) || !nrow(catalog)) return(NULL)
  fuera <- Filter(Negate(is.null), lapply(unique(partes), function(p) {
    pp_p <- pp[tolower(trimws(pp$parte)) == tolower(trimws(p)) & !(pp$is_interco %in% TRUE), , drop = FALSE]
    if (!nrow(pp_p)) return(NULL)
    pp_p <- pp_p[order(pp_p$policy_order), ]
    pols <- Filter(Negate(is.null), lapply(pp_p$policy_id, function(pid) {
      r <- catalog[catalog$id == pid, , drop = FALSE]
      if (!nrow(r)) return(NULL)
      list(type = r$type[[1]], params = r$params[[1]] %||% list(), roll_direction = r$roll_direction[[1]] %||% "forward")
    }))
    if (!length(pols)) return(NULL)
    pol <- tryCatch(as.Date(compose_policies(fecha, pols, list())), error = function(e) as.Date(NA))
    if (is.na(pol) || pol == fecha) return(NULL)
    tr("tablas.dia.politica_parte", parte = p, fecha = format(pol, "%d/%m/%Y"))
  }))
  if (!length(fuera)) return(NULL)
  tr("tablas.dia.politicas_fuera", partes = paste(unlist(fuera), collapse = ", "))
}

# The two views in the same window, `vista` showing first. The switch
# changes which one shows without closing the window (the registry's note),
# tells the server (ns("dia_vista")) and lets the shown table fit its height.
# Both tables are the whole modal, like the lupa (alto = "ventana"): as tall
# as their rows, up to the window's bottom (Luis: "ver más cosas a la vez").
dia_ui <- function(ns, vista = "resumen") {
  boton <- function(v, clave) shiny::tags$button(
    type = "button", class = paste("btn btn-sm", if (v == vista) "btn-primary" else "btn-outline-secondary"),
    `data-vista` = v, `aria-pressed` = if (v == vista) "true" else "false", tr(clave))
  caja <- function(v) shiny::div(class = "hd-dia-vista", `data-vista` = v,
                                 style = if (v != vista) "display:none;", hd_table_ui(ns(v), alto = "ventana"))
  shiny::tagList(
    shiny::div(class = "hd-dia-cambio btn-group mb-2", role = "group", `aria-label` = tr("tablas.dia.vista"),
               `data-input` = ns("dia_vista"),
               boton("resumen", "tablas.dia.vista_resumen"), boton("detalle", "tablas.dia.vista_detalle")),
    caja("resumen"), caja("detalle"),
    facturas_convertir_js(),
    htmltools::singleton(shiny::tags$script(shiny::HTML("
      $(document).on('click', '.hd-dia-cambio button[data-vista]', function () {
        var g = this.closest('.hd-dia-cambio'), v = this.dataset.vista, raiz = g.parentNode;
        g.querySelectorAll('button').forEach(function (b) {
          var on = b.dataset.vista === v;
          b.classList.toggle('btn-primary', on); b.classList.toggle('btn-outline-secondary', !on);
          b.setAttribute('aria-pressed', on ? 'true' : 'false');
        });
        raiz.querySelectorAll('.hd-dia-vista').forEach(function (c) { c.style.display = c.dataset.vista === v ? '' : 'none'; });
        window.dispatchEvent(new Event('resize'));   // the table now shown fits its height
        if (window.Shiny) Shiny.setInputValue(g.dataset.input, v, { priority: 'event' });
      });")))
  )
}

# ctx: the open day (reactive: detail, sel_date, ...). manejadores: what
# runs each action, given the day's keys --
#   agregar(keys, origen), quitar(keys), etiquetar(keys, tags), mover(keys, fecha),
#   restaurar(keys), eliminar(keys), editar(fila), crudos()
# (ledgerModuleServer's real ones; recorders in the tests).
dia_server <- function(input, output, session, ctx, ledger, shared, manejadores) {
  ns <- session$ns
  puede_convertir <- shiny::reactive(isTRUE(tryCatch(
    has_capability(shared$current_user(), "pasivos.convert_to_item"), error = function(e) FALSE)))
  datos <- shiny::reactive({
    c <- ctx()
    if (is.null(c)) return(NULL)
    dia_datos(c$detail, ledger, tryCatch(shared$tags_db(), error = function(e) NULL),
              tryCatch(shared$pagar_hoy_db(), error = function(e) NULL), puede_convertir())
  })
  dia_siguiente <- shiny::reactive({ c <- ctx(); if (!is.null(c)) c$sel_date + 1 })
  # The date in "Mover" while the edit bar shows it (the table reports it
  # with accion "campo"); NULL otherwise -- the hint only goes with it.
  fecha_mover <- shiny::reactiveVal(NULL)
  shiny::observeEvent(ctx(), if (!isTRUE(ctx()$prov_refresh)) fecha_mover(NULL))
  aviso <- shiny::reactive({
    c <- ctx()
    if (is.null(c) || is.null(fecha_mover())) return(NULL)
    dia_aviso_politicas(fecha_mover(), c$detail$Parte,
                        tryCatch(shared$partner_policies_db(), error = function(e) NULL),
                        tryCatch(shared$policy_catalog_db(), error = function(e) NULL))
  })
  comun <- list(datos = datos, acciones_fila = c("agregar", "editar", "provision"),
                acciones_barra = dia_acciones(dia_siguiente), aviso = aviso,
                filtros = list(list(campo = "Etiqueta"), list(campo = "Origen")),
                textos = list(contador = "tablas.contador.facturas", sin_bloqueadas = TRUE))
  iniciales <- shiny::reactive(tryCatch(shared$company_map(), error = function(e) NULL))
  tablas <- list(
    resumen = do.call(hd_table_server, c(list("resumen", tabla = "detalle_dia_resumen",
                                             columnas = function() dia_columnas("resumen", iniciales()),
                                             grupo = c("Empresa", "Parte")), comun)),
    detalle = do.call(hd_table_server, c(list("detalle", tabla = "detalle_dia_detalle",
                                             columnas = function() dia_columnas("detalle", iniciales())),
                                         comun)))
  ETIQUETA <- list(etiqueta_a = "urgent", etiqueta_b = c("important", "urgent"),
                   etiqueta_c = "important", quitar_etiqueta = character(0))
  lapply(tablas, function(t) shiny::observeEvent(t$accion(), {
    a <- t$accion()
    c <- shiny::isolate(ctx())
    if (is.null(c)) return()
    if (identical(a$accion, "campo")) { fecha_mover(if (nzchar(a$valor %||% "")) a$valor); return() }
    if (identical(a$accion, "crudos")) { manejadores$crudos(); return() }
    llaves <- dia_llaves(c$detail, a$ids)
    if (!nrow(llaves)) return()
    if (identical(a$accion, "provision")) {
      pid <- llaves$provision_id[1] %||% ""
      if (nzchar(pid)) session$sendCustomMessage("hdFacturasConvertir", as.character(pid))
      return()
    }
    switch(a$accion,
      agregar_todo      = manejadores$agregar(llaves, "stage_all"),
      agregar_seleccion = manejadores$agregar(llaves, "stage_sel"),
      # A row's ＋ or its group's (de = "grupo"), named in the log as before.
      agregar           = manejadores$agregar(llaves, if (identical(a$de, "grupo")) "cart_toggle" else "cart_inv_click"),
      quitar_agenda     = manejadores$quitar(llaves),
      etiqueta_a = , etiqueta_b = , etiqueta_c = , quitar_etiqueta = manejadores$etiquetar(llaves, ETIQUETA[[a$accion]]),
      mover             = manejadores$mover(llaves, as.Date(a$valor)),
      restaurar         = manejadores$restaurar(llaves),
      eliminar          = manejadores$eliminar(llaves),
      editar            = manejadores$editar(c$detail[match(a$ids[1], dia_ids(c$detail)), , drop = FALSE]),
      NULL)
  }))
  invisible(tablas)
}
