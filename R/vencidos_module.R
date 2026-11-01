# =============================================================================
# R/vencidos_module.R
# "Vencidos" tab — all unconfirmed AR + AP invoices due today or earlier.
# Respects empresa_sel filter. Always excludes intercompany.
# Edit actions (tag / move / delete / stage) share handle_invoice_action()
# with the monthly search modal (search_module.R).
#
# The table is the hd_table engine (R/hd_table.R, www/hdtable.js), table
# "vencidos" of the registry. Its columns, pieces and actions come from the
# facturas_*() functions below, which the lupa (search_module.R) uses too,
# so the two tables can't drift apart. This module keeps what is its own:
# which rows go in (overdue as of today), the tab's badge, the empty message
# and the counter's text.
# =============================================================================

# ── Shared with the lupa: one table of invoices ────────────────────────────────

# Stored tag of each row (urgent / both / important / ""), from tags_db, by
# ledger + Empresa + Moneda + Documento -- the same join add_tag_labels() does.
facturas_etiquetas <- function(df, tags_df) {
  out <- rep("", nrow(df))
  if (is.null(tags_df) || !nrow(tags_df) || !nrow(df)) return(out)
  llave <- function(l, e, m, d) paste(l, e, m, d, sep = "\u001f")
  tg <- split(as.character(tags_df$tag), llave(tags_df$ledger, tags_df$Empresa, tags_df$Moneda, tags_df$Documento))
  k  <- llave(df$Ledger, df$Empresa, df$Moneda, df$Documento)
  vapply(k, function(x) {
    t <- tg[[x]]
    if (is.null(t)) ""
    else if (all(c("urgent", "important") %in% t)) "both"
    else if ("urgent" %in% t) "urgent"
    else if ("important" %in% t) "important"
    else ""
  }, character(1), USE.NAMES = FALSE)
}

# Each invoice's id in the table: its ledger + company + currency + document
# + source + ids, unique even if two rows share a document. Built from the
# module's data in its own order, so the same data always gives the same ids.
facturas_ids <- function(df) {
  col <- function(n, def = "") { v <- df[[n]]; if (is.null(v)) rep(def, nrow(df)) else { v <- as.character(v); v[is.na(v)] <- def; v } }
  make.unique(paste(col("Ledger"), col("Empresa"), col("Moneda"), col("Documento"), col("source", SAP_B1_ERP_TYPE),
                    col("id"), col("provision_id"), sep = "|"), sep = "#")
}

# The data the table gets: one row per invoice, with its control columns.
# The order is the factory one (D3/G12): groups and loose rows by their
# strongest tag (A, B, C, none), then by amount, largest first; inside a group
# the same. The table's third click on a title comes back to it.
facturas_datos <- function(df, tags_df, agenda_df = NULL, puede_convertir = FALSE) {
  vacio <- data.frame(
    id = character(), Tipo = character(), Clase = character(), Empresa = character(),
    Fecha = as.Date(character()), Parte = character(), Referencia = character(),
    Documento = character(), Origen = character(), FechaOriginal = as.Date(character()),
    Importe = numeric(), Moneda = character(), Etiqueta = character(),
    .tipo = character(), .bloqueada = logical(), .provision = logical(), .acciones = character(),
    .agregado = logical(), stringsAsFactors = FALSE)
  if (is.null(df) || !nrow(df)) return(vacio)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  col <- function(n, def = "") { v <- df[[n]]; if (is.null(v)) rep(def, nrow(df)) else { v <- as.character(v); v[is.na(v)] <- def; v } }
  ledger    <- col("Ledger")
  empresa   <- col("Empresa")
  moneda    <- col("Moneda")
  documento <- col("Documento")
  source    <- col("source", SAP_B1_ERP_TYPE)
  inv_id    <- col("id")
  prov_id   <- col("provision_id")
  confirmed <- if ("confirmed" %in% names(df)) !is.na(df$confirmed) & as.logical(df$confirmed) else rep(FALSE, nrow(df))
  provision <- source == "provision" & nzchar(prov_id)
  origen <- vapply(source, function(s) switch(s,
    # Both the historical literal "sap" (pre-erp_type records)
    # and today's SAP_B1_ERP_TYPE value map to the same label --
    # never remove either case, they cover different eras of data.
    "sap"                  = "SAP",
    "sap_b1_service_layer" = "SAP",
    "manual"    = tr("tablas.facturas.manual"),
    "provision" = tr("tablas.provision"),
    "SAP"), character(1), USE.NAMES = FALSE)
  etiqueta <- facturas_etiquetas(data.frame(Ledger = ledger, Empresa = empresa, Moneda = moneda, Documento = documento),
                                 tags_df)
  # Already in Agenda de Hoy: the same pending queue the Agenda reads.
  agregado <- rep(FALSE, nrow(df))
  if (!is.null(agenda_df) && nrow(agenda_df) && "status" %in% names(agenda_df)) {
    pend <- agenda_df[!is.na(agenda_df$status) & agenda_df$status == "pending", , drop = FALSE]
    k_ag <- paste(pend$ledger, pend$Empresa, pend$Moneda, pend$Documento, sep = "\u001f")
    agregado <- paste(ledger, empresa, moneda, documento, sep = "\u001f") %in% k_ag
  }
  out <- data.frame(
    id            = facturas_ids(df),
    # A provision shows only its own badge ("Provisión"), as today.
    Tipo          = ifelse(provision, "", ifelse(ledger == "AR", "cobro", "pago")),
    Clase         = ifelse(ledger == "AR", tr("tablas.facturas.cobro"), tr("tablas.facturas.pago")),
    Empresa       = empresa,
    Fecha         = as.Date(df$Fecha),
    Parte         = col("Parte"),
    Referencia    = col("Factura"),
    Documento     = documento,
    Origen        = origen,
    FechaOriginal = if ("FechaOriginal" %in% names(df)) as.Date(df$FechaOriginal) else as.Date(NA),
    Importe       = as.numeric(df$Importe),
    Moneda        = moneda,
    Etiqueta      = etiqueta,
    .tipo         = ifelse(ledger == "AR", "cobro", "pago"),
    .bloqueada    = confirmed,
    .provision    = provision,
    # The ⚡ only for whoever may convert; the pasivos module checks again.
    .acciones     = ifelse(provision & puede_convertir, "provision", ""),
    .agregado     = agregado,
    stringsAsFactors = FALSE
  )
  # Factory order: by unit (a group of company + date + counterpart inside
  # its currency and type, or a loose row), strongest tag first, then the
  # unit's amount without paid rows; inside a unit, tag then amount.
  peso <- vapply(etiqueta, function(e) if (nzchar(e)) HD_ETIQUETAS[[e]]$peso else 4L, integer(1), USE.NAMES = FALSE)
  unidad <- ifelse(nzchar(empresa), paste(moneda, ledger, empresa, out$Fecha, out$Parte, sep = "\u001f"),
                   paste0("__solo__", seq_len(nrow(out))))
  unidad[unidad %in% names(which(table(unidad) < 2))] <- paste0("__solo__", which(unidad %in% names(which(table(unidad) < 2))))
  libre_imp <- ifelse(confirmed, 0, out$Importe)
  u_peso <- ave(peso, unidad, FUN = min)
  u_suma <- ave(libre_imp, unidad, FUN = sum)
  out[order(u_peso, -u_suma, unidad, peso, -out$Importe), , drop = FALSE]
}

# Fill missing Empresa (D4).
# Rows from old snapshots or manual entries may lack Empresa; without it
# they cannot be staged (pagar_hoy_module filters per-empresa).
#   1. Look it up by Documento + Ledger among the rows that have it
#      (SAP rows always carry Empresa).
#   2. Fall back to empresa_sel when it narrows to exactly one company.
facturas_llenar_empresa <- function(df, shared) {
  if (is.null(df) || !nrow(df)) return(df)
  emp <- as.character(df[["Empresa"]] %||% rep(NA_character_, nrow(df)))
  blank <- !nzchar(emp, keepNA = FALSE)
  if (any(blank)) {
    llave <- paste(df[["Ledger"]], df[["Documento"]])
    conocidas <- tapply(emp[!blank], llave[!blank], `[`, 1)
    emp[blank] <- unname(conocidas[llave[blank]])
    blank <- !nzchar(emp, keepNA = FALSE)
    emp_now <- tryCatch(shared$empresa_sel(), error = function(e) character(0))
    if (any(blank) && length(emp_now) == 1L) emp[blank] <- emp_now[1L]
    emp[is.na(emp)] <- ""
    df[["Empresa"]] <- emp
  }
  df
}

# visibles: optional columns each table shows until the user hides them
# (the lupa shows Documento, as it always did; Vencidos doesn't).
facturas_columnas <- function(visibles = character()) {
  list(
    col_insignia("Tipo", "tablas.facturas.tipo", mapa = list(
      cobro = list(texto = "tablas.facturas.cobro", tono = "info"),
      pago  = list(texto = "tablas.facturas.pago",  tono = "exito"))),
    col_empresa("Empresa", "tablas.facturas.empresa"),
    col_fecha("Fecha", "tablas.facturas.fecha_venc"),
    col_texto("Parte", "tablas.facturas.parte"),
    col_texto("Referencia", "tablas.facturas.referencia"),
    col_texto("Documento", "tablas.facturas.documento", opcional = TRUE, visible = "Documento" %in% visibles, ancho = 80),
    col_texto("Origen", "tablas.facturas.origen", recorte = FALSE, opcional = TRUE, visible = FALSE),
    col_fecha("FechaOriginal", "tablas.facturas.venc_original", opcional = TRUE, visible = FALSE),
    col_dinero("Importe", "tablas.facturas.importe", moneda = "Moneda"),
    col_texto("Moneda", "tablas.facturas.moneda", recorte = FALSE, opcional = TRUE, visible = FALSE),
    col_etiqueta("Etiqueta", "tablas.facturas.etiqueta")
  )
}

# Bar actions: stage to Agenda de Hoy (both confirm: "lo que va en bloque",
# in the words they always had), behind Editar the tags, moving and
# restoring the date, and deleting; and "Ver datos SAP", named as the lupa
# always named it.
facturas_acciones <- function() {
  list(
    list(accion = "agregar_todo", texto = "tablas.accion.agregar_todo", sin_seleccion = TRUE, confirmar = TRUE,
         confirmar_texto = "tablas.facturas.confirmar_agregar"),
    list(accion = "agregar_seleccion", texto = "tablas.accion.agregar_seleccion", confirmar = TRUE,
         confirmar_texto = "tablas.facturas.confirmar_agregar_sel"),
    # Each tag button in its tag's colour (HD_ETIQUETAS). Tagging unmarks
    # the rows when done (Luis: a click after it would add to the old ones);
    # moving, restoring and staging keep them (Dif5).
    list(accion = "etiqueta_a", texto = "tablas.accion.etiqueta_a", modo = "editar", tono = HD_ETIQUETAS$urgent$tono,
         limpiar_tras = TRUE),
    list(accion = "etiqueta_b", texto = "tablas.accion.etiqueta_b", modo = "editar", tono = HD_ETIQUETAS$both$tono,
         limpiar_tras = TRUE),
    list(accion = "etiqueta_c", texto = "tablas.accion.etiqueta_c", modo = "editar", tono = HD_ETIQUETAS$important$tono,
         limpiar_tras = TRUE),
    list(accion = "quitar_etiqueta", texto = "tablas.accion.quitar_etiqueta", modo = "editar", limpiar_tras = TRUE),
    list(accion = "mover", texto = "tablas.accion.mover", modo = "editar", campo = "fecha"),
    list(accion = "restaurar", texto = "tablas.accion.restaurar", modo = "editar"),
    list(accion = "eliminar", texto = "tablas.accion.eliminar", modo = "editar", confirmar = TRUE, peligro = TRUE,
         confirmar_texto = "tablas.facturas.confirmar_eliminar"),
    list(accion = "crudos", texto = "tablas.datos_sap.ver"))
}

# The table's action -> handle_invoice_action(), with the rows by id.
FACTURAS_ACCION <- c(
  agregar_todo = "stage_all", agregar_seleccion = "stage_all",
  etiqueta_a = "tag_urgent", etiqueta_b = "tag_both", etiqueta_c = "tag_important", quitar_etiqueta = "tag_clear",
  mover = "move", restaurar = "restore", eliminar = "delete"
)

# The row payload handle_invoice_action() has always taken (from the old
# venRowPayload() / getCheckedRows()), built from the data by id.
facturas_payload <- function(df, ids) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  filas <- match(ids, df$.id_tabla)
  filas <- filas[!is.na(filas)]
  txt <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  lapply(filas, function(i) list(
    ledger       = txt(df$Ledger[i]),
    empresa      = txt(df$Empresa[i]),
    moneda       = txt(df$Moneda[i]),
    documento    = txt(df$Documento[i]),
    source       = txt(df$source[i] %||% SAP_B1_ERP_TYPE),
    inv_id       = txt(df$id[i] %||% ""),
    provision_id = txt(df$provision_id[i] %||% ""),
    parte        = txt(df$Parte[i]),
    codigo       = txt(df$Codigo[i] %||% ""),
    importe      = as.numeric(df$Importe[i]),
    fecha        = format(as.Date(df$Fecha[i]), "%d/%m/%Y"),
    tipo         = if (identical(df$Ledger[i], "AR")) "Cobro" else "Pago"
  ))
}

# The invoices table, the same for Vencidos and the lupa. `datos` is the
# module's invoices (Ledger, Empresa, Fecha, Parte, Importe, ...); `crudos`
# runs on "Ver datos SAP" with the marked rows (maybe none); `despues`, if
# given, after each action reaches accion_factura (the lupa reloads its month).
facturas_tabla_server <- function(id, tabla, datos, shared, crudos, accion_factura = handle_invoice_action,
                                  vacio = NULL, textos = NULL, periodo = NULL, despues = NULL,
                                  visibles = character()) {
  moduleServer(id, function(input, output, session) {
    puede_convertir <- reactive(isTRUE(tryCatch(
      has_capability(shared$current_user(), "pasivos.convert_to_item"), error = function(e) FALSE)))
    base <- reactive({
      df <- datos()
      if (is.null(df) || !nrow(df)) return(NULL)
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      df
    })
    tabla_df <- reactive({
      df <- base()
      out <- facturas_datos(df, tryCatch(shared$tags_db(), error = function(e) NULL),
                            tryCatch(shared$pagar_hoy_db(), error = function(e) NULL), puede_convertir())
      out
    })
    t <- hd_table_server("t", tabla = tabla, datos = tabla_df, columnas = facturas_columnas(visibles),
                         secciones = list(list(campo = "Moneda", orden = "MXN"), ".tipo"),
                         grupo = c("Empresa", "Fecha", "Parte"),
                         filtros = list(list(campo = "Clase", titulo = "tablas.facturas.tipo"),
                                        list(campo = "Etiqueta")),
                         acciones_fila = "provision",
                         acciones_barra = facturas_acciones(),
                         vacio = vacio, textos = textos, periodo = periodo)

    shiny::observeEvent(t$accion(), {
      a <- t$accion()
      df <- base()
      if (is.null(df)) return()
      df$.id_tabla <- facturas_ids(df)   # the table's ids, row by row
      if (identical(a$accion, "provision")) {
        fila <- df[df$.id_tabla %in% a$ids, , drop = FALSE]
        if (nrow(fila) && nzchar(fila$provision_id[1] %||% ""))
          session$sendCustomMessage("hdFacturasConvertir", as.character(fila$provision_id[1]))
        return()
      }
      if (identical(a$accion, "crudos")) { crudos(a$ids, df); return() }
      accion <- FACTURAS_ACCION[a$accion]
      if (is.na(accion)) return()
      payload <- list(action = unname(accion), rows = facturas_payload(df, a$ids), nonce = a$nonce)
      if (identical(a$accion, "mover")) payload$move_to <- a$valor
      accion_factura(payload, shared, session = session)
      if (is.function(despues)) despues()
    })
    t
  })
}

# alto: the table's maximum height (a whole tab: what's left of the window).
# The ⚡ of a provision opens the pasivos module's convert dialog (once per
# page, whichever table shows first).
facturas_tabla_ui <- function(id, alto = "auto") {
  shiny::tagList(
    hd_table_ui(shiny::NS(id)("t"), alto = alto),
    htmltools::singleton(shiny::tags$script(shiny::HTML("
      (function registrar() {
        if (!(window.Shiny && Shiny.addCustomMessageHandler)) { setTimeout(registrar, 20); return; }
        Shiny.addCustomMessageHandler('hdFacturasConvertir', function (id) {
          Shiny.setInputValue('pasivos_convert_request', id, { priority: 'event' });
        });
      })();"))))
}

# Raw SAP rows behind the overdue invoices (due today or earlier), for
# "Ver datos SAP": all of them, as the lupa shows its whole month.
vencidos_crudos <- function(shared) {
  raw_list <- tryCatch(shared$sap_data(), error = function(e) NULL)
  if (is.null(raw_list)) return(NULL)
  emp <- tryCatch(shared$empresa_sel(), error = function(e) character(0))
  dplyr::bind_rows(lapply(c("AR", "AP"), function(ldg) {
    r <- raw_list[[ldg]]
    if (is.null(r) || !nrow(r)) return(NULL)
    date_col <- if ("Fecha de vencimiento" %in% names(r)) "Fecha de vencimiento"
                else if ("FechaEff" %in% names(r)) "FechaEff" else NULL
    if (!is.null(date_col)) r <- r[!is.na(r[[date_col]]) & as.Date(r[[date_col]]) <= Sys.Date(), , drop = FALSE]
    if (length(emp) && "Empresa" %in% names(r)) r <- r[r[["Empresa"]] %in% emp, , drop = FALSE]
    if (nrow(r)) { r[["Ledger"]] <- ldg; r } else NULL
  }))
}

# ── UI ────────────────────────────────────────────────────────────────────────

vencidosUI <- function(id) {
  ns <- NS(id)
  tagList(
    # The table is the whole tab: it fills what's left of the window below
    # it, footer included (alto = "ventana").
    div(class = "vencidos-wrap p-3", facturas_tabla_ui(ns("tabla"), alto = "ventana")),
    # The tab's badge (count of open overdue invoices).
    tags$script(HTML("
      $(document).on('shiny:connected', function () {
        Shiny.addCustomMessageHandler('vencidosBadge', function (msg) {
          var el = document.getElementById('ven_tab_badge');
          if (!el) return;
          if (msg.count > 0) { el.textContent = msg.count; el.style.display = ''; }
          else el.style.display = 'none';
        });
      });"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

# accion_factura: what runs an edit or stage action; handle_invoice_action()
# in the app, a recorder in the tests (no S3).
vencidosServer <- function(id, shared, accion_factura = handle_invoice_action) {
  moduleServer(id, function(input, output, session) {

    # ── Build overdue data ─────────────────────────────────────────────────
    vencidos_df <- reactive({
      # Consume the same df_combined() the calendar uses — empresa filter,
      # papelera, confirmed marks, and provisions are already applied there.
      ic_reg     <- shared$interco_v2()
      tags       <- shared$tags_db()
      ic_mode_ar <- tryCatch(shared$ic_mode[["AR"]](), error = function(e) "exclude")
      ic_mode_ap <- tryCatch(shared$ic_mode[["AP"]](), error = function(e) "exclude")
      today      <- Sys.Date()

      build_one <- function(ledger_name, ic_mode_val = "exclude") {
        df <- tryCatch(
          if (ledger_name == "AR") shared$df_combined_AR() else shared$df_combined_AP(),
          error = function(e) NULL
        )
        if (is.null(df) || !nrow(df)) return(NULL)
        # ── IC filter (two layers for robustness) ──────────────────────────
        # Layer 1: code-based filter (from registered interco_v2 registry)
        codes    <- build_ic_fullcodes(ic_reg, ledger_name)
        ic_rfcs  <- unique(toupper(trimws(unname(ic_reg$rfcs %||% character()))))
        ic_rfcs  <- ic_rfcs[nzchar(ic_rfcs)]
        code_col <- if (ledger_name == "AR") "Código de cliente" else "Código de proveedor"
        df <- apply_ic_filter(df, mode = ic_mode_val, code_col = code_col, ic_codes = codes, ic_rfcs = ic_rfcs,
                              cmap = shared$company_map())
        if (!is.null(df) && !nrow(df)) return(NULL)

        # Layer 2: Parte-name fallback (catches IC when codes not yet registered)
        # If Parte matches any company's display name, the invoice is intercompany.
        if (!is.null(df) && nrow(df) && "Parte" %in% names(df)) {
          company_names_up <- toupper(unname(shared$company_map()))
          is_ic_by_name    <- toupper(trimws(df[["Parte"]])) %in% company_names_up
          if (any(is_ic_by_name)) {
            if (ic_mode_val == "exclude") {
              df <- df[!is_ic_by_name, , drop = FALSE]
              message("[VEN] IC-by-name filter removed ", sum(is_ic_by_name),
                      " rows for ledger=", ledger_name)
            } else if (ic_mode_val == "only") {
              df <- df[is_ic_by_name, , drop = FALSE]
            }
            # "include" → keep everything, no change needed
          } else if (ic_mode_val == "only") {
            # No name-matches found but mode is "only" → nothing to show
            df <- df[FALSE, , drop = FALSE]
          }
        }
        if (is.null(df) || !nrow(df)) return(NULL)

        # Date filter: due today or overdue
        df <- df[!is.na(df[["FechaEff"]]) & as.Date(df[["FechaEff"]]) <= today, , drop = FALSE]
        if (!nrow(df)) return(NULL)

        df[["Ledger"]]        <- ledger_name
        df[["Tipo"]]          <- if (ledger_name == "AR") "Cobro" else "Pago"
        df[["Importe"]]       <- abs(tidyr::replace_na(df[["Saldo vencido"]], 0))
        df[["Fecha"]]         <- as.Date(df[["FechaEff"]])
        df[["FechaOriginal"]] <- if ("FechaVenc_Original" %in% names(df))
          as.Date(df[["FechaVenc_Original"]]) else as.Date(NA)
        df
      }

      ar <- tryCatch(build_one("AR", ic_mode_ar), error = function(e) { message("[VEN] AR error: ", e$message); NULL })
      ap <- tryCatch(build_one("AP", ic_mode_ap), error = function(e) { message("[VEN] AP error: ", e$message); NULL })
      # Papelera, empresa, and confirmed filters already applied in df_combined().
      dplyr::bind_rows(ar, ap)
    })

    # ── Badge ──────────────────────────────────────────────────────────────
    # Count only unconfirmed rows — confirmed SAP ghosts are shown but not
    # counted. Reuses the `confirmed` column df_combined() (ledger_module.R)
    # already computes from every confirmation source (bancos_confirmados,
    # pagar_hoy_db, legacy conciliacion, papelera), with proper
    # toupper(trimws(...)) normalization on Empresa+Documento+Moneda — do NOT
    # re-derive confirmed status here. This used to reimplement its own
    # independent (and weaker — no case/whitespace normalization, and the
    # bancos_confirmados key was missing Moneda entirely) version, which could
    # silently drift from what the calendar itself shows.
    observe({
      df <- vencidos_df()
      if (is.null(df) || !nrow(df)) {
        session$sendCustomMessage("vencidosBadge", list(count = 0L)); return()
      }
      confirmed <- if ("confirmed" %in% names(df))
        !is.na(df[["confirmed"]]) & df[["confirmed"]]
      else
        rep(FALSE, nrow(df))
      # Manual confirmed disappear; SAP confirmed become ghosts — both excluded from badge.
      session$sendCustomMessage("vencidosBadge", list(count = sum(!confirmed)))
    })

    # ── The table's data ───────────────────────────────────────────────────
    datos <- reactive(facturas_llenar_empresa(vencidos_df(), shared))

    facturas_tabla_server(
      "tabla", tabla = "vencidos", datos = datos, shared = shared,
      crudos = function(ids, df) mostrar_datos_sap(vencidos_crudos(shared)),
      accion_factura = accion_factura,
      vacio  = list(texto = "tablas.vencidos.vacio", tono = "exito"),
      # The counter, like the tab's badge, leaves paid rows out.
      textos = list(contador = "tablas.contador.vencidas", sin_bloqueadas = TRUE))
  })
}
