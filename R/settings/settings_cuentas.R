# =============================================================================
# R/settings/settings_cuentas.R
# Cuentas de Empresa UI helpers and settings_cuentas_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# Cuentas de Empresa
# =============================================================================
# editar = FALSE (no can_manage_bancos): the tables still show, without the
# buttons that write; the server checks again in each save.
.cuentas_panel_ui <- function(cmap, editar = TRUE) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
        tagList(icon("building-columns"), " Cuentas de Empresa")),
      tags$span(class = "text-muted small",
        "— La cuenta marcada ",
        tags$span(class = "badge bg-primary", "PPL"),
        " se usa como cuenta origen al generar archivos Baj\u00edo.")
    ),

    div(class = "row g-3",

      # ── Banco catalog (left) ─────────────────────────────────────────────
      div(class = "col-md-4",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center py-2",
            tags$span(class = "fw-semibold small flex-grow-1",
                      tagList(icon("university"), " Bancos")),
            if (editar)
              actionButton("cta_new_banco", tagList(icon("plus"), " Banco"),
                           class = "btn btn-sm btn-outline-primary py-0 px-2")
          ),
          div(class = "card-body p-0",
            hd_table_ui("bancos_cat_t", alto = "auto")
          )
        )
      ),

      # ── Accounts (right) ─────────────────────────────────────────────────
      div(class = "col-md-8",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center gap-2 py-2",
            tags$span(class = "fw-semibold small", tagList(icon("credit-card"), " Cuentas")),
            div(class = "ms-auto d-flex gap-2",
              div(style = "width:175px;",
                selectInput("cta_empresa_filter", NULL,
                  choices = c("Todas las empresas" = "",
                              setNames(names(cmap), unname(cmap))),
                  width = "100%")
              ),
              if (editar)
                actionButton("cta_new_cuenta", tagList(icon("plus"), " Cuenta"),
                             class = "btn btn-sm btn-outline-primary")
            )
          ),
          div(class = "card-body p-0",
            hd_table_ui("cuentas_t", alto = "auto")
          )
        )
      )
    ),

    tags$hr(class = "my-3"),
    uiOutput("cuentas_edit_form")
  )
}

# Rows of the bank catalog (bancos_cat, T16): id, nombre, clave.
.bancos_cat_filas <- function(bancos) {
  if (!is.data.frame(bancos) || !nrow(bancos))
    return(data.frame(id = character(), nombre = character(), clave = character(), stringsAsFactors = FALSE))
  data.frame(id = as.character(bancos$id), nombre = as.character(bancos$nombre),
             clave = as.character(bancos$clave), stringsAsFactors = FALSE)
}

# Rows of the company accounts (cuentas_empresa, T16): the active ones of
# `empresa` ("" = all), with their bank's name and "ppl" on the PPL source
# account. Selected by id, never by position.
.cuentas_empresa_filas <- function(ctas, bancos, empresa = "") {
  vacia <- data.frame(id = character(), ppl = character(), Empresa = character(), banco = character(),
                      Moneda = character(), alias = character(), cuenta = character(),
                      razon_social = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(ctas) || !nrow(ctas)) return(vacia)
  ctas <- ctas[!is.na(ctas$activa) & ctas$activa, , drop = FALSE]
  if (nzchar(empresa %||% "")) ctas <- ctas[!is.na(ctas$Empresa) & ctas$Empresa == empresa, , drop = FALSE]
  if (!nrow(ctas)) return(vacia)
  banco <- if (is.data.frame(bancos) && nrow(bancos)) bancos$nombre[match(ctas$banco_id, bancos$id)] else NA_character_
  data.frame(id = as.character(ctas$id),
             ppl = ifelse(!is.na(ctas$is_ppl_default) & ctas$is_ppl_default, "ppl", NA_character_),
             Empresa = as.character(ctas$Empresa), banco = as.character(banco),
             Moneda = as.character(ctas$Moneda), alias = as.character(ctas$alias),
             cuenta = as.character(ctas$cuenta), razon_social = as.character(ctas$razon_social),
             stringsAsFactors = FALSE)
}

# ── Cuentas observer (called once from app.R server) ──────────────────────────
settings_cuentas_observer <- function(input, output, session, shared) {

  editing_cuenta_id <- reactiveVal(NULL)
  editing_banco_id  <- reactiveVal(NULL)
  # Without can_manage_bancos the forms open read-only (no Guardar/Eliminar).
  .puede_bancos <- function() puede(tryCatch(isolate(shared$usuario()), error = function(e) NULL), "can_manage_bancos")

  # ── The two tables (T16: hd_table, rows by id) ──────────────────────────────
  # After a save or a delete the tables re-read S3: this observer runs after
  # the save handlers (lower priority), so they need no line of their own.
  refrescar <- reactiveVal(0L)
  observeEvent(list(input$banco_save, input$banco_delete, input$cuenta_save, input$cuenta_delete),
               refrescar(refrescar() + 1L), ignoreInit = TRUE, priority = -10)
  .leer_bancos <- function()
    tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
  .leer_cuentas <- function() tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas(client_id = shared$effective_client_id())
  }, error = function(e) .schema_ctas_cuentas())

  t_bancos <- hd_table_server(
    "bancos_cat_t", tabla = "bancos_cat",
    datos = reactive({ refrescar(); .bancos_cat_filas(.leer_bancos()) }),
    columnas = list(
      col_texto("nombre", "tablas.ajustes.bancos.banco"),
      col_texto("clave",  "tablas.ajustes.bancos.clave", recorte = FALSE)
    ),
    acciones_fila = "editar",
    vacio = "tablas.ajustes.bancos.vacio"
  )
  t_cuentas <- hd_table_server(
    "cuentas_t", tabla = "cuentas_empresa",
    datos = reactive({
      refrescar()
      .cuentas_empresa_filas(.leer_cuentas(), .leer_bancos(), input$cta_empresa_filter %||% "")
    }),
    columnas = list(
      col_insignia("ppl", "tablas.ajustes.cuentas.ppl", mapa = list(
        ppl = list(texto = "tablas.ajustes.cuentas.ppl", tono = "info"))),
      col_empresa("Empresa", "tablas.ajustes.cuentas.empresa"),
      col_texto("banco",        "tablas.ajustes.cuentas.banco"),
      col_texto("Moneda",       "tablas.ajustes.cuentas.moneda", recorte = FALSE),
      col_texto("alias",        "tablas.ajustes.cuentas.alias"),
      col_texto("cuenta",       "tablas.ajustes.cuentas.cuenta", recorte = FALSE),
      col_texto("razon_social", "tablas.ajustes.cuentas.razon_social")
    ),
    acciones_fila = "editar",
    vacio = "tablas.ajustes.cuentas.vacio"
  )

  # ── + Banco ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_banco, {
    editing_banco_id(NULL); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(NULL, editar = .puede_bancos()) })
  }, ignoreInit = TRUE)

  # ── Select banco ─────────────────────────────────────────────────────────────
  # ✎ or a double click opens the bank by its id (it used to take the row's
  # position in a fresh read: with another session's change in between, it
  # opened another bank).
  observeEvent(t_bancos$accion(), {
    a <- t_bancos$accion()
    if (!a$accion %in% c("editar", "detalle")) return()
    bancos <- .leer_bancos()
    row <- bancos[!is.na(bancos$id) & bancos$id == a$ids[1], , drop = FALSE]
    if (!nrow(row)) return()
    editing_banco_id(row$id[1]); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(row[1, ], editar = .puede_bancos()) })
  })

  # ── Save banco ────────────────────────────────────────────────────────────────
  observeEvent(input$banco_save, {
    if (!puede_o_avisa(shared, "can_manage_bancos")) return()
    eid    <- editing_banco_id()
    nombre <- trimws(input$banco_nombre %||% "")
    clave  <- trimws(input$banco_clave  %||% "")
    if (!nzchar(nombre)) {
      showNotification(tr("tablas.ajustes.bancos.falta_nombre"), type = "warning"); return()
    }
    new_row <- tibble::tibble(
      id = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      nombre = nombre, clave = clave
    )
    # On S3's current catalog (CM12); a failed read or save is said and
    # nothing else changes.
    guardado <- tryCatch({
      modificar_ctas_bancos(function(bancos) {
        if (is.null(eid)) dplyr::bind_rows(bancos, new_row)
        else dplyr::rows_update(bancos, new_row, by = "id", unmatched = "ignore")
      }, client_id = shared$effective_client_id()); TRUE
    }, error = function(e) { message("[CTAS] banco save failed: ", e$message); FALSE })
    if (!guardado) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    showNotification(tr(if (is.null(eid)) "tablas.ajustes.bancos.agregado" else "tablas.ajustes.bancos.actualizado"),
                     type = "message", duration = 2)
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_cuentas",
      action      = if (is.null(eid)) "agregar_banco" else "editar_banco",
      description = paste0(if (is.null(eid)) "Banco agregado: " else "Banco editado: ", nombre),
      target_id   = new_row$id,
      metadata    = list(nombre = nombre, clave = clave),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── Delete banco ──────────────────────────────────────────────────────────────
  observeEvent(input$banco_delete, {
    if (!puede_o_avisa(shared, "can_manage_bancos")) return()
    eid <- editing_banco_id(); if (is.null(eid)) return()
    bancos  <- tryCatch(load_ctas_bancos(client_id = shared$effective_client_id()), error = function(e) .schema_ctas_bancos())
    deleted_nombre <- { m <- bancos$nombre[bancos$id == eid]; if (length(m)) m[1] else eid }
    # Removes only this bank, on S3's current catalog (CM12); deleting the
    # last one empties it on purpose. A bank some account uses is refused
    # (CM17) and the accounts are listed, copyable, so they can be changed.
    res <- tryCatch(borrar_banco_si_libre(eid, client_id = shared$effective_client_id()),
                    error = function(e) { message("[CTAS] banco delete failed: ", e$message); NULL })
    if (is.null(res)) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    if (!res$borrado) {
      filas <- .banco_en_uso_filas(res$en_uso, shared$company_map())
      showModal(.banco_en_uso_modal(deleted_nombre, filas)); return()
    }
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    showNotification(tr("tablas.ajustes.bancos.eliminado"), type = "message", duration = 2)
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_cuentas",
      action      = "eliminar_banco",
      description = paste0("Banco eliminado: ", deleted_nombre),
      target_id   = eid,
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── + Cuenta ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_cuenta, {
    # The bank open in the form (by id) is the new account's bank, as the
    # selected bank row used to be.
    pre_b  <- editing_banco_id()
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    bancos <- .leer_bancos()
    if (!is.null(pre_b) && !pre_b %in% bancos$id) pre_b <- NULL
    pre_e  <- isolate(input$cta_empresa_filter %||% "")
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(NULL, bancos, pre_b, pre_e, cmap = shared$company_map(), editar = .puede_bancos())
    })
  }, ignoreInit = TRUE)

  # ── Select cuenta ─────────────────────────────────────────────────────────────
  # ✎ or a double click opens the account by its id. It used to take the row's
  # position in a list re-read and re-filtered: a change in between opened
  # another account -- the one whose number goes into the PPL file as source.
  observeEvent(t_cuentas$accion(), {
    a <- t_cuentas$accion()
    if (!a$accion %in% c("editar", "detalle")) return()
    ctas <- .leer_cuentas()
    row  <- ctas[!is.na(ctas$id) & ctas$id == a$ids[1], , drop = FALSE]
    if (!nrow(row)) return()
    row <- row[1, ]
    ef  <- isolate(input$cta_empresa_filter %||% "")
    editing_cuenta_id(row$id); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(row, .leer_bancos(), row$banco_id, ef, cmap = shared$company_map(), editar = .puede_bancos())
    })
  })

  # ── Save cuenta ───────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_save, {
    if (!puede_o_avisa(shared, "can_manage_bancos")) return()
    shinyjs::disable("cuenta_save")
    on.exit(shinyjs::enable("cuenta_save"), add = TRUE)

    eid         <- editing_cuenta_id()
    empresa_val <- input$cta_empresa %||% ""
    cuenta_val  <- trimws(input$cta_cuenta %||% "")
    moneda_val  <- input$cta_moneda  %||% "MXN"

    if (!nzchar(empresa_val)) {
      showNotification(tr("tablas.ajustes.cuentas.falta_empresa"), type = "warning"); return()
    }
    if (!nzchar(cuenta_val)) {
      showNotification(tr("tablas.ajustes.cuentas.falta_cuenta"), type = "warning"); return()
    }

    is_default <- isTRUE(input$cta_is_ppl_default)

    new_row <- tibble::tibble(
      id                  = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      banco_id            = input$cta_banco_id %||% NA_character_,
      Empresa             = empresa_val,
      razon_social        = trimws(input$cta_razon_social %||% ""),
      rfc                 = trimws(input$cta_rfc          %||% ""),
      Moneda              = toupper(moneda_val),
      alias               = trimws(substr(input$cta_alias %||% "", 1, 15)),
      cuenta              = cuenta_val,
      clabe_interbancaria = trimws(input$cta_clabe        %||% ""),
      is_ppl_default      = is_default,
      saldo_inicial       = as.numeric(input$cta_saldo_inicial %||% 0),
      activa              = TRUE
    )

    # On S3's current accounts, never the session's copy (CM12): the PPL
    # default of the siblings is cleared there too. A failure is said.
    updated <- tryCatch(
      modificar_ctas_cuentas(function(ctas) {
        if (is_default) {
          # With a new account (eid NULL) `is.null(eid) | ctas$id != eid` was
          # logical(0): no sibling lost its default, and two stayed default.
          otra <- if (is.null(eid)) rep(TRUE, nrow(ctas)) else (!is.na(ctas$id) & ctas$id != eid)
          mask <- !is.na(ctas$Empresa) & ctas$Empresa == empresa_val &
                  !is.na(ctas$Moneda)  & toupper(ctas$Moneda) == toupper(moneda_val) & otra
          ctas$is_ppl_default[mask] <- FALSE
        }
        if (is.null(eid)) dplyr::bind_rows(ctas, new_row)
        else dplyr::bind_rows(ctas[is.na(ctas$id) | ctas$id != eid, , drop = FALSE], new_row)
      }, client_id = shared$effective_client_id()),
      error = function(e) { message("[CTAS] cuenta save failed: ", e$message); NULL })
    if (is.null(updated)) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    bump_sync_version("ctas_cuentas", client_id = shared$effective_client_id())
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)

    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    showNotification(
      tr(if (is.null(eid)) "tablas.ajustes.cuentas.agregada" else "tablas.ajustes.cuentas.actualizada"),
      type = "message", duration = 2)
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_cuentas",
      action      = if (is.null(eid)) "agregar_cuenta" else "editar_cuenta",
      description = paste0(if (is.null(eid)) "Cuenta agregada: " else "Cuenta editada: ",
                           empresa_val, " — ", cuenta_val),
      target_id   = new_row$id,
      metadata    = list(empresa = empresa_val, moneda = moneda_val, cuenta = cuenta_val),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── Delete cuenta ─────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_delete, {
    if (!puede_o_avisa(shared, "can_manage_bancos")) return()
    eid <- editing_cuenta_id(); if (is.null(eid)) return()
    # Deactivates only this account, on S3's current accounts (CM12).
    updated <- tryCatch(
      modificar_ctas_cuentas(function(ctas) { ctas$activa[!is.na(ctas$id) & ctas$id == eid] <- FALSE; ctas },
                             client_id = shared$effective_client_id()),
      error = function(e) { message("[CTAS] cuenta deactivate failed: ", e$message); NULL })
    if (is.null(updated)) { showNotification(tr("comun.error_guardar"), type = "error"); return() }
    deleted_row <- updated[!is.na(updated$id) & updated$id == eid, , drop = FALSE]
    bump_sync_version("ctas_cuentas", client_id = shared$effective_client_id())
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)
    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    showNotification(tr("tablas.ajustes.cuentas.desactivada"), type = "message", duration = 2)
    log_action(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = "settings_cuentas",
      action      = "desactivar_cuenta",
      description = paste0("Cuenta desactivada: ",
                           if (nrow(deleted_row)) paste0(deleted_row$Empresa[1], " — ", deleted_row$cuenta[1]) else eid),
      target_id   = eid,
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
  }, ignoreInit = TRUE)

  # ── Cancel ────────────────────────────────────────────────────────────────────
  observeEvent(input$cta_cancel, {
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
  }, ignoreInit = TRUE)
}

# ── Empty placeholder ─────────────────────────────────────────────────────────
.cuentas_form_empty <- function() {
  div(class = "text-muted small fst-italic",
    icon("hand-pointer"),
    " Selecciona un banco o cuenta para editar, o usa los botones + para agregar.")
}

# ── Bank form ─────────────────────────────────────────────────────────────────
.banco_form_ui <- function(row = NULL, editar = TRUE) {
  is_new  <- is.null(row)
  nom_val <- if (!is_new) row$nombre %||% "" else ""
  cla_val <- if (!is_new) row$clave  %||% "" else ""

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Agregar banco")
      else        tagList(icon("pencil"), " Editar banco")),

    div(class = "row g-2",
      div(class = "col-12 mb-1",
        tags$label("Selecci\u00f3n r\u00e1pida", class = "form-label small text-muted mb-1"),
        selectInput("banco_picker", NULL,
                    choices = .BANCOS_MX_CHOICES, selected = "", width = "270px")
      ),
      div(class = "col-md-7",
        tags$label("Nombre del banco", class = "form-label small fw-semibold mb-1"),
        textInput("banco_nombre", NULL, value = nom_val, width = "100%",
                  placeholder = "BanBaj\u00edo")
      ),
      div(class = "col-md-3",
        tags$label("Clave CECOBAN", class = "form-label small fw-semibold mb-1"),
        textInput("banco_clave", NULL, value = cla_val, width = "100%",
                  placeholder = "030")
      )
    ),

    # Autofill: picker value is "Nombre|clave"; JS splits and fills the two inputs
    tags$script(HTML("
      $(document).on('change', '#banco_picker', function() {
        var parts = $(this).val().split('|');
        if (parts.length === 2 && parts[0] !== '' && parts[0] !== 'Otro') {
          $('#banco_nombre').val(parts[0]);
          $('#banco_clave' ).val(parts[1]);
          Shiny.setInputValue('banco_nombre', parts[0], {priority:'event'});
          Shiny.setInputValue('banco_clave',  parts[1], {priority:'event'});
        }
      });
    ")),

    div(class = "d-flex gap-2 mt-3",
      if (editar)
        actionButton("banco_save",
          tagList(icon("floppy-disk"), if (is_new) " Agregar banco" else " Guardar"),
          class = "btn btn-primary btn-sm"),
      if (editar && !is_new)
        actionButton("banco_delete", tagList(icon("trash"), " Eliminar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# ── Bank in use (CM17) ────────────────────────────────────────────────────────
# Shown only by the banco_delete observer, after its can_manage_bancos check.
# "Copiar" copies the tab-separated list; if the browser refuses, the text
# shows selected so it can be copied by hand.
.banco_en_uso_modal <- function(banco, filas) {
  modalDialog(
    title = tr("comun.banco_en_uso.titulo", banco = banco),
    tags$p(tr_n("comun.banco_en_uso.explica", nrow(filas))),
    tags$ul(class = "small mb-2", lapply(seq_len(nrow(filas)), function(i)
      tags$li(tr("comun.banco_en_uso.linea", empresa = filas$empresa[i], moneda = filas$moneda[i],
                 cuenta = filas$cuenta[i], activa = filas$activa[i])))),
    tags$textarea(id = "banco_en_uso_texto", readonly = NA, rows = min(nrow(filas) + 1L, 8L),
                  class = "form-control form-control-sm font-monospace d-none",
                  .banco_en_uso_texto(filas)),
    footer = tagList(
      tags$button(type = "button", class = "btn btn-outline-primary btn-sm",
        onclick = paste0(
          "var b=this,t=document.getElementById('banco_en_uso_texto');",
          "function bien(){b.textContent=hdT('comun.copiado');}",
          "function mal(){t.classList.remove('d-none');t.select();}",
          "try{navigator.clipboard.writeText(t.value).then(bien,mal);}catch(e){mal();}"),
        icon("copy"), " ", tr("comun.copiar")),
      modalButton(tr("comun.cerrar"))
    ),
    easyClose = TRUE
  )
}

# ── Account form ──────────────────────────────────────────────────────────────
.cuenta_form_ui <- function(row = NULL, bancos = NULL, pre_banco_id = NULL,
                             pre_empresa = "", cmap, editar = TRUE) {
  is_new <- is.null(row)

  banco_choices <- if (!is.null(bancos) && nrow(bancos)) {
    setNames(bancos$id, bancos$nombre)
  } else { c("(agrega un banco primero)" = "") }

  sel_banco   <- if (!is_new) row$banco_id %||% "" else pre_banco_id %||% ""
  emp_val     <- if (!is_new) row$Empresa  %||% "" else pre_empresa
  razon_val   <- if (!is_new) row$razon_social %||% "" else ""
  rfc_val     <- if (!is_new) row$rfc  %||% "" else ""
  moneda_val  <- if (!is_new) row$Moneda   %||% "MXN" else "MXN"
  alias_val   <- if (!is_new) row$alias    %||% "" else ""
  cuenta_val  <- if (!is_new) row$cuenta   %||% "" else ""
  clabe_val   <- if (!is_new) row$clabe_interbancaria %||% "" else ""
  default_val <- if (!is_new) isTRUE(row$is_ppl_default) else TRUE  # default TRUE for convenience

  razon_ph <- if (nzchar(emp_val) && emp_val %in% names(cmap))
    unname(cmap[emp_val]) else "Empresa Modelo SA de CV"

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nueva cuenta bancaria")
      else        tagList(icon("pencil"), " Editar cuenta bancaria")),

    div(class = "row g-2",

      # Row 1: Empresa + Banco + Moneda + PPL checkbox
      div(class = "col-md-3",
        tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_empresa", NULL,
          choices  = c("\u2014 seleccionar \u2014" = "",
                       setNames(names(cmap), unname(cmap))),
          selected = emp_val, width = "100%")
      ),
      div(class = "col-md-4",
        tags$label("Banco", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_banco_id", NULL,
                    choices = banco_choices, selected = sel_banco, width = "100%")
      ),
      div(class = "col-md-2",
        tags$label("Moneda", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_moneda", NULL,
                    choices = c("MXN","USD","EUR"), selected = moneda_val, width = "100%")
      ),
      div(class = "col-md-3 d-flex align-items-end pb-1",
        div(class = "form-check",
          tags$input(
            type  = "checkbox", class = "form-check-input",
            id    = "cta_is_ppl_default",
            # An attribute, not a child: list(checked = "checked") as an
            # unnamed argument rendered the word "checked" next to an always
            # empty box, so saving any edit cleared the account's PPL flag.
            checked = if (default_val) "checked" else NULL
          ),
          tags$label(class = "form-check-label small fw-semibold",
                     `for` = "cta_is_ppl_default",
            tagList(tags$span(class = "badge bg-primary me-1", "PPL"),
                    "usar como origen")
          )
        )
      ),

      # Row 2: Cuenta origen + CLABE interbancaria + Alias
      div(class = "col-md-4",
        # No digit count: it isn't known for every bank and it may change.
        tags$label("Cuenta origen", class = "form-label small fw-semibold mb-1"),
        textInput("cta_cuenta", NULL, value = cuenta_val, width = "100%",
                  placeholder = "N\u00famero de cuenta")
      ),
      div(class = "col-md-5",
        tags$label(
          tagList("CLABE interbancaria",
            tags$span(class = "text-muted fw-normal", " \u2014 18 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cta_clabe", NULL, value = clabe_val, width = "100%",
                  placeholder = "030310012345678901")
      ),
      div(class = "col-md-3",
        tags$label("Alias interno", class = "form-label small fw-semibold mb-1"),
        textInput("cta_alias", NULL, value = alias_val, width = "100%",
                  placeholder = "EMP-MXN")
      ),

      # Row 3: Saldo inicial + Razón social + RFC
      div(class = "col-md-3",
        tags$label(
          tagList("Saldo inicial",
            tags$span(class = "text-muted fw-normal", " \u2014 apertura")),
          class = "form-label small fw-semibold mb-1"),
        numericInput("cta_saldo_inicial", NULL,
                     value = if (!is_new) (row$saldo_inicial %||% 0) else 0,
                     min = 0, step = 1000, width = "100%")
      ),
      div(class = "col-md-6",
        tags$label("Raz\u00f3n Social", class = "form-label small fw-semibold mb-1"),
        textInput("cta_razon_social", NULL, value = razon_val, width = "100%",
                  placeholder = razon_ph)
      ),
      div(class = "col-md-3",
        tags$label("RFC", class = "form-label small fw-semibold mb-1"),
        textInput("cta_rfc", NULL, value = rfc_val, width = "100%",
                  placeholder = "EMP850101HX1")
      )
    ),

    div(class = "small text-muted mt-2 border-top pt-2",
      icon("circle-info"),
      " La ", tags$strong("Cuenta origen"), " es el n\u00famero de cuenta que va en el ",
      "campo origen del archivo PPL. La CLABE interbancaria es el n\u00famero de 18 d\u00edgitos para recibir transferencias SPEI."
    ),

    div(class = "d-flex gap-2 mt-3",
      if (editar)
        actionButton("cuenta_save",
          tagList(icon("floppy-disk"), if (is_new) " Agregar cuenta" else " Guardar cambios"),
          class = "btn btn-primary btn-sm"),
      if (editar && !is_new)
        actionButton("cuenta_delete", tagList(icon("trash"), " Desactivar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}


