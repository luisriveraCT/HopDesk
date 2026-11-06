# =============================================================================
# R/pasivos_list_module.R
# "Lista de pasivos" (lista_pasivos, class D) and "Papelera" (papelera_pasivos,
# class D): one row per liability, on hd_table (T18).
#
# Shiny module entry points:
#   pasivos_list_module_ui(id)     / pasivos_list_module_server(id, shared)
#   pasivos_papelera_ui(id)        / pasivos_papelera_server(id, shared)
#
# The Lista shows active and paused liabilities; the Papelera, the deleted
# ones ("Borrar" keeps all their provisions, hidden with them, and ↶ brings
# them back exactly). Every action acts on the row's id.
# =============================================================================

# ── UI ─────────────────────────────────────────────────────────────────────────
pasivos_list_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pasivos-list-module p-2", hd_table_ui(ns("lista")))
}

pasivos_papelera_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pasivos-papelera-module p-2", hd_table_ui(ns("papelera")))
}

# How many of a liability's payments are already in the Agenda: provisions
# converted into an item that was staged there (pagar_hoy_id). Deleting the
# liability leaves them as they are, and says so (Luis, T18).
pasivos_pagos_en_agenda <- function(provs, lid) {
  if (is.null(provs) || !nrow(provs) || !all(c("liability_id", "estado", "pagar_hoy_id") %in% names(provs)))
    return(0L)
  ph <- as.character(provs$pagar_hoy_id)
  sum(!is.na(provs$liability_id) & provs$liability_id == lid &
        provs$estado %in% "converted" & !is.na(ph) & nzchar(ph))
}

# ✎: the wizard listens to the global pasivos_edit_liability = id (the
# contract with the Pasivos table, T17b), and checks the permission there.
pasivos_pedir_editar <- function(lid)
  shinyjs::runjs(sprintf("Shiny.setInputValue('pasivos_edit_liability', %s, {priority: 'event'})",
                         jsonlite::toJSON(lid, auto_unbox = TRUE)))

# ── Rows ──────────────────────────────────────────────────────────────────────
# One row per liability in `estados`, visible to the user (`allowed`: NULL =
# every company, character(0) = none), sorted by empresa, categoria, nombre.
# Next provision: the liability's earliest provisional one from today on.
# `acciones(estado)` gives each row's .acciones (empty = no button).
pasivos_lista_filas <- function(liabs, provs, allowed = NULL, estados, acciones) {
  vacio <- data.frame(id = character(), nombre = character(), categoria = character(),
                      tipo = character(), subcategoria = character(), empresa = character(),
                      parte = character(), moneda = character(), moneda_pago = character(),
                      estado = character(), proxima_fecha = as.Date(character()),
                      proxima_monto = numeric(), borrado_el = as.POSIXct(character()),
                      borrado_por = character(), n_prov = integer(), .acciones = character(),
                      stringsAsFactors = FALSE)
  if (is.null(liabs) || !is.data.frame(liabs) || !nrow(liabs)) return(vacio)
  if (!is.null(allowed) && "empresa" %in% names(liabs))
    liabs <- liabs[liabs$empresa %in% allowed, , drop = FALSE]
  liabs <- liabs[liabs$estado %in% estados, , drop = FALSE]
  if (!nrow(liabs)) return(vacio)
  liabs <- liabs[order(liabs$empresa, liabs$categoria, liabs$nombre), , drop = FALSE]

  txt <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  n   <- nrow(liabs)
  if (is.null(provs) || !is.data.frame(provs)) provs <- .schema_pasivos_provision()
  de  <- function(lid) !is.na(provs$liability_id) & provs$liability_id == lid
  futura <- provs$estado %in% "provisional" & !is.na(provs$fecha_efectiva) &
            provs$fecha_efectiva >= Sys.Date()
  prox_f <- rep(as.Date(NA), n); prox_m <- rep(NA_real_, n); n_prov <- integer(n)
  for (i in seq_len(n)) {
    mio <- de(liabs$id[i])
    n_prov[i] <- sum(mio & provs$estado %in% "provisional")
    cand <- which(mio & futura)
    if (length(cand)) {
      j <- cand[which.min(provs$fecha_efectiva[cand])]
      prox_f[i] <- provs$fecha_efectiva[j]
      prox_m[i] <- tryCatch(pasivos_pago_amount(provs[j, , drop = FALSE]), error = function(e) NA_real_)
    }
  }
  flav <- txt(liabs$flavor)
  tipo <- ifelse(liabs$categoria %in% "financiero" & nzchar(flav),
                 vapply(flav, function(f) {
                   clave <- paste0("tablas.pasivos.flavor.", f)
                   if (is.null(i18n_dict("es")[[clave]])) f else tr(clave)
                 }, character(1), USE.NAMES = FALSE), "")
  mp <- txt(liabs$moneda_pago); ce <- txt(liabs$cotizado_en)
  moneda <- ifelse(nzchar(ce), vapply(seq_len(n), function(i)
    tr("tablas.pasivos.moneda_cotizada", moneda = mp[i], cotizado = ce[i]), character(1)), mp)
  borrado_el <- if (inherits(liabs$updated_at, "POSIXct")) liabs$updated_at else as.POSIXct(rep(NA, n))

  data.frame(
    id            = txt(liabs$id),
    nombre        = txt(liabs$nombre),
    categoria     = txt(liabs$categoria),
    tipo          = tipo,
    subcategoria  = txt(liabs$subcategoria),
    empresa       = txt(liabs$empresa),
    parte         = txt(liabs$parte),
    moneda        = moneda,
    moneda_pago   = mp,
    estado        = txt(liabs$estado),
    proxima_fecha = prox_f,
    proxima_monto = prox_m,
    borrado_el    = borrado_el,
    borrado_por   = txt(liabs$updated_by),
    n_prov        = n_prov,
    .acciones     = vapply(txt(liabs$estado), acciones, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

.pasivos_col_categoria <- function()
  col_insignia("categoria", "tablas.pasivos.categoria", mapa = list(
    regular    = list(texto = "tablas.pasivos.cat.regular",    tono = "neutro"),
    financiero = list(texto = "tablas.pasivos.cat.financiero", tono = "info"),
    tarjeta    = list(texto = "tablas.pasivos.cat.tarjeta",    tono = "neutro")))

# ── Lista: server ─────────────────────────────────────────────────────────────
pasivos_list_module_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    usuario <- function() tryCatch(shared$current_user(), error = function(e) "system")

    lista_datos <- shiny::reactive({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      provs <- tryCatch(shared$pasivos_provisions_db(),  error = function(e) NULL)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      user <- usuario()
      edita  <- isTRUE(has_capability(user, "pasivos.edit_liability"))
      borra  <- isTRUE(has_capability(user, "pasivos.delete_liability"))
      pasivos_lista_filas(liabs, provs, allowed, estados = c("active", "paused"),
        acciones = function(estado) paste(c(
          if (edita) "editar",
          if (edita) (if (identical(estado, "active")) "inactivar" else "reactivar"),
          if (borra) "eliminar"), collapse = ","))
    })

    t_lista <- hd_table_server(
      "lista", tabla = "lista_pasivos", datos = lista_datos,
      piezas = "columnas_opc",
      columnas = list(
        col_texto("nombre", "tablas.pasivos.nombre"),
        .pasivos_col_categoria(),
        col_texto("tipo", "tablas.pasivos.tipo"),
        col_texto("subcategoria", "tablas.pasivos.subcategoria", opcional = TRUE, visible = FALSE),
        col_texto("empresa", "tablas.pasivos.empresa", recorte = FALSE),
        col_texto("parte", "tablas.pasivos.parte"),
        col_texto("moneda", "tablas.pasivos.moneda", recorte = FALSE),
        col_insignia("estado", "tablas.pasivos.estado", mapa = list(
          active = list(texto = "tablas.pasivos.activo",  tono = "exito"),
          paused = list(texto = "tablas.pasivos.pausado", tono = "aviso"))),
        col_fecha("proxima_fecha", "tablas.pasivos.proxima"),
        col_dinero("proxima_monto", "tablas.pasivos.proximo_importe", moneda = "moneda_pago", sumar = FALSE)
      ),
      filtros = list(list(campo = "empresa"), list(campo = "categoria"),
                     list(campo = "subcategoria", titulo = "tablas.pasivos.subcategoria"),
                     list(campo = "moneda_pago", titulo = "tablas.pasivos.moneda"),
                     list(campo = "estado")),
      acciones_fila = c("editar", "inactivar", "reactivar", "eliminar"),
      # ✕ opens Borrar's own confirm (it says how many payments are in the
      # Agenda), so the engine's bar does not ask a second time (T12b).
      sin_confirmar = "eliminar",
      vacio = "tablas.pasivos.vacio_lista"
    )

    shiny::observeEvent(t_lista$accion(), {
      a   <- t_lista$accion()
      lid <- as.character(a$ids[1] %||% "")
      if (!nzchar(lid)) return()
      switch(a$accion,
        editar    = .editar(lid),
        inactivar = ,
        reactivar = .pedir_pausa(lid),
        eliminar  = .pedir_borrar(lid),
        NULL)
    })

    .editar <- function(lid) pasivos_pedir_editar(lid)

    # ── Pause / Resume ──────────────────────────────────────────────────────
    # Pausing asks first (Luis, T18): it removes the liability's future
    # provisions. Resuming does not ask, as before: it removes nothing.
    pause_target_rv <- shiny::reactiveVal("")
    .pedir_pausa <- function(lid) {
      user <- usuario()
      if (!has_capability(user, "pasivos.edit_liability")) {
        shiny::showNotification(tr("tablas.pasivos.sin_permiso"), type = "error"); return()
      }
      liabs <- tryCatch(shared$pasivos_liabilities_db(),
                        error = function(e) .schema_pasivos_liability())
      idx <- which(liabs$id == lid)
      if (!length(idx)) return()
      old_estado <- liabs$estado[idx[1]]
      if (identical(old_estado, "deleted")) return()  # deleted -- recover it from the Papelera
      if (!identical(old_estado, "active")) return(pasivos_cambiar_pausa(shared, lid, user, "active"))
      pause_target_rv(lid)
      shiny::showModal(shiny::modalDialog(
        tr("tablas.pasivos.pausar_pregunta"),
        footer = shiny::tagList(
          shiny::modalButton(tr("tablas.pasivos.cancelar")),
          shiny::actionButton(ns("plist_confirm_pause"), tr("tablas.pasivos.pausar"),
                              class = "btn btn-warning")
        ),
        size = "s"
      ))
    }

    shiny::observeEvent(input$plist_confirm_pause, ignoreInit = TRUE, {
      if (!pasivos_autoriza("pasivos.edit_liability")) return()
      lid <- pause_target_rv()
      shiny::removeModal()
      if (!nzchar(lid %||% "")) return()
      pasivos_cambiar_pausa(shared, lid, usuario(), "paused")
    })

    # ── Borrar ──────────────────────────────────────────────────────────────
    archive_target_rv <- shiny::reactiveVal("")
    .pedir_borrar <- function(lid) {
      if (!has_capability(usuario(), "pasivos.delete_liability")) {
        shiny::showNotification(tr("tablas.pasivos.sin_permiso"), type = "error"); return()
      }
      archive_target_rv(lid)
      provs <- tryCatch(shared$pasivos_provisions_db(),
                        error = function(e) .schema_pasivos_provision())
      n_agenda <- pasivos_pagos_en_agenda(provs, lid)
      shiny::showModal(shiny::modalDialog(
        shiny::p(tr("tablas.pasivos.borrar_pregunta")),
        if (n_agenda > 0L) shiny::p(class = "text-warning mb-0",
          if (n_agenda == 1L) tr("tablas.pasivos.borrar_agenda_uno")
          else tr("tablas.pasivos.borrar_agenda_otros", n = n_agenda)),
        footer = shiny::tagList(
          shiny::modalButton(tr("tablas.pasivos.cancelar")),
          shiny::actionButton(ns("plist_confirm_archive"), tr("tablas.pasivos.borrar"),
                              class = "btn btn-danger")
        ),
        size = "s"
      ))
    }

    shiny::observeEvent(input$plist_confirm_archive, ignoreInit = TRUE, {
      if (!pasivos_autoriza("pasivos.delete_liability")) return()
      lid  <- archive_target_rv()
      if (!nzchar(lid %||% "")) { shiny::removeModal(); return() }
      pasivos_borrar(shared, lid, usuario())
    })

    invisible(t_lista)
  })
}

# ── Papelera: server ──────────────────────────────────────────────────────────
# The deleted liabilities. ↶ (the engine's undo, which confirms in its bar)
# recovers one with all its provisions, as they were.
pasivos_papelera_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    usuario <- function() tryCatch(shared$current_user(), error = function(e) "system")

    papelera_datos <- shiny::reactive({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      provs <- tryCatch(shared$pasivos_provisions_db(),  error = function(e) NULL)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      recupera <- isTRUE(has_capability(usuario(), "pasivos.delete_liability"))
      pasivos_lista_filas(liabs, provs, allowed, estados = "deleted",
                          acciones = function(estado) if (recupera) "deshacer" else "")
    })

    t_pap <- hd_table_server(
      "papelera", tabla = "papelera_pasivos", datos = papelera_datos,
      columnas = list(
        col_texto("nombre", "tablas.pasivos.nombre"),
        .pasivos_col_categoria(),
        col_texto("tipo", "tablas.pasivos.tipo"),
        col_texto("empresa", "tablas.pasivos.empresa", recorte = FALSE),
        col_texto("parte", "tablas.pasivos.parte"),
        col_texto("moneda", "tablas.pasivos.moneda", recorte = FALSE),
        col_numero("n_prov", "tablas.pasivos.n_provisiones"),
        col_fecha_hora("borrado_el", "tablas.pasivos.borrado_el"),
        col_texto("borrado_por", "tablas.pasivos.borrado_por")
      ),
      filtros = list(list(campo = "empresa"), list(campo = "categoria")),
      acciones_fila = "deshacer",
      vacio = "tablas.pasivos.vacio_papelera"
    )

    shiny::observeEvent(t_pap$accion(), {
      a   <- t_pap$accion()
      lid <- as.character(a$ids[1] %||% "")
      if (!identical(a$accion, "deshacer") || !nzchar(lid)) return()
      # The engine's bar already asked; the permission is checked here, on
      # the server, whatever the browser sent.
      user <- usuario()
      if (!has_capability(user, "pasivos.delete_liability")) {
        shiny::showNotification(tr("tablas.pasivos.sin_permiso"), type = "error"); return()
      }
      pasivos_recuperar(shared, lid, user)
    })

    invisible(t_pap)
  })
}

# ── What the Lista's actions save, as functions of the liability id ──────────
# (T18: so the motor's row actions and the Papelera call the same code. The
# bodies are the handlers' own, unchanged.)

# One liability's estado, changed on S3 as it is now (T22). The decision
# ("is it still in an estado it can leave?") is taken on S3's row, not on the
# session's copy, and only that row is written. On a failed read or write it
# says so and changes nothing here: not the session, no bump, nothing after
# it (regla 17). It used to write the session's whole copy, swallow the error
# and go on as saved; a row another session had just changed went back to
# what this session held.
#   desde  the estados the row may leave (anything else: no change)
# Returns list(ok, cambio, antes = S3's row before, NULL if it isn't there).
.pasivos_cambiar_estado <- function(shared, lid, user, nuevo, desde) {
  cid   <- shared$effective_client_id()
  antes <- NULL
  liabs <- tryCatch(modificar_pasivos_liabilities(cid, function(fresco) {
    idx <- which(fresco$id %in% lid)
    if (!length(idx)) return(fresco)
    antes <<- fresco[idx[1], , drop = FALSE]
    if (!antes$estado[1] %in% desde || identical(antes$estado[1], nuevo)) return(fresco)
    fresco$estado[idx[1]]     <- nuevo
    fresco$updated_at[idx[1]] <- Sys.time()
    fresco$updated_by[idx[1]] <- user
    fresco
  }), error = function(e) e)
  if (inherits(liabs, "error")) {
    message("[pasivos] liability ", lid, " -> ", nuevo, " not saved: ", conditionMessage(liabs))
    shiny::showNotification(tr("tablas.pasivos.no_guardado"), type = "error", duration = 8)
    return(list(ok = FALSE, cambio = FALSE, antes = NULL))
  }
  # The session takes what S3 holds now, other sessions' changes included.
  shared$pasivos_liabilities_db(liabs)
  cambio <- !is.null(antes) && antes$estado[1] %in% desde && !identical(antes$estado[1], nuevo)
  if (cambio) bump_sync_version("pasivos_liabilities_db", client_id = cid)
  list(ok = TRUE, cambio = cambio, antes = antes)
}

# Sets an active liability to "paused" (dropping its future provisional
# provisions) or a paused one back to "active". Does nothing if, on S3, the
# liability is deleted or already in `new_estado` (e.g. another session
# changed it while the confirm was open).
pasivos_cambiar_pausa <- function(shared, lid, user, new_estado) {
      r <- .pasivos_cambiar_estado(shared, lid, user, new_estado,
                                   desde = if (identical(new_estado, "paused")) "active" else "paused")
      if (!isTRUE(r$cambio)) return(invisible(NULL))
      old_estado <- r$antes$estado[1]
      nombre     <- r$antes$nombre[1]

      if (identical(new_estado, "paused")) {
        # Remove future provisional provisions -- captured individually
        # BEFORE filtering (found 2026-09-05: these vanished from
        # pasivos_provisions_db entirely with only an uncounted mention in
        # the liability-level log below; an auditor could see a liability
        # was paused but not which provisions were dropped, when, or by
        # whom, since the row itself no longer exists to look up).
        provs <- tryCatch(shared$pasivos_provisions_db(),
                          error = function(e) .schema_pasivos_provision())
        provs_base <- provs   # CM09: only the dropped rows leave S3's table
        drop_mask <- !is.na(provs$liability_id) & provs$liability_id == lid &
                     !is.na(provs$estado) & provs$estado == "provisional" &
                     !is.na(provs$fecha_efectiva) & provs$fecha_efectiva >= Sys.Date()
        dropped_rows <- provs[drop_mask, , drop = FALSE]
        if (nrow(dropped_rows)) {
          provs <- provs[!drop_mask, , drop = FALSE]
          # Regla 17 (T22): this save's error was swallowed too, and the
          # provisions stayed on S3 while the session and the notice said
          # they were gone. The liability is paused already; say so.
          quitadas <- tryCatch({
            save_pasivos_provisions(provs, client_id = shared$effective_client_id(), base = provs_base)
            TRUE
          }, error = function(e) {
            message("[pasivos] future provisions of ", lid, " not removed: ", conditionMessage(e))
            FALSE
          })
          if (!quitadas) {
            shiny::showNotification(tr("tablas.pasivos.provisiones_no_quitadas"), type = "error", duration = 10)
          } else {
            tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
            shared$pasivos_provisions_db(provs)
            bump_sync_version("pasivos_provisions_db", client_id = shared$effective_client_id())
            for (i in seq_len(nrow(dropped_rows))) {
              tryCatch(pasivos_log_audit(
                action_type = "provision.deleted",
                user        = user,
                target_kind = "provision",
                target_id   = dropped_rows$id[i],
                before      = as.list(dropped_rows[i, , drop = FALSE]),
                notes       = sprintf("removed as future provisional provision when liability '%s' was paused", nombre),
                client_id   = shared$effective_client_id()
              ), error = function(e) NULL)
            }
          }
        }
        shiny::showNotification(tr("tablas.pasivos.pausado_ok"),
                                type = "message", duration = 3)
      } else {
        shiny::showNotification(
          tr("tablas.pasivos.reanudado_ok"),
          type = "message", duration = 5)
      }

      tryCatch(pasivos_log_audit(
        action_type = "liability.edited",
        user        = user,
        empresa     = r$antes$empresa[1],
        target_kind = "liability",
        target_id   = lid,
        before      = list(nombre = nombre, estado = old_estado),
        after       = list(nombre = nombre, estado = new_estado),
        notes       = sprintf("estado changed to %s", new_estado),
        client_id   = shared$effective_client_id()
      ), error = function(e) NULL)
      invisible(NULL)
}

# Marks a liability "deleted" ("Borrar"), keeping all its provisions (T18).
# The caller checked the permission and confirmed.
pasivos_borrar <- function(shared, lid, user) {
      r <- .pasivos_cambiar_estado(shared, lid, user, "deleted", desde = c("active", "paused"))
      shiny::removeModal()
      if (!isTRUE(r$cambio)) return()
      old_estado <- r$antes$estado[1]
      nombre     <- r$antes$nombre[1]

      # Its provisions are NOT removed (Luis, T18): a deleted liability leaves
      # every view together with all of them -- the Calendar, the Agenda and
      # the cash flow already skip every provisional provision of a "deleted"
      # liability (pasivos_provisions_as_ledger_rows, CM06) -- and recovering
      # it brings them back exactly, ids and hand edits included. Deleting
      # used to drop the future ones from S3, so a recovered liability came
      # back without them. Payments already in the Agenda (converted) stay.
      provs <- tryCatch(shared$pasivos_provisions_db(),
                        error = function(e) .schema_pasivos_provision())
      de_este <- !is.na(provs$liability_id) & provs$liability_id == lid
      n_prov   <- sum(de_este & provs$estado %in% "provisional")
      n_agenda <- pasivos_pagos_en_agenda(provs, lid)

      tryCatch(pasivos_log_audit(
        action_type = "liability.deleted",
        user        = user,
        empresa     = r$antes$empresa[1],
        target_kind = "liability",
        target_id   = lid,
        before      = list(nombre = nombre, estado = old_estado),
        after       = list(nombre = nombre, estado = "deleted"),
        notes       = sprintf("%d provisional provision(s) kept and hidden with the liability; %d payment(s) in the Agenda left as they are",
                              n_prov, n_agenda),
        client_id   = shared$effective_client_id()
      ), error = function(e) NULL)

      shiny::showNotification(tr("tablas.pasivos.borrado_ok"), type = "message", duration = 3)
}

# Brings a deleted liability back to "active" ("Recuperar"). The caller
# checked the permission. Only a liability that is deleted on S3 comes back.
pasivos_recuperar <- function(shared, lid, user) {
      r <- .pasivos_cambiar_estado(shared, lid, user, "active", desde = "deleted")
      if (!isTRUE(r$cambio)) return()   # not saved, or nothing to restore
      old_estado <- r$antes$estado[1]
      nombre     <- r$antes$nombre[1]

      tryCatch(pasivos_log_audit(
        action_type = "liability.restored",
        user        = user,
        empresa     = r$antes$empresa[1],
        target_kind = "liability",
        target_id   = lid,
        before      = list(nombre = nombre, estado = old_estado),
        after       = list(nombre = nombre, estado = "active"),
        notes       = "restored via Lista sub-tab",
        client_id   = shared$effective_client_id()
      ), error = function(e) NULL)

      # Its provisions were never removed (T18), so they are back as they were.
      shiny::showNotification(tr("tablas.pasivos.recuperado_ok"), type = "message", duration = 5)
}
