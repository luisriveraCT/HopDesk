# =============================================================================
# R/forecasting_module.R
# Forecasting tab — three sub-tabs: Series viewer, Methods & Subscriptions, Fetch log.
# =============================================================================

# Permissions (T26, R/permisos.R): can_refresh_forecasting (Actualizar),
# can_refresh_forecasting_all (Actualizar todas) and
# can_edit_forecasting_subscriptions (edit / + Global). Checked in each
# handler, on the server, with the key written out (test_permisos_vivos).

# Row ids for the tables (hd_table): the stored id, or one made up for a row
# written without it, and never repeated -- the engine refuses both, and one
# bad row must not take the whole table down. `clave`: what a made-up id is
# built from, so it stays the same when a refresh adds rows around it.
.fc_ids <- function(x, clave = seq_along(x)) {
  x <- as.character(x)
  vacio <- is.na(x) | !nzchar(x)
  x[vacio] <- paste0("sin-id-", gsub("[^A-Za-z0-9_.-]+", "_", as.character(clave[vacio])))
  make.unique(x, sep = "#")
}

# ── UI ────────────────────────────────────────────────────────────────────────

forecastingUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "forecasting-module p-3",
    shiny::tabsetPanel(
      id = ns("fc_tabs"), type = "tabs",

      # ── Tab 1: Series viewer ────────────────────────────────────────────────
      shiny::tabPanel(
        "Series",
        shiny::div(
          class = "mt-3",
          shiny::fluidRow(
            shiny::column(3,
              shiny::selectInput(ns("metric_sel"), "Métrica", choices = character(0))
            ),
            shiny::column(3,
              shiny::selectInput(ns("period_sel"), "Período",
                choices = c("30 días" = "30", "90 días" = "90",
                            "180 días" = "180", "1 año" = "365"),
                selected = "90")
            ),
            shiny::column(3,
              shiny::div(class = "mt-4",
                shiny::actionButton(ns("btn_refresh"), shiny::tagList(shiny::icon("rotate"), " Actualizar"),
                                    class = "btn btn-outline-primary btn-sm")
              )
            ),
            shiny::column(3,
              shiny::div(class = "mt-3",
                shiny::uiOutput(ns("fetch_status_badge"))
              )
            )
          ),
          shiny::plotOutput(ns("series_plot"), height = "320px"),
          shiny::hr(),
          shiny::uiOutput(ns("series_summary_card"))
        )
      ),

      # ── Tab 2: Methods & Subscriptions ────────────────────────────────────
      shiny::tabPanel(
        "Métodos y suscripciones",
        shiny::div(
          class = "mt-3",
          shiny::fluidRow(
            # Left: methods catalog
            shiny::column(5,
              shiny::h6("Métodos instalados", class = "text-muted text-uppercase small fw-bold"),
              hd_table_ui(ns("metodos_t"))
            ),
            # Right: subscriptions
            shiny::column(7,
              shiny::div(
                class = "d-flex justify-content-between align-items-center mb-2",
                shiny::h6("Suscripciones activas",
                          class = "text-muted text-uppercase small fw-bold mb-0"),
                shiny::actionButton(ns("btn_add_global_sub"),
                                    shiny::tagList(shiny::icon("plus"), " Suscripción global"),
                                    class = "btn btn-sm btn-outline-success")
              ),
              hd_table_ui(ns("subs_t"))
            )
          )
        )
      ),

      # ── Tab 3: Fetch log ──────────────────────────────────────────────────
      shiny::tabPanel(
        "Registro de consultas",
        shiny::div(
          class = "mt-3",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-3",
            shiny::p(class = "text-muted small mb-0", tr("tablas.pronosticos.consultas.ayuda")),
            shiny::actionButton(ns("btn_refresh_all"),
                                shiny::tagList(shiny::icon("bolt"), " Actualizar todas las métricas"),
                                class = "btn btn-sm btn-outline-warning")
          ),
          hd_table_ui(ns("consultas_t"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

forecastingServer <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive accessors ────────────────────────────────────────────────────
    .metrics <- shiny::reactive({
      tryCatch(
        shared$forecasting_metrics_db() %||% load_forecasting_metrics(client_id = shared$effective_client_id()),
        error = function(e) .schema_forecasting_metrics()
      )
    })

    .subs <- shiny::reactive({
      tryCatch(
        shared$forecasting_subscriptions_db() %||% load_forecasting_subscriptions(client_id = shared$effective_client_id()),
        error = function(e) .schema_forecasting_subscriptions()
      )
    })

    # Who may see the buttons that write (T26): without the permission they
    # are hidden; each handler checks again on the server (puede_o_avisa).
    .puede_fc <- function(k) shiny::reactive(isTRUE(tryCatch(puede(shared$usuario(), k), error = function(e) FALSE)))
    .puede_refrescar       <- .puede_fc("can_refresh_forecasting")
    .puede_refrescar_todas <- .puede_fc("can_refresh_forecasting_all")
    .puede_editar_subs     <- .puede_fc("can_edit_forecasting_subscriptions")
    shiny::observe({
      shinyjs::toggle("btn_refresh",        condition = .puede_refrescar())
      shinyjs::toggle("btn_refresh_all",    condition = .puede_refrescar_todas())
      shinyjs::toggle("btn_add_global_sub", condition = .puede_editar_subs())
    })

    .observations <- shiny::reactive({
      tryCatch(
        shared$forecasting_series_observations_db() %||% load_forecasting_series_observations(client_id = shared$effective_client_id()),
        error = function(e) .schema_forecasting_series_observations()
      )
    })

    # The fetch log is no synced table: a refresh here bumps this so the
    # status badge and the Registro de consultas read it again.
    .consultas_tick <- shiny::reactiveVal(0L)

    # ── Populate metric selector ──────────────────────────────────────────────
    shiny::observe({
      m  <- .metrics()
      if (!nrow(m)) return()
      ch <- stats::setNames(m$metric_id, m$label)
      shiny::updateSelectInput(session, "metric_sel", choices = ch)
    })

    # ── Fetch status badge ────────────────────────────────────────────────────
    output$fetch_status_badge <- shiny::renderUI({
      metric_id <- input$metric_sel %||% ""
      if (!nzchar(metric_id)) return(NULL)
      .consultas_tick()
      log <- tryCatch(load_forecasting_fetch_log(), error = function(e) NULL)
      if (is.null(log) || !nrow(log)) {
        return(shiny::tags$span(class = "badge bg-secondary", "Sin historial"))
      }
      m_log <- log[!is.na(log$metric_id) & log$metric_id == metric_id, , drop = FALSE]
      if (!nrow(m_log)) return(shiny::tags$span(class = "badge bg-secondary", "Sin historial"))
      last_ok <- m_log[!is.na(m_log$status) & m_log$status == "ok", , drop = FALSE]
      if (!nrow(last_ok)) {
        last <- m_log[order(m_log$attempted_at, decreasing = TRUE), , drop = FALSE][1, ]
        return(shiny::tags$span(class = "badge bg-danger",
                                paste0("Error: ", last$status)))
      }
      last_ok <- last_ok[order(last_ok$attempted_at, decreasing = TRUE), , drop = FALSE][1, ]
      shiny::tags$span(class = "badge bg-success",
                       paste0("OK — ", format(last_ok$attempted_at, "%d/%m/%Y %H:%M")))
    })

    # ── Series plot ───────────────────────────────────────────────────────────
    output$series_plot <- shiny::renderPlot({
      metric_id <- input$metric_sel %||% ""
      days      <- as.integer(input$period_sel %||% "90")
      if (!nzchar(metric_id)) return(NULL)

      obs  <- .observations()
      obs  <- obs[!is.na(obs$metric_id) & obs$metric_id == metric_id, , drop = FALSE]
      cutoff <- Sys.Date() - days
      obs  <- obs[!is.na(obs$fecha) & obs$fecha >= cutoff, , drop = FALSE]
      if (!nrow(obs)) {
        plot(1, type = "n", xlab = "", ylab = "", main = "Sin datos — utilice 'Actualizar'")
        return(invisible(NULL))
      }
      # Keep the canonical value per fecha
      obs <- obs[order(obs$fecha, obs$observation_type), , drop = FALSE]
      obs_daily <- obs[!duplicated(obs$fecha), , drop = FALSE]
      obs_daily <- obs_daily[order(obs_daily$fecha), , drop = FALSE]

      m_row <- .metrics()
      m_row <- m_row[m_row$metric_id == metric_id, , drop = FALSE]
      title <- if (nrow(m_row)) m_row$label[1] else metric_id

      plot(obs_daily$fecha, obs_daily$value,
           type = "l", col = "#0d6efd", lwd = 1.5,
           xlab = "", ylab = m_row$unit[1] %||% "",
           main = title)
      points(obs_daily$fecha, obs_daily$value, pch = 20, cex = 0.6, col = "#0d6efd")
    })

    # ── Series summary card ───────────────────────────────────────────────────
    output$series_summary_card <- shiny::renderUI({
      metric_id <- input$metric_sel %||% ""
      if (!nzchar(metric_id)) return(NULL)

      obs <- .observations()
      obs <- obs[!is.na(obs$metric_id) & obs$metric_id == metric_id, , drop = FALSE]
      obs <- obs[!is.na(obs$value), , drop = FALSE]
      days <- as.integer(input$period_sel %||% "90")
      recent <- obs[!is.na(obs$fecha) & obs$fecha >= Sys.Date() - days, , drop = FALSE]

      if (!nrow(obs)) {
        return(shiny::p(class = "text-muted small", "Sin observaciones almacenadas."))
      }
      latest <- obs[order(obs$fecha, decreasing = TRUE), , drop = FALSE][1, ]
      avg30  <- if (nrow(recent)) round(mean(recent$value, na.rm = TRUE), 4) else NA
      rng    <- if (nrow(recent)) range(recent$value, na.rm = TRUE) else c(NA, NA)

      shiny::div(
        class = "row g-2",
        shiny::column(3,
          shiny::div(class = "border rounded p-2 text-center",
            shiny::div(class = "text-muted small", "Último valor"),
            shiny::div(class = "fw-bold", format(latest$value, digits = 6)),
            shiny::div(class = "text-muted small", format(latest$fecha, "%d/%m/%Y"))
          )
        ),
        shiny::column(3,
          shiny::div(class = "border rounded p-2 text-center",
            shiny::div(class = "text-muted small", paste0("Promedio ", days, "d")),
            shiny::div(class = "fw-bold", if (!is.na(avg30)) format(avg30, digits = 6) else "—")
          )
        ),
        shiny::column(3,
          shiny::div(class = "border rounded p-2 text-center",
            shiny::div(class = "text-muted small", paste0("Rango ", days, "d")),
            shiny::div(class = "fw-bold small",
                       if (!is.na(rng[1]))
                         paste0(format(rng[1], digits = 5), " — ", format(rng[2], digits = 5))
                       else "—")
          )
        ),
        shiny::column(3,
          shiny::div(class = "border rounded p-2 text-center",
            shiny::div(class = "text-muted small", "Observaciones"),
            shiny::div(class = "fw-bold", nrow(obs))
          )
        )
      )
    })

    # ── Refresh button ────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_refresh, {
      if (!puede_o_avisa(shared, "can_refresh_forecasting")) return()
      metric_id <- input$metric_sel %||% ""
      if (!nzchar(metric_id)) return()
      user <- tryCatch(shared$current_user(), error = function(e) "system")

      shiny::showNotification("Actualizando datos…", id = "fc_refresh", duration = NULL,
                              type = "message")
      days <- as.integer(input$period_sel %||% "90")
      n_added <- tryCatch(
        fcs_fetch_and_store(metric_id,
                            date_from    = Sys.Date() - days,
                            date_to      = Sys.Date(),
                            triggered_by = "manual_ui",
                            user         = user),
        error = function(e) {
          shiny::showNotification(paste("Error:", e$message), type = "error")
          0L
        }
      )
      shiny::removeNotification("fc_refresh")
      shiny::showNotification(
        paste0(n_added, " observación(es) nuevas para ", metric_id, "."),
        type = "message", duration = 3
      )
      # Refresh in-memory cache so reactive updates
      fcs_flush_caches()
      .consultas_tick(.consultas_tick() + 1L)
      obs_new <- tryCatch(load_forecasting_series_observations(client_id = shared$effective_client_id()), error = function(e) NULL)
      if (!is.null(obs_new) && !is.null(shared$forecasting_series_observations_db)) {
        tryCatch(shared$forecasting_series_observations_db(obs_new), error = function(e) NULL)
      }
      log_action(
        user        = user,
        module      = "forecasting",
        action      = "refrescar_metrica",
        description = sprintf('Serie "%s" actualizada: %d observación(es) nueva(s)', metric_id, n_added),
        target_id   = metric_id,
        metadata    = list(metric_id = metric_id, n_added = n_added, days = days),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Refresh all button ────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_refresh_all, {
      if (!puede_o_avisa(shared, "can_refresh_forecasting_all")) return()
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      metrics <- .metrics()
      active_metrics <- metrics[!is.na(metrics$active) & metrics$active, , drop = FALSE]
      if (!nrow(active_metrics)) {
        shiny::showNotification("No hay métricas activas.", type = "warning"); return()
      }
      shiny::showNotification(
        paste0("Actualizando ", nrow(active_metrics), " métricas…"),
        id = "fc_refresh_all", duration = NULL, type = "message"
      )
      total <- 0L
      for (mid in active_metrics$metric_id) {
        total <- total + tryCatch(
          fcs_fetch_and_store(mid, date_from = Sys.Date() - 30L,
                              triggered_by = "manual_ui", user = user),
          error = function(e) 0L
        )
      }
      shiny::removeNotification("fc_refresh_all")
      shiny::showNotification(
        paste0(total, " observación(es) nuevas en ", nrow(active_metrics), " métricas."),
        type = "message", duration = 4
      )
      fcs_flush_caches()
      .consultas_tick(.consultas_tick() + 1L)
      log_action(
        user        = user,
        module      = "forecasting",
        action      = "refrescar_todas_metricas",
        description = sprintf("Actualización masiva de forecasting: %d observación(es) nueva(s) en %d métrica(s)",
                              total, nrow(active_metrics)),
        metadata    = list(n_added = total, n_metrics = nrow(active_metrics),
                           metric_ids = active_metrics$metric_id),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Methods table (hd_table, catalog) ─────────────────────────────────────
    # Static config, read once per session. Read-only: nothing to do with a
    # method, so no row actions; the bubble shows a long description whole.
    .metodos_tabla <- shiny::reactive({
      m <- tryCatch(load_forecasting_methods(), error = function(e) .schema_forecasting_methods())
      data.frame(
        id          = .fc_ids(m$method_id),
        method_id   = as.character(m$method_id),
        label       = as.character(m$label),
        kind        = as.character(m$kind),
        description = as.character(m$description),
        stringsAsFactors = FALSE
      )
    })
    hd_table_server(
      "metodos_t", tabla = "pronostico_metodos", datos = .metodos_tabla,
      columnas = list(
        col_texto("method_id",   "tablas.pronosticos.metodos.id", recorte = FALSE),
        col_texto("label",       "tablas.pronosticos.metodos.nombre"),
        col_texto("kind",        "tablas.pronosticos.metodos.tipo", recorte = FALSE),
        col_texto("description", "tablas.pronosticos.metodos.descripcion")
      ),
      vacio = "tablas.pronosticos.metodos.vacio"
    )

    # ── Subscriptions table (hd_table, catalog) ───────────────────────────────
    # ✎ or a double click opens the editor for that row's id. A click only
    # marks nothing: it used to open the editor at once (and a second click
    # on the same row did nothing, the selection hadn't changed).
    .subs_tabla <- shiny::reactive({
      s <- .subs()
      s <- s[!is.na(s$active) & s$active, , drop = FALSE]
      m <- .metrics()
      lbl_map <- stats::setNames(m$label, m$metric_id)
      metrica <- unname(lbl_map[s$metric_id])
      data.frame(
        id              = .fc_ids(s$subscription_id),
        subscription_id = as.character(s$subscription_id),
        consumidor      = ifelse(!is.na(s$consumer_id) & nzchar(s$consumer_id %||% ""),
                                 s$consumer_id, "—"),
        tipo            = as.character(s$consumer_type),
        metrica         = ifelse(is.na(metrica), s$metric_id, metrica),
        metodo          = as.character(s$method_id),
        .acciones       = rep(if (.puede_editar_subs()) "editar" else "", nrow(s)),
        stringsAsFactors = FALSE
      )
    })
    subs_t <- hd_table_server(
      "subs_t", tabla = "pronostico_subs", datos = .subs_tabla,
      columnas = list(
        col_texto("subscription_id", "tablas.pronosticos.subs.id"),
        col_texto("consumidor",      "tablas.pronosticos.subs.consumidor", recorte = FALSE),
        col_insignia("tipo",         "tablas.pronosticos.subs.tipo", mapa = list(
          global_default    = list(texto = "tablas.pronosticos.subs.global", tono = "info"),
          pasivos_liability = list(texto = "tablas.pronosticos.subs.pasivo", tono = "morado"))),
        col_texto("metrica",         "tablas.pronosticos.subs.metrica", recorte = FALSE),
        col_texto("metodo",          "tablas.pronosticos.subs.metodo", recorte = FALSE)
      ),
      acciones_fila = "editar",
      filtros       = list(list(campo = "tipo")),
      vacio         = "tablas.pronosticos.subs.vacio"
    )

    # `cambio(fresco)` on S3's subscriptions as they are now (T26): a failed
    # read or save writes nothing. Returns the table saved, or NULL. A failure
    # is never silent: the user is told, it is logged, and the open modal
    # stays so Guardar / Crear can be tried again.
    .guardar_subs <- function(cambio, accion, target_id, que) {
      ok <- tryCatch(modificar_forecasting_subscriptions(shared$effective_client_id(), cambio),
                     error = function(e) { message("[FC] ", accion, " failed: ", conditionMessage(e)); e })
      if (!inherits(ok, "error")) return(ok)
      shiny::showNotification(tr("comun.error_guardar"), type = "error", duration = 6)
      tryCatch(log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "forecasting",
        action      = paste0(accion, "_fallido"),
        description = paste0("Falló al guardar ", que, ": ", conditionMessage(ok)),
        target_id   = target_id,
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      ), error = function(e) NULL)
      NULL
    }

    # The subscription the editor is open on, by id -- what Guardar saves.
    # (It rode in a hidden <input>, which Shiny never binds: Guardar read
    # NULL and closed the editor without saving.)
    sub_editando <- shiny::reactiveVal(NULL)

    shiny::observeEvent(subs_t$accion(), {
      a <- subs_t$accion()
      if (!a$accion %in% c("editar", "detalle") || !length(a$ids)) return()
      # The editor it opens saves: no permission, no editor (sub_save checks
      # again). A double click (detalle) without it just does nothing.
      if (identical(a$accion, "detalle") && !shiny::isolate(.puede_editar_subs())) return()
      if (!puede_o_avisa(shared, "can_edit_forecasting_subscriptions")) return()
      d      <- .subs_tabla()
      sub_id <- d$subscription_id[match(a$ids[1], d$id)]
      if (is.na(sub_id) || !nzchar(sub_id)) return()
      s   <- .subs()
      s   <- s[!is.na(s$active) & s$active & !is.na(s$subscription_id) & s$subscription_id == sub_id, , drop = FALSE]
      if (!nrow(s)) return()
      row <- s[1, ]
      sub_editando(sub_id)

      methods <- tryCatch(load_forecasting_methods(), error = function(e) .schema_forecasting_methods())
      method_choices <- stats::setNames(methods$method_id, methods$label)

      m     <- .metrics()
      label <- (m$label[m$metric_id == row$metric_id[1]])[1] %||% row$metric_id[1]

      shiny::showModal(shiny::modalDialog(
        title     = "Editar suscripción",
        easyClose = TRUE,
        footer    = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("sub_save"), "Guardar", class = "btn btn-primary")
        ),
        shiny::p(shiny::strong("Consumidor: "), row$consumer_id[1] %||% "—",
                 shiny::strong(" ("), row$consumer_type[1], shiny::strong(")")),
        shiny::p(shiny::strong("Métrica: "), label),
        shiny::selectInput(ns("sub_method_new"), "Método nuevo:",
                           choices = method_choices,
                           selected = row$method_id[1]),
        shiny::uiOutput(ns("sub_curve_ui"))
      ))
    }, ignoreInit = TRUE)

    output$sub_curve_ui <- shiny::renderUI({
      if (!identical(input$sub_method_new, "manual_curve")) return(NULL)
      shiny::div(
        class = "mt-3 border rounded p-2",
        shiny::h6("Valores futuros (curva manual)", class = "small text-muted"),
        shiny::p(class = "text-muted small",
                 "Edición de curva manual disponible en la Etapa 6.2.")
      )
    })

    shiny::observeEvent(input$sub_save, {
      if (!puede_o_avisa(shared, "can_edit_forecasting_subscriptions")) return()
      sub_id    <- sub_editando() %||% ""
      new_meth  <- input$sub_method_new %||% ""
      if (!nzchar(sub_id) || !nzchar(new_meth)) { shiny::removeModal(); return() }

      user <- tryCatch(shared$current_user(), error = function(e) "system")
      old_meth <- NA_character_; metrica <- NA_character_
      # By id, on S3's subscriptions as they are now (T26).
      subs <- .guardar_subs(function(subs) {
        idx <- which(subs$subscription_id == sub_id)
        if (!length(idx)) return(subs)
        old_meth <<- subs$method_id[idx[1]]; metrica <<- subs$metric_id[idx[1]]
        subs$method_id[idx[1]]  <- new_meth
        subs$updated_at[idx[1]] <- Sys.time()
        subs
      }, "editar_suscripcion", sub_id, "el método de la suscripción")
      if (is.null(subs)) return()
      if (is.na(metrica)) { shiny::removeModal(); return() }   # no longer there
      tryCatch(shared$forecasting_subscriptions_db(subs), error = function(e) NULL)
      shiny::removeModal()
      shiny::showNotification("Suscripción actualizada.", type = "message", duration = 3)
      log_action(
        user        = user,
        module      = "forecasting",
        action      = "editar_suscripcion",
        description = sprintf('Suscripción de "%s" actualizada: método %s -> %s',
                              metrica, old_meth, new_meth),
        target_id   = sub_id,
        metadata    = list(subscription_id = sub_id, metric_id = metrica,
                           old_method = old_meth, new_method = new_meth),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Add global subscription ───────────────────────────────────────────────
    shiny::observeEvent(input$btn_add_global_sub, {
      if (!puede_o_avisa(shared, "can_edit_forecasting_subscriptions")) return()
      m     <- .metrics()
      subs  <- .subs()
      # Metrics that already have a global_default sub
      covered <- subs$metric_id[subs$consumer_type == "global_default" &
                                  !is.na(subs$active) & subs$active]
      uncovered <- m[!m$metric_id %in% covered, , drop = FALSE]
      if (!nrow(uncovered)) {
        shiny::showNotification(
          "Todas las métricas ya tienen suscripción global.", type = "message"); return()
      }
      methods <- tryCatch(load_forecasting_methods(), error = function(e) .schema_forecasting_methods())
      mc <- stats::setNames(methods$method_id, methods$label)
      mc_metrics <- stats::setNames(uncovered$metric_id, uncovered$label)

      shiny::showModal(shiny::modalDialog(
        title     = "Nueva suscripción global",
        easyClose = TRUE,
        footer    = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("global_sub_save"), "Crear",
                              class = "btn btn-success")
        ),
        shiny::selectInput(ns("global_sub_metric"), "Métrica:", choices = mc_metrics),
        shiny::selectInput(ns("global_sub_method"), "Método:",  choices = mc)
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$global_sub_save, {
      if (!puede_o_avisa(shared, "can_edit_forecasting_subscriptions")) return()
      metric_id <- input$global_sub_metric %||% ""
      method_id <- input$global_sub_method %||% ""
      if (!nzchar(metric_id) || !nzchar(method_id)) { shiny::removeModal(); return() }
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      now  <- Sys.time()
      new_sub <- tibble::tibble(
        subscription_id = uuid::UUIDgenerate(),
        consumer_type   = "global_default",
        consumer_id     = NA_character_,
        metric_id       = metric_id,
        method_id       = method_id,
        method_params   = list(list()),
        active          = TRUE,
        created_by      = user,
        created_at      = now,
        updated_at      = now,
        notes           = NA_character_
      )
      # Added to S3's subscriptions as they are now (T26): with the read
      # down it used to save only this one over all the others.
      updated <- .guardar_subs(function(subs) dplyr::bind_rows(subs, new_sub),
                               "crear_suscripcion_global", new_sub$subscription_id,
                               sprintf('la suscripción global de "%s"', metric_id))
      if (is.null(updated)) return()
      tryCatch(shared$forecasting_subscriptions_db(updated), error = function(e) NULL)
      shiny::removeModal()
      shiny::showNotification("Suscripción global creada.", type = "message", duration = 3)
      log_action(
        user        = user,
        module      = "forecasting",
        action      = "crear_suscripcion_global",
        description = sprintf('Suscripción global creada para "%s" (método: %s)', metric_id, method_id),
        target_id   = new_sub$subscription_id,
        metadata    = list(subscription_id = new_sub$subscription_id, metric_id = metric_id, method_id = method_id),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Fetch log table (hd_table, register) ──────────────────────────────────
    # Every fetch, newest first (it used to stop at the last 200; the engine
    # paints by window above 800 rows). Fecha sorts by the instant -- as
    # dd/mm text, 30/09 fell between 30/10 and 29/10. ℹ or a double click
    # shows the whole row, the error uncut.
    .consultas_tabla <- shiny::reactive({
      .consultas_tick()
      log <- tryCatch(load_forecasting_fetch_log(), error = function(e) .schema_forecasting_fetch_log())
      log <- log[order(log$attempted_at, decreasing = TRUE), , drop = FALSE]
      data.frame(
        id           = .fc_ids(log$fetch_id, paste(format(log$attempted_at, "%Y%m%d%H%M%OS3"), log$metric_id)),
        attempted_at = log$attempted_at,
        metric_id    = as.character(log$metric_id),
        source_id    = as.character(log$source_id),
        status       = as.character(log$status),
        rows_added   = log$rows_added,
        duration_ms  = log$duration_ms,
        error_msg    = as.character(log$error_msg),
        triggered_by = as.character(log$triggered_by),
        .acciones    = rep("detalle", nrow(log)),
        stringsAsFactors = FALSE
      )
    })
    .consultas_estados <- list(
      ok           = list(texto = "tablas.pronosticos.consultas.estado_ok",           tono = "exito"),
      no_data      = list(texto = "tablas.pronosticos.consultas.estado_no_data",      tono = "neutro"),
      rate_limited = list(texto = "tablas.pronosticos.consultas.estado_rate_limited", tono = "naranja"),
      timeout      = list(texto = "tablas.pronosticos.consultas.estado_timeout",      tono = "naranja"),
      http_error   = list(texto = "tablas.pronosticos.consultas.estado_http_error",   tono = "rojo"),
      parse_error  = list(texto = "tablas.pronosticos.consultas.estado_parse_error",  tono = "rojo"))
    .consultas_origenes <- list(
      scheduled = list(texto = "tablas.pronosticos.consultas.origen_scheduled", tono = "info"),
      manual_ui = list(texto = "tablas.pronosticos.consultas.origen_manual_ui", tono = "neutro"),
      first_use = list(texto = "tablas.pronosticos.consultas.origen_first_use", tono = "neutro"))
    consultas_t <- hd_table_server(
      "consultas_t", tabla = "bitacora_pronosticos", datos = .consultas_tabla,
      columnas = list(
        col_fecha_hora("attempted_at", "tablas.pronosticos.consultas.fecha"),
        col_texto("metric_id",         "tablas.pronosticos.consultas.metrica", recorte = FALSE),
        col_texto("source_id",         "tablas.pronosticos.consultas.fuente", recorte = FALSE),
        col_insignia("status",         "tablas.pronosticos.consultas.estado", mapa = .consultas_estados),
        col_numero("rows_added",       "tablas.pronosticos.consultas.filas"),
        col_numero("duration_ms",      "tablas.pronosticos.consultas.ms"),
        col_texto("error_msg",         "tablas.pronosticos.consultas.error"),
        col_insignia("triggered_by",   "tablas.pronosticos.consultas.origen", mapa = .consultas_origenes)
      ),
      piezas        = c("acc_fila", "recorte"),
      acciones_fila = "detalle",
      filtros       = list(list(campo = "status"), list(campo = "metric_id"), list(campo = "triggered_by")),
      vacio         = "tablas.pronosticos.consultas.vacio"
    )

    # The whole row, the error uncut.
    shiny::observeEvent(consultas_t$accion(), {
      a <- consultas_t$accion()
      if (!identical(a$accion, "detalle") || !length(a$ids)) return()
      d <- .consultas_tabla()
      r <- d[match(a$ids[1], d$id), , drop = FALSE]
      if (!nrow(r) || is.na(r$id)) return()
      etiqueta <- function(v, mapa) if (!is.na(v) && !is.null(mapa[[v]])) tr(mapa[[v]]$texto) else (v %||% "")
      dato <- function(clave, valor)
        shiny::tags$tr(shiny::tags$th(class = "text-nowrap pe-3", tr(clave)),
                       shiny::tags$td(style = "white-space: pre-wrap; word-break: break-word;",
                                      if (is.null(valor) || is.na(valor) || !nzchar(valor)) "—" else valor))
      shiny::showModal(shiny::modalDialog(
        title = tr("tablas.pronosticos.consultas.detalle_titulo"), easyClose = TRUE,
        footer = shiny::modalButton(tr("comun.cerrar")),
        shiny::tags$table(class = "table table-sm small mb-0", shiny::tags$tbody(
          dato("tablas.pronosticos.consultas.fecha",
               if (is.na(r$attempted_at)) NA else format(r$attempted_at, "%Y-%m-%d %H:%M:%S")),
          dato("tablas.pronosticos.consultas.metrica", r$metric_id),
          dato("tablas.pronosticos.consultas.fuente",  r$source_id),
          dato("tablas.pronosticos.consultas.estado",  etiqueta(r$status, .consultas_estados)),
          dato("tablas.pronosticos.consultas.filas",   as.character(r$rows_added)),
          dato("tablas.pronosticos.consultas.ms",      as.character(r$duration_ms)),
          dato("tablas.pronosticos.consultas.error",   r$error_msg),
          dato("tablas.pronosticos.consultas.origen",  etiqueta(r$triggered_by, .consultas_origenes))
        ))
      ))
    }, ignoreInit = TRUE)
  })
}
