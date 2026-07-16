# =============================================================================
# R/forecasting_module.R
# Forecasting tab — three sub-tabs: Series viewer, Methods & Subscriptions, Fetch log.
# =============================================================================

# ── Capability constants ──────────────────────────────────────────────────────
.FC_CAP_REFRESH_METRIC  <- "forecasting.refresh_metric"
.FC_CAP_REFRESH_ALL     <- "forecasting.refresh_all"
.FC_CAP_EDIT_SUB        <- "forecasting.edit_subscription"

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
              DT::dataTableOutput(ns("methods_tbl"))
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
              DT::dataTableOutput(ns("subs_tbl"))
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
            shiny::p(class = "text-muted small mb-0",
                     "Últimas 200 consultas a fuentes de datos."),
            shiny::actionButton(ns("btn_refresh_all"),
                                shiny::tagList(shiny::icon("bolt"), " Actualizar todas las métricas"),
                                class = "btn btn-sm btn-outline-warning")
          ),
          DT::dataTableOutput(ns("fetch_log_tbl"))
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

    .observations <- shiny::reactive({
      tryCatch(
        shared$forecasting_series_observations_db() %||% load_forecasting_series_observations(client_id = shared$effective_client_id()),
        error = function(e) .schema_forecasting_series_observations()
      )
    })

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
      obs_new <- tryCatch(load_forecasting_series_observations(client_id = shared$effective_client_id()), error = function(e) NULL)
      if (!is.null(obs_new) && !is.null(shared$forecasting_series_observations_db)) {
        tryCatch(shared$forecasting_series_observations_db(obs_new), error = function(e) NULL)
      }
    }, ignoreInit = TRUE)

    # ── Refresh all button ────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_refresh_all, {
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
    }, ignoreInit = TRUE)

    # ── Methods table ─────────────────────────────────────────────────────────
    output$methods_tbl <- DT::renderDataTable({
      m <- tryCatch(load_forecasting_methods(), error = function(e) .schema_forecasting_methods())
      if (!nrow(m)) return(data.frame())
      data.frame(
        ID          = m$method_id,
        Nombre      = m$label,
        Tipo        = m$kind,
        Descripción = m$description,
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 10, dom = "t"), rownames = FALSE, selection = "none")

    # ── Subscriptions table ───────────────────────────────────────────────────
    output$subs_tbl <- DT::renderDataTable({
      s <- .subs()
      s <- s[!is.na(s$active) & s$active, , drop = FALSE]
      if (!nrow(s)) return(data.frame())

      m <- .metrics()
      lbl_map <- stats::setNames(m$label, m$metric_id)

      data.frame(
        ID           = s$subscription_id,
        Consumidor   = ifelse(!is.na(s$consumer_id) & nzchar(s$consumer_id %||% ""),
                              s$consumer_id, "—"),
        Tipo         = s$consumer_type,
        Métrica      = lbl_map[s$metric_id] %||% s$metric_id,
        Método       = s$method_id,
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 15, dom = "ftp"), rownames = FALSE,
    selection = list(mode = "single", target = "row"))

    # ── Subscription edit modal ───────────────────────────────────────────────
    shiny::observeEvent(input$subs_tbl_rows_selected, {
      sel <- input$subs_tbl_rows_selected
      if (!length(sel)) return()
      s   <- .subs()
      s   <- s[!is.na(s$active) & s$active, , drop = FALSE]
      if (sel > nrow(s)) return()
      row <- s[sel, ]

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
        shiny::uiOutput(ns("sub_curve_ui")),
        shiny::tags$input(type = "hidden", id = ns("sub_edit_id"),
                          value = row$subscription_id[1])
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
      sub_id    <- input$sub_edit_id %||% ""
      new_meth  <- input$sub_method_new %||% ""
      if (!nzchar(sub_id) || !nzchar(new_meth)) { shiny::removeModal(); return() }

      user <- tryCatch(shared$current_user(), error = function(e) "system")
      subs <- load_forecasting_subscriptions(client_id = shared$effective_client_id())
      idx  <- which(subs$subscription_id == sub_id)
      if (!length(idx)) { shiny::removeModal(); return() }
      subs$method_id[idx[1]]  <- new_meth
      subs$updated_at[idx[1]] <- Sys.time()
      save_forecasting_subscriptions(subs, client_id = shared$effective_client_id())
      tryCatch(shared$forecasting_subscriptions_db(subs), error = function(e) NULL)
      shiny::removeModal()
      shiny::showNotification("Suscripción actualizada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Add global subscription ───────────────────────────────────────────────
    shiny::observeEvent(input$btn_add_global_sub, {
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
      subs    <- load_forecasting_subscriptions(client_id = shared$effective_client_id())
      updated <- dplyr::bind_rows(subs, new_sub)
      save_forecasting_subscriptions(updated, client_id = shared$effective_client_id())
      tryCatch(shared$forecasting_subscriptions_db(updated), error = function(e) NULL)
      shiny::removeModal()
      shiny::showNotification("Suscripción global creada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Fetch log table ───────────────────────────────────────────────────────
    output$fetch_log_tbl <- DT::renderDataTable({
      log <- tryCatch(load_forecasting_fetch_log(), error = function(e) NULL)
      if (is.null(log) || !nrow(log)) return(data.frame())
      log <- log[order(log$attempted_at, decreasing = TRUE), , drop = FALSE]
      log <- utils::head(log, 200L)
      data.frame(
        Fecha      = format(log$attempted_at, "%d/%m/%Y %H:%M"),
        Métrica    = log$metric_id,
        Fuente     = log$source_id,
        Estado     = log$status,
        Filas      = log$rows_added,
        ms         = log$duration_ms,
        Error      = log$error_msg,
        Origen     = log$triggered_by,
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 20, dom = "ftp", order = list(list(0, "desc"))),
    rownames = FALSE, selection = "none")
  })
}
