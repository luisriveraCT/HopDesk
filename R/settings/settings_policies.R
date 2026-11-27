# =============================================================================
# R/settings/settings_policies.R
# Políticas de Pago UI helpers, calendar preview, revert modal, and settings_policies_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# POLÍTICAS DE PAGO — UI helpers
# =============================================================================

# Whether this session's user may manage payment policies (T16b): the
# buttons that write are drawn only then. settings_policies_observer() keeps
# the answer in session$userData; a UI drawn outside a session (a test)
# shows them. Every save checks can_manage_policies on the server anyway.
.puede_politicas <- function() {
  f <- tryCatch(shiny::getDefaultReactiveDomain()$userData$puede_politicas, error = function(e) NULL)
  if (is.function(f)) isTRUE(f()) else TRUE
}

.policy_type_badge <- function(type) {
  cfg <- switch(type,
    "weekdays"      = list(label = "hábiles", cls = "bg-primary"),
    "skip_holidays" = list(label = "festivos",  cls = "bg-warning text-dark"),
    "last_day"      = list(label = "fin mes",   cls = "bg-success"),
    "month_days"    = list(label = "días", cls = "bg-info text-dark"),
    "offset_days"   = list(label = "offset",    cls = "bg-secondary"),
    list(label = type, cls = "bg-secondary")
  )
  tags$span(class = paste("badge rounded-pill", cfg$cls), style = "font-size:0.7em;",
            cfg$label)
}

.policy_roll_label <- function(dir) {
  if (identical(dir, "backward")) "Retroceder" else "Adelantar"
}

# ── Main panel shell ──────────────────────────────────────────────────────────
.policies_merged_panel_ui <- function(active_tab = "reglas") {
  tab_btn <- function(id, label, tab) {
    active <- identical(active_tab, tab)
    tags$button(
      class   = paste("btn py-0 px-2",
                      if (active) "btn-primary" else "btn-outline-secondary"),
      style   = "font-size: 0.72rem;",
      onclick = sprintf(
        "Shiny.setInputValue('pol_active_tab','%s',{priority:'event'})", tab),
      label
    )
  }

  header_actions <- if (identical(active_tab, "reglas"))
    div(class = "ms-auto",
      if (.puede_politicas())
        actionButton("pol_btn_new", tagList(icon("plus"), " Nueva política"),
                     class = "btn btn-sm btn-outline-primary"))
  else
    div(class = "ms-auto d-flex align-items-center gap-2",
      uiOutput("cmp_revert_btn_ui"),
      uiOutput("cmp_apply_btn_ui"))

  header <- div(
    class = "d-flex align-items-center gap-2 mb-2",
    tags$span(class = "fw-semibold small",
              tagList(icon("calendar-check"), " Políticas de Pago")),
    tags$div(class = "btn-group btn-group-sm",
      tab_btn("pol_tab_reglas", "Reglas",  "reglas"),
      tab_btn("pol_tab_socios", "Socios",  "socios")
    ),
    header_actions
  )

  if (identical(active_tab, "reglas")) {
    div(
      header,
      div(id = "policies_list_section", uiOutput("policies_list_ui")),
      shinyjs::hidden(
        div(id = "policies_editor_section",
          tags$hr(class = "my-3"),
          uiOutput("policies_editor_ui"))
      ),
      shinyjs::hidden(
        div(id = "policies_assign_section",
          tags$hr(class = "my-3"),
          uiOutput("policies_assign_ui"))
      )
    )
  } else {
    div(
      header,
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1", "Socio comercial"),
        selectizeInput("cmp_partner_sel", NULL, choices = character(0),
          width = "100%",
          options = list(placeholder = "Buscar proveedor / cliente...", maxOptions = 300))
      ),
      div(id = "cmp_list_section", uiOutput("cmp_assigned_list_ui")),
      shinyjs::hidden(
        div(id = "cmp_editor_section",
          tags$hr(class = "my-2"),
          div(class = "d-flex align-items-center mb-2",
            div(
              tags$span(class = "small fw-semibold", "Pila de políticas"),
              uiOutput("cmp_editor_title_ui", inline = TRUE)
            ),
            div(class = "ms-auto",
              actionButton("cmp_btn_clear_partner",
                tagList(icon("xmark"), " Limpiar"),
                class = "btn btn-sm btn-outline-secondary py-0 px-2"))
          ),
          uiOutput("cmp_stack_ui"),
          div(class = "d-flex align-items-center gap-2 mt-2 mb-3",
            div(class = "flex-grow-1",
              selectizeInput("cmp_add_sel", NULL, choices = character(0),
                width = "100%",
                options = list(placeholder = "Buscar política...", maxOptions = 100))
            ),
            if (.puede_politicas())
              actionButton("cmp_btn_add", tagList(icon("plus"), " Agregar"),
                           class = "btn btn-sm btn-outline-primary")
          ),
          tags$hr(class = "my-2"),
          div(class = "row g-2 mb-3",
            div(class = "col-md-6",
              tags$label(class = "form-label small fw-semibold mb-1",
                         "Aplicar a cuentas por:"),
              radioButtons("cmp_ledger", NULL, inline = TRUE,
                choices = c("Pagar" = "AP", "Cobrar" = "AR", "Ambas" = ""),
                selected = "AP")
            ),
            div(class = "col-md-6",
              tags$label(class = "form-label small fw-semibold mb-1 d-block",
                         "¿Es intercompañía?"),
              uiOutput("cmp_interco_toggle_ui")
            )
          ),
          if (.puede_politicas())
            actionButton("cmp_btn_save", tagList(icon("floppy-disk"), " Guardar cambios"),
                         class = "btn btn-outline-secondary btn-sm")
        )
      )
    )
  }
}

.policies_panel_ui <- function() .policies_merged_panel_ui("reglas")
# ── Policy list ───────────────────────────────────────────────────────────────
.policies_list_render <- function(catalog_df) {
  if (is.null(catalog_df) || !nrow(catalog_df)) {
    return(div(class = "text-center text-muted py-5 small",
      icon("calendar-xmark", style = "font-size:1.8rem; opacity:0.25;"),
      tags$p(class = "mt-2 mb-0", "No hay políticas configuradas."),
      tags$p("Crea tu primera política con el botón",
             em("+ Nueva política"), ".")
    ))
  }

  editar <- .puede_politicas()   # without it: the list and ✎ (read-only form), no assign or delete
  rows <- lapply(seq_len(nrow(catalog_df)), function(i) {
    pol      <- catalog_df[i, ]
    pol_id   <- pol$id
    pol_name <- gsub("'", "\\'", pol$name, fixed = TRUE)
    div(class = "d-flex align-items-center gap-2 py-2 border-bottom",
      .policy_type_badge(pol$type),
      tags$span(class = "flex-grow-1 small", pol$name),
      tags$small(class = "text-muted me-1",
                 .policy_roll_label(pol$roll_direction)),
      if (editar) tags$button(
        class = "btn btn-sm btn-outline-success py-0 px-2",
        title = "Asignar a socios",
        onclick = sprintf(
          "Shiny.setInputValue('pol_action',{action:'assign',id:'%s',name:'%s',nonce:Math.random()},{priority:'event'})",
          pol_id, pol_name),
        icon("share-nodes")
      ),
      tags$button(
        class = "btn btn-sm btn-outline-secondary py-0 px-2",
        title = "Editar",
        onclick = sprintf(
          "Shiny.setInputValue('pol_action',{action:'edit',id:'%s',name:'%s',nonce:Math.random()},{priority:'event'})",
          pol_id, pol_name),
        icon("pen-to-square")
      ),
      if (editar) tags$button(
        class = "btn btn-sm btn-outline-danger py-0 px-2",
        title = "Eliminar",
        onclick = sprintf(
          "Shiny.setInputValue('pol_action',{action:'delete',id:'%s',name:'%s',nonce:Math.random()},{priority:'event'})",
          pol_id, pol_name),
        icon("trash")
      )
    )
  })

  div(tagList(rows))
}

# ── Delete confirm modal ──────────────────────────────────────────────────────
.policy_delete_confirm_modal <- function(pol_name, affected_partners = character()) {
  n <- length(affected_partners)
  body <- tagList(
    tags$p("¿Eliminar la política ", strong(pol_name), "?"),
    if (n > 0L)
      tagList(
        tags$p(class = "mb-1",
               sprintf("Afecta a %d socio%s:", n, if (n == 1L) "" else "s")),
        tags$ul(class = "mb-2", lapply(sort(affected_partners), tags$li))
      )
    else
      tags$p(class = "small text-muted mb-1",
             "Ningún socio tiene esta política asignada."),
    tags$p(class = "small text-muted mb-0",
           "La política se eliminará automáticamente de todos los socios asignados.")
  )
  modalDialog(
    title = tagList(icon("triangle-exclamation"), " Eliminar política"),
    body,
    footer = tagList(
      actionButton("pol_delete_cancel", "Cancelar",
                   class = "btn btn-secondary btn-sm"),
      actionButton("pol_delete_confirm",
                   tagList(icon("trash"), " Sí, eliminar"),
                   class = "btn btn-danger btn-sm")
    ),
    size = "s", easyClose = FALSE
  )
}

# ── Editor form (rendered once when editor opens) ────────────────────────────
# ns (T120): the Pasivos wizard draws this same editor with its own ids
# (pwiz_pol_*); Configuración keeps the ids it always had (ns = identity).
.policy_editor_form <- function(pol = NULL, is_new = TRUE, ns = identity) {
  current_name <- pol$name           %||% ""
  current_type <- pol$type           %||% "weekdays"
  current_roll <- pol$roll_direction %||% "forward"
  current_parm <- pol$params         %||% list()

  tagList(
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nueva política")
      else        tagList(icon("pen-to-square"), " Editar política")
    ),

    div(class = "row g-2 mb-2",
      div(class = "col-md-6",
        tags$label(class = "form-label small fw-semibold mb-1", "Nombre"),
        textInput(ns("pol_name"), NULL, value = current_name,
                  placeholder = "Ej: Fin de mes MXN", width = "100%")
      ),
      div(class = "col-md-6",
        tags$label(class = "form-label small fw-semibold mb-1", "Tipo"),
        selectInput(ns("pol_type"), NULL, width = "100%",
          choices = c(
            "Días hábiles (Lun–Vie)"   = "weekdays",
            "Excluir festivos"                         = "skip_holidays",
            "Último día del mes"        = "last_day",
            "Días fijos del mes"                  = "month_days",
            "Desplazamiento en días"              = "offset_days"
          ),
          selected = current_type
        )
      )
    ),

    uiOutput(ns("pol_params_ui")),

    div(id = ns("pol_roll_dir_section"),
      tags$label(class = "form-label small fw-semibold d-block mb-1",
                 "Cuando cae en día no válido"),
      radioButtons(ns("pol_roll_direction"), NULL, inline = TRUE,
        choices = c(
          "Adelantar (siguiente válido)"  = "forward",
          "Retroceder (válido anterior)"  = "backward"
        ),
        selected = current_roll
      )
    ),

    tags$hr(class = "my-2"),
    tags$label(class = "form-label small fw-semibold d-block mb-1",
               tagList(icon("eye"), " Vista previa")),
    uiOutput(ns("pol_preview_ui")),

    tags$hr(class = "my-3"),
    div(class = "d-flex gap-2",
      if (.puede_politicas())
        actionButton(ns("pol_btn_save"),
          tagList(icon("floppy-disk"), " Guardar"),
          class = "btn btn-primary btn-sm"),
      actionButton(ns("pol_btn_cancel"),
        tagList(icon("xmark"), " Cancelar"),
        class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# ── Dynamic params sub-form ───────────────────────────────────────────────────
.policy_params_ui <- function(type, params = list(), ns = identity) {
  switch(type,
    weekdays = {
      sel <- if (length(params$days)) as.character(as.integer(params$days)) else as.character(1:5)
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1", "Días permitidos"),
        checkboxGroupInput(ns("pol_param_days"), NULL, inline = TRUE,
          choiceValues = as.character(1:7),
          choiceNames  = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"),
          selected = sel
        )
      )
    },
    skip_holidays = {
      country <- params$country %||% "MX"
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1",
                   "Calendario de festivos"),
        selectInput(ns("pol_param_country"), NULL, width = "260px",
          choices = c(
            "México (LFT + CNBV)"        = "MX",
            "Estados Unidos (Federal)"         = "US",
            "Francia"                          = "FR"
          ),
          selected = toupper(country)
        )
      )
    },
    last_day = {
      div(class = "alert alert-light small py-2 mb-2",
        icon("circle-info"), " ",
        "Mueve la fecha al último día calendario del mes. ",
        "Sin parámetros adicionales."
      )
    },
    month_days = {
      sel_days <- if (length(params$days)) {
        as.character(sort(as.integer(params$days)))
      } else {
        c("15", "30")
      }
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1",
                   "Días del mes para pago"),
        tags$p(class = "text-muted small mb-1",
               "Si el mes no tiene ese día (ej. día 31 en febrero), ",
               "se usa el último día hábil del mes."),
        div(style = "max-width: 380px;",
          checkboxGroupInput(ns("pol_param_month_days"), NULL, inline = TRUE,
            choiceValues = as.character(1:31),
            choiceNames  = as.character(1:31),
            selected = sel_days
          )
        )
      )
    },
    offset_days = {
      n_val    <- as.integer(params$n %||% 5L)
      unit_val <- params$unit %||% "naturales"
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1",
                   "Días a desplazar"),
        div(class = "d-flex align-items-center gap-2 flex-wrap",
          hd_numero_input(ns("pol_param_n"), NULL, formato = "entero",
                          value = n_val, min = -365L, max = 365L, step = 1L,
                          width = "110px"),
          selectInput(ns("pol_param_unit"), NULL,
            choices  = c("naturales" = "naturales", "hábiles" = "habiles"),
            selected = unit_val,
            width    = "120px"),
          tags$span(class = "small text-muted",
                    "(positivo = adelantar, negativo = retrasar)")
        )
      )
    },
    div()  # fallback
  )
}

# ── Collect params from form inputs ──────────────────────────────────────────
.collect_policy_params <- function(type, input, ns = identity) {
  switch(type,
    weekdays      = list(days = sort(as.integer(input[[ns("pol_param_days")]] %||% as.character(1:5)))),
    skip_holidays = list(country = toupper(trimws(input[[ns("pol_param_country")]] %||% "MX"))),
    last_day      = list(),
    month_days    = list(days = sort(as.integer(input[[ns("pol_param_month_days")]] %||% c("15","30")))),
    offset_days   = list(n    = as.integer(input[[ns("pol_param_n")]]    %||% 0L),
                         unit = input[[ns("pol_param_unit")]] %||% "naturales"),
    list()
  )
}

# =============================================================================
# Interactive dual-month calendar preview
# =============================================================================
.policy_cal_preview_render <- function(type, params, roll, hcache, offset, selected, ns = identity) {

  today   <- Sys.Date()
  m1_date <- lubridate::floor_date(today, "month") + months(as.integer(offset))
  m2_date <- m1_date + months(1L)

  # Month names from the dictionary (comun.mes.N), with a capital in the bar.
  meses <- vapply(1:12, function(m) {
    x <- tr(paste0("comun.mes.", m)); paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
  }, character(1))

  m1_lbl <- sprintf("%s %04d", meses[lubridate::month(m1_date)], lubridate::year(m1_date))
  m2_lbl <- sprintf("%s %04d", meses[lubridate::month(m2_date)], lubridate::year(m2_date))

  # Compute projected dates for every selected input date
  pol_obj   <- list(type = type, params = params, roll_direction = roll)
  projected <- if (length(selected)) {
    vapply(selected, function(ds) {
      tryCatch(
        as.character(compose_policies(as.Date(ds), list(pol_obj), hcache)),
        error = function(e) ds
      )
    }, character(1L))
  } else character(0L)

  # ── Build one month grid ──────────────────────────────────────────────────
  # Each month is rendered into a fixed-width container; both header and dot
  # grids use identical `repeat(7, 1fr)` columns + `justify-items:center` so
  # every column is identical regardless of dot vs letter size, and the layout
  # scales cleanly across different screen widths.
  DOT       <- "10px"
  MONTH_W   <- "112px"   # 7 cols × ~14px + a little breathing room
  GRID_STY  <- paste0(
    "display:grid;grid-template-columns:repeat(7,1fr);",
    "justify-items:center;align-items:center;row-gap:3px;width:", MONTH_W, ";"
  )

  .cal_month <- function(yr, mo, sel, proj, is_input) {
    first  <- as.Date(sprintf("%04d-%02d-01", as.integer(yr), as.integer(mo)))
    n_days <- as.integer(lubridate::days_in_month(first))
    # lubridate default wday: Sun = 1, Mon = 2, …, Sat = 7
    lead   <- lubridate::wday(first) - 1L
    lbl    <- sprintf("%s %04d", meses[as.integer(mo)], as.integer(yr))

    # Sunday first; the app's day initials (tablas.flujo.dia.N, 7 = Sunday).
    hdr <- lapply(vapply(c(7L, 1:6), function(n) tr(paste0("tablas.flujo.dia.", n)), character(1)), function(h)
      tags$span(h, style =
        "font-size:8px;color:var(--hd-leg-texto-4b5563);font-weight:600;line-height:1;"
      )
    )

    total  <- lead + n_days
    n_rows <- ceiling(total / 7L)

    dots <- lapply(seq_len(n_rows * 7L), function(i) {
      day_n <- i - lead
      if (day_n < 1L || day_n > n_days) {
        tags$span(style = paste0(
          "width:", DOT, ";height:", DOT, ";border-radius:50%;visibility:hidden;"
        ))
      } else {
        ds <- sprintf("%04d-%02d-%02d", as.integer(yr), as.integer(mo), day_n)
        is_sel  <- ds %in% sel
        is_proj <- ds %in% proj
        glow <- if (is_input && is_sel)
          "background:var(--hd-leg-acento-7c3aed);box-shadow:var(--hd-leg-sombra-7c3aed-bcc0b2);"
        else if (!is_input && is_proj)
          "background:var(--hd-leg-primario-2563eb);box-shadow:var(--hd-leg-sombra-2563eb-091a02);"
        else
          "background:var(--hd-leg-borde-fuerte-cbd5e1);"
        sty <- paste0(
          "width:", DOT, ";height:", DOT, ";border-radius:50%;",
          "display:block;",
          "transition:box-shadow .15s,background .15s;", glow
        )
        if (is_input)
          tags$span(
            style   = paste0(sty, "cursor:pointer;"),
            title   = ds,
            onclick = sprintf(
              "Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pol_cal_click"), ds
            )
          )
        else
          tags$span(style = sty, title = ds)
      }
    })

    div(style = paste0("width:", MONTH_W, ";"),
      div(style = paste0("font-size:9px;color:var(--hd-leg-texto-1e293b);font-weight:700;",
                         "text-align:center;margin-bottom:5px;"), lbl),
      div(style = GRID_STY, tagList(hdr)),
      div(style = GRID_STY, tagList(dots))
    )
  }

  m1y <- lubridate::year(m1_date);  m1m <- lubridate::month(m1_date)
  m2y <- lubridate::year(m2_date);  m2m <- lubridate::month(m2_date)

  in_m1  <- .cal_month(m1y, m1m, selected,  projected, TRUE)
  in_m2  <- .cal_month(m2y, m2m, selected,  projected, TRUE)
  out_m1 <- .cal_month(m1y, m1m, selected,  projected, FALSE)
  out_m2 <- .cal_month(m2y, m2m, selected,  projected, FALSE)

  btn_sty <- paste0(
    "background:none;border:1px solid var(--hd-leg-borde-fuerte-d1d5db);border-radius:5px;",
    "padding:3px 10px;font-size:15px;font-weight:bold;cursor:pointer;",
    "color:var(--hd-leg-texto-1e293b);line-height:1.3;"
  )
  pair_base   <- "display:flex;gap:14px;padding:10px 14px;border-radius:10px;"
  input_pair  <- paste0(pair_base, "background:color-mix(in srgb, var(--hd-leg-acento-7c3aed) 7%, transparent);")
  output_pair <- paste0(pair_base, "background:color-mix(in srgb, var(--hd-leg-primario-2563eb) 7%, transparent);")
  lbl_base <- paste0(
    "font-size:8px;font-weight:700;text-transform:uppercase;",
    "letter-spacing:.08em;margin-bottom:6px;text-align:center;"
  )
  arrow_sty <- paste0(
    "display:flex;align-items:center;justify-content:center;",
    "padding:0 4px;font-size:28px;font-weight:300;",
    "color:var(--hd-leg-texto-tenue-94a3b8);line-height:1;align-self:center;"
  )

  # ── Holiday list (skip_holidays only) ────────────────────────────────────
  holiday_panel <- if (type == "skip_holidays" && length(hcache) > 0L) {
    hols     <- sort(unique(as.Date(unlist(hcache, use.names = FALSE))))
    hols     <- hols[!is.na(hols)]
    cur_yr   <- as.integer(format(Sys.Date(), "%Y"))

    # Index of the first current-year holiday (for auto-scroll)
    first_cur_idx <- which(lubridate::year(hols) == cur_yr)[1L]
    # Pixel offset per row (~17px) — scroll target
    scroll_top <- if (!is.na(first_cur_idx) && first_cur_idx > 1L)
      (first_cur_idx - 1L) * 17L else 0L

    rows <- lapply(seq_along(hols), function(i) {
      d      <- hols[[i]]
      yr     <- lubridate::year(d)
      is_cur <- yr == cur_yr
      lbl    <- tr("tablas.ajustes.politicas.festivo_fecha", dia = as.character(lubridate::day(d)),
                   mes = tr(paste0("comun.mes.", lubridate::month(d))), anio = sprintf("%04d", yr))
      row_sty <- paste0(
        "font-size:9px;padding:2px 0;white-space:nowrap;",
        if (is_cur) "color:var(--hd-leg-texto-1e293b);font-weight:700;" else "color:var(--hd-texto-2);"
      )
      div(style = row_sty,
          HTML(paste0(
            "<span style='margin-right:4px;opacity:.5;'>&#10005;</span>", lbl
          )))
    })

    list_id <- ns("pol_hol_list")
    scroll_js <- if (scroll_top > 0L)
      sprintf("setTimeout(function(){var el=document.getElementById('%s');if(el)el.scrollTop=%d;},80);",
              list_id, scroll_top)
    else ""

    tagList(
      div(id    = list_id,
          style = paste0(
            "display:flex;flex-direction:column;align-self:stretch;",
            "padding:10px 12px;border-radius:10px;",
            "background:color-mix(in srgb, var(--hd-leg-primario-2563eb) 5%, transparent);",
            "max-height:160px;overflow-y:auto;min-width:130px;"),
        div(style = paste0(lbl_base, "color:var(--hd-leg-primario-1d4ed8);margin-bottom:6px;"),
            tr("tablas.ajustes.politicas.festivos")),
        tagList(rows)
      ),
      if (nzchar(scroll_js)) tags$script(HTML(scroll_js)) else NULL
    )
  } else NULL

  div(
    # ── Navigation bar: hint left · nav centred ───────────────────────────
    div(style = paste0(
          "display:grid;grid-template-columns:1fr auto 1fr;",
          "align-items:center;margin-bottom:8px;"),
      tags$span(
        style = "font-size:9px;color:var(--hd-leg-texto-tenue-94a3b8);font-style:italic;",
        tr("tablas.ajustes.politicas.simular_pista")
      ),
      div(style = "display:flex;align-items:center;gap:10px;",
        tags$button(style = btn_sty,
          onclick = paste0("Shiny.setInputValue('", ns("pol_cal_nav"), "',",
                           "{dir:-1,nonce:Math.random()},{priority:'event'})"),
          HTML("&#8249;")
        ),
        tags$span(
          style = "font-size:11px;color:var(--hd-leg-texto-1e293b);font-weight:600;min-width:150px;text-align:center;",
          sprintf("%s → %s", m1_lbl, m2_lbl)
        ),
        tags$button(style = btn_sty,
          onclick = paste0("Shiny.setInputValue('", ns("pol_cal_nav"), "',",
                           "{dir:1,nonce:Math.random()},{priority:'event'})"),
          HTML("&#8250;")
        )
      ),
      div()  # right spacer keeps nav centred
    ),
    # ── Calendar pairs row ────────────────────────────────────────────────
    div(style = "display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
      # Input pair
      div(style = input_pair,
        div(style = "display:flex;flex-direction:column;align-items:center;",
          tags$span(style = paste0(lbl_base, "color:var(--hd-leg-acento-texto-6d28d9);"), tr("tablas.ajustes.politicas.seleccion")),
          div(style = "display:flex;gap:14px;", in_m1, in_m2)
        )
      ),
      # Arrow separator
      div(style = arrow_sty, HTML("&#10230;")),
      # Output pair
      div(style = output_pair,
        div(style = "display:flex;flex-direction:column;align-items:center;",
          tags$span(style = paste0(lbl_base, "color:var(--hd-leg-primario-1d4ed8);"), tr("tablas.ajustes.politicas.proyeccion")),
          div(style = "display:flex;gap:14px;", out_m1, out_m2)
        )
      ),
      # Holiday list (only for skip_holidays)
      if (!is.null(holiday_panel)) holiday_panel else NULL
    )
  )
}



# ── Revert policies modal ──────────────────────────────────────────────────────
.cmp_revert_modal <- function(affected_partes) {
  n <- length(affected_partes)
  modalDialog(
    title = tagList(icon("rotate-left"), " Revertir fechas de política"),
    tagList(
      tags$p("Elimina las fechas calculadas por política. Las facturas volverán a mostrar su fecha SAP original."),
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1",
                   sprintf("Socios con fechas calculadas (%d):", n)),
        selectizeInput("cmp_revert_partes_sel", NULL,
          choices  = sort(affected_partes),
          selected = sort(affected_partes),
          multiple = TRUE,
          width    = "100%",
          options  = list(placeholder = "Seleccionar socios...", maxOptions = 500))
      ),
      tags$p(class = "small text-muted mb-0",
             "Solo se eliminan las fechas de política. Las asignaciones y las fechas manuales no se modifican.")
    ),
    footer = tagList(
      actionButton("cmp_revert_cancel2", "Cancelar", class = "btn btn-secondary btn-sm"),
      actionButton("cmp_revert_confirm",
                   tagList(icon("rotate-left"), " Revertir seleccionados"),
                   class = "btn btn-warning btn-sm")
    ),
    size = "m", easyClose = FALSE
  )
}
# ── Assign-to-partners form ───────────────────────────────────────────────────
.policies_assign_form <- function(pol_id, pol_name, pol_type,
                                   preselected, all_partes) {
  tagList(
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
        tagList(icon("share-nodes"), " Asignar política")),
      div(class = "ms-2",
        .policy_type_badge(pol_type),
        tags$span(class = "ms-1 small", pol_name)
      )
    ),

    if (!length(all_partes)) {
      div(class = "alert alert-warning small py-2",
          icon("triangle-exclamation"), " Sin socios disponibles. ",
          "Sincroniza SAP o configura socios primero.")
    } else {
      tagList(
        tags$label(class = "form-label small fw-semibold mb-1",
          sprintf("Socios seleccionados (%d / %d)",
                  length(preselected), length(all_partes))),
        selectizeInput("pol_assign_partners", NULL,
          choices  = setNames(all_partes, all_partes),
          selected = preselected,
          multiple = TRUE,
          width    = "100%",
          options  = list(
            placeholder  = "Buscar y seleccionar socios...",
            maxOptions   = 400,
            plugins      = list("remove_button")
          )
        )
      )
    },

    div(class = "row g-2 mb-3 mt-1",
      div(class = "col-md-6",
        tags$label(class = "form-label small fw-semibold mb-1",
                   "Aplicar a cuentas por:"),
        radioButtons("pol_assign_ledger", NULL, inline = TRUE,
          choices  = c("Pagar" = "AP", "Cobrar" = "AR", "Ambas" = ""),
          selected = "AP"
        )
      )
    ),

    tags$p(class = "small text-muted mb-3",
      icon("circle-info", class = "me-1"),
      "Los socios ",
      strong("marcados"), " tendrán esta política en su pila.",
      " Los que ya la tienen conservan su configuración actual.",
      " Los que se ",
      strong("demarquen"), " la perderán (otras políticas intactas)."
    ),

    div(class = "d-flex gap-2",
      if (.puede_politicas())
        actionButton("pol_assign_save",
          tagList(icon("floppy-disk"), " Guardar asignaciones"),
          class = "btn btn-primary btn-sm"),
      actionButton("pol_assign_cancel",
        tagList(icon("xmark"), " Cancelar"),
        class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# =============================================================================
# settings_policies_observer
# =============================================================================
# ── The editor's server side, for Configuración and the Pasivos wizard (T120) ──
# Its ids are ns("pol_*"): identity in Configuración (the ids it always had),
# "pwiz_pol_*" in the wizard, which draws the same editor inside its Revisión
# step (a modal cannot open another modal: it would close the wizard).
# al_guardar(res) runs after a save that worked (res: politica_guardar()'s),
# al_cancelar() on Cancelar.
.politica_editor_logica <- function(input, output, session, shared, ns = identity,
                                    pol_mode, pol_editing, pol_cal_offset, pol_cal_selected,
                                    al_guardar = function(res) NULL, al_cancelar = function() NULL) {
  # Dynamic params sub-form (reactive to type changes) -------------------------
  output[[ns("pol_params_ui")]] <- renderUI({
    req(!is.null(input[[ns("pol_type")]]))
    pol <- isolate(pol_editing())
    current_params <- if (!is.null(pol) && pol_mode() %in% c("edit") &&
                          identical(pol$type, input[[ns("pol_type")]])) {
      pol$params %||% list()
    } else {
      list()
    }
    .policy_params_ui(input[[ns("pol_type")]], current_params, ns = ns)
  })

  # Show/hide roll-direction section based on type -----------------------------
  observeEvent(input[[ns("pol_type")]], {
    req(!is.null(input[[ns("pol_type")]]))
    if (input[[ns("pol_type")]] == "last_day") {
      shinyjs::hide(ns("pol_roll_dir_section"))
      shinyjs::runjs(sprintf("var el = document.querySelector('input[name=%s][value=backward]'); if(el) el.click();",
                             ns("pol_roll_direction")))
    } else {
      shinyjs::show(ns("pol_roll_dir_section"))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Live preview — interactive dual-month calendar ----------------------------
  output[[ns("pol_preview_ui")]] <- renderUI({
    req(!is.null(input[[ns("pol_type")]]))
    type <- input[[ns("pol_type")]]
    roll <- input[[ns("pol_roll_direction")]] %||% "forward"
    params <- switch(type,
      weekdays      = list(days = as.integer(input[[ns("pol_param_days")]] %||% as.character(1:5))),
      skip_holidays = list(country = toupper(input[[ns("pol_param_country")]] %||% "MX")),
      last_day      = list(),
      month_days    = list(days = sort(as.integer(
                        input[[ns("pol_param_month_days")]] %||% c("15","30")))),
      offset_days   = list(n    = as.integer(input[[ns("pol_param_n")]]    %||% 0L),
                           unit = input[[ns("pol_param_unit")]] %||% "naturales"),
      list()
    )
    # T116: with the staff's holiday overrides, as the policy saves it.
    hcache <- if (type == "skip_holidays")
      festivos_cache(list(list(params = params)),
                     tryCatch(shared$holiday_overrides_db(), error = function(e) NULL))
    else list()
    .policy_cal_preview_render(
      type     = type,
      params   = params,
      roll     = roll,
      hcache   = hcache,
      offset   = pol_cal_offset(),
      selected = pol_cal_selected(),
      ns       = ns
    )
  })

  # Calendar navigation (‹ › buttons) ----------------------------------------
  observeEvent(input[[ns("pol_cal_nav")]], {
    dir <- as.integer(input[[ns("pol_cal_nav")]]$dir %||% 1L)
    pol_cal_offset(pol_cal_offset() + dir)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Dot click — toggle a date in/out of the selection -------------------------
  observeEvent(input[[ns("pol_cal_click")]], {
    ds  <- as.character(input[[ns("pol_cal_click")]] %||% "")
    req(nzchar(ds))
    sel <- pol_cal_selected()
    if (ds %in% sel) pol_cal_selected(sel[sel != ds])
    else             pol_cal_selected(c(sel, ds))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Reset selection when policy type changes ----------------------------------
  observeEvent(input[[ns("pol_type")]], {
    pol_cal_selected(character(0))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Save -----------------------------------------------------------------------
  observeEvent(input[[ns("pol_btn_save")]], {
    if (!puede_o_avisa(shared, "can_manage_policies")) return()
    nm <- trimws(input[[ns("pol_name")]] %||% "")
    if (!nzchar(nm)) {
      showNotification(tr("tablas.ajustes.aviso.politica_nombre_requerido"), type = "warning")
      return()
    }
    type <- input[[ns("pol_type")]] %||% "weekdays"
    roll <- input[[ns("pol_roll_direction")]] %||% "forward"
    if (type == "last_day") roll <- "backward"
    # T66: the field no longer stops a text, a decimal or a number out of
    # -365..365; checked here. Empty or a text (NA) used to become 0 days.
    if (identical(type, "offset_days")) {
      rev_n <- hd_numero_revisar(if (is.null(input[[ns("pol_param_n")]])) 0 else input[[ns("pol_param_n")]],
                                 tr("tablas.ajustes.politicas.dias_desplazar"),
                                 min = -365, max = 365, entero = TRUE)
      if (!rev_n$ok) {
        showNotification(rev_n$aviso, type = "warning"); return()
      }
    }

    params <- tryCatch(.collect_policy_params(type, input, ns), error = function(e) list())

    mode <- isolate(pol_mode())
    now  <- Sys.time()
    user <- tryCatch(shared$current_user_info()$username, error = function(e) "")

    pid <- if (mode == "new") NULL else isolate(pol_editing())$id
    res <- politica_guardar(shared, mode = mode, pid = pid, nombre = nm, tipo = type,
                            params = params, roll = roll, user = user)
    if (isTRUE(res$ok)) {
      al_guardar(res)
      showNotification(
        if (mode == "new") tr("tablas.ajustes.aviso.politica_creada") else tr("tablas.ajustes.aviso.politica_actualizada"),
        type = "message", duration = 3
      )
    } else showNotification(res$aviso, type = "error", duration = 5)
  }, ignoreInit = TRUE)

  # Cancel editor --------------------------------------------------------------
  observeEvent(input[[ns("pol_btn_cancel")]], {
    al_cancelar()
  }, ignoreInit = TRUE)

  invisible(NULL)
}

# The write of a new or edited policy (T120: Configuración's editor and the
# Pasivos wizard's). Checks can_manage_policies on the server; applies the
# change to S3's catalog as it is now (CM34c), bumps it, updates the session's
# copy and logs it. An edited policy re-places, right away, the provisional
# future provisions of the liabilities that use it (the daily refresh, forced
# for this client). Returns list(ok, db, pid, mode) or list(ok = FALSE, aviso).
politica_guardar <- function(shared, mode, pid = NULL, nombre, tipo, params, roll, user = "") {
  if (!isTRUE(tryCatch(puede(isolate(shared$usuario()), "can_manage_policies"), error = function(e) FALSE)))
    return(list(ok = FALSE, aviso = permiso_denegado_texto(tryCatch(isolate(shared$usuario()), error = function(e) NULL),
                                                          "can_manage_policies")))
  # CM38: after a client jump that could not read the catalog, the session's
  # copy is empty -- saving it would wipe this client's policies.
  if ("policy_catalog" %in% tryCatch(isolate(shared$ctx_no_cargadas()), error = function(e) character()))
    return(list(ok = FALSE, aviso = tr("comun.ctx.guardar_bloqueado")))
  now <- Sys.time()
  if (identical(mode, "new")) pid <- uuid::UUIDgenerate()
  cambio <- function(fresco) {
    if (identical(mode, "new"))
      return(dplyr::bind_rows(fresco, tibble::tibble(
        id = pid, name = nombre, type = tipo, params = list(params), roll_direction = roll,
        created_by = user, created_at = now, updated_at = now)))
    idx <- which(fresco$id == pid)
    if (!length(idx))   # deleted by another session meanwhile
      stop(structure(class = c("politica_no_encontrada", "error", "condition"),
                     list(message = "policy not found", call = NULL)))
    fresco$name[idx]           <- nombre
    fresco$type[idx]           <- tipo
    fresco$params[idx]         <- list(params)
    fresco$roll_direction[idx] <- roll
    fresco$updated_at[idx]     <- now
    fresco
  }
  cid <- shared$effective_client_id()
  tryCatch({
    new_db <- modificar_policy_catalog(cambio, client_id = cid)
    bump_sync_version("policy_catalog_db", client_id = cid)
    shared$policy_catalog_db(new_db)
    log_action(
      user        = user,
      module      = "settings_policies",
      action      = if (identical(mode, "new")) "crear_politica" else "editar_politica",
      description = paste0(if (identical(mode, "new")) "Política creada: " else "Política editada: ", nombre,
                           " (tipo=", tipo, ", roll=", roll, ")"),
      target_id   = pid,
      metadata    = list(name = nombre, type = tipo, roll_direction = roll, params = params),
      client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
    )
    if (!identical(mode, "new")) .politica_reubicar_pasivos(shared, cid, pid)
    list(ok = TRUE, db = new_db, pid = pid, mode = mode)
  }, politica_no_encontrada = function(e) {
    list(ok = FALSE, aviso = tr("tablas.ajustes.aviso.politica_no_encontrada_recarga"))
  }, error = function(e) {
    list(ok = FALSE, aviso = tr("tablas.ajustes.aviso.error_guardar", error = e$message))
  })
}

# T120: the liabilities (not deleted) that use policy `pid`, read strictly
# from S3 (a failed read stop()s: deleting is then refused, never allowed
# blind). Returns their names.
# Self-contained on purpose (no Pasivos helper): Configuración's fixtures
# load this file without the Pasivos engine.
politica_pasivos_que_usan <- function(cid, pid) {
  l <- .s3_leer_para_escribir(S3_KEYS$pasivos_liabilities, cid)
  if (is.null(l) || !is.data.frame(l) || !nrow(l) || !"policy_ids" %in% names(l)) return(character())
  ids <- lapply(as.character(l$policy_ids), function(v)
    if (is.na(v) || !nzchar(v)) character() else trimws(strsplit(v, ",", fixed = TRUE)[[1]]))
  usa <- vapply(ids, function(x) pid %in% x, logical(1))
  if ("estado" %in% names(l)) usa <- usa & !(l$estado %in% "deleted")
  nombres <- if ("nombre" %in% names(l)) as.character(l$nombre[usa]) else as.character(l$id[usa])
  sort(unique(nombres))
}

# T120: an edited policy re-places the provisions of the liabilities that use
# it now, not tomorrow (the daily refresh, forced for this client). A failure
# is logged and left to the next refresh; the policy is saved anyway.
.politica_reubicar_pasivos <- function(shared, cid, pid) {
  usan <- tryCatch(politica_pasivos_que_usan(cid, pid), error = function(e) character())
  if (!length(usan)) return(invisible(NULL))
  res <- tryCatch(pasivos_refrescar_provisiones(cid, hoy = hoy_cliente(shared), forzar = TRUE),
                  error = function(e) { message("[politicas] reubicar pasivos: ", conditionMessage(e)); NULL })
  if (!is.null(res) && !is.null(res$tabla)) {
    tryCatch(shared$pasivos_provisions_db(res$tabla), error = function(e) NULL)
    tryCatch(bump_sync_version("pasivos_provisions_db", client_id = cid), error = function(e) NULL)
  }
  # The refresh logs each liability in the Pasivos history; this says why.
  tryCatch(log_action(
    user        = tryCatch(shared$current_user(), error = function(e) "system"),
    module      = "settings_policies",
    action      = "reubicar_provisiones_politica",
    description = paste0("Política editada: provisiones de ", paste(usan, collapse = ", "), " reubicadas"),
    target_id   = pid,
    metadata    = list(pasivos = usan, reubicadas = if (!is.null(res$cambios)) sum(res$cambios$reubicadas %||% 0L) else 0L),
    client_id             = cid,
    viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
  ), error = function(e) NULL)
  invisible(NULL)
}

# T120: deleting a policy that liabilities use is refused: they are listed,
# to take it off them first. Nothing is removed silently.
.policy_delete_bloqueado_modal <- function(pol_name, pasivos) {
  modalDialog(
    title = tagList(icon("triangle-exclamation"), " ", tr("tablas.ajustes.politicas.borrar_bloqueado_titulo")),
    tags$p(tr("tablas.ajustes.politicas.borrar_bloqueado", politica = pol_name)),
    tags$ul(class = "mb-2", lapply(pasivos, tags$li)),
    footer = actionButton("pol_delete_cancel", tr("comun.cerrar"), class = "btn btn-secondary btn-sm"),
    size = "s", easyClose = FALSE
  )
}

settings_policies_observer <- function(input, output, session, shared) {

  session$userData$puede_politicas <- function()
    puede(tryCatch(isolate(shared$usuario()), error = function(e) NULL), "can_manage_policies")

  pol_mode         <- reactiveVal("list")       # "list" | "new" | "edit"
  pol_editing      <- reactiveVal(NULL)         # named list of the policy being edited
  pol_trigger      <- reactiveVal(0L)           # increment to re-render the list
  pol_cal_offset   <- reactiveVal(0L)           # month offset from current for preview cals
  pol_cal_selected <- reactiveVal(character(0)) # "YYYY-MM-DD" strings clicked in input cals

  .get_catalog <- function() {
    db <- tryCatch(shared$policy_catalog_db(), error = function(e) NULL)
    if (!is.null(db) && nrow(db) > 0) db else load_policy_catalog(client_id = shared$effective_client_id())
  }

  .open_editor <- function(pol = NULL, mode = "new") {
    pol_editing(pol)
    pol_mode(mode)
    pol_cal_offset(0L)
    pol_cal_selected(character(0))
    is_new <- identical(mode, "new")
    output$policies_editor_ui <- renderUI({
      .policy_editor_form(pol, is_new = is_new)
    })
    shinyjs::hide("policies_list_section")
    shinyjs::show("policies_editor_section")
  }

  .close_editor <- function() {
    pol_mode("list")
    pol_editing(NULL)
    shinyjs::hide("policies_editor_section")
    shinyjs::show("policies_list_section")
  }

  # Render list ----------------------------------------------------------------
  output$policies_list_ui <- renderUI({
    pol_trigger()
    .policies_list_render(.get_catalog())
  })

  # The editor's sub-form, preview, save and cancel (T120: shared with the
  # Pasivos wizard, which draws the same editor with its own ids).
  .politica_editor_logica(input, output, session, shared, ns = identity,
    pol_mode = pol_mode, pol_editing = pol_editing,
    pol_cal_offset = pol_cal_offset, pol_cal_selected = pol_cal_selected,
    al_guardar  = function(res) { pol_trigger(pol_trigger() + 1L); .close_editor() },
    al_cancelar = function() .close_editor())

  # "Nueva política" button ----------------------------------------------------
  observeEvent(input$pol_btn_new, {
    .open_editor(pol = NULL, mode = "new")
    shinyjs::runjs("setTimeout(function(){ var el=document.getElementById('pol_name'); if(el) el.focus(); }, 150);")
  }, ignoreInit = TRUE)

  # Edit / Delete / Assign dispatcher (JS fires pol_action) -------------------
  observeEvent(input$pol_action, {
    act <- input$pol_action
    req(!is.null(act), nzchar(act$action %||% ""))

    if (act$action == "edit") {
      db      <- .get_catalog()
      pol_row <- db[db$id == act$id, , drop = FALSE]
      if (!nrow(pol_row)) return()
      pol           <- as.list(pol_row[1, ])
      pol$params    <- pol_row$params[[1]] %||% list()
      .open_editor(pol = pol, mode = "edit")

    } else if (act$action == "delete") {
      # T120: liabilities that use it block the deletion (a failed read too).
      pasivos <- tryCatch(politica_pasivos_que_usan(shared$effective_client_id(), act$id), error = function(e) NA_character_)
      if (length(pasivos) && !identical(pasivos, NA_character_)) {
        showModal(.policy_delete_bloqueado_modal(act$name %||% act$id, pasivos)); return()
      }
      if (identical(pasivos, NA_character_)) {
        showNotification(tr("tablas.ajustes.politicas.borrar_sin_lectura"), type = "error"); return()
      }
      pp_now   <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
      affected <- if (!is.null(pp_now) && nrow(pp_now))
        unique(pp_now$parte[pp_now$policy_id == act$id])
      else character()
      pol_editing(list(id = act$id))
      showModal(.policy_delete_confirm_modal(act$name %||% act$id, affected))

    } else if (act$action == "assign") {
      db      <- .get_catalog()
      pol_row <- db[db$id == act$id, , drop = FALSE]
      if (!nrow(pol_row)) return()
      pol_editing(list(id = act$id, name = pol_row$name[[1]],
                       type = pol_row$type[[1]]))

      # Which partes already have this policy?
      pp         <- tryCatch(shared$partner_policies_db(),
                             error = function(e) load_partner_policies(client_id = shared$effective_client_id()))
      pp         <- if (is.null(pp)) .schema_partner_policies()[0L,] else pp
      preselected <- unique(pp$parte[pp$policy_id == act$id])

      # All known partes (SAP data + existing assignments)
      from_sap <- tryCatch({
        d <- shared$sap_data()
        c(if (!is.null(d$AP) && "Parte" %in% names(d$AP)) unique(d$AP$Parte) else character(),
          if (!is.null(d$AR) && "Parte" %in% names(d$AR)) unique(d$AR$Parte) else character())
      }, error = function(e) character())
      all_p <- sort(unique(c(from_sap, pp$parte)[nzchar(c(from_sap, pp$parte))]))

      output$policies_assign_ui <- renderUI({
        .policies_assign_form(act$id, pol_row$name[[1]], pol_row$type[[1]],
                               preselected, all_p)
      })
      shinyjs::hide("policies_list_section")
      shinyjs::hide("policies_editor_section")
      shinyjs::show("policies_assign_section")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Assign: close helper -------------------------------------------------------
  .close_assign <- function() {
    shinyjs::hide("policies_assign_section")
    shinyjs::show("policies_list_section")
    pol_editing(NULL)
  }

  # Assign: save ---------------------------------------------------------------
  observeEvent(input$pol_assign_save, {
    if (!puede_o_avisa(shared, "can_manage_policies")) return()
    pol   <- isolate(pol_editing())
    req(!is.null(pol$id))
    pol_id  <- pol$id
    ledger  <- input$pol_assign_ledger %||% "AP"
    sel_now <- input$pol_assign_partners %||% character(0)
    user    <- tryCatch(shared$current_user_info()$username, error = function(e) "")
    now     <- Sys.time()

    pp_old <- tryCatch(shared$partner_policies_db(),
                       error = function(e) load_partner_policies(client_id = shared$effective_client_id()))
    if (is.null(pp_old)) pp_old <- .schema_partner_policies()[0L, ]

    # Partes that had this policy in the copy the form showed
    had_policy <- unique(pp_old$parte[pp_old$policy_id == pol_id])

    # This user's change against that copy: partes to ADD and to REMOVE
    to_add    <- setdiff(sel_now, had_policy)
    to_remove <- setdiff(had_policy, sel_now)

    # CM34c: applied to S3's assignments as they are now, never to the
    # session's copy -- another session's assignments (of this policy to
    # other partes, or of other policies) stay.
    cambio <- function(fresco) {
      # 1. Remove this policy from de-selected partes (keep their OTHER policies)
      out <- fresco[!(fresco$parte %in% to_remove & fresco$policy_id == pol_id), , drop = FALSE]
      # 2. Append it to newly selected partes that don't have it yet (at the
      #    end of their existing stack)
      nuevos <- to_add[!to_add %in% out$parte[out$policy_id == pol_id]]
      if (length(nuevos)) {
        new_rows <- lapply(nuevos, function(p) {
          existing_p <- out[out$parte == p, , drop = FALSE]
          next_order <- if (nrow(existing_p)) max(existing_p$policy_order, na.rm = TRUE) + 1L else 1L
          # Preserve ledger/interco from existing assignments if any
          p_ledger   <- if (nrow(existing_p)) existing_p$ledger[[1]] %||% ledger else ledger
          p_interco  <- if (nrow(existing_p)) isTRUE(existing_p$is_interco[[1]]) else FALSE
          tibble::tibble(parte = p, policy_id = pol_id, policy_order = next_order, ledger = p_ledger,
                         is_interco = p_interco, is_manual = FALSE, linked_by = user, linked_at = now)
        })
        out <- dplyr::bind_rows(out, dplyr::bind_rows(new_rows))
      }
      out
    }

    tryCatch({
      pp_new <- modificar_partner_policies(cambio, client_id = shared$effective_client_id())
      bump_sync_version("partner_policies_db", client_id = shared$effective_client_id())
      shared$partner_policies_db(pp_new)
      n_add <- length(to_add); n_rem <- length(to_remove)
      msg <- paste0(
        if (n_add > 0L) sprintf("+%d asignado%s", n_add, if (n_add != 1L) "s" else "") else NULL,
        if (n_add > 0L && n_rem > 0L) ", " else NULL,
        if (n_rem > 0L) sprintf("-%d removido%s", n_rem, if (n_rem != 1L) "s" else "") else NULL,
        if (n_add == 0L && n_rem == 0L) "Sin cambios." else "."
      )
      .close_assign()
      showNotification(msg, type = "message", duration = 3)
      log_action(
        user        = user,
        module      = "settings_policies",
        action      = "asignar_politica_socios",
        description = paste0("Asignación de política ", pol_id, ": ", msg),
        target_id   = pol_id,
        metadata    = list(policy_id = pol_id, ledger = ledger, added = to_add, removed = to_remove),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, error = function(e) {
      showNotification(tr("tablas.ajustes.aviso.error_guardar_asignaciones", error = e$message),
                       type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  # Assign: cancel -------------------------------------------------------------
  observeEvent(input$pol_assign_cancel, {
    .close_assign()
  }, ignoreInit = TRUE)

  # Delete confirm -------------------------------------------------------------
  observeEvent(input$pol_delete_confirm, {
    if (!puede_o_avisa(shared, "can_manage_policies")) return()
    # CM38: same as saving -- never delete from a copy the jump could not read.
    if ("policy_catalog" %in% tryCatch(isolate(shared$ctx_no_cargadas()), error = function(e) character())) {
      showNotification(tr("comun.ctx.guardar_bloqueado"), type = "error"); return()
    }
    del_id   <- isolate(pol_editing())$id
    if (is.null(del_id)) return()   # nothing chosen: nothing deleted, nothing said
    # T120: checked again on S3 as it is now (another session may have
    # assigned it meanwhile); a failed read refuses too.
    pasivos <- tryCatch(politica_pasivos_que_usan(shared$effective_client_id(), del_id), error = function(e) NA_character_)
    if (length(pasivos)) {
      removeModal()
      if (identical(pasivos, NA_character_)) showNotification(tr("tablas.ajustes.politicas.borrar_sin_lectura"), type = "error")
      else showModal(.policy_delete_bloqueado_modal(del_id, pasivos))
      return()
    }
    db       <- .get_catalog()
    del_name <- { m <- db$name[db$id == del_id]; if (length(m)) m[1] else del_id }
    cid      <- shared$effective_client_id()
    tryCatch({
      # CM34c: removed from S3's catalog and assignments as they are now --
      # only this policy and its assignments leave (a deliberate reduction,
      # listed in the net's guardian); another session's rows stay.
      new_db <- modificar_policy_catalog(function(fresco) fresco[!(fresco$id %in% del_id), , drop = FALSE],
                                         client_id = cid, permitir_reduccion = TRUE)
      bump_sync_version("policy_catalog_db", client_id = cid)
      # Cascade: remove the deleted policy from all partner assignments
      pp_new <- modificar_partner_policies(function(fresco) fresco[!(fresco$policy_id %in% del_id), , drop = FALSE],
                                           client_id = cid, permitir_reduccion = TRUE)
      bump_sync_version("partner_policies_db", client_id = cid)
      shared$partner_policies_db(pp_new)
      shared$policy_catalog_db(new_db)
      pol_trigger(pol_trigger() + 1L)
      pol_editing(NULL)
      removeModal()
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
      showNotification(tr("tablas.ajustes.aviso.politica_eliminada"), type = "message", duration = 3)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "settings_policies",
        action      = "eliminar_politica",
        description = paste0("Política eliminada: ", del_name),
        target_id   = del_id,
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, error = function(e) {
      removeModal()
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
      showNotification(tr("tablas.ajustes.aviso.error_eliminar", error = e$message), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$pol_delete_cancel, {
    pol_editing(NULL)
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
  }, ignoreInit = TRUE)
}
