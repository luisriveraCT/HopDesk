# =============================================================================
# R/settings/settings_policies.R
# Políticas de Pago UI helpers, calendar preview, revert modal, and settings_policies_observer().
# =============================================================================
# (Originally part of R/settings_module.R; split in Stage 7.1 for session efficiency.)

# =============================================================================
# POLÍTICAS DE PAGO — UI helpers
# =============================================================================

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

  rows <- lapply(seq_len(nrow(catalog_df)), function(i) {
    pol      <- catalog_df[i, ]
    pol_id   <- pol$id
    pol_name <- gsub("'", "\\'", pol$name, fixed = TRUE)
    div(class = "d-flex align-items-center gap-2 py-2 border-bottom",
      .policy_type_badge(pol$type),
      tags$span(class = "flex-grow-1 small", pol$name),
      tags$small(class = "text-muted me-1",
                 .policy_roll_label(pol$roll_direction)),
      tags$button(
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
      tags$button(
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
.policy_editor_form <- function(pol = NULL, is_new = TRUE) {
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
        textInput("pol_name", NULL, value = current_name,
                  placeholder = "Ej: Fin de mes MXN", width = "100%")
      ),
      div(class = "col-md-6",
        tags$label(class = "form-label small fw-semibold mb-1", "Tipo"),
        selectInput("pol_type", NULL, width = "100%",
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

    uiOutput("pol_params_ui"),

    div(id = "pol_roll_dir_section",
      tags$label(class = "form-label small fw-semibold d-block mb-1",
                 "Cuando cae en día no válido"),
      radioButtons("pol_roll_direction", NULL, inline = TRUE,
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
    uiOutput("pol_preview_ui"),

    tags$hr(class = "my-3"),
    div(class = "d-flex gap-2",
      actionButton("pol_btn_save",
        tagList(icon("floppy-disk"), " Guardar"),
        class = "btn btn-primary btn-sm"),
      actionButton("pol_btn_cancel",
        tagList(icon("xmark"), " Cancelar"),
        class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# ── Dynamic params sub-form ───────────────────────────────────────────────────
.policy_params_ui <- function(type, params = list()) {
  switch(type,
    weekdays = {
      sel <- if (length(params$days)) as.character(as.integer(params$days)) else as.character(1:5)
      div(class = "mb-2",
        tags$label(class = "form-label small fw-semibold mb-1", "Días permitidos"),
        checkboxGroupInput("pol_param_days", NULL, inline = TRUE,
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
        selectInput("pol_param_country", NULL, width = "260px",
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
          checkboxGroupInput("pol_param_month_days", NULL, inline = TRUE,
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
          numericInput("pol_param_n", NULL,
                       value = n_val, min = -365L, max = 365L, step = 1L,
                       width = "110px"),
          selectInput("pol_param_unit", NULL,
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

# ── Live preview ──────────────────────────────────────────────────────────────
.policy_preview_render <- function(input) {
  type <- input$pol_type
  if (is.null(type)) return(div())

  roll <- input$pol_roll_direction %||% "forward"

  params <- switch(type,
    weekdays      = list(days = as.integer(input$pol_param_days %||% as.character(1:5))),
    skip_holidays = list(country = toupper(input$pol_param_country %||% "MX")),
    last_day      = list(),
    month_days    = list(days = sort(as.integer(input$pol_param_month_days %||% c("15", "30")))),
    offset_days   = list(n    = as.integer(input$pol_param_n    %||% 0L),
                         unit = input$pol_param_unit %||% "naturales"),
    list()
  )

  pol_obj <- list(type = type, params = params, roll_direction = roll)

  hcache <- list()
  if (type == "skip_holidays" && nzchar(params$country %||% "")) {
    hcache[[params$country]] <- tryCatch(
      get_holidays(params$country), error = function(e) as.Date(character())
    )
  }

  samples <- if (type == "skip_holidays" && nzchar(params$country %||% "")) {
    ctry     <- params$country
    hols     <- tryCatch(get_holidays(ctry), error = function(e) as.Date(character()))
    today    <- Sys.Date()
    upcoming <- sort(hols[hols >= today])
    if (length(upcoming) >= 3L) upcoming[1:3] else (today + c(0L, 7L, 14L))
  } else if (type == "month_days") {
    # Generate samples near each upcoming fixed day so the preview is informative
    fdays   <- if (length(params$days)) sort(as.integer(params$days)) else c(15L, 30L)
    today   <- Sys.Date()
    m_start <- lubridate::floor_date(today, "month")
    samp    <- as.Date(character())
    for (mo in 0L:3L) {
      m     <- as.Date(m_start) + months(mo)
      m_end <- as.Date(lubridate::ceiling_date(m, "month") - 1L)
      max_d <- lubridate::day(m_end)
      for (fd in fdays) {
        fd_date <- as.Date(paste0(format(m, "%Y-%m-"), sprintf("%02d", min(fd, max_d))))
        s       <- fd_date - 2L   # 2 days before each fixed day
        if (s >= today && !s %in% samp) {
          samp <- c(samp, s)
          if (length(samp) >= 3L) break
        }
      }
      if (length(samp) >= 3L) break
    }
    if (!length(samp)) samp <- today + c(0L, 7L, 14L)
    head(samp, 3L)
  } else {
    Sys.Date() + c(0L, 7L, 14L)
  }
  results <- lapply(samples, function(d) {
    computed <- tryCatch(
      compose_policies(d, list(pol_obj), hcache),
      error = function(e) d
    )
    list(orig = d, res = computed, diff = as.integer(computed - d))
  })

  meses_es <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  fmt_date <- function(d) {
    wds <- c("lun","mar","mié","jue","vie","sáb","dom")
    wd  <- lubridate::wday(d, week_start = 1L)
    sprintf("%d %s %d (%s)", lubridate::day(d),
            meses_es[lubridate::month(d)], lubridate::year(d), wds[wd])
  }

  tags$table(class = "table table-sm table-borderless small mb-0",
    tags$thead(class = "text-muted",
      tags$tr(
        tags$th("Fecha original"),
        tags$th(style = "width:20px;", "→"),
        tags$th("Fecha ajustada")
      )
    ),
    tags$tbody(
      tagList(lapply(results, function(r) {
        changed <- r$diff != 0L
        tags$tr(
          tags$td(fmt_date(r$orig)),
          tags$td("→"),
          tags$td(
            class = if (changed) "text-primary fw-semibold" else "text-muted",
            fmt_date(r$res),
            if (changed)
              tags$span(class = "ms-2 badge bg-primary-subtle text-primary",
                        style = "font-size:0.7em;",
                        sprintf("%+d d", r$diff))
            else NULL
          )
        )
      }))
    )
  )
}

# ── Collect params from form inputs ──────────────────────────────────────────
.collect_policy_params <- function(type, input) {
  switch(type,
    weekdays      = list(days = sort(as.integer(input$pol_param_days %||% as.character(1:5)))),
    skip_holidays = list(country = toupper(trimws(input$pol_param_country %||% "MX"))),
    last_day      = list(),
    month_days    = list(days = sort(as.integer(input$pol_param_month_days %||% c("15","30")))),
    offset_days   = list(n    = as.integer(input$pol_param_n    %||% 0L),
                         unit = input$pol_param_unit %||% "naturales"),
    list()
  )
}

# =============================================================================
# Interactive dual-month calendar preview
# =============================================================================
.policy_cal_preview_render <- function(type, params, roll, hcache, offset, selected) {

  today   <- Sys.Date()
  m1_date <- lubridate::floor_date(today, "month") + months(as.integer(offset))
  m2_date <- m1_date + months(1L)

  meses <- c("Ene","Feb","Mar","Abr","May","Jun",
             "Jul","Ago","Sep","Oct","Nov","Dic")

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

    hdr <- lapply(c("D","L","M","M","J","V","S"), function(h)
      tags$span(h, style =
        "font-size:8px;color:#4b5563;font-weight:600;line-height:1;"
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
          "background:#7c3aed;box-shadow:0 0 6px 3px rgba(124,58,237,0.75);"
        else if (!is_input && is_proj)
          "background:#2563eb;box-shadow:0 0 6px 3px rgba(37,99,235,0.75);"
        else
          "background:#cbd5e1;"
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
              "Shiny.setInputValue('pol_cal_click','%s',{priority:'event'})", ds
            )
          )
        else
          tags$span(style = sty, title = ds)
      }
    })

    div(style = paste0("width:", MONTH_W, ";"),
      div(style = paste0("font-size:9px;color:#1e293b;font-weight:700;",
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
    "background:none;border:1px solid #d1d5db;border-radius:5px;",
    "padding:3px 10px;font-size:15px;font-weight:bold;cursor:pointer;",
    "color:#1e293b;line-height:1.3;"
  )
  pair_base   <- "display:flex;gap:14px;padding:10px 14px;border-radius:10px;"
  input_pair  <- paste0(pair_base, "background:rgba(124,58,237,.07);")
  output_pair <- paste0(pair_base, "background:rgba(37,99,235,.07);")
  lbl_base <- paste0(
    "font-size:8px;font-weight:700;text-transform:uppercase;",
    "letter-spacing:.08em;margin-bottom:6px;text-align:center;"
  )
  arrow_sty <- paste0(
    "display:flex;align-items:center;justify-content:center;",
    "padding:0 4px;font-size:28px;font-weight:300;",
    "color:#94a3b8;line-height:1;align-self:center;"
  )

  # ── Holiday list (skip_holidays only) ────────────────────────────────────
  holiday_panel <- if (type == "skip_holidays" && length(hcache) > 0L) {
    hols     <- sort(unique(as.Date(unlist(hcache, use.names = FALSE))))
    hols     <- hols[!is.na(hols)]
    cur_yr   <- as.integer(format(Sys.Date(), "%Y"))
    meses_ab <- c("ene","feb","mar","abr","may","jun",
                  "jul","ago","sep","oct","nov","dic")

    # Index of the first current-year holiday (for auto-scroll)
    first_cur_idx <- which(lubridate::year(hols) == cur_yr)[1L]
    # Pixel offset per row (~17px) — scroll target
    scroll_top <- if (!is.na(first_cur_idx) && first_cur_idx > 1L)
      (first_cur_idx - 1L) * 17L else 0L

    rows <- lapply(seq_along(hols), function(i) {
      d      <- hols[[i]]
      yr     <- lubridate::year(d)
      is_cur <- yr == cur_yr
      lbl    <- sprintf("%d de %s %04d",
                        lubridate::day(d), meses_ab[lubridate::month(d)], yr)
      row_sty <- paste0(
        "font-size:9px;padding:2px 0;white-space:nowrap;",
        if (is_cur) "color:#1e293b;font-weight:700;" else "color:#6b7280;"
      )
      div(style = row_sty,
          HTML(paste0(
            "<span style='margin-right:4px;opacity:.5;'>&#10005;</span>", lbl
          )))
    })

    list_id <- "pol_hol_list"
    scroll_js <- if (scroll_top > 0L)
      sprintf("setTimeout(function(){var el=document.getElementById('%s');if(el)el.scrollTop=%d;},80);",
              list_id, scroll_top)
    else ""

    tagList(
      div(id    = list_id,
          style = paste0(
            "display:flex;flex-direction:column;align-self:stretch;",
            "padding:10px 12px;border-radius:10px;",
            "background:rgba(37,99,235,.05);",
            "max-height:160px;overflow-y:auto;min-width:130px;"),
        div(style = paste0(lbl_base, "color:#1d4ed8;margin-bottom:6px;"),
            "Festivos"),
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
        style = "font-size:9px;color:#94a3b8;font-style:italic;",
        "Seleccione días para simular cambios."
      ),
      div(style = "display:flex;align-items:center;gap:10px;",
        tags$button(style = btn_sty,
          onclick = paste0("Shiny.setInputValue('pol_cal_nav',",
                           "{dir:-1,nonce:Math.random()},{priority:'event'})"),
          HTML("&#8249;")
        ),
        tags$span(
          style = "font-size:11px;color:#1e293b;font-weight:600;min-width:150px;text-align:center;",
          sprintf("%s → %s", m1_lbl, m2_lbl)
        ),
        tags$button(style = btn_sty,
          onclick = paste0("Shiny.setInputValue('pol_cal_nav',",
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
          tags$span(style = paste0(lbl_base, "color:#6d28d9;"), "Selección"),
          div(style = "display:flex;gap:14px;", in_m1, in_m2)
        )
      ),
      # Arrow separator
      div(style = arrow_sty, HTML("&#10230;")),
      # Output pair
      div(style = output_pair,
        div(style = "display:flex;flex-direction:column;align-items:center;",
          tags$span(style = paste0(lbl_base, "color:#1d4ed8;"), "Proyección"),
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
settings_policies_observer <- function(input, output, session, shared) {

  pol_mode         <- reactiveVal("list")       # "list" | "new" | "edit"
  pol_editing      <- reactiveVal(NULL)         # named list of the policy being edited
  pol_trigger      <- reactiveVal(0L)           # increment to re-render the list
  pol_cal_offset   <- reactiveVal(0L)           # month offset from current for preview cals
  pol_cal_selected <- reactiveVal(character(0)) # "YYYY-MM-DD" strings clicked in input cals

  .get_catalog <- function() {
    db <- tryCatch(shared$policy_catalog_db(), error = function(e) NULL)
    if (!is.null(db) && nrow(db) > 0) db else load_policy_catalog()
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

  # Dynamic params sub-form (reactive to type changes) -------------------------
  output$pol_params_ui <- renderUI({
    req(!is.null(input$pol_type))
    pol <- isolate(pol_editing())
    current_params <- if (!is.null(pol) && pol_mode() %in% c("edit") &&
                          identical(pol$type, input$pol_type)) {
      pol$params %||% list()
    } else {
      list()
    }
    .policy_params_ui(input$pol_type, current_params)
  })

  # Show/hide roll-direction section based on type -----------------------------
  observeEvent(input$pol_type, {
    req(!is.null(input$pol_type))
    if (input$pol_type == "last_day") {
      shinyjs::hide("pol_roll_dir_section")
      shinyjs::runjs("var el = document.querySelector('input[name=pol_roll_direction][value=backward]'); if(el) el.click();")
    } else {
      shinyjs::show("pol_roll_dir_section")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Live preview — interactive dual-month calendar ----------------------------
  output$pol_preview_ui <- renderUI({
    req(!is.null(input$pol_type))
    type <- input$pol_type
    roll <- input$pol_roll_direction %||% "forward"
    params <- switch(type,
      weekdays      = list(days = as.integer(input$pol_param_days %||% as.character(1:5))),
      skip_holidays = list(country = toupper(input$pol_param_country %||% "MX")),
      last_day      = list(),
      month_days    = list(days = sort(as.integer(
                        input$pol_param_month_days %||% c("15","30")))),
      offset_days   = list(n    = as.integer(input$pol_param_n    %||% 0L),
                           unit = input$pol_param_unit %||% "naturales"),
      list()
    )
    hcache <- list()
    if (type == "skip_holidays") {
      ctry <- toupper(params$country %||% "MX")
      if (nzchar(ctry))
        hcache[[ctry]] <- tryCatch(
          get_holidays(ctry), error = function(e) as.Date(character()))
    }
    .policy_cal_preview_render(
      type     = type,
      params   = params,
      roll     = roll,
      hcache   = hcache,
      offset   = pol_cal_offset(),
      selected = pol_cal_selected()
    )
  })

  # Calendar navigation (‹ › buttons) ----------------------------------------
  observeEvent(input$pol_cal_nav, {
    dir <- as.integer(input$pol_cal_nav$dir %||% 1L)
    pol_cal_offset(pol_cal_offset() + dir)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Dot click — toggle a date in/out of the selection -------------------------
  observeEvent(input$pol_cal_click, {
    ds  <- as.character(input$pol_cal_click %||% "")
    req(nzchar(ds))
    sel <- pol_cal_selected()
    if (ds %in% sel) pol_cal_selected(sel[sel != ds])
    else             pol_cal_selected(c(sel, ds))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Reset selection when policy type changes ----------------------------------
  observeEvent(input$pol_type, {
    pol_cal_selected(character(0))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

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
                             error = function(e) load_partner_policies())
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

  # Save -----------------------------------------------------------------------
  observeEvent(input$pol_btn_save, {
    nm <- trimws(input$pol_name %||% "")
    if (!nzchar(nm)) {
      showNotification("El nombre de la política es requerido.", type = "warning")
      return()
    }
    type <- input$pol_type %||% "weekdays"
    roll <- input$pol_roll_direction %||% "forward"
    if (type == "last_day") roll <- "backward"

    params <- tryCatch(.collect_policy_params(type, input), error = function(e) list())

    mode <- isolate(pol_mode())
    now  <- Sys.time()
    user <- tryCatch(shared$current_user_info()$username, error = function(e) "")

    db <- .get_catalog()

    if (mode == "new") {
      new_row <- tibble::tibble(
        id             = uuid::UUIDgenerate(),
        name           = nm,
        type           = type,
        params         = list(params),
        roll_direction = roll,
        created_by     = user,
        created_at     = now,
        updated_at     = now
      )
      new_db <- dplyr::bind_rows(db, new_row)
    } else {
      pid <- isolate(pol_editing())$id
      idx <- which(db$id == pid)
      if (!length(idx)) {
        showNotification("No se encontró la política. Recarga el panel.", type = "error")
        return()
      }
      db$name[idx]           <- nm
      db$type[idx]           <- type
      db$params[[idx]]       <- params
      db$roll_direction[idx] <- roll
      db$updated_at[idx]     <- now
      new_db <- db
    }

    tryCatch({
      save_policy_catalog(new_db, client_id = shared$effective_client_id())
      bump_sync_version("policy_catalog_db")
      shared$policy_catalog_db(new_db)
      pol_trigger(pol_trigger() + 1L)
      .close_editor()
      showNotification(
        if (mode == "new") "Política creada." else "Política actualizada.",
        type = "message", duration = 3
      )
    }, error = function(e) {
      showNotification(paste("Error al guardar:", e$message), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  # Cancel editor --------------------------------------------------------------
  observeEvent(input$pol_btn_cancel, {
    .close_editor()
  }, ignoreInit = TRUE)

  # Assign: close helper -------------------------------------------------------
  .close_assign <- function() {
    shinyjs::hide("policies_assign_section")
    shinyjs::show("policies_list_section")
    pol_editing(NULL)
  }

  # Assign: save ---------------------------------------------------------------
  observeEvent(input$pol_assign_save, {
    pol   <- isolate(pol_editing())
    req(!is.null(pol$id))
    pol_id  <- pol$id
    ledger  <- input$pol_assign_ledger %||% "AP"
    sel_now <- input$pol_assign_partners %||% character(0)
    user    <- tryCatch(shared$current_user_info()$username, error = function(e) "")
    now     <- Sys.time()

    pp_old <- tryCatch(shared$partner_policies_db(),
                       error = function(e) load_partner_policies())
    if (is.null(pp_old)) pp_old <- .schema_partner_policies()[0L, ]

    # Partes that previously had this policy
    had_policy <- unique(pp_old$parte[pp_old$policy_id == pol_id])

    # Partes to ADD (selected now, didn't have it before)
    to_add    <- setdiff(sel_now, had_policy)
    # Partes to REMOVE (had it, not selected now)
    to_remove <- setdiff(had_policy, sel_now)

    # Build updated partner_policies:
    # 1. Remove this policy from de-selected partes (keep their OTHER policies)
    pp_new <- pp_old[!(pp_old$parte %in% to_remove & pp_old$policy_id == pol_id), ,
                     drop = FALSE]

    # 2. Append policy to newly selected partes (at end of their existing stack)
    if (length(to_add)) {
      new_rows <- lapply(to_add, function(p) {
        # Determine next policy_order for this parte
        existing_p <- pp_new[pp_new$parte == p, , drop = FALSE]
        next_order <- if (nrow(existing_p)) max(existing_p$policy_order, na.rm = TRUE) + 1L else 1L
        # Preserve ledger/interco from existing assignments if any
        p_ledger   <- if (nrow(existing_p)) existing_p$ledger[[1]] %||% ledger else ledger
        p_interco  <- if (nrow(existing_p)) isTRUE(existing_p$is_interco[[1]]) else FALSE
        tibble::tibble(
          parte        = p,
          policy_id    = pol_id,
          policy_order = next_order,
          ledger       = p_ledger,
          is_interco   = p_interco,
          is_manual    = FALSE,
          linked_by    = user,
          linked_at    = now
        )
      })
      pp_new <- dplyr::bind_rows(pp_new, dplyr::bind_rows(new_rows))
    }

    tryCatch({
      save_partner_policies(pp_new, client_id = shared$effective_client_id())
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
    }, error = function(e) {
      showNotification(paste("Error al guardar asignaciones:", e$message),
                       type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  # Assign: cancel -------------------------------------------------------------
  observeEvent(input$pol_assign_cancel, {
    .close_assign()
  }, ignoreInit = TRUE)

  # Delete confirm -------------------------------------------------------------
  observeEvent(input$pol_delete_confirm, {
    del_id <- isolate(pol_editing())$id
    db     <- .get_catalog()
    new_db <- if (!is.null(del_id)) db[db$id != del_id, , drop = FALSE] else db
    tryCatch({
      if (!is.null(del_id)) {
        save_policy_catalog(new_db, client_id = shared$effective_client_id())
        bump_sync_version("policy_catalog_db")
        # Cascade: remove deleted policy from all partner assignments
        pp_old <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
        if (!is.null(pp_old) && nrow(pp_old)) {
          pp_new <- pp_old[pp_old$policy_id != del_id, , drop = FALSE]
          if (nrow(pp_new) < nrow(pp_old)) {
            save_partner_policies(pp_new, client_id = shared$effective_client_id())
            shared$partner_policies_db(pp_new)
          }
        }
      }
      shared$policy_catalog_db(new_db)
      pol_trigger(pol_trigger() + 1L)
      pol_editing(NULL)
      removeModal()
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
      showNotification("Política eliminada.", type = "message", duration = 3)
    }, error = function(e) {
      removeModal()
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
      showNotification(paste("Error al eliminar:", e$message), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$pol_delete_cancel, {
    pol_editing(NULL)
    removeModal()
    show_settings_modal(input, output, session, shared)
    output$settings_panel <- renderUI({ .policies_merged_panel_ui("reglas") })
  }, ignoreInit = TRUE)
}

