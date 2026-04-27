# =============================================================================
# R/pagar_hoy_module.R  —  Agenda de Hoy
#
# Changes vs prior version:
#   1. AP table now color-codes rows by supplier catalog match status:
#        green  = alias found, CLABE present → will be included in PPL file
#        yellow = partial match (alias found but CLABE missing)
#        red    = no match → will be EXCLUDED from PPL file
#   2. "↓ Bajío" download button replaced by smarter flow:
#        - Generates PPL via generate_ppl() (ppl_generator.R, verified format)
#        - Shows a match-report modal FIRST: n included / n excluded + which ones
#        - User confirms, then download fires
#   3. Supplier catalog now loaded from load_proveedores() (S3, managed in
#      Bancos → Proveedores — Phase 2 admin UI). Falls back to empty gracefully.
#   4. generate_bajio_file() replaced by generate_ppl() from ppl_generator.R
# =============================================================================

pagarHoyUI <- function(id) {
  ns <- NS(id)
  tagList(
  tags$script(HTML(sprintf("
    $(document).off('click.phlookup').on('click.phlookup', '.ph-lookup-btn', function(e) {
      e.stopPropagation();
      Shiny.setInputValue('%s', {parte: $(this).data('parte'), codigo: $(this).data('codigo') || ''}, {priority:'event'});
    });
    $(document).off('click.phlink').on('click.phlink', '.ph-link-btn', function(e) {
      e.stopPropagation();
      Shiny.setInputValue('%s', {
        parte:  $(this).data('parte'),
        alias:  $(this).data('alias'),
        emp:    $(this).data('emp'),
        nonce:  Math.random()
      }, {priority:'event'});
    });
    $(document).off('click.phunlink').on('click.phunlink', '.ph-unlink-btn', function(e) {
      e.stopPropagation();
      Shiny.setInputValue('%s', {
        parte: $(this).data('parte'),
        emp:   $(this).data('emp'),
        nonce: Math.random()
      }, {priority:'event'});
    });
    $(document).off('click.phcycle').on('click.phcycle', '.ph-cycle-alias-btn', function(e) {
      e.stopPropagation();
      Shiny.setInputValue('%s', {
        parte: $(this).data('parte'),
        emp:   $(this).data('emp'),
        current_alias: $(this).data('current-alias'),
        current_clabe: $(this).data('current-clabe') || '',
        nonce: Math.random()
      }, {priority:'event'});
    });
  ",
    paste0(id, "-ph_lookup_parte"),
    paste0(id, "-ph_link_supplier"),
    paste0(id, "-ph_unlink_supplier"),
    paste0(id, "-ph_cycle_alias")
  ))),
  div(
    class = "ph-container d-flex flex-column h-100",
    div(class = "ph-summary-bar d-flex align-items-center gap-3 px-3 py-2 border-bottom bg-white",
      div(class = "d-flex align-items-center gap-2",
        tags$span(class = "text-muted small", "Fecha:"),
        tags$strong(format(Sys.Date(), "%d %b %Y"))
      ),
      div(class = "vr"),
      uiOutput(ns("global_total_ui")),
      actionButton(ns("load_bancos_saldos"),
        tagList(icon("download"), " Cargar saldos de Bancos"),
        class = "btn btn-sm btn-outline-info"
      ),
      div(class = "ms-auto d-flex gap-2",
        downloadButton(ns("download_all"), "\u2b07 Descargar todos (.zip)",
                       class = "btn btn-sm btn-outline-primary"),
        actionButton(ns("clear_all"), "\U0001f5d1 Vaciar agenda",
                     class = "btn btn-sm btn-outline-danger")
      )
    ),
    uiOutput(ns("ph_panels_ui"))
  )
  ) # end tagList
}

pagarHoyServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive company list — sourced from shared$company_map() so adding a new
    # empresa via the Empresas panel updates the tabs without app restart.
    all_companies_rv <- reactive({
      cmap <- shared$company_map()
      sort(unname(cmap))
    })

    # ── Opening balance store: per-company, per-currency ────────────────────
    saldos_apertura <- reactiveVal(setNames(list(), character()))

    observe({
      cur <- saldos_apertura()
      for (e in all_companies_rv()) {
        if (is.null(cur[[e]])) cur[[e]] <- list(MXN = 0, USD = 0)
      }
      saldos_apertura(cur)
    })

    # ── Staged queue ────────────────────────────────────────────────────────
    staged <- reactive({
      ph <- shared$pagar_hoy_db()
      if (is.null(ph) || !nrow(ph)) return(tibble::tibble(
        id=character(), ledger=character(), Empresa=character(),
        Moneda=character(), Documento=character(), Parte=character(),
        Importe=numeric(), FechaVenc=as.Date(character()),
        staged_by=character(), staged_at=as.POSIXct(character()),
        status=character()
      ))
      ph |> dplyr::filter(status == "pending")
    })

    # ── Supplier catalog (reactive, reloads when bancos module changes it) ──
    provs_db <- reactive({
      # If shared$proveedores_db exists (Phase 2), use it; else load from S3
      if (!is.null(shared$proveedores_db)) {
        shared$proveedores_db()
      } else {
        tryCatch(load_proveedores(), error = function(e) .schema_proveedores())
      }
    })

    # ── Match helper: returns annotated AP rows with ppl_status column ──────
    # ppl_status: "ok" | "no_clabe" | "no_match"
    #
    # Vectorized replacement for the old purrr::map_int approach (O(rows×suppliers)).
    # Strategy:
    #   1. One vectorized toupper pass over the filtered catalog — O(m), done once.
    #   2. Named-vector exact lookup for alias then nombre — O(1) per row via hash.
    #   3. Substring scan only for still-unmatched rows against aliases ≥3 chars.
    .annotate_ap <- function(ap_rows, empresa) {
      provs <- provs_db()
      if (is.null(provs) || !nrow(provs)) {
        return(dplyr::mutate(ap_rows, ppl_status = "no_match", alias = NA_character_,
                              clabe_dest = NA_character_, medio_pago = NA_character_))
      }

      ini          <- names(isolate(shared$company_map()))[unname(isolate(shared$company_map())) == empresa]
      empresa_keys <- c(empresa, ini)
      emp_provs <- provs |>
        dplyr::filter(is.na(Empresa) | Empresa == "" | Empresa %in% empresa_keys) |>
        dplyr::select(nombre, alias, clabe, medio_pago, banco) |>
        dplyr::filter(!is.na(alias) | !is.na(nombre))

      if (!nrow(emp_provs)) {
        return(dplyr::mutate(ap_rows, ppl_status = "no_match", alias = NA_character_,
                              clabe_dest = NA_character_, medio_pago = NA_character_))
      }

      # Full-catalog lookup — used ONLY for session overrides, which may
      # point to aliases from a different empresa entry (e.g. NTSBB vs TDCBANORTE).
      all_provs <- provs |>
        dplyr::filter(!is.na(alias) & nzchar(trimws(alias))) |>
        dplyr::select(nombre, alias, clabe, medio_pago, banco)
      all_alias_up     <- toupper(trimws(all_provs$alias))
      all_alias_lookup <- setNames(seq_len(nrow(all_provs)), all_alias_up)
      all_alias_lookup <- all_alias_lookup[nzchar(names(all_alias_lookup))]

      # ── Override map: Parte → alias (manually linked by user) ───────────────
      # Overrides stored in shared$parte_alias_map_db take priority over all
      # fuzzy matching. Empresa "" means "applies to all companies".
      override_map <- tryCatch(
        if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db()
        else load_parte_alias_map(),
        error = function(e) NULL
      )

      # Build a named vector: parte_upper → alias, for this empresa
      override_alias <- character(0)
      if (!is.null(override_map) && nrow(override_map)) {
        ov <- override_map[
          is.na(override_map$Empresa) | override_map$Empresa == "" |
          override_map$Empresa %in% empresa_keys, , drop = FALSE]
        if (nrow(ov)) {
          override_alias <- setNames(ov$alias, toupper(trimws(ov$Parte)))
        }
      }

      # Pre-compute lookup vectors
      alias_up  <- toupper(trimws(emp_provs$alias  %||% ""))
      nombre_up <- toupper(trimws(emp_provs$nombre %||% ""))
      alias_lookup  <- setNames(seq_len(nrow(emp_provs)), alias_up)
      nombre_lookup <- setNames(seq_len(nrow(emp_provs)), nombre_up)
      alias_lookup  <- alias_lookup[nzchar(names(alias_lookup))]
      nombre_lookup <- nombre_lookup[nzchar(names(nombre_lookup))]

      parte_up  <- toupper(trimws(ap_rows$Parte))
      codigo_up <- toupper(trimws(ap_rows$Codigo %||% rep("", nrow(ap_rows))))

      # Pre-initialize output columns so Pass -1 can assign by index into an existing column.
      # (Tibbles reject `df$newcol[i] <- val` when the column doesn't yet exist.)
      ap_rows$alias      <- NA_character_
      ap_rows$clabe_dest <- NA_character_
      ap_rows$banco_dest <- NA_character_
      ap_rows$medio_pago <- NA_character_

      # sess_resolved tracks rows whose fields are already written and must not be
      # overwritten by later passes. Initialized here so Pass 0 can set it.
      # (Pass -1 is the highest-priority user-facing override and ignores this flag.)
      idx           <- rep(NA_integer_, length(parte_up))
      sess_resolved <- rep(FALSE, length(parte_up))

      # Pass 0: exact Codigo match — uses FULL catalog so cross-empresa codes
      # (e.g. NTSBB stored under empresa NTS but staging under NL) resolve correctly.
      # Writes directly to ap_rows and marks sess_resolved so later passes can't clobber it.
      if (any(nzchar(codigo_up))) {
        for (i in which(nzchar(codigo_up))) {
          all_hit <- all_alias_lookup[codigo_up[i]]
          if (!is.na(all_hit)) {
            ap_rows$alias[i]      <- all_provs$alias[all_hit]
            ap_rows$clabe_dest[i] <- all_provs$clabe[all_hit]
            ap_rows$banco_dest[i] <- all_provs$banco[all_hit]
            ap_rows$medio_pago[i] <- all_provs$medio_pago[all_hit]
            sess_resolved[i]      <- TRUE
          }
        }
      }

      # Pass -1: session-only alias overrides — highest-priority, overrides even Pass 0.
      # Uses full-catalog lookup so aliases belonging to a different empresa entry resolve.
      # Writes all four fields directly; does not check sess_resolved (intentional).
      sess_ovr <- tryCatch(.session_alias_overrides(), error = function(e) list())
      if (length(sess_ovr)) {
        for (i in seq_along(parte_up)) {
          key <- paste0(empresa, "|", parte_up[i])
          if (!is.null(sess_ovr[[key]])) {
            ovr_val   <- sess_ovr[[key]]
            ovr_parts <- strsplit(ovr_val, "\\|")[[1]]
            ovr_alias <- toupper(trimws(ovr_parts[1]))
            ovr_clabe <- if (length(ovr_parts) > 1) trimws(ovr_parts[2]) else ""
            # Match by alias+CLABE for uniqueness; fall back to alias-only (backward compat)
            hit_rows <- if (nzchar(ovr_clabe)) {
              which(toupper(trimws(all_provs$alias)) == ovr_alias &
                    trimws(all_provs$clabe %||% "") == ovr_clabe)
            } else {
              which(toupper(trimws(all_provs$alias)) == ovr_alias)
            }
            hit <- if (length(hit_rows)) hit_rows[1L] else NA_integer_
            if (!is.na(hit)) {
              ap_rows$alias[i]      <- all_provs$alias[hit]
              ap_rows$clabe_dest[i] <- all_provs$clabe[hit]
              ap_rows$banco_dest[i] <- all_provs$banco[hit]
              ap_rows$medio_pago[i] <- all_provs$medio_pago[hit]
              idx[i]           <- NA_integer_
              sess_resolved[i] <- TRUE
            }
          }
        }
      }

      # Pass 0b: override map — if this Parte has a saved link, use it directly.
      # Skip rows already resolved by the session override (user's explicit choice wins).
      # Falls back to all_alias_lookup (full catalog) when the empresa-scoped lookup
      # misses — handles cross-empresa aliases (e.g. NTSBB stored under a different
      # Empresa entry). When resolved via all_provs, fields are written directly and
      # the row is marked resolved so later passes leave it alone.
      if (length(override_alias)) {
        ov_hit <- override_alias[parte_up]
        for (i in seq_along(parte_up)) {
          if (!sess_resolved[i] && !is.na(ov_hit[i])) {
            alias_key <- toupper(trimws(ov_hit[i]))
            hit <- alias_lookup[alias_key]
            if (!is.na(hit)) {
              idx[i] <- hit          # resolved in empresa-scoped catalog — normal path
            } else {
              # Fall back to full catalog (cross-empresa alias)
              all_hit <- all_alias_lookup[alias_key]
              if (!is.na(all_hit)) {
                ap_rows$alias[i]      <- all_provs$alias[all_hit]
                ap_rows$clabe_dest[i] <- all_provs$clabe[all_hit]
                ap_rows$banco_dest[i] <- all_provs$banco[all_hit]
                ap_rows$medio_pago[i] <- all_provs$medio_pago[all_hit]
                idx[i]           <- NA_integer_   # already written, skip idx assign
                sess_resolved[i] <- TRUE
              }
            }
          }
        }
      }

      # Pass 1: exact alias match (for rows not yet resolved by any override)
      miss <- is.na(idx) & !sess_resolved
      if (any(miss)) idx[miss] <- alias_lookup[parte_up[miss]]

      # Pass 2: exact nombre match
      miss <- is.na(idx) & !sess_resolved
      if (any(miss)) idx[miss] <- nombre_lookup[parte_up[miss]]

      # Pass 3: alias substring scan
      still_miss <- which(is.na(idx) & !sess_resolved)
      if (length(still_miss) && length(alias_lookup)) {
        valid_aliases <- names(alias_lookup)[nchar(names(alias_lookup)) >= 3]
        for (i in still_miss) {
          hit <- which(stringr::str_detect(parte_up[i], stringr::fixed(valid_aliases)))
          if (length(hit)) idx[i] <- alias_lookup[valid_aliases[hit[1]]]
        }
      }

      # Assign fields from idx — but only for rows NOT already resolved by Pass -1
      # (those have idx = NA_integer_ and their fields already written above).
      needs_idx <- !is.na(idx)
      if (any(needs_idx)) {
        ap_rows$alias[needs_idx]      <- emp_provs$alias[idx[needs_idx]]
        ap_rows$clabe_dest[needs_idx] <- emp_provs$clabe[idx[needs_idx]]
        ap_rows$banco_dest[needs_idx] <- emp_provs$banco[idx[needs_idx]]
        ap_rows$medio_pago[needs_idx] <- emp_provs$medio_pago[idx[needs_idx]]
      }
      # ppl_status: check actual column values so Pass -1 rows are classified correctly
      ap_rows$ppl_status <- dplyr::case_when(
        is.na(ap_rows$alias)                                                       ~ "no_match",
        is.na(ap_rows$clabe_dest) | trimws(ap_rows$clabe_dest) == "" |
          ap_rows$clabe_dest == "000000000000000000"                               ~ "no_clabe",
        TRUE                                                                       ~ "ok"
      )
      ap_rows
    }

    # ── Annotation cache — keyed on (empresa, Parte+Documento hash) ──────────
    # Prevents re-annotating the same AP rows on currency-switch or saldo edits.
    # The env lives for the module session; no manual invalidation needed.
    .annot_cache <- new.env(parent = emptyenv())

    # Session-only alias overrides: named list keyed by "empresa|parte_upper" → alias string.
    # Never saved to S3. Resets on page refresh. Used by .annotate_ap to override catalog match.
    .session_alias_overrides <- reactiveVal(list())

    # Incremented after every link/unlink to force table re-render
    .link_counter <- reactiveVal(0L)

    .annotate_ap_cached <- function(ap_rows, empresa) {
      key <- paste0(empresa, "_",
                    digest::digest(list(ap_rows$Parte, ap_rows$Documento, ap_rows$Codigo)))
      cached <- .annot_cache[[key]]
      if (!is.null(cached)) return(cached)
      result <- .annotate_ap(ap_rows, empresa)
      .annot_cache[[key]] <- result
      result
    }

    # ── Currency choices update ──────────────────────────────────────────────
    observe({
      s <- staged()
      lapply(all_companies_rv(), function(emp) {
        choices <- sort(unique(dplyr::filter(s, Empresa == emp)$Moneda))
        if (!length(choices)) choices <- c("MXN","USD")
        updateSelectInput(session, paste0("bal_cur_", emp),
          choices  = choices,
          selected = isolate(input[[paste0("bal_cur_", emp)]]) %||% choices[1])
      })
    })

    # ── Global totals bar ────────────────────────────────────────────────────
    output$global_total_ui <- renderUI({
      s <- staged()
      if (!nrow(s)) return(tags$span(class = "text-muted small", "Sin elementos en agenda"))
      ap <- s |> dplyr::filter(ledger == "AP") |>
        dplyr::group_by(Moneda) |> dplyr::summarise(t = sum(Importe, na.rm = TRUE), .groups = "drop")
      ar <- s |> dplyr::filter(ledger == "AR") |>
        dplyr::group_by(Moneda) |> dplyr::summarise(t = sum(Importe, na.rm = TRUE), .groups = "drop")
      all_cur <- sort(unique(c(ap$Moneda, ar$Moneda)))
      pills <- lapply(all_cur, function(cur) {
        out <- sum(ap$t[ap$Moneda == cur], na.rm = TRUE)
        in_ <- sum(ar$t[ar$Moneda == cur], na.rm = TRUE)
        net <- in_ - out
        tagList(
          if (out > 0) tags$span(class = "badge bg-danger me-1",
            paste0("\u2193 ", cur, " ", fmt_money(out))) else NULL,
          if (in_ > 0) tags$span(class = "badge bg-success me-1",
            paste0("\u2191 ", cur, " ", fmt_money(in_))) else NULL,
          tags$span(class = paste("badge me-2", if (net >= 0) "bg-success" else "bg-danger"),
            paste0("= ", cur, " ", fmt_money(net)))
        )
      })
      div(class = "d-flex align-items-center gap-1 flex-wrap",
        tags$span(class = "text-muted small me-1", "Posici\u00f3n neta:"), !!!pills)
    })

    # ── Tab labels via JS ────────────────────────────────────────────────────
    observe({
      s <- staged()
      lapply(all_companies_rv(), function(emp) {
        n <- nrow(dplyr::filter(s, Empresa == emp))
        lbl <- if (n > 0) paste0(emp, " (", n, ")") else emp
        session$sendCustomMessage("updateTabLabel", list(
          tabsetId = ns("company_tab"), value = emp, label = lbl))
      })
    })

    # ── Company tab panels — rebuilt reactively when company list changes ────
    output$ph_panels_ui <- renderUI({
      all_cos <- all_companies_rv()
      panels <- lapply(all_cos, function(emp) {
        tabPanel(
          title = emp,
          value = emp,
          div(class = "ph-company-panel p-3",
            div(class = "d-flex align-items-center gap-4 mb-3 px-3 py-2 rounded bg-light border",
              div(class = "d-flex gap-3 align-items-center",
                div(
                  tags$label("Moneda", class = "form-label small text-muted mb-1"),
                  selectInput(ns(paste0("bal_cur_", emp)), NULL,
                              choices = c("MXN","USD"), width = "100px")
                ),
                div(
                  tags$label("Saldo apertura", class = "form-label small text-muted mb-1"),
                  numericInput(ns(paste0("bal_", emp)), NULL,
                               value = 0, min = 0, step = 1000, width = "180px")
                )
              ),
              tags$div(class = "vr", style = "height:48px;"),
              uiOutput(ns(paste0("calc_", emp)))
            ),
            div(class = "ph-section mb-3",
              uiOutput(ns(paste0("hdr_ap_", emp))),
              uiOutput(ns(paste0("legend_", emp))),
              DT::dataTableOutput(ns(paste0("tbl_ap_", emp)))
            ),
            div(class = "ph-section",
              uiOutput(ns(paste0("hdr_ar_", emp))),
              DT::dataTableOutput(ns(paste0("tbl_ar_", emp)))
            )
          )
        )
      })
      do.call(tabsetPanel, c(panels, list(id = ns("company_tab"))))
    })

    # ── Per-company outputs and observers — registered once per empresa ──────
    observe({
      for (emp in all_companies_rv()) {
        if (isTRUE(session$userData[[paste0("ph_obs_", emp)]])) next
        session$userData[[paste0("ph_obs_", emp)]] <- TRUE

        local({
          emp <- emp  # capture loop variable

      # Balance calc strip
      output[[paste0("calc_", emp)]] <- renderUI({
        s_emp   <- staged() |> dplyr::filter(Empresa == emp)
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        bal_val <- saldos_apertura()[[emp]][[cur_sel]] %||% 0
        s_ap    <- s_emp |> dplyr::filter(ledger == "AP", Moneda == cur_sel)
        s_ar    <- s_emp |> dplyr::filter(ledger == "AR", Moneda == cur_sel)
        tp      <- sum(s_ap$Importe, na.rm = TRUE)
        tc      <- sum(s_ar$Importe, na.rm = TRUE)
        net     <- bal_val - tp + tc
        div(class = "d-flex gap-4 align-items-center small",
          div(class = "text-center",
            div(class = "text-muted mb-0", "Saldo apertura"),
            div(class = "fw-bold fs-6", fmt_money(bal_val))),
          div(class = "text-center",
            div(class = "text-danger mb-0", paste0("\u2212 Pagos (", cur_sel, ")")),
            div(class = "fw-bold text-danger", fmt_money(tp))),
          div(class = "text-center",
            div(class = "text-success mb-0", paste0("+ Cobros (", cur_sel, ")")),
            div(class = "fw-bold text-success", fmt_money(tc))),
          div(class = "text-center",
            div(class = "text-muted mb-0", "Posici\u00f3n proyectada"),
            div(class = paste("fw-bold fs-5", if (net < 0) "text-danger" else "text-success"),
                fmt_money(net))),
          actionButton(
            ns(paste0("open_icmap_", emp)),
            tagList(icon("diagram-project"), " Mapa IC"),
            class = "btn btn-sm btn-outline-secondary ms-auto",
            title = paste("Abrir mapa intercompany para", emp)
          )
        )
      })

      # ── Opening balance: sync numericInput ↔ saldos_apertura ────────────────
      # Observer 1 — currency switch: load stored value for new currency
      observeEvent(input[[paste0("bal_cur_", emp)]], {
        cur    <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        stored <- saldos_apertura()[[emp]][[cur]] %||% 0
        updateNumericInput(session, paste0("bal_", emp), value = stored)
      }, ignoreInit = FALSE)

      # Observer 2 — numericInput edit: persist value for current currency
      # Dirty-check: if Observer 1 fired updateNumericInput with the same value
      # that's already stored, skip the write to break the Observer1→2 loop.
      observeEvent(input[[paste0("bal_", emp)]], {
        cur         <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        new_val     <- as.numeric(input[[paste0("bal_", emp)]] %||% 0)
        current_val <- saldos_apertura()[[emp]][[cur]] %||% 0
        if (isTRUE(abs(new_val - current_val) < 0.001)) return()
        sa <- saldos_apertura()
        sa[[emp]][[cur]] <- new_val
        saldos_apertura(sa)
      }, ignoreInit = TRUE)

      # ── Legend for color codes ──────────────────────────────────────────────
      output[[paste0("legend_", emp)]] <- renderUI({
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
        if (!nrow(s_cur)) return(NULL)

        ann   <- .annotate_ap_cached(s_cur, emp)
        n_ok  <- sum(ann$ppl_status == "ok")
        n_par <- sum(ann$ppl_status == "no_clabe")
        n_no  <- sum(ann$ppl_status == "no_match")

        div(class = "d-flex gap-3 align-items-center mb-2 px-1",
          tags$span(class = "small text-muted", "Estado BanBajío:"),
          tags$span(class = "ph-legend-dot ph-ok"),
          tags$span(class = "small", paste0(n_ok, " incluido(s)")),
          if (n_par > 0) tagList(
            tags$span(class = "ph-legend-dot ph-partial"),
            tags$span(class = "small text-warning", paste0(n_par, " sin CLABE"))
          ) else NULL,
          if (n_no > 0) tagList(
            tags$span(class = "ph-legend-dot ph-nomatch"),
            tags$span(class = "small text-danger", paste0(n_no, " sin catálogo"))
          ) else NULL
        )
      })

      # ── AP section header ────────────────────────────────────────────────────
      output[[paste0("hdr_ap_", emp)]] <- renderUI({
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
        tp      <- sum(s_cur$Importe, na.rm = TRUE)

        # Count matchable rows for button label
        n_ok <- if (nrow(s_cur)) {
          ann <- .annotate_ap_cached(s_cur, emp)
          sum(ann$ppl_status == "ok")
        } else 0L

        div(class = "ph-section-header d-flex align-items-center gap-2 mb-1",
          tags$span(class = "badge bg-danger", "\u2193 Pagos"),
          tags$span(class = "text-muted small",
            if (nrow(s_cur))
              paste0(nrow(s_cur), " factura(s) \u2014 ", cur_sel, " ", fmt_money(tp))
            else paste0("Sin pagos para ", cur_sel)),
          if (nrow(s_cur))
            div(class = "ms-auto d-flex gap-2",
              actionButton(ns(paste0("remove_ap_", emp)), "\u2715 Quitar",
                           class = "btn btn-sm btn-outline-secondary"),
              actionButton(ns(paste0("dl_preview_", emp)),
                           tagList(icon("file-arrow-down"),
                             paste0(" BanBajío (", n_ok, ")")),
                           class = if (n_ok > 0) "btn btn-sm btn-outline-primary"
                                   else "btn btn-sm btn-outline-secondary disabled",
                           title = if (n_ok < nrow(s_cur))
                             paste0(nrow(s_cur) - n_ok, " proveedor(es) sin alias en catálogo")
                           else "Todos los proveedores encontrados"),
              actionButton(ns(paste0("confirm_ap_", emp)), "\u2713 Confirmar pagos",
                           class = "btn btn-sm btn-success")
            ) else NULL
        )
      })

      # ── AR section header ────────────────────────────────────────────────────
      output[[paste0("hdr_ar_", emp)]] <- renderUI({
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AR")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
        tc      <- sum(s_cur$Importe, na.rm = TRUE)
        div(class = "ph-section-header d-flex align-items-center gap-2 mb-1",
          tags$span(class = "badge bg-success", "\u2191 Cobros esperados"),
          tags$span(class = "text-muted small",
            if (nrow(s_cur))
              paste0(nrow(s_cur), " factura(s) \u2014 ", cur_sel, " ", fmt_money(tc))
            else paste0("Sin cobros para ", cur_sel)),
          if (nrow(s_cur))
            div(class = "ms-auto d-flex gap-2",
              actionButton(ns(paste0("remove_ar_", emp)), "\u2715 Quitar",
                           class = "btn btn-sm btn-outline-secondary"),
              actionButton(ns(paste0("confirm_ar_", emp)), "\u2713 Confirmar cobros",
                           class = "btn btn-sm btn-primary")
            ) else NULL
        )
      })

      # ── AP table — color-coded by catalog match ───────────────────────────
      output[[paste0("tbl_ap_", emp)]] <- DT::renderDataTable({
        .link_counter()   # reactive dependency — re-renders on link/unlink
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)

        if (!nrow(s_cur)) {
          return(DT::datatable(
            data.frame(Proveedor = character(), Referencia = character(),
                       Vencimiento = character(), Importe = character()),
            escape = FALSE, rownames = FALSE,
            options = list(pageLength = 50, dom = "t",
                           language = list(emptyTable = "Sin pagos en cola"))
          ))
        }

        # Referencia lookup: Documento → Factura, SAP rows first, manual entries overlay
        sap_ap_tbl <- tryCatch(shared$sap_data()[["AP"]], error = function(e) NULL)
        ref_lkup_tbl <- if (!is.null(sap_ap_tbl) &&
                             "Factura"   %in% names(sap_ap_tbl) &&
                             "Documento" %in% names(sap_ap_tbl)) {
          setNames(trimws(as.character(sap_ap_tbl[["Factura"]]   %||% "")),
                   trimws(as.character(sap_ap_tbl[["Documento"]])))
        } else character(0)
        man_inv_tbl <- tryCatch(shared$manual_inv(), error = function(e) NULL)
        if (!is.null(man_inv_tbl) && nrow(man_inv_tbl) &&
            all(c("Documento", "Factura", "ledger") %in% names(man_inv_tbl))) {
          man_ap <- man_inv_tbl[man_inv_tbl$ledger == "AP", , drop = FALSE]
          if (nrow(man_ap)) {
            man_lkup <- setNames(trimws(as.character(man_ap[["Factura"]]   %||% "")),
                                 trimws(as.character(man_ap[["Documento"]])))
            man_lkup <- man_lkup[nzchar(names(man_lkup))]
            ref_lkup_tbl[names(man_lkup)] <- man_lkup   # manual entries override SAP
          }
        }

        sess_ovr <- tryCatch(.session_alias_overrides(), error = function(e) list())
        ann <- .annotate_ap_cached(s_cur, emp) |>
          dplyr::arrange(FechaVenc)
        if (!"Codigo"     %in% names(ann)) ann[["Codigo"]]     <- NA_character_
        if (!"tipo_item" %in% names(ann)) ann[["tipo_item"]] <- NA_character_
        ann <- ann |> dplyr::mutate(
            # live_alias: session override wins over annotation result.
            # This ensures the badge always reflects the true current alias even when
            # .annotate_ap's empresa-filtered alias_lookup cannot resolve the override
            # (e.g. NTSBB belongs to a different empresa entry in the catalog).
            live_alias = {
              keys <- paste0(emp, "|", toupper(trimws(Parte)))
              resolved <- alias
              for (i in seq_along(keys)) {
                ovr <- sess_ovr[[keys[i]]]
                if (!is.null(ovr) && nzchar(ovr)) {
                  # Stored as "alias|clabe" compound key — extract alias part only
                  resolved[i] <- strsplit(ovr, "\\|")[[1]][1]
                }
              }
              resolved
            },
            Referencia = {
              doc <- trimws(as.character(Documento))
              ref <- if (length(ref_lkup_tbl)) trimws(ref_lkup_tbl[doc] %||% "") else ""
              dplyr::if_else(nzchar(ref) & !is.na(ref), ref, Documento)
            },
            abono_pfx = dplyr::if_else(
              !is.na(tipo_item) & tipo_item == "abono",
              '<span class="badge bg-warning text-dark me-1" style="font-size:0.65em;">ABONO</span>',
              ''
            ),
            # Build Proveedor cell with colored dot + alias badge if found
            Proveedor = dplyr::case_when(
              ppl_status == "ok" ~ paste0(
                abono_pfx,
                '<span class="ph-dot ph-dot-ok" title="Incluido en BanBajío"></span>',
                htmltools::htmlEscape(Parte),
                ' <button class="btn btn-xs btn-link p-0 ms-1 ph-cycle-alias-btn badge bg-success-subtle text-success border border-success-subtle" ',
                'data-parte="', htmltools::htmlEscape(Parte), '" ',
                'data-emp="', htmltools::htmlEscape(emp), '" ',
                'data-current-alias="', htmltools::htmlEscape(live_alias), '" ',
                'data-current-clabe="', ifelse(!is.na(clabe_dest), htmltools::htmlEscape(clabe_dest), ""), '" ',
                'title="Clic para cambiar cuenta bancaria" style="cursor:pointer;">',
                htmltools::htmlEscape(live_alias), ' \u21d5</button>',
                ' <button class="btn btn-xs btn-link p-0 ms-1 ph-lookup-btn text-secondary" ',
                'data-parte="', htmltools::htmlEscape(Parte), '" ',
                'data-codigo="', htmltools::htmlEscape(Codigo %||% ""), '" ',
                'title="Editar v\u00ednculo" style="font-size:0.8rem;line-height:1;">&#x270e;</button>'
              ),
              ppl_status == "no_clabe" ~ paste0(
                abono_pfx,
                '<span class="ph-dot ph-dot-partial" title="Sin CLABE en catálogo"></span>',
                htmltools::htmlEscape(Parte),
                ' <button class="btn btn-xs btn-link p-0 ms-1 ph-cycle-alias-btn badge bg-warning-subtle text-warning-emphasis border border-warning-subtle" ',
                'data-parte="', htmltools::htmlEscape(Parte), '" ',
                'data-emp="', htmltools::htmlEscape(emp), '" ',
                'data-current-alias="', htmltools::htmlEscape(live_alias), '" ',
                'data-current-clabe="', ifelse(!is.na(clabe_dest), htmltools::htmlEscape(clabe_dest), ""), '" ',
                'title="Clic para cambiar cuenta bancaria" style="cursor:pointer;">',
                htmltools::htmlEscape(live_alias), ' \u21d5 \u2014 sin CLABE</button>',
                ' <button class="btn btn-xs btn-link p-0 ms-1 ph-lookup-btn text-secondary" ',
                'data-parte="', htmltools::htmlEscape(Parte), '" ',
                'data-codigo="', htmltools::htmlEscape(Codigo %||% ""), '" ',
                'title="Editar v\u00ednculo" style="font-size:0.8rem;line-height:1;">&#x270e;</button>'
              ),
              TRUE ~ paste0(
                abono_pfx,
                '<span class="ph-dot ph-dot-nomatch" title="No encontrado en catálogo"></span>',
                htmltools::htmlEscape(Parte),
                ' <button class="btn btn-xs btn-link p-0 ms-1 ph-lookup-btn" ',
                'data-parte="', htmltools::htmlEscape(Parte), '" ',
                'data-codigo="', htmltools::htmlEscape(Codigo %||% ""), '" ',
                'title="Buscar en catálogo de proveedores" style="font-size:0.85rem;">\U0001f50d</button>'
              )
            ),
            Vencimiento = format(FechaVenc, "%d/%m/%Y"),
            Importe_fmt = fmt_money(Importe),
            row_bg = dplyr::case_when(
              ppl_status == "ok"       ~ "ph-row-ok",
              ppl_status == "no_clabe" ~ "ph-row-partial",
              TRUE                     ~ "ph-row-nomatch"
            )
          )

        tbl <- ann |> dplyr::select(Proveedor, Referencia, Vencimiento, Importe = Importe_fmt)
        row_classes <- ann$row_bg

        dt <- DT::datatable(
          tbl,
          escape    = FALSE,
          rownames  = FALSE,
          selection = list(mode = "multiple"),
          options   = list(
            pageLength = 50, dom = "t", scrollX = TRUE, scrollY = "200px",
            language   = list(emptyTable = "Sin pagos en cola"),
            rowCallback = DT::JS(sprintf(
              "function(row, data, index) {
                var classes = %s;
                if (index < classes.length) {
                  $(row).addClass(classes[index]);
                }
              }",
              jsonlite::toJSON(row_classes)
            ))
          )
        )
        dt
      }, server = FALSE)

      # ── AR table (unchanged) ─────────────────────────────────────────────
      output[[paste0("tbl_ar_", emp)]] <- DT::renderDataTable({
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AR")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
        tbl <- if (nrow(s_cur)) {
          s_cur |> dplyr::arrange(FechaVenc) |>
            dplyr::transmute(Cliente = Parte, Documento,
                             Vencimiento = format(FechaVenc, "%d/%m/%Y"),
                             Importe = fmt_money(Importe))
        } else {
          data.frame(Cliente = character(), Documento = character(),
                     Vencimiento = character(), Importe = character())
        }
        DT::datatable(tbl, escape = FALSE, rownames = FALSE,
          selection = list(mode = "multiple"),
          options   = list(pageLength = 50, dom = "t", scrollX = TRUE, scrollY = "180px",
                           language = list(emptyTable = "Sin cobros esperados")))
      }, server = FALSE)

      # ── PPL preview + download flow ──────────────────────────────────────
      # Step 1: user clicks "BanBajío (n)" → show match report modal
      observeEvent(input[[paste0("dl_preview_", emp)]], {
        s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
        sel     <- input[[paste0("tbl_ap_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
        rows    <- s_cur[sel, ]
        if (!nrow(rows)) {
          showNotification("Selecciona facturas primero.", type = "warning"); return()
        }

        ann  <- .annotate_ap_cached(rows, emp)
        ok   <- ann |> dplyr::filter(ppl_status == "ok")
        skip <- ann |> dplyr::filter(ppl_status != "ok")

        # Build Referencia lookup (Documento → Factura) — same logic as download handler
        # so the preview shows the concepto that will actually appear in the PPL file.
        sap_ap_prev <- tryCatch(shared$sap_data()[["AP"]], error = function(e) NULL)
        ref_lkup_prev <- if (!is.null(sap_ap_prev) &&
                              "Factura"   %in% names(sap_ap_prev) &&
                              "Documento" %in% names(sap_ap_prev)) {
          setNames(trimws(as.character(sap_ap_prev[["Factura"]] %||% "")),
                   trimws(as.character(sap_ap_prev[["Documento"]])))
        } else character(0)
        man_inv_prev <- tryCatch(shared$manual_inv(), error = function(e) NULL)
        if (!is.null(man_inv_prev) && nrow(man_inv_prev) &&
            all(c("Documento","Factura","ledger") %in% names(man_inv_prev))) {
          man_ap_prev <- man_inv_prev[man_inv_prev$ledger == "AP", , drop = FALSE]
          if (nrow(man_ap_prev)) {
            man_lkup_prev <- setNames(
              trimws(as.character(man_ap_prev[["Factura"]] %||% "")),
              trimws(as.character(man_ap_prev[["Documento"]])))
            man_lkup_prev <- man_lkup_prev[nzchar(names(man_lkup_prev))]
            ref_lkup_prev[names(man_lkup_prev)] <- man_lkup_prev
          }
        }
        # Helper: resolve Referencia for a Documento; falls back to Documento
        .ref_label <- function(doc) {
          d <- trimws(as.character(doc))
          r <- if (length(ref_lkup_prev)) {
            rv <- ref_lkup_prev[d]; if (is.na(rv)) "" else as.character(rv)
          } else ""
          if (nzchar(r)) r else d
        }

        # Origin account from bancos catalog
        cuenta_origen <- get_cuenta_ppl(emp, cur_sel, shared)

        showModal(modalDialog(
          title = tagList(icon("file-arrow-down"),
                          paste0(" BanBajío PPL — ", emp, " ", cur_sel)),
          size  = "m",
          easyClose = TRUE,

          tagList(
            # Summary counts
            div(class = "d-flex gap-3 mb-3",
              div(class = "text-center px-3 py-2 rounded bg-success-subtle border border-success-subtle",
                div(class = "fs-4 fw-bold text-success", nrow(ok)),
                div(class = "small text-success", "incluido(s)")),
              div(class = "text-center px-3 py-2 rounded bg-danger-subtle border border-danger-subtle",
                div(class = "fs-4 fw-bold text-danger", nrow(skip)),
                div(class = "small text-danger", "excluido(s)"))
            ),

            # Included list
            if (nrow(ok)) tagList(
              tags$p(class = "fw-semibold text-success mb-1",
                     icon("circle-check"), " Se incluirán en el archivo:"),
              tags$ul(class = "list-unstyled ms-2 small mb-3",
                lapply(seq_len(nrow(ok)), function(i)
                  tags$li(
                    tags$span(class = "badge bg-success me-1", ok$alias[i]),
                    ok$Parte[i], " — ", fmt_money(ok$Importe[i]),
                    tags$span(class = "text-muted ms-1",
                              paste0("[", .ref_label(ok$Documento[i]), "]"))
                  )
                )
              )
            ) else NULL,

            # Excluded list
            if (nrow(skip)) tagList(
              tags$p(class = "fw-semibold text-danger mb-1",
                     icon("circle-xmark"), " Se EXCLUIRÁN (sin alias/CLABE en catálogo):"),
              tags$ul(class = "list-unstyled ms-2 small mb-3",
                lapply(seq_len(nrow(skip)), function(i)
                  tags$li(class = "text-danger",
                    icon("triangle-exclamation"),
                    skip$Parte[i], " — ", fmt_money(skip$Importe[i]),
                    tags$span(class = "text-muted ms-1",
                              paste0("[", .ref_label(skip$Documento[i]), "]"))
                  )
                )
              ),
              tags$p(class = "small text-muted",
                     "Agrega estos proveedores al Catálogo (módulo Bancos) para incluirlos.")
            ) else NULL,

            if (!nzchar(cuenta_origen))
              tags$div(class = "alert alert-warning small",
                icon("triangle-exclamation"),
                " Cuenta origen no configurada. Ve a Bancos → Cuentas para registrar la cuenta de ",
                emp, ".")
            else
              tags$p(class = "small text-muted",
                "Cuenta origen: ", tags$code(cuenta_origen))
          ),

          footer = tagList(
            modalButton("Cancelar"),
            if (nrow(ok) > 0)
              downloadButton(ns(paste0("dl_ppl_", emp)),
                             paste0("\u2b07 Descargar (", nrow(ok), " pagos)"),
                             class = "btn btn-primary")
          )
        ))
      }, ignoreInit = TRUE)

      # Step 2: actual PPL download
      output[[paste0("dl_ppl_", emp)]] <- downloadHandler(
        filename = function() {
          paste0("pagos_", emp, "_",
                 format(Sys.Date(), "%d%m%Y"), "_001.txt")
        },
        content = function(file) {
          s_emp   <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
          cur_sel <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
          s_cur   <- s_emp |> dplyr::filter(Moneda == cur_sel)
          sel     <- input[[paste0("tbl_ap_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
          rows    <- s_cur[sel, ]
          ann     <- .annotate_ap_cached(rows, emp) |> dplyr::filter(ppl_status == "ok")

          if (!nrow(ann)) {
            writeLines("# No hay proveedores con alias en catálogo", file)
            return()
          }

          cuenta_origen <- get_cuenta_ppl(emp, cur_sel, shared)
          # Look up Factura (Referencia) from live SAP data to use as concepto.
          # Falls back to Documento if Factura is missing or blank.
          sap_ap   <- tryCatch(shared$sap_data()[["AP"]], error = function(e) NULL)
          ref_lkup <- if (!is.null(sap_ap) && "Factura" %in% names(sap_ap) &&
                          "Documento" %in% names(sap_ap)) {
            setNames(
              trimws(as.character(sap_ap[["Factura"]] %||% "")),
              trimws(as.character(sap_ap[["Documento"]]))
            )
          } else character(0)
          man_inv_dl <- tryCatch(shared$manual_inv(), error = function(e) NULL)
          if (!is.null(man_inv_dl) && nrow(man_inv_dl) &&
              all(c("Documento", "Factura", "ledger") %in% names(man_inv_dl))) {
            man_ap_dl <- man_inv_dl[man_inv_dl$ledger == "AP", , drop = FALSE]
            if (nrow(man_ap_dl)) {
              man_lkup_dl <- setNames(trimws(as.character(man_ap_dl[["Factura"]]   %||% "")),
                                      trimws(as.character(man_ap_dl[["Documento"]])))
              man_lkup_dl <- man_lkup_dl[nzchar(names(man_lkup_dl))]
              ref_lkup[names(man_lkup_dl)] <- man_lkup_dl
            }
          }

          ppl_rows <- ann |>
            dplyr::transmute(
              alias      = alias,
              clabe_dest = clabe_dest,
              banco_dest = banco_dest,
              medio_pago = medio_pago,
              importe    = Importe,
              concepto   = {
                doc <- trimws(as.character(Documento))
                ref <- if (length(ref_lkup)) as.character(ref_lkup[doc]) else rep("", length(doc))
                ref[is.na(ref)] <- ""
                substr(ifelse(nzchar(ref), ref, doc), 1, 75)
              }
            )

          lines <- generate_ppl(ppl_rows, cuenta_origen, Sys.Date(), 1L)
          write_ppl(lines, file)
          removeModal()
        }
      )

      # ── Remove AP ──────────────────────────────────────────────────────────
      observeEvent(input[[paste0("remove_ap_", emp)]], {
        s_emp <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur   <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur <- s_emp |> dplyr::filter(Moneda == cur)
        sel   <- input[[paste0("tbl_ap_", emp, "_rows_selected")]]
        if (!length(sel)) {
          showNotification(
            "Selecciona al menos una fila en la tabla de pagos para quitar.",
            type = "warning", duration = 3)
          return()
        }
        rows  <- s_cur[sel, ]
        if (!nrow(rows)) return()
        ph <- unstage_pagar_hoy(shared$pagar_hoy_db(),
               dplyr::mutate(rows, ledger = "AP") |> dplyr::select(ledger, Empresa, Moneda, Documento))
        shared$pagar_hoy_db(ph); save_pagar_hoy(ph)
        showNotification(paste0(nrow(rows), " pago(s) quitado(s)."), type = "message", duration = 2)
      }, ignoreInit = TRUE)

      # ── Remove AR ──────────────────────────────────────────────────────────
      observeEvent(input[[paste0("remove_ar_", emp)]], {
        s_emp <- staged() |> dplyr::filter(Empresa == emp, ledger == "AR")
        cur   <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur <- s_emp |> dplyr::filter(Moneda == cur)
        sel   <- input[[paste0("tbl_ar_", emp, "_rows_selected")]]
        if (!length(sel)) {
          showNotification(
            "Selecciona al menos una fila en la tabla de cobros para quitar.",
            type = "warning", duration = 3)
          return()
        }
        rows  <- s_cur[sel, ]
        if (!nrow(rows)) return()
        ph <- unstage_pagar_hoy(shared$pagar_hoy_db(),
               dplyr::mutate(rows, ledger = "AR") |> dplyr::select(ledger, Empresa, Moneda, Documento))
        shared$pagar_hoy_db(ph); save_pagar_hoy(ph)
        showNotification(paste0(nrow(rows), " cobro(s) quitado(s)."), type = "message", duration = 2)
      }, ignoreInit = TRUE)

      # ── Confirm AP ──────────────────────────────────────────────────────────
      # Helper: build bank account choices from shared$ctas_cuentas (Settings →
      # Cuentas de Empresa), filtered to matching empresa + moneda.
      # Falls back gracefully if not loaded.
      .bancos_cuenta_choices_for <- function(emp_name, moneda) {
        cts_raw <- tryCatch(
          if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
          else load_ctas_cuentas(),
          error = function(e) NULL
        )
        if (is.null(cts_raw) || !nrow(cts_raw)) return(c("Sin cuenta registrada" = ""))
        ini      <- names(shared$company_map())[unname(shared$company_map()) == emp_name]
        emp_keys <- unique(c(emp_name, ini))
        act <- dplyr::filter(cts_raw, activa == TRUE,
                             Empresa %in% emp_keys,
                             toupper(Moneda) == toupper(moneda))

        # Fallback: also include accounts whose alias starts with the empresa
        # initials — catches entries registered with a mismatched Empresa field
        # (e.g. NGFONDOAH stored as Empresa="" when COMPANY_MAP says "NG").
        if (length(ini) > 0) {
          ini_up <- toupper(ini[[1]])
          extra  <- dplyr::filter(cts_raw,
                                  activa == TRUE,
                                  toupper(Moneda) == toupper(moneda),
                                  !id %in% act$id,
                                  startsWith(toupper(trimws(alias)), ini_up))
          if (nrow(extra)) act <- dplyr::bind_rows(act, extra)
        }

        if (!nrow(act)) return(c("Sin cuenta registrada" = ""))
        # Resolve bank names for readable labels
        bnk     <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
        bnk_map <- setNames(bnk$nombre, bnk$id)
        setNames(act$id,
                 paste0(act$alias, " \u2014 ",
                        dplyr::coalesce(bnk_map[act$banco_id], "Banco")))
      }

      observeEvent(input[[paste0("confirm_ap_", emp)]], {
        s_emp <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur   <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur <- s_emp |> dplyr::filter(Moneda == cur)
        sel   <- input[[paste0("tbl_ap_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
        rows  <- s_cur[sel, ]
        if (!nrow(rows)) { showNotification("Selecciona facturas.", type = "warning"); return() }

        cuenta_choices <- .bancos_cuenta_choices_for(emp, cur)

        showModal(modalDialog(
          title = paste0("Confirmar pagos \u2014 ", emp), size = "m", easyClose = TRUE,
          footer = tagList(modalButton("Cancelar"),
            actionButton(ns(paste0("do_confirm_ap_", emp)),
              paste0("\u2713 Confirmar ", nrow(rows), " pago(s)"),
              class = "btn btn-success")),
          p(paste0(nrow(rows), " factura(s) \u2014 ", cur, " ",
                   fmt_money(sum(rows$Importe, na.rm = TRUE)))),
          div(class = "row g-2 mb-2",
            div(class = "col-md-7",
              tags$label("Cuenta bancaria", class = "form-label small"),
              selectInput(ns(paste0("conf_cuenta_ap_", emp)), NULL,
                          choices = cuenta_choices, width = "100%")
            ),
            div(class = "col-md-5",
              tags$label("Fecha de pago", class = "form-label small"),
              dateInput(ns(paste0("conf_fecha_ap_", emp)), NULL,
                        value = Sys.Date(), format = "dd/mm/yyyy",
                        language = "es", width = "100%")
            )
          ),
          tags$ul(lapply(seq_len(min(nrow(rows), 8)), function(i)
            tags$li(paste0(rows$Parte[i], " \u2014 ", fmt_money(rows$Importe[i])))))
        ))
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("do_confirm_ap_", emp)]], {
        s_emp      <- staged() |> dplyr::filter(Empresa == emp, ledger == "AP")
        cur        <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur      <- s_emp |> dplyr::filter(Moneda == cur)
        sel        <- input[[paste0("tbl_ap_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
        rows       <- s_cur[sel, ]; if (!nrow(rows)) return()
        cuenta_sel <- input[[paste0("conf_cuenta_ap_", emp)]] %||% NA_character_
        fecha_pago <- tryCatch(as.Date(input[[paste0("conf_fecha_ap_", emp)]]),
                               error = function(e) Sys.Date())

        # Remove from staged queue (mark "confirmed" by unstaging)
        ph <- unstage_pagar_hoy(shared$pagar_hoy_db(),
                dplyr::mutate(rows, ledger = "AP") |>
                  dplyr::select(ledger, Empresa, Moneda, Documento))
        shared$pagar_hoy_db(ph); save_pagar_hoy(ph)

        # Write to bancos_confirmados only (Historial de confirmaciones)
        # Does NOT write to bancos_movimientos — confirmed items must not
        # affect Libro de Banco balance calculations.
        if (!is.null(shared$bancos_confirmados)) {
          conf_db <- shared$bancos_confirmados() %||%
            .schema_bancos_confirmados()

          for (i in seq_len(nrow(rows))) {
            new_conf <- tibble::tibble(
              confirmacion_id = uuid::UUIDgenerate(),
              agenda_item_id  = as.character(rows$id[i]),
              empresa         = as.character(emp),
              parte           = as.character(rows$Parte[i]),
              documento       = as.character(rows$Documento[i]),
              codigo          = as.character(rows$Codigo[i] %||% NA_character_),
              importe         = rows$Importe[i],
              moneda          = as.character(cur),
              cuenta_id       = as.character(cuenta_sel %||% NA_character_),
              fecha           = as.Date(fecha_pago),
              tipo            = "pago",
              mov_id          = NA_character_,
              confirmado_at   = Sys.time(),
              eliminado       = FALSE
            )
            conf_db <- dplyr::bind_rows(conf_db, new_conf)
          }

          shared$bancos_confirmados(conf_db)
          tryCatch(
            save_bancos_confirmados(conf_db),
            error = function(e)
              showNotification(paste("Error S3 bancos:", e$message), type = "warning")
          )
        }

        # Also record in conciliacion for backward compat
        conc <- rows |> dplyr::mutate(
          id = purrr::map_chr(seq_len(dplyr::n()), ~uuid::UUIDgenerate()),
          tipo = "pago", FechaPago = fecha_pago,
          FechaContabilizacion = Sys.Date(),
          FechaVencimiento = FechaVenc,
          cuenta_id = cuenta_sel %||% NA_character_,
          comision = 0,
          notas = NA_character_,
          created_by = shared$current_user(), created_at = Sys.time()
        ) |> dplyr::select(id, tipo, Empresa, Parte, Documento, Moneda, Importe,
                            comision, FechaPago, FechaContabilizacion,
                            FechaVencimiento, cuenta_id, notas, created_by, created_at)
        new_conc <- dplyr::bind_rows(load_conciliacion(), conc)
        save_conciliacion(new_conc)
        if (!is.null(shared$conciliacion_rv))
          shared$conciliacion_rv(new_conc)

        # Remove confirmed manual entries from calendar (manual_inv)
        if (!is.null(shared$manual_inv)) {
          mi <- shared$manual_inv()
          if (!is.null(mi) && nrow(mi) && "id" %in% names(mi)) {
            manual_ids <- rows$id[rows$id %in% mi$id]
            if (length(manual_ids)) {
              mi_updated <- mi[!mi$id %in% manual_ids, , drop = FALSE]
              shared$manual_inv(mi_updated)
              tryCatch(save_manual(mi_updated),
                       error = function(e) showNotification(
                         paste("Error al eliminar entrada manual:", e$message), type = "warning"))
            }
          }
        }

        .link_counter(.link_counter() + 1L)
        removeModal()
        showNotification(
          paste0(nrow(rows), " pago(s) confirmados."),
          type = "message", duration = 3)
      }, ignoreInit = TRUE)

      # ── Confirm AR ─────────────────────────────────────────────────────────
      observeEvent(input[[paste0("confirm_ar_", emp)]], {
        s_emp <- staged() |> dplyr::filter(Empresa == emp, ledger == "AR")
        cur   <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur <- s_emp |> dplyr::filter(Moneda == cur)
        sel   <- input[[paste0("tbl_ar_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
        rows  <- s_cur[sel, ]
        if (!nrow(rows)) { showNotification("Selecciona facturas.", type = "warning"); return() }

        cuenta_choices <- .bancos_cuenta_choices_for(emp, cur)

        showModal(modalDialog(
          title = paste0("Confirmar cobros \u2014 ", emp), size = "m", easyClose = TRUE,
          footer = tagList(modalButton("Cancelar"),
            actionButton(ns(paste0("do_confirm_ar_", emp)),
              paste0("\u2713 Confirmar ", nrow(rows), " cobro(s)"),
              class = "btn btn-primary")),
          p(paste0(nrow(rows), " factura(s) \u2014 ", cur, " ",
                   fmt_money(sum(rows$Importe, na.rm = TRUE)))),
          div(class = "row g-2 mb-2",
            div(class = "col-md-7",
              tags$label("Cuenta bancaria", class = "form-label small"),
              selectInput(ns(paste0("conf_cuenta_ar_", emp)), NULL,
                          choices = cuenta_choices, width = "100%")
            ),
            div(class = "col-md-5",
              tags$label("Fecha de cobro", class = "form-label small"),
              dateInput(ns(paste0("conf_fecha_ar_", emp)), NULL,
                        value = Sys.Date(), format = "dd/mm/yyyy",
                        language = "es", width = "100%")
            )
          ),
          tags$ul(lapply(seq_len(min(nrow(rows), 8)), function(i)
            tags$li(paste0(rows$Parte[i], " \u2014 ", fmt_money(rows$Importe[i])))))
        ))
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("do_confirm_ar_", emp)]], {
        s_emp      <- staged() |> dplyr::filter(Empresa == emp, ledger == "AR")
        cur        <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        s_cur      <- s_emp |> dplyr::filter(Moneda == cur)
        sel        <- input[[paste0("tbl_ar_", emp, "_rows_selected")]] %||% seq_len(nrow(s_cur))
        rows       <- s_cur[sel, ]; if (!nrow(rows)) return()
        cuenta_sel <- input[[paste0("conf_cuenta_ar_", emp)]] %||% NA_character_
        fecha_cobro<- tryCatch(as.Date(input[[paste0("conf_fecha_ar_", emp)]]),
                               error = function(e) Sys.Date())

        ph <- unstage_pagar_hoy(shared$pagar_hoy_db(),
                dplyr::mutate(rows, ledger = "AR") |>
                  dplyr::select(ledger, Empresa, Moneda, Documento))
        shared$pagar_hoy_db(ph); save_pagar_hoy(ph)

        # Write to bancos_confirmados only (Historial de confirmaciones)
        # Does NOT write to bancos_movimientos — confirmed items must not
        # affect Libro de Banco balance calculations.
        if (!is.null(shared$bancos_confirmados)) {
          conf_db <- shared$bancos_confirmados() %||%
            .schema_bancos_confirmados()

          for (i in seq_len(nrow(rows))) {
            new_conf <- tibble::tibble(
              confirmacion_id = uuid::UUIDgenerate(),
              agenda_item_id  = as.character(rows$id[i]),
              empresa         = as.character(emp),
              parte           = as.character(rows$Parte[i]),
              documento       = as.character(rows$Documento[i]),
              codigo          = as.character(rows$Codigo[i] %||% NA_character_),
              importe         = rows$Importe[i],
              moneda          = as.character(cur),
              cuenta_id       = as.character(cuenta_sel %||% NA_character_),
              fecha           = as.Date(fecha_cobro),
              tipo            = "cobro",
              mov_id          = NA_character_,
              confirmado_at   = Sys.time(),
              eliminado       = FALSE
            )
            conf_db <- dplyr::bind_rows(conf_db, new_conf)
          }

          shared$bancos_confirmados(conf_db)
          tryCatch(
            save_bancos_confirmados(conf_db),
            error = function(e)
              showNotification(paste("Error S3 bancos:", e$message), type = "warning")
          )
        }

        conc <- rows |> dplyr::mutate(
          id = purrr::map_chr(seq_len(dplyr::n()), ~uuid::UUIDgenerate()),
          tipo = "cobro", FechaPago = fecha_cobro,
          FechaContabilizacion = Sys.Date(),
          FechaVencimiento = FechaVenc,
          cuenta_id = cuenta_sel %||% NA_character_,
          comision = 0,
          notas = NA_character_,
          created_by = shared$current_user(), created_at = Sys.time()
        ) |> dplyr::select(id, tipo, Empresa, Parte, Documento, Moneda, Importe,
                            comision, FechaPago, FechaContabilizacion,
                            FechaVencimiento, cuenta_id, notas, created_by, created_at)
        new_conc <- dplyr::bind_rows(load_conciliacion(), conc)
        save_conciliacion(new_conc)
        if (!is.null(shared$conciliacion_rv))
          shared$conciliacion_rv(new_conc)

        # Remove confirmed manual entries from calendar (manual_inv)
        if (!is.null(shared$manual_inv)) {
          mi <- shared$manual_inv()
          if (!is.null(mi) && nrow(mi) && "id" %in% names(mi)) {
            manual_ids <- rows$id[rows$id %in% mi$id]
            if (length(manual_ids)) {
              mi_updated <- mi[!mi$id %in% manual_ids, , drop = FALSE]
              shared$manual_inv(mi_updated)
              tryCatch(save_manual(mi_updated),
                       error = function(e) showNotification(
                         paste("Error al eliminar entrada manual:", e$message), type = "warning"))
            }
          }
        }

        .link_counter(.link_counter() + 1L)
        removeModal()
        showNotification(
          paste0(nrow(rows), " cobro(s) confirmados."),
          type = "message", duration = 3)
      }, ignoreInit = TRUE)

      # ── Interco map button ────────────────────────────────────────────────────
      observeEvent(input[[paste0("open_icmap_", emp)]], {
        cmap <- shared$company_map()
        ini  <- names(cmap)[cmap == emp]
        if (length(ini) && !is.null(shared$ic_map_target)) {
          shared$ic_map_target(list(ini = ini[1], nonce = as.numeric(Sys.time())))
        }
        shinyjs::runjs("
          var el = document.querySelector('[data-value=\"IC\"]');
          if (el) el.click();
        ")
      }, ignoreInit = TRUE)

        }) # end local
      }   # end for
    })    # end observe (per-empresa guard)

    # ── Supplier lookup & link modal ─────────────────────────────────────────
    # ── TIER GATE ────────────────────────────────────────────────────────────
    # Linking/unlinking suppliers to SAP Parte names is restricted to users
    # with tier "dev" or "admin". Finance-tier users see suggestions read-only.
    # TO CHANGE PERMISSIONS: edit the .can_link() helper below.
    # Tiers currently in use: "dev", "finance". Add "admin" when needed.
    .can_link <- function() {
      # ── PERMISSION CONTROL ───────────────────────────────────────────────────
      # Currently open to ALL logged-in users.
      # To restrict to specific tiers when needed, replace TRUE with:
      #   tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "finance")
      #   tier %in% c("dev", "admin")
      # Valid tiers in this app: "dev" (full access), "finance" (standard user)
      # ────────────────────────────────────────────────────────────────────────
      TRUE
    }

    # Reactive: current search query for the lookup modal
    lookup_search_rv <- reactiveVal("")
    lookup_parte_rv  <- reactiveVal("")
    lookup_codigo_rv <- reactiveVal("")

    observeEvent(input$ph_lookup_parte, {
      click    <- input$ph_lookup_parte
      q_parte  <- trimws(if (is.list(click)) click$parte %||% "" else click %||% "")
      q_codigo <- trimws(if (is.list(click)) click$codigo %||% "" else "")
      if (!nzchar(q_parte)) return()
      lookup_parte_rv(q_parte)
      lookup_codigo_rv(q_codigo)
      lookup_search_rv("")
      .render_lookup_modal(q_parte, "")
    }, ignoreInit = TRUE)

    # Update search rv — results are rendered by output$ph_lookup_results (renderUI).
    # Never call showModal() here: recreating the modal on every keystroke causes
    # Shiny to re-initialize the textInput, which fires this observer again → loop.
    observeEvent(input$ph_lookup_search, {
      lookup_search_rv(trimws(input$ph_lookup_search %||% ""))
    }, ignoreInit = TRUE)

    # Reactive search results — rendered inside the modal without rebuilding it.
    output$ph_lookup_results <- renderUI({
      search_txt <- lookup_search_rv()
      q_parte    <- lookup_parte_rv()
      if (!nzchar(q_parte) || !nzchar(search_txt)) return(NULL)

      can_link <- .can_link()
      provs <- tryCatch(
        if (!is.null(shared$proveedores_db)) shared$proveedores_db()
        else load_proveedores(),
        error = function(e) NULL
      )
      if (is.null(provs)) return(NULL)

      s   <- tolower(search_txt)
      act <- provs[!is.na(provs$activo) & provs$activo == TRUE, ]
      hits <- act[
        grepl(s, tolower(act$nombre    %||% ""), fixed = TRUE) |
        grepl(s, tolower(act$alias     %||% ""), fixed = TRUE) |
        grepl(s, tolower(act$rfc       %||% ""), fixed = TRUE) |
        grepl(s, tolower(act$no_cuenta %||% ""), fixed = TRUE),
      ]

      if (nrow(hits) == 0)
        return(tags$p(class = "small text-muted fst-italic", "Sin resultados."))

      mk_link_btn <- function(prov_row, is_linked = FALSE) {
        ali <- prov_row$alias %||% ""
        if (can_link) {
          if (is_linked)
            tags$button(class = "btn btn-sm btn-outline-danger ph-unlink-btn ms-auto",
                        `data-parte` = q_parte, `data-emp` = "", "Desvincular")
          else
            tags$button(class = "btn btn-sm btn-success ph-link-btn ms-auto",
                        `data-parte` = q_parte, `data-alias` = ali, `data-emp` = "",
                        "Vincular")
        } else {
          tags$span(class = "text-muted small ms-auto fst-italic", "Solo lectura")
        }
      }

      tagList(
        tags$p(class = "small text-muted mb-2",
               paste0(nrow(hits), " resultado(s):")),
        lapply(seq_len(min(nrow(hits), 8L)), function(i) {
          pr    <- hits[i, ]
          nom   <- pr$nombre    %||% ""
          ali   <- pr$alias     %||% ""
          rfc   <- pr$rfc       %||% "—"
          banco <- pr$banco %||% pr$banco_destino %||% "—"
          nc    <- pr$no_cuenta %||% pr$clabe %||% "—"
          div(class = "border rounded p-2 mb-2 bg-light",
            div(class = "d-flex align-items-start gap-2",
              div(class = "flex-grow-1",
                div(class = "fw-semibold small", nom,
                  if (nzchar(ali))
                    tags$span(class = "badge bg-secondary ms-1 fw-normal", ali)
                  else NULL
                ),
                div(class = "text-muted small",
                  paste0("RFC: ", rfc, "  •  Banco: ", banco),
                  if (nzchar(trimws(nc)) && nc != "—")
                    paste0("  •  Cta: ", nc)
                  else NULL
                )
              ),
              mk_link_btn(pr)
            )
          )
        })
      )
    })

    # Helper: build and show the lookup modal
    .render_lookup_modal <- function(q_parte, search_txt) {
      can_link  <- .can_link()
      provs     <- tryCatch(
        if (!is.null(shared$proveedores_db)) shared$proveedores_db()
        else load_proveedores(),
        error = function(e) NULL
      )
      override_map <- tryCatch(
        if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db()
        else load_parte_alias_map(),
        error = function(e) NULL
      )

      # Current link for this Parte
      current_link <- NULL
      if (!is.null(override_map) && nrow(override_map)) {
        hit <- override_map[toupper(trimws(override_map$Parte)) == toupper(q_parte), ]
        if (nrow(hit)) current_link <- hit[1, ]
      }

      # Auto-suggestions (fuzzy, threshold=15 for more results)
      auto_matches <- if (!is.null(provs) && nzchar(q_parte)) {
        tryCatch(
          find_proveedor_matches(
            query          = list(parte = q_parte, rfc = "", no_cuenta = "",
                                  alias = lookup_codigo_rv()),
            proveedores_df = provs,
            threshold      = 15L,
            top_n          = 5L
          ),
          error = function(e) NULL
        )
      } else NULL

      # Search results are rendered reactively by output$ph_lookup_results.

      # Helper: build one result card
      .result_card <- function(prov_row, score = NA, is_linked = FALSE) {
        nom   <- prov_row$nombre %||% ""
        ali   <- prov_row$alias  %||% ""
        rfc   <- prov_row$rfc    %||% "—"
        banco <- prov_row$banco  %||% prov_row$banco_destino %||% "—"
        nc    <- prov_row$no_cuenta %||% prov_row$clabe %||% "—"

        link_btn <- if (can_link) {
          if (is_linked) {
            tags$button(
              class    = "btn btn-sm btn-outline-danger ph-unlink-btn ms-auto",
              `data-parte` = q_parte,
              `data-emp`   = "",
              "Desvincular"
            )
          } else {
            tags$button(
              class    = "btn btn-sm btn-success ph-link-btn ms-auto",
              `data-parte` = q_parte,
              `data-alias` = ali,
              `data-emp`   = "",
              "Vincular"
            )
          }
        } else {
          tags$span(class = "text-muted small ms-auto fst-italic", "Solo lectura")
        }

        div(class = paste("border rounded p-2 mb-2",
                          if (is_linked) "border-success bg-success-subtle"
                          else "bg-light"),
          div(class = "d-flex align-items-start gap-2",
            div(class = "flex-grow-1",
              div(class = "fw-semibold small", nom,
                if (nzchar(ali))
                  tags$span(class = "badge bg-secondary ms-1 fw-normal", ali)
                else NULL,
                if (is_linked)
                  tags$span(class = "badge bg-success ms-1", "\u2713 Vinculado")
                else NULL
              ),
              div(class = "text-muted small",
                paste0("RFC: ", rfc, "  \u2022  Banco: ", banco),
                if (nzchar(trimws(nc)) && nc != "—")
                  paste0("  \u2022  Cta: ", nc)
                else NULL
              ),
              if (!is.na(score))
                tags$span(class = "badge bg-info-subtle text-info border border-info-subtle small",
                          paste0("score: ", score))
              else NULL
            ),
            link_btn
          )
        )
      }

      # Build modal body
      current_link_ui <- if (!is.null(current_link)) {
        prov_row <- if (!is.null(provs) && nrow(provs)) {
          hit <- provs[toupper(trimws(provs$alias)) == toupper(trimws(current_link$alias)), ]
          if (nrow(hit)) hit[1, ] else NULL
        } else NULL
        if (!is.null(prov_row)) {
          tagList(
            tags$h6(class = "fw-semibold text-success mb-2",
                    tagList(icon("link"), " Vínculo actual")),
            .result_card(prov_row, score = NA, is_linked = TRUE),
            tags$hr()
          )
        } else NULL
      } else NULL

      auto_ui <- if (!is.null(auto_matches) && nrow(auto_matches)) {
        tagList(
          tags$h6(class = "fw-semibold text-muted mb-2",
                  paste0("Sugerencias automáticas (", nrow(auto_matches), ")")),
          lapply(seq_len(nrow(auto_matches)), function(i) {
            .result_card(auto_matches[i, ], score = auto_matches$.score[i])
          })
        )
      } else {
        tags$p(class = "small text-muted fst-italic",
               "Sin sugerencias automáticas para este proveedor.")
      }

      search_ui <- tagList(
        tags$hr(),
        tags$h6(class = "fw-semibold text-muted mb-2",
                tagList(icon("magnifying-glass"), " Buscar en catálogo activo")),
        textInput(NS(id, "ph_lookup_search"),
                  NULL,
                  value       = "",
                  placeholder = "Nombre, alias, RFC, No. Cuenta...",
                  width       = "100%"),
        uiOutput(NS(id, "ph_lookup_results"))
      )

      tier_note <- if (!can_link) {
        div(class = "alert alert-secondary small py-1 px-2 mb-2",
            icon("lock"), " Vinculación disponible para administradores.")
      } else NULL

      showModal(modalDialog(
        title     = tagList(icon("link"), paste0(" Vincular proveedor: ", q_parte)),
        size      = "m",
        easyClose = TRUE,
        footer    = modalButton("Cerrar"),
        div(style = "max-height: 60vh; overflow-y: auto;",
          tier_note,
          current_link_ui,
          auto_ui,
          search_ui
        )
      ))
    }

    # ── Link supplier observer ────────────────────────────────────────────────
    observeEvent(input$ph_link_supplier, {
      req(.can_link())
      click  <- input$ph_link_supplier
      parte  <- trimws(click$parte %||% "")
      ali    <- trimws(click$alias %||% "")
      emp    <- trimws(click$emp   %||% "")
      if (!nzchar(parte) || !nzchar(ali)) return()

      am  <- tryCatch(
        if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db()
        else load_parte_alias_map(),
        error = function(e) load_parte_alias_map()
      )

      # Upsert: remove existing entry for this Parte+Empresa, add new one
      am <- am[!(toupper(trimws(am$Parte)) == toupper(parte) &
                 (am$Empresa == emp | (emp == "" & (is.na(am$Empresa) | am$Empresa == "")))),
               , drop = FALSE]
      am <- dplyr::bind_rows(am, tibble::tibble(
        Parte     = parte,
        Empresa   = emp,
        alias     = ali,
        linked_by = tryCatch(shared$current_user(), error = function(e) "unknown"),
        linked_at = as.character(Sys.time())
      ))

      save_parte_alias_map(am)
      if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db(am)

      # Invalidate annotation cache for this parte so it re-annotates immediately
      rm(list = ls(.annot_cache), envir = .annot_cache)
      .link_counter(.link_counter() + 1L)

      showNotification(
        paste0("\u2713 Vinculado: \"", parte, "\" \u2192 ", ali),
        type = "message", duration = 3)
      removeModal()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Unlink supplier observer ──────────────────────────────────────────────
    observeEvent(input$ph_unlink_supplier, {
      req(.can_link())
      click <- input$ph_unlink_supplier
      parte <- trimws(click$parte %||% "")
      emp   <- trimws(click$emp   %||% "")
      if (!nzchar(parte)) return()

      am <- tryCatch(
        if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db()
        else load_parte_alias_map(),
        error = function(e) load_parte_alias_map()
      )
      am <- am[!(toupper(trimws(am$Parte)) == toupper(parte) &
                 (am$Empresa == emp |
                  (emp == "" & (is.na(am$Empresa) | am$Empresa == "")))),
               , drop = FALSE]

      save_parte_alias_map(am)
      if (!is.null(shared$parte_alias_map_db)) shared$parte_alias_map_db(am)
      rm(list = ls(.annot_cache), envir = .annot_cache)
      .link_counter(.link_counter() + 1L)

      showNotification(paste0("Vínculo eliminado para: ", parte),
                       type = "message", duration = 3)
      removeModal()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Cycle alias observer ──────────────────────────────────────────────────
    observeEvent(input$ph_cycle_alias, {
      click <- input$ph_cycle_alias
      req(click)
      parte       <- toupper(trimws(click$parte         %||% ""))
      emp         <- trimws(click$emp                   %||% "")
      cur_alias   <- toupper(trimws(click$current_alias %||% ""))
      if (!nzchar(parte)) return()

      # Get all active catalog entries for this supplier via word-overlap on nombre.
      # Extract significant words (4+ chars) from the SAP Parte name, then require
      # >=75% of those words to appear in the catalog nombre. This handles:
      #   - Capitalization differences (all uppercased before comparison)
      #   - Missing legal suffixes ("SA", "DE", "CV" are 2-3 chars, filtered out)
      #   - Multiple accounts for the same supplier (same words, different alias)
      # The empresa filter is intentionally omitted — word overlap is the guard.
      provs <- tryCatch(provs_db(), error = function(e) NULL)
      if (is.null(provs) || !nrow(provs)) return()

      sig_words  <- unique(Filter(function(w) nchar(w) >= 4L, strsplit(parte, "\\s+")[[1]]))
      n_required <- max(1L, ceiling(length(sig_words) * 0.75))

      emp_provs <- provs |>
        dplyr::filter(!is.na(alias) & nzchar(trimws(alias))) |>
        dplyr::filter(sapply(toupper(trimws(nombre)), function(nm) {
          sum(sig_words %in% strsplit(nm, "\\s+")[[1]]) >= n_required
        }))

      if (!nrow(emp_provs)) return()

      # Deduplicate by alias+clabe so identical catalog entries (same payment code
      # AND same CLABE registered under multiple empresas) don't trap the cycle
      # in an infinite loop between two indistinguishable rows.
      emp_provs <- emp_provs |>
        dplyr::mutate(.clabe_key = trimws(clabe %||% "")) |>
        dplyr::distinct(alias, .clabe_key, .keep_all = TRUE) |>
        dplyr::select(-.clabe_key)

      # Cycle: find current position using alias+CLABE compound key for uniqueness
      cur_clabe <- toupper(trimws(click$current_clabe %||% ""))
      aliases   <- trimws(emp_provs$alias)
      clabes    <- trimws(emp_provs$clabe %||% rep("", nrow(emp_provs)))

      cur_pos <- if (nzchar(cur_clabe)) {
        compound <- paste0(toupper(trimws(aliases)), "|", clabes)
        m <- match(paste0(cur_alias, "|", cur_clabe), compound)
        if (is.na(m)) match(cur_alias, toupper(trimws(aliases))) else m
      } else {
        match(cur_alias, toupper(trimws(aliases)))
      }
      next_pos   <- if (is.na(cur_pos) || cur_pos >= length(aliases)) 1L else cur_pos + 1L
      next_alias <- aliases[next_pos]
      next_clabe <- clabes[next_pos]

      # Store compound key "alias|clabe" so Pass -1 can pick the exact row
      key <- paste0(emp, "|", parte)
      ovr <- .session_alias_overrides()
      ovr[[key]] <- paste0(next_alias, "|", next_clabe)
      .session_alias_overrides(ovr)

      # Clear annotation cache and bump counter so renderDT re-fires immediately
      rm(list = ls(.annot_cache), envir = .annot_cache)
      .link_counter(.link_counter() + 1L)

      showNotification(
        paste0("\u21c4 ", click$parte, ": ", cur_alias, " \u2192 ", toupper(next_alias),
               if (nzchar(next_clabe)) paste0(" (", substr(next_clabe, nchar(next_clabe)-3, nchar(next_clabe)), ")") else ""),
        type = "message", duration = 2
      )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Download all ZIP ─────────────────────────────────────────────────────
    output$download_all <- downloadHandler(
      filename = function() paste0("pagos_todos_", format(Sys.Date(), "%d%m%Y"), ".zip"),
      content  = function(file) {
        s <- staged() |> dplyr::filter(ledger == "AP")
        if (!nrow(s)) { writeLines("", file); return() }
        tmpdir <- tempfile(); dir.create(tmpdir)
        on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
        fnames <- unlist(lapply(unique(s$Empresa), function(emp)
          lapply(unique(dplyr::filter(s, Empresa == emp)$Moneda), function(cur) {
            rows     <- s |> dplyr::filter(Empresa == emp, Moneda == cur)
            ann      <- .annotate_ap_cached(rows, emp) |> dplyr::filter(ppl_status == "ok")
            if (!nrow(ann)) return(NULL)
            cuenta_origen <- get_cuenta_ppl(emp, cur, shared)
            sap_ap_z   <- tryCatch(shared$sap_data()[["AP"]], error = function(e) NULL)
            ref_lkup_z <- if (!is.null(sap_ap_z) && "Factura" %in% names(sap_ap_z) &&
                              "Documento" %in% names(sap_ap_z)) {
              setNames(
                trimws(as.character(sap_ap_z[["Factura"]] %||% "")),
                trimws(as.character(sap_ap_z[["Documento"]]))
              )
            } else character(0)
            man_inv_z <- tryCatch(shared$manual_inv(), error = function(e) NULL)
            if (!is.null(man_inv_z) && nrow(man_inv_z) &&
                all(c("Documento", "Factura", "ledger") %in% names(man_inv_z))) {
              man_ap_z <- man_inv_z[man_inv_z$ledger == "AP", , drop = FALSE]
              if (nrow(man_ap_z)) {
                man_lkup_z <- setNames(trimws(as.character(man_ap_z[["Factura"]]   %||% "")),
                                       trimws(as.character(man_ap_z[["Documento"]])))
                man_lkup_z <- man_lkup_z[nzchar(names(man_lkup_z))]
                ref_lkup_z[names(man_lkup_z)] <- man_lkup_z
              }
            }

            ppl_rows <- ann |> dplyr::transmute(
              alias = alias, clabe_dest = clabe_dest,
              banco_dest = banco_dest,
              medio_pago = medio_pago, importe = Importe,
              concepto = {
                doc <- trimws(as.character(Documento))
                ref <- if (length(ref_lkup_z)) as.character(ref_lkup_z[doc]) else rep("", length(doc))
                ref[is.na(ref)] <- ""
                substr(ifelse(nzchar(ref), ref, doc), 1, 75)
              })
            fname <- file.path(tmpdir,
              paste0("pagos_", emp, "_", cur, "_", format(Sys.Date(), "%d%m%Y"), "_001.txt"))
            write_ppl(generate_ppl(ppl_rows, cuenta_origen, Sys.Date(), 1L), fname)
            fname
          })))
        fnames <- Filter(Negate(is.null), fnames)
        if (length(fnames)) zip(file, unlist(fnames), flags = "-j")
        else writeLines("", file)
      }
    )

    # ── Clear all ────────────────────────────────────────────────────────────
    observeEvent(input$clear_all, {
      showModal(modalDialog(
        title = "\u00bfVaciar toda la agenda?", size = "s", easyClose = TRUE,
        footer = tagList(modalButton("Cancelar"),
          actionButton(ns("do_clear_all"), "S\u00ed, vaciar", class = "btn btn-danger")),
        "Esto quitar\u00e1 todos los pagos y cobros de la agenda de hoy."
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$do_clear_all, {
      ph <- shared$pagar_hoy_db() |> dplyr::filter(status != "pending")
      shared$pagar_hoy_db(ph); save_pagar_hoy(ph)
      removeModal()
      showNotification("Agenda vaciada.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    # ── Load opening balances from Bancos module ─────────────────────────────
    observeEvent(input$load_bancos_saldos, {
      # ctas_cuentas is the Settings bank-account registry — single source of
      # truth used by the bancos Libro de Banco (NOT bancos_cuentas_db which is
      # a separate, empty object from bancos_persistence.R).
      # Columns: id, Empresa (initials), Moneda, saldo_inicial, activa, …
      # Movements: cuenta_id maps to ctas_cuentas$id.
      ctas <- tryCatch(
        if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
        else load_ctas_cuentas(),
        error = function(e) NULL
      )
      movs <- tryCatch(shared$bancos_movimientos(), error = function(e) NULL)

      if (is.null(ctas) || !nrow(ctas)) {
        showNotification(
          "No hay cuentas registradas en Configuración > Cuentas.",
          type = "warning", duration = 4)
        return()
      }

      cts_active <- ctas[!is.na(ctas$activa) & ctas$activa == TRUE, , drop = FALSE]
      if (!nrow(cts_active)) {
        showNotification("No hay cuentas activas en Configuración > Cuentas.",
                         type = "warning")
        return()
      }

      # Compute running balance per account.
      # Mirror bancos_module movs_active: exclude eliminado=TRUE and fuente="agenda".
      # Include saldo_inicial from ctas_cuentas (opening balance set in Settings).
      # Balance = saldo_inicial + sum(abono) - sum(cargo) from active movements.
      saldos_por_cuenta <- list()
      for (i in seq_len(nrow(cts_active))) {
        ct <- cts_active[i, ]
        saldo_ini <- as.numeric(ct$saldo_inicial %||% 0)
        if (is.na(saldo_ini)) saldo_ini <- 0
        if (!is.null(movs) && nrow(movs)) {
          movs_ct <- movs[
            !is.na(movs$cuenta_id) & movs$cuenta_id == ct$id &
            !is.na(movs$eliminado) & movs$eliminado == FALSE &
            (is.na(movs$fuente) | movs$fuente != "agenda"), , drop = FALSE]
          total_cargo <- sum(movs_ct$cargo, na.rm = TRUE)
          total_abono <- sum(movs_ct$abono, na.rm = TRUE)
        } else {
          total_cargo <- 0; total_abono <- 0
        }
        saldo <- saldo_ini + total_abono - total_cargo

        emp_raw <- trimws(ct$Empresa %||% "")
        # ctas_cuentas stores initials (e.g. "NCS") — resolve to full name
        # COMPANY_MAP: c(NCS = "Networks Crossdocking Services", ...)
        cmap_load <- tryCatch(shared$company_map(), error = function(e) COMPANY_MAP)
        emp <- if (emp_raw %in% names(cmap_load)) {
          cmap_load[[emp_raw]]
        } else if (emp_raw %in% unname(cmap_load)) {
          emp_raw  # already a full name
        } else {
          ""  # unrecognised — skip
        }

        mon <- toupper(trimws(ct$Moneda %||% "MXN"))
        mon <- dplyr::case_when(
          grepl("peso",  mon, ignore.case = TRUE)              ~ "MXN",
          grepl("d.lar", mon, ignore.case = TRUE, perl = TRUE) ~ "USD",
          mon %in% c("MXN", "USD")                             ~ mon,
          TRUE                                                  ~ "MXN"
        )

        if (nzchar(emp) && emp %in% all_companies_rv()) {
          key <- paste0(emp, "_", mon)
          saldos_por_cuenta[[key]] <- (saldos_por_cuenta[[key]] %||% 0) + saldo
        }
      }

      # Write into saldos_apertura
      sa        <- saldos_apertura()
      n_updated <- 0L
      for (emp in all_companies_rv()) {
        for (mon in c("MXN", "USD")) {
          key <- paste0(emp, "_", mon)
          if (!is.null(saldos_por_cuenta[[key]])) {
            sa[[emp]][[mon]] <- saldos_por_cuenta[[key]]
            n_updated <- n_updated + 1L
          }
        }
      }
      saldos_apertura(sa)

      # Refresh visible numericInputs
      for (emp in all_companies_rv()) {
        cur <- input[[paste0("bal_cur_", emp)]] %||% "MXN"
        updateNumericInput(session, paste0("bal_", emp),
                           value = sa[[emp]][[cur]] %||% 0)
      }

      showNotification(
        paste0("Saldos cargados desde Bancos: ", n_updated, " cuenta(s) actualizada(s)."),
        type = "message", duration = 3)
    }, ignoreInit = TRUE)

  }) # end moduleServer
}

# get_cuenta_ppl() is now defined in settings_module.R and handles all lookup logic.

# ── Schema helper (for empty fallback) ────────────────────────────────────────
.schema_proveedores <- function() tibble::tibble(
  id = character(), Empresa = character(), codigo = character(),
  nombre = character(), alias = character(), clabe = character(),
  medio_pago = character(), rfc = character(), activo = logical()
)