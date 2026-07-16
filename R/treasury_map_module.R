# =============================================================================
# R/treasury_map_module.R — Treasury Cash Flow Map
# =============================================================================
# Visualizes intercompany cash flow as a directed graph.
# Nodes = group companies (with bank balances); Edges = IC AR balances.
# AR invoice (Empresa X, Parte Y) → Y owes X → edge Y→X.
# Sub-module called from interco_module.R.
# =============================================================================

# ── Helpers ───────────────────────────────────────────────────────────────────

# Match SAP Parte display names → company initials (case-insensitive, partial).
.tm_parte_to_ini <- function(parte_vec, cmap = COMPANY_MAP) {
  inv_map <- setNames(names(cmap), unname(cmap))
  lc_keys <- tolower(names(inv_map))

  vapply(parte_vec, function(p) {
    lp <- tolower(trimws(p %||% ""))
    if (!nzchar(lp)) return(NA_character_)
    idx <- which(lc_keys == lp)
    if (length(idx)) return(inv_map[[idx[1]]])
    idx2 <- which(startsWith(lp, lc_keys) | startsWith(lc_keys, lp))
    if (length(idx2)) return(inv_map[[idx2[1]]])
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Bank balance per company per currency.
# Returns named list: result[[ini]][[currency]] = numeric (NA if no data).
.tm_bank_balances <- function(shared) {
  cmap   <- tryCatch(shared$company_map(), error = function(e) COMPANY_MAP)
  result <- setNames(
    lapply(names(cmap), function(ini) list(MXN = NA_real_, USD = NA_real_)),
    names(cmap)
  )

  ctas <- tryCatch(
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else NULL,
    error = function(e) NULL
  )
  if (is.null(ctas) || !nrow(ctas)) return(result)

  cts_active <- ctas[!is.na(ctas$activa) & ctas$activa == TRUE, , drop = FALSE]
  if (!nrow(cts_active)) return(result)

  movs <- tryCatch(shared$bancos_movimientos(), error = function(e) NULL)

  for (i in seq_len(nrow(cts_active))) {
    ct <- cts_active[i, ]

    emp_raw <- trimws(ct$Empresa %||% "")
    ini <- if (emp_raw %in% names(cmap)) emp_raw
           else if (emp_raw %in% unname(cmap)) names(cmap)[cmap == emp_raw][1]
           else NA_character_
    if (is.na(ini)) next

    mon_raw <- toupper(trimws(ct$Moneda %||% "MXN"))
    mon <- dplyr::case_when(
      grepl("peso",  mon_raw, ignore.case = TRUE)              ~ "MXN",
      grepl("d.lar", mon_raw, ignore.case = TRUE, perl = TRUE) ~ "USD",
      mon_raw %in% c("MXN", "USD")                             ~ mon_raw,
      TRUE                                                      ~ "MXN"
    )

    saldo_ini <- as.numeric(ct$saldo_inicial %||% 0)
    if (is.na(saldo_ini)) saldo_ini <- 0

    if (!is.null(movs) && nrow(movs)) {
      movs_ct <- movs[
        !is.na(movs$cuenta_id) & movs$cuenta_id == ct$id &
        !is.na(movs$eliminado) & !movs$eliminado &
        (is.na(movs$fuente) | movs$fuente != "agenda"), , drop = FALSE]
      saldo <- saldo_ini + sum(movs_ct$abono, na.rm = TRUE) - sum(movs_ct$cargo, na.rm = TRUE)
    } else {
      saldo <- saldo_ini
    }

    cur_val <- result[[ini]][[mon]] %||% NA_real_
    result[[ini]][[mon]] <- if (is.na(cur_val)) saldo else cur_val + saldo
  }
  result
}

# BFS: all paths from any node → target, max max_hops hops.
# edges_df: data.frame(from, to) with company initials.
# Returns list of character vectors (each a path, e.g. c("NCS","NL","NTS")).
.tm_bfs_paths <- function(edges_df, target_ini, max_hops = 3) {
  if (is.null(edges_df) || !nrow(edges_df)) return(list())

  all_nodes <- unique(c(edges_df$from, edges_df$to))
  adj <- setNames(vector("list", length(all_nodes)), all_nodes)
  for (n in all_nodes) adj[[n]] <- character()
  for (i in seq_len(nrow(edges_df))) {
    f <- edges_df$from[i]; t <- edges_df$to[i]
    if (!is.na(f) && !is.na(t) && f != t)
      adj[[f]] <- unique(c(adj[[f]], t))
  }

  paths <- list()
  for (src in setdiff(all_nodes, target_ini)) {
    queue <- list(src)
    while (length(queue) > 0) {
      path  <- queue[[1]]; queue <- queue[-1]
      last  <- path[length(path)]
      if (last == target_ini) {
        if (length(path) > 1) paths <- c(paths, list(path))
        next
      }
      if (length(path) > max_hops) next
      for (nxt in (adj[[last]] %||% character())) {
        if (!nxt %in% path) queue <- c(queue, list(c(path, nxt)))
      }
    }
  }
  paths
}

.tm_fmt <- function(x, cur = "MXN") {
  if (is.na(x)) return("\u2014")
  paste0(cur, "\u00a0", format(round(abs(x)), big.mark = ",", scientific = FALSE))
}

# SVG node image with gradient fill — base64-encoded data URI.
# vis.js renders on canvas (not CSS), so gradients require SVG embedding.
# Square canvas (sz×sz) so vis.js aspect-ratio scaling doesn't distort the ellipse.
.tm_node_svg <- function(ini, bal_lbl, is_tgt, bv) {
  sz <- 120L; cx <- 60L; cy <- 60L
  rx <- 56L;  ry <- 24L   # wide, flat ellipse inside square canvas

  # ── Color scheme ───────────────────────────────────────────────────────────
  if (is_tgt) {
    g1 <- "#4a6fa5"; g2 <- "#1a2e5a"   # Reporte navy: steel-blue → deep navy
    fc <- "#ffffff";  sc <- "#0f1e3d"
    tc <- "rgba(255,255,255,0.68)"
  } else if (!is.na(bv) && bv > 0) {
    g1 <- "#6ee7b7"; g2 <- "#059669"   # emerald: mint → deep green
    fc <- "#064e3b";  sc <- "#047857"
    tc <- "#065f46"
  } else if (!is.na(bv) && bv <= 0) {
    g1 <- "#fda4af"; g2 <- "#be123c"   # rose: blush → deep rose
    fc <- "#4c0519";  sc <- "#9f1239"
    tc <- "#881337"
  } else {
    g1 <- "#e2e8f0"; g2 <- "#94a3b8"   # slate: light → medium
    fc <- "#1e293b";  sc <- "#64748b"
    tc <- "#475569"
  }

  uid <- ini   # company initials are unique alphanumeric — safe as SVG id suffix

  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="', sz, '" height="', sz, '">',
    '<defs>',
      '<linearGradient id="g', uid, '" x1="15%" y1="0%" x2="85%" y2="100%">',
        '<stop offset="0%" stop-color="', g1, '"/>',
        '<stop offset="100%" stop-color="', g2, '"/>',
      '</linearGradient>',
      '<filter id="sh', uid, '" x="-18%" y="-30%" width="136%" height="160%">',
        '<feDropShadow dx="0" dy="2" stdDeviation="3.5" flood-opacity="0.22"/>',
      '</filter>',
    '</defs>',
    '<ellipse cx="', cx, '" cy="', cy, '" rx="', rx, '" ry="', ry, '"',
      ' fill="url(#g', uid, ')" stroke="', sc, '" stroke-width="2"',
      ' filter="url(#sh', uid, ')"/>',
    # Company initials — bold, larger
    '<text x="', cx, '" y="', cy - 5L, '"',
      ' text-anchor="middle" dominant-baseline="middle"',
      ' font-family="system-ui,-apple-system,BlinkMacSystemFont,sans-serif"',
      ' font-size="15" font-weight="700" letter-spacing="0.4"',
      ' fill="', fc, '">', htmltools::htmlEscape(ini), '</text>',
    # Balance label — smaller, lighter
    '<text x="', cx, '" y="', cy + 11L, '"',
      ' text-anchor="middle" dominant-baseline="middle"',
      ' font-family="system-ui,-apple-system,BlinkMacSystemFont,sans-serif"',
      ' font-size="9.5" font-weight="500"',
      ' fill="', tc, '">', htmltools::htmlEscape(bal_lbl), '</text>',
    '</svg>'
  )

  paste0("data:image/svg+xml;base64,",
         base64enc::base64encode(charToRaw(enc2utf8(svg))))
}

# =============================================================================
# UI
# =============================================================================

treasuryMapUI <- function(id) {
  ns <- NS(id)
  div(
    class = "tm-wrap",
    tags$style(HTML("
      /* ── Outer container ── */
      .tm-wrap { padding: 4px 0 12px; }

      /* ── Controls bar ── */
      .tm-controls {
        display: flex;
        gap: 12px;
        align-items: flex-end;
        margin-bottom: 12px;
        flex-wrap: wrap;
      }
      .tm-controls .form-label {
        font-size: .75rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: .04em;
        color: #6b7280;
        margin-bottom: 4px;
      }

      /* ── Graph card ── */
      .tm-graph-card {
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        overflow: hidden;
        background: #ffffff;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
      }
      .tm-graph-card .vis-network canvas {
        background: transparent !important;
      }

      /* ── Bottom panels row ── */
      .tm-panels-row {
        display: flex;
        gap: 12px;
        margin-top: 12px;
      }

      /* ── Generic panel card ── */
      .tm-panel {
        flex: 1;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        background: #fff;
        box-shadow: 0 1px 4px rgba(0,0,0,.05);
        min-width: 0;
        overflow: hidden;
      }
      .tm-panel-header {
        background: #f9fafb;
        border-bottom: 1px solid #e5e7eb;
        padding: 9px 14px 8px;
        display: flex;
        align-items: center;
        gap: 7px;
      }
      .tm-panel-header .tm-panel-title {
        font-size: .82rem;
        font-weight: 700;
        color: #374151;
        letter-spacing: .01em;
        margin: 0;
      }
      .tm-panel-body { padding: 10px 14px 12px; }

      /* ── Route rows ── */
      .tm-route {
        display: flex;
        align-items: center;
        gap: 6px;
        padding: 5px 0;
        border-bottom: 1px solid #f3f4f6;
        font-size: .82rem;
      }
      .tm-route:last-child { border-bottom: none; }
      .tm-route-path { font-weight: 600; color: #1f2937; }
      .tm-route-amt  { font-size: .75rem; color: #9ca3af; margin-left: auto; }

      /* ── Hop badges ── */
      .tm-badge-direct { background:#d1fae5; color:#065f46; border:1px solid #6ee7b7; font-size:.7rem; padding:1px 7px; border-radius:99px; font-weight:700; }
      .tm-badge-2      { background:#fef9c3; color:#854d0e; border:1px solid #fde047; font-size:.7rem; padding:1px 7px; border-radius:99px; font-weight:700; }
      .tm-badge-3      { background:#fee2e2; color:#991b1b; border:1px solid #fca5a5; font-size:.7rem; padding:1px 7px; border-radius:99px; font-weight:700; }

      /* ── Plan items ── */
      .tm-plan-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 4px 0;
        border-bottom: 1px solid #f3f4f6;
        font-size: .81rem;
        color: #374151;
      }
      .tm-plan-item:last-child { border-bottom: none; }
      .tm-plan-item .tm-plan-amt { font-variant-numeric: tabular-nums; color: #6b7280; }
      .tm-plan-total {
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-weight: 700;
        font-size: .84rem;
        color: #111827;
        margin-top: 8px;
        padding-top: 8px;
        border-top: 2px solid #e5e7eb;
      }

      /* ── Edge detail panel ── */
      .tm-edge-panel {
        margin-top: 12px;
        border: 1px solid #e5e7eb;
        border-left: 4px solid #f97316;
        border-radius: 10px;
        background: #fff;
        box-shadow: 0 1px 4px rgba(0,0,0,.05);
        overflow: hidden;
      }
      .tm-edge-panel.tm-edge-empty {
        border-left-color: #d1d5db;
        background: #f9fafb;
      }
      .tm-edge-panel-header {
        background: #fff7ed;
        border-bottom: 1px solid #fed7aa;
        padding: 9px 14px 8px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .tm-edge-panel-header.tm-edge-header-empty {
        background: #f9fafb;
        border-bottom-color: #e5e7eb;
      }
      .tm-edge-panel-body { padding: 10px 14px 12px; }

      /* ── Empty state text ── */
      .tm-empty {
        color: #9ca3af;
        font-size: .82rem;
        font-style: italic;
        margin: 0;
      }
    ")),

    # ── Controls ──────────────────────────────────────────────────────────────
    div(class = "tm-controls",
      div(
        tags$label("Empresa objetivo:", class = "form-label d-block"),
        selectInput(ns("target_co"), NULL,
                    choices = setNames(names(COMPANY_MAP), unname(COMPANY_MAP)),
                    width = "230px")
      ),
      div(
        tags$label("Moneda:", class = "form-label d-block"),
        selectInput(ns("map_cur"), NULL, choices = c("MXN", "USD"), width = "90px")
      )
    ),

    # ── Graph canvas ──────────────────────────────────────────────────────────
    div(class = "tm-graph-card",
      visNetwork::visNetworkOutput(ns("map_net"), height = "400px")
    ),

    # ── Bottom: Routes + Payment plan ─────────────────────────────────────────
    div(class = "tm-panels-row",

      # Routes panel
      div(class = "tm-panel",
        div(class = "tm-panel-header",
          tags$span(icon("route"), style = "color:#6366f1; font-size:.9rem;"),
          tags$p(class = "tm-panel-title", "Rutas al objetivo")
        ),
        div(class = "tm-panel-body",
          uiOutput(ns("bfs_routes_ui"))
        )
      ),

      # Payment plan panel
      div(class = "tm-panel",
        div(class = "tm-panel-header",
          tags$span(icon("list-check"), style = "color:#10b981; font-size:.9rem;"),
          tags$p(class = "tm-panel-title", "Plan de pago")
        ),
        div(class = "tm-panel-body",
          uiOutput(ns("payment_plan_ui")),
          div(id = ns("send_btn_wrap"), style = "display:none; margin-top:8px;",
            actionButton(ns("send_to_agenda"),
                         tagList(icon("calendar-plus"), " Enviar a Agenda de Hoy"),
                         class = "btn btn-sm btn-success")
          )
        )
      )
    ),

    # ── Edge detail (invoice list) ─────────────────────────────────────────────
    uiOutput(ns("edge_detail_ui"))
  )
}

# =============================================================================
# Server
# =============================================================================

treasuryMapServer <- function(id, shared, ic_invoices_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    inv_map_rv <- reactive({
      cmap <- shared$company_map()
      setNames(names(cmap), unname(cmap))
    })

    observe({
      cmap <- shared$company_map()
      updateSelectInput(session, "target_co",
                        choices  = setNames(names(cmap), unname(cmap)),
                        selected = isolate(input$target_co) %||% names(cmap)[1])
    })

    # ── State ──────────────────────────────────────────────────────────────────
    payment_plan  <- reactiveVal(list())
    selected_edge <- reactiveVal(NULL)   # list(from_ini, to_ini) or NULL

    # ── Edge summary (AR only, by currency) ────────────────────────────────────
    edges_summary <- reactive({
      dat <- tryCatch(ic_invoices_rv(), error = function(e) NULL)
      cur <- input$map_cur %||% "MXN"
      ar  <- if (!is.null(dat)) dat$ar else NULL
      if (is.null(ar) || !nrow(ar)) return(NULL)

      amt_col <- if ("Saldo vencido" %in% names(ar)) "Saldo vencido" else "DocTotal"
      ar_c    <- ar[!is.na(ar$Moneda) & ar$Moneda == cur, , drop = FALSE]
      if (!nrow(ar_c)) return(NULL)

      ar_c$from_ini <- .tm_parte_to_ini(ar_c$Parte, shared$company_map())
      ar_c$to_ini   <- ar_c$.ini
      ar_c <- ar_c[!is.na(ar_c$from_ini) & !is.na(ar_c$to_ini) &
                    ar_c$from_ini != ar_c$to_ini, , drop = FALSE]
      if (!nrow(ar_c)) return(NULL)

      agg <- ar_c |>
        dplyr::group_by(from_ini, to_ini) |>
        dplyr::summarise(Total = sum(.data[[amt_col]], na.rm = TRUE),
                         Facturas = dplyr::n(), .groups = "drop") |>
        dplyr::filter(Total > 0)

      if (!nrow(agg)) NULL else agg
    })

    # ── Bank balances — recompute when IC data refreshes ───────────────────────
    bank_balances <- reactive({
      ic_invoices_rv()  # reactive dependency: refreshes together with IC data
      .tm_bank_balances(shared)
    })

    # ── visNetwork data ────────────────────────────────────────────────────────
    map_data <- reactive({
      cmap   <- shared$company_map()
      cur    <- input$map_cur %||% "MXN"
      target <- input$target_co %||% names(cmap)[1]
      bal    <- bank_balances()
      esum   <- edges_summary()

      # Nodes — SVG image nodes with gradient fill
      nodes <- do.call(rbind, lapply(names(cmap), function(ini) {
        full    <- cmap[[ini]]
        bv      <- bal[[ini]][[cur]] %||% NA_real_
        is_tgt  <- identical(ini, target)
        bal_lbl <- if (!is.na(bv)) .tm_fmt(bv, cur) else "sin datos"

        data.frame(
          id    = ini,
          label = "",          # text is embedded in SVG
          image = .tm_node_svg(ini, bal_lbl, is_tgt, bv),
          shape = "image",
          title = paste0("<b>", htmltools::htmlEscape(full), "</b><br/>",
                         "Saldo ", cur, ": <b>", htmltools::htmlEscape(bal_lbl), "</b>"),
          size             = if (is_tgt) 44 else 36,   # vis.js renders at 2×size px
          borderWidth      = 0,
          borderWidthSelected = 0,
          # Keep color fields transparent — visual is fully in SVG
          color.background           = "#00000000",
          color.border               = "#00000000",
          color.highlight.background = "#00000000",
          color.highlight.border     = "#00000000",
          stringsAsFactors = FALSE
        )
      }))

      # Edges
      if (!is.null(esum) && nrow(esum)) {
        max_t <- max(esum$Total, na.rm = TRUE)
        edges <- do.call(rbind, lapply(seq_len(nrow(esum)), function(i) {
          r   <- esum[i, ]
          lbl <- paste0(format(round(r$Total / 1000), big.mark = ",", scientific = FALSE), "K")
          data.frame(
            id    = i,
            from  = r$from_ini, to = r$to_ini,
            label = lbl,
            title = paste0("<b>", r$from_ini, " \u2192 ", r$to_ini, "</b><br/>",
                           .tm_fmt(r$Total, cur), "<br/>", r$Facturas, " factura(s)"),
            arrows = "to",
            width  = 1 + 4 * (r$Total / max_t),
            color  = "#1b3a2a",           # super dark green, almost black
            color.highlight = "#00c4a7",  # vivid teal on selection
            color.hover     = "#2d6a4f",  # medium dark green on hover
            font.size  = 11,
            font.color = "#1b3a2a",       # match arrow color
            font.strokeWidth = 3,
            font.strokeColor = "rgba(255,255,255,0.92)",  # white halo for legibility on white bg
            stringsAsFactors = FALSE
          )
        }))
      } else {
        edges <- data.frame(id = integer(), from = character(), to = character(),
                            label = character(), arrows = character(), width = numeric(),
                            stringsAsFactors = FALSE)
      }

      list(nodes = nodes, edges = edges, esum = esum)
    })

    # ── Render graph ──────────────────────────────────────────────────────────
    output$map_net <- visNetwork::renderVisNetwork({
      md <- map_data()
      visNetwork::visNetwork(md$nodes, md$edges, width = "100%") |>
        visNetwork::visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 0.65)),
          smooth = list(type = "curvedCW", roundness = 0.18)
          # No vis.js shadow on edges — edge labels need legibility on gradient bg
        ) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
        ) |>
        visNetwork::visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -80, springLength = 150,
                                  centralGravity = 0.01)
        ) |>
        visNetwork::visLayout(randomSeed = 42) |>
        visNetwork::visInteraction(hover = TRUE) |>
        visNetwork::visEvents(
          click = sprintf(
            "function(e) { Shiny.setInputValue('%s', {nodes: e.nodes, edges: e.edges, nonce: Math.random()}, {priority:'event'}); }",
            ns("net_click")
          )
        )
    })

    # ── Update selected edge color via proxy (no full re-render) ──────────────
    observe({
      edge <- selected_edge()
      md   <- isolate(map_data())
      esum <- md$esum
      if (is.null(esum) || !nrow(esum)) return()

      proxy <- visNetwork::visNetworkProxy(ns("map_net"))

      # Reset all edges to default dark green
      visNetwork::visUpdateEdges(proxy,
        data.frame(id = seq_len(nrow(esum)), color = "#1b3a2a", stringsAsFactors = FALSE)
      )

      # Highlight selected edge in orange
      if (!is.null(edge)) {
        sel_idx <- which(esum$from_ini == edge$from_ini & esum$to_ini == edge$to_ini)
        if (length(sel_idx))
          visNetwork::visUpdateEdges(proxy,
            data.frame(id = sel_idx[1], color = "#f97316", stringsAsFactors = FALSE)
          )
      }
    })

    # ── Unified click handler: node → set target; edge → show invoices ─────────
    observeEvent(input$net_click, {
      click <- input$net_click

      if (length(click$nodes) > 0) {
        node_id <- as.character(click$nodes[[1]])
        if (node_id %in% names(shared$company_map()))
          updateSelectInput(session, "target_co", selected = node_id)
        selected_edge(NULL)

      } else if (length(click$edges) > 0) {
        md      <- map_data()
        esum    <- md$esum
        if (is.null(esum)) return()
        edge_id <- suppressWarnings(as.integer(click$edges[[1]]))
        if (is.na(edge_id) || edge_id < 1 || edge_id > nrow(esum)) return()
        row <- esum[edge_id, ]
        selected_edge(list(from_ini = row$from_ini, to_ini = row$to_ini))

      } else {
        selected_edge(NULL)
      }
    }, ignoreNULL = TRUE)

    # ── BFS routes panel ──────────────────────────────────────────────────────
    output$bfs_routes_ui <- renderUI({
      target <- input$target_co %||% names(shared$company_map())[1]
      esum   <- edges_summary()
      if (is.null(esum) || !nrow(esum))
        return(tags$p(class = "tm-empty", "No hay saldos IC para esta moneda."))

      paths <- .tm_bfs_paths(
        data.frame(from = esum$from_ini, to = esum$to_ini, stringsAsFactors = FALSE),
        target, max_hops = 3
      )
      if (!length(paths))
        return(tags$p(class = "tm-empty",
                      paste0("Ninguna empresa debe directa o indirectamente a ", target, ".")))

      paths <- paths[order(vapply(paths, length, integer(1)))]

      tagList(lapply(paths, function(path) {
        hops <- length(path) - 1
        bottleneck <- Inf
        for (j in seq_len(hops)) {
          r <- esum[esum$from_ini == path[j] & esum$to_ini == path[j + 1], ]
          if (nrow(r)) bottleneck <- min(bottleneck, r$Total[1])
        }
        hop_lbl   <- if (hops == 1) "directo" else paste0(hops, " pasos")
        badge_cls <- if (hops == 1) "tm-badge-direct" else if (hops == 2) "tm-badge-2" else "tm-badge-3"
        amt_lbl   <- if (is.finite(bottleneck)) .tm_fmt(bottleneck, input$map_cur) else ""

        div(class = "tm-route",
          tags$span(class = "tm-route-path", paste(path, collapse = " \u2192 ")),
          tags$span(class = badge_cls, hop_lbl),
          if (nzchar(amt_lbl))
            tags$span(class = "tm-route-amt", paste0("m\u00e1x\u00a0", amt_lbl))
        )
      }))
    })

    # ── Edge detail panel ─────────────────────────────────────────────────────
    output$edge_detail_ui <- renderUI({
      edge <- selected_edge()

      if (is.null(edge)) {
        return(
          div(class = "tm-edge-panel tm-edge-empty",
            div(class = "tm-edge-panel-header tm-edge-header-empty",
              tags$span(icon("hand-pointer"), style = "color:#9ca3af; font-size:.9rem;"),
              tags$p(class = "tm-panel-title", style = "color:#6b7280; font-weight:500;",
                     "Detalle de facturas")
            ),
            div(class = "tm-edge-panel-body",
              tags$p(class = "tm-empty",
                     "Haz clic en una flecha del grafo para ver las facturas de ese par de empresas.")
            )
          )
        )
      }

      dat <- tryCatch(ic_invoices_rv(), error = function(e) NULL)
      ar  <- if (!is.null(dat)) dat$ar else NULL
      if (is.null(ar) || !nrow(ar)) return(NULL)

      cur      <- input$map_cur %||% "MXN"
      sub      <- ar[ar$.ini == edge$to_ini & !is.na(ar$Moneda) & ar$Moneda == cur, , drop = FALSE]
      cmap <- shared$company_map()
      sub$from_ini <- .tm_parte_to_ini(sub$Parte, cmap)
      sub <- sub[!is.na(sub$from_ini) & sub$from_ini == edge$from_ini, , drop = FALSE]

      from_full <- cmap[[edge$from_ini]] %||% edge$from_ini
      to_full   <- cmap[[edge$to_ini]]   %||% edge$to_ini

      div(class = "tm-edge-panel",
        div(class = "tm-edge-panel-header",
          tags$span(icon("arrow-right-arrow-left"), style = "color:#f97316; font-size:.9rem;"),
          tags$p(class = "tm-panel-title",
                 tagList(
                   tags$span(from_full),
                   tags$span(" \u2192 ", style = "color:#f97316;"),
                   tags$span(to_full),
                   tags$span(
                     class = "fw-normal ms-2",
                     style = "font-size:.76rem; color:#9ca3af;",
                     paste0(edge$from_ini, " debe a ", edge$to_ini)
                   )
                 )),
          actionButton(ns("close_edge_detail"), icon("xmark"),
                       class = "btn btn-sm btn-outline-secondary ms-auto",
                       style = "padding:1px 7px; font-size:.78rem;",
                       title = "Cerrar")
        ),
        div(class = "tm-edge-panel-body",
          if (!nrow(sub)) {
            tags$p(class = "tm-empty", "Sin facturas para este par en esta moneda.")
          } else {
            tagList(
              tags$p(class = "small text-muted mb-2",
                     "Selecciona filas y usa \u2018Agregar al plan\u2019 para incluirlas en el plan de pago."),
              DT::dataTableOutput(ns("edge_inv_tbl")),
              actionButton(ns("add_to_plan"),
                           tagList(icon("plus"), " Agregar al plan"),
                           class = "btn btn-sm btn-outline-primary mt-2",
                           style = "font-size:.8rem;")
            )
          }
        )
      )
    })

    observeEvent(input$close_edge_detail, selected_edge(NULL), ignoreInit = TRUE)

    # ── Invoice table ──────────────────────────────────────────────────────────
    output$edge_inv_tbl <- DT::renderDataTable({
      edge <- selected_edge(); req(edge)
      dat  <- tryCatch(ic_invoices_rv(), error = function(e) NULL)
      ar   <- if (!is.null(dat)) dat$ar else NULL
      req(!is.null(ar), nrow(ar) > 0)

      cur     <- input$map_cur %||% "MXN"
      amt_col <- if ("Saldo vencido" %in% names(ar)) "Saldo vencido" else "DocTotal"
      due_col <- "Fecha de vencimiento"
      doc_col <- "Fecha de contabilizaci\u00f3n"

      sub <- ar[ar$.ini == edge$to_ini & !is.na(ar$Moneda) & ar$Moneda == cur, , drop = FALSE]
      sub$from_ini <- .tm_parte_to_ini(sub$Parte)
      sub <- sub[!is.na(sub$from_ini) & sub$from_ini == edge$from_ini, , drop = FALSE]
      req(nrow(sub) > 0)

      keep    <- intersect(c("Documento", "Factura", doc_col, due_col, amt_col, "Moneda"), names(sub))
      display <- sub[, keep, drop = FALSE]
      if (due_col %in% names(display)) {
        display[["D\u00edas"]] <- as.integer(Sys.Date() - as.Date(display[[due_col]]))
        display <- display[order(display[[due_col]]), ]
      }

      dt <- DT::datatable(display, rownames = FALSE, selection = "multiple",
                          class = "table table-sm table-hover",
                          options = list(
                            pageLength = 12, dom = "ftp",
                            language = list(
                              search   = "Buscar:",
                              paginate = list(`next` = "\u203a", previous = "\u2039"),
                              info     = "Mostrando _START_\u2013_END_ de _TOTAL_",
                              zeroRecords = "Sin resultados"
                            )
                          ))
      if (amt_col %in% names(display)) dt <- DT::formatRound(dt, amt_col, digits = 2, mark = ",")
      if ("D\u00edas" %in% names(display))
        dt <- DT::formatStyle(dt, "D\u00edas",
                              color      = DT::styleInterval(c(0, 30), c("inherit", "#856404", "#dc3545")),
                              fontWeight = DT::styleInterval(30, c("normal", "bold")))
      dt
    }, server = FALSE)

    # ── Add to payment plan ────────────────────────────────────────────────────
    observeEvent(input$add_to_plan, {
      edge    <- selected_edge(); req(edge)
      sel_idx <- input$edge_inv_tbl_rows_selected
      if (!length(sel_idx)) {
        showNotification("Selecciona al menos una factura.", type = "warning", duration = 2)
        return()
      }

      dat <- tryCatch(ic_invoices_rv(), error = function(e) NULL)
      ar  <- if (!is.null(dat)) dat$ar else NULL; req(!is.null(ar))

      cur     <- input$map_cur %||% "MXN"
      amt_col <- if ("Saldo vencido" %in% names(ar)) "Saldo vencido" else "DocTotal"
      due_col <- "Fecha de vencimiento"

      cmap <- shared$company_map()
      sub <- ar[ar$.ini == edge$to_ini & !is.na(ar$Moneda) & ar$Moneda == cur, , drop = FALSE]
      sub$from_ini <- .tm_parte_to_ini(sub$Parte, cmap)
      sub <- sub[!is.na(sub$from_ini) & sub$from_ini == edge$from_ini, , drop = FALSE]
      req(nrow(sub) > 0)

      selected  <- sub[sel_idx, , drop = FALSE]
      new_items <- lapply(seq_len(nrow(selected)), function(i) {
        row <- selected[i, ]
        doc <- tryCatch(as.character(row[["Documento"]][1]), error = function(e)
                tryCatch(as.character(row[["Factura"]][1]), error = function(e) ""))
        list(
          from_ini  = edge$from_ini,
          to_ini    = edge$to_ini,
          from_full = cmap[[edge$from_ini]] %||% edge$from_ini,
          to_full   = cmap[[edge$to_ini]]   %||% edge$to_ini,
          Documento = doc,
          Importe   = as.numeric(row[[amt_col]][1] %||% 0),
          FechaVenc = tryCatch(as.character(as.Date(row[[due_col]][1])),
                               error = function(e) as.character(Sys.Date())),
          Moneda    = cur
        )
      })

      plan  <- payment_plan()
      exist <- vapply(plan, function(x) paste0(x$to_ini, "_", x$Documento), character(1))
      added <- 0L
      for (r in new_items) {
        key <- paste0(r$to_ini, "_", r$Documento)
        if (!key %in% exist) { plan <- c(plan, list(r)); exist <- c(exist, key); added <- added + 1L }
      }
      payment_plan(plan)
      showNotification(paste0(added, " factura(s) a\u00f1adida(s) al plan."), type = "message", duration = 2)
    }, ignoreInit = TRUE)

    observeEvent(input$clear_plan, { payment_plan(list()) }, ignoreInit = TRUE)

    # ── Payment plan UI ────────────────────────────────────────────────────────
    output$payment_plan_ui <- renderUI({
      plan <- payment_plan()
      if (!length(plan)) {
        shinyjs::hide("send_btn_wrap")
        return(tags$p(class = "tm-empty",
                      "Haz clic en una flecha, selecciona facturas y agr\u00e9galas aqu\u00ed."))
      }
      shinyjs::show("send_btn_wrap")

      cur   <- input$map_cur %||% "MXN"
      total <- sum(vapply(plan, function(x) x$Importe %||% 0, numeric(1)), na.rm = TRUE)

      div(
        tagList(lapply(plan, function(x) {
          div(class = "tm-plan-item",
            tags$span(paste0(x$from_ini, " \u2192 ", x$to_ini, " \u00b7 ", x$Documento)),
            tags$span(class = "tm-plan-amt",
                      format(round(x$Importe), big.mark = ",", scientific = FALSE))
          )
        })),
        div(class = "tm-plan-total",
          tags$span("Total"),
          tags$span(.tm_fmt(total, cur))
        ),
        actionButton(ns("clear_plan"), tagList(icon("trash"), " Limpiar"),
                     class = "btn btn-sm btn-outline-danger mt-2",
                     style = "font-size:.76rem; padding:2px 8px;")
      )
    })

    # ── Send to Agenda de Hoy ──────────────────────────────────────────────────
    observeEvent(input$send_to_agenda, {
      plan <- payment_plan(); req(length(plan) > 0)

      ph <- tryCatch(shared$pagar_hoy_db(), error = function(e) NULL) %||%
            tibble::tibble(id = character(), ledger = character(), Empresa = character(),
                           Moneda = character(), Documento = character(), Parte = character(),
                           Importe = numeric(), FechaVenc = as.Date(character()),
                           staged_by = character(), staged_at = as.POSIXct(character()),
                           status = character())

      user_id <- tryCatch(as.character(shared$current_user()), error = function(e) "icmap")

      new_rows <- dplyr::bind_rows(lapply(plan, function(x) {
        tibble::tibble(
          id        = paste0("IC_", x$from_ini, "_", x$Documento, "_", as.integer(Sys.time())),
          ledger    = "AP",
          Empresa   = x$from_full,   # the PAYER (debtor, "from" node)
          Moneda    = x$Moneda,
          Documento = x$Documento,
          Parte     = x$to_full,     # the RECIPIENT (creditor, "to" node)
          Codigo    = trimws(as.character(x$Codigo %||% "")),
          tipo_item = "factura",
          Importe   = x$Importe %||% 0,
          FechaVenc = tryCatch(as.Date(x$FechaVenc), error = function(e) Sys.Date()),
          staged_by = user_id,
          staged_at = Sys.time(),
          status    = "pending"
        )
      }))

      combined <- upsert_pagar_hoy(ph, new_rows)
      shared$pagar_hoy_db(combined)
      tryCatch(save_pagar_hoy(combined, shared$current_user(), client_id = shared$effective_client_id()), error = function(e) NULL)
      payment_plan(list())
      showNotification(paste0(nrow(new_rows), " factura(s) enviada(s) a Agenda de Hoy."),
                       type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Preset target from pagar_hoy button ───────────────────────────────────
    observe({
      req(!is.null(shared$ic_map_target))
      tgt <- shared$ic_map_target()
      req(!is.null(tgt), nzchar(tgt$ini %||% ""))
      if (tgt$ini %in% names(shared$company_map()))
        updateSelectInput(session, "target_co", selected = tgt$ini)
    })
  })
}
