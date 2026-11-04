# =============================================================================
# R/bancos_module.R
# Módulo Bancos — UI + Server
# 4 tabs: Libro de Banco | Importar TXT | Historial/Papelera | Cuentas
# =============================================================================

# ── Type labels & choices ─────────────────────────────────────────────────────
.TIPO_LABELS <- c(
  spei_out      = "SPEI Enviado",
  spei_in       = "SPEI Recibido",
  comision      = "Comisi\u00f3n",
  traspaso      = "Traspaso",
  nomina        = "N\u00f3mina",
  pos           = "POS",
  domiciliacion = "Domiciliaci\u00f3n",
  cambio        = "Cambio",
  plazo         = "Plazo",
  otro          = "Otro",
  manual_in     = "Manual"
)

# ── Currency color palette ─────────────────────────────────────────────────────
# Left border + legend text color per currency.
# Follows world convention. Add currencies here as needed — unused ones
# are defined but never rendered until an account with that currency exists.
.CURRENCY_COLORS <- c(
  MXN = "#C0674A",   # terracotta — warm, distinct
  USD = "#185FA5",   # blue — convention
  EUR = "#3C3489",   # indigo
  GBP = "#6B3FA0",   # purple
  CAD = "#B5451B",   # burnt orange
  JPY = "#1A6B5A",   # deep teal
  CHF = "#C0392B",   # swiss red
  AUD = "#0E7A6E",   # teal-green
  CNY = "#A0221C",   # deep red
  BRL = "#1E6B3A"    # forest green
)

.currency_color <- function(moneda) {
  col <- .CURRENCY_COLORS[toupper(trimws(moneda %||% ""))]
  if (is.na(col) || !length(col)) "#5F5E5A" else unname(col)
}

.PERIODO_CHOICES <- c(
  "Mes actual"        = "mes_actual",
  "Mes anterior"      = "mes_anterior",
  "Últimos 3 meses"   = "ultimos_3m",
  "Todo el historial" = "todo",
  "Rango..."          = "personalizado"
)

# The Libro's and the Eliminar confirmation's tables with no rows (hd_table, T12a).
.bnc_libro_vacia <- data.frame(id = character(), Empresa = character(), FechaHora = as.POSIXct(character()),
                               Moneda = character(), Tipo = character(), Descripcion = character(),
                               Recibo = character(), Cargo = numeric(), Abono = numeric(), Saldo = numeric(),
                               .acciones = character(), stringsAsFactors = FALSE, check.names = FALSE)
.bnc_elim_vacia <- data.frame(id = character(), Fecha = as.Date(character()), Parte = character(),
                              Cargo = numeric(), Abono = numeric(), Moneda = character(), stringsAsFactors = FALSE)

# Bancos' movement previews (Importar TXT, Reasignar; hd_table class E, T12b):
# the movements as they are or will be saved, one row each.
.BNC_COLUMNAS_PREVIA <- list(
  col_fecha("Fecha", "tablas.bancos.fecha"),
  col_insignia("Tipo", "tablas.facturas.tipo", mapa = lapply(as.list(.TIPO_LABELS), function(t) list(texto = t, tono = "neutro"))),
  col_texto("Parte", "tablas.facturas.parte"),
  col_texto("Concepto", "tablas.bancos.concepto"),
  col_dinero("Cargo", "tablas.bancos.cargo", moneda = "Moneda"),
  col_dinero("Abono", "tablas.bancos.abono", moneda = "Moneda"))
.bnc_previa_datos <- function(movs, cuentas_df) {
  if (is.null(movs) || !nrow(movs))
    return(data.frame(id = character(), Fecha = as.Date(character()), Tipo = character(), Parte = character(),
                      Concepto = character(), Cargo = numeric(), Abono = numeric(), Moneda = character(),
                      stringsAsFactors = FALSE))
  mon <- cuentas_df$moneda[match(movs$cuenta_id, cuentas_df$cuenta_id)]
  data.frame(id = if ("id" %in% names(movs) && !anyNA(movs$id)) as.character(movs$id) else sprintf("p%05d", seq_len(nrow(movs))),
             Fecha = as.Date(movs$fecha), Tipo = dplyr::coalesce(as.character(movs$tipo), "otro"),
             Parte = as.character(movs$parte %||% ""),
             Concepto = as.character(dplyr::coalesce(movs$concepto, movs$descripcion_raw, "")),
             Cargo = ifelse(movs$cargo > 0, movs$cargo, NA_real_), Abono = ifelse(movs$abono > 0, movs$abono, NA_real_),
             Moneda = toupper(trimws(dplyr::coalesce(mon, ""))), stringsAsFactors = FALSE)
}

# Filter movements by period
.filter_periodo <- function(df, periodo, desde = NULL, hasta = NULL) {
  hoy <- Sys.Date()
  ms  <- function(d) as.Date(format(d, "%Y-%m-01"))
  switch(periodo,
    mes_actual   = dplyr::filter(df, fecha >= ms(hoy), fecha <= hoy),
    mes_anterior = {
      # Derive inicio FROM fin (fin's own month-1st) rather than a fixed
      # "go back 32 days" offset -- that heuristic under-shoots whenever the
      # previous month has 31 days and today is early in the current month
      # (e.g. today=Aug 1: hoy-32=Jun 30, truncates to Jun 1 instead of Jul 1).
      fin    <- ms(hoy) - 1
      inicio <- ms(fin)
      dplyr::filter(df, fecha >= inicio, fecha <= fin)
    },
    ultimos_3m   = dplyr::filter(df, fecha >= ms(hoy - 90)),
    personalizado = {
      if (!is.null(desde) && !is.null(hasta))
        dplyr::filter(df, fecha >= as.Date(desde), fecha <= as.Date(hasta))
      else df
    },
    df  # todo
  )
}

# ── Inline CSS ────────────────────────────────────────────────────────────────
bancos_styles <- function() {
  tags$style(HTML("
    /* ── Bancos module ─────────────────────────────────────── */
    /* Balance cards, compact (T09b): company and balance on one line, the
       bank and the 30-day sparkline on the next; ~48 px a card. */
    .bnc-saldos-titulo {
      border:0; background:none; padding:0 2px; font-size:.78rem; font-weight:600;
      color:#6c757d; display:inline-flex; align-items:center; gap:4px; cursor:pointer;
    }
    .bnc-saldos-titulo:hover { color:#1a1a1a; }
    .bnc-saldos-flecha { display:inline-block; transition:transform .15s; }
    .bnc-saldos.bnc-plegado .bnc-saldos-flecha { transform:rotate(-90deg); }
    .bnc-saldos.bnc-plegado .bnc-dashboard,
    .bnc-saldos.bnc-plegado > .shiny-html-output { display:none; }
    .bnc-dashboard {
      display:grid; grid-template-columns:repeat(auto-fill, minmax(175px, 1fr));
      gap:6px; padding:4px 0 2px;
    }
    .bnc-card {
      background:#fff; border:0.5px solid #dee2e6; border-radius:6px;
      padding:4px 10px; min-width:0;
      border-left-width:3px; border-left-style:solid; border-left-color:#adb5bd;
      overflow:hidden;
    }
    .bnc-card .bnc-card-l1, .bnc-card .bnc-card-l2 {
      display:flex; align-items:baseline; gap:6px; min-width:0;
    }
    .bnc-card .bnc-card-l2 { align-items:center; }
    .bnc-card .bnc-card-emp {
      font-size:.7rem; font-weight:700; color:#1a1a1a;
      text-transform:uppercase; letter-spacing:.05em; flex:none;
    }
    .bnc-card .bnc-card-bal {
      font-size:1rem; font-weight:700; color:#1a1a1a; white-space:nowrap;
      margin-left:auto;
    }
    .bnc-card .bnc-card-bal.bnc-neg-bal { color:#dc3545; }
    .bnc-card .bnc-card-mon {
      font-size:.7rem; font-weight:600; flex:1; min-width:0;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
    }
    .bnc-card .bnc-spark-wrap { flex:none; width:48px; height:16px; }
    .bnc-card .bnc-spark { width:100%; height:16px; display:block; }
    .bnc-card.bnc-pos  { }
    .bnc-card.bnc-neg  { }
    .bnc-card.bnc-zero { }
    .bnc-tipo-badge {
      display:inline-block; font-size:.68rem; padding:1px 6px;
      border-radius:4px; font-weight:600; white-space:nowrap;
    }
    .bnc-tipo-spei_out    { background:#fff3cd; color:#664d03; }
    .bnc-tipo-spei_in     { background:#d1e7dd; color:#0a3622; }
    .bnc-tipo-comision    { background:#f8f9fa; color:#6c757d; }
    .bnc-tipo-traspaso    { background:#fff0d6; color:#7d4e00; border:1px solid #fd7e14; }
    .bnc-tipo-nomina      { background:#e0cffc; color:#3d0a91; }
    .bnc-tipo-pos         { background:#cfe2ff; color:#084298; }
    .bnc-tipo-manual_in   { background:#d1ecf1; color:#0c5460; }
    .bnc-tipo-otro,
    .bnc-tipo-domiciliacion,
    .bnc-tipo-cambio,
    .bnc-tipo-plazo       { background:#e9ecef; color:#495057; }
    .bnc-row-comision     { opacity:.55; }
    .bnc-row-conciliado   { background:#f0fff4 !important; }
    .bnc-row-cargo        { color:#dc3545; font-weight:600; }
    .bnc-row-abono        { color:#198754; font-weight:600; }
    .bnc-import-badge     {
      display:inline-flex; align-items:center; gap:4px;
      font-size:.8rem; padding:3px 10px; border-radius:20px;
    }
    .bnc-btn-xs {
      font-size:.72rem; padding:1px 5px; line-height:1.4;
      border:1px solid #dee2e6; background:#fff; cursor:pointer;
      border-radius:3px;
    }
    .bnc-btn-xs:hover { background:#f8f9fa; }
    .bnc-btn-xs--undo {
      border-color:#ffc107; color:#856404; background:#fffbf0;
    }
    .bnc-btn-xs--undo:hover { background:#fff3cd; }
    /* ── Day group banding (very subtle — must not look like selection) ─── */
    tr.bnc-day-alt:not(.bnc-row-selected) td { background:#f8f9fa; }
    /* ── Row selection (clearly distinct blue) ──────────────────────────── */
    .bnc-libro-wrap tbody tr { cursor:pointer; user-select:none; }
    tr.bnc-row-selected td  { background:#cfe2ff !important; }
    tr.bnc-row-selected td:first-child { border-left:3px solid #0d6efd !important; }
    /* ── Compact libro table ─────────────────────────────────────────────── */
    .bnc-libro-wrap table.dataTable { font-size:.76rem; }
    .bnc-libro-wrap table.dataTable.compact thead th,
    .bnc-libro-wrap table.dataTable.compact tbody td { padding:3px 7px !important; line-height:1.3; }
    /* ── Slim toolbar ────────────────────────────────────────────────────── */
    .bnc-toolbar { border-bottom:1px solid #e9ecef; padding-bottom:6px; margin-bottom:8px; }
    .bnc-toolbar .form-group { margin-bottom:0 !important; }
    .bnc-toolbar .selectize-control { margin-bottom:0 !important; }
    .bnc-toolbar .selectize-input,
    .bnc-toolbar .selectize-input.input-active {
      min-height:28px !important; height:28px !important;
      padding:3px 30px 3px 8px !important; font-size:.82rem !important; line-height:1.35 !important;
      white-space:nowrap !important; overflow:hidden !important; text-overflow:ellipsis !important;
      flex-wrap:nowrap !important;
    }
    .bnc-toolbar .selectize-input > * { white-space:nowrap !important; overflow:hidden !important; text-overflow:ellipsis !important; }
    .bnc-toolbar .selectize-dropdown { font-size:.82rem; }
    .bnc-toolbar-sep { width:1px; height:20px; background:#dee2e6; flex-shrink:0; align-self:center; }
    /* Flujo button states — shows current active filter */
    .bnc-flujo-todos    { background:transparent !important; border-color:#fd7e14 !important; color:#fd7e14 !important; font-weight:600; }
    .bnc-flujo-todos:hover { background:#fd7e14 !important; color:#fff !important; }
    .bnc-flujo-egresos  { background:#dc3545 !important; border-color:#dc3545 !important; color:#fff !important; font-weight:600; }
    .bnc-flujo-ingresos { background:#198754 !important; border-color:#198754 !important; color:#fff !important; font-weight:600; }
    /* Comisiones visible */
    .bnc-com-active { background:#fd7e14 !important; border-color:#fd7e14 !important; color:#fff !important; font-weight:600; }
    /* Trash enabled: fill red */
    .bnc-btn-trash:not([disabled]) { background:#dc3545 !important; color:#fff !important; border-color:#dc3545 !important; }
    .bnc-btn-trash:not([disabled]):hover { background:#bb2d3b !important; border-color:#b02a37 !important; }
    /* ── Selection pill ─────────────────────────────────────── */
    .bnc-sel-pill {
      display:inline-flex; align-items:center; gap:6px;
      background:#0d6efd; color:#fff; border-radius:20px;
      padding:3px 8px 3px 12px; font-size:.8rem; font-weight:500;
    }
    .bnc-sel-pill-clear {
      cursor:pointer; border:none; background:transparent;
      color:rgba(255,255,255,.8); font-size:1.15rem; line-height:1; padding:0 3px;
    }
    .bnc-sel-pill-clear:hover { color:#fff; }
    /* ── Vincular mode cursor ───────────────────────────────── */
    .bnc-vincular-mode .bnc-libro-wrap tbody tr { cursor:crosshair !important; }
    /* ── Vincular / Sugerencias modals ─────────────────────── */
    .bnc-vin-card {
      background:#f8f9fa; border:1px solid #dee2e6; border-radius:8px;
      padding:10px 14px; font-size:.88rem;
    }
    .bnc-vin-cand-row {
      display:flex; justify-content:space-between; align-items:center;
      padding:8px 10px; border-bottom:1px solid #f0f0f0;
      cursor:pointer; transition:background .1s;
    }
    .bnc-vin-cand-row:hover { background:#f5f7fa; }
    .bnc-score-badge {
      font-size:.7rem; color:#6c757d; background:#e9ecef;
      border-radius:20px; padding:1px 7px; white-space:nowrap; flex-shrink:0;
    }
    .bnc-sug-panel {
      border:1px solid #dee2e6; border-radius:6px;
      overflow-y:auto; max-height:380px;
    }
    .bnc-sug-item { padding:8px 12px; border-bottom:1px solid #f0f0f0; cursor:pointer; }
    .bnc-sug-item:hover { background:#f8f9fa; }
    .bnc-sug-item.bnc-sug-active { background:#EBF4FF; border-left:3px solid #0A58CA; padding-left:9px; }
    .bnc-sug-item.bnc-sug-dim { opacity:.35; pointer-events:none; }
    /* ── Import tab balance band ──────────────────────────────── */
    .bnc-ticker {
      display: flex;
      flex-wrap: wrap;
      gap: 0;
      background: #0d1117;
      border-radius: 8px;
      overflow: hidden;
      margin-bottom: 18px;
      border: 1px solid #1e2733;
    }
    .bnc-ticker-item {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 7px 18px;
      border-right: 1px solid #1e2733;
      border-bottom: 1px solid #1e2733;
      flex: 1 1 auto;
      min-width: 160px;
    }
    .bnc-ticker-item:last-child { border-right: none; }
    .bnc-ticker-label {
      font-size: .70rem;
      font-weight: 600;
      color: #8b949e;
      text-transform: uppercase;
      letter-spacing: .06em;
      white-space: nowrap;
    }
    .bnc-ticker-val {
      font-size: .92rem;
      font-weight: 700;
      font-variant-numeric: tabular-nums;
      white-space: nowrap;
    }
    .bnc-ticker-val.pos { color: #3fb950; }
    .bnc-ticker-val.neg { color: #f85149; }
    .bnc-ticker-val.zero { color: #8b949e; }
    .bnc-ticker-mon {
      font-size: .68rem;
      color: #484f58;
      white-space: nowrap;
      margin-left: 2px;
    }
    .bnc-ticker-dot {
      width: 6px; height: 6px;
      border-radius: 50%;
      flex-shrink: 0;
    }
    .bnc-ticker-dot.pos  { background: #3fb950; }
    .bnc-ticker-dot.neg  { background: #f85149; }
    .bnc-ticker-dot.zero { background: #8b949e; }
  "))
}

# ── The Libro bar's toggles (injected once in bancosUI) ─────────────────────
# Flujo cycles Todos -> Egresos -> Ingresos; Comisiones shows or hides them.
# Marking rows, Eliminar and Editar belong to the table engine now (T12a,
# hd_table "libro_banco"); Vincular and Conciliar were retired before it.
bancos_libro_js <- function(id) {
  pfx <- paste0(id, "-")   # e.g. "bnc-"
  js  <- sprintf(
'(function() {
  "use strict";
  var NS          = "%s";
  var _flujoIdx    = 0;      /* 0=todos 1=egresos 2=ingresos */
  var _flujoStates = ["todos","egresos","ingresos"];
  var _flujoLabels = ["Todos","Egresos","Ingresos"];
  var _comVisible  = false;
  var _flujoClasses = ["bnc-flujo-todos","bnc-flujo-egresos","bnc-flujo-ingresos"];

  $(document).on("click", "#" + NS + "btn_flujo_cycle", function() {
    _flujoIdx = (_flujoIdx + 1) %% 3;
    this.textContent = _flujoLabels[_flujoIdx];
    $(this).removeClass("bnc-flujo-todos bnc-flujo-egresos bnc-flujo-ingresos")
           .addClass(_flujoClasses[_flujoIdx]);
    Shiny.setInputValue(NS + "flujo_filter", _flujoStates[_flujoIdx], { priority: "event" });
  });

  $(document).on("click", "#" + NS + "btn_comisiones_toggle", function() {
    _comVisible = !_comVisible;
    this.textContent = _comVisible ? "Con comisiones" : "Sin comisiones";
    $(this).toggleClass("bnc-com-active", _comVisible);
    Shiny.setInputValue(NS + "mostrar_comisiones", _comVisible, { priority: "event" });
  });

  /* The engine drops deleted rows from its selection by itself. */
  Shiny.addCustomMessageHandler(NS + "clear_selection", function(x) {});
})();', pfx)
  tags$script(HTML(js))
}

# ── Match scoring ─────────────────────────────────────────────────────────────
# item:      one-row tibble from bancos_movimientos (has cargo/abono/fecha/rfc/
#            referencia/concepto/descripcion_raw fields)
# candidate: standardised list (rfc, importe, fecha, parte, concepto,
#            referencia, descripcion_raw — all optional)
# Returns integer score; threshold for display is 15.
score_vincular_match <- function(item, candidate) {
  score <- 0L

  # ── Importe / monto — highest weight ──────────────────────────────────────
  imp_a <- abs(as.numeric(item$cargo %||% 0) + as.numeric(item$abono %||% 0))
  # Candidates may carry either `importe` (confirmados/SAP) or cargo/abono (movimientos)
  imp_b <- if (!is.null(candidate$importe) && !is.na(candidate$importe)) {
    abs(as.numeric(candidate$importe))
  } else {
    abs(as.numeric(candidate$cargo %||% 0) + as.numeric(candidate$abono %||% 0))
  }
  if (imp_a > 0 && !is.na(imp_b) && imp_b > 0) {
    if (isTRUE(abs(imp_a - imp_b) < 0.01))              score <- score + 40L
    else if (isTRUE(abs(imp_a - imp_b) / imp_a < 0.01)) score <- score + 25L
    else if (isTRUE(abs(imp_a - imp_b) / imp_a < 0.05)) score <- score + 10L
  }

  # ── Document number overlap — extract 4+ digit tokens ─────────────────────
  doc_a <- tolower(paste(
    item$referencia %||% "", item$concepto %||% "",
    item$descripcion_raw %||% "", collapse = " "))
  doc_b <- tolower(paste(
    candidate$referencia %||% "", candidate$concepto %||% "",
    candidate$descripcion_raw %||% candidate$concepto %||% "",
    collapse = " "))
  nums_a <- regmatches(doc_a, gregexpr("[0-9]{4,}", doc_a))[[1]]
  nums_b <- regmatches(doc_b, gregexpr("[0-9]{4,}", doc_b))[[1]]
  if (length(nums_a) > 0 && length(nums_b) > 0 && any(nums_a %in% nums_b))
    score <- score + 30L

  # ── RFC exact match ────────────────────────────────────────────────────────
  rfc_a <- trimws(toupper(item$rfc %||% ""))
  rfc_b <- trimws(toupper(candidate$rfc %||% ""))
  if (!is.na(rfc_a) && nzchar(rfc_a) && !is.na(rfc_b) && nzchar(rfc_b) && rfc_a == rfc_b) score <- score + 20L

  # ── Date proximity — low weight, never a disqualifier ─────────────────────
  fecha_a <- tryCatch(as.Date(item$fecha),      error = function(e) NA)
  fecha_b <- tryCatch(as.Date(candidate$fecha), error = function(e) NA)
  if (isTRUE(!is.na(fecha_a)) && isTRUE(!is.na(fecha_b))) {
    diff_days <- abs(as.integer(fecha_b - fecha_a))
    if (isTRUE(diff_days <= 3))      score <- score + 10L
    else if (isTRUE(diff_days <= 7)) score <- score + 5L
  }

  score
}

# ── Helper: account choices vector ───────────────────────────────────────────
.cuenta_choices <- function(cuentas_df, include_sin_cuenta = TRUE, include_todas = FALSE) {
  if (is.null(cuentas_df) || !nrow(cuentas_df)) {
    choices <- if (include_sin_cuenta) c("Ingresos sin cuenta" = "__sin_cuenta__") else character(0)
    if (include_todas) choices <- c("Todas las cuentas" = "__todas__", choices)
    return(choices)
  }
  act <- dplyr::filter(cuentas_df, activa == TRUE)
  named <- setNames(act$cuenta_id,
    paste0(act$empresa, " \u2014 ", act$banco, " ", act$moneda,
           " (", act$alias, ")"))
  result <- if (include_sin_cuenta) c(named, "Ingresos sin cuenta" = "__sin_cuenta__") else named
  if (include_todas) c("Todas las cuentas" = "__todas__", result) else result
}

# ── Translate ctas_cuentas + ctas_bancos → bancos_cuentas format ─────────────
# This keeps the Bancos module in sync with the single source of truth in
# Settings → Cuentas de Empresa, without a redundant bancos_cuentas.rds.
#
# Field mapping:
#   ctas_cuentas$id                  → cuenta_id
#   ctas_cuentas$Empresa             → empresa  (initials)
#   ctas_bancos$nombre  (via FK)     → banco
#   ctas_cuentas$Moneda              → moneda
#   ctas_cuentas$cuenta              → numero_cuenta  (PPL account number)
#   ctas_cuentas$clabe_interbancaria → clabe
#   ctas_cuentas$alias               → alias
#   ctas_cuentas$activa              → activa
#   saldo_inicial / fecha_inicio / tarjetas_retenido → 0 / NA / 0  (defaults)
.ctas_to_bancos_cuentas <- function(ctas_cuentas_df, ctas_bancos_df = NULL) {
  if (is.null(ctas_cuentas_df) || !nrow(ctas_cuentas_df))
    return(.schema_bancos_cuentas())

  # Build bank-name lookup
  banco_map <- if (!is.null(ctas_bancos_df) && nrow(ctas_bancos_df)) {
    setNames(ctas_bancos_df$nombre, ctas_bancos_df$id)
  } else character(0)

  ctas_cuentas_df |>
    dplyr::transmute(
      cuenta_id         = id,
      empresa           = Empresa,
      banco             = dplyr::coalesce(banco_map[banco_id], "—"),
      moneda            = Moneda,
      numero_cuenta     = cuenta,
      clabe             = clabe_interbancaria,
      alias             = alias,
      saldo_inicial     = dplyr::coalesce(as.numeric(saldo_inicial), 0),
      fecha_inicio      = as.Date(NA),
      tarjetas_retenido = 0,
      activa            = activa
    )
}

# ── Safe company-map lookup ───────────────────────────────────────────────────
# cmap is the SESSION's map (shared$company_map()), never the global one.
# Uses single [ (returns NA, never errors) instead of [[ (errors on unknown key);
# an empty map (c() is NULL) gives NA for every key instead of a zero-length result.
.cmap_lookup <- function(cmap, keys) {
  if (!length(cmap)) return(rep(NA_character_, length(keys)))
  unname(cmap[keys])
}

.company_name <- function(ini, cmap) {
  ini <- trimws(ini %||% "")
  if (!nzchar(ini)) return(ini)
  nm <- .cmap_lookup(cmap, ini)
  if (is.na(nm)) ini else nm
}

# ── Badge HTML helper ─────────────────────────────────────────────────────────
.tipo_badge <- function(tipo) {
  # %||% only substitutes on NULL, not NA -- an NA tipo (e.g. a legacy/manual
  # row with none set) would otherwise render a literal "NA" badge instead of
  # falling back to the existing "Otro" bucket.
  if (is.na(tipo)) tipo <- "otro"
  lbl <- .TIPO_LABELS[tipo] %||% tipo
  sprintf('<span class="bnc-tipo-badge bnc-tipo-%s">%s</span>',
          htmltools::htmlEscape(tipo), htmltools::htmlEscape(lbl))
}

# =============================================================================
# bancosUI
# =============================================================================
bancosUI <- function(id) {
  ns <- NS(id)
  tagList(
    bancos_styles(),
    bancos_libro_js(id),
    bslib::navset_tab(
      id = ns("bancos_tab"),

      # ── 1. Libro de Banco ─────────────────────────────────────────────────
      bslib::nav_panel(
        title = "Libro de Banco", value = "libro",
        div(class = "p-2",
          # Balance cards, compact and foldable (T09b): at Luis's window
          # (1293x632) the 11 cards took 340 px and the table started below
          # it. The title folds them; the choice is kept in this browser.
          div(class = "bnc-saldos", id = ns("saldos"),
            tags$button(type = "button", class = "bnc-saldos-titulo", `aria-expanded` = "true",
                        `aria-controls` = ns("dashboard_cards"),
                        tags$span(class = "bnc-saldos-flecha", `aria-hidden` = "true", "▾"),
                        tr("tablas.bancos.saldos")),
            uiOutput(ns("dashboard_cards"))
          ),
          tags$script(HTML(sprintf(
            "(function () {
               var w = document.getElementById('%s'); if (!w) return;
               var b = w.querySelector('.bnc-saldos-titulo'), k = 'hd.bancos.saldos_plegado';
               var poner = function (p) { w.classList.toggle('bnc-plegado', p); b.setAttribute('aria-expanded', p ? 'false' : 'true'); };
               try { poner(localStorage.getItem(k) === '1'); } catch (e) {}
               b.addEventListener('click', function () {
                 var p = !w.classList.contains('bnc-plegado'); poner(p);
                 try { localStorage.setItem(k, p ? '1' : '0'); } catch (e) {}
               });
             })();", ns("saldos")))),
          tags$hr(class = "my-2"),
          # \u2500\u2500 Slim toolbar (one bar, no labels) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
          div(class = "d-flex align-items-center gap-2 bnc-toolbar",
            div(style = "flex-shrink:0;",
              selectInput(ns("libro_cuenta"), NULL,
                          choices  = c("Todas las cuentas" = "__todas__"),
                          selected = "__todas__", width = "210px")
            ),
            div(style = "flex-shrink:0;",
              selectInput(ns("libro_periodo"), NULL,
                          choices = .PERIODO_CHOICES, width = "148px")
            ),
            conditionalPanel(
              condition = "input.libro_periodo === 'personalizado'",
              ns = ns,
              div(style = "flex-shrink:0;",
                shinyWidgets::airDatepickerInput(
                  inputId    = ns("libro_rango"),
                  label      = NULL,
                  value      = c(Sys.Date() - 30, Sys.Date()),
                  range      = TRUE,
                  dateFormat = "dd-M-yyyy",
                  separator  = " — ",
                  width      = "225px",
                  placeholder = "Inicio — Fin"
                )
              )
            ),
            tags$div(class = "bnc-toolbar-sep"),
            tags$button(
              id = ns("btn_flujo_cycle"), type = "button",
              class = "btn btn-sm btn-outline-secondary bnc-flujo-todos",
              style = "white-space:nowrap; font-size:.78rem; padding:3px 9px;",
              "Todos"
            ),
            tags$button(
              id = ns("btn_comisiones_toggle"), type = "button",
              class = "btn btn-sm btn-outline-secondary",
              style = "white-space:nowrap; font-size:.78rem; padding:3px 9px;",
              "Sin comisiones"
            ),
            tags$div(class = "flex-grow-1"),
            tags$div(class = "bnc-toolbar-sep"),
            # The "Vincular" duplicate-merge button (retired 2026-07-30) and
            # "Conciliar" were here. Their code, kept inert in if(FALSE)
            # blocks, was removed in T12b; it is in git history.
            actionButton(ns("add_mov_manual"), icon("plus"),
                         label = " Agregar",
                         class  = "btn btn-sm btn-outline-primary",
                         style  = "font-size:.78rem; padding:3px 9px; white-space:nowrap;"),
            # Eliminar (bar, with rows marked) and Editar (✎ on manual rows)
            # live in the table now (T12a), and so does its Excel (the
            # table's columns); this one keeps the bank's layout.
            downloadButton(
              ns("download_libro"),
              label = HTML(paste0(
                '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" ',
                'stroke="currentColor" stroke-width="2" stroke-linecap="round" ',
                'stroke-linejoin="round" style="margin-right:3px;vertical-align:-1px">',
                '<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/>',
                '<polyline points="14 2 14 8 20 8"/>',
                '<line x1="16" y1="13" x2="8" y2="13"/>',
                '<line x1="16" y1="17" x2="8" y2="17"/>',
                '<polyline points="10 9 9 9 8 9"/>',
                '</svg>', htmltools::htmlEscape(tr(BANCOS_FORMATOS_EXPORT$banbajio$nombre)))),
              class = "btn btn-sm btn-outline-secondary",
              style = "font-size:.78rem; padding:3px 9px; white-space:nowrap;"
            ),
          ),
          # Movement table: right below the bar, down to the window's bottom.
          # Without can_manage_bancos its bar (Eliminar) isn't shown.
          tags$style(HTML(".bnc-solo-lectura .hdt-acciones { display: none !important; }")),
          div(id = ns("libro_wrap"), hd_table_ui(ns("libro"), alto = "auto")),
        )
      ),

      # ── 2. Importar TXT ───────────────────────────────────────────────────
      bslib::nav_panel(
        title = "Importar TXT", value = "importar",
        div(class = "p-3",
          uiOutput(ns("import_balance_band")),

          # ── File picker row ─────────────────────────────────────────────────
          div(class = "row align-items-end mb-2",
            div(class = "col-md-6",
              fileInput(ns("txt_file"),
                        "Subir archivo TXT de BanBaj\u00edo",
                        accept = ".txt",
                        buttonLabel = "Seleccionar...",
                        placeholder = "Ningún archivo seleccionado")
            ),
            # Hidden select — still needed for updateSelectInput() to work
            div(style = "display:none;",
              selectInput(ns("import_cuenta"), NULL,
                          choices = c("Cargando..." = ""), width = "100%")
            )
          ),

          # ── Smart company detection banner ──────────────────────────────────
          # Shown only after a file is loaded. Replaces the plain "Cuenta bancaria"
          # dropdown with a full-width confirmation card.
          uiOutput(ns("import_cuenta_banner")),

          uiOutput(ns("import_preview"))
        )
      ),

      # ── 3. Historial / Papelera ───────────────────────────────────────────
      bslib::nav_panel(
        title = "Historial / Papelera", value = "historial",
        bslib::navset_tab(
          bslib::nav_panel(
            "Historial de confirmaciones",
            div(class = "p-3", hd_table_ui(ns("historial")))
          ),
          bslib::nav_panel(
            "Papelera",
            tags$script(HTML(
              'Shiny.addCustomMessageHandler("bnc_collapse_reasignar", function(x) {
                var el = document.getElementById(x.target);
                if (el) {
                  var bsc = bootstrap.Collapse.getOrCreateInstance(el);
                  bsc.hide();
                }
              });'
            )),
            div(class = "p-3",
              # ── Reasignar cuenta de importación ─────────────────────────────
              tags$button(
                type = "button",
                class = "btn btn-outline-secondary btn-sm mb-2",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("reasignar_panel")),
                "\U0001F500 Reasignar cuenta de importaci\u00f3n"
              ),
              div(
                id    = ns("reasignar_panel"),
                class = "collapse border rounded p-3 mb-3 bg-light",
                tags$h6("Reasignar cuenta", class = "fw-semibold mb-3"),
                div(class = "row g-2 align-items-end mb-2",
                  div(class = "col-md-5",
                    tags$label("Sesi\u00f3n de importaci\u00f3n:",
                               class = "form-label small text-muted mb-1"),
                    selectInput(ns("reasig_sesion"), NULL,
                                choices = c("Sin sesiones" = ""), width = "100%")
                  ),
                  div(class = "col-md-3",
                    tags$label("Cuenta actual:",
                               class = "form-label small text-muted mb-1"),
                    div(
                      class = "form-control form-control-sm bg-white",
                      style = "cursor:default; pointer-events:none; color:#495057; min-height:38px; display:flex; align-items:center;",
                      textOutput(ns("reasig_cuenta_actual"), inline = TRUE)
                    )
                  ),
                  div(class = "col-md-4",
                    tags$label("Reasignar a:",
                               class = "form-label small text-muted mb-1"),
                    selectInput(ns("reasig_destino"), NULL,
                                choices = character(0), width = "100%")
                  )
                ),
                div(class = "mb-2",
                  hd_table_ui(ns("reasig"))
                ),
                div(class = "d-flex justify-content-between align-items-center",
                  uiOutput(ns("delete_sesion_btn_ui")),
                  uiOutput(ns("reasig_btn_ui"))
                )
              ),
              # ── Papelera info alert ──────────────────────────────────────────
              div(class = "alert alert-secondary small",
                icon("info-circle"),
                " Los elementos en papelera se conservan permanentemente",
                "para auditor\u00eda, pero nunca se muestran en otras vistas."
              ),
              hd_table_ui(ns("papelera"))
            )
          )
        )
      ),

      # ── 4. Cuentas ────────────────────────────────────────────────────────
      bslib::nav_panel(
        title = "Cuentas", value = "cuentas",
        div(class = "p-3",
          div(class = "alert alert-info d-flex align-items-center gap-3 mb-3",
            icon("circle-info"),
            div(
              tags$strong("Las cuentas se administran en Configuraci\u00f3n."),
              tags$br(),
              tags$span(class = "small",
                "Para agregar, editar o desactivar cuentas usa el m\u00f3dulo ",
                tags$strong("\u2699 Configuraci\u00f3n \u2192 Cuentas de Empresa.")
              )
            ),
            div(class = "ms-auto",
              actionButton(ns("open_settings_cuentas"),
                           tagList(icon("gear"), " Abrir Configuraci\u00f3n"),
                           class = "btn btn-sm btn-outline-primary",
                           onclick = "Shiny.setInputValue('btn_settings', Math.random(), {priority:'event'})")
            )
          ),
          hd_table_ui(ns("cuentas"))
        )
      )

    ) # end navset_tab
  )
}

# =============================================================================
# bancosServer
# =============================================================================
bancosServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Initialize S3-backed reactive vals ───────────────────────────────────
    # bancos_movimientos and bancos_confirmados may already be loaded by app.R.
    # ctas_cuentas (source of truth for accounts) is also loaded by app.R.
    # ctas_bancos is not in shared, so we load it once here.
    observe({
      if (is.null(shared$bancos_movimientos())) {
        shared$bancos_movimientos(tryCatch(
          load_bancos_movimientos(include_deleted = TRUE, client_id = shared$effective_client_id()),
          error = function(e) .schema_bancos_movimientos()
        ))
      }
      if (is.null(shared$bancos_confirmados())) {
        shared$bancos_confirmados(tryCatch(
          load_bancos_confirmados(client_id = shared$effective_client_id()), error = function(e) .schema_bancos_confirmados()
        ))
      }
      if (is.null(shared$conciliacion_rv())) {
        shared$conciliacion_rv(tryCatch(
          load_conciliacion(client_id = shared$effective_client_id()), error = function(e) .schema_conciliacion()
        ))
      }
    })

    # ── One-time cleanup: soft-delete any fuente="agenda" ghost rows ─────────
    # These were written by the old Confirmar pagos/cobros flow before the
    # wire-cut. They are excluded from movs_active() already, but this makes
    # the S3 file clean for any direct inspection or future data migration.
    # Runs once per session; no-op if no ghost rows exist.
    .agenda_cleanup_done <- FALSE
    observe({
      if (.agenda_cleanup_done) return()
      movs <- shared$bancos_movimientos()
      if (is.null(movs)) return()             # not loaded yet — wait
      movs_base <- movs   # session copy before this change (CM09)
      .agenda_cleanup_done <<- TRUE
      isolate({
        ghost_idx <- which(
          !is.na(movs$fuente) &
          movs$fuente == "agenda" &
          !movs$eliminado
        )
        if (!length(ghost_idx)) {
          message("[BANCOS] cleanup: no fuente='agenda' ghost rows found")
          return()
        }
        message(sprintf("[BANCOS] cleanup: soft-deleting %d fuente='agenda' ghost row(s)",
                        length(ghost_idx)))
        movs$eliminado[ghost_idx]    <- TRUE
        movs$eliminado_at[ghost_idx] <- Sys.time()
        shared$bancos_movimientos(movs)
        tryCatch({
          save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base)
          bump_sync_version("bancos_movimientos_db")
        }, error = function(e)
          message("[BANCOS] cleanup save error: ", e$message)
        )
        message("[BANCOS] cleanup: done — ghost rows marked eliminado=TRUE in S3")
      })
    })

    # ctas_bancos loaded once for bank-name resolution (not in shared)
    ctas_bancos_rv <- reactiveVal(NULL)
    observe({
      # Always establish a reactive dep on effective_client_id so ctas_bancos
      # reloads when the session context switches (client_id changes after login).
      # Without this, the NULL-guard fires once before login (wrong prefix) and
      # never re-fires, leaving all banco names as "—".
      cid <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
      ctas_bancos_rv(tryCatch(
        load_ctas_bancos(client_id = cid), error = function(e) .schema_ctas_bancos()
      ))
    })

    # ── Convenience accessors ────────────────────────────────────────────────
    # cuentas: derived from Settings ctas_cuentas — single source of truth.
    # cuenta_id in movimientos maps to ctas_cuentas$id.
    cuentas <- reactive({
      raw_cts <- tryCatch(
        if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
        else load_ctas_cuentas(client_id = shared$effective_client_id()),
        error = function(e) NULL
      )
      df      <- .ctas_to_bancos_cuentas(raw_cts, ctas_bancos_rv())
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed) && "empresa" %in% names(df) && nrow(df) > 0)
        df <- df[df$empresa %in% allowed, , drop = FALSE]
      df
    })

    movs_all   <- reactive({ shared$bancos_movimientos() %||% .schema_bancos_movimientos() })
    confirmados<- reactive({ shared$bancos_confirmados() %||% .schema_bancos_confirmados() })

    # Active movements for Libro de Banco calculations and display.
    # Excludes:
    #   - eliminado = TRUE  (soft-deleted rows)
    #   - fuente = "agenda" (rows written by old Confirmar pagos/cobros flow —
    #                        those are wire-cut; any survivors in S3 must not
    #                        affect balance calculations or appear in the table)
    movs_active <- reactive({
      dplyr::filter(movs_all(), !eliminado, fuente != "agenda")
    })

    # ── Update account selectors when cuentas change ─────────────────────────
    observe({
      ch_full  <- .cuenta_choices(cuentas(), include_sin_cuenta = TRUE, include_todas = TRUE)
      ch_import<- .cuenta_choices(cuentas(), include_sin_cuenta = FALSE)
      cur_sel  <- isolate(input$libro_cuenta) %||% "__todas__"
      updateSelectInput(session, "libro_cuenta",
        choices  = ch_full,
        selected = if (cur_sel %in% names(ch_full) || cur_sel %in% ch_full) cur_sel else "__todas__"
      )
      updateSelectInput(session, "import_cuenta", choices = ch_import)
    })

    # ── Update reasignar session choices ──────────────────────────────────────
    observe({
      movs     <- movs_all()
      txt_movs <- dplyr::filter(movs, fuente == "txt", !eliminado, !is.na(importado_at))
      if (!nrow(txt_movs)) {
        updateSelectInput(session, "reasig_sesion",
          choices = c("Sin sesiones de importaci\u00f3n" = ""))
        return()
      }
      cts       <- cuentas()
      alias_map <- setNames(as.character(cts$alias),   cts$cuenta_id)
      banco_map <- setNames(as.character(cts$banco),   cts$cuenta_id)
      mon_map   <- setNames(as.character(cts$moneda),  cts$cuenta_id)
      emp_map   <- setNames(as.character(cts$empresa), cts$cuenta_id)

      sessions <- txt_movs |>
        dplyr::mutate(sesion_key = as.character(importado_at)) |>
        dplyr::distinct(sesion_key, cuenta_id, importado_at) |>
        dplyr::arrange(dplyr::desc(importado_at))

      choices_labels <- vapply(seq_len(nrow(sessions)), function(i) {
        r   <- sessions[i, ]
        cid <- r$cuenta_id
        paste0(
          emp_map[cid] %||% cid, " \u2014 ",
          banco_map[cid] %||% "", " ",
          mon_map[cid] %||% "",
          " \u2014 ", format(r$importado_at, "%d/%m/%Y %H:%M")
        )
      }, character(1))

      updateSelectInput(session, "reasig_sesion",
        choices = setNames(sessions$sesion_key, choices_labels))
    })

    # ── Tab 1: Dashboard cards ────────────────────────────────────────────────
    output$dashboard_cards <- renderUI({
      cts <- dplyr::filter(cuentas(), activa == TRUE)
      if (!nrow(cts)) {
        return(div(class = "text-muted small py-2",
                   "No hay cuentas bancarias registradas. ",
                   "Ve a la pesta\u00f1a 'Cuentas' para agregar una."))
      }

      movs <- movs_active()

      # ── Current balance per account (saldo_inicial + all history) ────────────
      saldos <- movs |>
        dplyr::filter(!is.na(cuenta_id)) |>
        dplyr::group_by(cuenta_id) |>
        dplyr::summarise(
          total_cargo = sum(cargo, na.rm = TRUE),
          total_abono = sum(abono, na.rm = TRUE),
          .groups = "drop"
        )

      # ── 30-day rolling sparkline data per account ────────────────────────────
      hoy    <- Sys.Date()
      desde  <- hoy - 29L
      fechas <- seq.Date(desde, hoy, by = "day")

      spark_data <- function(cuenta_id_val) {
        mov_ct <- dplyr::filter(movs,
                                !is.na(cuenta_id),
                                cuenta_id == cuenta_id_val,
                                !is.na(fecha),
                                fecha >= desde)
        if (!nrow(mov_ct)) return(rep(NA_real_, 30L))

        daily <- mov_ct |>
          dplyr::group_by(fecha) |>
          dplyr::summarise(net = sum(abono, na.rm=TRUE) - sum(cargo, na.rm=TRUE),
                           .groups = "drop")

        # Cumulative net from beginning of history up to start of window
        base_movs <- dplyr::filter(movs,
                                   !is.na(cuenta_id),
                                   cuenta_id == cuenta_id_val,
                                   !is.na(fecha),
                                   fecha < desde)
        base_net <- if (nrow(base_movs))
          sum(base_movs$abono, na.rm=TRUE) - sum(base_movs$cargo, na.rm=TRUE)
        else 0

        ct_row <- cts[cts$cuenta_id == cuenta_id_val, , drop=FALSE]
        saldo_ini <- if (nrow(ct_row)) (ct_row$saldo_inicial %||% 0)[1] else 0

        # Build day-by-day running balance, carry forward on missing days
        vals <- numeric(30L)
        running <- saldo_ini + base_net
        for (k in seq_along(fechas)) {
          d <- fechas[k]
          day_row <- dplyr::filter(daily, fecha == d)
          if (nrow(day_row)) running <- running + day_row$net[1]
          vals[k] <- running
        }
        vals
      }

      # ── Build sparkline SVG ───────────────────────────────────────────────────
      make_spark_svg <- function(vals, color) {
        vals_clean <- vals
        # Forward-fill NAs
        last <- NA_real_
        for (k in seq_along(vals_clean)) {
          if (!is.na(vals_clean[k])) last <- vals_clean[k]
          else if (!is.na(last)) vals_clean[k] <- last
        }
        if (all(is.na(vals_clean))) {
          return(tags$svg(class="bnc-spark",
                          viewBox="0 0 120 28", preserveAspectRatio="none"))
        }
        mn <- min(vals_clean, na.rm=TRUE)
        mx <- max(vals_clean, na.rm=TRUE)
        rng <- if (mx == mn) 1 else mx - mn
        n  <- length(vals_clean)
        # Map to SVG coords: x 0-120, y 4-24 (inverted — higher value = lower y)
        xs <- round(seq(0, 120, length.out = n), 1)
        ys <- round(4 + (1 - (vals_clean - mn) / rng) * 20, 1)
        pts <- paste(xs, ys, sep=",", collapse=" ")
        last_x <- xs[n]; last_y <- ys[n]
        dot_color <- if (vals_clean[n] < 0) "#dc3545" else color
        tags$svg(
          class="bnc-spark", viewBox="0 0 120 28",
          preserveAspectRatio="none",
          tags$polyline(
            fill="none", stroke=color,
            `stroke-width`="1.5", `stroke-opacity`="0.35",
            points=pts
          ),
          tags$circle(
            cx=last_x, cy=last_y, r="2",
            fill=dot_color, opacity="0.85"
          )
        )
      }

      # ── Render cards ─────────────────────────────────────────────────────────
      cards <- lapply(seq_len(nrow(cts)), function(i) {
        ct      <- cts[i, ]
        row     <- dplyr::filter(saldos, cuenta_id == ct$cuenta_id)
        saldo   <- (ct$saldo_inicial %||% 0) +
                   (if (nrow(row)) row$total_abono - row$total_cargo else 0)
        color   <- .currency_color(ct$moneda)
        is_neg  <- saldo < 0
        bal_lbl <- if (is_neg)
          paste0("- ", fmt_money(abs(saldo)))
        else
          fmt_money(saldo)

        spark_vals <- spark_data(ct$cuenta_id)
        spark_svg  <- make_spark_svg(spark_vals, color)

        detalle <- paste0(ct$banco, " \u00b7 ", ct$moneda, " \u00b7 ", ct$alias)
        div(
          class = "bnc-card",
          style = paste0("border-left-color:", color, ";"),
          # Two lines (T09b): company and balance; bank and sparkline. The
          # bank line can be cut: its title shows it whole.
          div(class = "bnc-card-l1",
            span(class = "bnc-card-emp", ct$empresa),
            span(class = paste("bnc-card-bal", if (is_neg) "bnc-neg-bal" else ""), bal_lbl)),
          div(class = "bnc-card-l2",
            span(class = "bnc-card-mon", style = paste0("color:", color, ";"), title = detalle, detalle),
            div(class = "bnc-spark-wrap", spark_svg))
        )
      })

      div(class = "bnc-dashboard", !!!cards)
    })

    output$import_balance_band <- renderUI({
      cts <- dplyr::filter(cuentas(), activa == TRUE)
      if (!nrow(cts)) return(NULL)

      movs <- movs_active()

      saldos <- movs |>
        dplyr::filter(!is.na(cuenta_id)) |>
        dplyr::group_by(cuenta_id) |>
        dplyr::summarise(
          total_cargo = sum(cargo, na.rm = TRUE),
          total_abono = sum(abono, na.rm = TRUE),
          .groups = "drop"
        )

      items <- lapply(seq_len(nrow(cts)), function(i) {
        ct    <- cts[i, ]
        row   <- dplyr::filter(saldos, cuenta_id == ct$cuenta_id)
        saldo <- ct$saldo_inicial +
                  (if (nrow(row)) row$total_abono - row$total_cargo else 0)
        cls   <- if (saldo > 0) "pos" else if (saldo < 0) "neg" else "zero"
        mon   <- trimws(ct$moneda %||% "")

        div(class = "bnc-ticker-item",
          div(class = paste0("bnc-ticker-dot ", cls)),
          div(
            div(class = "bnc-ticker-label",
                paste0(.company_name(ct$empresa, shared$company_map()), " \u00b7 ", ct$alias)),
            div(
              tags$span(class = paste("bnc-ticker-val", cls), fmt_money(saldo)),
              tags$span(class = "bnc-ticker-mon", mon)
            )
          )
        )
      })

      div(class = "bnc-ticker", !!!items)
    })

    # ── Tab 1: Libro de Banco (hd_table "libro_banco", T12a) ─────────────────
    # The bar's own filters (account, period, flow, commissions) cut the rows;
    # the engine searches, sorts, marks (by id), sums and exports them.
    libro_movs <- reactive({
      cuenta_sel <- input$libro_cuenta %||% "__todas__"
      periodo    <- input$libro_periodo %||% "mes_actual"
      show_com   <- isTRUE(input$mostrar_comisiones)
      flujo      <- input$flujo_filter  %||% "todos"
      rango      <- input$libro_rango

      movs <- movs_active()

      # Filter by account (__todas__ = no filter)
      if (cuenta_sel == "__sin_cuenta__") {
        movs <- dplyr::filter(movs, is.na(cuenta_id) | cuenta_id == "")
      } else if (nzchar(cuenta_sel) && cuenta_sel != "__todas__") {
        movs <- dplyr::filter(movs, cuenta_id == cuenta_sel)
      }

      movs <- .filter_periodo(movs, periodo,
        desde = if (length(rango) >= 1) rango[1] else NULL,
        hasta = if (length(rango) >= 2) rango[2] else NULL
      )
      movs <- dplyr::arrange(movs, dplyr::desc(fecha), dplyr::desc(hora))

      if (!show_com) {
        movs <- dplyr::filter(movs, tipo != "comision")
      }

      # Flow filter (replaces the blanket cargo>0|abono>0 baseline)
      movs <- switch(flujo,
        "egresos"  = dplyr::filter(movs, cargo > 0),
        "ingresos" = dplyr::filter(movs, abono > 0),
        dplyr::filter(movs, cargo > 0 | abono > 0)
      )

      # Join cuentas for empresa + moneda (alias used as fallback when empresa is blank)
      cts_disp <- dplyr::select(cuentas(), cuenta_id, empresa, alias, moneda)
      dplyr::left_join(movs, cts_disp, by = "cuenta_id")
    })

    # Who can't change Bancos doesn't see the Libro's buttons that write:
    # the rows' ✎ (.acciones), the bar's Eliminar and ＋ Agregar. It follows
    # the session's user (ver como, a jump). The server checks
    # (puede_o_avisa() in each write observer) stay as the lock.
    .bnc_puede_ver_escritura <- reactive(isTRUE(tryCatch(puede(shared$usuario(), "can_manage_bancos"),
                                                         error = function(e) FALSE)))
    observe({
      ok <- .bnc_puede_ver_escritura()
      shinyjs::toggle("add_mov_manual", condition = ok)
      shinyjs::toggleClass("libro_wrap", "bnc-solo-lectura", condition = !ok)
    })

    libro_datos <- reactive({
      movs <- libro_movs()
      if (!nrow(movs)) return(.bnc_libro_vacia)
      empresa <- {
        ini <- dplyr::na_if(trimws(dplyr::coalesce(movs$empresa, "")), "")
        nm  <- .cmap_lookup(shared$company_map(), dplyr::coalesce(ini, ""))
        dplyr::coalesce(ifelse(!is.na(nm), unname(nm), NA_character_), ini, movs$alias)
      }
      data.frame(
        id          = movs$id,
        Empresa     = empresa,
        FechaHora   = as.POSIXct(paste(format(movs$fecha, "%Y-%m-%d"), dplyr::coalesce(movs$hora, "00:00:00"))),
        Moneda      = toupper(trimws(dplyr::coalesce(movs$moneda, ""))),
        # A commission or a reconciled row used to be a grey or green row;
        # the engine shows it as a badge (Luis, T12 P1).
        Tipo        = ifelse(movs$tipo == "comision", "comision",
                        ifelse(!is.na(movs$conciliado) & movs$conciliado, "conciliado", "")),
        Descripcion = dplyr::coalesce(dplyr::na_if(trimws(movs$descripcion_raw %||% ""), ""), "\u2014"),
        Recibo      = as.character(movs$recibo),
        Cargo       = ifelse(movs$cargo > 0, movs$cargo, NA_real_),
        Abono       = ifelse(movs$abono > 0, movs$abono, NA_real_),
        Saldo       = movs$saldo_banco,
        # ✎ only on manual entries (imported TXT rows can't be edited), and
        # only for who can change Bancos.
        .acciones   = ifelse(!is.na(movs$fuente) & movs$fuente == "manual" & .bnc_puede_ver_escritura(), "editar", ""),
        stringsAsFactors = FALSE, check.names = FALSE)
    })

    # Empresa only while every account shows (one account = one company).
    libro_columnas <- reactive({
      todas <- (input$libro_cuenta %||% "__todas__") == "__todas__"
      c(if (todas) list(col_empresa("Empresa", "tablas.facturas.empresa")),
        list(col_fecha_hora("FechaHora", "tablas.bancos.fecha_hora"),
             col_texto("Moneda", "tablas.facturas.moneda", recorte = FALSE),
             col_insignia("Tipo", "tablas.facturas.tipo", mapa = list(
               comision   = list(texto = "tablas.bancos.comision",   tono = "neutro"),
               conciliado = list(texto = "tablas.bancos.conciliado", tono = "exito"))),
             col_texto("Descripcion", "tablas.bancos.descripcion"),
             col_texto("Recibo", "tablas.bancos.recibo", recorte = FALSE, opcional = TRUE, visible = FALSE),
             col_dinero("Cargo", "tablas.bancos.cargo", moneda = "Moneda"),
             col_dinero("Abono", "tablas.bancos.abono", moneda = "Moneda"),
             col_dinero("Saldo", "tablas.bancos.saldo", moneda = "Moneda", sumar = FALSE)))
    })

    t_libro <- hd_table_server("libro", tabla = "libro_banco", datos = libro_datos, columnas = libro_columnas,
                               piezas = c("sel_fila", "sel_rango", "sumador_sel", "sumador_vis", "columnas_opc",
                                          "recorte", "acc_fila", "barra_acc"),
                               acciones_fila = "editar",
                               acciones_barra = list(list(accion = "eliminar", texto = "tablas.bancos.eliminar",
                                                          peligro = TRUE)),
                               vacio = "tablas.bancos.vacio_libro", visibles = TRUE)

    # Both Libro Excels are logged as a generated file ("bancos_libro_excel"),
    # with the bar's filters and which one (formato: "tabla" / "banbajio").
    .bnc_log_libro_excel <- function(formato, n_items, empresa, moneda, fname) {
      log_archivo_generado(
        shared, tipo = "bancos_libro_excel", empresa = empresa, moneda = moneda,
        detalle_list = list(formato = formato, cuenta = input$libro_cuenta %||% "__todas__",
                            periodo = input$libro_periodo %||% "mes_actual",
                            flujo = input$flujo_filter %||% "todos",
                            mostrar_comisiones = isTRUE(input$mostrar_comisiones)),
        n_items = n_items, fname = fname)
    }

    # The engine's Exportar a Excel. It sends the exported ids in
    # libro-vista just before every download.
    # TODO t$exportado(): read the engine's hook once it has one (ejecutor 2).
    observeEvent(input[["libro-vista"]], {
      ids   <- as.character(unlist(input[["libro-vista"]]$ids))
      movs  <- libro_movs()
      movs  <- movs[movs$id %in% ids, , drop = FALSE]
      todas <- (input$libro_cuenta %||% "__todas__") == "__todas__"
      mon   <- toupper(trimws(movs$moneda[1] %||% ""))
      .bnc_log_libro_excel("tabla", n_items = length(ids),
        empresa = if (todas) "Todas" else (movs$empresa[1] %||% NA_character_),
        moneda  = if (todas || !nzchar(mon)) NA_character_ else mon,
        fname   = paste0("libro_banco_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
    }, ignoreInit = TRUE)

    # "Exportar (formato BanBajío)": the Libro in the bank's own layout, used
    # to reconcile and for accounting (Luis, T12a). The format lives in
    # BANCOS_FORMATOS_EXPORT (R/bancos_formatos.R); the engine's Excel (the
    # table's columns) stays too.
    output$download_libro <- downloadHandler(
      filename = function() {
        cuenta_sel <- input$libro_cuenta %||% "__todas__"
        cts        <- cuentas()
        alias_str  <- if (cuenta_sel == "__todas__") {
          "todas"
        } else if (nzchar(cuenta_sel) && cuenta_sel != "__sin_cuenta__") {
          row <- dplyr::filter(cts, cuenta_id == cuenta_sel)
          if (nrow(row)) gsub("[^A-Za-z0-9_-]", "_", row$alias[1]) else "banco"
        } else "banco"
        BANCOS_FORMATOS_EXPORT$banbajio$archivo(alias_str)
      },
      content = function(file) {
        cuenta_sel <- input$libro_cuenta %||% "__todas__"

        # The Libro's rows (the module's filters, accounts joined), narrowed
        # to what the table's search or filters leave showing.
        movs <- libro_movs()
        vis  <- t_libro$visibles()
        if (!is.null(vis)) movs <- movs[movs$id %in% vis, , drop = FALSE]
        todas_exp <- cuenta_sel == "__todas__"

        # Written by the format's own entry (R/bancos_formatos.R).
        formato <- BANCOS_FORMATOS_EXPORT$banbajio
        res <- formato$escribir(movs, file, todas = todas_exp, company_map = shared$company_map())

        .bnc_log_libro_excel("banbajio", n_items = res$n, empresa = res$empresa, moneda = res$moneda,
          fname = paste0("movimientos_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
      }
    )

    # Eliminar (bar) opens the confirmation with the marked ids; ✎ opens
    # the manual-entry form for that row.
    observeEvent(t_libro$accion(), {
      a <- t_libro$accion()
      if (identical(a$accion, "eliminar")) .bnc_pedir_eliminar(a$ids)
      else if (identical(a$accion, "editar") && length(a$ids)) .bnc_editar(a$ids[1])
    }, ignoreInit = TRUE)

    # ── Reasignar cuenta reactives ────────────────────────────────────────────
    reasig_confirm_data_rv    <- reactiveVal(NULL)
    delete_sesion_confirm_rv  <- reactiveVal(NULL)
    elim_ids_rv               <- reactiveVal(NULL)

    # ── Import reactives ──────────────────────────────────────────────────────
    # Raw TXT lines of the current upload; the banner derives empresa,
    # account and currency from them (detectar_cuenta_banbajio).
    txt_lines_rv         <- reactiveVal(character(0))
    # TRUE once the user has consciously chosen an account in the banner dropdown.
    # Reset to FALSE on each new file upload so the best-match auto-selects again.
    user_picked_cuenta_rv <- reactiveVal(FALSE)

    # A Bancos save (rule 8): a failed one is shown to the user and logged
    # (<accion>_fallido), never silent and never announced as done. Callers
    # save BEFORE touching the session's copy, closing the modal or saying
    # it worked, and stop when this returns FALSE. `aviso`: the dictionary
    # key the user sees.
    .bnc_guardar <- function(accion, descripcion, escribir, aviso = "comun.error_guardar") {
      error <- tryCatch({ escribir(); NULL }, error = function(e) e)
      if (is.null(error)) return(TRUE)
      message("[bancos] ", accion, " failed: ", conditionMessage(error))
      showNotification(tr(aviso), type = "error", duration = 8)
      tryCatch(log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = paste0(accion, "_fallido"),
        description = paste0(descripcion, ": ", conditionMessage(error)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      ), error = function(e) NULL)
      FALSE
    }

    # ── Eliminar batch ────────────────────────────────────────────────────────
    # The Libro's bar sends the marked ids; the confirmation lists the ones
    # still there (hd_table "eliminar_mov").
    elim_filas_rv <- reactiveVal(NULL)
    t_elim <- hd_table_server("elim", tabla = "eliminar_mov",
                              datos = reactive(elim_filas_rv() %||% .bnc_elim_vacia),
                              columnas = list(col_fecha("Fecha", "tablas.bancos.fecha"),
                                              col_texto("Parte", "tablas.facturas.parte"),
                                              col_dinero("Cargo", "tablas.bancos.cargo", moneda = "Moneda"),
                                              col_dinero("Abono", "tablas.bancos.abono", moneda = "Moneda")))
    .bnc_pedir_eliminar <- function(ids) {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      req(length(ids) > 0)
      movs <- movs_all()
      rows <- dplyr::filter(movs, id %in% ids, !eliminado)
      if (!nrow(rows)) return()

      total_cargo <- sum(rows$cargo, na.rm = TRUE)
      total_abono <- sum(rows$abono, na.rm = TRUE)
      n           <- nrow(rows)

      mon <- dplyr::left_join(dplyr::select(rows, id, cuenta_id), dplyr::select(cuentas(), cuenta_id, moneda), by = "cuenta_id")
      elim_filas_rv(data.frame(
        id = rows$id, Fecha = rows$fecha, Parte = rows$parte %||% "",
        Cargo = ifelse(rows$cargo > 0, rows$cargo, NA_real_), Abono = ifelse(rows$abono > 0, rows$abono, NA_real_),
        Moneda = toupper(trimws(dplyr::coalesce(mon$moneda[match(rows$id, mon$id)], ""))),
        stringsAsFactors = FALSE))

      elim_ids_rv(ids)

      showModal(modalDialog(
        title = sprintf("\u00bfEliminar %d movimiento%s?", n, if (n == 1) "" else "s"),
        size  = "m", easyClose = TRUE,
        div(
          hd_table_ui(ns("elim")),
          tags$hr(),
          div(class = "d-flex justify-content-between small text-muted",
            span(paste("Total cargos:", fmt_money(total_cargo))),
            span(paste("Total abonos:", fmt_money(total_abono)))
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_eliminar_confirm"), "Eliminar",
                       class = "btn btn-danger")
        )
      ))
    }

    observeEvent(input$do_eliminar_confirm, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      shinyjs::disable("do_eliminar_confirm")
      on.exit(shinyjs::enable("do_eliminar_confirm"), add = TRUE)

      ids <- elim_ids_rv()
      req(length(ids) > 0)

      movs <- movs_all()
      movs_base <- movs   # session copy before this change (CM09)
      # Only rows still not deleted: another session may have deleted some
      # while this confirmation was open. Re-deleting them rewrote
      # eliminado_at and archived them a second time in bancos_papelera
      # (T12 inventory, VP4).
      idx  <- which(movs$id %in% ids & !(movs$eliminado %in% TRUE))
      if (!length(idx)) {
        elim_ids_rv(NULL)
        removeModal()
        showNotification(tr("tablas.bancos.ya_eliminados"), type = "warning", duration = 6)
        return()
      }

      # Snapshot the pre-delete rows BEFORE flipping eliminado, so the
      # bancos_papelera archive event (below) records the movement as it
      # actually looked at the moment of deletion, not post-mutation.
      del_rows <- movs[idx, , drop = FALSE]
      actor    <- tryCatch(shared$current_user(), error = function(e) "system")

      movs$eliminado[idx]    <- TRUE
      movs$eliminado_at[idx] <- Sys.time()
      # Saved first: if S3 refuses, nothing changes in the session, the
      # confirmation stays open and nothing says it was deleted (T12a).
      if (!.bnc_guardar("eliminar_movimientos", sprintf("Eliminar %d movimiento(s)", length(idx)),
                        function() save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base))) return()
      # Only now are these ids done with: a failed save keeps them, so the
      # same confirmation can try again.
      elim_ids_rv(NULL)
      shared$bancos_movimientos(movs)
      bump_sync_version("bancos_movimientos_db")

      # found 2026-08-03 (Stage 9, Issue C): Mouse asked for a dedicated,
      # separate backend store for deleted bank movements (see
      # R/bancos_persistence.R's "bancos_papelera" section for the full
      # reasoning). This is additive -- the eliminado flag above remains the
      # mechanism that hides the row from the live Libro de Banco view; this
      # writes a genuinely separate, permanent, restorable archive event.
      # The movements are already deleted; if their archive can't be written
      # the user is told so and it is logged (it used to fail silently).
      # (A failed read inside load_bancos_papelera() still comes back as an
      # empty table: that, and saving without a base, is the integrator's
      # VP3 in persistence, not this module's.)
      .bnc_guardar("archivar_movimientos_eliminados",
                   sprintf("Archivar %d movimiento(s) eliminados en la papelera de Bancos", nrow(del_rows)),
                   function() {
                     bpap <- load_bancos_papelera(client_id = shared$effective_client_id())
                     bpap <- add_to_bancos_papelera(bpap, del_rows, actor = actor)
                     save_bancos_papelera(bpap, client_id = shared$effective_client_id())
                   }, aviso = "tablas.bancos.error_archivar")

      removeModal()
      n <- length(idx)
      showNotification(
        sprintf("%d movimiento%s eliminado%s.", n, if (n==1)"" else "s", if (n==1)"" else "s"),
        type = "message", duration = 3
      )
      log_action(
        user        = actor,
        module      = "bancos",
        action      = "eliminar_movimientos",
        description = paste0(n, " movimiento(s) enviado(s) a papelera"),
        metadata    = c(list(n = n), .bancos_audit_rows_summary(del_rows)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
      # Clear JS selection
      session$sendCustomMessage(paste0(id, "-clear_selection"), TRUE)
    }, ignoreInit = TRUE)

    # ── Manual entry modal ────────────────────────────────────────────────────
    # editing_mov_id: NULL = "Agregar" (insert a new row); otherwise the id of
    # the manual row being edited via the "Editar" button (2026-07-30, the
    # Vantage Bank manual-entry correction tool -- scoped to fuente=="manual"
    # rows only, never TXT-imported ones, which could fight a future
    # re-import of the same statement).
    editing_mov_id <- reactiveVal(NULL)

    .show_manual_mov_modal <- function(prefill = NULL) {
      cts <- cuentas()
      cuenta_ch <- c(
        .cuenta_choices(cts, include_sin_cuenta = FALSE),
        "Sin cuenta registrada" = "__sin_cuenta__"
      )
      is_edit <- !is.null(prefill)

      showModal(modalDialog(
        title = if (is_edit) "Editar movimiento manual" else "Agregar movimiento manual",
        size  = "m", easyClose = TRUE,
        div(class = "row g-2",
          div(class = "col-md-6",
            tags$label("Cuenta", class = "form-label small"),
            selectInput(ns("man_cuenta"), NULL, choices = cuenta_ch,
                        selected = prefill$cuenta_id %||% "__sin_cuenta__", width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Fecha", class = "form-label small"),
            dateInput(ns("man_fecha"), NULL, value = prefill$fecha %||% Sys.Date(), width = "100%",
                      format = "dd/mm/yyyy", language = "es")
          ),
          div(class = "col-md-6",
            tags$label("Tipo", class = "form-label small"),
            selectInput(ns("man_tipo"), NULL,
                        choices = setNames(names(.TIPO_LABELS), .TIPO_LABELS),
                        selected = prefill$tipo %||% "manual_in", width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Parte (beneficiario/ordenante)", class = "form-label small"),
            textInput(ns("man_parte"), NULL, value = prefill$parte %||% "",
                      placeholder = "Nombre...", width = "100%")
          ),
          div(class = "col-12",
            tags$label("Concepto", class = "form-label small"),
            textInput(ns("man_concepto"), NULL, value = prefill$concepto %||% "",
                      placeholder = "Descripción...", width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Cargo ($)", class = "form-label small"),
            numericInput(ns("man_cargo"), NULL, value = prefill$cargo %||% 0, min = 0, step = 100,
                         width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Abono ($)", class = "form-label small"),
            numericInput(ns("man_abono"), NULL, value = prefill$abono %||% 0, min = 0, step = 100,
                         width = "100%")
          ),
          div(class = "col-12",
            tags$label("Notas (opcional)", class = "form-label small"),
            textAreaInput(ns("man_notas"), NULL, value = prefill$notas %||% "", rows = 2, width = "100%",
                          resize = "none")
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_add_manual"), "Guardar", class = "btn btn-primary")
        )
      ))
    }

    observeEvent(input$add_mov_manual, {
      editing_mov_id(NULL)
      .show_manual_mov_modal()
    }, ignoreInit = TRUE)

    .bnc_editar <- function(mov_id) {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      req(mov_id)
      movs <- movs_all()
      row  <- movs[!is.na(movs$id) & movs$id == mov_id &
                   movs$fuente == "manual" & !movs$eliminado, , drop = FALSE]
      if (!nrow(row)) {
        showNotification("Solo se pueden editar movimientos manuales (no importados de TXT).",
                         type = "warning")
        return()
      }
      editing_mov_id(mov_id)
      .show_manual_mov_modal(prefill = as.list(row[1, ]))
    }

    observeEvent(input$do_add_manual, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      shinyjs::disable("do_add_manual")
      on.exit(shinyjs::enable("do_add_manual"), add = TRUE)

      cuenta_id_sel <- input$man_cuenta %||% "__sin_cuenta__"
      tipo_sel      <- input$man_tipo   %||% "manual_in"
      cargo_v       <- as.numeric(input$man_cargo %||% 0)
      abono_v       <- as.numeric(input$man_abono %||% 0)
      edit_id       <- editing_mov_id()
      is_edit       <- !is.null(edit_id)

      movs   <- movs_all()
      movs_base <- movs   # session copy before this change (CM09)
      before <- NULL

      if (is_edit) {
        idx <- which(!is.na(movs$id) & movs$id == edit_id & movs$fuente == "manual")
        if (!length(idx)) {
          showNotification("El movimiento ya no existe (pudo haber sido eliminado en otra sesión).",
                           type = "warning")
          removeModal(); editing_mov_id(NULL)
          return()
        }
        before <- as.list(movs[idx[1], c("cuenta_id", "fecha", "tipo", "parte",
                                         "concepto", "cargo", "abono", "notas")])
        movs$cuenta_id[idx]       <- if (cuenta_id_sel == "__sin_cuenta__") NA_character_ else cuenta_id_sel
        movs$fecha[idx]           <- as.Date(input$man_fecha)
        movs$descripcion_raw[idx] <- trimws(input$man_concepto %||% "")
        movs$tipo[idx]            <- tipo_sel
        movs$parte[idx]           <- trimws(input$man_parte %||% "")
        movs$concepto[idx]        <- trimws(input$man_concepto %||% "")
        movs$cargo[idx]           <- if (is.na(cargo_v)) 0 else cargo_v
        movs$abono[idx]           <- if (is.na(abono_v)) 0 else abono_v
        movs$notas[idx]           <- trimws(input$man_notas %||% "")
        target_row <- movs[idx[1], ]
      } else {
        new_mov <- tibble::tibble(
          id              = uuid::UUIDgenerate(),
          cuenta_id       = if (cuenta_id_sel == "__sin_cuenta__") NA_character_
                            else cuenta_id_sel,
          fecha           = as.Date(input$man_fecha),
          hora            = format(Sys.time(), "%H:%M:%S"),
          recibo          = NA_character_,
          descripcion_raw = trimws(input$man_concepto %||% ""),
          tipo            = tipo_sel,
          parte           = trimws(input$man_parte    %||% ""),
          rfc             = NA_character_,
          referencia      = NA_character_,
          clave_rastreo   = NA_character_,
          concepto        = trimws(input$man_concepto %||% ""),
          cargo           = if (is.na(cargo_v)) 0 else cargo_v,
          abono           = if (is.na(abono_v)) 0 else abono_v,
          saldo_banco     = NA_real_,
          conciliado      = FALSE,
          doc_vinculado   = NA_character_,
          agenda_id       = NA_character_,
          importado_at    = Sys.time(),
          fuente          = "manual",
          eliminado       = FALSE,
          notas           = trimws(input$man_notas %||% "")
        )
        movs <- dplyr::bind_rows(movs, new_mov)
        target_row <- new_mov
      }

      # Saved first (rule 8, T12a): on failure nothing changes in the
      # session and nothing says it worked.
      if (!.bnc_guardar("guardar_movimiento_manual", paste0(if (is_edit) "Editar" else "Agregar", " movimiento manual"),
                        function() save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base))) return()
      shared$bancos_movimientos(movs)
      bump_sync_version("bancos_movimientos_db")
      removeModal()
      showNotification(if (is_edit) "Movimiento actualizado." else "Movimiento manual guardado.",
                       type = "message", duration = 2)

      log_desc <- sprintf("Movimiento manual %s: %s %s por $%s",
                          if (is_edit) "editado" else "agregado",
                          .TIPO_LABELS[[tipo_sel]] %||% tipo_sel,
                          target_row$parte[1],
                          format(max(target_row$cargo[1], target_row$abono[1]), big.mark = ",", nsmall = 2))
      log_meta <- if (is_edit) {
        after <- list(cuenta_id = target_row$cuenta_id[1], fecha = as.character(target_row$fecha[1]),
                      tipo = tipo_sel, parte = target_row$parte[1], concepto = target_row$concepto[1],
                      cargo = target_row$cargo[1], abono = target_row$abono[1], notas = target_row$notas[1])
        list(before = before, after = after)
      } else {
        list(cuenta_id = target_row$cuenta_id[1], tipo = tipo_sel,
             cargo = target_row$cargo[1], abono = target_row$abono[1])
      }
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = if (is_edit) "editar_movimiento_manual" else "agregar_movimiento_manual",
        description = log_desc,
        target_id   = target_row$id[1],
        metadata    = log_meta,
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
      editing_mov_id(NULL)
    }, ignoreInit = TRUE)

    # ── Tab 3: Reasignar cuenta (Papelera panel) ─────────────────────────────
    output$reasig_cuenta_actual <- renderText({
      sesion_key <- input$reasig_sesion %||% ""
      if (!nzchar(sesion_key)) return("\u2014")
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      if (!any(mask)) return("\u2014")
      cid <- movs$cuenta_id[mask][1]
      if (is.na(cid) || !nzchar(cid %||% "")) return("\u2014")
      cts <- cuentas()
      row <- dplyr::filter(cts, cuenta_id == cid)
      if (!nrow(row)) return(cid)
      paste0(row$empresa[1], " \u2014 ", row$banco[1], " ",
             row$moneda[1], " (", row$alias[1], ")")
    })

    observe({
      sesion_key <- input$reasig_sesion %||% ""
      cts <- cuentas()
      if (!nzchar(sesion_key) || !nrow(cts)) {
        updateSelectInput(session, "reasig_destino", choices = character(0))
        return()
      }
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      current_cid <- if (any(mask)) movs$cuenta_id[mask][1] else ""
      active_cts  <- dplyr::filter(cts, activa == TRUE)
      if (nzchar(current_cid %||% ""))
        active_cts <- dplyr::filter(active_cts, cuenta_id != current_cid)
      if (!nrow(active_cts)) {
        updateSelectInput(session, "reasig_destino",
          choices = c("Sin otras cuentas activas" = ""))
        return()
      }
      ch <- setNames(active_cts$cuenta_id,
        paste0(active_cts$empresa, " \u2014 ", active_cts$banco, " ",
               active_cts$moneda, " (", active_cts$alias, ")"))
      updateSelectInput(session, "reasig_destino", choices = ch)
    })

    # Reasignar's preview (hd_table "reasignar", T12b): the chosen import
    # session's movements, all of them (it used to page by 10).
    t_reasig <- hd_table_server("reasig", tabla = "reasignar",
      datos = reactive({
        sesion_key <- input$reasig_sesion %||% ""
        if (!nzchar(sesion_key)) return(.bnc_previa_datos(NULL))
        movs <- movs_all()
        mask <- !is.na(movs$importado_at) &
                as.character(movs$importado_at) == sesion_key &
                !is.na(movs$eliminado) & movs$eliminado == FALSE
        .bnc_previa_datos(movs[mask, , drop = FALSE], cuentas())
      }),
      columnas = .BNC_COLUMNAS_PREVIA, vacio = "tablas.bancos.vacio_reasignar")

    output$reasig_btn_ui <- renderUI({
      sesion_key <- input$reasig_sesion %||% ""
      dest_cid   <- input$reasig_destino %||% ""
      if (!nzchar(sesion_key)) return(NULL)
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      n <- sum(mask)
      btn_disabled <- n == 0 || !nzchar(dest_cid)
      btn <- actionButton(ns("btn_reasignar"),
        label = paste0("Reasignar ", n, " movimientos \u2192"),
        class = "btn btn-primary btn-sm")
      if (btn_disabled)
        div(style = "pointer-events:none; opacity:.65; display:inline-block;", btn)
      else
        btn
    })

    observeEvent(input$btn_reasignar, {
      sesion_key <- input$reasig_sesion %||% ""
      new_cid    <- input$reasig_destino %||% ""
      req(nzchar(sesion_key), nzchar(new_cid))
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      n <- sum(mask)
      if (n == 0) return()
      current_cid <- movs$cuenta_id[mask][1]
      cts <- cuentas()
      fmt_acct <- function(cid) {
        row <- dplyr::filter(cts, cuenta_id == cid)
        if (!nrow(row)) return(cid %||% "desconocida")
        paste0(row$empresa[1], " \u2014 ", row$banco[1], " ",
               row$moneda[1], " (", row$alias[1], ")")
      }
      cur_label  <- fmt_acct(current_cid)
      dest_label <- fmt_acct(new_cid)
      reasig_confirm_data_rv(list(
        sesion_key = sesion_key, new_cid = new_cid,
        n = n, cur_label = cur_label, dest_label = dest_label
      ))
      showModal(modalDialog(
        title = paste0("\u00bfReasignar ", n, " movimiento",
                       if (n == 1) "" else "s", "?"),
        size  = "m", easyClose = TRUE,
        div(
          p(class = "mb-2",
            paste0("\u00bfReasignar ", n, " movimiento",
                   if (n == 1) "" else "s", " de")),
          p(class = "text-center",
            tags$span(class = "badge bg-secondary", cur_label),
            tags$span(class = "mx-2 fw-bold", "\u2192"),
            tags$span(class = "badge bg-primary", dest_label)
          ),
          tags$hr(),
          p(class = "text-muted small mb-0",
            "Esta acci\u00f3n actualiza cuenta_id en todos los movimientos de esta sesi\u00f3n.",
            tags$br(),
            "No se puede deshacer autom\u00e1ticamente.")
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_reasignar_confirm"), "Confirmar reasignaci\u00f3n",
                       class = "btn btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$do_reasignar_confirm, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      shinyjs::disable("do_reasignar_confirm")
      on.exit(shinyjs::enable("do_reasignar_confirm"), add = TRUE)

      data <- reasig_confirm_data_rv()
      req(data)
      sesion_key <- data$sesion_key
      new_cid    <- data$new_cid
      dest_label <- data$dest_label
      movs <- movs_all()
      movs_base <- movs   # session copy before this change (CM09)
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      actual_n <- sum(mask)
      if (actual_n == 0) { removeModal(); return() }

      # Snapshot before mutation -- same reasoning as del_rows in
      # do_eliminar_confirm above -- so the audit entry's row detail reflects
      # what was actually reassigned, not the post-mutation state.
      reasig_rows <- movs[mask, , drop = FALSE]

      # Step 1: Reassign cuenta_id for the whole batch
      movs$cuenta_id[mask] <- new_cid

      # Step 2: Dedup the reassigned batch against existing target-account movements.
      # Pass all rows NOT in the batch as `existentes`; dedup_movimientos() scopes
      # internally to new_cid, so movements from other accounts are ignored.
      batch_ids        <- movs$id[mask]
      nuevos_batch     <- movs[mask, ]
      existentes_rest  <- movs[!mask, ]
      dedup_res        <- dedup_movimientos(nuevos_batch, existentes_rest)
      dup_ids          <- setdiff(batch_ids, dedup_res$nuevos$id)
      n_deduped        <- length(dup_ids)
      if (n_deduped > 0) {
        movs$eliminado[movs$id %in% dup_ids]    <- TRUE
        movs$eliminado_at[movs$id %in% dup_ids] <- Sys.time()
      }

      # Saved first (rule 8, T12a): on failure nothing changes in the
      # session and nothing says it worked.
      if (!.bnc_guardar("reasignar_cuenta", paste0("Reasignar ", actual_n, " movimiento(s) a ", dest_label),
                        function() save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base))) return()
      reasig_confirm_data_rv(NULL)   # only now: a failed save keeps the confirmation's data
      shared$bancos_movimientos(movs)
      bump_sync_version("bancos_movimientos_db")
      removeModal()
      session$sendCustomMessage("bnc_collapse_reasignar",
        list(target = ns("reasignar_panel")))
      n_kept <- actual_n - n_deduped
      msg <- paste0(n_kept, " movimiento", if (n_kept == 1) "" else "s",
                    " reasignado", if (n_kept == 1) "" else "s",
                    " a ", dest_label, ".")
      if (n_deduped > 0)
        msg <- paste0(msg, " (", n_deduped, " duplicado",
                      if (n_deduped == 1) "" else "s", " eliminado",
                      if (n_deduped == 1) "" else "s", ".)")
      showNotification(msg, type = "message", duration = 5)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "reasignar_sesion",
        description = msg,
        metadata    = c(list(sesion_key = sesion_key, new_cuenta_id = new_cid,
                             n_kept = n_kept, n_deduped = n_deduped),
                        .bancos_audit_rows_summary(reasig_rows)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Eliminar sesión de importación ────────────────────────────────────────
    output$delete_sesion_btn_ui <- renderUI({
      sesion_key <- input$reasig_sesion %||% ""
      if (!nzchar(sesion_key)) return(NULL)
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      n <- sum(mask)
      if (n == 0) return(NULL)
      actionButton(ns("btn_eliminar_sesion"),
        label = tagList(icon("trash"), paste0(" Eliminar sesión (", n, " mov.)")),
        class = "btn btn-outline-danger btn-sm")
    })

    observeEvent(input$btn_eliminar_sesion, {
      sesion_key <- input$reasig_sesion %||% ""
      req(nzchar(sesion_key))
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      n <- sum(mask)
      if (n == 0) return()
      cts <- cuentas()
      cid <- movs$cuenta_id[mask][1]
      row <- dplyr::filter(cts, cuenta_id == cid)
      sesion_label <- if (nrow(row))
        paste0(row$empresa[1], " — ", row$banco[1], " ",
               row$moneda[1], " (", row$alias[1], ")")
      else cid %||% "desconocida"
      delete_sesion_confirm_rv(list(sesion_key = sesion_key, n = n,
                                    sesion_label = sesion_label))
      showModal(modalDialog(
        title = "¿Eliminar sesión de importación?",
        size  = "m", easyClose = TRUE,
        div(
          div(class = "alert alert-danger d-flex align-items-center gap-2 mb-3",
            icon("triangle-exclamation"),
            tags$strong("Esta acción envía los movimientos a la papelera.")
          ),
          p(class = "mb-1",
            tags$strong(n), " movimiento", if (n == 1) "" else "s",
            " de la cuenta ", tags$strong(sesion_label),
            " serán marcados como eliminados."
          ),
          p(class = "text-muted small mb-0",
            "Los registros se conservan en papelera para auditoría."
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_eliminar_sesion_confirm"),
            paste0("Eliminar ", n, " movimiento", if (n == 1) "" else "s"),
            class = "btn btn-danger")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$do_eliminar_sesion_confirm, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      shinyjs::disable("do_eliminar_sesion_confirm")
      on.exit(shinyjs::enable("do_eliminar_sesion_confirm"), add = TRUE)

      data <- delete_sesion_confirm_rv()
      req(data)   # cleared after the save: a failed one keeps it, to try again
      sesion_key <- data$sesion_key
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      movs <- movs_all()
      movs_base <- movs   # session copy before this change (CM09)
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      actual_n <- sum(mask)
      if (actual_n == 0) { removeModal(); return() }
      sesion_rows <- movs[mask, , drop = FALSE]
      movs$eliminado[mask]     <- TRUE
      movs$eliminado_at[mask]  <- Sys.time()
      movs$eliminado_por[mask] <- user
      # Saved first (rule 8, T12a): on failure nothing changes in the
      # session and nothing says it worked.
      if (!.bnc_guardar("eliminar_sesion", paste0("Eliminar los ", actual_n, " movimiento(s) de una sesión de importación"),
                        function() save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base))) return()
      shared$bancos_movimientos(movs)
      bump_sync_version("bancos_movimientos_db")
      delete_sesion_confirm_rv(NULL)
      removeModal()
      session$sendCustomMessage("bnc_collapse_reasignar",
        list(target = ns("reasignar_panel")))
      showNotification(
        paste0(actual_n, " movimiento", if (actual_n == 1) "" else "s",
               " enviado", if (actual_n == 1) "" else "s", " a papelera."),
        type = "message", duration = 5)
      log_action(
        user        = user,
        module      = "bancos",
        action      = "eliminar_sesion",
        description = paste0(actual_n, " movimiento(s) de sesión eliminados"),
        metadata    = c(list(n = actual_n, sesion_key = sesion_key),
                        .bancos_audit_rows_summary(sesion_rows)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Tab 2: Import TXT ─────────────────────────────────────────────────────
    parsed_preview <- reactiveVal(NULL)

    # ── Smart import banner — shown after file is loaded ──────────────────────
    output$import_cuenta_banner <- renderUI({
      req(parsed_preview())   # only show after file is parsed

      txt_lines <- txt_lines_rv()
      cid       <- input$import_cuenta %||% ""
      cts       <- cuentas()

      # Active BanBaj\u00edo accounts only (the only parser we support right now)
      cts_baj <- dplyr::filter(cts, activa == TRUE,
                               grepl("baj", banco, ignore.case = TRUE))

      if (!nrow(cts_baj))
        return(div(
          style = "background:#fff3cd; border:1.5px solid #664d03; border-radius:8px; padding:14px 18px; margin-bottom:14px;",
          icon("triangle-exclamation", style = "color:#997404;"),
          " No hay cuentas BanBaj\u00edo activas configuradas."
        ))

      # Rank by account number + subcuenta + currency + the CLIENT's raz\u00f3n
      # social registry (see R/bancos_detect.R for the evidence behind it).
      emp_df <- tryCatch(shared$empresas_db(), error = function(e) NULL)
      det <- detectar_cuenta_banbajio(txt_lines, cts_baj, emp_df,
                                      parsed = parsed_preview())
      cts_baj <- det$ranking
      txt_emp <- det$encabezado$razon_social

      choices <- setNames(cts_baj$cuenta_id, vapply(seq_len(nrow(cts_baj)), function(i) {
        num  <- gsub("[^0-9]", "", cts_baj$numero_cuenta[i] %||% "")
        tail <- if (!is.na(num) && nchar(num) >= 4L)
                  paste0(" \u00b7 \u2026", substr(num, nchar(num) - 3L, nchar(num))) else ""
        paste0(.company_name(cts_baj$empresa[i], shared$company_map()), " \u2014 ",
               cts_baj$banco[i], " ", cts_baj$moneda[i], tail,
               " (", cts_baj$alias[i], ")")
      }, character(1)))

      seguro    <- identical(det$confianza, "alta")
      det_icon  <- if (seguro)
                     icon("circle-check", style = "color:#198754;")
                   else
                     icon("circle-exclamation", style = "color:#e0a100;")
      # Pre-select the best-scoring account on every new file upload.
      # Once the user has consciously changed the dropdown (user_picked_cuenta_rv),
      # preserve their choice on subsequent re-renders.
      sel <- if (user_picked_cuenta_rv() && nzchar(cid) && cid %in% choices) cid
             else choices[[1L]]

      # Motivos describe the SUGGESTED account; once the user picks another one
      # they no longer apply, so say that instead of showing stale evidence.
      det_notas <- tagList(
        if (!identical(unname(sel), det$sugerida))
          div(style = "font-size:.75rem; color:#495057; margin-top:2px;",
              .BNC_DETECT_TXT$nota_otra_cuenta)
        else if (length(det$motivos))
          div(style = "font-size:.75rem; color:#495057; margin-top:2px;", det$motivos),
        lapply(det$avisos, function(a)
          div(style = "font-size:.75rem; color:#7a5c00; margin-top:2px;",
              icon("circle-info", style = "color:#e0a100;"), " ", a))
      )

      div(style = "background:#d1e7dd; border:1.5px solid #0f5132; border-radius:8px; padding:14px 18px; margin-bottom:14px;",
        div(class = "d-flex align-items-start gap-3",
          div(style = "flex:1;",
            div(style = "font-size:.8rem; font-weight:700; text-transform:uppercase; letter-spacing:.05em; margin-bottom:4px; color:#212529;",
                det_icon, " Empresa detectada en el archivo"),
            div(style = "font-size:1rem; font-weight:700; color:#212529; margin-bottom:10px;",
                if (nzchar(txt_emp)) txt_emp else "(no detectada)"),
            div(style = "font-size:.8rem; font-weight:600; text-transform:uppercase; letter-spacing:.04em; margin-bottom:4px; color:#495057;",
                "Importar a esta cuenta:"),
            selectInput(ns("import_cuenta"), NULL,
                        choices  = choices,
                        selected = sel,
                        width    = "100%"),
            det_notas
          )
        )
      )
    })

    # When account selection changes re-parse with the new cuenta_id.
    # This is critical — parsed rows carry cuenta_id stamped at parse time.
    # Without re-parsing, importing after a cuenta change duplicates everything.
    observeEvent(input$import_cuenta, {
      # Once the banner is showing, any change to import_cuenta means the user
      # has consciously picked an account (or the banner auto-selected on first
      # render). Either way, preserve this choice on subsequent re-renders.
      if (!is.null(parsed_preview())) user_picked_cuenta_rv(TRUE)
      # Re-parse only if a file is already loaded
      req(input$txt_file)
      path <- input$txt_file$datapath
      lines <- tryCatch(
        readLines(path, encoding = "UTF-8", warn = FALSE),
        error = function(e) tryCatch(
          readLines(path, encoding = "latin1", warn = FALSE),
          error = function(e2) NULL)
      )
      if (is.null(lines) || length(lines) < 3L) return()
      cuenta_id_sel <- input$import_cuenta %||% ""
      result <- tryCatch(
        parse_banbajio_txt(lines, cuenta_id_sel),
        error = function(e) NULL
      )
      if (!is.null(result)) parsed_preview(result)
    }, ignoreInit = TRUE)

    observeEvent(input$txt_file, {
      req(input$txt_file)
      path <- input$txt_file$datapath

      lines <- tryCatch(
        readLines(path, encoding = "UTF-8", warn = FALSE),
        error = function(e) {
          tryCatch(readLines(path, encoding = "latin1", warn = FALSE),
                   error = function(e2) NULL)
        }
      )

      if (is.null(lines) || length(lines) < 3L) {
        showNotification("No se pudo leer el archivo TXT.", type = "error")
        parsed_preview(NULL)
        return()
      }

      # Keep the raw lines for the account detector (banner); reset the
      # account-picker flag so the new file's best match is pre-selected.
      txt_lines_rv(lines)
      user_picked_cuenta_rv(FALSE)

      cuenta_id_sel <- input$import_cuenta %||% ""
      result <- tryCatch(
        parse_banbajio_txt(lines, cuenta_id_sel),
        error = function(e) {
          showNotification(paste("Error al parsear:", e$message), type = "error")
          NULL
        }
      )

      if (is.null(result)) { parsed_preview(NULL); return() }

      # Account suggestion lives in output$import_cuenta_banner, which re-ranks
      # every render via detectar_cuenta_banbajio() (R/bancos_detect.R).
      parsed_preview(result)
    }, ignoreInit = TRUE)

    output$import_preview <- renderUI({
      pv <- parsed_preview()
      if (is.null(pv)) return(NULL)

      cid_final <- input$import_cuenta %||% ""
      if (nzchar(cid_final)) pv$cuenta_id <- cid_final

      existentes <- movs_all()
      dedup_res  <- dedup_movimientos(pv, existentes)
      nuevos     <- dedup_res$nuevos
      n_dup      <- dedup_res$n_duplicados

      n_total    <- nrow(pv)
      n_spei_out <- sum(pv$tipo == "spei_out")
      n_spei_in  <- sum(pv$tipo == "spei_in")
      n_tras     <- sum(pv$tipo == "traspaso")
      n_com      <- sum(pv$tipo == "comision")
      n_new      <- nrow(nuevos)

      # Header
      cnt_header <- div(class = "mb-2 fw-semibold",
        paste0(n_total, " movimientos detectados"))

      # Summary badges
      badges <- div(class = "d-flex flex-wrap gap-2 mb-3",
        tags$span(class = "bnc-import-badge bg-success-subtle text-success border border-success-subtle",
                  paste0(n_spei_out + n_spei_in, " SPEI (auto-parseados)")),
        tags$span(class = "bnc-import-badge bg-warning-subtle text-warning-emphasis border border-warning-subtle",
                  paste0(n_tras, " traspasos (revisar)")),
        tags$span(class = "bnc-import-badge bg-secondary-subtle text-secondary border border-secondary-subtle",
                  paste0(n_com, " comisiones (ocultas en tabla)")),
        tags$span(class = if (n_dup > 0)
                    "bnc-import-badge bg-danger-subtle text-danger border border-danger-subtle"
                  else
                    "bnc-import-badge bg-light text-muted border",
                  paste0(n_dup, " duplicados encontrados"))
      )

      # Import footer
      import_footer <- if (n_new > 0) {
        tagList(
          tags$hr(),
          div(class = "d-flex justify-content-end gap-2",
            uiOutput(ns("import_btn_ui"))
          )
        )
      } else {
        tagList(
          tags$hr(),
          div(class = "alert alert-info small",
              icon("circle-info"),
              " Ya importados \u2014 0 movimientos nuevos.")
        )
      }

      tagList(cnt_header, badges,
              hd_table_ui(ns("import")), import_footer)
    })

    # The import's preview (hd_table "importar_txt", T12b): EVERY new movement,
    # commissions included -- exactly what "Importar N movimientos" saves. It
    # used to show the first 20 non-commission rows and import all of them.
    t_import <- hd_table_server("import", tabla = "importar_txt",
      datos = reactive({
        pv <- parsed_preview()
        if (is.null(pv) || !nrow(pv)) return(.bnc_previa_datos(NULL))
        cid_final <- input$import_cuenta %||% ""
        if (nzchar(cid_final)) pv$cuenta_id <- cid_final
        .bnc_previa_datos(dedup_movimientos(pv, movs_all())$nuevos, cuentas())
      }),
      columnas = .BNC_COLUMNAS_PREVIA, vacio = "tablas.bancos.vacio_import")

    output$import_btn_ui <- renderUI({
      pv <- parsed_preview()
      if (is.null(pv)) return(NULL)
      cid_final <- input$import_cuenta %||% ""
      if (nzchar(cid_final)) pv$cuenta_id <- cid_final
      existentes <- movs_all()
      n_new      <- nrow(dedup_movimientos(pv, existentes)$nuevos)
      if (n_new == 0) return(NULL)
      actionButton(ns("do_import"), icon("file-import"),
                   label = paste0(" Importar ", n_new, " movimientos"),
                   class = "btn btn-primary")
    })

    observeEvent(input$do_import, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      shinyjs::disable("do_import")
      on.exit(shinyjs::enable("do_import"), add = TRUE)

      pv        <- parsed_preview()
      req(pv, nrow(pv) > 0)

      # Always use the currently selected account — the parsed_preview carries a
      # cuenta_id stamped at parse time, which can be stale if the user changed
      # the account selector after the file was uploaded (or if import_cuenta was
      # empty at parse time due to the reactive init order).
      cid_final <- input$import_cuenta %||% ""
      if (nzchar(cid_final)) pv$cuenta_id <- cid_final

      existentes <- movs_all()
      movs_base <- existentes   # session copy before this change (CM09)
      dedup_res  <- dedup_movimientos(pv, existentes)
      nuevos     <- dedup_res$nuevos
      if (!nrow(nuevos)) {
        showNotification("No hay movimientos nuevos.", type = "warning"); return()
      }

      # Auto-match against confirmados
      conf       <- confirmados()
      nuevos     <- auto_match_agenda(nuevos, conf)

      # Assign server-side columns
      nuevos$id           <- vapply(seq_len(nrow(nuevos)),
                                    function(i) uuid::UUIDgenerate(), character(1))
      nuevos$importado_at <- Sys.time()
      nuevos$eliminado    <- FALSE

      # Normalise missing cols from schema
      schema_cols <- names(.schema_bancos_movimientos())
      for (col in schema_cols) {
        if (!col %in% names(nuevos)) {
          nuevos[[col]] <- .schema_bancos_movimientos()[[col]][NA_integer_]
        }
      }
      nuevos <- dplyr::select(nuevos, dplyr::all_of(schema_cols))

      movs_new <- dplyr::bind_rows(existentes, nuevos)
      # Saved first (rule 8, T12a): on failure nothing changes in the
      # session and nothing says it worked.
      if (!.bnc_guardar("importar_movimientos", paste0("Importar ", nrow(nuevos), " movimiento(s)"),
                        function() save_bancos_movimientos(movs_new, client_id = shared$effective_client_id(), base = movs_base))) return()
      shared$bancos_movimientos(movs_new)
      bump_sync_version("bancos_movimientos_db")

      n_auto  <- sum(nuevos$conciliado, na.rm = TRUE)
      msg     <- paste0(nrow(nuevos), " movimiento(s) importado(s).")
      if (n_auto > 0)
        msg <- paste0(msg, " ", n_auto, " conciliado(s) autom\u00e1ticamente.")

      showNotification(msg, type = "message", duration = 4)
      parsed_preview(NULL)  # clear preview
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "importar_movimientos",
        description = msg,
        metadata    = c(list(cuenta_id = cid_final, n_importados = nrow(nuevos), n_auto_conciliado = n_auto),
                        .bancos_audit_rows_summary(nuevos)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Historial de confirmaciones (hd_table "historial_conf", T12b) ───────
    # Three sources, as before: the live confirmations (an undo's own
    # "recovered" row is not one: listing it offered a second Deshacer on
    # the undo itself), the legacy conciliación (items confirmed before
    # bancos_confirmados existed; not repeated by empresa + documento) and
    # the active abonos. Each row's id says which source it is ("conf:",
    # "legado:", "abono:"), so ↶ and ✕ act on that row by id. Filters by
    # empresa and tipo, and the search, are the engine's (T12 P4); the rows
    # come newest confirmed first, with "Confirmado el" (P6).
    # One source's rows; a column the source lacks comes as NA.
    .bnc_hist_filas <- function(fuente, ids, confirmado_el, fecha, empresa, parte, documento, importe, moneda, tipo,
                                origen, acciones) {
      n <- length(ids)
      v <- function(x, como, na) { x <- if (length(x)) x else rep(na, n); rep(como(x), length.out = n) }
      data.frame(id = if (n) paste0(fuente, ":", ids) else character(0),
                 ConfirmadoEl = v(confirmado_el, as.POSIXct, NA_real_), Fecha = v(fecha, as.Date, NA_real_),
                 Empresa = v(empresa, as.character, NA), Parte = v(parte, as.character, NA),
                 Documento = v(documento, as.character, NA), Importe = v(importe, as.numeric, NA),
                 Moneda = v(moneda, as.character, NA), Tipo = v(tipo, as.character, NA),
                 Origen = v(origen, as.character, NA), .acciones = v(acciones, as.character, NA),
                 stringsAsFactors = FALSE, check.names = FALSE)
    }
    historial_datos <- reactive({
      escribe <- .bnc_puede_ver_escritura()
      conf <- confirmaciones_vigentes(confirmados())
      filas <- list(
        .bnc_hist_filas("conf", conf$confirmacion_id, conf$confirmado_at, conf$fecha, conf$empresa,
                        conf$parte, conf$documento, conf$importe, conf$moneda, conf$tipo, "",
                        if (escribe) "deshacer,eliminar" else ""))

      conc_raw <- tryCatch(shared$conciliacion_rv(), error = function(e) NULL)
      if (!is.null(conc_raw) && nrow(conc_raw) > 0) {
        bc_keys <- paste(tolower(trimws(conf$empresa)), tolower(trimws(conf$documento)), sep = "|")
        conc <- conc_raw[!paste(tolower(trimws(conc_raw$Empresa)), tolower(trimws(conc_raw$Documento)),
                                sep = "|") %in% bc_keys, , drop = FALSE]
        # No buttons: they were confirmed by the old route.
        if (nrow(conc))
          filas$legado <- .bnc_hist_filas("legado", conc$id, conc$created_at, conc$FechaPago,
                                          trimws(conc$Empresa), trimws(conc$Parte), trimws(conc$Documento),
                                          conc$Importe, conc$Moneda, conc$tipo, "legado", "")
      }

      # Active abonos (Abono Parcial; abonos_db, not bancos_confirmados):
      # shown in this same history so their ↶ lives next to every other undo.
      ab <- tryCatch(shared$abonos_db(), error = function(e) NULL)
      if (!is.null(ab) && nrow(ab)) {
        ab <- ab[ab$status %in% "active", , drop = FALSE]
        if (nrow(ab))
          filas$abono <- .bnc_hist_filas("abono", ab$id, ab$created_at, ab$fecha_abono, ab$Empresa,
                                         ab$Parte, ab$Documento, ab$importe, ab$Moneda, "abono", "",
                                         if (escribe) "deshacer" else "")
      }
      h <- do.call(rbind, unname(filas))
      h[order(h$ConfirmadoEl, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    })

    t_hist <- hd_table_server("historial", tabla = "historial_conf", datos = historial_datos,
      columnas = list(col_fecha_hora("ConfirmadoEl", "tablas.bancos.confirmado_el"),
                      col_fecha("Fecha", "tablas.bancos.fecha"),
                      col_empresa("Empresa", "tablas.facturas.empresa"),
                      col_texto("Parte", "tablas.facturas.parte"),
                      col_texto("Documento", "tablas.facturas.documento", recorte = FALSE),
                      col_dinero("Importe", "tablas.facturas.importe", moneda = "Moneda"),
                      col_texto("Moneda", "tablas.facturas.moneda", recorte = FALSE),
                      col_insignia("Tipo", "tablas.facturas.tipo", mapa = list(
                        pago  = list(texto = "tablas.bancos.pago",  tono = "rojo"),
                        cobro = list(texto = "tablas.bancos.cobro", tono = "exito"),
                        abono = list(texto = "tablas.bancos.abono_parcial", tono = "aviso"))),
                      col_insignia("Origen", "tablas.bancos.origen", mapa = list(
                        legado = list(texto = "tablas.bancos.legado", tono = "neutro")))),
      piezas = "acc_fila", acciones_fila = c("deshacer", "eliminar"),
      filtros = list(list(campo = "Empresa"), list(campo = "Tipo")),
      vacio = "tablas.bancos.vacio_historial")

    # ↶ undoes a confirmation (back to the calendar) or an abono; ✕ asks
    # (its own modal, which says what goes with it) to delete a
    # confirmation. Both already went through the table's confirmation.
    observeEvent(t_hist$accion(), {
      a <- t_hist$accion()
      id <- as.character(a$ids[1] %||% "")
      fuente <- sub(":.*$", "", id)
      clave  <- sub("^[^:]*:", "", id)
      if (identical(a$accion, "deshacer") && fuente == "conf") .bnc_deshacer_confirmacion(clave)
      else if (identical(a$accion, "deshacer") && fuente == "abono") .bnc_deshacer_abono(clave)
      else if (identical(a$accion, "eliminar") && fuente == "conf") .bnc_pedir_eliminar_conf(clave)
    }, ignoreInit = TRUE)

    # \u2500\u2500 Deshacer eliminaci\u00f3n de movimiento manual \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    # Restores a manually-entered bank movement that was deleted by mistake
    # (2026-07-30, part of the Vantage Bank manual-entry correction tool --
    # scoped to fuente=="manual" only, see the Papelera's .acciones below;
    # a TXT-imported row never gets this button at all).
    .bnc_deshacer_eliminacion <- function(mov_id) {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      req(mov_id)
      movs <- movs_all()
      movs_base <- movs   # session copy before this change (CM09)
      idx  <- which(movs$id == mov_id & movs$fuente == "manual")
      if (!length(idx)) return()
      # This button is rendered per-row with a onclick nonce (to defeat
      # Shiny's dedup), not a persistent actionButton -- shinyjs::disable()
      # has no real DOM id to target here, unlike do_add_manual/do_*_confirm.
      # A genuine double-click re-fires this observer with the same mov_id,
      # so guard by checking the row is still actually eliminado (idempotent
      # check-before-act, same idea as elim_ids_rv being cleared after use)
      # instead of relying on a disable that wouldn't work anyway.
      if (!isTRUE(movs$eliminado[idx[1]])) return()

      movs$eliminado[idx]    <- FALSE
      movs$eliminado_at[idx] <- as.POSIXct(NA)
      # Saved first (rule 8, T12a): on failure nothing changes in the
      # session and nothing says it worked.
      if (!.bnc_guardar("deshacer_eliminar_movimiento_manual", "Restaurar un movimiento manual eliminado",
                        function() save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base))) return()
      shared$bancos_movimientos(movs)
      bump_sync_version("bancos_movimientos_db")

      # found 2026-08-03 (Stage 9, Issue C): mirror of the archive write in
      # do_eliminar_confirm above -- records this restore as its own
      # permanent bancos_papelera event (action="restored"), rather than
      # only un-flipping the live eliminado flag. Best-effort: if no
      # archived event exists (e.g. this movement was deleted before this
      # stage shipped), the flag un-flip above already fully restores it;
      # this is additive bookkeeping, not the primary restore mechanism.
      # The movement is already restored. No archived event to reverse (a
      # movement deleted before the archive existed) is expected and stays
      # silent; failing to write the restore is not: the user is told and
      # it is logged (it used to be a message() on the console).
      .bnc_guardar("registrar_restauracion_papelera", "Registrar la restauración en la papelera de Bancos",
                   function() {
                     bpap <- load_bancos_papelera(client_id = shared$effective_client_id())
                     res  <- tryCatch(restore_from_bancos_papelera(bpap, mov_id,
                                        actor = tryCatch(shared$current_user(), error = function(e) "system")),
                                      error = function(e) NULL)
                     if (!is.null(res)) save_bancos_papelera(res$papelera_df, client_id = shared$effective_client_id())
                   }, aviso = "tablas.bancos.error_archivar_restauracion")

      showNotification("Movimiento restaurado.", type = "message", duration = 3)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "deshacer_eliminar_movimiento_manual",
        description = paste0("Movimiento manual restaurado: ", htmltools::htmlEscape(movs$parte[idx[1]] %||% "")),
        target_id   = mov_id,
        metadata    = list(fecha = as.character(movs$fecha[idx[1]]), parte = movs$parte[idx[1]] %||% "",
                           concepto = movs$concepto[idx[1]] %||% "", cargo = movs$cargo[idx[1]] %||% 0,
                           abono = movs$abono[idx[1]] %||% 0),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

    # \u2500\u2500 Deshacer abono \u2014 void it, restoring the invoice's balance \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    # void_abono() (R/persistence.R) already existed but nothing called it
    # (found 2026-07-24's Abono Parcial audit). Soft-delete only (status \u2192
    # "voided", same permanent-record convention as every other undo in this
    # module) -- the invoice's Saldo vencido nets back up automatically on
    # the next render, since active_abonos_summary() only sums status=="active".
    .bnc_deshacer_abono <- function(ab_id) {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      req(ab_id)
      ab <- tryCatch(shared$abonos_db(), error = function(e) NULL) %||% tibble::tibble()
      if (!nrow(ab) || !ab_id %in% ab[["id"]]) return()
      # Same per-row dynamic-button caveat as undo_mov_delete above (no real
      # DOM id for shinyjs::disable() to target) -- guard idempotency by
      # checking it isn't already voided instead.
      if (identical(ab$status[ab$id == ab_id][1], "voided")) return()

      ab_row  <- ab[ab$id == ab_id, , drop = FALSE][1, ]
      updated <- void_abono(ab, ab_id)
      shared$abonos_db(updated)
      tryCatch({
        save_abonos(updated, client_id = shared$effective_client_id())
        bump_sync_version("abonos_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))

      showNotification("Abono deshecho \u2014 el saldo del comprobante se restaur\u00f3.",
                       type = "message", duration = 3)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "deshacer_abono",
        description = "Abono parcial deshecho",
        target_id   = ab_id,
        metadata    = list(empresa = ab_row$Empresa %||% "", documento = ab_row$Documento %||% "",
                           parte = ab_row$Parte %||% "", importe = ab_row$importe %||% 0,
                           fecha_abono = as.character(ab_row$fecha_abono %||% NA)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

    .bnc_pedir_eliminar_conf <- function(conf_id) {
      conf    <- confirmados()
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) return()
      if (length(idx_c) > 1) {
        message("[bancos] confirmacion_id '", conf_id, "' matched ", length(idx_c),
                " rows (should be unique) -- data integrity issue, using the first match")
        idx_c <- idx_c[1]
      }

      row <- conf[idx_c, , drop = FALSE]
      session$userData[[paste0(ns("pending_delete_conf_id"))]] <- conf_id

      showModal(modalDialog(
        title = "\u00bfEliminar esta confirmaci\u00f3n?",
        tagList(
          tags$p("Esta acci\u00f3n mover\u00e1 la confirmaci\u00f3n a la papelera."),
          tags$ul(
            tags$li(paste("Parte:", row$parte)),
            tags$li(paste("Importe:", fmt_money(row$importe))),
            tags$li(paste("Fecha:", format(as.Date(row$fecha), "%d/%m/%Y")))
          ),
          if (!is.na(row$mov_id) && nzchar(row$mov_id %||% ""))
            tags$p(class = "text-muted small",
                   "El movimiento bancario vinculado tambi\u00e9n se eliminar\u00e1.")
          else NULL
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_delete_conf_confirm"), "Eliminar",
                       class = "btn-danger btn-sm", icon = icon("trash"))
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$do_delete_conf_confirm, {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      conf_id <- session$userData[[paste0(ns("pending_delete_conf_id"))]]
      session$userData[[paste0(ns("pending_delete_conf_id"))]] <- NULL
      req(conf_id)

      conf    <- confirmados()
      conf_base <- conf   # session copy before this change (CM09/C5)
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) { removeModal(); return() }
      if (length(idx_c) > 1) {
        message("[bancos] confirmacion_id '", conf_id, "' matched ", length(idx_c),
                " rows (should be unique) -- data integrity issue, using the first match")
        idx_c <- idx_c[1]
      }

      mov_id_linked <- conf$mov_id[idx_c]
      conf$eliminado[idx_c]    <- TRUE
      conf$eliminado_at[idx_c] <- Sys.time()

      if (!is.null(mov_id_linked) && !is.na(mov_id_linked) && nzchar(mov_id_linked)) {
        movs <- movs_all()
        movs_base <- movs   # session copy before this change (CM09)
        idx_m <- which(movs$id == mov_id_linked)
        if (length(idx_m)) {
          movs$eliminado[idx_m]    <- TRUE
          movs$eliminado_at[idx_m] <- Sys.time()
          shared$bancos_movimientos(movs)
          tryCatch({
            save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base)
            bump_sync_version("bancos_movimientos_db")
          }, error = function(e)
            showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
          )
        }
      }

      shared$bancos_confirmados(conf)
      tryCatch({
        save_bancos_confirmados(conf, client_id = shared$effective_client_id(), base = conf_base)
        bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
      )

      # Deleting a provision's confirmation leaves it unpaid with no item
      # (the item was archived at confirm time and is not restored here), so
      # the provision is pending again -- unless another live confirmation
      # still pays it.
      pid <- conf$provision_id[idx_c] %||% NA_character_
      if (!is.na(pid) && nzchar(pid) && !any(confirmaciones_vigentes(conf)$provision_id %in% pid)) {
        tryCatch(pasivos_provision_revive(
          pid, user = tryCatch(shared$current_user(), error = function(e) "system"),
          client_id = shared$effective_client_id()),
          error = function(e) message("[bancos] provision revive failed: ", conditionMessage(e)))
        tryCatch({
          shared$suppress_ledger_prov_refresh(TRUE)
          shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$effective_client_id()))
        }, error = function(e) NULL)
      }

      removeModal()
      showNotification("Confirmaci\u00f3n movida a papelera.", type = "message", duration = 2)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "eliminar_confirmacion",
        description = "Confirmaci\u00f3n bancaria movida a papelera",
        target_id   = conf_id,
        metadata    = list(empresa = conf$empresa[idx_c] %||% "", parte = conf$parte[idx_c] %||% "",
                           documento = conf$documento[idx_c] %||% "", importe = conf$importe[idx_c] %||% 0,
                           fecha = as.character(conf$fecha[idx_c] %||% NA), tipo = conf$tipo[idx_c] %||% ""),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }, ignoreInit = TRUE)

    # ── Deshacer confirmación — restore to calendar as pending ───────────────
    .bnc_deshacer_confirmacion <- function(conf_id) {
      if (!puede_o_avisa(shared, "can_manage_bancos")) return()
      conf    <- confirmados()
      conf_base <- conf   # session copy before this change (CM09/C5)
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) return()
      if (length(idx_c) > 1) {
        message("[bancos] confirmacion_id '", conf_id, "' matched ", length(idx_c),
                " rows (should be unique) -- data integrity issue, using the first match")
        idx_c <- idx_c[1]
      }

      row <- conf[idx_c, , drop = FALSE]
      mov_id_linked <- conf$mov_id[idx_c]

      # Also soft-delete the linked movimiento if present
      if (!is.null(mov_id_linked) && !is.na(mov_id_linked) && nzchar(mov_id_linked)) {
        movs  <- movs_all()
        movs_base <- movs   # session copy before this change (CM09)
        idx_m <- which(movs$id == mov_id_linked)
        if (length(idx_m)) {
          movs$eliminado[idx_m]    <- TRUE
          movs$eliminado_at[idx_m] <- Sys.time()
          shared$bancos_movimientos(movs)
          tryCatch({
            save_bancos_movimientos(movs, client_id = shared$effective_client_id(), base = movs_base)
            bump_sync_version("bancos_movimientos_db")
          }, error = function(e)
            showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
          )
        }
      }

      # recover_confirmacion() soft-deletes the original row (as before) AND
      # appends a permanent, separate "recovered" event row referencing it
      # (Stage 3) -- the actual fix for "recovery should add a line."
      # Guarded (unlike before): it stop()s if conf_id is already recovered --
      # reachable via a double-click/fast re-click, since the nonce on this
      # button's onclick defeats Shiny's normal unchanged-value dedup, and
      # nothing here debounces it. An uncaught stop() inside an observeEvent
      # (not an output binding) crashes the whole session, not just this
      # action -- caught here the same way every other risky write in this
      # handler already is.
      # Recovered on S3's table as it is now (CM09): the "already recovered"
      # check sees another session's recovery too, so two sessions cannot both
      # add a "recovered" row. Read, change and save in one step; nothing else
      # in this recovery runs if it fails.
      actor_rc <- tryCatch(shared$current_user(), error = function(e) "user")
      new_conf <- tryCatch(
        modificar_bancos_confirmados(shared$effective_client_id(),
          function(fresco) recover_confirmacion(fresco, conf_id, actor = actor_rc)),
        error = function(e) {
          if (grepl("already recovered", conditionMessage(e), fixed = TRUE)) {
            message("[bancos] recover_confirmacion failed (likely an already-recovered double-click): ",
                    conditionMessage(e))
            showNotification("Esta confirmación ya fue recuperada.", type = "warning", duration = 3)
          } else {
            message("[bancos] recover_confirmacion save failed: ", conditionMessage(e))
            showNotification(tr("comun.error_guardar"), type = "error", duration = NULL)
          }
          NULL
        }
      )
      if (is.null(new_conf)) return()
      conf <- new_conf
      shared$bancos_confirmados(conf)
      bump_sync_version("bancos_confirmados_db")

      # ── Philosophy: recovering a CONFIRMED item always recovers the ITEM ──
      # It doesn't matter whether that item originated from a converted
      # provision or was always a plain manual entry — a provision itself is
      # NEVER confirmed (it can never reach Agenda in the first place, per
      # Mouse's explicit rule); only its DERIVED item can be. So
      # `provision_id` on this confirmation is pure lineage/traceability
      # metadata — it does not change how the item's own recovery works.
      # (Recovering a DELETED, unconverted provision is a completely
      # separate operation, at the Pasivos level, unrelated to
      # Agenda/bancos_confirmados entirely — nothing to do here for that.)
      has_archive <- "archive_event_id" %in% names(row) &&
                        !is.na(row$archive_event_id) && nzchar(row$archive_event_id %||% "")
      restored_fecha <- as.Date(row$fecha)  # fallback; overridden below when a real date is recovered

      if (has_archive) {
        # Manual entry (plain or provision-derived alike), archived — not
        # deleted — at confirm time (Stages 4-5). Restore the REAL row
        # losslessly. Never touches pagar_hoy_db; Agenda has nothing to do
        # with this recovery at all.
        pap <- shared$papelera_rv() %||% load_papelera(client_id = shared$effective_client_id())
        restore_result <- tryCatch(
          restore_from_papelera(pap, row$archive_event_id,
                                actor = tryCatch(shared$current_user(), error = function(e) "user")),
          error = function(e) {
            showNotification(paste("Error al recuperar la entrada original:", e$message), type = "warning")
            NULL
          }
        )
        if (!is.null(restore_result)) {
          shared$papelera_rv(restore_result$papelera_df)
          # A failed save (or one the CM09 net refuses) is shown and logged,
          # never swallowed (rule 8).
          tryCatch(save_papelera(restore_result$papelera_df, client_id = shared$effective_client_id()),
                   error = function(e) {
                     showNotification(tr("comun.error_guardar"), type = "error", duration = NULL)
                     tryCatch(log_action(
                       user        = tryCatch(shared$current_user(), error = function(e2) "user"),
                       module      = "bancos",
                       action      = "recuperar_papelera_fallo_guardado",
                       description = paste0("No se pudo guardar la papelera al recuperar ",
                                            row$archive_event_id, ": ", conditionMessage(e)),
                       metadata    = list(archive_event_id = row$archive_event_id, error = conditionMessage(e)),
                       client_id             = tryCatch(shared$effective_client_id(), error = function(e2) NULL),
                       viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e2) NULL)),
                       error = function(e2) NULL)
                   })
          # Read fresh from S3 first, not just as a NULL-fallback -- this is
          # a restore path (undo a confirmation), so a stale in-memory read
          # here would silently drop any row another session added/removed
          # since this session's snapshot (found 2026-07-24, a real
          # incident).
          mi <- tryCatch(load_manual(client_id = shared$effective_client_id()),
                         error = function(e) NULL) %||% shared$manual_inv()
          restored_row <- as.data.frame(restore_result$restored_data, stringsAsFactors = FALSE)
          mi_updated <- dplyr::bind_rows(mi, restored_row)
          shared$manual_inv(mi_updated)
          tryCatch(save_manual(mi_updated, client_id = shared$effective_client_id(), base = mi),
                   error = function(e) showNotification(
                     paste("Error al restaurar entrada manual:", e$message), type = "warning"))
          if ("Fecha de vencimiento" %in% names(restored_row))
            restored_fecha <- as.Date(restored_row[["Fecha de vencimiento"]][1])
        }
      }
      # else: ERP-sourced (or a manual confirmation from before Stage 4, with
      # no archive to restore from) -- nothing else to do. Clearing the
      # bancos_confirmados flag above is enough; the item reappears in
      # Calendario on its own the moment nothing matches it as confirmed.

      # A provision-derived item is back unpaid: its provision goes back to
      # "converted", unless another live confirmation still pays it.
      pid <- row$provision_id[1] %||% NA_character_
      if (!is.na(pid) && nzchar(pid)) {
        vivas <- confirmaciones_vigentes(conf)
        if (!any(vivas$provision_id %in% pid))
          tryCatch(pasivos_provision_item_reversed(
            pid, user = tryCatch(shared$current_user(), error = function(e) "system"),
            client_id = shared$effective_client_id()),
            error = function(e) message("[bancos] provision revert failed: ", conditionMessage(e)))
        tryCatch({
          shared$suppress_ledger_prov_refresh(TRUE)
          shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$effective_client_id()))
        }, error = function(e) NULL)
      }

      cal_label <- if (isTRUE(row$tipo == "cobro")) "CxC" else "CxP"
      fecha_fmt <- format(restored_fecha, "%d/%m/%Y")
      showNotification(
        paste0("\u21a9 ", if (isTRUE(row$tipo == "cobro")) "Cobro" else "Pago",
               " devuelto al calendario ",
               cal_label, " \u2014 para el ", fecha_fmt),
        type = "message", duration = 5
      )
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "revertir_confirmacion",
        description = paste0("Confirmaci\u00f3n revertida: ",
                             if (isTRUE(row$tipo == "cobro")) "Cobro" else "Pago",
                             " ", fecha_fmt),
        target_id   = conf_id,
        metadata    = list(empresa = row$empresa[1] %||% "", parte = row$parte[1] %||% "",
                           documento = row$documento[1] %||% "", importe = row$importe[1] %||% 0,
                           tipo = row$tipo[1] %||% "", fecha_devuelta = as.character(restored_fecha)),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

    # ── Papelera (hd_table "papelera", T12b) ─────────────────────────────────
    # Four sources, as before: deleted movements, deleted confirmations,
    # reconciled movements with a linked document, and the Calendario's trash
    # (papelera_rv: SAP ghosts, deleted manual and provision rows). Each
    # row's id says its source ("mov:", "conf:", "vin:", "cal:"). ↶ only on
    # a deleted MANUAL movement -- restoring a TXT-imported row would fight
    # a future re-import of the same statement (2026-07-30) -- and only for
    # who can change Bancos. The Calendario's rows used to show the document
    # under Tipo and the company under "Doc. vinculado" (T12 inventory, seen
    # in passing 6): Tipo is now pago/cobro by its ledger, the document goes
    # under "Doc. vinculado" and the company under Empresa.
    .bnc_pap_filas <- function(fuente, ids, origen, fecha, empresa, parte, importe, moneda, tipo, doc, eliminado_el,
                               acciones = "") {
      n <- length(ids)
      v <- function(x, como, na) { x <- if (length(x)) x else rep(na, n); rep(como(x), length.out = n) }
      data.frame(id = if (n) paste0(fuente, ":", ids) else character(0), Origen = v(origen, as.character, NA),
                 Fecha = v(fecha, as.Date, NA_real_), Empresa = v(empresa, as.character, NA),
                 Parte = v(parte, as.character, NA), Importe = v(importe, as.numeric, NA),
                 Moneda = v(moneda, as.character, NA), Tipo = v(tipo, as.character, NA),
                 DocVinculado = v(doc, as.character, NA), EliminadoEl = v(eliminado_el, as.POSIXct, NA_real_),
                 .acciones = v(acciones, as.character, NA), stringsAsFactors = FALSE, check.names = FALSE)
    }
    papelera_datos <- reactive({
      escribe <- .bnc_puede_ver_escritura()
      movs <- movs_all()
      cts  <- cuentas()
      mon_de <- function(cuenta_id) toupper(trimws(cts$moneda[match(cuenta_id, cts$cuenta_id)]))
      emp_de <- function(cuenta_id) cts$empresa[match(cuenta_id, cts$cuenta_id)]
      importe_mov <- function(m) ifelse(m$cargo > 0, m$cargo, m$abono)

      movs_del <- movs[movs$eliminado %in% TRUE, , drop = FALSE]
      conf_del <- confirmados()
      conf_del <- conf_del[conf_del$eliminado %in% TRUE, , drop = FALSE]
      movs_vin <- movs[!(movs$eliminado %in% TRUE) & movs$conciliado %in% TRUE &
                         !is.na(movs$doc_vinculado) & nzchar(movs$doc_vinculado), , drop = FALSE]
      filas <- list(
        .bnc_pap_filas("mov", movs_del$id, "eliminado", movs_del$fecha, emp_de(movs_del$cuenta_id), movs_del$parte,
                       importe_mov(movs_del), mon_de(movs_del$cuenta_id), dplyr::coalesce(movs_del$tipo, "otro"), "",
                       movs_del$eliminado_at,
                       ifelse(movs_del$fuente %in% "manual" & escribe, "deshacer", "")),
        .bnc_pap_filas("conf", conf_del$confirmacion_id, "eliminado", conf_del$fecha, conf_del$empresa, conf_del$parte,
                       conf_del$importe, conf_del$moneda, conf_del$tipo, "", conf_del$eliminado_at),
        .bnc_pap_filas("vin", movs_vin$id, "vinculado", movs_vin$fecha, emp_de(movs_vin$cuenta_id), movs_vin$parte,
                       importe_mov(movs_vin), mon_de(movs_vin$cuenta_id), dplyr::coalesce(movs_vin$tipo, "otro"),
                       movs_vin$doc_vinculado, NA))

      # The Calendario's trash. Both the historical "sap" and today's
      # SAP_B1_ERP_TYPE are SAP; any other ERP source is "erp".
      cal <- tryCatch(shared$papelera_rv(), error = function(e) NULL)
      if (!is.null(cal) && nrow(cal)) {
        src <- dplyr::coalesce(dplyr::na_if(as.character(cal$source), ""), SAP_B1_ERP_TYPE)
        origen <- ifelse(src %in% c("sap", SAP_B1_ERP_TYPE), "sap", ifelse(src %in% c("manual", "provision"), src, "erp"))
        ids <- make.unique(dplyr::coalesce(as.character(cal$event_id), as.character(cal$id), as.character(seq_len(nrow(cal)))))
        filas$cal <- .bnc_pap_filas("cal", ids, origen, dplyr::coalesce(as.Date(cal$FechaEff), as.Date(cal$deleted_at)),
                                    cal$Empresa, dplyr::coalesce(cal$Parte, cal$Empresa), cal$Importe, cal$Moneda,
                                    ifelse(cal$ledger %in% "AR", "cobro", "pago"), cal$Documento, cal$deleted_at)
      }
      do.call(rbind, unname(filas))
    })

    t_papelera <- hd_table_server("papelera", tabla = "papelera", datos = papelera_datos,
      columnas = list(col_insignia("Origen", "tablas.bancos.origen", mapa = list(
                        eliminado = list(texto = "tablas.bancos.eliminado",  tono = "neutro"),
                        vinculado = list(texto = "tablas.bancos.vinculado",  tono = "info"),
                        sap       = list(texto = "SAP",                      tono = "neutro"),
                        manual    = list(texto = "tablas.bancos.manual",     tono = "info"),
                        provision = list(texto = "tablas.bancos.provision",  tono = "aviso"),
                        erp       = list(texto = "ERP",                      tono = "neutro"))),
                      col_fecha("Fecha", "tablas.bancos.fecha"),
                      col_empresa("Empresa", "tablas.facturas.empresa"),
                      col_texto("Parte", "tablas.facturas.parte"),
                      col_dinero("Importe", "tablas.facturas.importe", moneda = "Moneda", sumar = FALSE),
                      col_texto("Moneda", "tablas.facturas.moneda", recorte = FALSE),
                      col_insignia("Tipo", "tablas.facturas.tipo", mapa = c(
                        list(pago  = list(texto = "tablas.bancos.pago",  tono = "rojo"),
                             cobro = list(texto = "tablas.bancos.cobro", tono = "exito")),
                        lapply(as.list(.TIPO_LABELS), function(t) list(texto = t, tono = "neutro")))),
                      col_texto("DocVinculado", "tablas.bancos.doc_vinculado", recorte = FALSE),
                      col_fecha_hora("EliminadoEl", "tablas.bancos.eliminado_el")),
      piezas = "acc_fila", acciones_fila = "deshacer",
      vacio = "tablas.bancos.vacio_papelera")

    # ↶ (already confirmed in the table's bar) restores that manual movement.
    observeEvent(t_papelera$accion(), {
      a <- t_papelera$accion()
      id <- as.character(a$ids[1] %||% "")
      if (identical(a$accion, "deshacer") && startsWith(id, "mov:")) .bnc_deshacer_eliminacion(sub("^mov:", "", id))
    }, ignoreInit = TRUE)

    # ── Tab 4: Cuentas — read-only view of Settings → Cuentas de Empresa ────────
    # cuenta_id here = ctas_cuentas$id. Edit/add/deactivate via ⚙ Settings.
    # ── Cuentas (hd_table "cuentas_bancos", T12b): read only, with Excel ──
    t_cuentas <- hd_table_server("cuentas", tabla = "cuentas_bancos",
      datos = reactive({
        cts <- dplyr::filter(cuentas(), activa == TRUE) |> dplyr::arrange(empresa, banco, moneda)
        data.frame(id = as.character(cts$cuenta_id), Empresa = as.character(cts$empresa),
                   Banco = as.character(cts$banco), Moneda = as.character(cts$moneda),
                   Alias = as.character(cts$alias %||% ""), Cuenta = as.character(cts$numero_cuenta %||% ""),
                   CLABE = as.character(cts$clabe %||% ""), stringsAsFactors = FALSE)
      }),
      columnas = list(col_empresa("Empresa", "tablas.facturas.empresa"),
                      col_texto("Banco", "tablas.bancos.banco"),
                      col_texto("Moneda", "tablas.facturas.moneda", recorte = FALSE),
                      col_texto("Alias", "tablas.bancos.alias"),
                      col_texto("Cuenta", "tablas.bancos.cuenta", recorte = FALSE),
                      col_texto("CLABE", "tablas.bancos.clabe", recorte = FALSE)),
      vacio = "tablas.bancos.vacio_cuentas")
  }) # end moduleServer
}
