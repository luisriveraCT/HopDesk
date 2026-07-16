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

# Filter movements by period
.filter_periodo <- function(df, periodo, desde = NULL, hasta = NULL) {
  hoy <- Sys.Date()
  ms  <- function(d) as.Date(format(d, "%Y-%m-01"))
  switch(periodo,
    mes_actual   = dplyr::filter(df, fecha >= ms(hoy), fecha <= hoy),
    mes_anterior = {
      inicio <- ms(hoy - 32)
      fin    <- ms(hoy) - 1
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
    .bnc-dashboard { display:flex; flex-wrap:wrap; gap:12px; padding:12px 0; }
    .bnc-card {
      background:#fff; border:0.5px solid #dee2e6; border-radius:8px;
      padding:12px 16px; min-width:180px; flex:1;
      border-left-width:4px; border-left-style:solid; border-left-color:#adb5bd;
      overflow:hidden;
    }
    .bnc-card .bnc-card-emp {
      font-size:.75rem; font-weight:700; color:#1a1a1a;
      text-transform:uppercase; letter-spacing:.07em; margin-bottom:2px;
    }
    .bnc-card .bnc-card-bal {
      font-size:1.4rem; font-weight:700; margin:3px 0 2px; color:#1a1a1a;
    }
    .bnc-card .bnc-card-bal.bnc-neg-bal { color:#dc3545; }
    .bnc-card .bnc-card-mon {
      font-size:.78rem; font-weight:600;
    }
    .bnc-card .bnc-spark-wrap { margin-top:6px; height:28px; }
    .bnc-card .bnc-spark { width:100%; height:28px; display:block; }
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

# ── Row-selection + Vincular-mode JS (injected once in bancosUI) ─────────────
bancos_libro_js <- function(id) {
  pfx <- paste0(id, "-")   # e.g. "bnc-"
  js  <- sprintf(
'(function() {
  "use strict";
  var NS      = "%s";
  var sel     = [];
  var _lastRow        = null;   /* last-clicked TR for shift-range */
  var vinMode         = false;
  var _vinBtnOrigHtml = null;
  var _flujoIdx       = 0;      /* 0=todos 1=egresos 2=ingresos */
  var _flujoStates    = ["todos","egresos","ingresos"];
  var _flujoLabels    = ["Todos","Egresos","Ingresos"];
  var _comVisible     = false;

  /* Inject namespaced crosshair CSS so it scopes to this module instance */
  (function() {
    var s = document.createElement("style");
    s.textContent = ".bnc-vincular-mode #" + NS + "libro_tbl tbody tr { cursor:crosshair !important; }";
    document.head.appendChild(s);
  })();

  function $id(n) { return document.getElementById(NS + n); }

  function updatePill() {
    var n = sel.length;
    var p = $id("sel_pill");
    if (!p) return;
    p.style.display = n > 0 ? "inline-flex" : "none";
    if (n > 0) {
      var t = $id("sel_pill_text");
      if (t) t.textContent = "\\u2713 " + n +
        " movimiento" + (n === 1 ? "" : "s") +
        " seleccionado" + (n === 1 ? "" : "s");
    }
    var b = $id("btn_eliminar");
    if (b) b.disabled = (n === 0);
  }

  function clearAll() {
    sel = []; _lastRow = null;
    $("#" + NS + "libro_tbl tbody tr").removeClass("bnc-row-selected");
    updatePill();
  }

  /* Use data-id (set by createdRow) — avoids reading td:eq(0) which
     resolves to the first VISIBLE cell (Fecha) when the id column is
     hidden, causing all rows with the same date to share one apparent ID. */
  function getRowId(tr) {
    return (tr.getAttribute && tr.getAttribute("data-id")) ||
           $(tr).find("td").eq(0).text().trim();
  }

  function highlightVisible() {
    $("#" + NS + "libro_tbl tbody tr").each(function() {
      $(this).toggleClass("bnc-row-selected", sel.indexOf(getRowId(this)) >= 0);
    });
  }

  /* ── Row clicks ─────────────────────────────────────────── */
  $(document).on("click", "#" + NS + "libro_tbl tbody tr", function(e) {
    if (vinMode) {
      var rid = getRowId(this);
      if (rid) Shiny.setInputValue(NS + "vincular_row_id",
        { id: rid, nonce: Math.random() }, { priority: "event" });
      return;
    }
    var rid = getRowId(this);
    if (!rid) return;

    if (e.shiftKey && _lastRow && _lastRow !== this) {
      var rows = $("#" + NS + "libro_tbl tbody tr").toArray();
      var i1   = rows.indexOf(_lastRow), i2 = rows.indexOf(this);
      if (i1 >= 0 && i2 >= 0) {
        var lo  = Math.min(i1, i2), hi = Math.max(i1, i2);
        var add = sel.indexOf(rid) < 0;   /* match target row state */
        for (var k = lo; k <= hi; k++) {
          var r = getRowId(rows[k]);
          if (!r) continue;
          var p = sel.indexOf(r);
          if (add  && p < 0) sel.push(r);
          if (!add && p >= 0) sel.splice(p, 1);
        }
      } else {
        var pos = sel.indexOf(rid);
        if (pos >= 0) sel.splice(pos, 1); else sel.push(rid);
      }
    } else {
      var pos = sel.indexOf(rid);
      if (pos >= 0) sel.splice(pos, 1); else sel.push(rid);
    }
    _lastRow = this;
    highlightVisible(); updatePill();
  });

  /* Click outside rows → deselect all */
  $(document).on("click", "#" + NS + "libro_tbl", function(e) {
    if (!$(e.target).closest("tbody tr").length) clearAll();
  });

  /* ── Pill clear ─────────────────────────────────────────── */
  $(document).on("click", "#" + NS + "sel_pill_clear", function(e) {
    e.stopPropagation(); clearAll();
  });

  /* ── Eliminar button ────────────────────────────────────── */
  $(document).on("click", "#" + NS + "btn_eliminar:not([disabled])", function() {
    if (sel.length === 0) return;
    Shiny.setInputValue(NS + "eliminar_rows",
      { ids: sel.slice(), nonce: Math.random() }, { priority: "event" });
  });

  /* ── Flujo cycling button ───────────────────────────────── */
  var _flujoClasses = ["bnc-flujo-todos","bnc-flujo-egresos","bnc-flujo-ingresos"];
  $(document).on("click", "#" + NS + "btn_flujo_cycle", function() {
    _flujoIdx = (_flujoIdx + 1) %% 3;
    this.textContent = _flujoLabels[_flujoIdx];
    $(this).removeClass("bnc-flujo-todos bnc-flujo-egresos bnc-flujo-ingresos")
           .addClass(_flujoClasses[_flujoIdx]);
    Shiny.setInputValue(NS + "flujo_filter", _flujoStates[_flujoIdx], { priority: "event" });
  });

  /* ── Comisiones toggle button ───────────────────────────── */
  $(document).on("click", "#" + NS + "btn_comisiones_toggle", function() {
    _comVisible = !_comVisible;
    this.textContent = _comVisible ? "Con comisiones" : "Sin comisiones";
    $(this).toggleClass("bnc-com-active", _comVisible);
    Shiny.setInputValue(NS + "mostrar_comisiones", _comVisible, { priority: "event" });
  });

  /* ── Vincular button ────────────────────────────────────── */
  function deactivateVin() {
    vinMode = false;
    var b = $id("btn_vincular");
    if (b) {
      $(b).removeClass("btn-warning").addClass("btn-outline-primary");
      if (_vinBtnOrigHtml) b.innerHTML = _vinBtnOrigHtml;
    }
    $("body").removeClass("bnc-vincular-mode");
  }

  $(document).on("click", "#" + NS + "btn_vincular", function() {
    if (vinMode) { deactivateVin(); return; }
    if (!_vinBtnOrigHtml) _vinBtnOrigHtml = this.innerHTML;
    vinMode = true;
    $(this).removeClass("btn-outline-primary").addClass("btn-warning")
           .html("\\u2715 Cancelar");
    $("body").addClass("bnc-vincular-mode");
  });

  /* ESC cancels Vincular mode */
  $(document).on("keydown.bnc", function(e) {
    if (e.key === "Escape" && vinMode) deactivateVin();
  });

  /* ── Conciliar button (REMOVED — code preserved for future use) ──────────
  $(document).on("click", "#" + NS + "btn_sugerencias", function() {
    Shiny.setInputValue(NS + "open_sugerencias", Math.random(), { priority: "event" });
  });
  ── END CONCILIAR_REMOVED ─────────────────────────────────────────────── */

  /* ── Re-highlight after DT redraw; reset last-row anchor ── */
  $(document).on("draw.dt", "#" + NS + "libro_tbl table", function() {
    _lastRow = null;   /* stale DOM ref after redraw */
    highlightVisible();
  });

  window.__bncDeactivateVin  = deactivateVin;
  window.__bncClearSelection = clearAll;

  /* Custom message handlers — namespaced to avoid collisions across instances */
  Shiny.addCustomMessageHandler(NS + "clear_selection", function(x) { clearAll(); });
  Shiny.addCustomMessageHandler(NS + "deactivate_vin",  function(x) { deactivateVin(); });
  /* sug_badge handler (REMOVED with Conciliar feature):
  Shiny.addCustomMessageHandler(NS + "sug_badge", function(n) {
    var b = $id("sug_badge");
    if (!b) return;
    if (n > 0) { b.textContent = n; b.style.display = ""; }
    else        { b.style.display = "none"; }
  }); */
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

# ── Safe COMPANY_MAP lookup ───────────────────────────────────────────────────
# Uses single [ (returns NA, never errors) instead of [[ (errors on unknown key).
.company_name <- function(ini) {
  ini <- trimws(ini %||% "")
  if (!nzchar(ini)) return(ini)
  nm <- COMPANY_MAP[ini]          # returns NA_character_ for unknown keys
  if (is.na(nm)) ini else unname(nm)
}

# ── Badge HTML helper ─────────────────────────────────────────────────────────
.tipo_badge <- function(tipo) {
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
        div(class = "p-3",
          # Dashboard cards
          uiOutput(ns("dashboard_cards")),
          tags$hr(),
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
            # Trash icon only (enabled = fills red via .bnc-btn-trash CSS)
            tags$button(
              id = ns("btn_eliminar"), type = "button",
              class = "btn btn-sm btn-outline-danger bnc-btn-trash",
              disabled = "disabled",
              title = "Eliminar seleccionados",
              HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6l-1 14a2 2 0 0 1-2 2H8a2 2 0 0 1-2-2L5 6"/><path d="M10 11v6"/><path d="M14 11v6"/><path d="M9 6V4h6v2"/></svg>')
            ),
            # Vincular
            tags$button(
              id = ns("btn_vincular"), type = "button",
              class = "btn btn-sm btn-outline-primary",
              style = "font-size:.78rem; padding:3px 9px; white-space:nowrap;",
              HTML('<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round" style="margin-right:3px;vertical-align:-1px"><path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"/><path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"/></svg>Vincular')
            ),
            # CONCILIAR_REMOVED: Conciliar button was here; code preserved in
            # if(FALSE) blocks — see bancos_libro_js() and server section.
            actionButton(ns("add_mov_manual"), icon("plus"),
                         label = " Agregar",
                         class  = "btn btn-sm btn-outline-primary",
                         style  = "font-size:.78rem; padding:3px 9px; white-space:nowrap;"),
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
                '</svg>Exportar')),
              class = "btn btn-sm btn-outline-secondary",
              style = "font-size:.78rem; padding:3px 9px; white-space:nowrap;"
            )
          ),
          # ── Selection pill ───────────────────────────────────────────────
          div(style = "min-height:28px; margin-bottom:6px;",
            div(
              id    = ns("sel_pill"),
              class = "bnc-sel-pill",
              style = "display:none;",
              tags$span(id = ns("sel_pill_text"), "\u2713 0 seleccionados"),
              tags$button(
                id = ns("sel_pill_clear"), type = "button",
                class = "bnc-sel-pill-clear",
                HTML("&times;")
              )
            )
          ),
          # Movement table (wrapped for CSS scoping)
          div(class = "bnc-libro-wrap",
            DT::dataTableOutput(ns("libro_tbl"))
          ),
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
            div(class = "p-3",
              div(class = "d-flex gap-2 mb-2 flex-wrap",
                selectInput(ns("hist_empresa_filter"), NULL,
                            choices = c("Todas las empresas" = ""),
                            width = "200px"),
                selectInput(ns("hist_tipo_filter"), NULL,
                            choices = c("Pagos y cobros" = "",
                                        "Solo pagos" = "pago",
                                        "Solo cobros" = "cobro"),
                            width = "180px"),
                textInput(ns("hist_search"), NULL,
                          placeholder = "Buscar parte, documento…",
                          width = "250px")
              ),
              DT::dataTableOutput(ns("historial_tbl"))
            )
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
                  DT::dataTableOutput(ns("reasig_preview_tbl"))
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
              div(class = "mb-2",
                textInput(ns("papelera_search"), NULL,
                          placeholder = "Buscar parte, documento\u2026",
                          width = "250px")
              ),
              DT::dataTableOutput(ns("papelera_tbl"))
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
          DT::dataTableOutput(ns("cuentas_tbl"))
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
          save_bancos_movimientos(movs, client_id = shared$effective_client_id())
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

    # ── CONCILIAR_REMOVED: sug_scoped_data (preserved, not active) ───────────
    if (FALSE) {
    sug_scoped_data <- reactive({
      cuenta_sel <- input$libro_cuenta %||% ""
      m <- movs_all()
      c <- confirmados()
      if (nzchar(cuenta_sel) && cuenta_sel != "__sin_cuenta__") {
        m <- dplyr::filter(m, cuenta_id == cuenta_sel)
        c <- dplyr::filter(c, cuenta_id == cuenta_sel)
      } else if (cuenta_sel == "__sin_cuenta__") {
        m <- dplyr::filter(m, is.na(cuenta_id) | cuenta_id == "")
        c <- dplyr::filter(c, is.na(cuenta_id) | cuenta_id == "")
      }
      list(movs = m, conf = c)
    })
    } # END CONCILIAR_REMOVED

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

    # Update historial empresa filter
    observe({
      emps <- c("Todas las empresas" = "",
                sort(unique(confirmados()$empresa)))
      updateSelectInput(session, "hist_empresa_filter", choices = emps)
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

        div(
          class = "bnc-card",
          style = paste0("border-left-color:", color, ";"),
          div(class = "bnc-card-emp", ct$empresa),
          div(class = paste("bnc-card-bal", if (is_neg) "bnc-neg-bal" else ""),
              bal_lbl),
          div(class = "bnc-card-mon",
              style = paste0("color:", color, ";"),
              paste0(ct$banco, " \u00b7 ", ct$moneda, " \u00b7 ", ct$alias)),
          div(class = "bnc-spark-wrap", spark_svg)
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
                paste0(.company_name(ct$empresa), " \u00b7 ", ct$alias)),
            div(
              tags$span(class = paste("bnc-ticker-val", cls), fmt_money(saldo)),
              tags$span(class = "bnc-ticker-mon", mon)
            )
          )
        )
      })

      div(class = "bnc-ticker", !!!items)
    })

    # ── Tab 1: Movement table ─────────────────────────────────────────────────
    output$libro_tbl <- DT::renderDataTable({
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
      movs     <- dplyr::left_join(movs, cts_disp, by = "cuenta_id")

      todas <- cuenta_sel == "__todas__"

      if (!nrow(movs)) {
        empty_df <- data.frame(
          id = character(), Empresa = character(),
          `Fecha y Hora` = character(), Moneda = character(),
          `Descripcion` = character(),
          Cargo = character(), Abono = character(), Saldo = character(),
          check.names = FALSE
        )
        col_defs <- list(
          list(visible = FALSE, targets = 0),
          if (!todas) list(visible = FALSE, targets = 1) else NULL
        )
        col_defs <- Filter(Negate(is.null), col_defs)
        return(DT::datatable(
          empty_df, escape = FALSE, rownames = FALSE, class = "display compact",
          options = list(
            dom = "t", columnDefs = col_defs,
            language = list(emptyTable = "Sin movimientos en este per\u00edodo")
          )
        ))
      }

      tbl <- movs |>
        dplyr::mutate(
          .row_class  = dplyr::case_when(
            tipo == "comision" ~ "bnc-row-comision",
            conciliado == TRUE ~ "bnc-row-conciliado",
            TRUE               ~ ""
          ),
          Empresa       = {
            ini <- dplyr::na_if(trimws(dplyr::coalesce(empresa, "")), "")
            nm  <- COMPANY_MAP[dplyr::coalesce(ini, "")]
            dplyr::coalesce(
              ifelse(!is.na(nm), unname(nm), NA_character_),
              ini,
              alias
            )
          },
          `Fecha y Hora` = paste(format(fecha, "%d/%m/%Y"), hora),
          Moneda        = toupper(trimws(moneda %||% "")),
          `Descripcion` = htmltools::htmlEscape(
            dplyr::coalesce(dplyr::na_if(trimws(descripcion_raw %||% ""), ""), "\u2014")
          ),
          Cargo         = dplyr::if_else(cargo > 0,
            sprintf('<span class="bnc-row-cargo">%s</span>', fmt_money(cargo)), ""),
          Abono         = dplyr::if_else(abono > 0,
            sprintf('<span class="bnc-row-abono">%s</span>', fmt_money(abono)), ""),
          Saldo         = dplyr::if_else(!is.na(saldo_banco),
            fmt_money(saldo_banco), "<span class='text-muted'>\u2014</span>")
        )

      row_classes <- tbl$.row_class

      date_group_idx <- {
        d <- tbl$fecha
        cumsum(c(TRUE, d[-1] != d[-length(d)])) %% 2L
      }

      # id(0,hidden), Empresa(1,conditional), FechaHora(2), Moneda(3), Desc(4), Cargo(5), Abono(6), Saldo(7)
      disp <- dplyr::select(tbl, id, Empresa, `Fecha y Hora`, Moneda,
                            `Descripcion`, Cargo, Abono, Saldo)

      col_defs <- list(
        list(visible = FALSE, targets = 0),
        list(className = "dt-center", targets = 3),
        list(className = "dt-right",  targets = c(5, 6, 7)),
        list(width = "90px",  targets = 2),
        list(width = "50px",  targets = 3),
        list(width = "80px",  targets = c(5, 6, 7)),
        list(targets = 4, className = "dt-left")
      )
      if (!todas) col_defs <- c(col_defs, list(list(visible = FALSE, targets = 1)))

      DT::datatable(
        disp,
        escape    = FALSE,
        rownames  = FALSE,
        selection = "none",
        class     = "display compact",
        options   = list(
          pageLength  = 50,
          dom         = "lfrtip",
          scrollX     = FALSE,
          autoWidth   = FALSE,
          order       = list(),
          columnDefs  = col_defs,
          language    = list(emptyTable = "Sin movimientos"),
          createdRow  = DT::JS("function(row, data) { $(row).attr('data-id', data[0]); }"),
          rowCallback = DT::JS(sprintf(
            "function(row, data, index) {
              var cls = %s;
              if (index < cls.length && cls[index]) $(row).addClass(cls[index]);
              var dg = %s;
              if (index < dg.length && dg[index] === 1) $(row).addClass('bnc-day-alt');
            }",
            jsonlite::toJSON(row_classes),
            jsonlite::toJSON(date_group_idx)
          ))
        )
      )
    }, server = TRUE)

    # ── Exportar Excel ────────────────────────────────────────────────────────
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
        paste0("movimientos_", alias_str, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        cuenta_sel <- input$libro_cuenta %||% "__todas__"
        periodo    <- input$libro_periodo %||% "mes_actual"
        show_com   <- isTRUE(input$mostrar_comisiones)
        flujo      <- input$flujo_filter  %||% "todos"
        rango      <- input$libro_rango

        movs <- movs_active()

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
        if (!show_com) movs <- dplyr::filter(movs, tipo != "comision")
        movs <- switch(flujo,
          "egresos"  = dplyr::filter(movs, cargo > 0),
          "ingresos" = dplyr::filter(movs, abono > 0),
          dplyr::filter(movs, cargo > 0 | abono > 0)
        )

        todas_exp <- cuenta_sel == "__todas__"
        cts_exp   <- dplyr::select(cuentas(), cuenta_id, empresa, alias, moneda)
        movs      <- dplyr::left_join(movs, cts_exp, by = "cuenta_id")

        base_exp <- dplyr::transmute(movs,
          `Fecha Movimiento` = format(as.Date(fecha), "%d-%b-%Y"),
          Hora               = as.character(hora),
          Recibo             = as.character(recibo),
          `Descripcion`      = descripcion_raw,
          Cargos             = cargo,
          Abonos             = abono,
          Saldo              = saldo_banco,
          .empresa           = {
            ini <- dplyr::na_if(trimws(dplyr::coalesce(empresa, "")), "")
            nm  <- COMPANY_MAP[dplyr::coalesce(ini, "")]
            dplyr::coalesce(
              ifelse(!is.na(nm), unname(nm), NA_character_),  # full name from map
              ini,                                              # raw initials if not in map
              alias                                             # account alias when empresa is unset
            )
          },
          .moneda            = toupper(trimws(moneda %||% ""))
        )

        if (todas_exp) {
          export <- dplyr::transmute(base_exp,
            Empresa            = .empresa,
            `Fecha Movimiento` = `Fecha Movimiento`,
            Hora               = Hora,
            Recibo             = Recibo,
            `Descripcion`      = `Descripcion`,
            Cargos             = Cargos,
            Abonos             = Abonos,
            Saldo              = Saldo,
            Moneda             = .moneda
          )
          # col layout: Empresa(1),Fecha(2),Hora(3),Recibo(4),Desc(5),Cargos(6),Abonos(7),Saldo(8),Moneda(9)
          txt_cols   <- 1:5
          col_cargo  <- 6L; col_abono <- 7L; col_saldo <- 8L; col_moneda <- 9L
          col_widths <- c(30, 16, 10, 16, 55, 14, 14, 14, 10)
        } else {
          export <- dplyr::select(base_exp, -`.empresa`, -`.moneda`)
          # col layout: Fecha(1),Hora(2),Recibo(3),Desc(4),Cargos(5),Abonos(6),Saldo(7)
          txt_cols   <- 1:4
          col_cargo  <- 5L; col_abono <- 6L; col_saldo <- 7L; col_moneda <- NULL
          col_widths <- c(16, 10, 16, 55, 14, 14, 14)
        }

        wb <- openxlsx::createWorkbook()
        ws <- "Movimientos"
        openxlsx::addWorksheet(wb, ws)
        openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 10)
        openxlsx::freezePane(wb, ws, firstRow = TRUE)

        mxn <- '$#,##0.00'

        st_hdr <- openxlsx::createStyle(
          fgFill = "#1E3A5F", fontColour = "#FFFFFF",
          fontName = "Calibri", fontSize = 10, textDecoration = "bold",
          halign = "center", valign = "center",
          border = "TopBottomLeftRight", borderColour = "#4A7DB5", borderStyle = "thin"
        )
        st_txt <- openxlsx::createStyle(
          fontName = "Calibri", fontSize = 10, halign = "left", valign = "center",
          border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
        )
        st_num <- openxlsx::createStyle(
          fontName = "Calibri", fontSize = 10, halign = "right", valign = "center",
          numFmt = mxn,
          border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
        )
        st_cargo <- openxlsx::createStyle(
          fontName = "Calibri", fontSize = 10, fontColour = "#C0392B",
          halign = "right", valign = "center", numFmt = mxn,
          border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
        )
        st_abono <- openxlsx::createStyle(
          fontName = "Calibri", fontSize = 10, fontColour = "#1A7A3A",
          halign = "right", valign = "center", numFmt = mxn,
          border = "TopBottomLeftRight", borderColour = "#CCCCCC", borderStyle = "thin"
        )

        openxlsx::writeData(wb, ws, export, startRow = 1, startCol = 1,
                            headerStyle = st_hdr)

        nr <- nrow(export)
        if (nr > 0) {
          data_rows <- seq(2L, nr + 1L)

          openxlsx::addStyle(wb, ws, st_txt, rows = data_rows, cols = txt_cols,  gridExpand = TRUE)
          openxlsx::addStyle(wb, ws, st_num, rows = data_rows, cols = col_saldo, gridExpand = TRUE)
          if (!is.null(col_moneda))
            openxlsx::addStyle(wb, ws, st_txt, rows = data_rows, cols = col_moneda, gridExpand = TRUE)

          cargo_vals <- export$Cargos
          rows_cargo_red   <- which(!is.na(cargo_vals) & cargo_vals > 0) + 1L
          rows_cargo_plain <- setdiff(data_rows, rows_cargo_red)
          if (length(rows_cargo_red)   > 0)
            openxlsx::addStyle(wb, ws, st_cargo, rows = rows_cargo_red,   cols = col_cargo, gridExpand = TRUE)
          if (length(rows_cargo_plain) > 0)
            openxlsx::addStyle(wb, ws, st_num,   rows = rows_cargo_plain, cols = col_cargo, gridExpand = TRUE)

          abono_vals <- export$Abonos
          rows_abono_grn   <- which(!is.na(abono_vals) & abono_vals > 0) + 1L
          rows_abono_plain <- setdiff(data_rows, rows_abono_grn)
          if (length(rows_abono_grn)   > 0)
            openxlsx::addStyle(wb, ws, st_abono, rows = rows_abono_grn,   cols = col_abono, gridExpand = TRUE)
          if (length(rows_abono_plain) > 0)
            openxlsx::addStyle(wb, ws, st_num,   rows = rows_abono_plain, cols = col_abono, gridExpand = TRUE)
        }

        openxlsx::setColWidths(wb, ws, cols = seq_along(col_widths), widths = col_widths)
        openxlsx::setRowHeights(wb, ws, rows = 1, heights = 20)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # ── Reactive state for Vincular / Sugerencias modals ─────────────────────
    vincular_item_rv  <- reactiveVal(NULL)   # chosen movement row
    vin_confirm_rv    <- reactiveVal(NULL)   # NULL=search, list=confirm panel data
    # ── CONCILIAR_REMOVED: Sugerencias reactive state (preserved, not active) ─
    if (FALSE) {
    sug_state_rv      <- reactiveVal("list") # "list" | "confirm"
    sug_confirm_rv    <- reactiveVal(NULL)   # confirm data for sugerencias
    sug_left_rv       <- reactiveVal(NULL)   # selected left-panel item id
    sug_right_rv      <- reactiveVal(NULL)   # selected right-panel item id
    } # END CONCILIAR_REMOVED

    # ── Reasignar cuenta reactives ────────────────────────────────────────────
    reasig_confirm_data_rv    <- reactiveVal(NULL)
    delete_sesion_confirm_rv  <- reactiveVal(NULL)

    # ── Import reactives ──────────────────────────────────────────────────────
    txt_empresa_rv       <- reactiveVal("")
    # TRUE once the user has consciously chosen an account in the banner dropdown.
    # Reset to FALSE on each new file upload so the best-match auto-selects again.
    user_picked_cuenta_rv <- reactiveVal(FALSE)

    # ── Eliminar batch ────────────────────────────────────────────────────────
    observeEvent(input$eliminar_rows, {
      ids  <- input$eliminar_rows$ids
      req(length(ids) > 0)
      movs <- movs_all()
      rows <- dplyr::filter(movs, id %in% ids, !eliminado)
      if (!nrow(rows)) return()

      total_cargo <- sum(rows$cargo, na.rm = TRUE)
      total_abono <- sum(rows$abono, na.rm = TRUE)
      n           <- nrow(rows)

      tbl_rows <- rows |>
        dplyr::transmute(
          Fecha  = format(fecha, "%d/%m/%Y"),
          Parte  = htmltools::htmlEscape(parte %||% ""),
          Cargo  = dplyr::if_else(cargo > 0, fmt_money(cargo), ""),
          Abono  = dplyr::if_else(abono > 0, fmt_money(abono), "")
        )

      showModal(modalDialog(
        title = sprintf("\u00bfEliminar %d movimiento%s?", n, if (n == 1) "" else "s"),
        size  = "m", easyClose = TRUE,
        div(
          DT::dataTableOutput(ns("elim_preview_tbl")),
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
        ),
        tags$input(type = "hidden", id = ns("elim_ids_hidden"),
                   value = paste(ids, collapse = "|"))
      ))

      output$elim_preview_tbl <- DT::renderDataTable({
        DT::datatable(tbl_rows, escape = FALSE, rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = 200, scrollX = FALSE))
      }, server = FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$do_eliminar_confirm, {
      ids_raw <- input$elim_ids_hidden %||% ""
      ids     <- strsplit(ids_raw, "\\|")[[1]]
      ids     <- ids[nzchar(ids)]
      req(length(ids) > 0)

      movs <- movs_all()
      idx  <- which(movs$id %in% ids)
      if (!length(idx)) { removeModal(); return() }

      movs$eliminado[idx]    <- TRUE
      movs$eliminado_at[idx] <- Sys.time()
      shared$bancos_movimientos(movs)
      tryCatch({ save_bancos_movimientos(movs, client_id = shared$effective_client_id()); bump_sync_version("bancos_movimientos_db") },
               error = function(e)
                 showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
      removeModal()
      n <- length(idx)
      showNotification(
        sprintf("%d movimiento%s eliminado%s.", n, if (n==1)"" else "s", if (n==1)"" else "s"),
        type = "message", duration = 3
      )
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "eliminar_movimientos",
        description = paste0(n, " movimiento(s) enviado(s) a papelera"),
        metadata    = list(n = n)
      )
      # Clear JS selection
      session$sendCustomMessage(paste0(id, "-clear_selection"), TRUE)
    }, ignoreInit = TRUE)

    # ── Vincular modal ────────────────────────────────────────────────────────
    # Step 4-5: Vincular mode activates via JS; clicking a row fires
    # input$vincular_row_id. Server opens modal with search panel.

    # Helper: build standardised candidate list from bancos_confirmados + SAP
    .build_candidates_bank_side <- function(item, movs, conf, sap_list) {
      out <- list()
      # Source 1: bancos_confirmados (eliminado=FALSE)
      cf <- dplyr::filter(conf, !eliminado)
      if (nrow(cf)) {
        for (i in seq_len(nrow(cf))) {
          r <- cf[i, ]
          sc <- tryCatch(
            score_vincular_match(item, list(
              rfc = NA_character_, importe = r$importe,
              fecha = r$fecha, parte = r$parte,
              concepto = r$documento
            )),
            error = function(e) 0L
          )
          out[[length(out) + 1]] <- list(
            id        = as.character(r$confirmacion_id)[1L]              %||% "",
            source    = "confirmado",
            score     = sc,
            parte     = as.character(r$parte)[1L]                        %||% "",
            importe   = as.numeric(r$importe)[1L]                        %||% 0,
            fecha     = tryCatch(as.Date(r$fecha)[1L], error = function(e) as.Date(NA)),
            documento = as.character(r$documento)[1L]                    %||% "",
            empresa   = as.character(r$empresa)[1L]                      %||% "",
            tipo      = as.character(r$tipo)[1L]                         %||% ""
          )
        }
      }
      # Source 2: open SAP invoices (AR + AP, current month ±2)
      hoy <- Sys.Date()
      date_lo <- as.Date(format(hoy - 62, "%Y-%m-01"))
      date_hi <- as.Date(format(hoy + 62, "%Y-%m-28"))
      for (ledger_name in c("AR", "AP")) {
        sap_df <- sap_list[[ledger_name]]
        if (is.null(sap_df) || !nrow(sap_df)) next
        date_col <- if ("FechaEff" %in% names(sap_df)) "FechaEff" else
                    if ("Fecha" %in% names(sap_df)) "Fecha" else NA
        if (!is.na(date_col)) {
          sap_df <- tryCatch({
            dates <- as.Date(sap_df[[date_col]])
            sap_df[!is.na(dates) & dates >= date_lo & dates <= date_hi, ]
          }, error = function(e) sap_df)
        }
        if (!nrow(sap_df)) next
        for (i in seq_len(min(nrow(sap_df), 200))) {
          r <- sap_df[i, ]
          rfc_v  <- tryCatch(r[["RFC"]], error = function(e) NA_character_)
          imp_v  <- tryCatch(as.numeric(r[["Importe"]]), error = function(e) NA_real_)
          date_v <- tryCatch(as.Date(r[[date_col]])[1L], error = function(e) as.Date(NA_character_))
          doc_v  <- tryCatch(as.character(r[["Documento"]]), error = function(e) "")
          parte_v<- tryCatch(as.character(r[["Parte"]]), error = function(e) "")
          sc     <- tryCatch(
            score_vincular_match(item, list(
              rfc = rfc_v %||% NA_character_, importe = imp_v %||% 0,
              fecha = date_v, parte = parte_v, concepto = doc_v
            )),
            error = function(e) 0L
          )
          out[[length(out) + 1]] <- list(
            id = paste0("sap_", ledger_name, "_", i), source = paste0("sap_", ledger_name),
            score = sc, parte = parte_v, importe = imp_v %||% 0,
            fecha = date_v, documento = doc_v, empresa = "",
            tipo = if (ledger_name == "AR") "cobro" else "pago"
          )
        }
      }
      out
    }

    .build_candidates_agenda_side <- function(item, movs) {
      bank_movs <- dplyr::filter(movs, !eliminado, id != item$id)
      out <- list()
      for (i in seq_len(nrow(bank_movs))) {
        r  <- bank_movs[i, ]
        sc <- tryCatch(
          score_vincular_match(item, list(
            rfc = r$rfc %||% NA_character_,
            importe = max(r$cargo, r$abono, na.rm = TRUE),
            fecha = r$fecha, parte = r$parte %||% "",
            concepto = r$concepto %||% r$referencia %||% ""
          )),
          error = function(e) 0L
        )
        out[[length(out) + 1]] <- list(
          id        = as.character(r$id)[1L]                              %||% "",
          source    = "mov_txt",
          score     = sc,
          parte     = as.character(r$parte)[1L]                          %||% "",
          importe   = as.numeric(max(r$cargo, r$abono, na.rm = TRUE))[1L] %||% 0,
          fecha     = tryCatch(as.Date(r$fecha)[1L], error = function(e) as.Date(NA)),
          documento = as.character(r$referencia)[1L]                     %||% "",
          empresa   = "",
          tipo      = as.character(r$tipo)[1L]                           %||% ""
        )
      }
      out
    }

    # Render Vincular modal content (search or confirm panel)
    output$vin_modal_content <- renderUI({
      item <- vincular_item_rv()
      req(item)
      confirm <- vin_confirm_rv()

      if (!is.null(confirm)) {
        # ── Confirm panel ──────────────────────────────────────────────────
        it   <- confirm$item
        ca   <- confirm$candidate
        it_imp  <- if (!is.na(it$cargo) && it$cargo > 0) it$cargo else it$abono
        ca_imp  <- ca$importe %||% 0
        it_ref  <- trimws(it$referencia %||% "")
        it_doc  <- trimws(it$concepto   %||% "")
        it_meta <- paste0(
          if (nzchar(it_ref)) paste0('<span style="color:#6c757d;font-size:0.82em;">Ref: ',
            htmltools::htmlEscape(it_ref), '</span>') else "",
          if (nzchar(it_ref) && nzchar(it_doc))
            '<span style="color:#adb5bd;margin:0 6px;">\u00b7</span>' else "",
          if (nzchar(it_doc)) paste0('<span style="color:#6c757d;font-size:0.82em;">Ref: ',
            htmltools::htmlEscape(it_doc), '</span>') else ""
        )
        ca_doc  <- trimws(ca$documento %||% "")
        ca_meta <- if (nzchar(ca_doc))
          paste0('<span style="color:#6c757d;font-size:0.82em;">Ref: ',
                 htmltools::htmlEscape(ca_doc), '</span>')
        else ""

        tagList(
          div(class = "mb-3",
            actionButton(ns("vin_back"), "\u2190 Volver a la lista",
                         class = "btn btn-sm btn-outline-secondary")
          ),
          div(class = "row g-3 mb-3",
            div(class = "col-md-6",
              tags$h6("A \u2014 Movimiento bancario elegido", class = "text-primary"),
              div(class = "bnc-vin-card",
                div(class = "d-flex flex-wrap gap-2 mb-1",
                  tags$strong(htmltools::htmlEscape(it$parte %||% "\u2014")),
                  tags$span(class = "text-muted small", format(it$fecha, "%d/%m/%Y")),
                  HTML(.tipo_badge(it$tipo %||% "otro")),
                  tags$span(class = "badge bg-secondary small", it$fuente %||% "")
                ),
                div(class = "fw-bold", fmt_money(it_imp)),
                if (nzchar(it_meta)) div(class = "text-muted small mt-1", HTML(it_meta))
              )
            ),
            div(class = "col-md-6",
              tags$h6("B \u2014 Coincidencia seleccionada", class = "text-success"),
              div(class = "bnc-vin-card",
                div(class = "d-flex flex-wrap gap-2 mb-1",
                  tags$strong(htmltools::htmlEscape(ca$parte %||% "\u2014")),
                  tags$span(class = "text-muted small",
                    if (length(ca$fecha) && !is.na(ca$fecha[1L])) format(as.Date(ca$fecha[1L]), "%d/%m/%Y") else ""),
                  HTML(.tipo_badge(ca$tipo %||% "otro")),
                  tags$span(class = "badge bg-light text-dark border",
                            htmltools::htmlEscape(ca$source %||% ""))
                ),
                div(class = "fw-bold", fmt_money(ca_imp)),
                if (nzchar(ca_meta)) div(class = "text-muted small mt-1", HTML(ca_meta))
              )
            )
          ),
          p(class = "text-center text-muted small",
            "\u00bfCu\u00e1l deseas conservar? El otro ser\u00e1 marcado como eliminado."),
          div(class = "d-flex gap-3 justify-content-center",
            actionButton(ns("vin_keep_a"), "Conservar A",
                         class = "btn btn-outline-primary"),
            actionButton(ns("vin_keep_b"), "Conservar B",
                         class = "btn btn-outline-success")
          )
        )
      } else {
        # ── Search panel ───────────────────────────────────────────────────
        item_imp  <- if (!is.na(item$cargo) && item$cargo > 0) item$cargo else item$abono
        ref_val   <- trimws(item$referencia %||% "")
        doc_val   <- trimws(item$concepto   %||% "")
        meta_line <- paste0(
          if (nzchar(ref_val)) paste0('<span style="color:#6c757d;font-size:0.82em;">Ref: ',
            htmltools::htmlEscape(ref_val), '</span>') else "",
          if (nzchar(ref_val) && nzchar(doc_val))
            '<span style="color:#adb5bd;margin:0 6px;">\u00b7</span>' else "",
          if (nzchar(doc_val)) paste0('<span style="color:#6c757d;font-size:0.82em;">Ref: ',
            htmltools::htmlEscape(doc_val), '</span>') else ""
        )
        tagList(
          # Chosen item card
          div(class = "bnc-vin-card mb-3",
            div(class = "d-flex flex-wrap gap-2 align-items-center",
              tags$strong(htmltools::htmlEscape(item$parte %||% "\u2014")),
              tags$span(class = "text-muted small", format(item$fecha, "%d/%m/%Y")),
              tags$span(class = "fw-bold", fmt_money(item_imp)),
              HTML(.tipo_badge(item$tipo %||% "otro")),
              tags$span(class = "badge bg-secondary small", item$fuente %||% "")
            ),
            if (nzchar(meta_line)) div(class = "mt-1 small", HTML(meta_line))
          ),
          # Search bar
          div(class = "mb-2",
            textInput(ns("vin_search"), NULL,
                      placeholder = "Buscar por Parte, Documento, RFC, Concepto\u2026",
                      width = "100%")
          ),
          # Candidates list (dynamic)
          uiOutput(ns("vin_candidates_list"))
        )
      }
    })

    # Render candidate list (filtered by search text)
    output$vin_candidates_list <- renderUI({
      item <- vincular_item_rv()
      req(item)
      search_txt <- strip_accents(tolower(trimws(input$vin_search %||% "")))
      use_search <- nzchar(search_txt)

      movs   <- movs_all()
      conf   <- confirmados()
      sap_ls <- tryCatch(shared$sap_data(), error = function(e) list(AR=NULL, AP=NULL))

      cands <- c(
        .build_candidates_agenda_side(item, movs),
        .build_candidates_bank_side(item, movs, conf, sap_ls)
      )

      if (use_search) {
        cands <- Filter(function(c) {
          txt <- strip_accents(tolower(paste(c$parte, c$documento, c$empresa, sep = " ")))
          grepl(search_txt, txt, fixed = TRUE)
        }, cands)
      } else {
        cands <- Filter(function(c) c$score >= 15L, cands)
      }
      cands <- cands[order(sapply(cands, `[[`, "score"), decreasing = TRUE)]

      if (!length(cands)) {
        return(div(class = "text-center text-muted py-3 small",
                   if (use_search) "Sin resultados para esa b\u00fasqueda."
                   else "Sin coincidencias (puntaje \u226515)."))
      }

      div(style = "max-height:340px; overflow-y:auto; border:1px solid #dee2e6; border-radius:6px;",
        lapply(cands, function(ca) {
          ca_source <- as.character(ca$source)[1L] %||% ""
          ca_parte  <- as.character(ca$parte)[1L]  %||% "\u2014"
          ca_fecha  <- ca$fecha[1L]
          ca_doc    <- as.character(ca$documento)[1L] %||% ""
          ca_imp    <- as.numeric(ca$importe)[1L]  %||% 0
          ca_score  <- as.integer(ca$score)[1L]    %||% 0L
          ca_id     <- as.character(ca$id)[1L]     %||% ""
          tags$div(
            class = "bnc-vin-cand-row",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {id:'%s', src:'%s', nonce:Math.random()}, {priority:'event'})",
              ns("vin_candidate"), ca_id, ca_source
            ),
            div(class = "flex-grow-1",
              div(class = "d-flex gap-2 flex-wrap",
                tags$strong(htmltools::htmlEscape(ca_parte)),
                tags$span(class = "text-muted small",
                  if (!is.na(ca_fecha)) format(as.Date(ca_fecha), "%d/%m/%Y") else ""),
                tags$span(class = "badge bg-light text-dark border small",
                          htmltools::htmlEscape(ca_source))
              ),
              div(class = "small text-muted",
                htmltools::htmlEscape(ca_doc))
            ),
            div(class = "d-flex flex-column align-items-end gap-1",
              tags$span(class = "fw-semibold small", fmt_money(ca_imp)),
              if (!use_search)
                tags$span(class = "bnc-score-badge", paste0(ca_score, "%"))
            )
          )
        })
      )
    })

    # Open Vincular modal when a row is clicked in vincular mode
    observeEvent(input$vincular_row_id, {
      req(input$vincular_row_id)
      mov_id <- input$vincular_row_id$id
      movs   <- movs_all()
      row    <- dplyr::filter(movs, id == mov_id)
      if (!nrow(row)) return()

      vincular_item_rv(row[1, ])
      vin_confirm_rv(NULL)

      showModal(modalDialog(
        title = "Vincular movimiento",
        size  = "l", easyClose = FALSE,
        uiOutput(ns("vin_modal_content")),
        footer = modalButton("Cerrar")
      ))
    }, ignoreInit = TRUE)

    # User clicks a candidate → switch to confirm panel
    observeEvent(input$vin_candidate, {
      item <- vincular_item_rv()
      req(item)
      ca_id  <- input$vin_candidate$id
      ca_src <- input$vin_candidate$src

      # Find candidate data from the same sources used in vin_candidates_list
      movs   <- movs_all()
      conf   <- confirmados()
      sap_ls <- tryCatch(shared$sap_data(), error = function(e) list(AR=NULL, AP=NULL))

      cands <- c(
        .build_candidates_agenda_side(item, movs),
        .build_candidates_bank_side(item, movs, conf, sap_ls)
      )
      ca_match <- Filter(function(c) c$id == ca_id && c$source == ca_src, cands)
      if (!length(ca_match)) return()

      vin_confirm_rv(list(item = item, candidate = ca_match[[1]]))
    }, ignoreInit = TRUE)

    # Back to search panel
    observeEvent(input$vin_back, {
      vin_confirm_rv(NULL)
    }, ignoreInit = TRUE)

    # Execute vinculation: Conservar A (keep bank item, mark candidate as eliminated)
    .do_vinculation <- function(keep_row, discard_id, discard_source, movs, conf) {
      keep_id        <- as.character(keep_row$id)[1L]  %||% ""
      discard_id     <- as.character(discard_id)[1L]   %||% ""
      discard_source <- as.character(discard_source)[1L] %||% ""

      # Update surviving row: conciliado=TRUE
      idx_keep <- which(movs$id == keep_id)
      if (length(idx_keep)) {
        movs$conciliado[idx_keep]    <- TRUE
        movs$doc_vinculado[idx_keep] <- discard_id
      }

      if (grepl("^sap_", discard_source)) {
        # SAP items don't live in our tables; just mark the bank row as conciliado
      } else if (discard_source == "confirmado") {
        idx_d <- which(conf$confirmacion_id == discard_id)
        if (length(idx_d)) { conf$eliminado[idx_d] <- TRUE; conf$eliminado_at[idx_d] <- Sys.time() }
      } else {
        # mov_txt or other bancos_movimientos source
        idx_d <- which(movs$id == discard_id)
        if (length(idx_d)) { movs$eliminado[idx_d] <- TRUE; movs$eliminado_at[idx_d] <- Sys.time() }
      }
      list(movs = movs, conf = conf)
    }

    observeEvent(input$vin_keep_a, {
      confirm <- vin_confirm_rv()
      req(confirm)
      item <- confirm$item
      ca   <- confirm$candidate

      movs <- movs_all()
      conf <- confirmados()

      ca_id     <- as.character(ca$id)[1L]     %||% ""
      ca_source <- as.character(ca$source)[1L] %||% ""
      result <- .do_vinculation(item, ca_id, ca_source, movs, conf)
      isolate({
        shared$bancos_movimientos(result$movs)
        shared$bancos_confirmados(result$conf)
      })
      tryCatch({
        save_bancos_movimientos(result$movs, client_id = shared$effective_client_id())
        save_bancos_confirmados(result$conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_movimientos_db")
        bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))

      removeModal()
      vin_confirm_rv(NULL)
      vincular_item_rv(NULL)
      showNotification("Vinculaci\u00f3n completada.", type = "message", duration = 3)
      session$sendCustomMessage(paste0(id, "-deactivate_vin"), TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$vin_keep_b, {
      confirm <- vin_confirm_rv()
      req(confirm)
      item <- confirm$item
      ca   <- confirm$candidate

      # Keep candidate, discard bank item
      movs <- movs_all()
      conf <- confirmados()

      item_id   <- as.character(item$id)[1L]   %||% ""
      ca_id     <- as.character(ca$id)[1L]     %||% ""
      ca_source <- as.character(ca$source)[1L] %||% ""

      # Mark original item as eliminated
      idx_item <- which(movs$id == item_id)
      if (length(idx_item)) { movs$eliminado[idx_item] <- TRUE; movs$eliminado_at[idx_item] <- Sys.time() }

      # Mark candidate as conciliado if it's in movimentos
      if (ca_source == "mov_txt") {
        idx_ca <- which(movs$id == ca_id)
        if (length(idx_ca)) {
          movs$conciliado[idx_ca]    <- TRUE
          movs$doc_vinculado[idx_ca] <- item_id
        }
      } else if (ca_source == "confirmado") {
        # confirmado is the winner; bank movement eliminated above
      }

      isolate({
        shared$bancos_movimientos(movs)
        shared$bancos_confirmados(conf)
      })
      tryCatch({
        save_bancos_movimientos(movs, client_id = shared$effective_client_id())
        save_bancos_confirmados(conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_movimientos_db")
        bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))

      removeModal()
      vin_confirm_rv(NULL)
      vincular_item_rv(NULL)
      showNotification("Vinculaci\u00f3n completada.", type = "message", duration = 3)
      session$sendCustomMessage(paste0(id, "-deactivate_vin"), TRUE)
    }, ignoreInit = TRUE)

    # ── Manual entry modal ────────────────────────────────────────────────────
    observeEvent(input$add_mov_manual, {
      cts <- cuentas()
      cuenta_ch <- c(
        .cuenta_choices(cts, include_sin_cuenta = FALSE),
        "Sin cuenta registrada" = "__sin_cuenta__"
      )

      showModal(modalDialog(
        title = "Agregar movimiento manual",
        size  = "m", easyClose = TRUE,
        div(class = "row g-2",
          div(class = "col-md-6",
            tags$label("Cuenta", class = "form-label small"),
            selectInput(ns("man_cuenta"), NULL, choices = cuenta_ch, width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Fecha", class = "form-label small"),
            dateInput(ns("man_fecha"), NULL, value = Sys.Date(), width = "100%",
                      format = "dd/mm/yyyy", language = "es")
          ),
          div(class = "col-md-6",
            tags$label("Tipo", class = "form-label small"),
            selectInput(ns("man_tipo"), NULL,
                        choices = setNames(names(.TIPO_LABELS), .TIPO_LABELS),
                        selected = "manual_in", width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Parte (beneficiario/ordenante)", class = "form-label small"),
            textInput(ns("man_parte"), NULL, placeholder = "Nombre...", width = "100%")
          ),
          div(class = "col-12",
            tags$label("Concepto", class = "form-label small"),
            textInput(ns("man_concepto"), NULL, placeholder = "Descripci\u00f3n...",
                      width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Cargo ($)", class = "form-label small"),
            numericInput(ns("man_cargo"), NULL, value = 0, min = 0, step = 100,
                         width = "100%")
          ),
          div(class = "col-md-6",
            tags$label("Abono ($)", class = "form-label small"),
            numericInput(ns("man_abono"), NULL, value = 0, min = 0, step = 100,
                         width = "100%")
          ),
          div(class = "col-12",
            tags$label("Notas (opcional)", class = "form-label small"),
            textAreaInput(ns("man_notas"), NULL, rows = 2, width = "100%",
                          resize = "none")
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("do_add_manual"), "Guardar", class = "btn btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$do_add_manual, {
      cuenta_id_sel <- input$man_cuenta %||% "__sin_cuenta__"
      tipo_sel      <- input$man_tipo   %||% "manual_in"
      cargo_v       <- as.numeric(input$man_cargo %||% 0)
      abono_v       <- as.numeric(input$man_abono %||% 0)

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

      movs <- dplyr::bind_rows(movs_all(), new_mov)
      shared$bancos_movimientos(movs)
      tryCatch({ save_bancos_movimientos(movs, client_id = shared$effective_client_id()); bump_sync_version("bancos_movimientos_db") },
               error = function(e)
                 showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
      removeModal()
      showNotification("Movimiento manual guardado.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    # ── CONCILIAR_REMOVED: Sugerencias modal logic (preserved, not active) ────
    if (FALSE) {
    # Helper: compute all unmatched pairs scored ≥25
    .compute_sug_pairs <- function(movs, conf) {
      # Right panel: bank movements (fuente="txt", conciliado=FALSE, eliminado=FALSE)
      bank <- dplyr::filter(movs, fuente == "txt", !conciliado, !eliminado)
      # Left panel: confirmados (eliminado=FALSE, conciliado=FALSE not applicable,
      #             but exclude those already linked) + manual movimientos
      left_conf <- dplyr::filter(conf, !eliminado)
      left_man  <- dplyr::filter(movs, fuente == "manual", !conciliado, !eliminado)

      # Pre-compute candidate amounts once (vectorized, outside the loop)
      conf_amts <- left_conf$importe %||% NA_real_
      man_amts  <- pmax(left_man$cargo %||% 0, left_man$abono %||% 0, na.rm = TRUE)

      pairs <- list()
      # Score each bank movement against each left item
      for (bi in seq_len(nrow(bank))) {
        b     <- bank[bi, ]
        b_amt <- max(b$cargo, b$abono, na.rm = TRUE)

        # Amount pre-screen: only score candidates within 20% of bank amount
        if (!is.na(b_amt) && b_amt > 0) {
          keep_conf <- !is.na(conf_amts) & abs(conf_amts - b_amt) / b_amt <= 0.20
          keep_man  <- !is.na(man_amts)  & abs(man_amts  - b_amt) / b_amt <= 0.20
          sub_conf  <- left_conf[keep_conf, , drop = FALSE]
          sub_man   <- left_man[keep_man,   , drop = FALSE]
        } else {
          sub_conf <- left_conf
          sub_man  <- left_man
        }

        # vs confirmados
        for (ci in seq_len(nrow(sub_conf))) {
          lc <- sub_conf[ci, ]
          sc <- score_vincular_match(b, list(
            rfc = NA_character_, importe = lc$importe,
            fecha = lc$fecha, parte = lc$parte %||% "",
            concepto = lc$documento %||% ""
          ))
          if (sc >= 15L) pairs[[length(pairs)+1]] <- list(
            right_id = b$id, right_src = "mov_txt",
            left_id  = lc$confirmacion_id, left_src = "confirmado",
            score = sc,
            right = list(id=b$id, source="mov_txt",
                         parte=b$parte%||%"", importe=b_amt,
                         fecha=b$fecha, documento=b$referencia%||%"",
                         tipo=b$tipo%||%""),
            left  = list(id=lc$confirmacion_id, source="confirmado",
                         parte=lc$parte%||%"", importe=lc$importe,
                         fecha=lc$fecha, documento=lc$documento%||%"",
                         tipo=lc$tipo%||%"")
          )
        }
        # vs manual movements
        for (mi in seq_len(nrow(sub_man))) {
          lm <- sub_man[mi, ]
          sc <- score_vincular_match(b, list(
            rfc = lm$rfc %||% NA_character_,
            importe = max(lm$cargo, lm$abono, na.rm=TRUE),
            fecha = lm$fecha, parte = lm$parte %||% "",
            concepto = lm$concepto %||% lm$referencia %||% ""
          ))
          if (sc >= 15L) pairs[[length(pairs)+1]] <- list(
            right_id = b$id, right_src = "mov_txt",
            left_id  = lm$id, left_src  = "manual",
            score = sc,
            right = list(id=b$id, source="mov_txt",
                         parte=b$parte%||%"", importe=b_amt,
                         fecha=b$fecha, documento=b$referencia%||%"",
                         tipo=b$tipo%||%""),
            left  = list(id=lm$id, source="manual",
                         parte=lm$parte%||%"",
                         importe=max(lm$cargo,lm$abono,na.rm=TRUE),
                         fecha=lm$fecha, documento=lm$concepto%||%"",
                         tipo=lm$tipo%||%"")
          )
        }
      }
      pairs[order(sapply(pairs, `[[`, "score"), decreasing = TRUE)]
    }

    # Render Sugerencias modal content
    output$sug_modal_content <- renderUI({
      state   <- sug_state_rv()
      confirm <- sug_confirm_rv()
      scoped  <- sug_scoped_data()
      movs    <- scoped$movs
      conf    <- scoped$conf

      if (state == "confirm" && !is.null(confirm)) {
        # ── Confirm panel (same layout as Vincular) ────────────────────────
        it <- confirm$left   # left panel item
        ca <- confirm$right  # right panel item

        tagList(
          div(class = "mb-3",
            actionButton(ns("sug_back"), "\u2190 Volver a sugerencias",
                         class = "btn btn-sm btn-outline-secondary")
          ),
          div(class = "row g-3 mb-3",
            div(class = "col-md-6",
              tags$h6("A \u2014 Calendario / Manual", class = "text-primary"),
              div(class = "bnc-vin-card",
                div(class = "d-flex flex-wrap gap-2 mb-1",
                  tags$strong(htmltools::htmlEscape(it$parte %||% "\u2014")),
                  tags$span(class = "text-muted small",
                    if (!is.na(it$fecha)) format(as.Date(it$fecha), "%d/%m/%Y") else ""),
                  tags$span(class = "badge bg-light text-dark border small",
                            htmltools::htmlEscape(it$source %||% ""))
                ),
                div(class = "fw-bold", fmt_money(it$importe %||% 0)),
                if (nzchar(it$documento %||% ""))
                  div(class = "text-muted small", htmltools::htmlEscape(it$documento))
              )
            ),
            div(class = "col-md-6",
              tags$h6("B \u2014 Movimiento bancario", class = "text-success"),
              div(class = "bnc-vin-card",
                div(class = "d-flex flex-wrap gap-2 mb-1",
                  tags$strong(htmltools::htmlEscape(ca$parte %||% "\u2014")),
                  tags$span(class = "text-muted small",
                    if (!is.na(ca$fecha)) format(as.Date(ca$fecha), "%d/%m/%Y") else ""),
                  .tipo_badge(ca$tipo %||% "otro")
                ),
                div(class = "fw-bold", fmt_money(ca$importe %||% 0)),
                if (nzchar(ca$documento %||% ""))
                  div(class = "text-muted small", htmltools::htmlEscape(ca$documento))
              )
            )
          ),
          p(class = "text-center text-muted small",
            "\u00bfCu\u00e1l deseas conservar?"),
          div(class = "d-flex gap-3 justify-content-center",
            actionButton(ns("sug_keep_a"), "Conservar A",
                         class = "btn btn-outline-primary"),
            actionButton(ns("sug_keep_b"), "Conservar B",
                         class = "btn btn-outline-success")
          )
        )
      } else {
        # ── List panel ─────────────────────────────────────────────────────
        if (!nrow(movs) && !nrow(conf)) {
          return(div(class = "p-3 text-center text-muted small",
                     "No hay movimientos para conciliar."))
        }
        pairs    <- .compute_sug_pairs(movs, conf)
        n_pairs  <- length(pairs)
        left_sel <- sug_left_rv()
        right_sel<- sug_right_rv()

        # Update badge on button via custom message
        session$sendCustomMessage(paste0(id, "-sug_badge"), n_pairs)

        # Build left items (unique left ids in pairs)
        seen_left <- character(0)
        left_items <- Filter(Negate(is.null), lapply(pairs, function(p) {
          if (p$left_id %in% seen_left) return(NULL)
          seen_left <<- c(seen_left, p$left_id)
          p$left
        }))

        seen_right <- character(0)
        right_items <- Filter(Negate(is.null), lapply(pairs, function(p) {
          if (p$right_id %in% seen_right) return(NULL)
          seen_right <<- c(seen_right, p$right_id)
          p$right
        }))

        # If a side is selected, filter the opposite to only show matching pairs
        active_right_ids <- if (!is.null(left_sel)) {
          sapply(Filter(function(p) p$left_id == left_sel, pairs), `[[`, "right_id")
        } else NULL

        active_left_ids <- if (!is.null(right_sel)) {
          sapply(Filter(function(p) p$right_id == right_sel, pairs), `[[`, "left_id")
        } else NULL

        make_sug_item <- function(item, is_active, is_dimmed, side) {
          cls <- paste("bnc-sug-item",
                       if (is_active) "bnc-sug-active" else "",
                       if (is_dimmed) "bnc-sug-dim"    else "")
          tags$div(
            class = cls,
            onclick = if (!is_dimmed) sprintf(
              "Shiny.setInputValue('%s', {id:'%s', src:'%s', side:'%s', nonce:Math.random()}, {priority:'event'})",
              ns("sug_pick"), item$id, item$source, side
            ) else NULL,
            div(class = "fw-semibold small",
                htmltools::htmlEscape(item$parte %||% "\u2014")),
            div(class = "d-flex gap-2 text-muted",
              tags$span(class = "small",
                if (!is.na(item$fecha)) format(as.Date(item$fecha), "%d/%m/%Y") else ""),
              tags$span(class = "small fw-bold", fmt_money(item$importe %||% 0)),
              tags$span(class = "badge bg-light text-dark border",
                        htmltools::htmlEscape(item$source %||% ""))
            ),
            if (nzchar(item$documento %||% ""))
              div(class = "text-muted", style = "font-size:.75rem;",
                  htmltools::htmlEscape(item$documento))
          )
        }

        div(
          div(class = "d-flex justify-content-between align-items-center mb-2",
            actionButton(ns("sug_refresh"), "\u21ba Actualizar",
                         class = "btn btn-sm btn-outline-secondary"),
            tags$span(class = "text-muted small",
                      paste0(n_pairs, " par", if (n_pairs == 1) "" else "es",
                             " sugerido", if (n_pairs == 1) "" else "s"))
          ),
          div(class = "row g-2",
            div(class = "col-md-6",
              tags$h6("Calendario / Manuales", class = "small text-muted text-uppercase"),
              div(class = "bnc-sug-panel",
                if (!length(left_items)) {
                  div(class = "p-3 text-center text-muted small",
                      "Sin elementos sin conciliar")
                } else {
                  lapply(left_items, function(it) {
                    is_active <- !is.null(left_sel) && it$id == left_sel
                    is_dimmed <- !is.null(active_left_ids) && !(it$id %in% active_left_ids)
                    make_sug_item(it, is_active, is_dimmed, "left")
                  })
                }
              )
            ),
            div(class = "col-md-6",
              tags$h6("Movimientos bancarios", class = "small text-muted text-uppercase"),
              div(class = "bnc-sug-panel",
                if (!length(right_items)) {
                  div(class = "p-3 text-center text-muted small",
                      "Sin movimientos bancarios sin conciliar")
                } else {
                  lapply(right_items, function(it) {
                    is_active <- !is.null(right_sel) && it$id == right_sel
                    is_dimmed <- !is.null(active_right_ids) && !(it$id %in% active_right_ids)
                    make_sug_item(it, is_active, is_dimmed, "right")
                  })
                }
              )
            )
          )
        )
      }
    })

    observeEvent(input$open_sugerencias, {
      sug_state_rv("list")
      sug_confirm_rv(NULL)
      sug_left_rv(NULL)
      sug_right_rv(NULL)
      showModal(modalDialog(
        title = "Conciliar movimientos",
        size  = "xl", easyClose = FALSE,
        uiOutput(ns("sug_modal_content")),
        footer = modalButton("Cerrar")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$sug_refresh, {
      sug_left_rv(NULL)
      sug_right_rv(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$sug_pick, {
      req(input$sug_pick)
      side <- input$sug_pick$side
      id   <- input$sug_pick$id

      if (side == "left") {
        if (!is.null(sug_left_rv()) && sug_left_rv() == id) {
          sug_left_rv(NULL)   # deselect
        } else {
          sug_left_rv(id)
          sug_right_rv(NULL)  # clear opposite selection
        }
      } else {
        if (!is.null(sug_right_rv()) && sug_right_rv() == id) {
          sug_right_rv(NULL)
        } else {
          sug_right_rv(id)
          sug_left_rv(NULL)
        }
      }

      # If both sides selected → open confirm panel
      if (!is.null(sug_left_rv()) && !is.null(sug_right_rv())) {
        scoped <- sug_scoped_data()
        pairs  <- .compute_sug_pairs(scoped$movs, scoped$conf)
        match_pair <- Filter(function(p)
          p$left_id == sug_left_rv() && p$right_id == sug_right_rv(), pairs)
        if (length(match_pair)) {
          sug_confirm_rv(list(left = match_pair[[1]]$left,
                              right = match_pair[[1]]$right))
          sug_state_rv("confirm")
        }
      }
    }, ignoreInit = TRUE)

    observeEvent(input$sug_back, {
      sug_state_rv("list")
      sug_confirm_rv(NULL)
      sug_left_rv(NULL)
      sug_right_rv(NULL)
    }, ignoreInit = TRUE)

    # Execute sugerencia vinculation — same logic as Vincular
    observeEvent(input$sug_keep_a, {
      confirm <- sug_confirm_rv()
      req(confirm)
      # Keep LEFT (calendario/manual), eliminate RIGHT (bank movement)
      it <- confirm$left
      ca <- confirm$right
      movs <- movs_all()
      conf <- confirmados()

      # Eliminate bank movement
      idx_r <- which(movs$id == ca$id)
      if (length(idx_r)) { movs$eliminado[idx_r] <- TRUE; movs$eliminado_at[idx_r] <- Sys.time() }

      # Mark left as conciliado if it's a manual movement
      if (it$source == "manual") {
        idx_l <- which(movs$id == it$id)
        if (length(idx_l)) {
          movs$conciliado[idx_l]    <- TRUE
          movs$doc_vinculado[idx_l] <- ca$id
        }
      }
      # confirmado items don't have conciliado field; bank movement elimination is enough

      shared$bancos_movimientos(movs)
      shared$bancos_confirmados(conf)
      tryCatch({
        save_bancos_movimientos(movs, client_id = shared$effective_client_id()); save_bancos_confirmados(conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_movimientos_db"); bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
      sug_state_rv("list"); sug_confirm_rv(NULL)
      sug_left_rv(NULL); sug_right_rv(NULL)
      showNotification("Vinculaci\u00f3n completada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    observeEvent(input$sug_keep_b, {
      confirm <- sug_confirm_rv()
      req(confirm)
      # Keep RIGHT (bank movement), eliminate LEFT (calendario/manual)
      it <- confirm$left
      ca <- confirm$right
      movs <- movs_all()
      conf <- confirmados()

      # Mark bank movement as conciliado
      idx_r <- which(movs$id == ca$id)
      if (length(idx_r)) {
        movs$conciliado[idx_r]    <- TRUE
        movs$doc_vinculado[idx_r] <- it$id
      }

      # Eliminate left item
      if (it$source == "manual") {
        idx_l <- which(movs$id == it$id)
        if (length(idx_l)) { movs$eliminado[idx_l] <- TRUE; movs$eliminado_at[idx_l] <- Sys.time() }
      } else if (it$source == "confirmado") {
        idx_l <- which(conf$confirmacion_id == it$id)
        if (length(idx_l)) { conf$eliminado[idx_l] <- TRUE; conf$eliminado_at[idx_l] <- Sys.time() }
      }

      shared$bancos_movimientos(movs)
      shared$bancos_confirmados(conf)
      tryCatch({
        save_bancos_movimientos(movs, client_id = shared$effective_client_id()); save_bancos_confirmados(conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_movimientos_db"); bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
      sug_state_rv("list"); sug_confirm_rv(NULL)
      sug_left_rv(NULL); sug_right_rv(NULL)
      showNotification("Vinculaci\u00f3n completada.", type = "message", duration = 3)
    }, ignoreInit = TRUE)
    } # END CONCILIAR_REMOVED

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

    output$reasig_preview_tbl <- DT::renderDataTable({
      sesion_key <- input$reasig_sesion %||% ""
      empty_dt <- DT::datatable(
        data.frame(Fecha=character(), Parte=character(), Concepto=character(),
                   Cargo=character(), Abono=character(), Tipo=character()),
        escape = FALSE, rownames = FALSE, selection = "none",
        options = list(dom = "t",
          language = list(emptyTable = "Selecciona una sesi\u00f3n"))
      )
      if (!nzchar(sesion_key)) return(empty_dt)
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      sess_movs <- movs[mask, ]
      if (!nrow(sess_movs)) return(empty_dt)
      tbl <- sess_movs |>
        dplyr::transmute(
          Fecha    = format(fecha, "%d/%m/%Y"),
          Parte    = htmltools::htmlEscape(parte %||% ""),
          Concepto = htmltools::htmlEscape(concepto %||% descripcion_raw %||% ""),
          Cargo    = dplyr::if_else(cargo > 0, fmt_money(cargo), ""),
          Abono    = dplyr::if_else(abono > 0, fmt_money(abono), ""),
          Tipo     = vapply(tipo, .tipo_badge, character(1))
        )
      DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    }, server = FALSE)

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
      data <- reasig_confirm_data_rv()
      req(data)
      sesion_key <- data$sesion_key
      new_cid    <- data$new_cid
      dest_label <- data$dest_label
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      actual_n <- sum(mask)
      if (actual_n == 0) { removeModal(); return() }

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

      shared$bancos_movimientos(movs)
      tryCatch({ save_bancos_movimientos(movs, client_id = shared$effective_client_id()); bump_sync_version("bancos_movimientos_db") },
               error = function(e)
                 showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
      reasig_confirm_data_rv(NULL)
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
      data <- delete_sesion_confirm_rv()
      req(data)
      sesion_key <- data$sesion_key
      user <- tryCatch(shared$current_user(), error = function(e) "system")
      movs <- movs_all()
      mask <- !is.na(movs$importado_at) &
              as.character(movs$importado_at) == sesion_key &
              !is.na(movs$eliminado) & movs$eliminado == FALSE
      actual_n <- sum(mask)
      if (actual_n == 0) { removeModal(); return() }
      movs$eliminado[mask]     <- TRUE
      movs$eliminado_at[mask]  <- Sys.time()
      movs$eliminado_por[mask] <- user
      shared$bancos_movimientos(movs)
      tryCatch({ save_bancos_movimientos(movs, client_id = shared$effective_client_id()); bump_sync_version("bancos_movimientos_db") },
               error = function(e)
                 showNotification("Error al guardar. Intenta de nuevo.", type = "warning"))
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
        metadata    = list(n = actual_n, sesion_key = sesion_key)
      )
    }, ignoreInit = TRUE)

    # ── Tab 2: Import TXT ─────────────────────────────────────────────────────
    parsed_preview <- reactiveVal(NULL)

    # ── Smart import banner — shown after file is loaded ──────────────────────
    output$import_cuenta_banner <- renderUI({
      req(parsed_preview())   # only show after file is parsed

      txt_emp <- txt_empresa_rv()
      cid     <- input$import_cuenta %||% ""
      cts     <- cuentas()
      txt_up  <- toupper(trimws(txt_emp))

      # Active BanBaj\u00edo accounts only (the only parser we support right now)
      cts_baj <- dplyr::filter(cts, activa == TRUE,
                               grepl("baj", banco, ignore.case = TRUE))

      if (!nrow(cts_baj))
        return(div(
          style = "background:#fff3cd; border:1.5px solid #664d03; border-radius:8px; padding:14px 18px; margin-bottom:14px;",
          "\u26a0\ufe0f No hay cuentas BanBaj\u00edo activas configuradas."
        ))

      # Rank by company-name token match: 1 pt per 4+ char word from the
      # account's full company name found in the TXT company string.
      scores <- vapply(seq_len(nrow(cts_baj)), function(i) {
        nm     <- toupper(.company_name(cts_baj$empresa[i]))
        tokens <- strsplit(nm, "\\s+")[[1]]
        tokens <- tokens[nchar(tokens) >= 4]
        if (!length(tokens) || !nzchar(txt_up)) return(0L)
        as.integer(sum(vapply(tokens, function(tok)
          grepl(tok, txt_up, fixed = TRUE), logical(1))))
      }, integer(1))

      cts_baj <- cts_baj[order(scores, decreasing = TRUE), ]

      choices <- setNames(cts_baj$cuenta_id, vapply(seq_len(nrow(cts_baj)), function(i) {
        paste0(.company_name(cts_baj$empresa[i]), " \u2014 ",
               cts_baj$banco[i], " ", cts_baj$moneda[i],
               " (", cts_baj$alias[i], ")")
      }, character(1)))

      # Pre-select the best-scoring account on every new file upload.
      # Once the user has consciously changed the dropdown (user_picked_cuenta_rv),
      # preserve their choice on subsequent re-renders.
      sel <- if (user_picked_cuenta_rv() && nzchar(cid) && cid %in% choices) cid
             else choices[[1L]]

      div(style = "background:#d1e7dd; border:1.5px solid #0f5132; border-radius:8px; padding:14px 18px; margin-bottom:14px;",
        div(class = "d-flex align-items-start gap-3",
          div(style = "flex:1;",
            div(style = "font-size:.8rem; font-weight:700; text-transform:uppercase; letter-spacing:.05em; margin-bottom:4px; color:#212529;",
                "\u2705 Empresa detectada en el archivo"),
            div(style = "font-size:1rem; font-weight:700; color:#212529; margin-bottom:10px;",
                if (nzchar(txt_emp)) txt_emp else "(no detectada)"),
            div(style = "font-size:.8rem; font-weight:600; text-transform:uppercase; letter-spacing:.04em; margin-bottom:4px; color:#495057;",
                "Importar a esta cuenta:"),
            selectInput(ns("import_cuenta"), NULL,
                        choices  = choices,
                        selected = sel,
                        width    = "100%")
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

      # Extract empresa from TXT header line 1; reset account-picker flag
      hdr_parts <- strsplit(lines[1L], ",", fixed = TRUE)[[1]]
      txt_empresa_rv(if (length(hdr_parts) >= 1L) trimws(hdr_parts[1L]) else "")
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

      # Auto-suggest account based on numero_cuenta in line 1.
      # BanBajío TXT headers carry the short 7-8 digit account number, while
      # the Settings "Cuenta origen" field stores the 11-12 digit PPL format
      # (e.g. TXT "13386156" vs Settings "133861560201").  The PPL number
      # always starts with the short TXT number, so we accept either an exact
      # match OR a prefix match (Settings starts with TXT number).
      numero_from_txt <- attr(result, "numero_cuenta")
      if (!is.na(numero_from_txt) && nzchar(numero_from_txt)) {
        txt_num <- trimws(numero_from_txt)
        match_ct <- dplyr::filter(cuentas(),
          trimws(numero_cuenta) == txt_num |
          startsWith(trimws(numero_cuenta), txt_num),
          activa == TRUE)
        if (nrow(match_ct) == 1L && !nzchar(cuenta_id_sel)) {
          updateSelectInput(session, "import_cuenta",
                            selected = match_ct$cuenta_id[1L])
        }
      }

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

      # Preview table (non-comision, first 20 rows)
      preview_rows <- nuevos |>
        dplyr::filter(tipo != "comision") |>
        head(20L) |>
        dplyr::transmute(
          Fecha   = format(fecha, "%d/%m/%Y"),
          Tipo    = vapply(tipo, .tipo_badge, character(1)),
          Parte   = htmltools::htmlEscape(parte %||% ""),
          Concepto= htmltools::htmlEscape(concepto %||% ""),
          Cargo   = dplyr::if_else(cargo > 0, fmt_money(cargo), ""),
          Abono   = dplyr::if_else(abono > 0, fmt_money(abono), "")
        )

      dt_preview <- DT::datatable(
        preview_rows,
        escape = FALSE, rownames = FALSE,
        options = list(dom = "t", pageLength = 20, scrollX = TRUE,
                       language = list(emptyTable = "Sin movimientos nuevos"))
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
              DT::dataTableOutput(ns("import_preview_tbl")), import_footer)
    })

    # Render the import preview DT separately (driven by parsed_preview reactive)
    output$import_preview_tbl <- DT::renderDataTable({
      pv <- parsed_preview()
      if (is.null(pv) || !nrow(pv)) return(DT::datatable(
        data.frame(Fecha=character(), Tipo=character(), Parte=character(),
                   Concepto=character(), Cargo=character(), Abono=character()),
        escape = FALSE, rownames = FALSE,
        options = list(dom = "t", language = list(emptyTable = ""))
      ))

      cid_final <- input$import_cuenta %||% ""
      if (nzchar(cid_final)) pv$cuenta_id <- cid_final

      existentes <- movs_all()
      nuevos     <- dedup_movimientos(pv, existentes)$nuevos

      nuevos |>
        dplyr::filter(tipo != "comision") |>
        head(20L) |>
        dplyr::transmute(
          Fecha    = format(fecha, "%d/%m/%Y"),
          Tipo     = vapply(tipo, .tipo_badge, character(1)),
          Parte    = htmltools::htmlEscape(parte %||% ""),
          Concepto = htmltools::htmlEscape(concepto %||% ""),
          Cargo    = dplyr::if_else(cargo > 0, fmt_money(cargo), ""),
          Abono    = dplyr::if_else(abono > 0, fmt_money(abono), "")
        ) |>
        DT::datatable(escape = FALSE, rownames = FALSE,
          options = list(dom = "t", pageLength = 20, scrollX = TRUE,
                         language = list(emptyTable = "Sin movimientos nuevos")))
    }, server = FALSE)

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
      pv        <- parsed_preview()
      req(pv, nrow(pv) > 0)

      # Always use the currently selected account — the parsed_preview carries a
      # cuenta_id stamped at parse time, which can be stale if the user changed
      # the account selector after the file was uploaded (or if import_cuenta was
      # empty at parse time due to the reactive init order).
      cid_final <- input$import_cuenta %||% ""
      if (nzchar(cid_final)) pv$cuenta_id <- cid_final

      existentes <- movs_all()
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
      shared$bancos_movimientos(movs_new)
      tryCatch({ save_bancos_movimientos(movs_new, client_id = shared$effective_client_id()); bump_sync_version("bancos_movimientos_db") },
               error = function(e)
                 showNotification("Error al guardar. Intenta de nuevo.", type = "error"))

      n_auto  <- sum(nuevos$conciliado, na.rm = TRUE)
      msg     <- paste0(nrow(nuevos), " movimiento(s) importado(s).")
      if (n_auto > 0)
        msg <- paste0(msg, " ", n_auto, " conciliado(s) autom\u00e1ticamente.")

      showNotification(msg, type = "message", duration = 4)
      parsed_preview(NULL)  # clear preview
    }, ignoreInit = TRUE)

    # ── Tab 3: Historial ─────────────────────────────────────────────────────
    output$historial_tbl <- DT::renderDataTable({
      # Primary source: bancos_confirmados (active, not deleted)
      conf <- dplyr::filter(confirmados(), !is.na(eliminado) & !eliminado) |>
        dplyr::mutate(.legacy = FALSE)

      # Secondary source: conciliacion_rv (legacy path — items confirmed before
      # bancos_confirmados existed). Deduplicate by empresa + documento pair.
      conc_raw <- tryCatch(shared$conciliacion_rv(), error = function(e) NULL)
      if (!is.null(conc_raw) && nrow(conc_raw) > 0) {
        bc_keys <- paste(tolower(trimws(conf$empresa)),
                         tolower(trimws(conf$documento)), sep = "|")
        conc_norm <- dplyr::transmute(conc_raw,
          confirmacion_id = id,
          empresa         = tolower(trimws(Empresa)),
          parte           = tolower(trimws(Parte)),
          documento       = tolower(trimws(Documento)),
          moneda          = Moneda,
          importe         = Importe,
          fecha           = FechaPago,
          tipo            = tipo,
          confirmado_at   = created_at,
          .legacy         = TRUE
        )
        conc_norm <- dplyr::filter(conc_norm,
          !paste(empresa, documento, sep = "|") %in% bc_keys)
        if (nrow(conc_norm)) conf <- dplyr::bind_rows(conf, conc_norm)
      }

      # Filters: empresa, tipo, free-text search
      emp_f    <- input$hist_empresa_filter %||% ""
      tipo_f   <- input$hist_tipo_filter    %||% ""
      search_q <- trimws(input$hist_search  %||% "")
      if (nzchar(emp_f))  conf <- dplyr::filter(conf, tolower(empresa) == tolower(emp_f))
      if (nzchar(tipo_f)) conf <- dplyr::filter(conf, tipo == tipo_f)
      if (nzchar(search_q)) {
        q <- tolower(search_q)
        conf <- dplyr::filter(conf,
          grepl(q, tolower(parte      %||% ""), fixed = TRUE) |
          grepl(q, tolower(documento  %||% ""), fixed = TRUE) |
          grepl(q, tolower(empresa    %||% ""), fixed = TRUE)
        )
      }

      if (!nrow(conf)) {
        return(DT::datatable(
          data.frame(Fecha=character(), Empresa=character(), Parte=character(),
                     Documento=character(), Importe=character(),
                     Moneda=character(), Tipo=character(), Acciones=character()),
          escape = FALSE, rownames = FALSE,
          options = list(dom = "t",
                         language = list(emptyTable = "Sin confirmaciones"))
        ))
      }

      tbl <- conf |>
        dplyr::arrange(dplyr::desc(confirmado_at)) |>
        dplyr::mutate(
          Fecha     = format(as.Date(fecha), "%d/%m/%Y"),
          Empresa   = htmltools::htmlEscape(empresa   %||% ""),
          Parte     = htmltools::htmlEscape(parte     %||% ""),
          Documento = htmltools::htmlEscape(documento %||% ""),
          Importe   = fmt_money(importe),
          Moneda    = moneda %||% "",
          Tipo      = ifelse(tipo == "pago",
            '<span class="badge bg-danger">Pago</span>',
            '<span class="badge bg-success">Cobro</span>'),
          Acciones  = ifelse(
            .legacy,
            '<span class="badge bg-secondary" title="Confirmado por la ruta anterior">Legado</span>',
            paste0(
              sprintf(
            '<button class="bnc-btn-xs bnc-btn-xs--undo" title="Deshacer confirmaci\u00f3n y devolver al calendario" onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Math.random()}, {priority:\'event\'})">&#8630;</button>',
                ns("undo_conf"), confirmacion_id),
              sprintf('<button class="bnc-btn-xs" onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Math.random()}, {priority:\'event\'})">&#128465;</button>',
                ns("delete_conf"), confirmacion_id)
            )
          )
        ) |>
        dplyr::select(Fecha, Empresa, Parte, Documento, Importe, Moneda, Tipo, Acciones)

      DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 25, scrollX = TRUE, dom = "lrtip"))
    }, server = TRUE)

    observeEvent(input$delete_conf, {
      conf_id <- input$delete_conf$id
      conf    <- confirmados()
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) return()

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
    }, ignoreInit = TRUE)

    observeEvent(input$do_delete_conf_confirm, {
      conf_id <- session$userData[[paste0(ns("pending_delete_conf_id"))]]
      session$userData[[paste0(ns("pending_delete_conf_id"))]] <- NULL
      req(conf_id)

      conf    <- confirmados()
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) { removeModal(); return() }

      mov_id_linked <- conf$mov_id[idx_c]
      conf$eliminado[idx_c]    <- TRUE
      conf$eliminado_at[idx_c] <- Sys.time()

      if (!is.null(mov_id_linked) && !is.na(mov_id_linked) && nzchar(mov_id_linked)) {
        movs <- movs_all()
        idx_m <- which(movs$id == mov_id_linked)
        if (length(idx_m)) {
          movs$eliminado[idx_m]    <- TRUE
          movs$eliminado_at[idx_m] <- Sys.time()
          shared$bancos_movimientos(movs)
          tryCatch({
            save_bancos_movimientos(movs, client_id = shared$effective_client_id())
            bump_sync_version("bancos_movimientos_db")
          }, error = function(e)
            showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
          )
        }
      }

      shared$bancos_confirmados(conf)
      tryCatch({
        save_bancos_confirmados(conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
      )

      removeModal()
      showNotification("Confirmaci\u00f3n movida a papelera.", type = "message", duration = 2)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "bancos",
        action      = "eliminar_confirmacion",
        description = "Confirmaci\u00f3n bancaria movida a papelera",
        target_id   = conf_id
      )
    }, ignoreInit = TRUE)

    # ── Deshacer confirmación — restore to calendar as pending ───────────────
    observeEvent(input$undo_conf, {
      conf_id <- input$undo_conf$id
      conf    <- confirmados()
      idx_c   <- which(conf$confirmacion_id == conf_id)
      if (!length(idx_c)) return()

      row <- conf[idx_c, , drop = FALSE]

      # Soft-delete the confirmation
      mov_id_linked <- conf$mov_id[idx_c]
      conf$eliminado[idx_c]    <- TRUE
      conf$eliminado_at[idx_c] <- Sys.time()

      # Also soft-delete the linked movimiento if present
      if (!is.null(mov_id_linked) && !is.na(mov_id_linked) && nzchar(mov_id_linked)) {
        movs  <- movs_all()
        idx_m <- which(movs$id == mov_id_linked)
        if (length(idx_m)) {
          movs$eliminado[idx_m]    <- TRUE
          movs$eliminado_at[idx_m] <- Sys.time()
          shared$bancos_movimientos(movs)
          tryCatch({
            save_bancos_movimientos(movs, client_id = shared$effective_client_id())
            bump_sync_version("bancos_movimientos_db")
          }, error = function(e)
            showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
          )
        }
      }

      shared$bancos_confirmados(conf)
      tryCatch({
        save_bancos_confirmados(conf, client_id = shared$effective_client_id())
        bump_sync_version("bancos_confirmados_db")
      }, error = function(e)
        showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
      )

      # Re-stage into pagar_hoy so item reappears on calendar.
      # Primary path: restore the ORIGINAL pagar_hoy row in-place via agenda_item_id.
      # This preserves the original FechaVenc (not the payment date) and exact
      # Empresa/Moneda casing so the item lands on the correct calendar day and
      # appears under the right company tab in Agenda.
      ph_ledger  <- if (isTRUE(row$tipo == "cobro")) "AR" else "AP"
      ph_current <- shared$pagar_hoy_db() %||% load_pagar_hoy()
      orig_id    <- as.character(row$agenda_item_id %||% "")
      orig_idx   <- if (nzchar(orig_id)) which(ph_current$id == orig_id) else integer(0)

      if (length(orig_idx) > 0) {
        # Restore in-place — keeps original FechaVenc, Empresa casing, all fields
        ph_current$status[orig_idx]       <- "pending"
        ph_current$confirmed_at[orig_idx] <- as.POSIXct(NA)
        ph_updated <- ph_current
      } else {
        # Original row gone (e.g., manual item that was physically deleted);
        # synthesize from bancos_confirmados data — FechaVenc falls back to
        # the payment date, which is the best available approximation.
        new_ph_row <- tibble::tibble(
          id           = uuid::UUIDgenerate(),
          ledger       = ph_ledger,
          Empresa      = as.character(row$empresa),
          Moneda       = as.character(row$moneda),
          Documento    = as.character(row$documento),
          Parte        = as.character(row$parte),
          Codigo       = trimws(as.character(row$codigo %||% "")),
          tipo_item    = "factura",
          Importe      = as.numeric(row$importe),
          FechaVenc    = as.Date(row$fecha),
          staged_by    = shared$current_user(),
          staged_at    = Sys.time(),
          status       = "pending",
          provision_id = if ("provision_id" %in% names(row)) as.character(row$provision_id) else NA_character_,
          liability_id = if ("liability_id" %in% names(row)) as.character(row$liability_id) else NA_character_
        )
        ph_updated <- upsert_pagar_hoy(ph_current, new_ph_row)
      }

      shared$pagar_hoy_db(ph_updated)
      tryCatch(
        save_pagar_hoy(ph_updated, shared$current_user(), client_id = shared$effective_client_id()),
        error = function(e)
          showNotification("Error al guardar. Intenta de nuevo.", type = "warning")
      )

      # Notification date: prefer restored row's FechaVenc, fall back to bancos fecha
      restored_fecha <- if (length(orig_idx) > 0 && "FechaVenc" %in% names(ph_updated))
        ph_updated$FechaVenc[orig_idx] else as.Date(row$fecha)
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
        target_id   = conf_id
      )
    }, ignoreInit = TRUE)

    # ── Tab 3: Papelera ───────────────────────────────────────────────────────
    output$papelera_tbl <- DT::renderDataTable({
      movs_del <- dplyr::filter(movs_all(), eliminado == TRUE)
      conf_del <- dplyr::filter(confirmados(), eliminado == TRUE)
      movs_vin <- dplyr::filter(movs_all(),
                                !eliminado,
                                conciliado == TRUE,
                                !is.na(doc_vinculado) & nzchar(doc_vinculado))

      # ── Unified ledger papelera (SAP ghosts, deleted manual/provision rows) ──
      ledger_pap <- if (!is.null(shared$papelera_rv)) {
        tryCatch(shared$papelera_rv(), error = function(e) NULL)
      } else NULL
      rows_ledger <- if (!is.null(ledger_pap) && nrow(ledger_pap)) {
        src_badge <- function(s) {
          # switch(NA, ...) returns NULL in R (not the default value),
          # which breaks vapply. Normalise to "sap" for unknown/missing sources.
          s <- if (is.null(s) || is.na(s) || !nzchar(s)) "sap" else s
          switch(s,
            "sap"       = '<span class="badge bg-secondary">SAP</span>',
            "manual"    = '<span class="badge bg-info text-dark">Manual</span>',
            "provision" = '<span class="badge bg-warning text-dark">Provisión</span>',
            '<span class="badge bg-secondary">ERP</span>'
          )
        }
        ledger_pap |>
          dplyr::transmute(
            Origen          = vapply(source %||% "sap", src_badge, character(1)),
            Fecha           = dplyr::if_else(
              !is.na(FechaEff),
              format(as.Date(FechaEff), "%d/%m/%Y"),
              format(as.Date(deleted_at), "%d/%m/%Y")
            ),
            Parte           = htmltools::htmlEscape(Parte %||% Empresa %||% ""),
            Importe         = fmt_money(Importe %||% 0),
            Tipo            = htmltools::htmlEscape(Documento %||% ""),
            `Doc. vinculado` = htmltools::htmlEscape(Empresa %||% ""),
            `Eliminado el`  = dplyr::if_else(
              !is.na(deleted_at),
              format(as.POSIXct(deleted_at), "%d/%m/%Y %H:%M"),
              "\u2014"
            )
          )
      } else NULL

      if (!nrow(movs_del) && !nrow(conf_del) && !nrow(movs_vin) &&
          (is.null(rows_ledger) || !nrow(rows_ledger))) {
        return(DT::datatable(
          data.frame(Origen=character(), Fecha=character(), Parte=character(),
                     Importe=character(), Tipo=character(),
                     `Doc. vinculado`=character(),
                     `Eliminado el`=character(), check.names=FALSE),
          escape = FALSE, rownames = FALSE,
          options = list(dom = "t", language = list(emptyTable = "Papelera vac\u00eda"))
        ))
      }

      rows_mov <- if (nrow(movs_del)) {
        movs_del |>
          dplyr::transmute(
            Origen          = "Eliminado",
            Fecha           = format(fecha, "%d/%m/%Y"),
            Parte           = htmltools::htmlEscape(parte %||% ""),
            Importe         = dplyr::if_else(cargo > 0, fmt_money(cargo), fmt_money(abono)),
            Tipo            = vapply(tipo, .tipo_badge, character(1)),
            `Doc. vinculado` = "",
            `Eliminado el`  = dplyr::if_else(!is.na(eliminado_at), format(eliminado_at, "%d/%m/%Y %H:%M"), "\u2014")
          )
      } else NULL

      rows_conf <- if (nrow(conf_del)) {
        conf_del |>
          dplyr::transmute(
            Origen          = "Eliminado",
            Fecha           = format(fecha, "%d/%m/%Y"),
            Parte           = htmltools::htmlEscape(parte),
            Importe         = fmt_money(importe),
            Tipo            = dplyr::if_else(tipo == "pago",
              '<span class="badge bg-danger">Pago</span>',
              '<span class="badge bg-success">Cobro</span>'),
            `Doc. vinculado` = "",
            `Eliminado el`  = dplyr::if_else(!is.na(eliminado_at), format(eliminado_at, "%d/%m/%Y %H:%M"), "\u2014")
          )
      } else NULL

      rows_vin <- if (nrow(movs_vin)) {
        movs_vin |>
          dplyr::transmute(
            Origen          = '<span class="badge bg-primary">Vinculado</span>',
            Fecha           = format(fecha, "%d/%m/%Y"),
            Parte           = htmltools::htmlEscape(parte %||% ""),
            Importe         = dplyr::if_else(cargo > 0, fmt_money(cargo), fmt_money(abono)),
            Tipo            = vapply(tipo, .tipo_badge, character(1)),
            `Doc. vinculado` = htmltools::htmlEscape(doc_vinculado %||% ""),
            `Eliminado el`  = "\u2014"
          )
      } else NULL

      tbl <- dplyr::bind_rows(rows_mov, rows_conf, rows_vin, rows_ledger)

      pap_q <- trimws(input$papelera_search %||% "")
      if (nzchar(pap_q) && nrow(tbl)) {
        q <- tolower(pap_q)
        tbl <- dplyr::filter(tbl,
          grepl(q, tolower(Parte              %||% ""), fixed = TRUE) |
          grepl(q, tolower(`Doc. vinculado`   %||% ""), fixed = TRUE)
        )
      }

      DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 25, scrollX = TRUE, dom = "lrtip",
                       language = list(emptyTable = "Papelera vac\u00eda")))
    }, server = TRUE)

    # ── Tab 4: Cuentas — read-only view of Settings → Cuentas de Empresa ────────
    # cuenta_id here = ctas_cuentas$id. Edit/add/deactivate via ⚙ Settings.
    output$cuentas_tbl <- DT::renderDataTable({
      cts <- dplyr::filter(cuentas(), activa == TRUE) |>
        dplyr::arrange(empresa, banco, moneda)

      if (!nrow(cts)) {
        return(DT::datatable(
          data.frame(Empresa = character(), Banco = character(),
                     Moneda  = character(), Alias = character(),
                     Cuenta  = character(), CLABE = character()),
          escape = FALSE, rownames = FALSE,
          options = list(dom = "t",
                         language = list(emptyTable =
                           "Sin cuentas — agrega una en \u2699 Configuraci\u00f3n \u2192 Cuentas de Empresa"))
        ))
      }

      tbl <- cts |>
        dplyr::transmute(
          Empresa = htmltools::htmlEscape(empresa),
          Banco   = htmltools::htmlEscape(banco),
          Moneda  = moneda,
          Alias   = htmltools::htmlEscape(alias %||% ""),
          Cuenta  = htmltools::htmlEscape(numero_cuenta %||% ""),
          CLABE   = htmltools::htmlEscape(clabe %||% "")
        )

      DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 25, dom = "lrtip", scrollX = TRUE))
    }, server = FALSE)
  }) # end moduleServer
}
