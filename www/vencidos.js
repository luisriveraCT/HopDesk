(function() {
  console.info('[VEN] vencidos.js v14 loaded');

  // -- State -----------------------------------------------------------------
  var _venSortCol        = null;
  var _venSortDir        = 'asc';
  var _venLastRow        = null;
  var _venGroupsExpanded = false;
  var _venDocShown       = false;
  var _venOrigShown      = false;
  var _venFechaOrigShown = false;

  // -- Row helpers -----------------------------------------------------------
  function venAllRows()  { return Array.from(document.querySelectorAll('.ven-row')); }
  function venVisibleRows() {
    return venAllRows().filter(function(r) { return r.style.display !== 'none'; });
  }
  function venSelectedRows() {
    return Array.from(document.querySelectorAll('.ven-row.ven-row-selected'));
  }

  function venRowPayload(r) {
    return {
      ledger      : r.dataset.ledger,
      empresa     : r.dataset.empresa,
      moneda      : r.dataset.moneda,
      documento   : r.dataset.documento,
      source      : r.dataset.source,
      inv_id      : r.dataset.invid       || '',
      provision_id: r.dataset.provisionid || '',
      parte       : r.dataset.parteraw || r.dataset.parte,
      codigo      : r.dataset.codigo   || '',
      importe     : parseFloat(r.dataset.importe),
      fecha       : r.dataset.fecha,
      tipo        : r.dataset.tipo
    };
  }

  // -- Filter predicate (reused everywhere) ----------------------------------
  function venMatchesFilter(r) {
    var q    = ((document.getElementById('ven_search_text') || {}).value || '').toLowerCase();
    var tipo = (document.getElementById('ven_tipo')         || {}).value || '';
    var tagF = (document.getElementById('ven_tag_filter')   || {}).value || '';
    if (tipo && r.dataset.tipo !== tipo)                               return false;
    if (q && !((r.dataset.parte || '').includes(q) ||
               (r.dataset.doc   || '').includes(q) ||
               (r.dataset.ref   || '').includes(q)))                   return false;
    var tag = r.dataset.tag || '';
    if (tagF === 'tagged'    && !tag)                        return false;
    if (tagF === 'urgent'    && tag.indexOf('Urgente')    < 0) return false;
    if (tagF === 'important' && tag.indexOf('Importante') < 0) return false;
    if (tagF === 'both'      && tag.indexOf('Ambas')      < 0) return false;
    return true;
  }

  // -- Click handler ---------------------------------------------------------
  document.addEventListener('click', function(e) {
    // Group header click -> select/deselect all its sub-rows
    var groupRow = e.target.closest('.ven-group-row');
    if (groupRow) {
      var gid     = groupRow.dataset.groupId;
      var subRows = Array.from(document.querySelectorAll(
                      '.ven-row.ven-subrow[data-group-id="' + gid + '"]'));
      var allSel  = subRows.length > 0 &&
                    subRows.every(function(r) { return r.classList.contains('ven-row-selected'); });
      subRows.forEach(function(r) {
        r.classList[allSel ? 'remove' : 'add']('ven-row-selected');
      });
      venSyncSelButtons();
      return;
    }
    // Individual row click
    var row = e.target.closest('.ven-row');
    if (!row) return;
    console.log('[VEN] row click cls=' + row.className + ' empresa=' + row.dataset.empresa);
    if (e.shiftKey && _venLastRow && _venLastRow !== row) {
      var all = venVisibleRows();
      var i1  = all.indexOf(_venLastRow);
      var i2  = all.indexOf(row);
      if (i1 < 0 || i2 < 0) {
        row.classList.toggle('ven-row-selected');
      } else {
        var lo = Math.min(i1, i2), hi = Math.max(i1, i2);
        var tgt = !row.classList.contains('ven-row-selected');
        for (var k = lo; k <= hi; k++) all[k].classList[tgt ? 'add' : 'remove']('ven-row-selected');
      }
    } else {
      row.classList.toggle('ven-row-selected');
    }
    _venLastRow = row;
    venSyncSelButtons();
  });

  // -- Sync selection UI -----------------------------------------------------
  function venSyncSelButtons() {
    var n       = venSelectedRows().length;
    var noneBtn = document.getElementById('ven_sel_none_btn');
    var allBtn  = document.getElementById('ven_sel_all_btn');
    if (noneBtn) noneBtn.style.display = n > 0 ? '' : 'none';
    if (allBtn)  allBtn.style.display  = n > 0 ? 'none' : '';
    try {
      document.querySelectorAll('.ven-group-row').forEach(function(gr) {
        var gid     = gr.dataset.groupId;
        var subRows = Array.from(document.querySelectorAll(
                        '.ven-row.ven-subrow[data-group-id="' + gid + '"]'))
                      .filter(venMatchesFilter);
        var allSel  = subRows.length > 0 &&
                      subRows.every(function(r) { return r.classList.contains('ven-row-selected'); });
        var anySel  = subRows.some(function(r)  { return r.classList.contains('ven-row-selected'); });
        gr.classList.toggle('ven-group-all-selected',  allSel);
        gr.classList.toggle('ven-group-some-selected', anySel && !allSel);
      });
    } catch(e) {
      console.error('[VEN] venSyncSelButtons group loop error:', e);
    }
    venUpdateBubble();
  }

  // -- Selection bubble (split AR / AP, then per currency) ------------------
  function venUpdateBubble() {
    var rows   = venSelectedRows();
    var bubble = document.getElementById('ven_sel_bubble');
    if (!bubble) return;
    if (!rows.length) { bubble.style.display = 'none'; return; }
    var ar = {}, ap = {};
    rows.forEach(function(r) {
      var cur = r.dataset.moneda  || '?';
      var lgr = (r.dataset.ledger || '').toUpperCase();
      var map = (lgr === 'AR') ? ar : ap;
      map[cur] = (map[cur] || 0) + (parseFloat(r.dataset.importe) || 0);
    });
    function makeSection(label, map, color) {
      var keys = Object.keys(map);
      if (!keys.length) return '';
      var lines = keys.sort().map(function(cur) {
        var fmt = '$ ' + map[cur].toLocaleString('es-MX',
                    { minimumFractionDigits: 2, maximumFractionDigits: 2 });
        return '<div class="ven-bubble-line">' +
               '<span class="ven-bubble-cur">' + cur + '</span>' +
               '<span class="ven-bubble-amt" style="color:' + color + '">' + fmt + '</span>' +
               '</div>';
      });
      return '<div class="ven-bubble-section-label" style="color:' + color + '">' +
             label + '</div>' + lines.join('');
    }
    var arHtml = makeSection('Cobros (AR)', ar, '#0a58ca');
    var apHtml = makeSection('Pagos (AP)',  ap, '#198754');
    var sep    = (arHtml && apHtml)
                 ? '<div class="ven-bubble-sep"></div>'
                 : '';
    var n = rows.length;
    bubble.innerHTML =
      '<div class="ven-bubble-count">' + n + ' factura' + (n !== 1 ? 's' : '') +
      ' seleccionada' + (n !== 1 ? 's' : '') + '</div>' +
      arHtml + sep + apHtml;
    bubble.style.display = 'block';
  }

  window.venSelectAll = function() {
    venAllRows().forEach(function(r) {
      if (venMatchesFilter(r)) r.classList.add('ven-row-selected');
    });
    venSyncSelButtons();
  };
  window.venSelectNone = function() {
    venAllRows().forEach(function(r) { r.classList.remove('ven-row-selected'); });
    venSyncSelButtons();
  };

  // -- Group expand / collapse -----------------------------------------------
  window.venExpandGroup = function(gid) {
    var gidStr   = String(gid);
    var groupRow = document.querySelector('.ven-group-row[data-group-id="' + gidStr + '"]');
    console.log('[VEN] venExpandGroup gid=' + gidStr + ' found=' + !!groupRow);
    if (!groupRow) return;
    var expanded = groupRow.dataset.expanded === 'true';
    var newExp   = !expanded;
    groupRow.dataset.expanded = String(newExp);
    var btn = groupRow.querySelector('.ven-expand-btn');
    if (btn) btn.innerHTML = newExp ? '&#9650;' : '&#9660;';
    var subs = document.querySelectorAll('.ven-row.ven-subrow[data-group-id="' + gidStr + '"]');
    console.log('[VEN] venExpandGroup subrows=' + subs.length + ' newExp=' + newExp);
    subs.forEach(function(r) {
      var matches = true;
      try { matches = venMatchesFilter(r); } catch(e) { console.error('[VEN] venExpandGroup filter error:', e); }
      r.style.display = (newExp && matches) ? '' : 'none';
    });
  };

  window.venToggleAllGroups = function() {
    _venGroupsExpanded = !_venGroupsExpanded;
    document.querySelectorAll('.ven-group-row').forEach(function(gr) {
      var gidStr = gr.dataset.groupId;
      gr.dataset.expanded = _venGroupsExpanded;
      var btn = gr.querySelector('.ven-expand-btn');
      if (btn) btn.innerHTML = _venGroupsExpanded ? '&#9650;' : '&#9660;';
      document.querySelectorAll('.ven-row.ven-subrow[data-group-id="' + gidStr + '"]')
        .forEach(function(r) {
          var matches = true;
          try { matches = venMatchesFilter(r); } catch(e) { console.error('[VEN] venToggleAllGroups filter error:', e); }
          r.style.display = (_venGroupsExpanded && matches) ? '' : 'none';
        });
    });
    var btn = document.getElementById('ven_groups_btn');
    if (btn) btn.innerHTML = _venGroupsExpanded ? '&#9650;&#9650;' : '&#9660;&#9660;';
  };

  // -- Documento column toggle -----------------------------------------------
  window.venToggleDoc = function() {
    _venDocShown = !_venDocShown;
    var disp = _venDocShown ? '' : 'none';
    document.querySelectorAll('.ven-doc-cell, .ven-doc-th').forEach(function(el) {
      el.style.display = disp;
    });
    var btn = document.getElementById('ven_doc_toggle_btn');
    if (btn) {
      btn.classList.toggle('btn-secondary',         _venDocShown);
      btn.classList.toggle('btn-outline-secondary', !_venDocShown);
    }
  };

  // -- Origen column toggle --------------------------------------------------
  window.venToggleOrig = function() {
    _venOrigShown = !_venOrigShown;
    var disp = _venOrigShown ? '' : 'none';
    document.querySelectorAll('.ven-orig-cell, .ven-orig-th').forEach(function(el) {
      el.style.display = disp;
    });
    var btn = document.getElementById('ven_orig_toggle_btn');
    if (btn) {
      btn.classList.toggle('btn-secondary',         _venOrigShown);
      btn.classList.toggle('btn-outline-secondary', !_venOrigShown);
    }
  };

  // -- Venc. original column toggle -------------------------------------------
  window.venToggleFechaOrig = function() {
    _venFechaOrigShown = !_venFechaOrigShown;
    var disp = _venFechaOrigShown ? '' : 'none';
    document.querySelectorAll('.ven-fechaorig-cell, .ven-fechaorig-th').forEach(function(el) {
      el.style.display = disp;
    });
    var btn = document.getElementById('ven_fechaorig_toggle_btn');
    if (btn) {
      btn.classList.toggle('btn-secondary',         _venFechaOrigShown);
      btn.classList.toggle('btn-outline-secondary', !_venFechaOrigShown);
    }
  };

  // -- Column sort -----------------------------------------------------------
  window.venSortByCol = function(col) {
    if (_venSortCol === col) {
      _venSortDir = _venSortDir === 'asc' ? 'desc' : 'asc';
    } else {
      _venSortCol = col;
      _venSortDir = 'asc';
    }
    document.querySelectorAll('.ven-th-sort').forEach(function(th) {
      var arrow = th.querySelector('.ven-sort-arrow');
      if (!arrow) return;
      if (th.dataset.col === col) {
        arrow.textContent = _venSortDir === 'asc' ? ' \u2191' : ' \u2193';
        arrow.classList.add('active');
      } else {
        arrow.textContent = ' \u2195';
        arrow.classList.remove('active');
      }
    });
    venFilterAndSort();
  };

  function venToYMD(s) {
    return (s || '').replace(/(\d{2})\/(\d{2})\/(\d{4})/, '$3$2$1');
  }

  function venSortRow(a, b) {
    var col = _venSortCol;
    var dir = _venSortDir === 'asc' ? 1 : -1;
    if (!col) {
      var wa = parseInt(a.dataset.tagweight || '4');
      var wb = parseInt(b.dataset.tagweight || '4');
      if (wa !== wb) return wa - wb;
      return parseFloat(b.dataset.importe) - parseFloat(a.dataset.importe);
    }
    if (col === 'importe') {
      return dir * (parseFloat(a.dataset.importe) - parseFloat(b.dataset.importe));
    }
    if (col === 'fecha') {
      return dir * venToYMD(a.dataset.fecha).localeCompare(venToYMD(b.dataset.fecha));
    }
    if (col === 'fecha_orig') {
      return dir * venToYMD(a.dataset.fechaorig).localeCompare(venToYMD(b.dataset.fechaorig));
    }
    var keyMap = { tipo:'tipo', empresa:'empresa', parte:'parte',
                   documento:'doc', referencia:'ref', etiqueta:'tag' };
    var key = keyMap[col] || col;
    return dir * (a.dataset[key] || '').toLowerCase().localeCompare((b.dataset[key] || '').toLowerCase());
  }

  // -- Filter + sort ---------------------------------------------------------
  window.venFilterAndSort = function() {
    try { venFilterAndSortImpl(); } catch(e) { console.error('[VEN] venFilterAndSort error:', e); }
  };
  function venFilterAndSortImpl() {
    document.querySelectorAll('.ven-tbody').forEach(function(tb) {
      var allRows      = Array.from(tb.querySelectorAll('.ven-row'));
      var allGroupRows = Array.from(tb.querySelectorAll('.ven-group-row'));
      var matchingRows = allRows.filter(venMatchesFilter);
      var matchingGids = {};
      matchingRows.forEach(function(r) {
        if (r.classList.contains('ven-subrow') && r.dataset.groupId)
          matchingGids[r.dataset.groupId] = true;
      });
      var standaloneRows = matchingRows.filter(function(r) {
        return !r.classList.contains('ven-subrow');
      });
      var units = [];
      standaloneRows.forEach(function(r) { units.push({ type: 'standalone', row: r }); });
      allGroupRows.forEach(function(gr) {
        var gidStr = gr.dataset.groupId;
        if (!matchingGids[gidStr]) return;
        var grpSubs = matchingRows.filter(function(r) {
          return r.classList.contains('ven-subrow') && r.dataset.groupId === gidStr;
        });
        grpSubs.sort(venSortRow);
        units.push({ type: 'group', groupRow: gr, subRows: grpSubs });
      });
      units.sort(function(a, b) {
        var ra = a.type === 'standalone' ? a.row : a.groupRow;
        var rb = b.type === 'standalone' ? b.row : b.groupRow;
        return venSortRow(ra, rb);
      });
      allRows.forEach(function(r)       { r.style.display = 'none'; });
      allGroupRows.forEach(function(gr) { gr.style.display = 'none'; });
      units.forEach(function(unit) {
        if (unit.type === 'standalone') {
          unit.row.style.display = '';
          tb.appendChild(unit.row);
        } else {
          var gr  = unit.groupRow;
          var exp = gr.dataset.expanded === 'true';
          gr.style.display = '';
          tb.appendChild(gr);
          unit.subRows.forEach(function(r) { r.style.display = exp ? '' : 'none'; tb.appendChild(r); });
        }
      });
      var sec = tb.closest('.ven-ledger-section');
      if (sec) sec.style.display = units.length ? '' : 'none';
    });
    document.querySelectorAll('.ven-cur-section').forEach(function(sec) {
      var any = Array.from(sec.querySelectorAll('.ven-ledger-section'))
                  .some(function(ls) { return ls.style.display !== 'none'; });
      sec.style.display = any ? '' : 'none';
    });
    var n   = venAllRows().filter(venMatchesFilter).length;
    var cnt = document.getElementById('ven_count');
    if (cnt) cnt.textContent = n + ' factura' + (n !== 1 ? 's' : '');
    venSyncSelButtons();
  };

  // -- Edit toggle -----------------------------------------------------------
  window.venToggleEdit = function() {
    var toolbar = document.getElementById('ven_edit_toolbar');
    var btn     = document.getElementById('ven_edit_toggle');
    var active  = toolbar && toolbar.style.display !== 'none';
    if (active) {
      if (toolbar) toolbar.style.display = 'none';
      if (btn) { btn.classList.remove('btn-secondary'); btn.classList.add('btn-outline-secondary'); }
    } else {
      if (toolbar) toolbar.style.display = 'block';
      if (btn) { btn.classList.remove('btn-outline-secondary'); btn.classList.add('btn-secondary'); }
    }
  };

  // -- Actions (incl. hidden sub-rows of collapsed groups) -------------------
  window.venAction = function(action) {
    var rows = venSelectedRows();
    if (!rows.length) {
      Shiny.setInputValue('search_stage_toast',
        { msg: 'Selecciona al menos una factura (haz clic en filas para seleccionar).', type: 'warning', nonce: Math.random() },
        { priority: 'event' });
      return;
    }
    if (action === 'delete') {
      var dbar = document.getElementById('ven_delete_confirm_bar');
      var dmsg = document.getElementById('ven_delete_confirm_msg');
      if (dbar && dmsg) {
        dmsg.textContent = '\u00bfEliminar ' + rows.length + ' factura(s)? Se guardar\u00e1n en la papelera.';
        dbar.style.display = 'flex';
        dbar.dataset.pendingRows = JSON.stringify(rows.map(venRowPayload));
      }
      return;
    }
    var payload = { action: action, rows: rows.map(venRowPayload), nonce: Math.random() };
    if (action === 'move') {
      var d = document.getElementById('ven_move_date');
      payload.move_to = d ? d.value : '';
      if (!payload.move_to) {
        Shiny.setInputValue('search_stage_toast',
          { msg: 'Elige una fecha para mover.', type: 'warning', nonce: Math.random() },
          { priority: 'event' });
        return;
      }
    }
    Shiny.setInputValue('vencidos_action', payload, { priority: 'event' });
    venSelectNone();
  };

  // -- Stage: Agregar todo ---------------------------------------------------
  window.venStageAll = function() {
    var rows = venAllRows().filter(venMatchesFilter);
    if (!rows.length) {
      Shiny.setInputValue('search_stage_toast',
        { msg: 'No hay facturas.', type: 'warning', nonce: Math.random() },
        { priority: 'event' });
      return;
    }
    var bar = document.getElementById('ven_stage_confirm_bar');
    var msg = document.getElementById('ven_stage_confirm_msg');
    if (!bar || !msg) return;
    msg.textContent = '\u00bfAgregar ' + rows.length + ' factura(s) a la Agenda del d\u00eda?';
    bar.style.display = 'flex';
    bar.dataset.pendingRows = JSON.stringify(rows.map(venRowPayload));
  };

  // -- Stage: Agregar seleccion ----------------------------------------------
  window.venStageSelected = function() {
    var rows = venSelectedRows();
    if (!rows.length) {
      Shiny.setInputValue('search_stage_toast',
        { msg: 'Selecciona al menos una factura.', type: 'warning', nonce: Math.random() },
        { priority: 'event' });
      return;
    }
    var bar = document.getElementById('ven_stage_confirm_bar');
    var msg = document.getElementById('ven_stage_confirm_msg');
    if (!bar || !msg) return;
    msg.textContent = '\u00bfAgregar ' + rows.length + ' factura(s) seleccionadas a la Agenda del d\u00eda?';
    bar.style.display = 'flex';
    bar.dataset.pendingRows = JSON.stringify(rows.map(venRowPayload));
  };

  // -- Delete confirmation handlers ------------------------------------------
  window.venConfirmDelete = function() {
    var bar = document.getElementById('ven_delete_confirm_bar');
    if (!bar) return;
    var rows = JSON.parse(bar.dataset.pendingRows || '[]');
    bar.style.display = 'none';
    if (!rows.length) return;
    venSelectNone();
    Shiny.setInputValue('vencidos_action',
      { action: 'delete', rows: rows, nonce: Math.random() },
      { priority: 'event' });
  };
  window.venCancelDelete = function() {
    var bar = document.getElementById('ven_delete_confirm_bar');
    if (bar) bar.style.display = 'none';
  };

  window.venConfirmStage = function() {
    var bar = document.getElementById('ven_stage_confirm_bar');
    if (!bar) return;
    var rows = JSON.parse(bar.dataset.pendingRows || '[]');
    bar.style.display = 'none';
    if (!rows.length) return;
    venSelectNone();
    Shiny.setInputValue('vencidos_action',
      { action: 'stage_all', rows: rows, nonce: Math.random() },
      { priority: 'event' });
  };
  window.venCancelStage = function() {
    var bar = document.getElementById('ven_stage_confirm_bar');
    if (bar) bar.style.display = 'none';
  };

  // -- Restore column/expand state after Shiny re-renders the table ----------
  function venRestoreState() {
    // Expand all groups if the global flag says expanded
    if (_venGroupsExpanded) {
      document.querySelectorAll('.ven-group-row').forEach(function(gr) {
        gr.dataset.expanded = 'true';
        var btn = gr.querySelector('.ven-expand-btn');
        if (btn) btn.innerHTML = '&#9650;';
        var gidStr = gr.dataset.groupId;
        document.querySelectorAll('.ven-row.ven-subrow[data-group-id="' + gidStr + '"]')
          .forEach(function(r) {
            var matches = true;
            try { matches = venMatchesFilter(r); } catch(e) {}
            r.style.display = matches ? '' : 'none';
          });
      });
      var gb = document.getElementById('ven_groups_btn');
      if (gb) gb.innerHTML = '&#9650;&#9650;';
    }
    // Restore Documento column
    if (_venDocShown) {
      document.querySelectorAll('.ven-doc-cell, .ven-doc-th').forEach(function(el) {
        el.style.display = '';
      });
      var db = document.getElementById('ven_doc_toggle_btn');
      if (db) { db.classList.add('btn-secondary'); db.classList.remove('btn-outline-secondary'); }
    }
    // Restore Origen column
    if (_venOrigShown) {
      document.querySelectorAll('.ven-orig-cell, .ven-orig-th').forEach(function(el) {
        el.style.display = '';
      });
      var ob = document.getElementById('ven_orig_toggle_btn');
      if (ob) { ob.classList.add('btn-secondary'); ob.classList.remove('btn-outline-secondary'); }
    }
    // Restore Venc. original column
    if (_venFechaOrigShown) {
      document.querySelectorAll('.ven-fechaorig-cell, .ven-fechaorig-th').forEach(function(el) {
        el.style.display = '';
      });
      var fob = document.getElementById('ven_fechaorig_toggle_btn');
      if (fob) { fob.classList.add('btn-secondary'); fob.classList.remove('btn-outline-secondary'); }
    }
  }

  // Re-apply state whenever Shiny replaces the table body
  $(document).on('shiny:value', function(e) {
    if (e.name && e.name.indexOf('ven_table_ui') >= 0) {
      setTimeout(venRestoreState, 20);
      setTimeout(venRecalcStickyOffsets, 20);
    }
  });

  // -- Sticky offset stack ----------------------------------------------------
  // #ven_sticky_header, the per-section band, and the column headers all
  // live inside the SAME scrolling container (.h-100.overflow-auto). Sticky
  // "top" is relative to that container's own viewport, NOT the outer page —
  // its top edge already sits flush below the global control-bar (see the
  // comment above .ven-section-band in vencidos_module.R), so control-bar's
  // height must NOT be added in here. Only #ven_sticky_header's own height
  // (and, for thead, the band's too) matters, and those can change after
  // first paint (edit toolbar opening) — recomputed from the real DOM.
  function venRecalcStickyOffsets() {
    var hdr  = document.getElementById('ven_sticky_header');
    var band = document.querySelector('.ven-section-band');
    var hdrH  = hdr  ? Math.ceil(hdr.getBoundingClientRect().height)  : 50;
    var bandH = band ? Math.ceil(band.getBoundingClientRect().height) : 34;
    var root = document.documentElement.style;
    root.setProperty('--ven-hdr-h',  hdrH + 'px');
    root.setProperty('--ven-band-h', (hdrH + bandH) + 'px');
  }
  window.venRecalcStickyOffsets = venRecalcStickyOffsets;

  if (window.ResizeObserver) {
    var _venStickyRO = new ResizeObserver(function() { venRecalcStickyOffsets(); });
    var _venStickyObserved = { hdr: null };
    function venObserveStickyEls() {
      var hdr = document.getElementById('ven_sticky_header');
      if (hdr && hdr !== _venStickyObserved.hdr) { _venStickyRO.observe(hdr); _venStickyObserved.hdr = hdr; }
    }
    $(document).on('shiny:connected shiny:value', function() { venObserveStickyEls(); });
    setTimeout(venObserveStickyEls, 300);
  } else {
    window.addEventListener('resize', venRecalcStickyOffsets);
  }
  // Several early attempts: company pills (uiOutput) and fonts can settle
  // their final size slightly after these events fire, and any one wrong
  // reading otherwise persists until the next resize/toggle.
  [0, 100, 300, 600, 1200].forEach(function(ms) { setTimeout(venRecalcStickyOffsets, ms); });
  // Continuous self-heal: cheap (just a few getBoundingClientRect reads) and
  // rAF-throttled, so any residual drift corrects itself within a frame of
  // scrolling instead of staying wrong until the next resize/toggle.
  var _venScrollTicking = false;
  window.addEventListener('scroll', function() {
    if (_venScrollTicking) return;
    _venScrollTicking = true;
    requestAnimationFrame(function() { venRecalcStickyOffsets(); _venScrollTicking = false; });
  }, true);

  // -- Badge -----------------------------------------------------------------
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('vencidosBadge', function(msg) {
      var el = document.getElementById('ven_tab_badge');
      if (!el) return;
      if (msg.count > 0) { el.textContent = msg.count; el.style.display = ''; }
      else                { el.style.display = 'none'; }
    });
  });

  // -- Wire filter inputs ----------------------------------------------------
  function venAttachListeners() {
    ['ven_search_text','ven_tipo','ven_tag_filter'].forEach(function(id) {
      var el = document.getElementById(id);
      if (el && !el._venBound) { el.addEventListener('input', venFilterAndSort); el._venBound = true; }
    });
  }
  $(document).on('shiny:bound shiny:value', venAttachListeners);
  setTimeout(venAttachListeners, 300);

  // -- Cell hover tooltip: full content + copy ------------------------------
  // Table cells (Parte, Referencia, ...) clip long text with text-overflow:
  // ellipsis, but the full text is already in the DOM — no extra data
  // attributes needed. One shared floating tooltip, shown near whichever
  // cell is hovered, with a button to copy that cell's full text.
  var _venTipShowTimer = null;
  var _venTipHideTimer = null;

  function venCellTipEl() { return document.getElementById('ven_cell_tip'); }

  function venCellTipShow(td) {
    var tip = venCellTipEl();
    if (!tip) return;
    var text = (td.innerText || td.textContent || '').replace(/\s+/g, ' ').trim();
    if (!text) return;
    tip.dataset.copyText = text;
    tip.querySelector('.ven-tip-text').textContent = text;
    tip.style.display = 'flex';

    var r = td.getBoundingClientRect();
    tip.style.left = r.left + 'px';
    tip.style.top  = (r.bottom + 4) + 'px';

    // Clamp inside the viewport once we know the tooltip's own size.
    var tr = tip.getBoundingClientRect();
    var left = r.left, top = r.bottom + 4;
    if (tr.right  > window.innerWidth  - 8) left = Math.max(8, window.innerWidth  - tr.width  - 8);
    if (tr.bottom > window.innerHeight - 8) top  = r.top - tr.height - 4;
    tip.style.left = left + 'px';
    tip.style.top  = top  + 'px';
  }

  function venCellTipScheduleHide() {
    clearTimeout(_venTipShowTimer);
    clearTimeout(_venTipHideTimer);
    _venTipHideTimer = setTimeout(function() {
      var tip = venCellTipEl();
      if (tip) tip.style.display = 'none';
    }, 200);
  }

  document.addEventListener('mouseover', function(e) {
    var td = e.target.closest('.ven-tbody td');
    if (td) {
      clearTimeout(_venTipHideTimer);
      clearTimeout(_venTipShowTimer);
      _venTipShowTimer = setTimeout(function() { venCellTipShow(td); }, 350);
      return;
    }
    if (e.target.closest('#ven_cell_tip')) { clearTimeout(_venTipHideTimer); }
  });
  document.addEventListener('mouseout', function(e) {
    var td  = e.target.closest('.ven-tbody td');
    var tip = e.target.closest('#ven_cell_tip');
    if (!td && !tip) return;
    // Moving between nested elements within the same td (e.g. a badge span)
    // or from the td onto the tooltip fires mouseout too — only actually
    // hide once the pointer leaves both for good.
    var to = e.relatedTarget;
    if (to && ((td && td.contains(to)) || (tip && tip.contains(to)))) return;
    venCellTipScheduleHide();
  });

  var _venTip = venCellTipEl();
  if (_venTip) {
    var _venTipCopyBtn = _venTip.querySelector('.ven-tip-copy');
    if (_venTipCopyBtn) {
      _venTipCopyBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        var text = _venTip.dataset.copyText || '';
        if (!text || !navigator.clipboard) return;
        navigator.clipboard.writeText(text).then(function() {
          // Icons render as inline SVG (fontawesome pkg) — the shape is baked
          // into the SVG's path data, so swapping a fa-* class does nothing.
          // Cache/restore the actual markup instead, works regardless of icon
          // rendering mode.
          var origHTML = _venTipCopyBtn.innerHTML;
          _venTipCopyBtn.classList.add('ven-tip-copied');
          _venTipCopyBtn.textContent = '✓';
          setTimeout(function() {
            _venTipCopyBtn.classList.remove('ven-tip-copied');
            _venTipCopyBtn.innerHTML = origHTML;
          }, 900);
        }).catch(function(err) { console.error('[VEN] copy failed:', err); });
      });
    }
  }

})();
