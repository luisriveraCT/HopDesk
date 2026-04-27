(function() {
  console.info('[VEN] vencidos.js v6 loaded');

  // -- State -----------------------------------------------------------------
  var _venSortCol        = null;
  var _venSortDir        = 'asc';
  var _venLastRow        = null;
  var _venGroupsExpanded = false;
  var _venDocShown       = false;

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
      ledger   : r.dataset.ledger,
      empresa  : r.dataset.empresa,
      moneda   : r.dataset.moneda,
      documento: r.dataset.documento,
      source   : r.dataset.source,
      inv_id   : r.dataset.invid    || '',
      parte    : r.dataset.parteraw || r.dataset.parte,
      importe  : parseFloat(r.dataset.importe),
      fecha    : r.dataset.fecha,
      tipo     : r.dataset.tipo
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
      var toYMD = function(s) {
        return (s || '').replace(/(\d{2})\/(\d{2})\/(\d{4})/, '$3$2$1');
      };
      return dir * toYMD(a.dataset.fecha).localeCompare(toYMD(b.dataset.fecha));
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
    if (!rows.length) { alert('Selecciona al menos una factura (haz clic en filas para seleccionar).'); return; }
    if (action === 'delete') {
      if (!confirm('\u00bfEliminar ' + rows.length + ' factura(s)?\n\nSe guardar\u00e1n en la papelera.')) return;
    }
    var payload = { action: action, rows: rows.map(venRowPayload), nonce: Math.random() };
    if (action === 'move') {
      var d = document.getElementById('ven_move_date');
      payload.move_to = d ? d.value : '';
      if (!payload.move_to) { alert('Elige una fecha para mover.'); return; }
    }
    Shiny.setInputValue('vencidos_action', payload, { priority: 'event' });
    venSelectNone();
  };

  // -- Stage: Agregar todo ---------------------------------------------------
  window.venStageAll = function() {
    var rows = venAllRows().filter(venMatchesFilter);
    if (!rows.length) { alert('No hay facturas.'); return; }
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
    if (!rows.length) { alert('Selecciona al menos una factura.'); return; }
    var bar = document.getElementById('ven_stage_confirm_bar');
    var msg = document.getElementById('ven_stage_confirm_msg');
    if (!bar || !msg) return;
    msg.textContent = '\u00bfAgregar ' + rows.length + ' factura(s) seleccionadas a la Agenda del d\u00eda?';
    bar.style.display = 'flex';
    bar.dataset.pendingRows = JSON.stringify(rows.map(venRowPayload));
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

})();
