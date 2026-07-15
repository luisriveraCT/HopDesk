(function () {
  'use strict';
  console.info('[CAL-CART] cal_cart.js v4 loaded');

  // ── Inject CSS once ──────────────────────────────────────────────────────
  if (!document.getElementById('cal-cart-styles')) {
    var s = document.createElement('style');
    s.id = 'cal-cart-styles';
    s.textContent =
      '.cart-row { cursor: pointer; }\n' +
      '.cart-row:hover:not(.cart-row-selected) { background: #f0f5ff !important; }\n' +
      '.cart-row.cart-row-selected {\n' +
      '  background: #d0e4ff !important;\n' +
      '  outline: 2px solid #0a58ca; outline-offset: -2px;\n' +
      '}\n' +
      '.cart-inv-row[data-j] { cursor: pointer; }\n' +
      '.cart-inv-row[data-j]:hover:not(.cart-inv-row-selected) { background: #f0f5ff !important; }\n' +
      '.cart-inv-row.cart-inv-row-selected { background: #e8f0fe; }\n' +
      '#cal_cart_bubble {\n' +
      '  position: fixed; bottom: 22px; right: 22px; z-index: 9999;\n' +
      '  background: #fff; border: 2px solid #0a58ca; border-radius: 10px;\n' +
      '  padding: 9px 15px; box-shadow: 0 4px 18px rgba(10,88,202,.18);\n' +
      '  min-width: 155px; pointer-events: none;\n' +
      '}\n' +
      '.cal-bubble-count {\n' +
      '  font-size: .72rem; color: #6c757d; margin-bottom: 4px;\n' +
      '  border-bottom: 1px solid #dee2e6; padding-bottom: 4px;\n' +
      '}\n' +
      '.cal-bubble-line {\n' +
      '  display: flex; justify-content: space-between; gap: 10px;\n' +
      '  font-size: .83rem; font-weight: 600; margin-top: 3px;\n' +
      '}\n' +
      '.cal-bubble-cur { color: #6c757d; font-weight: 400; font-size: .72rem; }';
    document.head.appendChild(s);
  }

  // ── State ────────────────────────────────────────────────────────────────
  var _lastRow    = null;
  var _lastSubRow = null;

  // ── Bubble ───────────────────────────────────────────────────────────────
  function getBubble() {
    var el = document.getElementById('cal_cart_bubble');
    if (!el) {
      el = document.createElement('div');
      el.id = 'cal_cart_bubble';
      el.style.display = 'none';
      document.body.appendChild(el);
    }
    return el;
  }

  function updateBubble(list) {
    var bubble = getBubble();
    if (!list) { bubble.style.display = 'none'; return; }
    var cur = list.dataset.moneda || '';

    // Group-level selected rows
    var groupRows = Array.from(list.querySelectorAll('.cart-row.cart-row-selected'));
    var groupTotal = groupRows.reduce(function (acc, r) {
      return acc + (parseFloat(r.dataset.importe) || 0);
    }, 0);

    // Individual sub-rows selected independently (parent group NOT selected)
    var invRows = Array.from(list.querySelectorAll('.cart-inv-row[data-j].cart-inv-row-selected'))
      .filter(function (sr) {
        var gi = sr.dataset.i;
        var groupRow = list.querySelector('.cart-row[data-i="' + gi + '"]');
        return !groupRow || !groupRow.classList.contains('cart-row-selected');
      });
    var invTotal = invRows.reduce(function (acc, sr) {
      return acc + (parseFloat(sr.dataset.importe) || 0);
    }, 0);

    var n     = groupRows.length + invRows.length;
    var total = groupTotal + invTotal;
    if (!n) { bubble.style.display = 'none'; return; }

    var fmt = '$ ' + total.toLocaleString('es-MX',
      { minimumFractionDigits: 2, maximumFractionDigits: 2 });
    bubble.innerHTML =
      '<div class="cal-bubble-count">' + n + ' partida' + (n !== 1 ? 's' : '') +
      ' seleccionada' + (n !== 1 ? 's' : '') + '</div>' +
      '<div class="cal-bubble-line">' +
      (cur ? '<span class="cal-bubble-cur">' + cur + '</span>' : '') +
      '<span style="color:#0a58ca">' + fmt + '</span></div>';
    bubble.style.display = 'block';
  }

  // ── Sync group-row selection indices to Shiny ─────────────────────────────
  function syncToShiny(list, inputId) {
    if (typeof Shiny === 'undefined' || !inputId) return;
    var indices = Array.from(list.querySelectorAll('.cart-row.cart-row-selected'))
      .map(function (r) { return parseInt(r.dataset.i, 10); })
      .filter(function (x) { return !isNaN(x); });
    Shiny.setInputValue(inputId, indices.length ? indices : null,
      { priority: 'event' });
  }

  // ── Sync individual sub-row selection to Shiny ───────────────────────────
  function syncInvSelToShiny(list, invInputId) {
    if (typeof Shiny === 'undefined' || !invInputId) return;
    var items = Array.from(list.querySelectorAll('.cart-inv-row[data-j].cart-inv-row-selected'))
      .filter(function (sr) {
        var gi = sr.dataset.i;
        var groupRow = list.querySelector('.cart-row[data-i="' + gi + '"]');
        return !groupRow || !groupRow.classList.contains('cart-row-selected');
      })
      .map(function (sr) {
        return { i: parseInt(sr.dataset.i, 10), j: parseInt(sr.dataset.j, 10) };
      });
    Shiny.setInputValue(invInputId, items.length ? items : null,
      { priority: 'event' });
  }

  // ── Mirror group selection state to its expanded sub-rows ────────────────
  function syncSubRows(list, rowEl) {
    var gi    = rowEl.dataset.i;
    var isSel = rowEl.classList.contains('cart-row-selected');
    if (gi && list) {
      list.querySelectorAll('.cart-inv-row[data-i="' + gi + '"]')
        .forEach(function (sr) {
          sr.classList[isSel ? 'add' : 'remove']('cart-inv-row-selected');
        });
    }
  }

  // ── Public: group row toggle ──────────────────────────────────────────────
  window.calCartToggleRow = function (rowEl, inputId, invInputId) {
    var evt = window.event;
    if (evt && (evt.target.closest('button') || evt.target.closest('a'))) return;

    var list    = rowEl.closest('.cart-list');
    if (!list) return;

    var allRows = Array.from(list.querySelectorAll('.cart-row'));

    if (evt && evt.shiftKey && _lastRow && _lastRow !== rowEl) {
      var targetSel = !rowEl.classList.contains('cart-row-selected');
      var i1 = allRows.indexOf(_lastRow);
      var i2 = allRows.indexOf(rowEl);
      if (i1 >= 0 && i2 >= 0) {
        var lo = Math.min(i1, i2), hi = Math.max(i1, i2);
        for (var k = lo; k <= hi; k++) {
          allRows[k].classList[targetSel ? 'add' : 'remove']('cart-row-selected');
          syncSubRows(list, allRows[k]);
        }
      } else {
        rowEl.classList.toggle('cart-row-selected');
        syncSubRows(list, rowEl);
      }
    } else {
      rowEl.classList.toggle('cart-row-selected');
      syncSubRows(list, rowEl);
    }

    _lastRow = rowEl;
    syncToShiny(list, inputId);
    if (invInputId) syncInvSelToShiny(list, invInputId);
    updateBubble(list);
  };

  // ── Public: individual sub-row toggle ────────────────────────────────────
  window.calCartToggleSubRow = function (rowEl, grpInputId, invInputId) {
    var evt = window.event;
    if (evt && (evt.target.closest('button') || evt.target.closest('a'))) return;

    var list = rowEl.closest('.cart-list');
    if (!list) return;

    var allSubRows = Array.from(list.querySelectorAll('.cart-inv-row[data-j]'));

    if (evt && evt.shiftKey && _lastSubRow && _lastSubRow !== rowEl) {
      var targetSel = !rowEl.classList.contains('cart-inv-row-selected');
      var i1 = allSubRows.indexOf(_lastSubRow);
      var i2 = allSubRows.indexOf(rowEl);
      if (i1 >= 0 && i2 >= 0) {
        var lo = Math.min(i1, i2), hi = Math.max(i1, i2);
        for (var k = lo; k <= hi; k++) {
          allSubRows[k].classList[targetSel ? 'add' : 'remove']('cart-inv-row-selected');
        }
      } else {
        rowEl.classList.toggle('cart-inv-row-selected');
      }
    } else {
      rowEl.classList.toggle('cart-inv-row-selected');
    }

    _lastSubRow = rowEl;
    syncInvSelToShiny(list, invInputId);
    updateBubble(list);
  };

  // ── Shiny message: clear all selections ──────────────────────────────────
  function _registerHandlers() {
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler('calCartClearSel', function (msg) {
      var grpInputId = msg.grpInputId;
      var invInputId = msg.invInputId;
      var list = grpInputId
        ? document.querySelector('.cart-list[data-sel-input="' + grpInputId + '"]')
        : null;
      if (list) {
        list.querySelectorAll('.cart-row').forEach(function (r) {
          r.classList.remove('cart-row-selected');
        });
        list.querySelectorAll('.cart-inv-row').forEach(function (r) {
          r.classList.remove('cart-inv-row-selected');
        });
      }
      _lastRow    = null;
      _lastSubRow = null;
      var bubble = document.getElementById('cal_cart_bubble');
      if (bubble) bubble.style.display = 'none';
      if (grpInputId) Shiny.setInputValue(grpInputId, null, { priority: 'event' });
      if (invInputId) Shiny.setInputValue(invInputId, null, { priority: 'event' });
    });
  }
  // Try immediately; retry once Shiny session is connected
  _registerHandlers();
  document.addEventListener('shiny:connected', _registerHandlers);

  // ── Hide bubble when Bootstrap modal closes ──────────────────────────────
  document.addEventListener('hidden.bs.modal', function () {
    _lastRow    = null;
    _lastSubRow = null;
    var bubble = document.getElementById('cal_cart_bubble');
    if (bubble) bubble.style.display = 'none';
  });

  // ── Hide bubble when navigating away from the Calendar tab ───────────────
  document.addEventListener('shown.bs.tab', function (e) {
    var val = e.target && e.target.getAttribute('data-value');
    if (val && val !== 'CAL') {
      _lastRow    = null;
      _lastSubRow = null;
      var bubble = document.getElementById('cal_cart_bubble');
      if (bubble) bubble.style.display = 'none';
    }
  });

})();
