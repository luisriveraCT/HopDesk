// hd_numero.js — the browser side of hd_numero_input() (R/ui_components.R, T66).
// A text field for numbers: no spinner, the wheel and the arrow keys never
// change it. It sends Shiny a number (or null -> NA) only when the field is
// left or on Enter, and never the same number twice.
// Other scripts read one of these fields with hdNumero.valor(el), never with
// parseFloat on its text ("1,234" would be 1).
(function () {
  'use strict';
  if (window.hdNumero) return;

  var PATRON = /^[-+]?(\d{1,3}(,\d{3})+|\d+)?(\.\d+)?$/;
  var FMT = {};
  function fmt(d) {
    return FMT[d] || (FMT[d] = new Intl.NumberFormat('es-MX', { minimumFractionDigits: d, maximumFractionDigits: d, useGrouping: true }));
  }
  function formatoDe(el) { return el.getAttribute('data-formato') || 'tasa'; }
  function decimalesDe(el) {
    if (formatoDe(el) === 'entero') return 0;
    var d = parseInt(el.getAttribute('data-decimales'), 10);
    return isNaN(d) ? 4 : d;
  }

  // A decimal string rounded half away from zero to d decimals, exactly (no
  // binary floating point on the way: 1.005 -> "1.01").
  function redondearTexto(signo, ent, dec, d) {
    if (dec.length <= d) return signo + (ent || '0') + (dec ? '.' + dec : '');
    var digitos = (ent || '0') + dec.slice(0, d);
    var n = BigInt(digitos);
    if (dec.charCodeAt(d) >= 53) n = n + 1n;          // next digit >= '5'
    var s = n.toString();
    while (s.length <= d) s = '0' + s;
    return signo + (d ? s.slice(0, s.length - d) + '.' + s.slice(s.length - d) : s);
  }

  // Text -> number, or null when empty or not a number.
  function interpretar(texto, formato, d) {
    var s = String(texto == null ? '' : texto).replace(/[\s ]/g, '');
    if (!s || !/\d/.test(s) || !PATRON.test(s)) return null;
    s = s.replace(/,/g, '');
    var signo = '';
    if (s.charAt(0) === '-' || s.charAt(0) === '+') { signo = s.charAt(0) === '-' ? '-' : ''; s = s.slice(1); }
    var partes = s.split('.'), ent = partes[0], dec = partes[1] || '';
    if (dec.length > d) {
      if (formato !== 'monto') return null;           // a tasa or entero with too many decimals
      s = redondearTexto(signo, ent, dec, d);
    } else {
      s = signo + (ent || '0') + (dec ? '.' + dec : '');
    }
    var n = Number(s);
    if (!isFinite(n)) return null;
    return n === 0 ? 0 : n;                            // no -0
  }

  // Number -> the text the field shows.
  function texto(n, formato, d) {
    if (n === null || n === undefined || !isFinite(n)) return '';
    if (formato === 'monto') return fmt(d).format(n);
    var s = n.toFixed(Math.min(Math.max(d, 0), 20));
    if (s.indexOf('.') >= 0) s = s.replace(/0+$/, '').replace(/\.$/, '');
    return s === '-0' ? '0' : s;
  }

  function valor(el) {
    if (!el) return null;
    return interpretar(el.value, formatoDe(el), decimalesDe(el));
  }

  // When the field is left: a monto shows its separator and its decimals; a
  // tasa or entero keeps what was typed (an invalid text stays as typed, so
  // the person sees it; the server gets NA and its validation says so).
  function pulir(el) {
    var n = valor(el);
    if (n !== null && formatoDe(el) === 'monto') el.value = texto(n, 'monto', decimalesDe(el));
    else el.value = String(el.value || '').trim();
  }

  window.hdNumero = { valor: valor, interpretar: interpretar, texto: texto };

  if (!window.Shiny || !Shiny.InputBinding) return;
  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find('input.hd-numero'); },
    getId: function (el) { return el.id; },
    getType: function () { return 'shiny.number'; },   // null -> NA in R, as numericInput
    getValue: function (el) {
      var n = valor(el);
      el._hdUltimo = n;
      return n;
    },
    setValue: function (el, n) { el.value = texto(n, formatoDe(el), decimalesDe(el)); },
    subscribe: function (el, callback) {
      function enviar() {
        pulir(el);
        if (valor(el) === el._hdUltimo) return;       // the same number: nothing to send
        callback(false);
      }
      $(el).on('change.hdNumero', enviar);
      $(el).on('keydown.hdNumero', function (e) { if (e.key === 'Enter') enviar(); });
      $(el).on('hdnumero:enviar.hdNumero', enviar);
    },
    unsubscribe: function (el) { $(el).off('.hdNumero'); },
    getRatePolicy: function () { return null; },
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('label')) {
        var lab = document.getElementById(el.id + '-label');
        if (lab) { lab.textContent = data.label; lab.classList.toggle('shiny-label-null', !data.label); }
      }
      if (data.hasOwnProperty('min')) el.setAttribute('data-min', data.min);
      if (data.hasOwnProperty('max')) el.setAttribute('data-max', data.max);
      if (data.vaciar) el.value = '';
      else if (data.hasOwnProperty('value')) el.value = texto(Number(data.value), formatoDe(el), decimalesDe(el));
      if (data.vaciar || data.hasOwnProperty('value')) $(el).trigger('hdnumero:enviar');
    },
    getState: function (el) {
      return { label: $('#' + el.id + '-label').text(), value: valor(el), formato: formatoDe(el) };
    }
  });
  Shiny.inputBindings.register(binding, 'hopdesk.hdNumero');
  // When this file arrives with dynamic UI (a modal, a renderUI), the fields
  // may have been bound before the binding existed: bind them now (bindAll
  // skips what is already bound).
  setTimeout(function () {
    if (Shiny.shinyapp && Shiny.bindAll) Shiny.bindAll(document.body);
  }, 0);
})();
