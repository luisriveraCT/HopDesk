// hd_table engine, browser side (R/hd_table.R sends the data).
//
// One "hdt-datos" message per data change carries the rows already formatted,
// column by column: filas = {ids, bloqueada, moneda, tipo, celdas: [{mostrar,
// orden, copiar[, tono]}, ...]}. This file paints, sorts, searches, selects
// and sums here, and sends back only ids: ns + "sel" and ns + "accion".
//
// Core rules (can't be turned off): rows are identified by id; every text is
// painted with textContent; sorting uses the real value; empty and loading
// messages; new data keeps scroll, sort, search and selection (minus ids that
// no longer exist) and never re-creates the container.
(function () {
  'use strict';

  // Above this many visible rows (after search) only the rows in view are in
  // the DOM, with spacers above and below: Chrome's layout of a full 5,000-row
  // table alone takes ~1 s. At or below it every row is painted, so the
  // browser's Ctrl+F and natural column widths keep working.
  var HDT_UMBRAL_VENTANA = 800;
  var HDT_COLCHON = 20;   // rows painted beyond each edge of the view in window mode

  var TABLAS = {};        // root element id -> state
  var ULTIMO = {};        // root element id -> last data message received for it
  var observador = null;
  var recibidos = 0;      // data messages received (diagnostics / tests)

  // ── Helpers ──────────────────────────────────────────────────────────────
  function t(k, v) { return window.hdT ? window.hdT(k, v) : k; }
  function tn(k, n, v) { return window.hdTn ? window.hdTn(k, n, v) : k; }
  function nodo(tag, cls, texto) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (texto != null) e.textContent = texto;
    return e;
  }
  // Lowercase, accent-free: what search compares.
  function plano(s) {
    return String(s == null ? '' : s).normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
  }
  var FMT_N = new Intl.NumberFormat('es-MX');
  var FMT_D = new Intl.NumberFormat('es-MX', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  function dinero(x) {
    x = Math.round(x * 100) / 100;
    if (x === 0) x = 0;                                   // no "-$0.00"
    return (x < 0 ? '-' : '') + '$' + FMT_D.format(Math.abs(x));
  }
  function arr(x) { return x == null ? [] : (Array.isArray(x) ? x : [x]); }

  // ── Incoming data ────────────────────────────────────────────────────────
  function recibir(msg) {
    recibidos++;
    ULTIMO[msg.id] = msg;
    vigilar();
    pintarDesde(msg);
  }

  // Paints a message into its container, if the container is in the page.
  // A container that isn't the one the table's state was built on (the
  // module's UI was rendered again: a modal closed and reopened, a renderUI)
  // starts from scratch, with an empty selection the server is told about,
  // so seleccion() never keeps ids from the previous container.
  function pintarDesde(msg) {
    var root = document.getElementById(msg.id);
    if (!root) return;
    var st = TABLAS[msg.id], nuevo = !!st && st.root !== root;
    if (!st || nuevo) st = TABLAS[msg.id] = crear(root, msg);
    cargar(st, msg);
    if (nuevo) enviarSel(st);
  }

  // The server sends data only when it changes, so a container that appears
  // later (rendered after the message, or re-created with the same id while
  // the data stayed the same) is painted from the last message kept for it.
  function vigilar() {
    if (observador) return;
    observador = new MutationObserver(function () {
      Object.keys(ULTIMO).forEach(function (id) {
        var el = document.getElementById(id);
        if (el && (!TABLAS[id] || TABLAS[id].root !== el)) pintarDesde(ULTIMO[id]);
      });
    });
    observador.observe(document.body, { childList: true, subtree: true });
  }

  function crear(root, msg) {
    var p = {};
    arr(msg.piezas).forEach(function (x) { p[x] = true; });
    var st = {
      root: root, ns: root.getAttribute('data-ns') || '', tabla: msg.tabla, p: p,
      textos: msg.textos || {}, cols: [], n: 0, ids: [], idx: {}, bloq: [], moneda: [], tipo: [],
      celdas: [], busq: [], trs: [], vis: [], sel: new Set(), ancla: null,
      orden: { col: -1, dir: 1 }, q: '', abajo: null, medidas: {}
    };
    root.textContent = '';
    root.classList.toggle('hdt-fijo', !!p.encabezado_fijo);
    root.classList.toggle('hdt-selec', !!(p.sel_fila || p.casillas));
    root.classList.toggle('hdt-orden', !!p.ordenar);

    st.barra = nodo('div', 'hdt-barra');
    if (p.buscar) {
      st.buscar = nodo('input', 'hdt-buscar');
      st.buscar.type = 'search';
      st.buscar.placeholder = t('tablas.buscar');
      st.buscar.setAttribute('aria-label', t('tablas.buscar'));
      st.buscar.addEventListener('input', function () {
        st.q = plano(st.buscar.value.trim());
        aplicar(st);
      });
      st.barra.appendChild(st.buscar);
    }
    if (p.sel_todos) {
      st.btnTodos = nodo('button', 'hdt-btn');
      st.btnTodos.type = 'button';
      st.btnTodos.addEventListener('click', function () { selTodos(st); });
      st.barra.appendChild(st.btnTodos);
    }
    if (p.densidad) {
      st.btnDens = nodo('button', 'hdt-btn');
      st.btnDens.type = 'button';
      st.btnDens.addEventListener('click', function () {
        densidad(st, root.classList.contains('hdt-normal') ? 'compacta' : 'normal', true);
      });
      st.barra.appendChild(st.btnDens);
      var guardada = null;
      try { guardada = localStorage.getItem('hdt.densidad.' + st.tabla); } catch (e) {}
      densidad(st, guardada === 'normal' ? 'normal' : 'compacta', false);
    }
    if (p.contador) { st.contador = nodo('span', 'hdt-contador'); st.barra.appendChild(st.contador); }
    st.barra.hidden = !st.barra.childNodes.length;

    st.cuerpo = nodo('div', 'hdt-cuerpo');
    st.scroll = nodo('div', 'hdt-scroll');
    st.tabla_el = nodo('table', 'hdt-tabla');
    st.thead = nodo('thead');
    st.tbody = nodo('tbody');
    st.tabla_el.appendChild(st.thead);
    st.tabla_el.appendChild(st.tbody);
    st.vacio = nodo('div', 'hdt-vacio');
    st.vacio.hidden = true;
    st.scroll.appendChild(st.tabla_el);
    st.scroll.appendChild(st.vacio);
    st.pie = nodo('div', 'hdt-pie');
    st.pie.hidden = true;
    st.cuerpo.appendChild(st.scroll);
    st.cuerpo.appendChild(st.pie);
    root.appendChild(st.barra);
    root.appendChild(st.cuerpo);

    eventos(st);
    return st;
  }

  function densidad(st, modo, guardar) {
    st.root.classList.toggle('hdt-normal', modo === 'normal');
    st.btnDens.textContent = t(modo === 'normal' ? 'tablas.densidad.normal' : 'tablas.densidad.compacta');
    st.btnDens.classList.toggle('hdt-on', modo === 'normal');
    if (guardar) { try { localStorage.setItem('hdt.densidad.' + st.tabla, modo); } catch (e) {} }
    // Row height and column widths change with the density: measure again.
    st.altoFila = null; st.altoMedido = false; st.anchos = null;
    if (st.modo === 'ventana') { var top = st.scroll.scrollTop; salirVentana(st); st.modo = null; pintarFilas(st, top); }
  }

  function cargar(st, msg) {
    var t0 = performance.now();
    var f = msg.filas || {};
    var arriba = st.scroll.scrollTop, izq = st.scroll.scrollLeft;

    var colsCambian = JSON.stringify(msg.columnas) !== JSON.stringify(st.colsMeta);
    st.colsMeta = msg.columnas;
    st.cols = arr(msg.columnas);
    st.ids = arr(f.ids).map(String);
    st.n = st.ids.length;
    st.bloq = arr(f.bloqueada);
    st.moneda = arr(f.moneda);
    st.tipo = arr(f.tipo);
    st.celdas = arr(f.celdas).map(function (c) {
      return { mostrar: arr(c.mostrar), orden: arr(c.orden), copiar: arr(c.copiar), tono: c.tono ? arr(c.tono) : null };
    });
    st.idx = {};
    for (var i = 0; i < st.n; i++) st.idx[st.ids[i]] = i;
    st.suma = -1;
    for (var j = 0; j < st.cols.length; j++) {
      if (st.cols[j].tipo === 'dinero' && st.cols[j].sumar) { st.suma = j; break; }
    }
    if (colsCambian) {
      cabecera(st);
      st.anchos = null;
      if (st.orden.col >= st.cols.length) st.orden.col = -1;
    }

    // Selection survives a refresh, minus ids that no longer exist.
    var antes = st.sel.size;
    st.sel.forEach(function (id) { if (!(id in st.idx) || st.bloq[st.idx[id]]) st.sel.delete(id); });
    if (st.ancla != null && !(st.ancla in st.idx)) st.ancla = null;

    if (st.p.buscar) {
      st.busq = new Array(st.n);
      for (i = 0; i < st.n; i++) {
        var partes = [];
        // Shown and raw values: "1234.5" and "2026-10-29" find rows too.
        for (j = 0; j < st.celdas.length; j++) partes.push(st.celdas[j].mostrar[i], st.celdas[j].copiar[i]);
        st.busq[i] = plano(partes.join(' '));
      }
    }
    // Rows are built when first painted (trDe), not all up front.
    st.trs = new Array(st.n);

    aplicar(st, true, arriba);
    st.scroll.scrollLeft = izq;
    st.medidas.pintado = performance.now() - t0;
    if (st.sel.size !== antes) enviarSel(st);
  }

  function cabecera(st) {
    st.thead.textContent = '';
    var tr = nodo('tr');
    if (st.p.casillas) {
      var th0 = nodo('th', 'hdt-cb');
      if (st.p.sel_todos) {
        st.cbTodos = nodo('input', 'hdt-cbx-todos');
        st.cbTodos.type = 'checkbox';
        st.cbTodos.setAttribute('aria-label', t('tablas.seleccionar_todos'));
        th0.appendChild(st.cbTodos);
      }
      tr.appendChild(th0);
    }
    st.ths = st.cols.map(function (c, j) {
      var th = nodo('th', c.alinear === 'derecha' ? 'hdt-der' : '');
      th.appendChild(nodo('span', '', c.titulo));
      if (st.p.ordenar) { th.classList.add('hdt-ordenable'); th.dataset.col = j; th.appendChild(nodo('span', 'hdt-flecha')); }
      tr.appendChild(th);
      return th;
    });
    st.thead.appendChild(tr);
  }

  function trDe(st, i) { return st.trs[i] || (st.trs[i] = fila(st, i)); }

  function fila(st, i) {
    var tr = nodo('tr');
    tr._i = i;
    if (st.bloq[i]) tr.className = 'hdt-bloq';
    var marcada = st.sel.has(st.ids[i]);
    if (marcada) tr.classList.add('hdt-sel');
    if (st.p.casillas) {
      var td0 = nodo('td', 'hdt-cb');
      var cb = nodo('input', 'hdt-cbx');
      cb.type = 'checkbox';
      cb.checked = marcada;
      cb.disabled = !!st.bloq[i];
      td0.appendChild(cb);
      tr.appendChild(td0);
    }
    for (var j = 0; j < st.cols.length; j++) {
      var c = st.cols[j], v = st.celdas[j].mostrar[i];
      var td = nodo('td');
      td._c = j;
      if (c.alinear === 'derecha') td.classList.add('hdt-der');
      if (c.recorte && st.p.recorte) td.classList.add('hdt-recorte');
      if (c.tipo === 'empresa' && v) td.appendChild(nodo('span', 'hdt-emp', v));
      else if (c.tipo === 'insignia' && v) {
        var tono = String((st.celdas[j].tono || [])[i] || 'neutro').replace(/[^a-z_]/g, '');
        td.appendChild(nodo('span', 'hdt-ins hdt-tono-' + tono, v));
      } else td.textContent = v == null ? '' : v;
      tr.appendChild(td);
    }
    return tr;
  }

  // ── Filter, sort, paint ──────────────────────────────────────────────────
  // `arriba`: scroll position to land on (new data keeps where you were);
  // omitted, the current one.
  function aplicar(st, desdeDatos, arriba) {
    var vis = [];
    for (var i = 0; i < st.n; i++) {
      if (!st.q || st.busq[i].indexOf(st.q) >= 0) vis.push(i);
    }
    if (st.orden.col >= 0) ordenarVis(st, vis);
    st.vis = vis;

    // Without sel_persistente, rows the search hides drop out of the selection.
    if (!desdeDatos && !st.p.sel_persistente && st.q) {
      var visibles = new Set(vis.map(function (k) { return st.ids[k]; }));
      var cambio = false;
      st.sel.forEach(function (id) { if (!visibles.has(id)) { st.sel.delete(id); cambio = true; } });
      if (cambio) enviarSel(st);
    }

    pintarFilas(st, arriba == null ? st.scroll.scrollTop : arriba);

    st.vacio.hidden = vis.length > 0;
    if (!vis.length) st.vacio.textContent = t(st.n && st.q ? 'tablas.vacio_filtro' : 'tablas.vacio');
    pintarSel(st);
    flechas(st);
    if (st.contador) st.contador.textContent = tn(st.textos.contador || 'tablas.contador.registros', vis.length, { n: FMT_N.format(vis.length) });
  }

  // The mode is decided on every paint from the visible count; selection,
  // sort, search and totals never know which one is on.
  // TODO(T03): a table with `multilinea` never enters window mode (its rows
  // don't have one fixed height).
  function pintarFilas(st, arriba) {
    var ventana = st.vis.length > HDT_UMBRAL_VENTANA && !st.p.multilinea;
    if (ventana) {
      if (st.modo !== 'ventana') entrarVentana(st);
      st.modo = 'ventana';
      pintarVentana(st, true, arriba);
      st.scroll.scrollTop = arriba;
      // The first paint at this density measures the real row height; if
      // it differs from the estimate, repaint once with the right spacers.
      if (medirFila(st)) { pintarVentana(st, true, arriba); st.scroll.scrollTop = arriba; }
      return;
    }
    if (st.modo === 'ventana') salirVentana(st);
    st.modo = 'completo';
    var frag = document.createDocumentFragment();
    for (var k = 0; k < st.vis.length; k++) frag.appendChild(trDe(st, st.vis[k]));
    st.tbody.textContent = '';
    st.tbody.appendChild(frag);
    // No scrollTop assignment here: the browser keeps the position when the
    // rows are replaced (clamped if the table got shorter), and assigning it
    // would force a synchronous layout of every painted row.
  }

  // Column widths are measured once, on a sample of rows painted with the
  // natural layout, and then frozen so they don't jump while scrolling.
  function entrarVentana(st) {
    if (!st.anchos) {
      var paso = Math.max(1, Math.floor(st.vis.length / 150)), frag = document.createDocumentFragment();
      for (var k = 0; k < st.vis.length; k += paso) frag.appendChild(trDe(st, st.vis[k]).cloneNode(true));
      st.tbody.textContent = '';
      st.tbody.appendChild(frag);
      st.anchos = Array.prototype.map.call(st.thead.querySelectorAll('th'),
                                           function (th) { return th.getBoundingClientRect().width; });
    }
    if (!st.colgroup) { st.colgroup = nodo('colgroup'); st.tabla_el.insertBefore(st.colgroup, st.thead); }
    st.colgroup.textContent = '';
    st.anchos.forEach(function (w) { var c = nodo('col'); c.style.width = w + 'px'; st.colgroup.appendChild(c); });
    st.tabla_el.classList.add('hdt-fijas');
    st.root.classList.add('hdt-ventana');
  }

  function salirVentana(st) {
    if (st.colgroup) st.colgroup.textContent = '';
    st.tabla_el.classList.remove('hdt-fijas');
    st.root.classList.remove('hdt-ventana');
    st.vIni = st.vFin = null;
  }

  function espaciador(st, alto) {
    var tr = nodo('tr', 'hdt-esp'), td = nodo('td');
    td.colSpan = st.cols.length + (st.p.casillas ? 1 : 0);
    td.style.height = alto + 'px';
    tr.appendChild(td);
    return tr;
  }

  // Paints only the rows around `arriba` (or the current scroll position).
  function pintarVentana(st, forzar, arriba) {
    var h = st.altoFila || 23, n = st.vis.length;
    var top = arriba == null ? st.scroll.scrollTop : arriba;
    var vista = st.scroll.clientHeight || 420;
    var ini = Math.max(0, Math.floor(top / h) - HDT_COLCHON);
    var fin = Math.min(n, Math.ceil((top + vista) / h) + HDT_COLCHON);
    if (!forzar && ini === st.vIni && fin === st.vFin) return;
    st.vIni = ini; st.vFin = fin;
    var frag = document.createDocumentFragment();
    frag.appendChild(espaciador(st, ini * h));
    for (var k = ini; k < fin; k++) frag.appendChild(trDe(st, st.vis[k]));
    frag.appendChild(espaciador(st, (n - fin) * h));
    st.tbody.textContent = '';
    st.tbody.appendChild(frag);
  }

  // Returns true when the measured row height changed the one in use.
  function medirFila(st) {
    if (st.altoMedido) return false;
    var filas = st.tbody.querySelectorAll('tr:not(.hdt-esp)');
    if (filas.length < 2) return false;
    var a = filas[0].getBoundingClientRect(), z = filas[filas.length - 1].getBoundingClientRect();
    var h = (z.bottom - a.top) / filas.length;
    // Not laid out yet (just inserted, or inside something hidden): keep the
    // estimate and try again on the next paint.
    if (!(h >= 8 && h <= 200)) return false;
    st.altoMedido = true;
    if (Math.abs(h - (st.altoFila || 23)) < 0.01) return false;
    st.altoFila = h;
    return true;
  }

  function ordenarVis(st, vis) {
    var o = st.celdas[st.orden.col].orden, dir = st.orden.dir;
    vis.sort(function (a, b) {
      var x = o[a], y = o[b];
      var xn = x == null || x === '', yn = y == null || y === '';
      if (xn || yn) return xn && yn ? a - b : (xn ? 1 : -1);   // empty last, both directions
      if (x < y) return -dir;
      if (x > y) return dir;
      return a - b;                                             // stable
    });
  }

  function flechas(st) {
    if (!st.p.ordenar || !st.ths) return;
    st.ths.forEach(function (th, j) {
      var f = th.querySelector('.hdt-flecha');
      if (f) f.textContent = j === st.orden.col ? (st.orden.dir > 0 ? '▲' : '▼') : '';
    });
  }

  // ── Selection ────────────────────────────────────────────────────────────
  function seleccionables(st) {
    return st.vis.filter(function (i) { return !st.bloq[i]; });
  }

  function pintarSel(st) {
    // Only rows already built; the rest are born with their state (fila()).
    for (var k = 0; k < st.vis.length; k++) {
      var i = st.vis[k], tr = st.trs[i];
      if (!tr) continue;
      var on = st.sel.has(st.ids[i]);
      tr.classList.toggle('hdt-sel', on);
      if (st.p.casillas) tr.firstChild.firstChild.checked = on;
    }
    var sel = seleccionables(st), todos = sel.length > 0 && sel.every(function (i) { return st.sel.has(st.ids[i]); });
    if (st.btnTodos) {
      st.btnTodos.textContent = t(todos ? 'tablas.deseleccionar' : 'tablas.seleccionar_todos');
      st.btnTodos.disabled = !sel.length;
    }
    if (st.cbTodos) {
      st.cbTodos.checked = todos;
      st.cbTodos.indeterminate = !todos && sel.some(function (i) { return st.sel.has(st.ids[i]); });
    }
    sumador(st);
  }

  // Ids in the order they're shown; selected rows hidden by the search last.
  function idsSel(st) {
    var out = [], vistos = new Set();
    st.vis.forEach(function (i) { if (st.sel.has(st.ids[i])) { out.push(st.ids[i]); vistos.add(st.ids[i]); } });
    st.ids.forEach(function (id) { if (st.sel.has(id) && !vistos.has(id)) out.push(id); });
    return out;
  }

  function enviarSel(st) {
    if (window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue(st.ns + 'sel', idsSel(st), { priority: 'event' });
  }

  function alternar(st, i) {
    var id = st.ids[i];
    if (st.bloq[i]) return;
    if (st.sel.has(id)) st.sel.delete(id); else st.sel.add(id);
    st.ancla = id;
  }

  // Shift + click: from the last row touched to this one, in visible order,
  // skipping locked rows.
  function rango(st, i) {
    if (!st.p.sel_rango || st.ancla == null || !(st.ancla in st.idx)) return false;
    var a = st.vis.indexOf(st.idx[st.ancla]), b = st.vis.indexOf(i);
    if (a < 0 || b < 0) return false;
    for (var k = Math.min(a, b); k <= Math.max(a, b); k++) {
      if (!st.bloq[st.vis[k]]) st.sel.add(st.ids[st.vis[k]]);
    }
    var s = window.getSelection && window.getSelection();
    if (s) s.removeAllRanges();
    return true;
  }

  function selTodos(st) {
    var sel = seleccionables(st);
    var todos = sel.length > 0 && sel.every(function (i) { return st.sel.has(st.ids[i]); });
    sel.forEach(function (i) { if (todos) st.sel.delete(st.ids[i]); else st.sel.add(st.ids[i]); });
    pintarSel(st);
    enviarSel(st);
  }

  function limpiarSel(st) {
    st.sel.clear();
    st.ancla = null;
    pintarSel(st);
    enviarSel(st);
  }

  // ── Totals footer ────────────────────────────────────────────────────────
  // With a selection: what's marked, per currency, split into Cobros / Pagos.
  // Without: what's visible (locked rows don't count).
  function sumador(st) {
    if (!st.pie) return;
    var conSel = st.p.sumador_sel && st.sel.size > 0;
    var conVis = !conSel && st.p.sumador_vis && st.vis.length > 0;
    if (st.suma < 0 || (!conSel && !conVis)) { st.pie.hidden = true; return; }
    var filas = conSel ? st.ids.filter(function (id) { return st.sel.has(id); }).map(function (id) { return st.idx[id]; })
                       : st.vis.filter(function (i) { return !st.bloq[i]; });
    var o = st.celdas[st.suma].orden, grupos = {}, orden = [];
    filas.forEach(function (i) {
      var tipo = st.tipo[i] === 'cobro' || st.tipo[i] === 'pago' ? st.tipo[i] : '';
      var mon = st.moneda[i] || '';
      if (!grupos[tipo]) { grupos[tipo] = {}; orden.push(tipo); }
      grupos[tipo][mon] = (grupos[tipo][mon] || 0) + (Number(o[i]) || 0);
    });
    orden.sort(function (a, b) { return ['cobro', 'pago', ''].indexOf(a) - ['cobro', 'pago', ''].indexOf(b); });

    st.pie.textContent = '';
    st.pie.appendChild(nodo('span', 'hdt-pie-titulo',
      conSel ? tn('tablas.sumador.seleccion', filas.length, { n: FMT_N.format(filas.length) })
             : t('tablas.sumador.visible')));
    orden.forEach(function (tipo) {
      var g = nodo('span', 'hdt-pie-grupo');
      if (tipo) g.appendChild(nodo('span', 'hdt-pie-tipo hdt-' + tipo, t(tipo === 'cobro' ? 'tablas.cobros' : 'tablas.pagos')));
      Object.keys(grupos[tipo]).sort().forEach(function (mon) {
        g.appendChild(nodo('span', 'hdt-pie-monto', (mon ? mon + ' ' : '') + dinero(grupos[tipo][mon])));
      });
      st.pie.appendChild(g);
    });
    if (conSel) {
      var b = nodo('button', 'hdt-btn hdt-deseleccionar', t('tablas.deseleccionar'));
      b.type = 'button';
      b.addEventListener('click', function () { limpiarSel(st); });
      st.pie.appendChild(b);
    }
    st.pie.hidden = false;
  }

  // ── Events ───────────────────────────────────────────────────────────────
  function eventos(st) {
    var pendiente = false;
    st.scroll.addEventListener('scroll', function () {
      if (st.modo !== 'ventana' || pendiente) return;
      pendiente = true;
      requestAnimationFrame(function () { pendiente = false; if (st.modo === 'ventana') pintarVentana(st, false); });
    }, { passive: true });

    st.tbody.addEventListener('mousedown', function (e) {
      st.abajo = { x: e.clientX, y: e.clientY };
      // Shift + click must not shade the text between the two rows.
      if (e.shiftKey && st.p.sel_rango && e.target.closest('tr')) e.preventDefault();
    });

    st.tbody.addEventListener('click', function (e) {
      var tr = e.target.closest('tr');
      if (!tr || tr._i == null) return;
      var i = tr._i;
      var cb = e.target.closest('input.hdt-cbx');
      if (st.p.casillas) {
        if (!cb) return;                         // with checkboxes, the row click is free for other uses
      } else {
        if (!st.p.sel_fila) return;
        // A drag (to select text) never touches the row selection.
        var lejos = st.abajo && (Math.abs(e.clientX - st.abajo.x) > 4 || Math.abs(e.clientY - st.abajo.y) > 4);
        var texto = window.getSelection && String(window.getSelection()).length > 0;
        if (lejos || (texto && !e.shiftKey)) return;
      }
      if (st.bloq[i]) { if (cb) cb.checked = false; return; }
      if (!(e.shiftKey && rango(st, i))) alternar(st, i);
      pintarSel(st);
      enviarSel(st);
    });

    if (st.p.casillas) {
      st.thead.addEventListener('change', function (e) {
        if (e.target.classList.contains('hdt-cbx-todos')) selTodos(st);
      });
    }

    if (st.p.ordenar) {
      st.thead.addEventListener('click', function (e) {
        var th = e.target.closest('th.hdt-ordenable');
        if (!th) return;
        var t0 = performance.now();
        var j = Number(th.dataset.col);
        st.orden = { col: j, dir: st.orden.col === j ? -st.orden.dir : 1 };
        aplicar(st);
        st.medidas.orden = performance.now() - t0;
      });
    }

    if (st.p.copiar_celda) burbuja(st);
  }

  // Shift held: the whole table stops shading text (CSS .hdt-mayus).
  function mayus(on) {
    Object.keys(TABLAS).forEach(function (id) { TABLAS[id].root.classList.toggle('hdt-mayus', on); });
  }
  window.addEventListener('keydown', function (e) { if (e.key === 'Shift') mayus(true); });
  window.addEventListener('keyup',   function (e) { if (e.key === 'Shift') mayus(false); });
  window.addEventListener('blur',    function () { mayus(false); });

  // ── Full-value bubble (copiar_celda) ─────────────────────────────────────
  // One bubble for the page. 350 ms over a cell shows the full value and a
  // copy button; it hides 200 ms after leaving and on any scroll; the mouse
  // can enter it to select text. Copies the raw value.
  var TIP = null, tipVer = null, tipOcultar = null;
  function tip() {
    if (TIP) return TIP;
    TIP = nodo('div', 'hdt-tip');
    TIP.hidden = true;
    TIP.setAttribute('role', 'tooltip');
    TIP.texto = nodo('span', 'hdt-tip-texto');
    TIP.aviso = nodo('span', 'hdt-tip-aviso');
    TIP.boton = nodo('button', 'hdt-tip-copiar');
    TIP.boton.type = 'button';
    TIP.boton.title = t('tablas.copiar_valor');
    TIP.boton.setAttribute('aria-label', t('tablas.copiar_valor'));
    TIP.boton.innerHTML = '<svg viewBox="0 0 16 16" aria-hidden="true"><rect x="5" y="5" width="8.5" height="8.5" rx="1.5" fill="none" stroke="currentColor" stroke-width="1.4"/><path d="M3 10.5V3.5A1 1 0 0 1 4 2.5h6.5" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg>';
    TIP.icono = TIP.boton.innerHTML;
    TIP.appendChild(TIP.texto);
    TIP.appendChild(TIP.aviso);
    TIP.appendChild(TIP.boton);
    document.body.appendChild(TIP);
    TIP.addEventListener('mouseover', function () { clearTimeout(tipOcultar); });
    TIP.addEventListener('mouseout', function (e) { if (!TIP.contains(e.relatedTarget)) tipLuego(); });
    TIP.boton.addEventListener('click', copiar);
    window.addEventListener('scroll', function () { TIP.hidden = true; }, true);
    return TIP;
  }
  function tipLuego() {
    clearTimeout(tipVer);
    clearTimeout(tipOcultar);
    tipOcultar = setTimeout(function () { if (TIP) TIP.hidden = true; }, 200);
  }
  function tipAbrir(st, td) {
    var i = td.parentNode._i, j = td._c;
    var texto = st.celdas[j].mostrar[i];
    if (texto == null || texto === '') return;
    var b = tip();
    b.texto.textContent = texto;
    b.aviso.textContent = '';
    b.copia = st.celdas[j].copiar[i] == null ? String(texto) : String(st.celdas[j].copiar[i]);
    b.boton.classList.remove('hdt-hecho');
    b.boton.innerHTML = b.icono;
    b.hidden = false;
    var r = td.getBoundingClientRect(), tr = b.getBoundingClientRect();
    var left = r.left, top = r.bottom + 4;
    if (left + tr.width > window.innerWidth - 8) left = Math.max(8, window.innerWidth - tr.width - 8);
    if (top + tr.height > window.innerHeight - 8) top = r.top - tr.height - 4;
    b.style.left = left + 'px';
    b.style.top = top + 'px';
  }
  function burbuja(st) {
    st.tbody.addEventListener('mouseover', function (e) {
      var td = e.target.closest('td');
      if (!td || td._c == null) return;
      clearTimeout(tipOcultar);
      clearTimeout(tipVer);
      tipVer = setTimeout(function () { tipAbrir(st, td); }, 350);
    });
    st.tbody.addEventListener('mouseout', function (e) {
      var td = e.target.closest('td');
      if (!td) return;
      if (e.relatedTarget && (td.contains(e.relatedTarget) || (TIP && TIP.contains(e.relatedTarget)))) return;
      tipLuego();
    });
  }
  // Clipboard API, then a hidden textarea + execCommand; if both fail, select
  // the shown text so the user can copy it by hand.
  function copiar() {
    var b = TIP, valor = b.copia;
    function hecho() {
      b.boton.classList.add('hdt-hecho');
      b.boton.textContent = '✓';
      b.aviso.textContent = t('comun.copiado');
      setTimeout(function () { b.boton.classList.remove('hdt-hecho'); b.boton.innerHTML = b.icono; }, 900);
    }
    function aMano() {
      var ta = nodo('textarea');
      ta.value = valor;
      ta.setAttribute('readonly', '');
      ta.style.position = 'fixed'; ta.style.opacity = '0'; ta.style.top = '0';
      document.body.appendChild(ta);
      ta.select();
      var ok = false;
      try { ok = document.execCommand('copy'); } catch (e) {}
      document.body.removeChild(ta);
      if (ok) { hecho(); return; }
      var r = document.createRange();
      r.selectNodeContents(b.texto);
      var s = window.getSelection();
      s.removeAllRanges();
      s.addRange(r);
      b.aviso.textContent = t('comun.copiar');
    }
    try {
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(valor).then(hecho, aMano);
      else aMano();
    } catch (e) { aMano(); }
  }

  // ── Test / diagnostics hook ──────────────────────────────────────────────
  window.HDT = {
    recibidos: function () { return recibidos; },
    estado: function (id) {
      var st = TABLAS[id];
      if (!st) return null;
      return {
        visibles: st.vis.map(function (i) { return st.ids[i]; }),
        seleccion: idsSel(st), orden: st.orden, busqueda: st.q,
        scrollTop: st.scroll.scrollTop, medidas: st.medidas,
        modo: st.modo, pintadas: st.tbody.querySelectorAll('tr:not(.hdt-esp)').length,
        altoFila: st.altoFila, ventana: [st.vIni, st.vFin],
        bloqueadas: st.ids.filter(function (id, i) { return !!st.bloq[i]; })
      };
    }
  };

  (function registrar() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('hdt-datos', recibir);
    else setTimeout(registrar, 20);
  })();
})();
