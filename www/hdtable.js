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

  // alto = "auto" (R/hd_table.R): never fewer than 8 rows tall; up to 15
  // rows while 30 or fewer are visible; above that, up to 70% of the window
  // (or what fits in the modal around it).
  var HDT_ALTO_MIN = 8, HDT_ALTO_NORMAL = 15, HDT_ALTO_CRECE = 30, HDT_ALTO_VENTANA = 0.7;

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
      filtros: [], fsel: {}, trasBusq: [],
      orden: { col: -1, dir: 1 }, q: '', abajo: null, medidas: {}
    };
    root.textContent = '';
    root.classList.toggle('hdt-fijo', !!p.encabezado_fijo);
    root.classList.toggle('hdt-selec', !!(p.sel_fila || p.casillas));
    root.classList.toggle('hdt-orden', !!p.ordenar);
    root.classList.toggle('hdt-tachado', !!p.tachado);

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
    // Filters: one button per field, each with its checkbox menu, plus
    // "Limpiar filtros" while any is on. The fields are fixed per table;
    // their values come with each data message (cargarFiltros).
    if (p.filtros && arr(msg.filtros).length) {
      st.filtrosEl = nodo('span', 'hdt-filtros');
      arr(msg.filtros).forEach(function (f, k) {
        var b = nodo('button', 'hdt-btn hdt-filtro');
        b.type = 'button';
        b.dataset.k = k;
        b.setAttribute('aria-haspopup', 'true');
        b.setAttribute('aria-expanded', 'false');
        st.filtrosEl.appendChild(b);
      });
      st.filtrosEl.addEventListener('click', function (e) {
        var b = e.target.closest('button.hdt-filtro');
        if (!b) return;
        var k = Number(b.dataset.k);
        if (MENU && MENU.st === st && MENU.k === k) cerrarMenu(false);
        else abrirMenu(st, k, b, true);
      });
      st.barra.appendChild(st.filtrosEl);
      st.btnLimpiar = nodo('button', 'hdt-btn hdt-limpiar', t('tablas.filtros.limpiar'));
      st.btnLimpiar.type = 'button';
      st.btnLimpiar.hidden = true;
      st.btnLimpiar.addEventListener('click', function () { st.fsel = {}; cerrarMenu(false); aplicar(st); });
      st.barra.appendChild(st.btnLimpiar);
    }
    if (p.sel_todos) {
      st.btnTodos = nodo('button', 'hdt-btn hdt-todos');
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
    if (p.excel) {
      // The visible button asks the server to store the view first; the
      // hidden Shiny download link fires only when the server confirms.
      st.btnExcel = nodo('button', 'hdt-btn hdt-excel', t('tablas.exportar'));
      st.btnExcel.type = 'button';
      st.btnExcel.addEventListener('click', function () { exportar(st); });
      st.barra.appendChild(st.btnExcel);
      st.lnkExcel = nodo('a', 'shiny-download-link hdt-excel-link');
      st.lnkExcel.id = st.ns + 'excel';
      st.lnkExcel.setAttribute('download', '');
      st.lnkExcel.setAttribute('aria-hidden', 'true');
      st.lnkExcel.style.display = 'none';
      st.barra.appendChild(st.lnkExcel);
    }
    if (p.contador) { st.contador = nodo('span', 'hdt-contador'); st.barra.appendChild(st.contador); }
    st.barra.hidden = !st.barra.childNodes.length;

    st.cuerpo = nodo('div', 'hdt-cuerpo');
    st.aviso = nodo('div', 'hdt-aviso');
    st.aviso.hidden = true;
    st.aviso.setAttribute('role', 'status');
    st.cuerpo.appendChild(st.aviso);
    st.scroll = nodo('div', 'hdt-scroll');
    // Focusable, so Ctrl+C (copiar_tsv) knows which table it is for.
    if (p.copiar_tsv) st.scroll.tabIndex = 0;
    // Auto height starts at the minimum and is measured after the first paint.
    if (root.classList.contains('hdt-auto')) st.scroll.style.height = (HDT_ALTO_MIN * 23 + 28) + 'px';
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
    if (p.fichas && msg.fichas) {
      st.fichasEl = nodo('div', 'hdt-fichas');
      st.fichasEl.setAttribute('role', 'group');
      st.fichasEl.addEventListener('click', function (e) {
        var b = e.target.closest('button.hdt-ficha');
        if (!b) return;
        var v = b.dataset.todas ? null : b.dataset.valor;
        st.ficha = (v == null || v === st.ficha) ? null : v;   // touching the active chip again clears it
        aplicar(st);
      });
      root.appendChild(st.fichasEl);
    }
    // Bar actions: shown while rows are marked (or always, for an action
    // that works on what's visible). Then the confirmation bar, inside the
    // table: never a confirm() or a modal on top of a modal.
    st.accBarra = p.barra_acc ? arr(msg.acciones_barra) : [];
    if (st.accBarra.length) {
      st.accBarraEl = nodo('div', 'hdt-acciones');
      st.accBarraEl.hidden = true;
      st.accBarraEl.addEventListener('click', function (e) {
        var b = e.target.closest('button[data-accion]');
        if (!b) return;
        var a = st.accBarra.filter(function (x) { return x.accion === b.dataset.accion; })[0];
        if (!a) return;
        var ids = a.sin_seleccion
          ? st.vis.filter(function (i) { return !st.bloq[i]; }).map(function (i) { return st.ids[i]; })
          : idsSel(st);
        if (!ids.length) return;
        if (a.confirmar) pedirConfirmacion(st, a.accion, a.texto, ids, a.peligro);
        else enviarAccion(st, a.accion, ids);
      });
      root.appendChild(st.accBarraEl);
    }
    st.confEl = nodo('div', 'hdt-confirmar');
    st.confEl.hidden = true;
    st.confEl.setAttribute('role', 'alertdialog');
    root.appendChild(st.confEl);
    root.appendChild(st.cuerpo);

    eventos(st);
    if (st.lnkExcel && window.Shiny && Shiny.bindAll) Shiny.bindAll(st.barra);
    return st;
  }

  // ── Automatic height (alto = "auto") ─────────────────────────────────────
  // Measured after the paint (next frame), so it never forces a synchronous
  // layout of the rows. It grows right away; it only shrinks on new data, a
  // density change or a window resize -- never while typing a search.
  function programarAlto(st, encoger) {
    if (!st.root.classList.contains('hdt-auto')) return;
    st.altoEncoger = st.altoEncoger || !!encoger;
    if (st.altoPendiente) return;
    st.altoPendiente = true;
    requestAnimationFrame(function () {
      st.altoPendiente = false;
      var enc = st.altoEncoger;
      st.altoEncoger = false;
      ajustarAlto(st, enc, 0);
    });
  }

  function ajustarAlto(st, encoger, intento) {
    var cabeza = st.thead.offsetHeight;
    if (!cabeza) {                                 // not laid out yet (hidden, just inserted)
      if (intento < 10) requestAnimationFrame(function () { ajustarAlto(st, encoger, intento + 1); });
      return;
    }
    var filas = st.tbody.querySelectorAll('tr:not(.hdt-esp)');
    var h = st.modo === 'ventana' && st.altoFila ? st.altoFila
          : filas.length > 1 ? (filas[filas.length - 1].getBoundingClientRect().bottom - filas[0].getBoundingClientRect().top) / filas.length
          : (st.hFila || (st.root.classList.contains('hdt-normal') ? 34 : 23));
    st.hFila = h;
    var n = st.vis.length;
    var contenido = st.modo === 'ventana' ? cabeza + n * h
                  : st.tabla_el.offsetHeight + (st.vacio.hidden ? 0 : st.vacio.offsetHeight);
    var minimo = cabeza + HDT_ALTO_MIN * h;
    var tope;
    if (n <= HDT_ALTO_CRECE) tope = cabeza + HDT_ALTO_NORMAL * h;
    else {
      tope = window.innerHeight * HDT_ALTO_VENTANA;
      // Inside a modal: what fits without the page itself scrolling.
      var dlg = st.root.closest('.modal-dialog');
      if (dlg) {
        var resto = dlg.getBoundingClientRect().height - st.scroll.getBoundingClientRect().height;
        tope = Math.min(tope, window.innerHeight - resto - 2 * 28);
      }
    }
    var alto = Math.max(minimo, Math.min(contenido, Math.max(tope, minimo)));
    if (!encoger && st.altoActual) alto = Math.max(alto, st.altoActual);
    alto = Math.round(alto);
    if (alto !== st.altoActual) {
      st.altoActual = alto;
      st.scroll.style.height = alto + 'px';
      if (st.modo === 'ventana') pintarVentana(st, false);   // a taller view needs more rows
    }
  }

  // ── Bar actions and confirmation ─────────────────────────────────────────
  function pintarBarraAcc(st) {
    if (!st.accBarraEl) return;
    var n = idsSel(st).length;
    var siempre = st.accBarra.some(function (a) { return a.sin_seleccion; });
    st.accBarraEl.hidden = !n && !siempre;
    if (st.accBarraEl.hidden) return;
    st.accBarraEl.textContent = '';
    if (n) st.accBarraEl.appendChild(nodo('span', 'hdt-acciones-n',
      tn('tablas.sumador.seleccion', n, { n: FMT_N.format(n) })));
    st.accBarra.forEach(function (a) {
      if (!a.sin_seleccion && !n) return;
      var b = nodo('button', 'hdt-btn' + (a.peligro ? ' hdt-peligro' : ''), a.texto);
      b.type = 'button';
      b.dataset.accion = a.accion;
      st.accBarraEl.appendChild(b);
    });
  }

  // accion() fires only on Confirmar. The bar closes if the selection or
  // the visible rows change meanwhile.
  function pedirConfirmacion(st, accion, texto, ids, peligro) {
    st.pendiente = { accion: accion, ids: ids };
    var c = st.confEl;
    c.textContent = '';
    c.appendChild(nodo('span', 'hdt-confirmar-texto',
      tn('tablas.confirmar_accion', ids.length, { accion: texto, n: FMT_N.format(ids.length) })));
    var si = nodo('button', 'hdt-btn hdt-confirmar-si' + (peligro ? ' hdt-peligro' : ''), t('comun.confirmar'));
    var no = nodo('button', 'hdt-btn hdt-confirmar-no', t('comun.cancelar'));
    si.type = no.type = 'button';
    si.addEventListener('click', function () {
      var p = st.pendiente;
      cerrarConfirmacion(st);
      if (p) enviarAccion(st, p.accion, p.ids);
    });
    no.addEventListener('click', function () { cerrarConfirmacion(st); });
    c.appendChild(si);
    c.appendChild(no);
    c.hidden = false;
  }

  function cerrarConfirmacion(st) {
    st.pendiente = null;
    if (st.confEl) { st.confEl.hidden = true; st.confEl.textContent = ''; }
  }

  // ── Chips (fichas) ───────────────────────────────────────────────────────
  // Counts (or amounts) are over what the search leaves, so they tell what
  // each chip would show; the active chip survives new data.
  function pintarFichas(st, base) {
    if (!st.fichasEl || !st.fichas) return;
    var val = st.fichas.valores, monto = st.fichas.mostrar === 'monto' && st.suma >= 0;
    var o = monto ? st.celdas[st.suma].orden : null;
    var cuenta = {}, sumas = {};
    base.forEach(function (i) {
      var v = val[i];
      cuenta[v] = (cuenta[v] || 0) + 1;
      if (monto && !st.bloq[i]) {
        var m = st.moneda[i] || '';
        sumas[v] = sumas[v] || {};
        sumas[v][m] = (sumas[v][m] || 0) + (Number(o[i]) || 0);
        sumas.__todas = sumas.__todas || {};
        sumas.__todas[m] = (sumas.__todas[m] || 0) + (Number(o[i]) || 0);
      }
    });
    function cifra(v) {
      if (!monto) return FMT_N.format(v == null ? base.length : (cuenta[v] || 0));
      var s = (v == null ? sumas.__todas : sumas[v]) || {};
      var mons = Object.keys(s).sort();
      if (!mons.length) return dinero(0);
      return mons.map(function (m) { return (mons.length > 1 && m ? m + ' ' : '') + dinero(s[m]); }).join(' · ');
    }
    function chip(texto, valor) {
      var activa = valor == null ? st.ficha == null : st.ficha === valor;
      var b = nodo('button', 'hdt-ficha' + (activa ? ' hdt-on' : ''));
      b.type = 'button';
      if (valor == null) b.dataset.todas = '1'; else b.dataset.valor = valor;
      b.setAttribute('aria-pressed', activa ? 'true' : 'false');
      b.appendChild(nodo('span', 'hdt-ficha-texto', texto));
      b.appendChild(nodo('span', 'hdt-ficha-cifra', cifra(valor)));
      return b;
    }
    var frag = document.createDocumentFragment();
    frag.appendChild(chip(t('tablas.fichas.todas'), null));
    arr(st.fichas.opciones).forEach(function (op) { frag.appendChild(chip(op.texto, op.valor)); });
    st.fichasEl.textContent = '';
    st.fichasEl.appendChild(frag);
  }

  // ── Filters (filtros) ────────────────────────────────────────────────────
  // st.fsel: field -> {value: true}. It survives new data; a chosen value
  // that's no longer in the data drops out of its filter.
  function cargarFiltros(st, filtros) {
    st.filtros = arr(filtros).map(function (f) {
      return { campo: f.campo, titulo: f.titulo, valores: arr(f.valores), opciones: arr(f.opciones) };
    });
    var fsel = {};
    st.filtros.forEach(function (f) {
      var antes = st.fsel[f.campo], quedan = {}, alguno = false;
      if (!antes) return;
      f.opciones.forEach(function (op) { if (antes[op.valor]) { quedan[op.valor] = true; alguno = true; } });
      if (alguno) fsel[f.campo] = quedan;
    });
    st.fsel = fsel;
  }

  function filtrosActivos(st) {
    return st.filtros.filter(function (f) { var s = st.fsel[f.campo]; return !!s && Object.keys(s).length > 0; });
  }

  // OR inside a filter, AND across them; `salvo` is left out (a menu counts
  // what each of its values would show with the other filters as they are).
  function pasaFiltros(st, activos, i, salvo) {
    for (var k = 0; k < activos.length; k++) {
      var f = activos[k];
      if (f !== salvo && !st.fsel[f.campo][f.valores[i]]) return false;
    }
    return true;
  }

  function pintarFiltros(st, activos) {
    if (!st.filtrosEl) return;
    Array.prototype.forEach.call(st.filtrosEl.children, function (b, k) {
      var f = st.filtros[k];
      b.hidden = !f;
      if (!f) return;
      var s = st.fsel[f.campo] || {};
      var elegidas = f.opciones.filter(function (op) { return s[op.valor]; });
      // Nothing chosen: just the title ("Moneda"), so an idle filter reads
      // differently from an active one and no "Todas/Todos" is needed.
      var valor = elegidas.length === 1 ? elegidas[0].texto
                : tn('tablas.filtros.n', elegidas.length, { n: FMT_N.format(elegidas.length) });
      b.textContent = elegidas.length ? t('tablas.filtros.boton', { filtro: f.titulo, valor: valor }) : f.titulo;
      b.title = elegidas.map(function (op) { return op.texto; }).join(', ');
      b.classList.toggle('hdt-on', elegidas.length > 0);
    });
    st.btnLimpiar.hidden = !activos.length;
    if (MENU && MENU.st === st) contarMenu(st);
  }

  // One menu open in the page at a time. It lives inside the table's root
  // (so a modal's focus trap doesn't fight it) with position: fixed, placed
  // like the bubble so it never leaves the window.
  var MENU = null;   // { st, k, el, boton }
  function abrirMenu(st, k, boton, enfocar) {
    cerrarMenu(false);
    var f = st.filtros[k];
    if (!f) return;
    var m = nodo('div', 'hdt-menu');
    m.setAttribute('role', 'group');
    m.setAttribute('aria-label', f.titulo);
    f.opciones.forEach(function (op) {
      var l = nodo('label', 'hdt-menu-op');
      var cb = nodo('input');
      cb.type = 'checkbox';
      cb.dataset.valor = op.valor;
      l.appendChild(cb);
      l.appendChild(nodo('span', 'hdt-menu-texto', op.texto));
      l.appendChild(nodo('span', 'hdt-menu-n'));
      m.appendChild(l);
    });
    m.addEventListener('change', function (e) {
      var v = e.target.dataset && e.target.dataset.valor;
      if (v == null) return;
      var s = st.fsel[f.campo] || (st.fsel[f.campo] = {});
      if (e.target.checked) s[v] = true; else delete s[v];
      aplicar(st);
    });
    st.root.appendChild(m);
    MENU = { st: st, k: k, el: m, boton: boton };
    boton.setAttribute('aria-expanded', 'true');
    contarMenu(st);
    colocar(m, boton.getBoundingClientRect());
    var primera = m.querySelector('input');
    if (enfocar && primera) primera.focus({ preventScroll: true });
  }

  function cerrarMenu(devolverFoco) {
    if (!MENU) return;
    var m = MENU;
    MENU = null;
    m.boton.setAttribute('aria-expanded', 'false');
    if (m.el.parentNode) m.el.parentNode.removeChild(m.el);
    if (devolverFoco) m.boton.focus({ preventScroll: true });
  }

  // Counts over what the search, the chip and the other filters leave.
  function contarMenu(st) {
    var f = st.filtros[MENU.k];
    if (!f) return;
    var activos = filtrosActivos(st), cuenta = {};
    var fv = st.fichas && st.ficha != null ? st.fichas.valores : null;
    st.trasBusq.forEach(function (i) {
      if ((fv && fv[i] !== st.ficha) || !pasaFiltros(st, activos, i, f)) return;
      cuenta[f.valores[i]] = (cuenta[f.valores[i]] || 0) + 1;
    });
    var s = st.fsel[f.campo] || {};
    Array.prototype.forEach.call(MENU.el.querySelectorAll('input'), function (cb) {
      cb.checked = !!s[cb.dataset.valor];
      cb.parentNode.lastChild.textContent = FMT_N.format(cuenta[cb.dataset.valor] || 0);
    });
  }

  // Esc and a press anywhere outside close it. Esc is caught before a
  // Bootstrap modal around the table sees it, so only the menu closes.
  document.addEventListener('keydown', function (e) {
    if (!MENU || e.key !== 'Escape') return;
    e.preventDefault();
    e.stopPropagation();
    cerrarMenu(true);
  }, true);
  document.addEventListener('mousedown', function (e) {
    if (MENU && !MENU.el.contains(e.target) && !MENU.boton.contains(e.target)) cerrarMenu(false);
  }, true);
  // Scrolling the page (or resizing it) moves the button: the menu follows.
  var menuPendiente = false;
  function seguirMenu() {
    if (!MENU || menuPendiente) return;
    menuPendiente = true;
    requestAnimationFrame(function () {
      menuPendiente = false;
      if (!MENU) return;
      if (!document.body.contains(MENU.boton)) { cerrarMenu(false); return; }
      colocar(MENU.el, MENU.boton.getBoundingClientRect());
    });
  }
  window.addEventListener('scroll', function (e) { if (MENU && !MENU.el.contains(e.target)) seguirMenu(); }, true);
  window.addEventListener('resize', seguirMenu);

  // Places a position: fixed element under the rect r (above it if it
  // doesn't fit below), always inside the window. The bubble and the filter
  // menus share it.
  function colocar(el, r) {
    var w = el.getBoundingClientRect();
    var left = r.left, top = r.bottom + 4;
    if (left + w.width > window.innerWidth - 8) left = Math.max(8, window.innerWidth - w.width - 8);
    if (top + w.height > window.innerHeight - 8) top = Math.max(8, r.top - w.height - 4);
    el.style.left = left + 'px';
    el.style.top = top + 'px';
    // A transformed ancestor (a modal while it animates) moves what's
    // fixed inside it; correct by what actually landed.
    var real = el.getBoundingClientRect();
    if (Math.abs(real.left - left) > 0.5) el.style.left = (2 * left - real.left) + 'px';
    if (Math.abs(real.top - top) > 0.5) el.style.top = (2 * top - real.top) + 'px';
  }

  function densidad(st, modo, guardar) {
    st.root.classList.toggle('hdt-normal', modo === 'normal');
    st.btnDens.textContent = t(modo === 'normal' ? 'tablas.densidad.normal' : 'tablas.densidad.compacta');
    st.btnDens.classList.toggle('hdt-on', modo === 'normal');
    if (guardar) { try { localStorage.setItem('hdt.densidad.' + st.tabla, modo); } catch (e) {} }
    // Row height and column widths change with the density: measure again.
    st.altoFila = null; st.altoMedido = false; st.anchos = null;
    if (st.modo === 'ventana') { var top = st.scroll.scrollTop; salirVentana(st); st.modo = null; pintarFilas(st, top); }
    if (st.scroll) programarAlto(st, true);
  }

  function cargar(st, msg) {
    var t0 = performance.now();
    var f = msg.filas || {};
    var arriba = st.scroll.scrollTop, izq = st.scroll.scrollLeft;

    var cabeza = { columnas: msg.columnas, acciones: msg.acciones_fila || null };
    var colsCambian = JSON.stringify(cabeza) !== JSON.stringify(st.colsMeta);
    st.colsMeta = cabeza;
    // Row actions in catalog order; .acciones narrows them per row.
    st.accFila = st.p.acc_fila ? arr(msg.acciones_fila) : [];
    st.permitidas = f.acciones == null ? null : arr(f.acciones).map(function (s) {
      var set = {};
      String(s || '').split(',').forEach(function (a) { if (a) set[a] = true; });
      return set;
    });
    st.cols = arr(msg.columnas);
    st.ids = arr(f.ids).map(String);
    st.n = st.ids.length;
    st.bloq = arr(f.bloqueada);
    st.moneda = arr(f.moneda);
    st.tipo = arr(f.tipo);
    st.prov = arr(f.provision);
    if (msg.fichas) st.fichas = { valores: arr(msg.fichas.valores), opciones: arr(msg.fichas.opciones), mostrar: msg.fichas.mostrar };
    if (st.filtrosEl) cargarFiltros(st, msg.filtros);
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
    // An open filter menu is rebuilt: its values may have changed.
    if (MENU && MENU.st === st) abrirMenu(st, MENU.k, MENU.boton, false);
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
    if (st.accFila.length) {
      var tha = nodo('th', 'hdt-acc');
      tha.setAttribute('aria-label', t('tablas.acciones'));
      // Invisible mold with every button, so the column is always as wide as
      // the widest row can be -- also when window mode freezes widths from a
      // sample that may not include the text button.
      var molde = nodo('div', 'hdt-acc-molde');
      molde.setAttribute('aria-hidden', 'true');
      st.accFila.forEach(function (a) {
        var texto = a.estilo === 'texto';
        molde.appendChild(nodo('span', 'hdt-accion' + (texto ? ' hdt-accion-texto' : ''), texto ? a.titulo : a.icono));
      });
      tha.appendChild(molde);
      tr.appendChild(tha);
    }
    st.thead.appendChild(tr);
  }

  function trDe(st, i) { return st.trs[i] || (st.trs[i] = fila(st, i)); }

  // Right-aligned row buttons, always in catalog order. .acciones only hides
  // buttons; the server checks the permission again (see R/hd_table.R).
  function botonesFila(st, i) {
    var td = nodo('td', 'hdt-acc');
    var permitidas = st.permitidas ? st.permitidas[i] : null;
    st.accFila.forEach(function (a) {
      // crear_provision is the one button with text instead of an icon.
      var texto = a.estilo === 'texto';
      // An action this row doesn't show leaves an invisible gap of its own
      // width, so every action keeps its column across rows.
      if ((permitidas && !permitidas[a.accion]) || (a.accion === 'provision' && !st.prov[i])) {
        var hueco = nodo('span', 'hdt-accion hdt-accion-hueco' + (texto ? ' hdt-accion-texto' : ''), texto ? a.titulo : a.icono);
        hueco.setAttribute('aria-hidden', 'true');
        td.appendChild(hueco);
        return;
      }
      var b = nodo('button', 'hdt-accion' + (texto ? ' hdt-accion-texto' : ''), texto ? a.titulo : a.icono);
      b.type = 'button';
      b.dataset.accion = a.accion;
      b.title = a.titulo;
      b.setAttribute('aria-label', a.titulo);
      td.appendChild(b);
    });
    return td;
  }

  function enviarAccion(st, accion, ids) {
    if (window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue(st.ns + 'accion', { accion: accion, ids: ids, nonce: Math.random() }, { priority: 'event' });
  }

  function fila(st, i) {
    var tr = nodo('tr');
    tr._i = i;
    if (st.bloq[i]) tr.className = 'hdt-bloq';
    var prov = st.p.provision && st.prov[i];
    if (prov) tr.classList.add('hdt-prov');
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
      // Provision rows: one badge, always in the first column.
      if (prov && j === 0) td.appendChild(nodo('span', 'hdt-ins hdt-tono-morado hdt-ins-prov', t('tablas.provision')));
      // Badges only with the `insignias` piece; otherwise the same text, plain.
      if (st.p.insignias && c.tipo === 'empresa' && v) td.appendChild(nodo('span', 'hdt-ins hdt-emp', v));
      else if (st.p.insignias && c.tipo === 'insignia' && v) {
        var tono = String((st.celdas[j].tono || [])[i] || 'neutro').replace(/[^a-z_]/g, '');
        td.appendChild(nodo('span', 'hdt-ins hdt-tono-' + tono, v));
      } else td.appendChild(document.createTextNode(v == null ? '' : v));
      tr.appendChild(td);
    }
    if (st.accFila.length) tr.appendChild(botonesFila(st, i));
    return tr;
  }

  // ── Filter, sort, paint ──────────────────────────────────────────────────
  // `arriba`: scroll position to land on (new data keeps where you were);
  // omitted, the current one.
  function aplicar(st, desdeDatos, arriba) {
    cerrarConfirmacion(st);   // the rows it was about may no longer be the ones shown
    // Search, then filters (base: what the chips count), then the chip.
    var activos = filtrosActivos(st), trasBusq = [], base = [], vis = [];
    for (var i = 0; i < st.n; i++) {
      if (st.q && st.busq[i].indexOf(st.q) < 0) continue;
      trasBusq.push(i);
      if (pasaFiltros(st, activos, i, null)) base.push(i);
    }
    st.trasBusq = trasBusq;
    pintarFichas(st, base);
    var fv = st.fichas && st.ficha != null ? st.fichas.valores : null;
    vis = fv ? base.filter(function (k) { return fv[k] === st.ficha; }) : base;
    if (st.orden.col >= 0) ordenarVis(st, vis);
    st.vis = vis;
    pintarFiltros(st, activos);
    var filtrada = !!st.q || !!fv || activos.length > 0;

    // Without sel_persistente, rows the search or a chip hides drop out of
    // the selection.
    if (!desdeDatos && !st.p.sel_persistente && filtrada) {
      var visibles = new Set(vis.map(function (k) { return st.ids[k]; }));
      var cambio = false;
      st.sel.forEach(function (id) { if (!visibles.has(id)) { st.sel.delete(id); cambio = true; } });
      if (cambio) enviarSel(st);
    }

    pintarFilas(st, arriba == null ? st.scroll.scrollTop : arriba);

    st.vacio.hidden = vis.length > 0;
    if (!vis.length) st.vacio.textContent = t(st.n && filtrada ? 'tablas.vacio_filtro' : 'tablas.vacio');
    pintarSel(st);
    flechas(st);
    if (st.contador) st.contador.textContent = tn(st.textos.contador || 'tablas.contador.registros', vis.length, { n: FMT_N.format(vis.length) });
    programarAlto(st, desdeDatos);
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
    st.espArriba = st.espAbajo = null;
    if (st.colgroup) st.colgroup.textContent = '';
    st.tabla_el.classList.remove('hdt-fijas');
    st.root.classList.remove('hdt-ventana');
    st.vIni = st.vFin = null;
  }

  function espaciador(st, alto) {
    var tr = nodo('tr', 'hdt-esp'), td = nodo('td');
    td.colSpan = st.cols.length + (st.p.casillas ? 1 : 0) + (st.accFila.length ? 1 : 0);
    td.style.height = alto + 'px';
    tr.appendChild(td);
    return tr;
  }

  // Paints only the rows around `arriba` (or the current scroll position).
  function pintarVentana(st, forzar, arriba) {
    var h = st.altoFila || 23, n = st.vis.length;
    var top = arriba == null ? st.scroll.scrollTop : arriba;
    // In auto height the view is the height being set, not what the
    // spacers would stretch the area to before the first measurement.
    var vista = st.root.classList.contains('hdt-auto')
      ? (st.altoActual || window.innerHeight * HDT_ALTO_VENTANA)
      : (st.scroll.clientHeight || 420);
    var ini = Math.max(0, Math.floor(top / h) - HDT_COLCHON);
    var fin = Math.min(n, Math.ceil((top + vista) / h) + HDT_COLCHON);
    if (!forzar && ini === st.vIni && fin === st.vFin) return;
    var vIni = st.vIni, vFin = st.vFin;
    st.vIni = ini; st.vFin = fin;
    // Scrolling: only the rows leaving the window go out and only the ones
    // entering come in. Rows that stay are never detached, so a click that
    // lands while the window moves (mousedown and mouseup on the same row)
    // is never lost.
    if (!forzar && st.espArriba && vIni != null && ini < vFin && fin > vIni) {
      var k;
      for (k = vIni; k < Math.min(ini, vFin); k++) st.tbody.removeChild(trDe(st, st.vis[k]));
      for (k = Math.max(fin, vIni); k < vFin; k++) st.tbody.removeChild(trDe(st, st.vis[k]));
      var antes = document.createDocumentFragment();
      for (k = ini; k < Math.min(vIni, fin); k++) antes.appendChild(trDe(st, st.vis[k]));
      st.tbody.insertBefore(antes, st.espArriba.nextSibling);
      var despues = document.createDocumentFragment();
      for (k = Math.max(vFin, ini); k < fin; k++) despues.appendChild(trDe(st, st.vis[k]));
      st.tbody.insertBefore(despues, st.espAbajo);
      st.espArriba.firstChild.style.height = (ini * h) + 'px';
      st.espAbajo.firstChild.style.height = ((n - fin) * h) + 'px';
      return;
    }
    var frag = document.createDocumentFragment();
    st.espArriba = espaciador(st, ini * h);
    st.espAbajo = espaciador(st, (n - fin) * h);
    frag.appendChild(st.espArriba);
    for (var j = ini; j < fin; j++) frag.appendChild(trDe(st, st.vis[j]));
    frag.appendChild(st.espAbajo);
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
    pintarBarraAcc(st);
  }

  // Ids in the order they're shown; selected rows hidden by the search last.
  function idsSel(st) {
    var out = [], vistos = new Set();
    st.vis.forEach(function (i) { if (st.sel.has(st.ids[i])) { out.push(st.ids[i]); vistos.add(st.ids[i]); } });
    st.ids.forEach(function (id) { if (st.sel.has(id) && !vistos.has(id)) out.push(id); });
    return out;
  }

  function enviarSel(st) {
    cerrarConfirmacion(st);   // the selection changed: a pending confirmation no longer applies
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
      // The second press of a double click would shade a word.
      if (st.p.detalle && e.detail > 1 && !e.target.closest('input, button')) e.preventDefault();
    });

    st.tbody.addEventListener('click', function (e) {
      var tr = e.target.closest('tr');
      if (!tr || tr._i == null) return;
      var i = tr._i;
      // A row button sends its action and never touches the selection.
      var btn = e.target.closest('button.hdt-accion');
      if (btn) {
        var a = st.accFila.filter(function (x) { return x.accion === btn.dataset.accion; })[0];
        if (a && a.confirmar) pedirConfirmacion(st, a.accion, a.titulo, [st.ids[i]], a.peligro);
        else enviarAccion(st, btn.dataset.accion, [st.ids[i]]);
        return;
      }
      // Second click of a double click: the dblclick handler undoes the first.
      if (st.p.detalle && e.detail > 1) return;
      if (st.p.detalle) st.antesClic = { sel: new Set(st.sel), ancla: st.ancla };
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
      if (st.p.copiar_tsv && document.activeElement !== st.scroll) st.scroll.focus({ preventScroll: true });
    });

    // Double click opens the detail -- locked rows too -- and leaves the
    // selection as it was before the first of its two clicks.
    if (st.p.detalle) {
      st.tbody.addEventListener('dblclick', function (e) {
        var tr = e.target.closest('tr');
        if (!tr || tr._i == null || e.target.closest('input, button')) return;
        if (st.antesClic) {
          st.sel = st.antesClic.sel;
          st.ancla = st.antesClic.ancla;
          st.antesClic = null;
          pintarSel(st);
          enviarSel(st);
        }
        var s = window.getSelection && window.getSelection();
        if (s) s.removeAllRanges();
        enviarAccion(st, 'detalle', [st.ids[tr._i]]);
      });
    }

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
  window.addEventListener('resize',  function () {
    Object.keys(TABLAS).forEach(function (id) { programarAlto(TABLAS[id], true); });
  });

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
    colocar(b, td.getBoundingClientRect());
  }
  function burbuja(st) {
    // mousemove, not mouseover: after a scroll the browser sends mouseover
    // for whatever cell slid under a still cursor, and the bubble would pop
    // up (and cover the next click) without the mouse having moved.
    var ultimoTd = null;
    st.tbody.addEventListener('mousemove', function (e) {
      var td = e.target.closest('td');
      if (!td || td._c == null || td === ultimoTd) return;
      ultimoTd = td;
      clearTimeout(tipOcultar);
      clearTimeout(tipVer);
      tipVer = setTimeout(function () { tipAbrir(st, td); }, 350);
    });
    st.tbody.addEventListener('mouseleave', function () { ultimoTd = null; });
    // A scroll cancels a bubble about to open, too.
    st.scroll.addEventListener('scroll', function () { clearTimeout(tipVer); ultimoTd = null; }, { passive: true });
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

  // ── Export (excel) ───────────────────────────────────────────────────────
  function exportar(st) {
    st.excelNonce = String(Math.random());
    var ids = st.vis.map(function (i) { return st.ids[i]; });
    Shiny.setInputValue(st.ns + 'vista', { ids: ids, nonce: st.excelNonce }, { priority: 'event' });
  }
  function excelListo(msg) {
    var st = TABLAS[msg.id];
    if (!st || !st.lnkExcel || msg.nonce !== st.excelNonce) return;
    st.excelNonce = null;
    st.lnkExcel.click();
  }

  // ── Copy marked rows as TSV (copiar_tsv) ─────────────────────────────────
  // Ctrl+C with rows marked, focus in the table and no text shaded: header
  // plus marked rows in visible order, raw values, tab-separated, so they
  // paste straight into Excel. With text shaded, the browser copies it.
  function neutralizar(s) {
    s = String(s == null ? '' : s).replace(/[\t\r\n]+/g, ' ');
    return /^[=+\-@]/.test(s) ? "'" + s : s;
  }
  function filasTsv(st) {
    var ids = idsSel(st);
    var lineas = [st.cols.map(function (c) { return neutralizar(c.titulo); }).join('\t')];
    ids.forEach(function (id) {
      var i = st.idx[id];
      lineas.push(st.cols.map(function (c, j) { return neutralizar(st.celdas[j].copiar[i]); }).join('\t'));
    });
    return { texto: lineas.join('\r\n'), n: ids.length };
  }
  function avisar(st, texto) {
    st.aviso.textContent = texto;
    st.aviso.hidden = false;
    clearTimeout(st.avisoT);
    st.avisoT = setTimeout(function () { st.aviso.hidden = true; }, 1600);
  }
  document.addEventListener('copy', function (e) {
    var activo = document.activeElement;
    var st = null;
    Object.keys(TABLAS).forEach(function (id) {
      var x = TABLAS[id];
      if (activo && x.root.contains(activo) && document.body.contains(x.root)) st = x;
    });
    if (!st || !st.p.copiar_tsv || !st.sel.size) return;
    if (window.getSelection && String(window.getSelection()).length) return;   // shaded text: normal copy
    var r = filasTsv(st);
    if (!r.n || !e.clipboardData) return;
    e.clipboardData.setData('text/plain', r.texto);
    e.preventDefault();
    avisar(st, tn('tablas.copiadas_n', r.n, { n: FMT_N.format(r.n) }));
  });

  // ── Test / diagnostics hook ──────────────────────────────────────────────
  window.HDT = {
    recibidos: function () { return recibidos; },
    estado: function (id) {
      var st = TABLAS[id];
      if (!st) return null;
      return {
        visibles: st.vis.map(function (i) { return st.ids[i]; }),
        seleccion: idsSel(st), orden: st.orden, busqueda: st.q,
        filtros: Object.keys(st.fsel).reduce(function (o, c) { o[c] = Object.keys(st.fsel[c]); return o; }, {}),
        scrollTop: st.scroll.scrollTop, medidas: st.medidas,
        modo: st.modo, pintadas: st.tbody.querySelectorAll('tr:not(.hdt-esp)').length,
        altoFila: st.altoFila, ventana: [st.vIni, st.vFin],
        bloqueadas: st.ids.filter(function (id, i) { return !!st.bloq[i]; })
      };
    }
  };

  (function registrar() {
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('hdt-datos', recibir);
      Shiny.addCustomMessageHandler('hdt-excel', excelListo);
    }
    else setTimeout(registrar, 20);
  })();
})();
