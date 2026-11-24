/* overlay_init.js
 * Loading overlay, from the click on "Entrar" until the app is really done.
 * Sourced via tags$script(src=) so R never needs to parse the JS content.
 *
 * Two pages load it (app.R):
 *   - the sign-in page (secure_app's tags_top): the overlay shows on the
 *     click and covers until the browser leaves for the app page --
 *     shinymanager RELOADS the page after a good sign-in;
 *   - the app page (data-pagina="app", in <head>): it keeps covering from
 *     the very first frame when the page comes from a sign-in (the mark,
 *     section 0b), and goes once the app is done (section 0).
 * T88-1b: before, the app page never had this file, so a client saw the app
 * half-built for ~30 s (the tab bar at ~15 s, data until ~50 s).
 */
(function() {
  if (window._hldCargado) return;   /* once per page: handlers never register twice */
  window._hldCargado = true;

  /* 0 -- When the overlay may go (T88). Never half-loaded:
       - the 100 % came (Phase 2 finished);
       - the client's theme came (the server's hdTema message);
       - the open tab has content and nothing in it is recalculating;
       - Shiny has been idle for SILENCIO ms (no shiny:busy since).
     TOPE ms after the 100 % it goes anyway; TOPE_CARGA ms after the page
     loaded too (a session that breaks never sends the 100 %). Pure (state
     and a clock in, a verdict out) for tests/test_overlay_salida.R. */
  window.hldSalida = {
    SILENCIO: 1500,
    TOPE: 30000,
    TOPE_CARGA: 120000,
    /* s: {cargaAt, pct100At, temaAt, busy, idleAt, pestanaLista}; times in ms.
       'esperar' | 'quitar' | 'tope' */
    decidir: function(s, ahora) {
      if (!s) return 'esperar';
      if (s.cargaAt && ahora - s.cargaAt >= this.TOPE_CARGA) return 'tope';
      if (!s.pct100At) return 'esperar';
      if (ahora - s.pct100At >= this.TOPE) return 'tope';
      if (!s.temaAt || s.busy || !s.pestanaLista) return 'esperar';
      if (ahora - Math.max(s.idleAt || 0, s.pct100At) < this.SILENCIO) return 'esperar';
      return 'quitar';
    }
  };

  /* 0b -- The sign-in mark: set on the click, read by the app page that the
     reload brings. sessionStorage can be missing or blocked (private mode,
     site data off): every touch is in try/catch, and without it everything
     still works -- the server's showOverlay covers instead, a bit later. */
  window.hldMarca = {
    CLAVE: 'hld-entrando',
    VIGENCIA: 180000,   /* a cold start took 57 s from the click to the app page */
    poner: function(pct, ahora) {
      try { window.sessionStorage.setItem(this.CLAVE, JSON.stringify({ t: ahora, pct: pct })); } catch (e) {}
    },
    leer: function(ahora) {
      var v = null;
      try { v = JSON.parse(window.sessionStorage.getItem(this.CLAVE) || 'null'); } catch (e) { v = null; }
      if (!v || typeof v.t !== 'number' || !(ahora - v.t >= 0 && ahora - v.t < this.VIGENCIA)) {
        this.borrar();
        return null;
      }
      return v;
    },
    borrar: function() {
      try { window.sessionStorage.removeItem(this.CLAVE); } catch (e) {}
    }
  };

  /* 0c -- Right now, while the page is still being parsed. On the app page,
     with a fresh mark: hide everything but the overlay from the first frame
     (the overlay itself is built when the DOM is ready). On any other page:
     drop a mark left over (a failed sign-in, a sign-out, the login again). */
  var _yo = document.currentScript;
  var _esApp = !!(_yo && _yo.getAttribute('data-pagina') === 'app');
  var _cargaAt = Date.now();
  var _marca = _esApp ? window.hldMarca.leer(_cargaAt) : null;
  if (!_esApp) window.hldMarca.borrar();

  /* Never stuck: lets the page go whatever happens to the rest of this file.
     Safe to call more than once. */
  window._hldSoltar = function(motivo) {
    try { document.documentElement.classList.remove('hld-entrando'); } catch (e) {}
    window.hldMarca.borrar();
    var H = window._hld;
    if (H) {
      if (H.salidaT) { clearInterval(H.salidaT); H.salidaT = null; }
      if (H.msgT)    { clearInterval(H.msgT);    H.msgT    = null; }
      if (H.creepT)  { clearInterval(H.creepT);  H.creepT  = null; }
      if (H.salida && !H.salida.motivo) { H.salida.motivo = motivo; H.salida.quitadaAt = Date.now(); }
      H.done = true;
    }
    var ov = document.getElementById('hopdesk-loading');
    if (ov && !ov.classList.contains('hld-fadeout')) {
      ov.classList.add('hld-fadeout');
      setTimeout(function() { ov.style.display = 'none'; }, 950);
    }
  };

  if (_marca) {
    document.documentElement.classList.add('hld-entrando');
    var _tapa = document.createElement('style');
    _tapa.id = 'hld-entrando-css';
    _tapa.textContent =
      'html.hld-entrando{background:var(--hd-acceso-carga-fondo);}' +
      'html.hld-entrando body{background:var(--hd-acceso-carga-fondo)!important;}' +
      'html.hld-entrando body>*:not(#hopdesk-loading){visibility:hidden!important;}';
    (document.head || document.documentElement).appendChild(_tapa);
  }
  if (_esApp) {
    setTimeout(function() { window._hldSoltar('tope_carga'); }, window.hldSalida.TOPE_CARGA);
  }

  function _cuandoListo(f) {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', f);
    else f();
  }

  _cuandoListo(function() {
    try {
      _iniciar();
    } catch (e) {
      /* Whatever broke, the page must not stay covered. */
      if (window.console) console.error('[overlay] ', e);
      window._hldSoltar('error');
    }
  });

  function _iniciar() {
    var $ = window.jQuery;

    /* 1 -- Shared state */
    if (!window._hld) {
      window._hld = {
        ov: null, msgT: null, creepT: null, salidaT: null,
        done: false, active: false, arReadyAt: 0,
        msgs: [], mi: 0, cur: 8, target: 8,
        failObs: null
      };
    }
    var H = window._hld;
    if (!H.salida) H.salida = { cargaAt: _cargaAt, pct100At: 0, temaAt: 0, busy: false, idleAt: 0, pestanaLista: false };

    /* 1a -- What section 0 needs: the theme message and Shiny's busy/idle.
       A listener on shiny:message, not a second hdTema handler (that would
       replace R/tema_registro.R's). */
    $(document).on('shiny:message', function(e) {
      var c = e.message && e.message.custom;
      if (c && c.hdTema) H.salida.temaAt = Date.now();
    });
    $(document).on('shiny:busy', function() { H.salida.busy = true; });
    $(document).on('shiny:idle', function() { H.salida.busy = false; H.salida.idleAt = Date.now(); });

    /* 1b -- Texts: the dictionary (cuentas.carga.*). The sign-in page carries
       only these keys (i18n_head_parcial() in app.R), without www/i18n.js:
       a plain lookup when hdT() is not there. A missing key shows the key. */
    var hdT = window.hdT || function(k) {
      var d = window.HD_I18N || {};
      return d[k] != null ? d[k] : k;
    };

    /* 2 -- Inject CSS once (idempotent) */
    if (!document.getElementById('hld-css')) {
      var _hldCss = document.createElement('style');
      _hldCss.id  = 'hld-css';
      _hldCss.textContent = [
        '#hopdesk-loading{position:fixed;inset:0;background:var(--hd-acceso-carga-fondo);z-index:99999;',
        'display:flex;flex-direction:column;align-items:center;justify-content:center;',
        'transition:opacity .9s ease;}',
        '#hopdesk-loading.hld-fadeout{opacity:0;pointer-events:none;}',
        '.hld-logo-area{display:flex;flex-direction:column;align-items:center;margin-bottom:52px;}',
        '.hld-icon-svg{width:58px;height:58px;margin-bottom:20px;',
        'animation:hld-pulse 2.4s ease-in-out infinite;}',
        '@keyframes hld-pulse{0%,100%{opacity:.72;transform:scale(1);}',
        '50%{opacity:1;transform:scale(1.06);}}',
        '.hld-brand-name{font-size:2rem;font-weight:800;letter-spacing:.3em;',
        'color:var(--hd-acceso-carga-oro);text-transform:uppercase;}',
        '.hld-brand-tagline{font-size:.68rem;letter-spacing:.35em;color:var(--hd-acceso-carga-lema);',
        'text-transform:uppercase;margin-top:5px;}',
        '.hld-msgs{text-align:center;margin-bottom:34px;min-height:54px;}',
        '#hld-msg{font-size:1rem;font-weight:500;color:var(--hd-acceso-carga-texto);transition:opacity .35s ease;}',
        '#hld-msgsub{font-size:.82rem;font-style:italic;color:var(--hd-acceso-carga-texto-2);',
        'margin-top:5px;transition:opacity .35s ease;}',
        '.hld-bar-wrap{width:340px;max-width:82vw;}',
        '.hld-bar-track{height:3px;background:var(--hd-acceso-carga-riel);border-radius:2px;overflow:hidden;}',
        '#hld-bar{height:100%;width:8%;background:linear-gradient(90deg,var(--hd-acceso-carga-oro),var(--hd-acceso-carga-oro-claro));',
        'border-radius:2px;transition:width .9s cubic-bezier(.4,0,.2,1);}',
        '#hld-pct{font-size:.7rem;color:var(--hd-acceso-carga-pct);text-align:right;margin-top:7px;letter-spacing:.04em;}',
        '#ar-calendar.shiny-output-recalculating,',
        '#ap-calendar.shiny-output-recalculating{opacity:1!important;}'
      ].join('');
      document.head.appendChild(_hldCss);
    }

    /* 3 -- Create overlay once */
    var _existing = document.getElementById('hopdesk-loading');
    if (!_existing) {
      H.ov = document.createElement('div');
      H.ov.id = 'hopdesk-loading';
      H.ov.style.display = 'none';
      H.ov.innerHTML =
        '<div class="hld-logo-area">' +
          '<svg class="hld-icon-svg" viewBox="0 0 60 60" fill="none">' +
            '<line x1="30" y1="2"  x2="30" y2="14" style="stroke:var(--hd-acceso-carga-oro)" stroke-width="1.2"/>' +
            '<line x1="30" y1="46" x2="30" y2="58" style="stroke:var(--hd-acceso-carga-oro)" stroke-width="1.2"/>' +
            '<line x1="2"  y1="30" x2="14" y2="30" style="stroke:var(--hd-acceso-carga-oro)" stroke-width="1.2"/>' +
            '<line x1="46" y1="30" x2="58" y2="30" style="stroke:var(--hd-acceso-carga-oro)" stroke-width="1.2"/>' +
            '<path d="M30 16 L44 30 L30 44 L16 30 Z" style="stroke:var(--hd-acceso-carga-oro)" stroke-width="1.2" fill="none"/>' +
            '<circle cx="30" cy="30" r="2.5" style="fill:var(--hd-acceso-carga-oro)"/>' +
          '</svg>' +
          '<div class="hld-brand-name">HOPDESK</div>' +
          '<div class="hld-brand-tagline">' + hdT('cuentas.carga.lema') + '</div>' +
        '</div>' +
        '<div class="hld-msgs">' +
          '<div id="hld-msg"></div>' +
          '<div id="hld-msgsub"></div>' +
        '</div>' +
        '<div class="hld-bar-wrap">' +
          '<div class="hld-bar-track"><div id="hld-bar"></div></div>' +
          '<div id="hld-pct">8%</div>' +
        '</div>';
      document.body.appendChild(H.ov);
    } else {
      H.ov = _existing;
    }

    /* 4 -- Message pool: cuentas.carga.mensaje.NN.{titulo,detalle}, from 01
       while the dictionary has them. */
    var _ALL = [];
    for (var _i = 1; _i < 100; _i++) {
      var _k = 'cuentas.carga.mensaje.' + (_i < 10 ? '0' : '') + _i;
      if ((window.HD_I18N || {})[_k + '.titulo'] == null) break;
      _ALL.push({ h: hdT(_k + '.titulo'), s: hdT(_k + '.detalle') });
    }
    if (!_ALL.length) _ALL = [{ h: '', s: '' }];
    function _shuffle(a) {
      var b = a.slice();
      for (var i = b.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var t = b[i]; b[i] = b[j]; b[j] = t;
      }
      return b;
    }
    if (!H.msgs.length) { H.msgs = _shuffle(_ALL); }

    /* 5 -- DOM helpers */
    function _bar()   { return document.getElementById('hld-bar');    }
    function _pct()   { return document.getElementById('hld-pct');    }
    function _msgEl() { return document.getElementById('hld-msg');    }
    function _subEl() { return document.getElementById('hld-msgsub'); }

    function _setPct(p) {
      H.target = p; H.cur = p;
      if (_bar()) _bar().style.width = p + '%';
      if (_pct()) _pct().textContent = Math.round(p) + '%';
    }
    function _setMsg(obj) {
      var h = _msgEl(), s = _subEl();
      if (!h || !s) return;
      h.style.opacity = '0'; s.style.opacity = '0';
      setTimeout(function() {
        h.textContent = obj.h; s.textContent = obj.s;
        h.style.opacity = '1'; s.style.opacity = '1';
      }, 340);
    }
    function _startAnim() {
      _setMsg(H.msgs[0]);
      H.creepT = setInterval(function() {
        if (H.done || H.cur >= Math.min(H.target + 6, 93)) return;
        H.cur += 0.4;
        if (_bar()) _bar().style.width = H.cur + '%';
        if (_pct()) _pct().textContent = Math.round(H.cur) + '%';
        if (!_esApp) window.hldMarca.poner(H.cur, Date.now());   /* the app page goes on from here */
      }, 900);
      H.msgT = setInterval(function() {
        if (H.done) return;
        H.mi = (H.mi + 1) % H.msgs.length;
        _setMsg(H.msgs[H.mi]);
      }, 11000);
      /* Sign-in page only: hide after 6 min if the app page never comes. */
      if (!_esApp) setTimeout(function() {
        if (!H.done) { H.done = true; H.ov.style.display = 'none'; window.hldMarca.borrar(); }
      }, 360000);
    }
    function _hideForRetry() {
      H.ov.style.display = 'none';
      H.active = false; H.done = false;
      window.hldMarca.borrar();   /* a failed sign-in: no app page is coming */
      if (H.msgT)   { clearInterval(H.msgT);   H.msgT   = null; }
      if (H.creepT) { clearInterval(H.creepT); H.creepT = null; }
      H.msgs = _shuffle(_ALL); H.mi = 0; H.cur = 8; H.target = 8;
      if (_bar()) _bar().style.width = '8%';
      if (_pct()) _pct().textContent = '8%';
    }

    /* 5b -- The open tab: the outermost active pane. Ready when nothing in it
       is recalculating and at least one of its outputs has something drawn. */
    function _pestanaLista() {
      var panes = document.querySelectorAll('.tab-pane.active');
      var pane = null;
      for (var i = 0; i < panes.length; i++) {
        if (!panes[i].parentElement || !panes[i].parentElement.closest('.tab-pane')) { pane = panes[i]; break; }
      }
      if (!pane) return false;
      if (pane.querySelector('.recalculating, .shiny-output-recalculating')) return false;
      var outs = pane.querySelectorAll('.shiny-bound-output');
      for (var j = 0; j < outs.length; j++) {
        if (outs[j].children.length || (outs[j].textContent || '').trim()) return true;
      }
      return false;
    }
    function _quitar(motivo) {
      if (H.salidaT) { clearInterval(H.salidaT); H.salidaT = null; }
      if (H.ov.classList.contains('hld-fadeout')) return;
      H.salida.motivo = motivo;
      H.salida.quitadaAt = Date.now();
      requestAnimationFrame(function() {
        requestAnimationFrame(function() {
          window._hldSoltar(motivo);
        });
      });
    }
    function _vigilarSalida() {
      if (H.salidaT) return;
      H.salidaT = setInterval(function() {
        try {
          H.salida.pestanaLista = _pestanaLista();
          var v = window.hldSalida.decidir(H.salida, Date.now());
          if (v !== 'esperar') _quitar(v);
        } catch (e) {
          if (window.console) console.error('[overlay] ', e);
          window._hldSoltar('error');
        }
      }, 250);
    }

    /* 5c -- The app page that a sign-in brought: the overlay is already
       covering (section 0c); show it, from where the sign-in page left the
       bar, and watch for the way out from now. */
    if (_esApp && _marca) {
      H.active = true;
      H.ov.classList.remove('hld-fadeout');
      H.ov.style.display = 'flex';
      _setPct(Math.max(8, Math.min(Number(_marca.pct) || 8, 44)));
      if (!H.msgT && !H.creepT) _startAnim();
    }
    if (_esApp) _vigilarSalida();

    /* 6 -- Intercept login button (sign-in page only; the app page has none) */
    function _wireBtn() {
      var btn = document.querySelector(
        '#shinymanager-content .btn-primary, .panel-auth .btn-primary'
      );
      if (btn && !btn._hldWired) {
        btn._hldWired = true;
        btn.addEventListener('click', function() {
          if (H.active) return;
          H.active = true;
          H.ov.style.display = 'flex';
          window.hldMarca.poner(8, Date.now());
          _startAnim();
          H.failObs = new MutationObserver(function() {
            var alert = document.querySelector('.alert-danger');
            if (alert && alert.offsetParent !== null) {
              H.failObs.disconnect(); H.failObs = null;
              _hideForRetry();
              btn._hldWired = false;
              setTimeout(_wireBtn, 150);
            }
          });
          H.failObs.observe(document.body, { childList: true, subtree: true });
        });
      }
    }
    if (!_esApp) {
      var _btnObs = new MutationObserver(_wireBtn);
      _btnObs.observe(document.body, { childList: true, subtree: true });
      _wireBtn();
    }

    /* 7 -- showOverlay: the server says the app's session is starting. With
       the mark the overlay is already up; without it (no sessionStorage, an
       F5 on the app) it comes up here. */
    Shiny.addCustomMessageHandler('showOverlay', function() {
      if (H.failObs) { H.failObs.disconnect(); H.failObs = null; }
      if (H.salida.motivo) return;   /* already gone for this page: never back */
      H.done = false;
      H.ov.classList.remove('hld-fadeout');
      H.active = true;
      H.ov.style.display = 'flex';
      if (!H.msgT && !H.creepT) _startAnim();
      _vigilarSalida();
    });

    /* 8 -- loadingProgress: Phase 1 / Phase 2 progress bar */
    Shiny.addCustomMessageHandler('loadingProgress', function(d) {
      if (H.salida.motivo) return;
      _setPct(d.pct);
      if (d.pct >= 100) {
        H.done = true;
        if (!H.salida.pct100At) H.salida.pct100At = Date.now();
        _vigilarSalida();
        if (H.msgT)   clearInterval(H.msgT);
        if (H.creepT) clearInterval(H.creepT);
        if (_bar()) _bar().style.width = '100%';
        if (_pct()) _pct().textContent = '100%';
        var h = _msgEl(), s = _subEl();
        if (h) h.style.opacity = '0';
        if (s) s.style.opacity = '0';
        setTimeout(function() {
          if (h) { h.textContent = hdT('cuentas.carga.listo.titulo');  h.style.opacity = '1'; }
          if (s) { s.textContent = hdT('cuentas.carga.listo.detalle'); s.style.opacity = '1'; }
        }, 380);
      }
    });

    /* 9 -- calFadeNow: nothing sends it today (app.R, ~2920-2958). Kept so a
       future server signal can still take the overlay down at once. */
    document.addEventListener('hop-cal-ready', function(e) {
      if (e.detail && e.detail.ledger === 'AR') H.arReadyAt = Date.now();
    });
    Shiny.addCustomMessageHandler('calFadeNow', function() { _quitar('calFadeNow'); });
  }
})();
