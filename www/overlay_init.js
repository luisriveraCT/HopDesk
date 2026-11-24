/* overlay_init.js
 * Loading overlay: shared state survives the shinymanager session swap.
 * Sourced via tags$script(src=) so R never needs to parse the JS content.
 */

/* 0 -- When the overlay may go (T88). It used to stay a fixed 30 s after the
   100 % (nothing sent calFadeNow any more, section 9): the app was ready at
   ~28 s and the overlay left at ~45 s. Now it goes once the app is really
   done, never half-loaded:
     - the 100 % came (Phase 2 finished);
     - the client's theme came (the server's hdTema message);
     - the open tab has content and nothing in it is recalculating;
     - Shiny has been idle for SILENCIO ms (no shiny:busy since).
   TOPE ms after the 100 % it goes anyway, as it always did. Pure (state and
   a clock in, a verdict out) for tests/test_overlay_salida.R. */
window.hldSalida = {
  SILENCIO: 1500,
  TOPE: 30000,
  /* s: {pct100At, temaAt, busy, idleAt, pestanaLista}; times in ms.
     'esperar' | 'quitar' | 'tope' */
  decidir: function(s, ahora) {
    if (!s || !s.pct100At) return 'esperar';
    if (ahora - s.pct100At >= this.TOPE) return 'tope';
    if (!s.temaAt || s.busy || !s.pestanaLista) return 'esperar';
    if (ahora - Math.max(s.idleAt || 0, s.pct100At) < this.SILENCIO) return 'esperar';
    return 'quitar';
  }
};

$(function() {

  /* 1 -- Shared state across shinymanager UI swaps */
  if (!window._hld) {
    window._hld = {
      ov: null, msgT: null, creepT: null,
      done: false, active: false, arReadyAt: 0,
      msgs: [], mi: 0, cur: 8, target: 8,
      failObs: null
    };
  }
  var H = window._hld;
  if (!H.salida) H.salida = { pct100At: 0, temaAt: 0, busy: false, idleAt: 0, pestanaLista: false };

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

  /* 3 -- Create overlay once; on session-swap reinit, reuse existing element */
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
    }, 900);
    H.msgT = setInterval(function() {
      if (H.done) return;
      H.mi = (H.mi + 1) % H.msgs.length;
      _setMsg(H.msgs[H.mi]);
    }, 11000);
    /* Safety: hide after 6 min if server never signals done */
    setTimeout(function() {
      if (!H.done) { H.done = true; H.ov.style.display = 'none'; }
    }, 360000);
  }
  function _hideForRetry() {
    H.ov.style.display = 'none';
    H.active = false; H.done = false;
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
        H.ov.classList.add('hld-fadeout');
        setTimeout(function() { H.ov.style.display = 'none'; }, 950);
      });
    });
  }
  function _vigilarSalida() {
    if (H.salidaT) return;
    H.salidaT = setInterval(function() {
      H.salida.pestanaLista = _pestanaLista();
      var v = window.hldSalida.decidir(H.salida, Date.now());
      if (v !== 'esperar') _quitar(v);
    }, 250);
  }

  /* 6 -- Intercept login button */
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
  var _btnObs = new MutationObserver(_wireBtn);
  _btnObs.observe(document.body, { childList: true, subtree: true });
  _wireBtn();

  /* 7 -- showOverlay: server signals session #2 is starting.
     Kill the auth-failure watcher first — it must not interfere with
     session #2's DOM.  Then force the overlay on top regardless of its
     current display state (covers the blank navbar during app init).  */
  Shiny.addCustomMessageHandler('showOverlay', function() {
    if (H.failObs) { H.failObs.disconnect(); H.failObs = null; }
    H.done = false;
    H.salida.pct100At = 0; H.salida.motivo = null; H.salida.quitadaAt = 0;
    H.ov.classList.remove('hld-fadeout');
    H.active = true;
    H.ov.style.display = 'flex';
    if (!H.msgT && !H.creepT) _startAnim();
  });

  /* 8 -- loadingProgress: Phase 1 / Phase 2 progress bar */
  Shiny.addCustomMessageHandler('loadingProgress', function(d) {
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


  /* 9 -- calFadeNow: server sends this after the 2nd post-Phase-2 AR render,
     which is when the calendar is fully settled (empresa cascade complete).
     Two rAFs guarantee the browser has painted the calendar before we fade.
     No timers, no observers, no caps — server decides when it's ready.
     H.arReadyAt tracks the last hop-cal-ready{AR} event (informational).  */
  document.addEventListener('hop-cal-ready', function(e) {
    if (e.detail && e.detail.ledger === 'AR') H.arReadyAt = Date.now();
  });

  Shiny.addCustomMessageHandler('calFadeNow', function() {
    requestAnimationFrame(function() {
      requestAnimationFrame(function() {
        if (!H.ov.classList.contains('hld-fadeout')) {
          H.ov.classList.add('hld-fadeout');
          setTimeout(function() { H.ov.style.display = 'none'; }, 950);
        }
      });
    });
  });
});
