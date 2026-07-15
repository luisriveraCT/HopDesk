/* overlay_init.js
 * Loading overlay: shared state survives the shinymanager session swap.
 * Sourced via tags$script(src=) so R never needs to parse the JS content.
 */
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

  /* 2 -- Inject CSS once (idempotent) */
  if (!document.getElementById('hld-css')) {
    var _hldCss = document.createElement('style');
    _hldCss.id  = 'hld-css';
    _hldCss.textContent = [
      '#hopdesk-loading{position:fixed;inset:0;background:#f8f6f1;z-index:99999;',
      'display:flex;flex-direction:column;align-items:center;justify-content:center;',
      'transition:opacity .9s ease;}',
      '#hopdesk-loading.hld-fadeout{opacity:0;pointer-events:none;}',
      '.hld-logo-area{display:flex;flex-direction:column;align-items:center;margin-bottom:52px;}',
      '.hld-icon-svg{width:58px;height:58px;margin-bottom:20px;',
      'animation:hld-pulse 2.4s ease-in-out infinite;}',
      '@keyframes hld-pulse{0%,100%{opacity:.72;transform:scale(1);}',
      '50%{opacity:1;transform:scale(1.06);}}',
      '.hld-brand-name{font-size:2rem;font-weight:800;letter-spacing:.3em;',
      'color:#b8963e;text-transform:uppercase;}',
      '.hld-brand-tagline{font-size:.68rem;letter-spacing:.35em;color:#a09070;',
      'text-transform:uppercase;margin-top:5px;}',
      '.hld-msgs{text-align:center;margin-bottom:34px;min-height:54px;}',
      '#hld-msg{font-size:1rem;font-weight:500;color:#3a3630;transition:opacity .35s ease;}',
      '#hld-msgsub{font-size:.82rem;font-style:italic;color:#9a8e7e;',
      'margin-top:5px;transition:opacity .35s ease;}',
      '.hld-bar-wrap{width:340px;max-width:82vw;}',
      '.hld-bar-track{height:3px;background:#e4ddd0;border-radius:2px;overflow:hidden;}',
      '#hld-bar{height:100%;width:8%;background:linear-gradient(90deg,#b8963e,#d4af55);',
      'border-radius:2px;transition:width .9s cubic-bezier(.4,0,.2,1);}',
      '#hld-pct{font-size:.7rem;color:#b0a090;text-align:right;margin-top:7px;letter-spacing:.04em;}',
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
          '<line x1="30" y1="2"  x2="30" y2="14" stroke="#b8963e" stroke-width="1.2"/>' +
          '<line x1="30" y1="46" x2="30" y2="58" stroke="#b8963e" stroke-width="1.2"/>' +
          '<line x1="2"  y1="30" x2="14" y2="30" stroke="#b8963e" stroke-width="1.2"/>' +
          '<line x1="46" y1="30" x2="58" y2="30" stroke="#b8963e" stroke-width="1.2"/>' +
          '<path d="M30 16 L44 30 L30 44 L16 30 Z" stroke="#b8963e" stroke-width="1.2" fill="none"/>' +
          '<circle cx="30" cy="30" r="2.5" fill="#b8963e"/>' +
        '</svg>' +
        '<div class="hld-brand-name">HOPDESK</div>' +
        '<div class="hld-brand-tagline">TREASURY INTELLIGENCE PLATFORM</div>' +
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

  /* 4 -- Message pool */
  var _ALL = [
    { h: 'Conectando con los servidores…',          s: 'El servidor estaba dormido. Lo despertamos con mucho cariño.' },
    { h: 'Negociando con el ERP…',                  s: 'Diciéndole "por favor" al ERP. La educación no cuesta nada.' },
    { h: 'Descargando facturas del año pasado…', s: 'Y del antepasado, por si las dudas.' },
    { h: 'Calculando quién nos debe qué…', s: 'Spoiler: siempre son más de los que esperamos.' },
    { h: 'Sincronizando vencimientos…',              s: 'Sus facturas, ordenadas por fecha y nivel de angustia.' },
    { h: 'Revisando políticas de pago…',        s: 'Política #1: cobrar antes de pagar. La cumplimos a veces.' },
    { h: 'Cargando la agenda de pagos…',             s: '¿Ya le pagaron al proveedor del 304? (No. Nunca.)' },
    { h: 'Verificando tipo de cambio…',              s: 'El peso está bien, gracias por preguntar.' },
    { h: 'Aplicando overrides manuales…',            s: 'Respetando sus decisiones ejecutivas, sin juzgar.' },
    { h: 'Contando los días de vencimiento…',  s: 'Son más de diez. Necesitamos más dedos.' },
    { h: 'Revisando el historial de “como quedamos”…', s: 'Archivo muy pesado. Muchos, muchos correos.' },
    { h: 'Alineando los astros fiscales…',           s: 'El IVA en retención y el SAT en mantenimiento.' },
    { h: 'Calculando el flujo de efectivo…',         s: 'Hay flujo. No necesariamente en la dirección correcta.' },
    { h: 'Revisando intercompañías…',     s: 'Todos se deben entre sí. Es una tradición corporativa.' },
    { h: 'Cargando el forecast del trimestre…',      s: 'Optimista en el presupuesto, realista en la ejecución.' },
    { h: 'Buscando la factura de marzo…',            s: 'Siempre aparece justo después del corte.' },
    { h: 'Cargando proveedores activos…',            s: 'Algunos llevan más tiempo en el catálogo que el café en la cocineta.' }
  ];
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
      if (H.msgT)   clearInterval(H.msgT);
      if (H.creepT) clearInterval(H.creepT);
      if (_bar()) _bar().style.width = '100%';
      if (_pct()) _pct().textContent = '100%';
      var h = _msgEl(), s = _subEl();
      if (h) h.style.opacity = '0';
      if (s) s.style.opacity = '0';
      setTimeout(function() {
        if (h) { h.textContent = '¡HopDesk listo para el rescate!'; h.style.opacity = '1'; }
        if (s) { s.textContent = 'Sus números, a su servicio.';      s.style.opacity = '1'; }
        /* Hard fallback -- calFadeNow should arrive well before 30 s */
        setTimeout(function() {
          if (!H.ov.classList.contains('hld-fadeout')) {
            H.ov.classList.add('hld-fadeout');
            setTimeout(function() { H.ov.style.display = 'none'; }, 950);
          }
        }, 30000);
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
