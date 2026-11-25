/* hd_botones.js (T107) -- every button answers the instant it is clicked.
 *
 * Luis: "la app sí lo registró". On the click, in the same frame, the button
 * shows it is working (pressed, a small spinner, disabled so a second click
 * cannot send it twice). It goes back to normal when the server has finished
 * answering (the first shiny:idle after the server started on it).
 *
 * Never a success mark by inference (the Coordinador, T107): an observer
 * that stops in a req() or fails without a word says nothing, and a check
 * there would read "saved" when nothing was. So:
 *   - released on idle: back to NEUTRAL, no mark;
 *   - a check (ok) or a cross (error) ONLY when the server says so:
 *       hd_boton_listo(session, id, ok) in R ("hd-boton" message), or
 *       a Shiny error notification while the button was working (the module
 *       said "error", explicitly) -> cross. A success notification adds no
 *       check: it already says it, and nothing ties it to this button.
 *
 * Which buttons: every .action-button (actionButton, actionLink) and every
 * element with data-hd-espera. data-hd-espera: "auto" (as an actionButton),
 * "servidor" (released only by hd_boton_listo(), for work that ends after the
 * idle, e.g. a callr job), "no" (never held: opening/closing, navigating).
 * Closing buttons (.btn-close, [data-bs-dismiss]) are never held.
 * The list of exceptions and their reasons: HD_BOTONES_SIN_ESPERA in
 * R/ui_components.R; tests/test_t107_botones.R keeps both honest.
 *
 * Loaded on the app page only (app.R), never on the sign-in page.
 */
(function () {
  if (window._hdBotonesCargado) return;
  window._hdBotonesCargado = true;

  var B = window.hdBotones = {
    TOPE: 60000,          /* never stuck: released anyway after this */
    SIN_ESPERA_MS: 1500,  /* a click the server never got busy on: released after this */
    MARCA_MS: { ok: 900, error: 1600 },

    /* the button a click landed on, or null (pure: tests/test_t107_botones.R) */
    objetivo: function (t) {
      var el = t && t.closest ? t.closest('.action-button, [data-hd-espera]') : null;
      if (!el) return null;
      if (el.classList.contains('btn-close') || el.hasAttribute('data-bs-dismiss')) return null;
      return el;
    },
    /* 'auto' | 'servidor' | 'no' */
    modo: function (el) {
      var m = el && el.getAttribute ? el.getAttribute('data-hd-espera') : null;
      return (m === 'servidor' || m === 'no' || m === 'auto') ? m : 'auto';
    },
    /* whether an idle releases a held button: only an 'auto' one, and only
       after the server got busy AFTER its click (an idle from earlier work
       that was still running when it was clicked does not count) */
    sueltaEnIdle: function (est, idleAt) {
      return !!est && est.modo === 'auto' && est.busyAt > 0 && est.busyAt >= est.clicAt && idleAt >= est.busyAt;
    },

    pendientes: [],
    shinyOcupado: false,   /* between a shiny:busy and its shiny:idle */

    ocupar: function (el, ahora) {
      if (!el || el.__hdEst) return false;
      var modo = B.modo(el);
      if (modo === 'no') return false;
      /* clicked while the server is already busy: no new shiny:busy will
         come for it, so the click counts as the start of that busy stretch
         (the first idle releases it -- never later than the real end) */
      el.__hdEst = { modo: modo, clicAt: ahora, busyAt: B.shinyOcupado ? ahora : 0, error: false,
                     estabaDeshabilitado: !!el.disabled, tope: null, sinEspera: null };
      el.classList.add('hd-btn-ocupado');
      el.setAttribute('aria-busy', 'true');
      if ('disabled' in el && el.tagName !== 'A') el.disabled = true;
      else el.classList.add('hd-btn-inerte');            /* a link: no pointer events */
      el.__hdEst.tope = setTimeout(function () { B.soltar(el, null); }, B.TOPE);
      if (modo === 'auto')
        el.__hdEst.sinEspera = setTimeout(function () {
          if (el.__hdEst && !el.__hdEst.busyAt) B.soltar(el, null);
        }, B.SIN_ESPERA_MS);
      B.pendientes.push(el);
      return true;
    },

    /* estado: null (neutral), 'ok' or 'error' -- the last two only when the
       server said so */
    soltar: function (el, estado) {
      var est = el && el.__hdEst;
      if (!est) return;
      clearTimeout(est.tope); clearTimeout(est.sinEspera);
      el.__hdEst = null;
      B.pendientes = B.pendientes.filter(function (x) { return x !== el; });
      el.classList.remove('hd-btn-ocupado', 'hd-btn-inerte');
      el.removeAttribute('aria-busy');
      if ('disabled' in el && el.tagName !== 'A') el.disabled = est.estabaDeshabilitado;
      if (est.error && !estado) estado = 'error';
      if (estado === 'ok' || estado === 'error') {
        var c = 'hd-btn-' + estado;
        el.classList.add(c);
        setTimeout(function () { el.classList.remove(c); }, B.MARCA_MS[estado]);
      }
    }
  };

  /* the click: bubble phase, so Shiny's own binding already took it */
  document.addEventListener('click', function (e) {
    var el = B.objetivo(e.target);
    if (el && !el.disabled) B.ocupar(el, Date.now());
  }, false);

  function conJQuery(f) {
    if (window.jQuery) return f(window.jQuery);
    var t = setInterval(function () { if (window.jQuery) { clearInterval(t); f(window.jQuery); } }, 20);
  }
  conJQuery(function ($) {
    $(document).on('shiny:busy', function () {
      var t = Date.now(); B.shinyOcupado = true;
      B.pendientes.forEach(function (el) { if (el.__hdEst && !el.__hdEst.busyAt) el.__hdEst.busyAt = t; });
    });
    $(document).on('shiny:idle', function () {
      var t = Date.now(); B.shinyOcupado = false;
      B.pendientes.slice().forEach(function (el) { if (B.sueltaEnIdle(el.__hdEst, t)) B.soltar(el, null); });
    });
  });

  /* an error notification while a button works: the module said "error" */
  function vigilarAvisos() {
    var p = document.getElementById('shiny-notification-panel') || document.body;
    new MutationObserver(function (ms) {
      ms.forEach(function (m) {
        Array.prototype.forEach.call(m.addedNodes || [], function (n) {
          if (n.classList && n.classList.contains('shiny-notification-error'))
            B.pendientes.forEach(function (el) { if (el.__hdEst) el.__hdEst.error = true; });
        });
      });
    }).observe(p, { childList: true, subtree: true });
  }
  if (document.body) vigilarAvisos(); else document.addEventListener('DOMContentLoaded', vigilarAvisos);

  /* hd_boton_listo(session, id, ok): the server's own word */
  (function registrar() {
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('hd-boton', function (m) {
        var el = m && m.id ? document.getElementById(m.id) : null;
        if (el) B.soltar(el, m.ok === true ? 'ok' : (m.ok === false ? 'error' : null));
      });
    } else setTimeout(registrar, 20);
  })();
})();
