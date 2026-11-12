// =============================================================================
// www/panorama.js — Panorama (front B). Only presentation: the feed chips,
// the Destacados carousel, the bell's jump and the SAP refresh shortcut.
// Nothing here decides what a user may see: the server sends only their avisos.
// =============================================================================
(function () {
  'use strict';
  var ROTAR_MS = 12000;
  var quieto = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  function mostrar(carrusel, i) {
    var slides = carrusel.querySelectorAll('.pan-destacado');
    var puntos = carrusel.querySelectorAll('.pan-punto-nav');
    if (!slides.length) return;
    i = ((i % slides.length) + slides.length) % slides.length;
    slides.forEach(function (s, k) { s.classList.toggle('activa', k === i); });
    puntos.forEach(function (p, k) {
      p.classList.toggle('activo', k === i);
      p.setAttribute('aria-current', k === i ? 'true' : 'false');
    });
    carrusel.dataset.i = i;
  }

  // One timer for the page; a re-rendered carousel is picked up on the next tick.
  setInterval(function () {
    if (quieto || document.hidden) return;
    document.querySelectorAll('.hd-panorama .pan-carrusel').forEach(function (c) {
      if (c.matches(':hover') || c.contains(document.activeElement)) return;
      if (c.querySelectorAll('.pan-destacado').length < 2) return;
      mostrar(c, (parseInt(c.dataset.i || '0', 10) || 0) + 1);
    });
  }, ROTAR_MS);

  function filtrar(chip) {
    var tarjeta = chip.closest('.pan-feed');
    if (!tarjeta) return;
    var tipo = chip.getAttribute('data-tipo');
    tarjeta.querySelectorAll('.pan-chip').forEach(function (c) {
      c.setAttribute('aria-pressed', c === chip ? 'true' : 'false');
    });
    tarjeta.querySelectorAll('.pan-item').forEach(function (it) {
      it.hidden = !(tipo === 'todo' || it.getAttribute('data-tipo') === tipo);
    });
    tarjeta.dataset.filtro = tipo;
  }

  // Publicar: its three sections (Nueva, Publicadas, Alertas del equipo).
  function seccion(boton) {
    var grupo = boton.closest('.pan-publicar');
    if (!grupo) return;
    grupo.querySelectorAll('.pan-pestana').forEach(function (b) {
      var on = b === boton;
      b.setAttribute('aria-selected', on ? 'true' : 'false');
      var s = document.getElementById(b.getAttribute('data-seccion'));
      if (s) s.hidden = !on;
    });
    // hd_table measures itself when shown.
    $(window).trigger('resize');
  }

  // Title and message counters ("12 / 80").
  function contar(e) {
    var el = e.target;
    if (!el.closest || !el.closest('.hd-panorama')) return;
    var id = el.getAttribute('data-contador');
    if (!id) return;
    var c = document.getElementById(id);
    if (c) c.textContent = el.value.length + ' / ' + el.getAttribute('maxlength');
  }
  document.addEventListener('input', contar);
  document.addEventListener('change', contar);   // emptied by the server after publishing

  document.addEventListener('click', function (e) {
    var pestana = e.target.closest('.hd-panorama .pan-pestana');
    if (pestana) { seccion(pestana); return; }
    var chip = e.target.closest('.hd-panorama .pan-chip');
    if (chip) { filtrar(chip); return; }
    var punto = e.target.closest('.hd-panorama .pan-punto-nav');
    if (punto) mostrar(punto.closest('.pan-carrusel'), parseInt(punto.getAttribute('data-i'), 10) || 0);
  });

  // The feed re-renders every minute: keep the chip the user picked.
  $(document).on('shiny:value', function (e) {
    if (!/feed$/.test(e.name || '')) return;
    var antes = document.getElementById(e.name);
    var filtro = antes && antes.querySelector('.pan-feed') && antes.querySelector('.pan-feed').dataset.filtro;
    if (!filtro || filtro === 'todo') return;
    setTimeout(function () {
      var chip = document.querySelector('#' + CSS.escape(e.name) + ' .pan-chip[data-tipo="' + filtro + '"]');
      if (chip) filtrar(chip);
    }, 0);
  });

  window.hdPanorama = {
    irA: function (id) {
      var el = document.getElementById(id);
      if (el) el.scrollIntoView({ behavior: quieto ? 'auto' : 'smooth', block: 'start' });
    },
    // The app's own refresh button (control bar), so the refresh runs the
    // exact same path as clicking it.
    refrescarSap: function () {
      var b = document.getElementById('btn_refresh');
      if (b) b.click();
    }
  };
})();
