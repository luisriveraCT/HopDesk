// =============================================================================
// www/requisiciones_carpetas.js
// T61: folding of the Requisiciones folder tree (R/requisiciones_module.R,
// output$carpetas). A convenience of this browser only: localStorage key
// "hdreq.plegadas.<client>" holds the ids of the folded folders, never their
// names. If storage is blocked or empty, everything shows unfolded.
// =============================================================================
(function () {
  function llave(arbol) { return 'hdreq.plegadas.' + (arbol.getAttribute('data-cliente') || ''); }
  function leer(k) {
    try { var v = JSON.parse(window.localStorage.getItem(k) || '[]'); return Array.isArray(v) ? v : []; }
    catch (e) { return []; }
  }
  function guardar(k, v) { try { window.localStorage.setItem(k, JSON.stringify(v)); } catch (e) { /* unfolded next time */ } }

  window.hdreqPlegar = function (btn) {
    var li = btn.closest('li[data-carpeta]'), arbol = btn.closest('ul[data-cliente]');
    if (!li || !arbol) return;
    var k = llave(arbol), id = li.getAttribute('data-carpeta');
    var plegada = li.classList.toggle('plegada');
    btn.setAttribute('aria-expanded', plegada ? 'false' : 'true');
    var v = leer(k).filter(function (x) { return x !== id; });
    if (plegada) v.push(id);
    guardar(k, v);
  };

  function aplicar() {
    document.querySelectorAll('ul.hdreq-arbol[data-cliente]').forEach(function (arbol) {
      var v = leer(llave(arbol));
      arbol.querySelectorAll('li.con-hijas[data-carpeta]').forEach(function (li) {
        if (v.indexOf(li.getAttribute('data-carpeta')) < 0) return;
        li.classList.add('plegada');
        var b = li.querySelector('.hdreq-plegar');
        if (b) b.setAttribute('aria-expanded', 'false');
      });
    });
  }
  if (window.jQuery) {
    window.jQuery(document).on('shiny:value', function (e) {
      if (e.name && /carpetas$/.test(e.name)) window.setTimeout(aplicar, 0);
    });
  }
})();
