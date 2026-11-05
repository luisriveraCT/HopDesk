// Requisiciones (front E): evidence by paste, drag or choose.
//
// One upload path for every file: a pasted image or dropped files are put
// into the requisition's hidden Shiny fileInput, so they travel by Shiny's
// normal HTTP upload (never as base64 over the websocket). The server
// (R/adjuntos_s3.R) decides what each file is by its bytes and re-encodes
// images; nothing here is trusted beyond a size check that saves an upload.
//
// Pasted cells: only the clipboard's text/plain (Excel's tab-separated text)
// is read, never text/html. The server turns it into a table.
//
// Usage (from R/requisiciones_module.R):
//   HDReqPaste.bind({ zona: "<id>", archivo: "<fileInput id>",
//                     texto: "<input id>", origen: "<input id>",
//                     error: "<input id>" });
(function() {
  var MAX_PEGADO  = 4 * 1024 * 1024;
  var MAX_ARCHIVO = 10 * 1024 * 1024;
  var MAX_TEXTO   = 2000000;
  var TIPOS_IMAGEN = ['image/png', 'image/jpeg', 'image/webp'];

  function avisar(ids, clave, vars) {
    if (window.Shiny && ids.error)
      Shiny.setInputValue(ids.error, { mensaje: hdT(clave, vars), t: Date.now() }, { priority: 'event' });
  }

  // Hand files to the Shiny fileInput as if they had been chosen there.
  function subir(ids, archivos, origen) {
    var input = document.getElementById(ids.archivo);
    if (!input || !archivos.length) return;
    var limite = origen === 'pegado' ? MAX_PEGADO : MAX_ARCHIVO;
    var dt = new DataTransfer();
    for (var i = 0; i < archivos.length; i++) {
      if (archivos[i].size > limite) {
        avisar(ids, 'requisiciones.evidencia.error_tamano', { mb: limite / 1024 / 1024 });
        continue;
      }
      dt.items.add(archivos[i]);
    }
    if (!dt.files.length) return;
    if (window.Shiny && ids.origen)
      Shiny.setInputValue(ids.origen, origen, { priority: 'event' });
    input.files = dt.files;
    input.dispatchEvent(new Event('change', { bubbles: true }));
  }

  function alPegar(ids, e) {
    var cd = e.clipboardData;
    if (!cd) return;
    var imagenes = [];
    for (var i = 0; i < (cd.items || []).length; i++) {
      var it = cd.items[i];
      if (it.kind === 'file' && TIPOS_IMAGEN.indexOf(it.type) >= 0) {
        var f = it.getAsFile();
        if (f) imagenes.push(f);
      }
    }
    // A screenshot pasted from Excel also carries text; the image wins only
    // when there is no tab-separated text, so pasting cells gives a table.
    var texto = cd.getData('text/plain') || '';
    var esTabla = texto.indexOf('\t') >= 0;
    if (imagenes.length && !esTabla) {
      e.preventDefault();
      subir(ids, imagenes.slice(0, 1), 'pegado');
      return;
    }
    if (!texto) return;
    e.preventDefault();
    if (texto.length > MAX_TEXTO) { avisar(ids, 'requisiciones.tabla.error_grande'); return; }
    if (window.Shiny && ids.texto)
      Shiny.setInputValue(ids.texto, { texto: texto, t: Date.now() }, { priority: 'event' });
  }

  // The zone may not be in the page yet (a modal still opening): try again
  // for a couple of seconds.
  function bind(ids, intento) {
    var zona = document.getElementById(ids.zona);
    if (!zona) {
      if ((intento || 0) < 20) setTimeout(function() { bind(ids, (intento || 0) + 1); }, 100);
      return;
    }
    if (zona.getAttribute('data-hdreq') === '1') return;
    zona.setAttribute('data-hdreq', '1');
    // Chosen with the file dialog: the 10 MB limit, never the paste one.
    var input = document.getElementById(ids.archivo);
    if (input) input.addEventListener('click', function() {
      if (window.Shiny && ids.origen) Shiny.setInputValue(ids.origen, 'archivo', { priority: 'event' });
    });
    if (!zona.hasAttribute('tabindex')) zona.setAttribute('tabindex', '0');
    zona.addEventListener('paste', function(e) { alPegar(ids, e); });
    zona.addEventListener('dragover', function(e) {
      e.preventDefault(); zona.classList.add('hdreq-arrastrando');
    });
    zona.addEventListener('dragleave', function() { zona.classList.remove('hdreq-arrastrando'); });
    zona.addEventListener('drop', function(e) {
      e.preventDefault(); zona.classList.remove('hdreq-arrastrando');
      var fs = e.dataTransfer && e.dataTransfer.files ? Array.prototype.slice.call(e.dataTransfer.files) : [];
      subir(ids, fs, 'archivo');
    });
  }

  window.HDReqPaste = { bind: bind };

  // A download the server prepared (evidence PDF/XLSX/image): it clicks the
  // module's hidden download link once the bytes are ready on the server.
  function descargar(msg) {
    var a = document.getElementById(msg.id);
    if (a && a.href && a.getAttribute('href') !== '') a.click();
  }

  // The screen asks from R: session$sendCustomMessage("hdreq_bind", ids)
  function registrar() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler || window.__hdreqRegistrado) return !!window.__hdreqRegistrado;
    window.__hdreqRegistrado = true;
    Shiny.addCustomMessageHandler('hdreq_bind', function(ids) { setTimeout(function() { bind(ids, 0); }, 0); });
    Shiny.addCustomMessageHandler('hdreq_descargar', descargar);
    return true;
  }
  if (!registrar()) document.addEventListener('DOMContentLoaded', registrar);
})();
