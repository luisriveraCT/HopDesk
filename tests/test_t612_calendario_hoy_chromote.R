# =============================================================================
# tests/test_t612_calendario_hoy_chromote.R
# T6-12 in the real app (tests/helpers/arranque_app.R, dev bucket, no
# writes), at Luis's window (1293 x 632), once per theme (Clásico, Marca,
# forced with ARR_FORZAR_TEMA in the app's process only) and per user
# (staff "prueba", client): Flujo › Calendario, the ledger it opens on.
#   - exactly one tile is today, and it is the BROWSER's day (CM27);
#   - its number says title "Hoy" and aria-current="date";
#   - the number is a filled pill in the calendar's colour (Cobros primario,
#     Pagos modo-pago, as the theme resolves them), weight 800, text >= 4.5;
#   - the tile has the ring (box-shadow in that colour), >= 3 on its ground;
#   - the pill fits inside its tile.
# A view the user does not have, or a month without today, is NOT COVERED.
# Screenshots of each calendar go to TEMA_SALIDA.
# Run from project root:  Rscript tests/test_t612_calendario_hoy_chromote.R
# =============================================================================
cat("=== test_t612_calendario_hoy_chromote ===\n\n")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
if (nzchar(Sys.getenv("HOPDESK_PRODUCTION"))) stop("HOPDESK_PRODUCTION is set: this test runs on dev only")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
usuarios <- list(
  prueba  = c(user = Sys.getenv("HOPDESK_TEST_USER"),         pass = Sys.getenv("HOPDESK_TEST_PASS")),
  cliente = c(user = Sys.getenv("HOPDESK_TEST_CLIENTE_USER"), pass = Sys.getenv("HOPDESK_TEST_CLIENTE_PASS")))
quienes <- strsplit(Sys.getenv("TEMA_USUARIOS", "prueba,cliente"), ",")[[1]]
if (!all(vapply(usuarios[quienes], function(u) all(nzchar(u)), logical(1)))) {
  cat("SKIP: faltan las credenciales de prueba.\n     Esta prueba NO paso: no se corrio.\n"); quit(status = 2L) }
SALIDA <- Sys.getenv("TEMA_SALIDA", file.path(tempdir(), "t612_calendario_hoy"))
dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
unlink(list.files(SALIDA, pattern = "\\.png$", full.names = TRUE))
LIMITE <- as.numeric(Sys.getenv("ARR_LIMITE", "300"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}
NO_CUBIERTAS <- character()
no_cubierta <- function(linea) { cat("      | NOT COVERED", linea, "\n"); NO_CUBIERTAS <<- c(NO_CUBIERTAS, linea) }

tema <- new.env(); sys.source("R/tema_registro.R", envir = tema); sys.source("R/tema_contraste.R", envir = tema)
a_hex <- function(rgb) {   # "rgb(r, g, b)" / "rgba(...)" -> "#RRGGBB"
  v <- as.integer(regmatches(rgb, gregexpr("[0-9]+", rgb))[[1]][1:3]); sprintf("#%02X%02X%02X", v[1], v[2], v[3]) }

source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39062L
app <- NULL; b <- NULL
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
cargada <- function() isTRUE(tryCatch(ev(
  "(function(){ var f = document.getElementById('auth-user_id'), o = document.getElementById('hopdesk-loading');
     return (!f || f.offsetParent === null) && !!document.querySelector('.nav-link[data-value]') &&
            (!o || o.style.display === 'none' || o.classList.contains('hld-fadeout')); })()"), error = function(e) FALSE))
tema_de <- function() tryCatch(ev("document.documentElement.getAttribute('data-hd-tema') || ''"), error = function(e) "?")
captura <- function(nombre) tryCatch({
  s <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(s$data), file.path(SALIDA, paste0(nombre, ".png")))
}, error = function(e) NULL)

MEDIR_JS <- "(function(){
  var t = document.querySelectorAll('.cal-tile.cal-tile--today'), out = {n: t.length};
  var vis = Array.prototype.filter.call(t, function(e){ return e.offsetParent !== null; });
  out.visibles = vis.length;
  var d = new Date(); out.hoy_nav = d.getFullYear() + '-' + (d.getMonth() + 1) + '-' + d.getDate();
  out.periodo = (document.querySelector('.cal-period-label') || {}).textContent || '';
  if (!vis.length) return JSON.stringify(out);
  var tile = vis[0], num = tile.querySelector('.cal-daynum'), cs = getComputedStyle(num), ct = getComputedStyle(tile);
  var cal = tile.closest('.cal-theme-ap') ? 'ap' : (tile.closest('.cal-theme-ar') ? 'ar' : '?');
  var raiz = getComputedStyle(document.documentElement);
  var rt = tile.getBoundingClientRect(), rn = num.getBoundingClientRect();
  out.cal = cal; out.dia = num.textContent.trim(); out.title = num.getAttribute('title') || '';
  out.aria = num.getAttribute('aria-current') || '';
  out.aria_n = document.querySelectorAll('[aria-current=\"date\"]').length;
  out.pill_bg = cs.backgroundColor; out.pill_fg = cs.color; out.peso = cs.fontWeight;
  out.sombra = ct.boxShadow; out.fondo = ct.backgroundColor;
  out.token = raiz.getPropertyValue(cal === 'ap' ? '--hd-modo-pago' : '--hd-primario').trim();
  out.dentro = rn.left >= rt.left && rn.right <= rt.right && rn.top >= rt.top && rn.bottom <= rt.bottom;
  out.ancho_tile = rt.width; out.alto_num = rn.height;
  tile.scrollIntoView({block: 'center'});
  return JSON.stringify(out); })()"

medir <- function(nombre_tema, quien) {
  u <- usuarios[[quien]]
  try(b$close(), silent = TRUE)
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE))
  for (k in 1:2) if (!inherits(tryCatch(b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT), wait_ = TRUE, timeout_ = 120),
                                        error = function(e) e), "error")) break
  poll_until(function() isTRUE(tryCatch(ev("!!document.getElementById('auth-user_id')"), error = function(e) FALSE)), timeout_s = 120)
  Sys.sleep(1)
  ev(sprintf("(function(){ var u = document.getElementById('auth-user_id'), p = document.getElementById('auth-user_pwd');
     u.value = %s; p.value = %s; $(u).trigger('change'); $(p).trigger('change'); return true; })()",
     jsonlite::toJSON(u[["user"]], auto_unbox = TRUE), jsonlite::toJSON(u[["pass"]], auto_unbox = TRUE)))
  Sys.sleep(0.5); ev("document.getElementById('auth-go_auth').click(); true")
  et <- sprintf("[%s · %s]", nombre_tema, quien)
  if (!ok(sprintf("%s signed in, the page loaded in its theme", et),
          poll_until(cargada, timeout_s = LIMITE, interval_s = 1) &&
            poll_until(function() identical(tema_de(), nombre_tema), timeout_s = LIMITE, interval_s = 0.5))) return(invisible())
  # The Calendar tab: at 1293 the navbar is behind ☰; Bootstrap's own tab show.
  abre <- poll_until(function() isTRUE(tryCatch(ev("(function(){
      var a = document.querySelector(\".nav-link[data-value='CAL']\"); if (!a) return false;
      bootstrap.Tab.getOrCreateInstance(a).show(); return true; })()"), error = function(e) FALSE)), timeout_s = 60, interval_s = 1)
  if (!abre) { no_cubierta(sprintf("%s Calendario: no CAL tab for this user", et)); return(invisible()) }
  dibujado <- poll_until(function() isTRUE(tryCatch(ev("(function(){ var t = document.querySelector('.tab-pane.active .cal-tile');
      return !!t && t.offsetParent !== null; })()"), error = function(e) FALSE)), timeout_s = LIMITE, interval_s = 1)
  # The Calendar's own pane: a user whose menu has no Calendario (staff at
  # HopDesk · casa since T117) never gets it shown -- NOT COVERED, not a FAIL.
  pane_activo <- isTRUE(tryCatch(ev("(function(){
      var a = document.querySelector(\".nav-link[data-value='CAL']\"); if (!a) return false;
      var id = (a.getAttribute('data-bs-target') || a.getAttribute('href') || '').replace(/^#/, '');
      var p = id && document.getElementById(id); return !!p && p.classList.contains('active') && p.getClientRects().length > 0; })()"),
    error = function(e) FALSE))
  if (!dibujado && !pane_activo) {
    captura(sprintf("%s_%s_sin_calendario", nombre_tema, quien))
    no_cubierta(sprintf("%s Calendario: the tab does not open for this user (staff at HopDesk · casa has no Calendario)", et))
    return(invisible())
  }
  if (!dibujado) {
    # What the tab shows instead (first T6-12 run: the staff user, no tiles).
    captura(sprintf("%s_%s_calendario_sin_casillas", nombre_tema, quien))
    txt <- tryCatch(ev("(function(){ var p = document.querySelector('.tab-pane.active'); return p ? p.innerText.replace(/\\s+/g,' ').slice(0, 300) : '(no active pane)'; })()"),
                    error = function(e) "?")
    cat("      | active tab says:", txt, "\n")
  }
  if (!ok(sprintf("%s Calendario: its tiles drew", et), dibujado)) return(invisible())
  Sys.sleep(3)
  m <- jsonlite::fromJSON(ev(MEDIR_JS))
  Sys.sleep(1); captura(sprintf("%s_%s_calendario", nombre_tema, quien))
  if (!m$visibles) { no_cubierta(sprintf("%s Calendario (%s): no today tile in the month shown", et, m$periodo)); return(invisible()) }
  hoy <- as.integer(strsplit(m$hoy_nav, "-")[[1]])
  ok(sprintf("%s exactly one tile is today, and it is the browser's day (%s; shown %s)", et, m$hoy_nav, m$dia),
     m$n == 1L && identical(as.integer(m$dia), hoy[3]))
  ok(sprintf("%s its number: title \"%s\", aria-current \"%s\" (the only one on the page: %d)", et, m$title, m$aria, m$aria_n),
     identical(m$title, "Hoy") && identical(m$aria, "date") && m$aria_n == 1L)
  bg <- a_hex(m$pill_bg); fg <- a_hex(m$pill_fg); tk <- toupper(m$token)
  ok(sprintf("%s the pill is the %s calendar's colour (%s = token %s), weight %s", et, m$cal, bg, tk, m$peso),
     identical(bg, tk) && as.integer(m$peso) >= 800L)
  ok(sprintf("%s pill text %s on %s: %.2f (>= 4.5)", et, fg, bg, tema$tema_contraste(fg, bg)), tema$tema_contraste(fg, bg) >= 4.5)
  anillo <- regmatches(m$sombra, regexpr("rgba?\\([^)]*\\)", m$sombra))
  fondo <- a_hex(m$fondo)
  ok(sprintf("%s ring %s in the token, on the tile's ground %s: %.2f (>= 3)", et, m$sombra, fondo,
             if (length(anillo)) tema$tema_contraste(a_hex(anillo), fondo) else 0),
     length(anillo) == 1L && identical(a_hex(anillo), tk) && tema$tema_contraste(a_hex(anillo), fondo) >= 3)
  ok(sprintf("%s the pill fits inside its tile at 1293 (tile %.0f px wide, number %.0f px high)", et, m$ancho_tile, m$alto_num),
     isTRUE(m$dentro))
}

for (nombre_tema in strsplit(Sys.getenv("TEMA_TEMAS", "clasico,marca"), ",")[[1]]) {
  cat(sprintf("\n── %s ──\n", nombre_tema))
  codigo <- sprintf(paste0('Sys.setenv(ARR_MODO = "normal", ARR_RAIZ = "%s", ARR_FORZAR_TEMA = "%s", ',
                           'ARR_FORZAR_TEMA_CLIENTE = "[^/]+"); setwd("%s"); source("tests/helpers/arranque_app.R")$value'),
                    repo, nombre_tema, repo)
  app <- tryCatch(start_bg_app(codigo, PORT, timeout_s = 300, www = file.path(repo, "www")),
                  error = function(e) { message(conditionMessage(e)); NULL })
  if (ok(sprintf("[%s] the real app starts on dev, every client read as %s", nombre_tema, nombre_tema), !is.null(app)))
    for (quien in quienes) tryCatch(medir(nombre_tema, quien), error = function(e) ok(sprintf("[%s · %s] %s", nombre_tema, quien, conditionMessage(e)), FALSE))
  try(b$close(), silent = TRUE); b <- NULL
  stop_bg_app(app); app <- NULL
}
cat(sprintf("\nVistas NO CUBIERTAS: %d\n", length(NO_CUBIERTAS)))
for (x in NO_CUBIERTAS) cat("  NOT COVERED", x, "\n")
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
quit(status = if (.fail) 1L else 0L)
