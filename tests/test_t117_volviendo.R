# =============================================================================
# tests/test_t117_volviendo.R
# T117: going back home, the header and the ☰ card say "Volviendo a casa…" at
# once (the script in app.R's ui, next to uiOutput("navbar_user_badge")).
#   A. The script as the page gets it: tags$script(HTML()), nothing escaped,
#      home is .HD_ADMIN_CID, the dictionary key in both languages.
#   B. The script run in node with a fake page:
#      - going home from inside a client: both get the sign;
#      - a jump INTO a client: no sign (contraprueba);
#      - already home (no "ver como" selector: nothing will change, so no
#        new value would come to clear it): no sign (contraprueba);
#      - each sign leaves with its output's shiny:value or shiny:error;
#      - and after 30 s whatever happens (the server never answers).
# Run from project root:  Rscript --vanilla tests/test_t117_volviendo.R
# =============================================================================
cat("=== test_t117_volviendo ===\n\n")
suppressPackageStartupMessages(library(htmltools))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.HD_ADMIN_CID <- local({ e <- new.env(); txt <- readLines("R/persistence.R", warn = FALSE, encoding = "UTF-8")
  eval(parse(text = grep("^\\.HD_ADMIN_CID <- ", txt, value = TRUE)), e); e$.HD_ADMIN_CID })

# ── A. The script as the page gets it ───────────────────────────────────────
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
i0 <- grep("^    tags\\$style\\(HTML\\(\"\\.hd-ctx-volviendo", app)
i1 <- grep("^\\}\\)\\(\\);\", \\.HD_ADMIN_CID\\)\\)\\)$", app)
ok("A.1 app.R has the style and the script, once, next to uiOutput(\"navbar_user_badge\")",
   length(i0) == 1L && length(i1) == 1L && i1 > i0 && any(grepl("uiOutput(\"navbar_user_badge\")", app[(i0 - 12L):i0], fixed = TRUE)))
txt <- paste(app[i0:i1], collapse = "\n")
partes <- eval(parse(text = paste0("list(", sub(",\\s*$", "", txt), ")")))
estilo <- as.character(partes[[1]]); script <- as.character(partes[[2]])
js <- sub("^<script>", "", sub("</script>$", "", script))
ok("A.2 tags$script(HTML()): what reaches the page is the code itself, nothing escaped",
   grepl("^\\(function\\(\\)\\{", js) && !grepl("&amp;|&lt;|&gt;", script) && !grepl("&amp;|&lt;|&gt;", estilo))
ok("A.3 home is .HD_ADMIN_CID (no client id written by hand)",
   grepl(sprintf("var CASA = '%s'", .HD_ADMIN_CID), js, fixed = TRUE) && !grepl("'hd-admin'", paste(app[i0:i1], collapse = "\n"), fixed = TRUE))
ok("A.4 the text comes from the dictionary (hdT), in es and en",
   grepl("hdT('niveles.ctx.volviendo')", js, fixed = TRUE) &&
     all(vapply(c("es", "en"), function(l) nzchar(jsonlite::fromJSON(file.path("www/i18n", paste0(l, ".json")))[["niveles.ctx.volviendo"]] %||% ""), logical(1))))

# ── B. In node, with a fake page ────────────────────────────────────────────
node <- Sys.which("node")
if (!nzchar(node)) {
  cat("SKIP (B): node no está instalado\n")
} else {
  correr <- function(destino, en_cliente, despues) {
    pagina <- sprintf("
var handlers = {}, timers = [], now = 0;
function El(id) { this.id = id; this.attrs = {}; var c = {}; this.classList = {
  add: function(k) { c[k] = true; }, remove: function(k) { delete c[k]; }, contains: function(k) { return !!c[k]; } }; }
El.prototype.setAttribute = function(k, v) { this.attrs[k] = v; };
El.prototype.removeAttribute = function(k) { delete this.attrs[k]; };
var els = { 'trs-header_badge': new El('trs-header_badge'), 'navbar_user_badge': new El('navbar_user_badge'),
            'trs-modal_ctx_select': { selectize: { getValue: function() { return %s; } } } };
if (%s) els['ver_como_sel'] = new El('ver_como_sel');
var document = { getElementById: function(id) { return els[id] || null; } };
var window = { hdT: function(k) { return 'T:' + k; } };
var seq = 0;
function setTimeout(f, ms) { seq += 1; timers.push({ f: f, at: now + ms, id: seq }); return seq; }
function clearTimeout(id) { timers = timers.filter(function(t) { return t.id !== id; }); }
function $(x) { return { on: function(ev, a, b) { ev.split(' ').forEach(function(e) {
  handlers[e] = handlers[e] || []; handlers[e].push(b ? { sel: a, f: b } : { sel: null, f: a }); }); } }; }
function disparar(ev, target, sel) { (handlers[ev] || []).forEach(function(h) {
  if (h.sel === null || h.sel === sel) h.f({ target: target }); }); }
function avanzar(ms) { now += ms; var v = timers.filter(function(t) { return t.at <= now; });
  timers = timers.filter(function(t) { return t.at > now; }); v.forEach(function(t) { t.f(); }); }
function estado() { return ['trs-header_badge', 'navbar_user_badge'].map(function(id) {
  var e = els[id]; return e.classList.contains('hd-ctx-volviendo') ? (e.attrs['data-hd-volviendo'] || '?') : ''; }); }
", jsonlite::toJSON(destino, auto_unbox = TRUE), if (en_cliente) "true" else "false")
    prog <- paste(pagina, js, "var r = {};",
                  "disparar('click', null, '#trs-modal_ctx_confirm'); r.clic = estado();",
                  despues, "console.log(JSON.stringify(r));", sep = "\n")
    f <- tempfile(fileext = ".js"); on.exit(unlink(f)); writeLines(prog, f, useBytes = TRUE)
    jsonlite::fromJSON(paste(system2(node, f, stdout = TRUE), collapse = ""))
  }
  S <- "T:niveles.ctx.volviendo"
  r <- correr(.HD_ADMIN_CID, TRUE, "avanzar(9900); r.antes = estado();
    disparar('shiny:value', els['navbar_user_badge']); r.tarjeta = estado();
    disparar('shiny:error', els['trs-header_badge']); r.ambos = estado();")
  ok("B.1 going home from a client: both say it at once, from the dictionary", identical(r$clic, c(S, S)))
  ok("B.2 ...and keep saying it while the server works (9.9 s, nothing new yet)", identical(r$antes, c(S, S)))
  ok("B.3 the card's new value clears the card's sign only", identical(r$tarjeta, c(S, "")))
  ok("B.4 an error on the header's output clears its sign too", identical(r$ambos, c("", "")))
  r <- correr(.HD_ADMIN_CID, TRUE, "avanzar(29999); r.casi = estado(); avanzar(1); r.tope = estado();")
  ok("B.5 the server never answers: the sign is still there at 29.999 s...", identical(r$casi, c(S, S)))
  ok("B.6 ...and gone at 30 s, whatever happens", identical(r$tope, c("", "")))
  r <- correr("acme", TRUE, "")
  ok("B.7 contraprueba: a jump INTO a client shows no sign", identical(r$clic, c("", "")))
  r <- correr(.HD_ADMIN_CID, FALSE, "")
  ok("B.8 contraprueba: already home (no 'ver como', nothing will change): no sign", identical(r$clic, c("", "")))
  r <- correr(.HD_ADMIN_CID, TRUE, "avanzar(20000); disparar('click', null, '#trs-modal_ctx_confirm'); avanzar(20000);
    r.doble = estado(); avanzar(10000); r.doble2 = estado();")
  ok("B.9 a second click restarts the 30 s and leaves no stray timer behind", identical(r$doble, c(S, S)) && identical(r$doble2, c("", "")))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
