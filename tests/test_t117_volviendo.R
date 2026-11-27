# =============================================================================
# tests/test_t117_volviendo.R
# T117: going back home, the header and the ☰ card say "Volviendo a casa…" at
# once (a script in the staff-only part of output$navbar_user_badge).
#   A. Only a staff page inside a client carries it (M1): app.R's ui never
#      names home nor carries the logic (contraprueba: db8ac981 did); the
#      card's render as staff in / at home / a client; HTML(); dictionary.
#   B. The script run in node with a fake page:
#      - going home from inside a client: both get the sign;
#      - a jump INTO a client: no sign (contraprueba);
#      - already home (no "ver como" selector: nothing will change, so no
#        new value would come to clear it): no sign (contraprueba);
#      - each sign leaves with its output's shiny:value or shiny:error;
#      - and after 30 s whatever happens (the server never answers);
#      - run again by a re-render, it binds once.
#   C. The header's label at home is a name, never the id (tiers_ctx_etiqueta).
# Run from project root:  Rscript --vanilla tests/test_t117_volviendo.R
# =============================================================================
cat("=== test_t117_volviendo ===\n\n")
suppressPackageStartupMessages({ library(htmltools); library(shiny) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.HD_ADMIN_CID <- local({ e <- new.env(); txt <- readLines("R/persistence.R", warn = FALSE, encoding = "UTF-8")
  eval(parse(text = grep("^\\.HD_ADMIN_CID <- ", txt, value = TRUE)), e); e$.HD_ADMIN_CID })

# ── A. Only a staff page carries it (M1) ────────────────────────────────────
for (f in c("R/i18n.R", "R/tier_registry.R")) suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")
TEXTO <- tr("niveles.ctx.volviendo")
# The page every session gets: app.R before the server. It never names home
# nor carries this logic; db8ac981 (the first T117) did -- the contraprueba.
ui_de <- function(txt) { k <- which(startsWith(txt, "server <- function(input, output, session)"))[1]; paste(txt[seq_len(k - 1L)], collapse = "\n") }
limpia <- function(t) !any(vapply(c("HD_ADMIN_CID", "hd-admin", "niveles.ctx.volviendo", "hd-ctx-volviendo", "trs-modal_ctx_confirm"),
                                  function(p) grepl(p, t, fixed = TRUE), logical(1)))
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("A.1 the page every session gets (app.R's ui) has no home id and no going-home logic", limpia(ui_de(app)))
viejo <- tryCatch(system2("git", c("show", "db8ac981:app.R"), stdout = TRUE, stderr = FALSE), error = function(e) character())
ok("A.2 contraprueba: db8ac981's ui did carry them (the check sees the finding)", length(viejo) > 100L && !limpia(ui_de(viejo)))

# The card's render, run in a session as each kind of user.
i0 <- which(app == "  output$navbar_user_badge <- renderUI({")
i1 <- i0 - 1L + which(app[i0:length(app)] == "  })")[1]
render <- parse(text = app[i0:i1], keep.source = FALSE)
tarjeta <- function(info, salto) {
  html <- NULL
  testServer(function(input, output, session) {
    shared <- list(current_user_info = function() info)
    jump_client_id <- function() salto
    tier_efectivo <- function() info$tier
    eval(render[[1]])
  }, { html <<- as.character(output$navbar_user_badge$html) })
  html
}
staff_info <- list(user = "s", name = "S", tier = nivel_tope(), is_staff = TRUE)
cli_info <- list(user = "c", name = "C", tier = niveles_cliente()[1], is_staff = FALSE)
h_dentro <- tarjeta(staff_info, "acme"); h_casa <- tarjeta(staff_info, NULL); h_cli <- tarjeta(cli_info, NULL)
ok("A.3 staff inside a client: the card carries the script, with home's id and the dictionary's text",
   grepl("trs-modal_ctx_confirm", h_dentro, fixed = TRUE) && grepl(sprintf("var CASA = \"%s\"", .HD_ADMIN_CID), h_dentro, fixed = TRUE) &&
     grepl(TEXTO, h_dentro, fixed = TRUE))
ok("A.4 a client's card: none of it (contraprueba)", nzchar(h_cli) && limpia(h_cli) && !grepl(TEXTO, h_cli, fixed = TRUE))
ok("A.5 staff at home: none of it either (nothing to go back from)", nzchar(h_casa) && limpia(h_casa))
js <- sub("</script>.*$", "", sub("^.*?<script>", "", h_dentro))
ok("A.6 HTML(): the script reaches the page as code, nothing escaped",
   startsWith(js, "(function(){") && grepl("> *{display:none", js, fixed = TRUE) && !grepl("&amp;|&lt;|&gt;", js))
ok("A.7 the text is in the dictionary in es and en",
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
var estilos = [];
var document = { getElementById: function(id) { return els[id] || null; },
  createElement: function(t) { return { tag: t }; }, head: { appendChild: function(e) { estilos.push(e); } } };
var window = {};
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
  S <- TEXTO
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
  # The card re-renders (each "ver como", each jump): the script runs again.
  r <- correr(.HD_ADMIN_CID, TRUE, paste(js, "r.manejadores = (handlers['click'] || []).length; r.estilos = estilos.length;", sep = ";"))
  ok("B.10 run again by a re-render: still one click handler and one style in <head>", r$manejadores == 1L && r$estilos == 1L)
}

# ── C. The header's label ───────────────────────────────────────────────────
# Just the helper, from the module's own text (sourcing the module pulls the
# whole app).
local({
  tm <- readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8")
  a <- which(startsWith(tm, "tiers_ctx_etiqueta <- function(")); b <- a - 1L + which(tm[a:length(tm)] == "}")[1]
  eval(parse(text = tm[a:b]), globalenv())
})
g <- data.frame(client_id = c("acme", "hd-x"), name = c("Acme SA", ""), stringsAsFactors = FALSE)
ok("C.1 a client with a group name: the name", identical(tiers_ctx_etiqueta(g, "acme"), "Acme SA"))
ok("C.2 home with no name in the data: the dictionary's words, never the id",
   identical(tiers_ctx_etiqueta(g, .HD_ADMIN_CID), tr("niveles.ctx.casa")) && !grepl(.HD_ADMIN_CID, tr("niveles.ctx.casa"), fixed = TRUE))
ok("C.3 home WITH a name in the data: the data wins",
   identical(tiers_ctx_etiqueta(rbind(g, data.frame(client_id = .HD_ADMIN_CID, name = "Casa HD")), .HD_ADMIN_CID), "Casa HD"))
ok("C.4 another client without a name keeps its id (as before); no data at all is no error",
   identical(tiers_ctx_etiqueta(g, "beta"), "beta") && identical(tiers_ctx_etiqueta(NULL, "beta"), "beta") &&
     identical(tiers_ctx_etiqueta(NULL, .HD_ADMIN_CID), tr("niveles.ctx.casa")))
ok("C.5 the header uses it", any(grepl("ctx_label  <- tiers_ctx_etiqueta(grps, active_cid)", readLines("R/tiers_module.R", warn = FALSE), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
