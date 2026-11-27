# =============================================================================
# tests/test_t118_diccionario_audiencia.R
# T118: the page carries only the dictionary keys its JavaScript asks for
# (R/i18n.R i18n_claves_navegador()); a table's own keys travel with its data
# (R/hd_table.R -> www/hdtable.js). Luis's rule: a client must not even learn
# about the staff's tools, and the texts describe them.
#   a. i18n_head(): exactly the browser's keys, no niveles.* / permisos.*;
#      es and en. Contraprueba: 706a0eff carried the whole dictionary.
#   b. Every hdT/hdTn/t/tn call of the page's JavaScript (www/*.js and the JS
#      in R/ and app.R) is either a key in that set or a known call site that
#      gets its key another way. A new site fails here until it says how.
#   c. hd_table in a session: the data message carries the table's counter,
#      confirmation and pinned-label words; nothing of staff for a client
#      table; the engine merges them (hdtable.js).
#   d. (Chrome, E3's turn: no raw key visible as client and as staff.)
# Run from project root:  Rscript --vanilla tests/test_t118_diccionario_audiencia.R
# =============================================================================
cat("=== test_t118_diccionario_audiencia ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
claves_head <- function(h) {
  # Both forms: merged (T118) and the old replacing one (the contraprueba).
  h <- as.character(h)
  js <- if (grepl("window.HD_I18N=Object.assign(", h, fixed = TRUE))
    sub("[)];</script>.*$", "", sub("^.*?window[.]HD_I18N=Object[.]assign[(]window[.]HD_I18N[|][|][{][}],", "", h))
  else sub(";</script>.*$", "", sub("^.*?window[.]HD_I18N=", "", h))
  names(jsonlite::fromJSON(gsub("<\\/", "</", js, fixed = TRUE), simplifyVector = FALSE))
}
staff <- function(k) startsWith(k, "niveles.") | startsWith(k, "permisos.")
TODAS <- names(i18n_dict("es"))

cat("== a. the page's dictionary ==\n")
base <- i18n_claves_navegador()
k_es <- claves_head(i18n_head("es")); k_en <- claves_head(i18n_head("en"))
ok(sprintf("a.1 the page carries exactly the browser's keys (%d of %d)", length(base), length(TODAS)),
   setequal(k_es, base) && length(base) > 50L && length(base) < 400L)
ok("a.2 none of staff: no niveles.*, no permisos.*", !any(staff(k_es)))
ok("a.3 the T117 sign's text is not in the page (it comes with the staff card's render)",
   !"niveles.ctx.volviendo" %in% k_es && !"niveles.ctx.casa" %in% k_es)
ok("a.4 English: the same keys, in English where translated",
   setequal(k_en, base) && {
     h <- as.character(i18n_head("en")); grepl(i18n_dict("en")[["tablas.buscar"]] %||% "\001", h, fixed = TRUE) })
ok("a.5 the keys the engine builds at run time come as families (comun.mes.*, cuentas.carga.mensaje.*)",
   all(grep("^comun[.]mes[.]", TODAS, value = TRUE) %in% base) &&
     all(grep("^cuentas[.]carga[.]mensaje[.]", TODAS, value = TRUE) %in% base))
viejo <- tryCatch(system2("git", c("show", "706a0eff:R/i18n.R"), stdout = TRUE, stderr = FALSE), error = function(e) character())
k_viejo <- if (length(viejo)) local({
  e <- new.env(); eval(parse(text = viejo, keep.source = FALSE), e)
  e$.i18n_dir <- .i18n_dir; e$`%||%` <- `%||%`; claves_head(e$i18n_head("es")) }) else character()
ok(sprintf("a.6 contraprueba: 706a0eff's page carried %d keys, staff's among them", length(k_viejo)),
   length(k_viejo) > 2000L && any(staff(k_viejo)) && !setequal(k_viejo, k_es))

cat("== b. every call site of the page's JavaScript ==\n")
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# Calls hdT( hdTn( t( tn( and their first argument (up to the first , or ) at depth 0).
llamadas <- function(x) {
  pos <- gregexpr("(?<![A-Za-z0-9_.$])(hdTn?|tn?)\\(", x, perl = TRUE)[[1]]
  if (pos[1] < 0) return(character())
  vapply(seq_along(pos), function(i) {
    j <- pos[i] + attr(pos, "match.length")[i]; d <- 0L; s <- j
    ch <- strsplit(substr(x, j, j + 300L), "")[[1]]
    q <- ""
    for (n in seq_along(ch)) {
      c <- ch[n]
      if (nzchar(q)) { if (c == q) q <- ""; next }
      if (c %in% c("'", "\"")) { q <- c; next }
      if (c %in% c("(", "[", "{")) d <- d + 1L
      if (c %in% c(")", "]", "}")) { if (d == 0L) break; d <- d - 1L }
      if (c == "," && d == 0L) break
    }
    trimws(paste(ch[seq_len(n - 1L)], collapse = ""))
  }, character(1))
}
es_clave <- function(s) grepl("^'[a-z][A-Za-z0-9_]*([.][A-Za-z0-9_]+)+'$", s)
literales <- function(s) gsub("'", "", regmatches(s, gregexpr("'[a-z][A-Za-z0-9_]*([.][A-Za-z0-9_]+)+'", s))[[1]])
en_base <- function(k) k %in% base || paste0(k, ".uno") %in% base
# Known sites whose key arrives another way, and how. Exact argument text.
CONOCIDOS <- list(
  "www/hdtable.js" = c("k" = "wrapper", "clave || 'tablas.confirmar_accion'" = "tabla: confirmar_texto",
                       "clave" = "tabla: textos$contador", "st.textos.banda || st.textos.contador || 'tablas.contador.registros'" = "tabla: textos$banda",
                       "st.fijarTitulo" = "tabla: periodo$fijar_titulo", "'comun.mes.' + Number(m.slice(5, 7))" = "familia comun.mes.",
                       "'comun.mes.' + k" = "familia comun.mes."),
  "www/overlay_init.js" = c("_k + '.titulo'" = "familia cuentas.carga.mensaje.", "_k + '.detalle'" = "familia cuentas.carga.mensaje.",
                            "k" = "wrapper"),
  "www/requisiciones_paste.js" = c("clave" = "avisar(): its callers pass literals of this same file"),
  "R/ui_components.R" = c("k" = "wrapper"))
js <- setdiff(list.files("www", pattern = "[.]js$", full.names = TRUE), "www/i18n.js")
rs <- c(setdiff(list.files("R", pattern = "[.]R$", full.names = TRUE), "R/i18n.R"), "app.R")   # i18n.R: the definitions and their docs
sin_base <- character(); desconocidas <- character(); n_lit <- 0L
for (f in c(js, rs)) {
  x <- leer(f)
  a <- llamadas(x)
  # In R files only the JavaScript they carry: single-quoted or non-literal
  # args inside a JS-looking call (R's own t() on a matrix has no quotes and
  # never matches a known site; those files have none today).
  if (f %in% rs) a <- a[grepl("'", a) | f %in% names(CONOCIDOS)]
  for (s in a) {
    if (!nzchar(s) || grepl("^function", s)) next
    lits <- literales(s)
    soloLit <- es_clave(s) || (length(lits) && !grepl("[A-Za-z_][A-Za-z0-9_.]*\\s*[+(]", gsub("'[^']*'", "''", s)) &&
                                 grepl("\\?", s))
    if (soloLit) { n_lit <- n_lit + length(lits); sin_base <- c(sin_base, paste0(f, ": ", lits[!vapply(lits, en_base, logical(1))])); next }
    if (!s %in% names(CONOCIDOS[[f]] %||% character())) desconocidas <- c(desconocidas, paste0(f, ": ", s))
  }
}
sin_base <- sin_base[!grepl(": $", sin_base)]
ok(sprintf("b.1 every literal key the page's JavaScript asks for is in the page (%d literals)", n_lit),
   n_lit > 40L && !length(sin_base))
if (length(sin_base)) cat("     faltan:", head(sin_base, 10), sep = "\n       ")
ok("b.2 every non-literal call is a known site with its route (a new one must say how its key arrives)",
   !length(desconocidas))
if (length(desconocidas)) cat("     desconocidas:", head(desconocidas, 10), sep = "\n       ")
ok("b.3 the requisiciones_paste.js avisar() callers pass literal keys that are in the page",
   { x <- leer("www/requisiciones_paste.js"); l <- .i18n_literales(x, "avisar\\([^,]+,\\s*'([a-z][A-Za-z0-9_.]+)'")
     length(l) >= 1L && all(l %in% base) })

cat("== c. a table's own words travel with its data ==\n")
d <- data.frame(id = c("a", "b"), Parte = c("UNO", "DOS"), Importe = c(10, 20), stringsAsFactors = FALSE)
cols <- list(col_texto("Parte", "tablas.facturas.parte"), col_dinero("Importe", "tablas.facturas.importe"))
mensajes <- list()
srv <- function(id) moduleServer(id, function(input, output, session) {
  rs <- session$rootScope()
  unlockBinding("sendCustomMessage", rs)
  rs$sendCustomMessage <- function(type, message) mensajes[[length(mensajes) + 1L]] <<- list(type = type, msg = message)
  datos <- reactive(d)
  hd_table_server("req", tabla = "detalle_dia_resumen", datos = datos, columnas = cols,
                  textos = list(contador = "requisiciones.contador"),
                  acciones_barra = list(list(accion = "papelera", texto = "tablas.accion.agregar_seleccion",
                                             confirmar = TRUE, confirmar_texto = "requisiciones.confirmar_papelera")))
  hd_table_server("simple", tabla = "detalle_dia_resumen", datos = datos, columnas = cols)
})
de <- function(t) { m <- Filter(function(m) identical(m$type, "hdt-datos") && grepl(paste0(t, "-hdt$"), m$msg$id), mensajes)
  if (length(m)) m[[length(m)]]$msg else NULL }
testServer(srv, {
  session$flushReact()
  r <- de("req"); s <- de("simple")
  ok("c.1 both tables sent their data", !is.null(r) && !is.null(s))
  ok("c.2 the table's counter and confirmation words come with its data (.uno and .otros)",
     all(c("requisiciones.contador.uno", "requisiciones.contador.otros",
           "requisiciones.confirmar_papelera.uno", "requisiciones.confirmar_papelera.otros") %in% names(r$i18n)) &&
       identical(r$i18n[["requisiciones.contador.uno"]], tr("requisiciones.contador.uno")))
  ok("c.3 a table without its own keys: only its class's counter, nothing of staff",
     length(s$i18n) >= 1L && all(startsWith(names(s$i18n), "tablas.")) && !any(staff(names(s$i18n))))
  ok("c.4 no table message ever carries an internal key", !any(names(c(r$i18n, s$i18n)) %in% i18n_claves_internas()))
})
hd <- leer("www/hdtable.js")
ok("c.5 the engine merges a message's words into the page's dictionary before drawing",
   grepl("window.HD_I18N = Object.assign(window.HD_I18N || {}, msg.i18n);", hd, fixed = TRUE) &&
     regexpr("Object.assign(window.HD_I18N", hd, fixed = TRUE) < regexpr("pintarDesde(msg);", hd, fixed = TRUE))
ok("c.7 the page's <head> merges into HD_I18N, never replaces it (it also comes with the first table a renderUI draws, after other tables' words arrived)",
   grepl("window.HD_I18N=Object.assign(window.HD_I18N||{},", as.character(i18n_head()), fixed = TRUE) &&
     !grepl("window.HD_I18N={", as.character(i18n_head()), fixed = TRUE))
# At least 48 (T118's): a later change moves it again (T102: 49), never back.
ok("c.6 hdtable.js's version moved (browsers drop the old copy)",
   isTRUE(as.integer(sub(".*hdtable\\.js\\?v=([0-9]+).*", "\\1", grep("hdtable\\.js\\?v=", leer("R/hd_table.R"), value = TRUE)[1])) >= 48L))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
