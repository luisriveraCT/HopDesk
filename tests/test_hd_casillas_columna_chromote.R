# =============================================================================
# tests/test_hd_casillas_columna_chromote.R
# T68, the engine in the browser: casillas_columna on a 3 000-person table
# in memory (hd_table "avisos_de", alto = "ventana", grouped by tier), at
# 1293x632 and 1772x866. No S3, no login.
#   - window mode: far fewer rows painted than 3 000; the table scrolls
#     inside, the page doesn't grow;
#   - the header's count ("N de 3,000", "Todos", "Nadie");
#   - a cell's box -> t$casillas() on the server;
#   - the header's box with a search on acts only on the rows found;
#   - a group's box acts on that group's rows; a click on a box never sorts.
# Run from the repo root:  Rscript tests/test_hd_casillas_columna_chromote.R
# =============================================================================
cat("=== test_hd_casillas_columna_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/live_app_harness.R")
PORT <- httpuv::randomPort()
raiz <- normalizePath(".", winslash = "/")
FOTOS <- Sys.getenv("T68_FOTOS", "")
app <- NULL; b <- NULL
limpiar <- function() { try(if (!is.null(b)) b$close(), silent = TRUE); try(if (!is.null(app)) app$proc$kill(), silent = TRUE) }

APP <- sprintf('
setwd(%s)
suppressPackageStartupMessages(library(shiny))
`%%||%%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R")) suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
N <- 3000L
D <- data.frame(id = sprintf("u%%04d", seq_len(N)), nombre = sprintf("Persona %%04d", seq_len(N)),
                nivel = rep(c("Directivo", "Operación", "Compras"), length.out = N),
                puesto = ifelse(seq_len(N) %%%% 50L == 0L, "Planta Norte", "Oficina"),
                req = seq_len(N) %%%% 10L == 0L, bancos = FALSE, stringsAsFactors = FALSE)
ui <- fluidPage(style = "height:100vh; overflow:hidden;",
  div(id = "caja", style = "height:584px; display:flex; flex-direction:column;", hd_table_ui("t", alto = "ventana")),
  pre(id = "eco", textOutput("eco_txt")))
server <- function(input, output, session) {
  t <- hd_table_server("t", tabla = "avisos_de", datos = reactive(D), grupo = "nivel",
    columnas = list(col_texto("nombre", "Persona"), col_texto("nivel", "Nivel"), col_texto("puesto", "Puesto"),
                    col_casilla("req", "Requisiciones"), col_casilla("bancos", "Bancos")))
  output$eco_txt <- renderText({ c <- t$casillas(); jsonlite::toJSON(list(req = sum(c$req), bancos = sum(c$bancos),
    bancos_ids = I(c$id[c$bancos])), auto_unbox = TRUE) })
}
shinyApp(ui, server)
', deparse(raiz))

correr <- function(ancho, alto) {
  app <<- start_bg_app(APP, PORT <<- httpuv::randomPort(), timeout_s = 60)
  b <<- new_chromote_session()
  invisible(b$Emulation$setDeviceMetricsOverride(width = ancho, height = alto, deviceScaleFactor = 1, mobile = FALSE))
  invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT), wait_ = FALSE))
}
ev <- function(js) { r <- b$Runtime$evaluate(js, returnByValue = TRUE); r$result$value }
espera <- function(js, s = 30) isTRUE(tryCatch(poll_until(function() isTRUE(tryCatch(ev(js), error = function(e) FALSE)),
  timeout_s = s, interval_s = 0.3), error = function(e) FALSE))
eco <- function() { t <- trimws(ev("(document.getElementById('eco_txt') || {}).textContent || ''"))
  if (nzchar(t)) tryCatch(jsonlite::fromJSON(t), error = function(e) list()) else list() }
eco_es <- function(campo, v, s = 10) isTRUE(poll_until(function() identical(as.numeric(eco()[[campo]]), as.numeric(v)), timeout_s = s, interval_s = 0.25))
clic <- function(sel) {
  p <- jsonlite::fromJSON(ev(sprintf("(function(){ var e = document.querySelector(%s); if (!e) return 'null';
    e.scrollIntoView({block: 'center'}); var r = e.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
    jsonlite::toJSON(sel, auto_unbox = TRUE))))
  if (is.null(p)) return(FALSE)
  for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = p[1], y = p[2], button = "left", clickCount = 1)
  Sys.sleep(0.4); TRUE
}
cuenta_cab <- function(col) ev(sprintf("(function(){ var th = document.querySelectorAll('#t-hdt thead th.hdt-cas-th')[%d];
  return th ? th.querySelector('.hdt-cas-n').textContent : ''; })()", col))
foto <- function(n) if (nzchar(FOTOS)) { dir.create(FOTOS, showWarnings = FALSE, recursive = TRUE)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file.path(FOTOS, paste0(n, ".png"))) }

for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  et <- paste0(tam[1], "x", tam[2])
  tryCatch({
    correr(tam[1], tam[2])
    ok(sprintf("%s: the table is up, no login", et), espera("document.querySelectorAll('#t-hdt tr.hdt-grupo').length === 3", 60) &&
         !isTRUE(ev("!!document.querySelector('input[type=password]')")))
    ok(sprintf("%s: the header counts the column over 3,000 ('300 de 3,000', 'Nadie')", et),
       identical(cuenta_cab(0), "300 de 3,000") && identical(cuenta_cab(1), "Nadie"))
    clic("#t-hdt tr.hdt-grupo button.hdt-grupo-abrir")
    ok(sprintf("%s: a group opens; window mode paints far fewer rows than its 1,000", et),
       espera("document.querySelectorAll('#t-hdt tbody input.hdt-cas').length > 0") &&
         isTRUE(ev("document.querySelectorAll('#t-hdt tbody tr:not(.hdt-grupo)').length") < 300))
    ok(sprintf("%s: the page doesn't grow: the table scrolls inside", et),
       isTRUE(ev("document.documentElement.scrollHeight <= window.innerHeight + 2")) &&
         isTRUE(ev("(function(){ var s = document.querySelector('#t-hdt .hdt-scroll') || document.querySelector('#t-hdt [class*=scroll]'); return !!s && s.scrollHeight > s.clientHeight; })()")))
    foto(paste0("A_", et))
    # a cell's box
    clic("#t-hdt tbody tr:not(.hdt-grupo) input.hdt-cas[data-col='4']")
    ok(sprintf("%s: a cell's box reaches the server (Bancos: 1)", et), eco_es("bancos", 1))
    ok(sprintf("%s: and the header says it", et), identical(cuenta_cab(1), "1 de 3,000"))
    # the header's box with a search: only the rows found
    ev("(function(){ var q = document.querySelector('#t-hdt input.hdt-buscar'); q.value = 'planta norte'; q.dispatchEvent(new Event('input', {bubbles: true})); return true; })()")
    espera("(function(){ var th = document.querySelectorAll('#t-hdt thead th.hdt-cas-th')[1]; return th && /de 60$/.test(th.querySelector('.hdt-cas-n').textContent) || th.querySelector('.hdt-cas-n').textContent === 'Nadie'; })()", 10)
    orden_antes <- ev("(document.querySelector('#t-hdt thead th.hdt-cas-th .hdt-flecha') || {}).textContent || ''")
    clic("#t-hdt thead th.hdt-cas-th:nth-of-type(n) input.hdt-cas-todos[data-col='4']")
    ok(sprintf("%s: the header's box with 'planta norte' ticks only the 60 found (+1 before = 61)", et), eco_es("bancos", 61))
    ok(sprintf("%s: a click on a header box never sorts", et),
       identical(ev("(document.querySelector('#t-hdt thead th.hdt-cas-th .hdt-flecha') || {}).textContent || ''"), orden_antes))
    ids <- eco()$bancos_ids
    ok(sprintf("%s: the ones ticked are the Planta Norte ones (every 50th) and the first one", et),
       all(as.integer(sub("u", "", ids[-1])) %% 50L == 0L) || all(as.integer(sub("u", "", setdiff(ids, ids[1]))) %% 50L == 0L))
    foto(paste0("B_busqueda_", et))
    # clear the search; a group's box
    ev("(function(){ var q = document.querySelector('#t-hdt input.hdt-buscar'); q.value = ''; q.dispatchEvent(new Event('input', {bubbles: true})); return true; })()")
    Sys.sleep(0.5)
    n_antes <- as.numeric(eco()$req)
    clic("#t-hdt tr.hdt-grupo:last-of-type input.hdt-cas-grupo[data-col='3'], #t-hdt tr.hdt-grupo input.hdt-cas-grupo[data-col='3']")
    ok(sprintf("%s: a group's box ticks its rows (Requisiciones goes up by its unticked ones)", et),
       isTRUE(poll_until(function() as.numeric(eco()$req) > n_antes, timeout_s = 10, interval_s = 0.25)) &&
         (as.numeric(eco()$req) - n_antes) %in% c(900, 1000))
  }, error = function(e) ok(paste(et, ": no error running the checks:", conditionMessage(e)), FALSE))
  limpiar()
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
