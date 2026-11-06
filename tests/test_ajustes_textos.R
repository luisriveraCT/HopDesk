# =============================================================================
# tests/test_ajustes_textos.R
# T23 · 4: the notices of Ajustes' scanner, Business Partners and Cuentas de
# Empresa, and the scanner's and Business Partners' titles and buttons, come
# from the dictionary (tr("tablas.ajustes.*")), in Spanish and in English.
#   1. static: no showNotification() with a literal text in settings_hub.R,
#      settings_bp.R or settings_cuentas.R;
#   2. every tr("tablas.ajustes.*") key those files use exists in es and en;
#   3. the Business Partners panel in English: its title and buttons are the
#      English ones, with no Spanish left.
#
# Run from project root:  Rscript tests/test_ajustes_textos.R
# =============================================================================
cat("=== test_ajustes_textos ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
suppressMessages(source("R/i18n.R"))
ARCHIVOS <- c("R/settings/settings_hub.R", "R/settings/settings_bp.R", "R/settings/settings_cuentas.R")

# ── 1. No literal notices ────────────────────────────────────────────────────
literales <- function(f) {
  out <- character()
  walk <- function(x) if (is.call(x)) {
    if (identical(x[[1]], as.name("showNotification")) && length(x) >= 2L && is.character(x[[2]]))
      out <<- c(out, x[[2]])
    args <- as.list(x)[-1]
    for (i in seq_along(args)) if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
  }
  for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) walk(e)
  out
}
for (f in ARCHIVOS) {
  l <- literales(f)
  ok(sprintf("%s: ningún showNotification() con texto fijo%s", basename(f),
             if (length(l)) paste0(" (", paste(l, collapse = " | "), ")") else ""), !length(l))
}

# ── 2. Every key exists in both languages ────────────────────────────────────
claves <- unique(unlist(lapply(ARCHIVOS, function(f) {
  t <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  regmatches(t, gregexpr('"tablas\\.ajustes\\.[a-z_.]+"', t))[[1]]
})))
claves <- gsub('"', "", claves)
es <- i18n_dict("es"); en <- i18n_dict("en")
falta <- claves[vapply(claves, function(k) is.null(es[[k]]) || is.null(en[[k]]), logical(1))]
ok(sprintf("las %d claves tablas.ajustes.* que usan existen en es y en%s", length(claves),
           if (length(falta)) paste0(" (faltan: ", paste(falta, collapse = ", "), ")") else ""),
   length(claves) > 20L && !length(falta))

# ── 3. Business Partners in English ──────────────────────────────────────────
for (e in parse("R/settings/settings_bp.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(as.character(e[[2]]), ".bp_panel_ui")) eval(e)
hd_table_ui <- function(id, ...) div(id = id)
puede <- function(...) TRUE
proveedoresUI <- function(id) div()
`%||%` <- function(a, b) if (is.null(a)) b else a
html <- function(cands) paste(as.character(.bp_panel_ui(list(usuario = function() NULL), cands)), collapse = "")
op <- options(hopdesk.idioma = "en")
con_datos <- html(data.frame(x = 1)); sin_datos <- html(NULL)
options(op)
ok("Business Partners en inglés: título, Refresh y Apply as IC en inglés; nada del español de antes",
   grepl("Apply as IC", con_datos, fixed = TRUE) && grepl("Refresh", con_datos, fixed = TRUE) &&
     grepl("Unique codes", con_datos, fixed = TRUE) &&
     !grepl("Aplicar como IC|Actualizar|extra.dos de los snapshots", con_datos))
ok("Business Partners en inglés sin datos SAP: el aviso en inglés",
   grepl("No SAP data loaded", sin_datos, fixed = TRUE) && !grepl("No hay datos SAP", sin_datos, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
