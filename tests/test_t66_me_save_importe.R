# =============================================================================
# tests/test_t66_me_save_importe.R
# T66: Agregar Item's Importe (R/manual_entry_handlers.R, me_importe) is a
# hd_numero_input; the browser no longer stops a negative or a text, and
# app.R's me_save read it with %||% 0 (global.R's %||% takes NA too: empty
# or a text became 0 silently). me_save now checks it after the date and
# before anything is built or stored; edit, Agenda sync and insert use the
# checked number.
#
# app.R's server is the whole app (no testServer harness), so this takes
# me_save's REAL check out of the parsed app.R (not its text) and runs it:
#   1. the browser half, in node: "32,000.50" -> 32000.5, "abc" -> null
#      (NA in R), "-5" -> -5 (www/hd_numero.js, the component's reading);
#   2. the check: a text (NA), an empty field (NA) and a negative are told
#      and stop the handler; 32000.5 and 0 go on;
#   3. it sits after the date's check and before both modes; the three
#      reads of the Importe use the checked number, none reads me_importe.
# Run from the repo root:  Rscript tests/test_t66_me_save_importe.R
# Contraprueba: HD_APP_R=<5e102e3's app.R> fails 2 and 3.
# =============================================================================
cat("=== test_t66_me_save_importe ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/ui_components.R", c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                              "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))
APP <- Sys.getenv("HD_APP_R", "app.R")

# ── 1. The browser half (node) ───────────────────────────────────────────────
node <- Sys.which("node")
if (nzchar(node)) {
  js <- tempfile(fileext = ".js")
  writeLines(c("global.window = {};",
               sprintf("eval(require('fs').readFileSync(%s, 'utf8'));",
                       jsonlite::toJSON(normalizePath("www/hd_numero.js", winslash = "/"), auto_unbox = TRUE)),
               "var I = window.hdNumero.interpretar;",
               "console.log(JSON.stringify([I('32,000.50','monto',2), I('abc','monto',2), I('','monto',2), I('-5','monto',2)]));"), js)
  lee <- jsonlite::fromJSON(paste(system2(node, js, stdout = TRUE), collapse = ""), simplifyVector = FALSE)
  ok("node: '32,000.50' -> 32000.5", identical(lee[[1]], 32000.5))
  ok("node: 'abc' and '' -> null (NA in R)", is.null(lee[[2]]) && is.null(lee[[3]]))
  ok("node: '-5' -> -5 (the browser lets it through; the server stops it)", identical(as.numeric(lee[[4]]), -5))
} else cat(" SKIP: node is not installed (section 1)\n")

# ── 2. me_save's check, from the parsed app.R ────────────────────────────────
buscar <- function(x) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]], as.name("observeEvent")) && length(x) >= 3L &&
      identical(deparse(x[[2]]), "input$me_save")) return(x[[3]])
  args <- as.list(x)[-1]   # an empty argument (x[, 1]) can't be bound to a variable
  for (i in seq_along(args)) if (is.call(args[[i]])) { r <- buscar(args[[i]]); if (!is.null(r)) return(r) }
  NULL
}
cuerpo <- NULL
for (e in parse(APP, keep.source = FALSE, encoding = "UTF-8")) { cuerpo <- buscar(e); if (!is.null(cuerpo)) break }
ok("me_save is found in app.R", is.call(cuerpo))
pasos <- if (is.call(cuerpo)) as.list(cuerpo)[-1] else list()
txt <- vapply(pasos, function(p) paste(deparse(p), collapse = " "), "")
i_rev   <- which(grepl("hd_numero_revisar(input$me_importe", txt, fixed = TRUE))
i_stop  <- if (length(i_rev)) i_rev[1] + 1L else NA_integer_
i_fecha <- which(grepl("comun.aviso.fecha_requerida", txt, fixed = TRUE))
i_modos <- which(grepl("nzchar(edit_id)", txt, fixed = TRUE))
ok("exactly one check of me_importe, followed by its stop", length(i_rev) == 1L &&
     grepl("rev_imp$ok", txt[i_stop], fixed = TRUE) && grepl("return()", txt[i_stop], fixed = TRUE))

# A function whose body is the check and its stop, then "sigue": its
# return() leaves this function as it leaves the handler.
correr <- function(valor) {
  AVISOS <- character(0)
  showNotification <- function(ui, ...) AVISOS <<- c(AVISOS, as.character(ui))
  f <- function(input) NULL
  body(f) <- call("{", pasos[[i_rev[1]]], pasos[[i_stop]], quote(list("sigue", rev_imp$valor)))
  environment(f) <- environment()
  r <- f(list(me_importe = valor))
  list(sigue = identical(r[[1]], "sigue"), valor = if (is.list(r)) r[[2]] else NULL, avisos = AVISOS)
}
for (caso in list(list(NA_real_, "a text (NA)", "Importe: escribe un número."),
                  list(NULL, "an empty field (NULL)", "Importe: escribe un número."),
                  list(-5, "a negative", "Importe no puede ser negativo."))) {
  r <- correr(caso[[1]])
  ok(sprintf("%s: '%s', the handler stops (nothing built or saved)", caso[[2]], caso[[3]]),
     !r$sigue && identical(r$avisos, caso[[3]]))
}
r <- correr(32000.5)
ok("32000.5 (typed '32,000.50'): no aviso, goes on with 32000.5", r$sigue && !length(r$avisos) && identical(r$valor, 32000.5))
r <- correr(0)
ok("0: goes on with 0", r$sigue && identical(r$valor, 0))

# ── 3. Where it sits, and who reads the number ───────────────────────────────
ok("the check comes after the date's and before the edit/insert branch",
   length(i_fecha) >= 1L && length(i_modos) >= 1L && length(i_rev) == 1L && i_fecha[1] < i_rev && i_rev < i_modos[1])
resto <- paste(txt[seq_along(txt) > (if (length(i_stop)) i_stop else 0L)], collapse = " ")
ok("after the check nothing reads input$me_importe again", !grepl("input$me_importe", resto, fixed = TRUE))
ok("the three reads (edit row, Agenda sync, insert row) use rev_imp$valor",
   lengths(regmatches(resto, gregexpr("Importe = rev_imp$valor", resto, fixed = TRUE))) == 2L &&
     grepl("new_imp = rev_imp$valor", resto, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
