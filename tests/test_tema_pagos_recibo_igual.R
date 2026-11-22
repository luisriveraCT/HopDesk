# =============================================================================
# tests/test_tema_pagos_recibo_igual.R
# TEMA stage 4: the payment file's receipt (.build_pago_receipt_html(),
# R/pagos_audit_viewer_module.R), a standalone HTML/PDF, paints in R: its
# colours are {{rol}} markers resolved by tema_pintar() (R/tema_registro.R).
# The base and the branch build four receipts (a pago_ppl with beneficiaries
# and without, another type with its detail and without), each in a clean
# process, and:
#   - with the colours masked, every receipt is the base's, byte for byte;
#   - every colour is paired with the base's, in order, within ΔE 2 (CIE76);
#   - nothing is left unpainted; tema = NULL is Clásico; Marca follows;
#   - the words come from the dictionary (tablas.archivos_pago.recibo.* and
#     .tipo.*): in English no fixed Spanish is left; in Spanish the only
#     changes are "(N pago(s))" -> "(N pago)"/"(N pagos)" and <html lang>.
#
#   TEMA_BASE=<checkout before this change> Rscript --vanilla tests/test_tema_pagos_recibo_igual.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_pagos_recibo_igual ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

recibos <- function(raiz, tema = NULL, idioma = "es") callr::r(function(raiz, tema, idioma) {
  setwd(raiz)
  options(hopdesk.idioma = idioma)
  `%||%` <- function(a, b) if (is.null(a)) b else a
  assign("%||%", `%||%`, envir = globalenv())
  for (f in c("R/monedas.R", "R/i18n.R", "R/tema_registro.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
  # Only the definitions (and top-level constants) of the modules: their
  # server parts need a running app.
  defs <- function(archivo) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        (!is.call(e[[3]]) || identical(e[[3]][[1]], as.name("function")) || identical(e[[3]][[1]], as.name("c"))))
      eval(e, globalenv())
  for (f in c("R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R", "R/pagos_generados_persistence.R")) defs(f)
  ben <- list(list(alias = "PROV <A>", clabe = "012345678901234567", banco = "BBVA", importe = 1234.5, concepto = "F-1"),
              list(alias = "PROV B", clabe = "012345678901234568", banco = "BBVA", importe = 50, concepto = "F-2"))
  fila <- data.frame(id = "p1", ts = as.POSIXct("2026-11-19 10:00:00", tz = "UTC"), user = "luis", tipo = "pago_ppl",
                     empresa = "NTS", moneda = "USD", total = 1284.5, n_pagos = 2L, archivo = "f.txt", source = "individual",
                     cuenta_origen = "c", banco_origen = "b", detalle = NA_character_,
                     beneficiarios = as.character(jsonlite::toJSON(ben, auto_unbox = TRUE)), stringsAsFactors = FALSE)
  sin_ben <- fila; sin_ben$beneficiarios <- "[]"
  otro <- fila; otro$tipo <- names(get(".PAGOS_TIPO_LABELS"))[2] %||% "otro"; otro$beneficiarios <- NA_character_
  otro$detalle <- as.character(jsonlite::toJSON(list(Banco = "BBVA", Archivos = c("a.txt", "b.txt")), auto_unbox = TRUE))
  sin_det <- otro; sin_det$detalle <- NA_character_; sin_det$total <- NA_real_
  hacer <- function(r) {
    a <- list(r); if (!is.null(tema)) a$tema <- tema
    paste(as.character(do.call(.build_pago_receipt_html, a)), collapse = "\n")
  }
  c(pago = hacer(fila), pago_vacio = hacer(sin_ben), otro = hacer(otro), otro_vacio = hacer(sin_det))
}, args = list(raiz = raiz, tema = tema, idioma = idioma))

RX <- "(?<![&\\w])#(?:[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])"
enmascarar <- function(html) gsub(RX, "§color", html, perl = TRUE)
colores <- function(html) lapply(regmatches(html, gregexpr(RX, html, perl = TRUE))[[1]], function(s) {
  h <- substring(s, 2L); if (nchar(h) == 3L) h <- paste(rep(strsplit(h, "")[[1]], each = 2L), collapse = "")
  strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L)
})
lab <- function(rgb) grDevices::convertColor(matrix(rgb / 255, nrow = 1), from = "sRGB", to = "Lab")
de  <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))
reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)

rama <- recibos(RAMA)
ok("the branch builds the four receipts", length(rama) == 4L && all(nchar(rama) > 500L))
ok("R/pagos_audit_viewer_module.R has no hex of its own", {
  x <- readLines("R/pagos_audit_viewer_module.R", warn = FALSE, encoding = "UTF-8"); x <- x[!grepl("^\\s*#", x)]
  !any(grepl(RX, x, perl = TRUE)) && !any(grepl("\\brgba?\\(", x))
})
ok("nothing left unpainted ({{...}})", !any(grepl("{{", rama, fixed = TRUE)))
ok("tema = NULL is Clásico", identical(rama, recibos(RAMA, tema = "clasico")))
marca <- recibos(RAMA, tema = "marca")
ok("under Marca the receipt follows the theme (its text and borders)",
   grepl(reg$tema_color("borde-fuerte", "marca"), marca[["pago"]], fixed = TRUE) && !identical(marca, rama))
ok("the empty receipts keep their gray note (no beneficiaries / no detail)",
   grepl(reg$tema_color("leg-texto-2-888888"), rama[["pago_vacio"]], fixed = TRUE) &&
   grepl(reg$tema_color("leg-texto-2-888888"), rama[["otro_vacio"]], fixed = TRUE))

# The words (tablas.archivos_pago.recibo.*, tablas.archivos_pago.tipo.*).
ingles <- recibos(RAMA, idioma = "en")
ESPANOL <- c("Recibo de archivo", "Generado por", "Fecha:", "Tipo:", "Empresa:", "Archivo:", "ID de registro",
             "Beneficiario", "<th>Banco</th>", "Importe", "Concepto", "Cuenta origen", "Sin beneficiarios", "Sin detalle",
             " pago)", " pagos)", "Generado automáticamente", ">Pago<", "lang=\"es\"")
ok("in English the receipts keep no fixed Spanish text",
   !any(vapply(ESPANOL, function(s) any(grepl(s, ingles, fixed = TRUE)), logical(1))))
ok("in English: the payment file receipt, its columns, \"(2 payments)\" and <html lang=\"en\">",
   all(vapply(c("Payment file receipt", "Beneficiary", "Amount", "(2 payments)", "<html lang=\"en\">", "Source account:"),
              function(s) grepl(s, ingles[["pago"]], fixed = TRUE), logical(1))))

if (nzchar(BASE)) {
  base <- recibos(BASE)
  # The text changes on purpose: "(N pago(s))" says one or the other, and the
  # page says its language (<html lang="es">). Applied to the base before comparing.
  traducir <- function(h) {
    h <- sub("<html>", "<html lang=\"es\">", h, fixed = TRUE)
    m <- regmatches(h, regexpr("\\(([0-9]+) pago\\(s\\)\\)", h))
    if (length(m)) { n <- as.integer(sub("^\\(([0-9]+).*$", "\\1", m))
      h <- sub(m, sprintf("(%d %s)", n, if (n == 1L) "pago" else "pagos"), h, fixed = TRUE) }
    h
  }
  base_trad <- vapply(base, traducir, "")
  ok("the base's only text changes are \"pago(s)\" (the two pago_ppl receipts' total) and <html lang>",
     sum(grepl("pago(s)", base, fixed = TRUE)) == 2L && all(grepl("<html>", base, fixed = TRUE)))
  for (k in names(base)) {
    ok(sprintf("%s: with the colours masked, the base's receipt byte for byte (\"pago(s)\" and lang translated)", k),
       identical(enmascarar(base_trad[[k]]), enmascarar(rama[[k]])))
    cb <- colores(base[[k]]); cr <- colores(rama[[k]])
    d <- if (length(cb) == length(cr)) mapply(de, cb, cr) else Inf
    ok(sprintf("%s: %d colours paired in order, each within ΔE 2 (max %.2f)", k, length(cr), max(c(0, d))),
       length(cb) == length(cr) && length(cr) > 0L && all(d <= 2))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
