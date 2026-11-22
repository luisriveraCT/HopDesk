# =============================================================================
# tests/test_tipo_labels_bancos.R
# A prod bug (found 2026-11-22, TEMA): R/bancos_module.R and
# R/pagos_audit_viewer_module.R both defined a global .TIPO_LABELS -- the bank
# movements' types and the payment files' types. app.R sources R/global.R
# (which sources bancos_module.R) and THEN pagos_audit_viewer_module.R, so
# the payment files' labels won, and Bancos' runtime reads (the type selector's
# choices, bancos_module.R:1263, :1405, :2645) saw "Pago", "Reporte Word"...
# The payment files' labels are now .PAGOS_TIPO_LABELS.
#
# The real load order, no app started: shiny sources R/ alphabetically
# (loadSupport), then app.R sources its files again, following source()
# into the files they source (R/global.R -> R/bancos_module.R). Of each file
# only the top-level assignments of the two constants are evaluated.
#   1. after that order, .TIPO_LABELS is Bancos' (spei_out, spei_in...);
#   2. the payment files' types are .PAGOS_TIPO_LABELS, and .tipo_label()
#      reads them ("pago_ppl" -> "Pago");
#   3. only R/bancos_module.R defines .TIPO_LABELS at top level.
# Contraprueba: with the payment file's constant under its old name, the same
# order leaves the payment files' labels in .TIPO_LABELS (the bug).
# Run from the project root:  Rscript tests/test_tipo_labels_bancos.R
# =============================================================================
cat("=== test_tipo_labels_bancos ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CONSTANTES <- c(".TIPO_LABELS", ".PAGOS_TIPO_LABELS")
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# The files a file sources at top level, in order, recursively (once each).
fuentes <- function(f, vistos = character()) {
  out <- character()
  for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) {
    if (is.call(e) && identical(e[[1]], as.name("source")) && is.character(e[[2]]) && startsWith(e[[2]], "R/") &&
        file.exists(e[[2]]) && !e[[2]] %in% c(vistos, out)) {
      out <- c(out, e[[2]])
      out <- c(out, setdiff(fuentes(e[[2]], c(vistos, out)), c(vistos, out)))
    }
  }
  out
}
# Evaluates only the two constants' top-level assignments of a file (its text
# may be replaced, for the contraprueba).
constantes_de <- function(texto, env) {
  for (e in parse(text = texto, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% CONSTANTES)
      eval(e, env)
}
cargar <- function(reemplazo = NULL) {
  env <- new.env()
  orden <- c(sort(list.files("R", pattern = "\\.R$", full.names = TRUE)), fuentes("app.R"))
  for (f in orden) {
    texto <- leer(f)
    if (!is.null(reemplazo) && identical(f, reemplazo$archivo)) texto <- reemplazo$fn(texto)
    constantes_de(texto, env)
  }
  env
}

orden_app <- fuentes("app.R")
ok("app.R sources R/global.R, which sources R/bancos_module.R, before R/pagos_audit_viewer_module.R",
   all(c("R/global.R", "R/bancos_module.R", "R/pagos_audit_viewer_module.R") %in% orden_app) &&
   match("R/bancos_module.R", orden_app) < match("R/pagos_audit_viewer_module.R", orden_app))

env <- cargar()
ok("1: after the real load order, .TIPO_LABELS is Bancos' (the bank movements' types)",
   all(c("spei_out", "spei_in", "comision") %in% names(env$.TIPO_LABELS)) && !"pago_ppl" %in% names(env$.TIPO_LABELS))
ok("2: the payment files' types are .PAGOS_TIPO_LABELS",
   all(c("pago_ppl", "reporte_word", "bancos_libro_excel") %in% names(env$.PAGOS_TIPO_LABELS)))
defs <- function(f) any(vapply(parse(f, keep.source = FALSE, encoding = "UTF-8"), function(e)
  is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".TIPO_LABELS")), logical(1)))
quienes <- Filter(defs, c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)))
ok(sprintf("3: only R/bancos_module.R defines .TIPO_LABELS at top level (%s)", paste(quienes, collapse = ", ")),
   identical(unname(quienes), "R/bancos_module.R"))
ok("2: .tipo_label() reads the payment files' labels (\"pago_ppl\" -> \"Pago\")", {
  # .tipo_label() names each type through the dictionary (tr(), R/i18n.R).
  i18 <- new.env(); assign("%||%", function(a, b) if (is.null(a)) b else a, envir = i18)
  sys.source("R/i18n.R", envir = i18)
  e2 <- new.env(parent = i18)
  for (x in parse("R/pagos_audit_viewer_module.R", keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) &&
        as.character(x[[2]]) %in% c(CONSTANTES, ".tipo_label")) eval(x, e2)
  identical(e2$.tipo_label(c("pago_ppl", "algo", NA)), c("Pago", "algo", ""))
})

bug <- cargar(list(archivo = "R/pagos_audit_viewer_module.R",
                   fn = function(t) gsub(".PAGOS_TIPO_LABELS", ".TIPO_LABELS", t, fixed = TRUE)))
ok("contraprueba: under its old name, the payment files' labels win .TIPO_LABELS (the bug)",
   "pago_ppl" %in% names(bug$.TIPO_LABELS) && !"spei_out" %in% names(bug$.TIPO_LABELS))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
