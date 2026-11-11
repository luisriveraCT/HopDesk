# =============================================================================
# tests/test_avisos_sueltos_r.R
# The 8 loose fixed notices (avisos-sueltos) come from the dictionary, in
# R/audit_log_viewer_module.R, R/pagos_audit_viewer_module.R,
# R/notes_handlers.R, R/reporte_module.R and R/treasury_map_module.R:
#   1. none of the 5 files has a fixed-text notice (each one named if it does);
#   2. every key of the 9 exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built as the old code built it.
# Run from project root:  Rscript tests/test_avisos_sueltos_r.R
# AVISOS_RAIZ=<checkout> checks another copy of the 5 files (the contraprueba).
# =============================================================================
cat("=== test_avisos_sueltos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
raiz <- Sys.getenv("AVISOS_RAIZ", ".")

# ── 1. No fixed text, file by file ──────────────────────────────────────────
archivos <- c("R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R", "R/notes_handlers.R",
              "R/reporte_module.R", "R/treasury_map_module.R")
for (f in archivos) {
  fijos <- avisos_fijos(file.path(raiz, f))
  ok(sprintf("%s: ningún aviso con texto fijo (quedan %d)", f, length(fijos)),
     { if (length(fijos)) message("  fijos: ", paste(fijos, collapse = " | ")); !length(fijos) })
}

# ── 2. The keys exist in es and en ──────────────────────────────────────────
claves <- c("tablas.actividad.aviso.error_documento", "tablas.archivos_pago.aviso.error_documento",
            "tablas.notas.aviso.guardada.personal", "tablas.notas.aviso.guardada.publica",
            "tablas.notas.aviso.eliminada", "tablas.reporte.aviso.pdf_generado",
            "tablas.interco.aviso.elige_factura", "tablas.interco.aviso.anadidas_plan",
            "tablas.interco.aviso.enviadas_agenda")
codigo <- paste(unlist(lapply(file.path(raiz, archivos), readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
ok("los 5 archivos usan las 9 claves", all(vapply(claves, function(k) grepl(k, codigo, fixed = TRUE), logical(1))))
ok("las 9 claves existen en es y en",
   { falta <- claves[!vapply(claves, clave_en_es_y_en, logical(1))]
     if (length(falta)) message("  faltan: ", paste(falta, collapse = ", ")); !length(falta) })

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es <- function(k, ...) tr(k, ..., idioma = "es")
nota_antes <- function(visibility) {
  lbl <- if (visibility == "personal") "personal" else "pública"
  paste0("Nota ", lbl, " guardada.")
}
nota_ahora <- function(visibility)
  es(if (visibility == "personal") "tablas.notas.aviso.guardada.personal" else "tablas.notas.aviso.guardada.publica")
added <- 1234L; new_rows <- data.frame(x = 1:3)
iguales <- list(
  list(es("tablas.actividad.aviso.error_documento", error = "pandoc"), paste("Error generando el documento:", "pandoc")),
  list(es("tablas.archivos_pago.aviso.error_documento", error = "pandoc"), paste("Error generando el documento:", "pandoc")),
  list(nota_ahora("personal"), nota_antes("personal")),
  list(nota_ahora("publica"), nota_antes("publica")),
  list(es("tablas.notas.aviso.eliminada"), "Nota eliminada."),
  list(es("tablas.reporte.aviso.pdf_generado"), "✓ PDF generado correctamente."),
  list(es("tablas.interco.aviso.elige_factura"), "Selecciona al menos una factura."),
  list(es("tablas.interco.aviso.anadidas_plan", n = as.character(added)),
       paste0(added, " factura(s) añadida(s) al plan.")),                  # 1234: no thousands separator
  list(es("tablas.interco.aviso.enviadas_agenda", n = as.character(nrow(new_rows))),
       paste0(nrow(new_rows), " factura(s) enviada(s) a Agenda de Hoy.")))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d casos dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
