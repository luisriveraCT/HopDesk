# =============================================================================
# tests/test_cashflow_etiquetas.R
# The cash-flow export's glossary and its preview name tags as everywhere
# else, A / B / C from HD_ETIQUETAS (T04): "Etiqueta A" / "Tag A", never the
# old "Etiqueta: Urgente" / "Importante".
#
# Run from project root:  Rscript tests/test_cashflow_etiquetas.R
# =============================================================================
cat("=== test_cashflow_etiquetas ===\n\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("R/i18n.R")
HTML <- htmltools::HTML
source("R/hd_columnas.R")
# Only these definitions (the files' other code needs the app).
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
cargar("R/persistence.R", "tag_label")
cargar("R/cashflow_export_module.R", c(".cf_etiqueta", ".cf_lbl"))
cargar("R/cashflow_preview_module.R", c("cashflow_tabla_datos", "cashflow_tabla_columnas"))

ok("the export's glossary names the tags A, B and C, in Spanish",
   identical(c(.cf_lbl("g_tag_urg", "es"), .cf_lbl("g_tag_both", "es"), .cf_lbl("g_tag_imp", "es")),
             c("Etiqueta A", "Etiqueta B", "Etiqueta C")))
ok("... and in English", identical(.cf_lbl("g_tag_urg", "en"), "Tag A"))
ok("B has its description, in both languages",
   grepl("urgentes e importantes", .cf_lbl("g_tag_both_desc", "es")) && grepl("urgent and important", .cf_lbl("g_tag_both_desc", "en")))
src <- paste(readLines("R/cashflow_export_module.R", warn = FALSE), collapse = "\n")
ok("every glossary lists A, B, C in that order (Word, HTML, Excel)",
   lengths(regmatches(src, gregexpr('g_tag_urg"\\),\\s*Lp\\("g_tag_urg_desc"\\),\\s*"#8B1A1A", FALSE\\),\\s*c\\(L\\("g_tag_both"\\),\\s*Lp\\("g_tag_both_desc"\\),\\s*"#8B0000", FALSE\\),\\s*c\\(L\\("g_tag_imp"\\)', src))) == 3L)
ok("no old tag names left in the export or the preview",
   !any(grepl("Etiqueta: |Tag: Urgent|title=\"Urgente\"|title=\"Importante\"",
              c(src, readLines("R/cashflow_preview_module.R", warn = FALSE)))))

wide <- function(u, i) data.frame(Parte = "ACME", `Sem 1` = 100, Total = 100, Pct = 1, tag_urgent = u, tag_important = i,
                                  is_provision = FALSE, check.names = FALSE)
# T17c: the preview is the table engine; the tag is its own column (col_etiqueta).
d <- cashflow_tabla_datos(list(bucket_labels = "Sem 1", currencies = "MXN", data = list(
  list(currency = "MXN", ar_wide = rbind(wide(TRUE, FALSE), wide(TRUE, TRUE), wide(FALSE, TRUE)), ap_wide = NULL,
       summary = NULL))))
et <- Find(function(c) identical(c$campo, "Etiqueta"), cashflow_tabla_columnas("Sem 1"))
ok("the preview's tag column shows the invoice's tag: A, B (both), C",
   identical(d$Etiqueta, c("urgent", "both", "important")) && identical(et$valores(d$Etiqueta)$mostrar, c("A", "B", "C")))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
