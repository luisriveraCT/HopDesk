# =============================================================================
# tests/test_pronosticos_avisos_r.R
# The notices of R/forecasting_module.R (refresh one metric or all, errors,
# subscriptions) come from the dictionary, not fixed text (avisos-varios).
#   1. no showNotification() in forecasting_module.R carries fixed text;
#   2. every tablas.pronosticos.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built it.
#
# Run from project root:  Rscript tests/test_pronosticos_avisos_r.R
# =============================================================================
cat("=== test_pronosticos_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/forecasting_module.R"; PRE <- "tablas.pronosticos.aviso."

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de forecasting_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- avisos_claves(F, PRE)
ok(sprintf("las %d claves %s* existen en es y en", length(claves), PRE),
   length(claves) == 9L && all(vapply(claves, clave_en_es_y_en, logical(1))))

es <- function(k, ...) tr(paste0(PRE, k), ..., idioma = "es")
n <- 1234L; m <- 13L; err <- "HTTP 503"; metric_id <- "usd_mxn"
iguales <- list(
  list(es("actualizando"), "Actualizando datos…"),
  list(es("error", error = err), paste("Error:", err)),
  list(es("observaciones_metrica", n = as.character(n), metrica = metric_id),
       paste0(n, " observación(es) nuevas para ", metric_id, ".")),
  list(es("sin_metricas"), "No hay métricas activas."),
  list(es("actualizando_n", n = as.character(n)), paste0("Actualizando ", n, " métricas…")),
  list(es("observaciones_total", n = as.character(n), m = as.character(m)),
       paste0(n, " observación(es) nuevas en ", m, " métricas.")),
  list(es("suscripcion_actualizada"), "Suscripción actualizada."),
  list(es("todas_con_global"), "Todas las métricas ya tienen suscripción global."),
  list(es("global_creada"), "Suscripción global creada."))
dif <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

# "Error: {error}" reads the same in both languages; every other key is translated.
IGUAL_EN_INGLES <- paste0(PRE, "error")
vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido (salvo 'Error: {error}') y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     (k %in% IGUAL_EN_INGLES || !identical(a, b)) && identical(vars(a), vars(b)) }, logical(1))))
codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("los números van como texto (sin separador de miles)",
   !grepl('tr\\("tablas\\.pronosticos\\.aviso\\.[a-z_]+",[^)]*\\b(n|m) = (?!as\\.character\\()', codigo, perl = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
