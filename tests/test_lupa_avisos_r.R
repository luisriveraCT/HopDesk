# =============================================================================
# tests/test_lupa_avisos_r.R
# The notices of R/search_module.R (the Lupa: tag, move, restore, close
# provisions, trash, send to the Agenda) come from the dictionary, not fixed
# text (avisos-varios).
#   1. no showNotification() in search_module.R carries fixed text;
#   2. every tablas.lupa.aviso.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built the way the old code built it.
#
# Run from project root:  Rscript tests/test_lupa_avisos_r.R
# =============================================================================
cat("=== test_lupa_avisos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F <- "R/search_module.R"; PRE <- "tablas.lupa.aviso."

fijos <- avisos_fijos(F)
ok(sprintf("ningún aviso de search_module.R con texto fijo (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })

claves <- avisos_claves(F, PRE)
ok(sprintf("las %d claves %s* existen en es y en", length(claves), PRE),
   length(claves) == 11L && all(vapply(claves, clave_en_es_y_en, logical(1))))

es <- function(k, ...) tr(paste0(PRE, k), ..., idioma = "es")
n <- 1234L; err <- "sin red"; new_date <- as.Date("2026-11-30")
fallas <- paste(c("moves: boom", "papelera: sin red"), collapse = "; ")
ledg_str <- "Pagos"; cur_str <- "MXN"; total <- "$ 1,234.50"
iguales <- list(
  list(es("etiquetadas", n = as.character(n)), paste0(n, " factura(s) etiquetadas.")),
  list(es("etiquetas_no_guardadas", error = err),
       paste0("No se pudieron guardar las etiquetas (\"", err,
              "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a intentarlo.")),
  list(es("fecha_invalida"), "Fecha inválida."),
  list(es("movidas", n = as.character(n), fecha = format(new_date, "%d/%m/%Y")),
       paste0(n, " factura(s) movidas a ", format(new_date, "%d/%m/%Y"), ".")),
  list(es("restauradas", n = as.character(n)), paste0(n, " factura(s) restauradas a fecha original.")),
  list(es("provisiones_cerradas", n = as.character(n)), paste0(n, " provisión(es) cerrada(s).")),
  list(es("borrado_parcial", fallas = fallas),
       paste0("No se pudo guardar completamente el borrado (", fallas,
              "). Puede que quede en un estado parcial -- vuelve a intentarlo en unos minutos y ",
              "no cierres la sesion mientras tanto.")),
  list(es("papelera", n = as.character(n)), paste0(n, " factura(s) enviadas a la papelera.")),
  list(es("sin_validas"), "No hay facturas válidas para agregar (se excluyeron provisiones)."),
  list(es("sin_documento"), "No se encontraron facturas con documento válido."),
  list(es("enviadas", n = as.character(n), ledger = ledg_str, moneda = cur_str, total = total),
       paste0("✓ ", n, " factura(s) enviadas a ", ledg_str, " — ", cur_str, " ", total, ".")))
dif <- Filter(function(p) !identical(p[[1]], p[[2]]), iguales)
ok(sprintf("el español de los %d avisos es idéntico al de antes", length(iguales)),
   { for (p in dif) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(dif) && length(iguales) == length(claves) })

vars <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
ok("el inglés está traducido y lleva las mismas {variables}",
   all(vapply(claves, function(k) { a <- i18n_dict("es")[[k]]; b <- i18n_dict("en")[[k]]
     !identical(a, b) && identical(vars(a), vars(b)) }, logical(1))))
codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("los números van como texto (sin separador de miles)",
   !grepl('tr\\("tablas\\.lupa\\.aviso\\.[a-z_]+", n = (?!as\\.character\\()', codigo, perl = TRUE))
ok("un error de guardado sin mensaje no rompe el aviso (paste0 lo omitía)",
   identical(es("etiquetas_no_guardadas", error = NULL %||% ""),
             paste0("No se pudieron guardar las etiquetas (\"", NULL,
                    "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a intentarlo.")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
