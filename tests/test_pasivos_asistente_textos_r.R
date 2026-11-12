# =============================================================================
# tests/test_pasivos_asistente_textos_r.R
# The messages of the Pasivos wizard's validators (R/pasivos_wizard_validate.R)
# come from the dictionary (fix/pasivos-asistente-fecha, commit 2):
#   1. no fixed text goes into an error list, nor into the step warnings;
#   2. every tablas.pasivos.asistente.* key it uses exists in es and in en;
#   3. the Spanish reads EXACTLY as before, built as the old code built it.
# Run from project root:  Rscript tests/test_pasivos_asistente_textos_r.R
# VALIDA_R=<file> checks another copy of the validators (the contraprueba).
# =============================================================================
cat("=== test_pasivos_asistente_textos_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/avisos_guardiana.R")
F_M <- Sys.getenv("VALIDA_R", "R/pasivos_wizard_validate.R")
codigo <- paste(readLines(F_M, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ── 1. No fixed text ─────────────────────────────────────────────────────────
fijos <- avisos_errores_fijos(F_M)
ok(sprintf("ningún mensaje fijo en las listas de errores (quedan %d)", length(fijos)),
   { if (length(fijos)) message("  fijos: ", paste(utils::head(fijos, 5), collapse = " | ")); !length(fijos) })
ok("el aviso de corte = vencimiento sale del diccionario",
   !grepl('c(w, "', codigo, fixed = TRUE))

# ── 2. The keys exist in es and en ──────────────────────────────────────────
claves <- avisos_claves(F_M, "tablas.pasivos.asistente.")
ok(sprintf("las %d claves tablas.pasivos.asistente.* existen en es y en", length(claves)),
   { falta <- avisos_claves_faltan(claves); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     length(claves) >= 31L && !length(falta) })

# ── 3. The Spanish reads exactly as before ──────────────────────────────────
es <- function(k, ...) tr(paste0("tablas.pasivos.asistente.", k), ..., idioma = "es")
tipo <- "variable_otro"; missing_cols <- c("capital", "fees")
iguales <- list(
  list(es("nombre_obligatorio"), "El nombre es obligatorio."),
  list(es("nombre_largo"), "El nombre no puede superar 80 caracteres."),
  list(es("empresa_obligatoria"), "La empresa es obligatoria."),
  list(es("empresa_desconocida"), "Empresa no reconocida."),
  list(es("parte_obligatoria"), "El proveedor / parte es obligatorio."),
  list(es("moneda_invalida"), "Moneda de pago no válida."),
  list(es("dia_corte"), "Día de corte debe ser entre 1 y 31."),
  list(es("dia_vencimiento"), "Día de vencimiento debe ser entre 1 y 31."),
  list(es("periodicidad"), "Debes elegir una periodicidad."),
  list(es("monto_positivo"), "El monto debe ser mayor que cero."),
  list(es("origen_provision"), "Debes elegir el origen de la provisión."),
  list(es("monto_base_positivo"), "El monto base debe ser mayor que cero."),
  list(es("principal_positivo"), "El principal debe ser mayor que cero."),
  list(es("tasa_tipo_no_disponible", tipo = tipo),
       paste0("tasa_tipo '", tipo, "' no está disponible en el asistente. ",
              "Selecciona Fija, Variable SOFR o Variable TIIE28.")),
  list(es("tasa_negativa"), "La tasa no puede ser negativa."),
  list(es("plazo_minimo"), "El plazo debe tener al menos 1 período."),
  list(es("dia_pago"), "Día de pago debe ser entre 1 y 31."),
  list(es("dia_semana"), "Día de semana debe ser entre 1 (lunes) y 7 (domingo)."),
  list(es("mes_pago"), "Mes de pago debe ser entre 1 y 12."),
  list(es("gracia_negativa"), "El período de gracia no puede ser negativo."),
  list(es("cuota_gracia_negativa"), "La cuota de gracia personalizada no puede ser negativa."),
  list(es("tabla_obligatoria"), "Debes pegar la tabla de amortización."),
  list(es("tabla_error", error = "line 2 did not have 5 elements"),
       paste0("Error al parsear la tabla: ", "line 2 did not have 5 elements")),
  list(es("tabla_columnas", columnas = paste(missing_cols, collapse = ", ")),
       paste0("Faltan columnas: ", paste(missing_cols, collapse = ", "))),
  list(es("categoria"), "Debes elegir una categoría."),
  list(es("corte_igual_vencimiento"), "El día de corte y el día de vencimiento son iguales. Verifica que sea intencional."))
distintos <- avisos_distintos(iguales)
ok(sprintf("el español de los %d mensajes dice exactamente lo mismo que antes", length(iguales)),
   { for (p in distintos) message("  ahora: ", p[[1]], "\n  antes: ", p[[2]]); !length(distintos) })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
