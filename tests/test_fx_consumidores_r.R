# =============================================================================
# tests/test_fx_consumidores_r.R
# banxico-fix 2/3: who shows the Banxico FIX (R/fx_banxico.R).
#   1. Flujo's Exportar: .suggest_fx_rate("USD","MXN") suggests the FIX
#      (4 decimals) and, without one, NA -- never the old 18.50; the other
#      pairs keep their values (a separate design task);
#   2. an export with a missing rate stops and says which one
#      (.fx_tasas_completas), in the three paths (Word, Excel, PDF);
#   3. the Pasivos wizard shows the FIX for USD -> MXN, or says there is
#      none, from the dictionary.
#
# Run from project root:  Rscript tests/test_fx_consumidores_r.R
# =============================================================================
cat("=== test_fx_consumidores_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
F <- "R/cashflow_export_module.R"
env <- new.env()
for (e in parse(F, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".suggest_fx_rate", ".fx_tasas_completas")) eval(e, env)
avisos <- character()
env$showNotification <- function(ui, ...) { avisos <<- c(avisos, as.character(ui)); invisible() }
env$tr <- function(...) tr(..., idioma = "es")
FIX <- NULL
env$fx_fix <- function() FIX

# ── 1. The suggestion ───────────────────────────────────────────────────────
FIX <- list(valor = 18.23456, fecha = as.Date("2026-11-12"))
ok("con FIX: sugiere el FIX con 4 decimales", identical(env$.suggest_fx_rate("USD", "MXN"), 18.2346))
FIX <- NULL
ok("sin FIX: NA (campo vacío), nunca 18.50", is.na(env$.suggest_fx_rate("USD", "MXN")))
env$fx_fix <- function() stop("caído")
ok("fx_fix() con error: NA", is.na(env$.suggest_fx_rate("USD", "MXN")))
env$fx_fix <- function() FIX
ok("las otras monedas siguen igual (tarea de diseño aparte)",
   identical(env$.suggest_fx_rate("EUR", "MXN"), 20) && identical(env$.suggest_fx_rate("EUR", "USD"), 1.08))
ok("el módulo ya no tiene el 18.50 (fuera de los comentarios)",
   { l <- readLines(F, warn = FALSE); !any(grepl("18\\.50?\\b", sub("#.*$", "", l))) })

# ── 2. A missing rate stops the export ──────────────────────────────────────
avisos <- character()
ok("todas las tasas: sigue, sin aviso", isTRUE(env$.fx_tasas_completas(c(USD = 18.2, EUR = 20), "MXN")) && !length(avisos))
ok("sin tasas (moneda única): sigue", isTRUE(env$.fx_tasas_completas(c(), "MXN")))
r <- env$.fx_tasas_completas(c(USD = NA_real_, EUR = 20), "MXN")
ok("USD vacía: se detiene y dice cuál falta", identical(r, FALSE) && length(avisos) == 1L && grepl("USD/MXN", avisos[1]))
ok("el aviso viene del diccionario",
   identical(avisos[1], tr("tablas.flujo.aviso.tc_faltante", monedas = "USD/MXN", idioma = "es")))
avisos <- character()
ok("una tasa en 0 tampoco sirve", identical(env$.fx_tasas_completas(c(USD = 0), "MXN"), FALSE) && length(avisos) == 1L)
codigo <- paste(readLines(F, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("las tres exportaciones (Word, Excel, PDF) revisan las tasas antes de convertir",
   lengths(regmatches(codigo, gregexpr("if (!.fx_tasas_completas(fx_rates, base_cur))", codigo, fixed = TRUE))) == 3L)

# ── 3. The Pasivos wizard ───────────────────────────────────────────────────
wiz <- paste(readLines("R/pasivos_wizard_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("el asistente toma el FIX de fx_fix(), no de forecasting_get_estimate",
   grepl("tryCatch(fx_fix(), error = function(e) NULL)", wiz, fixed = TRUE) &&
     !grepl('FX usado: %.4f', wiz, fixed = TRUE) && !grepl("FX no disponible para esta par", wiz, fixed = TRUE))
ok("sus textos vienen del diccionario (es y en)",
   all(vapply(c("tablas.pasivos.asistente.fx_fix", "tablas.pasivos.asistente.fx_sin", "tablas.flujo.aviso.tc_faltante"),
              function(k) !is.null(i18n_dict("es")[[k]]) && !is.null(i18n_dict("en")[[k]]), logical(1))))
ok("el texto del FIX lleva el valor y la fecha",
   identical(tr("tablas.pasivos.asistente.fx_fix", valor = "18.2346", fecha = "12/11/2026", idioma = "es"),
             "Tipo de cambio FIX de referencia: 18.2346 (12/11/2026)"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
