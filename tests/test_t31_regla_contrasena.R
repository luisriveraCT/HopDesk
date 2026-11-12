# =============================================================================
# tests/test_t31_regla_contrasena.R
# T31 (Luis, 2026-11-12): ONE password rule -- 8 to 128 characters, a digit
# and a symbol -- on every screen that sets a password. Each screen asks
# contrasena_error() (R/usuarios_cuentas.R) and none keeps its own length
# check: the activation had "6 + digit + symbol", Usuarios "8, nothing else".
# Existing passwords are not touched: the rule runs only when one is set.
# Run from project root:  Rscript tests/test_t31_regla_contrasena.R
# T31_ARCHIVOS="a.R;b.R" checks other copies (contraprueba), same order.
# =============================================================================
cat("=== test_t31_regla_contrasena ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# file -> how many places set a password there
PANTALLAS <- c("R/tiers_module.R" = 2L, "R/usuarios_cuentas.R" = 1L, "app.R" = 1L)
reales <- names(PANTALLAS)
otros <- strsplit(Sys.getenv("T31_ARCHIVOS", ""), ";", fixed = TRUE)[[1]]
if (length(otros)) reales <- otros

for (k in seq_along(PANTALLAS)) {
  f <- names(PANTALLAS)[k]
  x <- sub("#.*$", "", readLines(reales[k], warn = FALSE, encoding = "UTF-8"))
  propia <- grep("nchar\\((password|new_pass|pw1|pw|nueva)\\)\\s*<", x, value = TRUE)
  ok(sprintf("%s: ninguna regla de largo propia", f),
     { if (length(propia)) message("  ", paste(trimws(propia), collapse = " | ")); !length(propia) })
  n <- length(grep("contrasena_error\\(", x))
  ok(sprintf("%s: %d lugar(es) preguntan a contrasena_error() (hay %d)", f, PANTALLAS[[k]], n),
     n >= PANTALLAS[[k]])
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
