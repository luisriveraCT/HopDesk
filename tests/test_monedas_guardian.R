# =============================================================================
# tests/test_monedas_guardian.R
# Monedas fase 1 (Luis, 2026-11-16: "cero hardcoding"). Counts every currency
# code written by hand in the app's code -- a string literal "XXX" that is in
# the ISO catalog (R/monedas.R) -- outside R/monedas.R and comments.
#
# Ratchet: the count may only go down. TOPE is today's count; this test fails
# when the code has more, and asks to lower TOPE when it has fewer, so each
# migration (monedas fase 3) locks its gain in. At zero (minus EXCEPCIONES,
# rules of a bank or an ERP, each with its reason) it is a plain guardian.
#
# Run from project root:  Rscript tests/test_monedas_guardian.R
# =============================================================================
cat("=== test_monedas_guardian ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/monedas.R")

TOPE <- 61L    # 2026-11-17: 222 (fase 1) -> 194 (fase 2, Flujo + R/fx_banxico.R) -> 118 (fase 3: T33 Pasivos -43, T34 Pagar hoy -33) -> 92 (T35 Reporte -26) -> 87 (T36: plan total, providers Excel) -> 74 (T36: Mapa de tesorería) -> 61 (T36: Proveedores); lowered by every migration

# R/monedas.R is never counted: it is the catalog, and it holds the one
# deliberate code, MONEDA_BASE_OMISION = "MXN" (Luis, 2026-11-16: "es una app
# mexicana"), the base of a client that has not chosen one.
# Legitimate: a currency that is a rule of the outside world, not a choice.
# file = reason. Their literals are not counted.
EXCEPCIONES <- c(
  "R/bancos_detect.R" = "BanBajío's statement header names its currencies",
  "R/fx_banxico.R"    = "Banxico: FIX USD→MXN and its series table",
  "R/ppl_generator.R" = "BanBajío's PPL layout: MXN by SPEI/BCO, USD by SPID; its accounts' \"Pesos\"/\"Dólares\""
)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

archivos <- c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE))
archivos <- setdiff(archivos, c("R/monedas.R", names(EXCEPCIONES)))
codigos <- monedas_catalogo()

contar <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x <- sub("#.*$", "", x)      # comments out (a "#" inside a string is rare enough)
  lit <- unlist(regmatches(x, gregexpr("[\"'][A-Z]{3}[\"']", x)))
  sum(substr(lit, 2, 4) %in% codigos)
}
por_archivo <- vapply(archivos, contar, integer(1))
total <- sum(por_archivo)
cat(sprintf("  currency codes written by hand: %d (TOPE %d)\n", total, TOPE))
top <- utils::head(sort(por_archivo[por_archivo > 0], decreasing = TRUE), 10)
for (n in names(top)) cat(sprintf("    %3d  %s\n", top[[n]], n))

ok(total <= TOPE, sprintf("no new currency written by hand (%d <= %d); use R/monedas.R", total, TOPE))
ok(total >= TOPE, sprintf("TOPE is up to date: lower it to %d in this file", total))
ok(all(file.exists(names(EXCEPCIONES))), "every exception names a file that exists")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
