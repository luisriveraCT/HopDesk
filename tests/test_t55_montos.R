# =============================================================================
# tests/test_t55_montos.R
# T55: one format for every amount, monto_moneda() ("1,234.56 MXN", no "$").
# Part 1: monto_moneda() moves to R/monedas.R (the engine of tables uses it
# too, and everything loads that file), with decimales= and codigo=, and
# formats per group of decimals. With its defaults it is identical() to the
# T46 version.
#
# Run from project root:  Rscript --vanilla tests/test_t55_montos.R
# =============================================================================
cat("=== test_t55_montos ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R")
MENOS <- "−"; RAYA <- "—"

# The T46 version (R/ui_components.R at 3ec07df + ae31eb7), as the reference.
t46 <- function(x, moneda) {
  n <- max(length(x), length(moneda))
  if (!n) return(character())
  x <- rep_len(suppressWarnings(as.numeric(x)), n)
  moneda <- rep_len(toupper(trimws(as.character(moneda))), n)
  dec <- moneda_decimales(moneda); dec[is.na(dec)] <- 2L
  cifra <- vapply(seq_len(n), function(i) if (is.na(x[i])) "" else
    formatC(abs(round(x[i], dec[i])), format = "f", digits = dec[i], big.mark = ","), character(1))
  signo <- ifelse(!is.na(x) & round(x, dec) < 0, "−", "")
  codigo <- ifelse(is.na(moneda) | !nzchar(moneda), "", paste0(" ", moneda))
  ifelse(is.na(x), "—", paste0(signo, cifra, codigo))
}

cat("-- one definition, in R/monedas.R --\n")
defs <- unlist(lapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)), function(f)
  if (any(grepl("^monto_moneda <- function", readLines(f, warn = FALSE)))) f))
ok("monto_moneda() is defined once, in R/monedas.R", identical(defs, "R/monedas.R"))
src <- paste(readLines("R/monedas.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("its minus sign and dash are escaped (\\u2212, \\u2014), not literal characters",
   grepl('"\\u2212"', src, fixed = TRUE) && grepl('"\\u2014"', src, fixed = TRUE) &&
     !grepl("−|—", sub("(?s)^.*monto_moneda <- function", "", src, perl = TRUE)))

cat("\n-- with its defaults: identical() to the T46 version --\n")
x   <- c(842300, -10950296, 0, -0.004, 1234.4, 1.5, NA, 2500.5, 999999.995, 0.5, -0.5, 12.345, 1e9)
mon <- c("MXN", "MXN", "USD", "USD", "JPY", "KWD", "EUR", "usd ", "MXN", "JPY", "JPY", "ZZZ", NA)
ok("a mixed vector (MXN, USD, JPY, KWD, NA amount, a code outside the catalog, no code, negatives, rounding edges)",
   identical(monto_moneda(x, mon), t46(x, mon)))
ok("recycling and empty input, as before",
   identical(monto_moneda(c(1, 2.5), "MXN"), t46(c(1, 2.5), "MXN")) &&
     identical(monto_moneda(numeric(), character()), character()))
ok("NA is still the dash, negative the true minus",
   identical(monto_moneda(NA, "MXN"), RAYA) && identical(monto_moneda(-5, "MXN"), paste0(MENOS, "5.00 MXN")))
set.seed(55)
xx <- round(runif(3000, -2e7, 2e7), sample(0:3, 3000, TRUE)); mm <- sample(c("MXN", "USD", "JPY", "KWD", "CLP", NA), 3000, TRUE)
xx[sample(3000, 50)] <- NA
ok("3,000 random amounts in six currencies: identical() to the T46 version", identical(monto_moneda(xx, mm), t46(xx, mm)))

cat("\n-- decimales= and codigo= --\n")
ok("decimales = 0: whole units for every currency (the Calendar's cells)",
   identical(monto_moneda(c(432152.4, 1387735.5, -7673.2), "MXN", decimales = 0),
             c("432,152 MXN", "1,387,736 MXN", paste0(MENOS, "7,673 MXN"))))
ok("codigo = FALSE: the number alone, with the catalog's decimals",
   identical(monto_moneda(c(2550363.49, 1234, 1.5), c("MXN", "JPY", "KWD"), codigo = FALSE),
             c("2,550,363.49", "1,234", "1.500")))
ok("both together", identical(monto_moneda(598265.2, "MXN", decimales = 0, codigo = FALSE), "598,265"))
ok("NA stays the dash with either", identical(monto_moneda(NA, "MXN", decimales = 0, codigo = FALSE), RAYA))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
