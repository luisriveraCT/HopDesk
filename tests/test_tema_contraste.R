# =============================================================================
# tests/test_tema_contraste.R
# TEMA: the WCAG 2.x contrast checker (R/tema_contraste.R) against known
# pairs. Reference values: WCAG's own definition (black/white = 21) and the
# WebAIM contrast checker (#777 on white = 4.48, #767676 = 4.54, the lightest
# gray that passes AA on white).
#
# Run from project root:  Rscript --vanilla tests/test_tema_contraste.R
# =============================================================================
cat("=== test_tema_contraste ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
casi <- function(a, b, tol = 0.005) isTRUE(all(abs(a - b) < tol))
falla <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")

# Pure file: only function definitions at the top (rule 29), so it can be
# sourced alone and in any order.
ex <- parse("R/tema_contraste.R", keep.source = FALSE, encoding = "UTF-8")
ok("R/tema_contraste.R solo define funciones arriba",
   all(vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")), logical(1))))
source("R/tema_contraste.R")

cat("\n-- Luminancia --\n")
ok("negro = 0, blanco = 1", casi(tema_luminancia(c("#000000", "#FFFFFF")), c(0, 1), 1e-12))
ok("#777777 = 0.1845", casi(tema_luminancia("#777777"), 0.1845, 1e-4))
ok("el verde pesa más que el rojo y el azul (0.7152 / 0.2126 / 0.0722)",
   casi(tema_luminancia(c("#FF0000", "#00FF00", "#0000FF")), c(0.2126, 0.7152, 0.0722), 1e-12))

cat("\n-- Pares conocidos --\n")
ok("#000 / #fff = 21", casi(tema_contraste("#000", "#fff"), 21, 1e-9))
ok("#777 / #fff ≈ 4.48 (4.478)", casi(tema_contraste("#777", "#fff"), 4.478, 0.001))
ok("#767676 / #fff ≈ 4.54", casi(tema_contraste("#767676", "#fff"), 4.54, 0.005))
ok("un color consigo mismo = 1", casi(tema_contraste("#13213A", "#13213A"), 1, 1e-12))
ok("primario Clásico: blanco / #0A58CA ≈ 6.44", casi(tema_contraste("#FFFFFF", "#0A58CA"), 6.44, 0.005))
ok("btn-success de flatly: blanco / #18BC9C ≈ 2.41", casi(tema_contraste("#FFFFFF", "#18BC9C"), 2.41, 0.005))
ok("--hd-tinta / --hd-papel ≈ 14.71", casi(tema_contraste("#1A2233", "#F7F6F2"), 14.71, 0.005))

cat("\n-- Forma --\n")
ok("el orden no importa", casi(tema_contraste("#fff", "#777"), tema_contraste("#777", "#fff"), 1e-12))
ok("#rgb = #rrggbb", casi(tema_contraste("#abc", "#123"), tema_contraste("#AABBCC", "#112233"), 1e-12))
ok("mayúsculas = minúsculas", casi(tema_contraste("#c9ae6e", "#ffffff"), tema_contraste("#C9AE6E", "#FFFFFF"), 1e-12))
ok("sin # también", casi(tema_contraste("777777", "ffffff"), tema_contraste("#777777", "#ffffff"), 1e-12))
ok("vectorizado y con reciclaje",
   casi(tema_contraste(c("#000", "#777", "#fff"), "#fff"), c(21, 4.478, 1), 0.001))
ok("NA da NA", identical(is.na(tema_contraste(c("#000", NA), "#fff")), c(FALSE, TRUE)))
ok("siempre entre 1 y 21", {
  set.seed(1)
  hx <- sprintf("#%06X", sample.int(16777215L, 200L))
  r <- tema_contraste(hx, rev(hx)); all(r >= 1 & r <= 21)
})

cat("\n-- Rechaza lo que no es un color opaco --\n")
ok("con alfa (#rrggbbaa) falla", falla(tema_contraste("#00000080", "#fff")))
ok("con alfa (#rgba) falla", falla(tema_luminancia("#0008")))
ok("un nombre (white) falla", falla(tema_luminancia("white")))
ok("rgb() falla", falla(tema_luminancia("rgb(0,0,0)")))
ok("el error nombra el valor malo",
   grepl("zzz", tryCatch(tema_luminancia(c("#fff", "#zzz")), error = conditionMessage)))

cat("\n-- AA --\n")
ok("#777 / #fff no pasa AA (4.478 < 4.5: no se redondea)", identical(tema_pasa_aa("#777", "#fff"), FALSE))
ok("#767676 / #fff pasa AA", identical(tema_pasa_aa("#767676", "#fff"), TRUE))
ok("#777 / #fff sí pasa como texto grande (>= 3)", identical(tema_pasa_aa("#777", "#fff", grande = TRUE), TRUE))
ok("--hd-oro-osc / blanco no pasa ni como texto grande (2.97)", identical(tema_pasa_aa("#B0924F", "#FFFFFF", grande = TRUE), FALSE))
ok("vectorizado, con `grande` por par",
   identical(tema_pasa_aa(c("#777", "#777"), "#fff", grande = c(FALSE, TRUE)), c(FALSE, TRUE)))

cat(sprintf("\n=== %d PASS, %d FAIL ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
