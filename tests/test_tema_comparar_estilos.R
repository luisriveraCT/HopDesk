# =============================================================================
# tests/test_tema_comparar_estilos.R
# The comparison test_tema_clasico_igual_chromote.R uses, without Chrome
# (tests/helpers/tema_comparar_estilos.R).
#
# Run from project root:  Rscript --vanilla tests/test_tema_comparar_estilos.R
# =============================================================================
cat("=== test_tema_comparar_estilos ===\n\n")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/tema_comparar_estilos.R")
cmp <- tema_comparar_valor
estado <- function(...) cmp(...)$estado

# The four cases asked for (Diseñador, 2026-11-20).
ok("rgba equals color(srgb) at ΔE 0: 'igual', also strict (de_max 0)",
   estado("background-color", "rgba(30, 58, 95, 0.07)", "color(srgb 0.117647 0.227451 0.372549 / 0.07)") == "igual" &&
     estado("background-color", "rgba(30, 58, 95, 0.07)", "color(srgb 0.117647 0.227451 0.372549 / 0.07)", de_max = 0) == "igual")
r <- cmp("border-top-color", "rgb(229, 233, 239)", "rgb(229, 231, 235)")   # #e5e9ef -> #E5E7EB
ok(sprintf("ΔE 1.37 passes as a warning ('aviso', ΔE %.2f)", r$de), r$estado == "aviso" && abs(r$de - 1.37) < 0.02)
ok("ΔE 1.37 fails when strict (de_max 0)",
   estado("border-top-color", "rgb(229, 233, 239)", "rgb(229, 231, 235)", de_max = 0) == "falla")
r <- cmp("color", "rgb(0, 0, 0)", "rgb(19, 19, 19)")
ok(sprintf("ΔE 6 fails (ΔE %.2f)", r$de), r$de > 5.5 && r$de < 6.5 && r$estado == "falla")
ok("alpha 0.05 apart fails",
   estado("background-color", "rgba(220, 38, 38, 0.06)", "rgba(220, 38, 38, 0.11)") == "falla")

# The rest of the rules.
ok("TEMA_DE_MAX 5 lets ΔE 3.35 through as a warning, 2 does not",
   estado("color", "rgb(221, 228, 237)", "rgb(229, 231, 235)", de_max = 5) == "aviso" &&
     estado("color", "rgb(221, 228, 237)", "rgb(229, 231, 235)", de_max = 2) == "falla")
ok("transparent equals rgba(0, 0, 0, 0) and any fully transparent colour",
   estado("background-color", "transparent", "rgba(0, 0, 0, 0)") == "igual" &&
     estado("background-color", "rgba(0, 0, 0, 0)", "color(srgb 0.5 0.2 0.9 / 0)") == "igual")
ok("not a colour property: exact (font-size 13px vs 13.0001px fails)",
   estado("font-size", "13px", "13.0001px") == "falla" && estado("font-size", "13px", "13px") == "igual")
ok("background-image stays exact (gradients are not colour properties here)",
   estado("background-image", "linear-gradient(rgb(0, 0, 0), rgb(1, 1, 1))",
          "linear-gradient(rgb(0, 0, 0), rgb(1, 1, 2))") == "falla")
ok("box-shadow: the colour within tolerance, the rest exact",
   estado("box-shadow", "rgba(0, 0, 0, 0.45) 0px 4px 20px 0px", "color(srgb 0 0 0 / 0.45) 0px 4px 20px 0px") == "igual" &&
     estado("box-shadow", "rgba(0, 0, 0, 0.45) 0px 4px 20px 0px", "rgba(0, 0, 0, 0.45) 0px 4px 21px 0px") == "falla" &&
     estado("box-shadow", "rgba(0, 0, 0, 0.45) 0px 4px 20px 0px", "rgba(0, 0, 0, 0.45) 0px 4px 20px 0px, rgb(0, 0, 0) 0px 0px 1px 0px") == "falla" &&
     estado("box-shadow", "none", "rgba(0, 0, 0, 0.45) 0px 4px 20px 0px") == "falla")
ok("an unreadable colour falls back to exact",
   estado("color", "currentcolor", "currentcolor") == "igual" && estado("color", "currentcolor", "rgb(0, 0, 0)") == "falla")

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
