# =============================================================================
# tests/test_t104_texto.R
# T104: a payment confirmation's origin account as shown (R/pagos_audit_viewer_module.R)
#   1  cuenta_foto(): the snapshot -- last 4 digits, alias, bank -- of a
#      catalog account; NULL if it isn't there or is in another currency
#   2  cuenta_confirmacion_texto(): always masked, "••••0201 · NCSBB — BanBajío";
#      the snapshot first, else the id in the catalog as it is now, else
#      "No registrada" -- never a guessed account, never the whole number
#   3  cuenta_id_de_numero(): an account NUMBER (the bank's report, its batch)
#      to its catalog id, in that currency; leading zeros don't matter; two
#      accounts with the same number are told apart by company, else NA
# Run from project root:  Rscript tests/test_t104_texto.R
# =============================================================================
cat("=== test_t104_texto ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
suppressMessages(source("R/i18n.R"))
source("tests/helpers/cuenta_origen_t104.R")
NR <- tr("tablas.archivos_pago.cuenta_no_registrada")

# ── 1 · The snapshot ─────────────────────────────────────────────────────────
f <- cuenta_foto("CT-NCS-MXN", CTAS_T104, BANCOS_T104)
ok("1 · la foto: últimos 4, alias y banco", identical(f, list(ultimos4 = "0201", alias = "NCSBB", banco = "BanBajío")))
ok("1 · en su moneda sí, en otra no (una cuenta USD no paga MXN)",
   !is.null(cuenta_foto("CT-NCS-MXN", CTAS_T104, BANCOS_T104, moneda = "mxn")) &&
     is.null(cuenta_foto("CT-NCS-USD", CTAS_T104, BANCOS_T104, moneda = "MXN")))
ok("1 · un id que no está, NA, \"\" o sin catálogo: NULL",
   is.null(cuenta_foto("borrada", CTAS_T104)) && is.null(cuenta_foto(NA, CTAS_T104)) &&
     is.null(cuenta_foto("", CTAS_T104)) && is.null(cuenta_foto("CT-NCS-MXN", NULL)))
ok("1 · sin catálogo de bancos: la foto sin banco, nunca un error",
   is.na(cuenta_foto("CT-NCS-MXN", CTAS_T104)$banco))
sin_cuenta <- transform(CTAS_T104[1, ], cuenta = "", clabe_interbancaria = "012180000073543099")
ok("1 · sin número de cuenta, los 4 de la CLABE", identical(cuenta_foto("CT-NCS-MXN", sin_cuenta)$ultimos4, "3099"))

# ── 2 · As shown ─────────────────────────────────────────────────────────────
ok("2 · con la foto: enmascarada, con alias y banco",
   identical(cuenta_confirmacion_texto(f), "••••0201 · NCSBB — BanBajío"))
ok("2 · la foto manda aunque la cuenta ya no exista (renombrada o borrada después)",
   identical(cuenta_confirmacion_texto(f, "borrada", CTAS_T104[0, ]), cuenta_confirmacion_texto(f)))
ok("2 · sin foto (de antes de T104): el id en el catálogo de hoy",
   identical(cuenta_confirmacion_texto(NULL, "CT-NCS-MXN", CTAS_T104, BANCOS_T104), cuenta_confirmacion_texto(f)))
ok("2 · id que ya no está, NA o sin catálogo: \"No registrada\", nunca otra cuenta",
   identical(cuenta_confirmacion_texto(NULL, "borrada", CTAS_T104), NR) &&
     identical(cuenta_confirmacion_texto(NULL, NA_character_, CTAS_T104), NR) &&
     identical(cuenta_confirmacion_texto(NULL, "CT-NCS-MXN", NULL), NR) && identical(cuenta_confirmacion_texto(), NR))
ok("2 · solo los 4 dígitos (el reporte sin cuenta en el catálogo): \"••••0201\", sin nombre",
   identical(cuenta_confirmacion_texto(list(ultimos4 = "0201")), "••••0201"))
ok("2 · banco vacío (null en el JSON del log): solo el alias",
   identical(cuenta_confirmacion_texto(list(ultimos4 = "0201", alias = "NCSBB", banco = NULL)),
             "••••0201 · NCSBB"))
ok("2 · una foto rota (sin 4 dígitos) no se muestra: \"No registrada\"",
   identical(cuenta_confirmacion_texto(list(ultimos4 = "73543010201")), NR) &&
     identical(cuenta_confirmacion_texto(list(alias = "NCSBB")), NR))
todos <- c(cuenta_confirmacion_texto(f), cuenta_confirmacion_texto(NULL, "CT-NCS-MXN", CTAS_T104))
ok("2 · nunca más de 4 dígitos seguidos ni el id", !any(grepl("[0-9]{5,}", todos)) && !any(grepl("CT-", todos)))

# ── 3 · From an account number ───────────────────────────────────────────────
ok("3 · el número del lote, con o sin cero inicial, a su id",
   identical(cuenta_id_de_numero("073543010201", CTAS_T104, "MXN"), "CT-NCS-MXN") &&
     identical(cuenta_id_de_numero("73543010201", CTAS_T104, "mxn"), "CT-NCS-MXN"))
ok("3 · en otra moneda, un número que no está, vacío o sin catálogo: NA",
   is.na(cuenta_id_de_numero("73543010201", CTAS_T104, "USD")) && is.na(cuenta_id_de_numero("999", CTAS_T104, "MXN")) &&
     is.na(cuenta_id_de_numero(NA, CTAS_T104, "MXN")) && is.na(cuenta_id_de_numero("73543010201", NULL, "MXN")))
dos <- rbind(CTAS_T104[1, ], transform(CTAS_T104[1, ], id = "CT-OTRA", Empresa = "OTR"))
ok("3 · el mismo número en dos empresas: la empresa decide; si no la dice, NA (nunca la primera)",
   identical(cuenta_id_de_numero("73543010201", dos, "MXN", "OTR"), "CT-OTRA") &&
     is.na(cuenta_id_de_numero("73543010201", dos, "MXN")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
