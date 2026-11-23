# =============================================================================
# tests/test_pasivos_reconciliar_overrides.R
# Editing a liability in the wizard never erases what was edited by hand on
# its pending provisions (found 2026-11-22, PV-1). The reconciler's
# "regenerate" copied every generated column onto the existing row, the
# *_override columns too (NA in a generated row): renaming the liability was
# enough to erase every hand-edited amount and date.
#   1. An edit that changes no amount or date (the name): the hand edits stay.
#   2. The rows without hand edits still take the new terms.
#   3. A change the hand edit would hide is still a conflict, untouched.
# Contraprueba: on 1a54cda (before the fix) 1.1 and 1.2 fail (NA).
# Run: Rscript --vanilla tests/test_pasivos_reconciliar_overrides.R (repo root)
# =============================================================================
source("tests/helpers/pasivos_arnes.R")

pass <- 0L; fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  PASS ", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL ", label, "\n") }
}

l <- .schema_pasivos_liability()[1, ]
l$id <- "L1"; l$categoria <- "regular"; l$estado <- "active"; l$nombre <- "Renta"
l$empresa <- "NG"; l$moneda_pago <- "MXN"; l$amount_default <- 1000
l$recurrence_type <- "monthly_day"; l$recurrence_params <- list(list(day = 5L))
l$fecha_inicio <- as.Date("2025-11-01"); l$fecha_vence <- as.Date(NA)
l$documento_template <- "R-{YYYY}{MM}"
hoy <- as.Date("2025-11-01")
t <- pasivos_generate_provisions(l, window_start = hoy, client_id = PASIVOS_CID)
t$amount_pago_override[3]    <- 777
t$fecha_efectiva_override[5] <- as.Date("2026-03-20")
aplicar <- function(r) dplyr::bind_rows(r$keep, r$conflicts, r$update, r$insert)

cat("\n== 1. Renaming the liability ==\n")
l2 <- l; l2$nombre <- "Renta oficina"; l2$documento_template <- "RO-{YYYY}{MM}"
r <- pasivos_reconcile_provisions(t, pasivos_generate_provisions(l2, window_start = hoy, client_id = PASIVOS_CID),
                                  mode = "regenerate")
fin <- aplicar(r)
de <- function(i, cl) fin[[cl]][fin$id == t$id[i]]
ok("1.1 the hand-edited amount stays", identical(de(3, "amount_pago_override"), 777))
ok("1.2 the hand-edited date stays", identical(de(5, "fecha_efectiva_override"), as.Date("2026-03-20")))
ok("1.3 no conflict (the edit touched none of them)", nrow(r$conflicts) == 0L)
ok("1.4 no row lost or added", setequal(fin$id, t$id))

cat("\n== 2. The others take the new terms ==\n")
ok("2.1 a row without hand edits takes the new document", grepl("^RO-", de(2, "documento")))

cat("\n== 3. A real conflict is still one ==\n")
l3 <- l; l3$amount_default <- 1500
r3 <- pasivos_reconcile_provisions(t, pasivos_generate_provisions(l3, window_start = hoy, client_id = PASIVOS_CID),
                                   mode = "regenerate")
ok("3.1 the hand-edited amount's row is a conflict", t$id[3] %in% r3$conflicts$id)
ok("3.2 and is left exactly as it was",
   isTRUE(all.equal(as.data.frame(r3$conflicts[r3$conflicts$id == t$id[3], ]), as.data.frame(t[3, ]),
                    check.attributes = FALSE)))
ok("3.3 a row without hand edits takes the new amount",
   aplicar(r3)$amount_pago[aplicar(r3)$id == t$id[2]] == 1500)

s3_memoria_restaurar()
cat(sprintf("\nOverrides en el reconciliador: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
