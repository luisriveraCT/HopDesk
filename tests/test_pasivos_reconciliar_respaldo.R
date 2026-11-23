# =============================================================================
# tests/test_pasivos_reconciliar_respaldo.R
# The wizard's save never loses a liability's provisions when the
# reconciliation fails (found 2026-11-22, PV-1). Before, its tryCatch fallback
# kept NONE of the liability's existing provisions and inserted every
# generated one, so the save removed the paid, converted and hand-edited rows
# from S3. pasivos_reconciliar_o_conservar() (R/pasivos_wizard_module.R) keeps
# them exactly, inserts nothing and says so.
#   1. Reconciliation failing: the rows stay identical; nothing inserted.
#   2. Contraprueba: the old fallback, applied the way the save applies it,
#      drops every one of them.
#   3. Reconciliation working: the result is the reconciler's, unchanged.
# Run: Rscript --vanilla tests/test_pasivos_reconciliar_respaldo.R (repo root)
# =============================================================================
source("tests/helpers/pasivos_arnes.R")
.extract_fn("R/pasivos_wizard_module.R", "pasivos_reconciliar_o_conservar")

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
existentes <- pasivos_generate_provisions(l, window_start = as.Date("2025-11-01"), client_id = PASIVOS_CID)[1:4, ]
existentes$estado[1:2] <- c("converted", "item_confirmed")      # paid / converted
existentes$amount_pago_override[4] <- 777                        # edited by hand
generadas <- pasivos_generate_provisions(l, window_start = as.Date("2025-12-10"), client_id = PASIVOS_CID)
otros <- existentes[0, ]   # the other liabilities' rows, as the save binds them

cat("\n== 1. The reconciliation fails ==\n")
original <- pasivos_reconcile_provisions
pasivos_reconcile_provisions <- function(...) stop("boom")
r <- pasivos_reconciliar_o_conservar(existentes, generadas, mode = "regenerate", nota_anterior = NA_character_)
pasivos_reconcile_provisions <- original
final <- dplyr::bind_rows(otros, r$keep, r$conflicts, r$update, r$insert)   # what the save writes
ok("1.1 every existing row is kept", nrow(final) == nrow(existentes) && setequal(final$id, existentes$id))
ok("1.2 identical, field by field",
   isTRUE(all.equal(as.data.frame(final[match(existentes$id, final$id), ]), as.data.frame(existentes),
                    check.attributes = FALSE)))
ok("1.3 nothing inserted", nrow(r$insert) == 0L)
ok("1.4 the error is reported", identical(r$error, "boom"))

cat("\n== 2. Contraprueba: the old fallback ==\n")
viejo <- list(keep = .schema_pasivos_provision(), update = .schema_pasivos_provision(),
              insert = generadas, conflicts = .schema_pasivos_provision())
final_viejo <- dplyr::bind_rows(otros, viejo$keep, viejo$conflicts, viejo$update, viejo$insert)
ok("2.1 the old fallback dropped all of them (paid, converted, edited)", !any(existentes$id %in% final_viejo$id))

cat("\n== 3. The reconciliation works ==\n")
directo <- pasivos_reconcile_provisions(existentes, generadas, mode = "regenerate", nota_anterior = NA_character_)
envuelto <- pasivos_reconciliar_o_conservar(existentes, generadas, mode = "regenerate", nota_anterior = NA_character_)
ok("3.1 same parts as the reconciler", setequal(names(envuelto), names(directo)))
ok("3.2 same rows in each part",
   all(vapply(names(directo), function(k) setequal(envuelto[[k]]$id, directo[[k]]$id) ||
                (nrow(envuelto[[k]]) == nrow(directo[[k]]) && k == "insert"), logical(1))))

s3_memoria_restaurar()
cat(sprintf("\nRespaldo del reconciliador: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
