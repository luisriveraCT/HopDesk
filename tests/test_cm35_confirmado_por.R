# =============================================================================
# tests/test_cm35_confirmado_por.R
# CM35 (H1). bancos_confirmados said when a payment was confirmed or undone,
# never by whom, so the requisition timeline could not say who paid it.
# Forward only: rows written before CM35 keep NA ("por —").
#   1  the schema has confirmado_por and eliminado_por; an old table without
#      them normalizes to NA;
#   2  recover_confirmacion() stores its `actor` (it took one and dropped it):
#      eliminado_por on the original row, confirmado_por on the "recovered"
#      row; a table read before CM35 (no such columns) still works;
#   3  every site that soft-deletes a confirmation sets eliminado_por next
#      to eliminado_at (Bancos "Eliminar confirmación", Pasivos deleting a
#      paid provision), and every site that creates one sets confirmado_por
#      (Agenda AP/AR).
#
# Run from project root:  Rscript tests/test_cm35_confirmado_por.R
# =============================================================================
cat("=== test_cm35_confirmado_por ===\n\n")
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
source("R/bancos_persistence.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. Schema ─────────────────────────────────────────────────────────────
ok("1 the schema has confirmado_por and eliminado_por",
   all(c("confirmado_por", "eliminado_por") %in% names(.schema_bancos_confirmados())))
vieja <- tibble::tibble(confirmacion_id = "c1", agenda_item_id = "a1", empresa = "NRS", importe = 10,
                        moneda = "MXN", confirmado_at = as.POSIXct("2026-11-01", tz = "UTC"),
                        eliminado = FALSE, provision_id = "p1")
n <- .normalize(vieja, .schema_bancos_confirmados)
ok("1 an old row (before CM35) normalizes with both as NA, nothing guessed",
   is.na(n$confirmado_por) && is.na(n$eliminado_por))

# ── 2. recover_confirmacion() stores its actor ─────────────────────────────
r <- recover_confirmacion(n, "c1", actor = "ana")
orig <- r[r$confirmacion_id == "c1", ]
rec  <- r[r$action %in% "recovered", ]
ok("2 the undone original row says who undid it (eliminado_por = ana)",
   isTRUE(orig$eliminado) && identical(orig$eliminado_por, "ana"))
ok("2 the 'recovered' row says who recovered it (confirmado_por = ana), and is not itself deleted",
   nrow(rec) == 1L && identical(rec$confirmado_por, "ana") && is.na(rec$eliminado_por))
# Read before CM35 = normalized with the old schema: every column but the two new ones.
pre_cm35 <- n[, setdiff(names(n), c("confirmado_por", "eliminado_por"))]
r2 <- tryCatch(recover_confirmacion(pre_cm35, "c1", actor = "ana"), error = function(e) e)
ok("2 a table read before CM35 (no such columns) is recovered too, with the actor",
   is.data.frame(r2) && identical(r2$eliminado_por[r2$confirmacion_id == "c1"], "ana"))

# ── 3. Every site that confirms or undoes says who ───────────────────────
codigo <- function(f) { x <- readLines(f, warn = FALSE, encoding = "UTF-8"); x[grepl("^\\s*#", x)] <- ""; x }
# Soft-deletes of a CONFIRMATION (conf / c), not of a bank movement (movs).
sin_por <- function(f) {
  x <- codigo(f)
  i <- grep("\\b(conf|c)\\$eliminado_at\\[", x)
  i[!vapply(i, function(k) any(grepl("eliminado_por", x[max(1L, k - 2L):min(length(x), k + 2L)])), logical(1))]
}
for (f in c("R/bancos_module.R", "R/pasivos_module.R")) {
  m <- sin_por(f)
  for (k in m) message("  without eliminado_por: ", f, ":", k)
  ok(sprintf("3 %s: every confirmation soft-delete sets eliminado_por", f), !length(m))
}
ph <- codigo("R/pagar_hoy_module.R")
crea <- grep("confirmacion_id = uuid::UUIDgenerate()", ph, fixed = TRUE)
ok(sprintf("3 pagar_hoy_module.R: both Agenda confirmations (found %d) set confirmado_por", length(crea)),
   length(crea) == 2L && all(vapply(crea, function(k) any(grepl("confirmado_por", ph[k:min(length(ph), k + 25L)])), logical(1))))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
