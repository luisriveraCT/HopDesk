# =============================================================================
# tests/test_pv1_indice_recurrentes.R
# PV-1 (2026-11-22, paso 0 hopdesk_coord/tablas/reportes/PROVISIONES_VIVAS_paso0.md):
# a regular / tarjeta liability's occurrence_index is counted from its
# fecha_inicio, and the reconciler matches its rows by date. Before, the
# index was counted from the day the provisions were generated, so editing
# the liability a month (or a year) later moved every pending provision to
# other months and left months without one.
#   1. The index is absolute: generating on two days gives a date one index.
#   2. The damage already done (the old edit, one month and one year later)
#      is repaired on the next save: every month from today on has exactly
#      one provision; paid, converted and hand-edited rows are not touched.
#   3. A second save changes nothing.
#   4. Editing the day (5 -> 10) moves the k-th pending one to the new day,
#      without duplicates.
#   5. Nothing before a future fecha_inicio; without fecha_inicio, as before
#      but matched by date (an edit no longer moves anything).
#   6. The wizard passes the liability and the terms before the edit.
# Run: Rscript --vanilla tests/test_pv1_indice_recurrentes.R (repo root)
# =============================================================================
source("tests/helpers/pasivos_arnes.R")

pass <- 0L; fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  PASS ", label, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL ", label, "\n") }
}

renta <- function(day = 5L, inicio = as.Date("2025-11-01")) {
  l <- .schema_pasivos_liability()[1, ]
  l$id <- "L1"; l$categoria <- "regular"; l$estado <- "active"; l$nombre <- "Renta"
  l$empresa <- "NG"; l$moneda_pago <- "MXN"; l$amount_default <- 1000
  l$recurrence_type <- "monthly_day"; l$recurrence_params <- list(list(day = as.integer(day)))
  l$fecha_inicio <- as.Date(inicio); l$fecha_vence <- as.Date(NA)
  l
}
gen <- function(l, desde) pasivos_generate_provisions(l, window_start = desde, client_id = PASIVOS_CID)
aplicar <- function(r) dplyr::bind_rows(r$keep, r$conflicts, r$update, r$insert)
vivas <- function(t) t[!t$estado %in% "deleted", , drop = FALSE]
meses <- function(desde, hasta, dia = 5L) {
  m <- seq(as.Date(format(desde, "%Y-%m-01")), as.Date(hasta), by = "month")
  m <- as.Date(sprintf("%s-%02d", format(m, "%Y-%m"), dia))
  m[m >= desde & m <= hasta]
}
# The old wizard edit, to build the damage prod has: the generation counted
# from the day it runs (no fecha_inicio) and the reconciler by stored index.
edicion_vieja <- function(t, l, dia_edicion) {
  l0 <- l; l0$fecha_inicio <- as.Date(NA)
  aplicar(pasivos_reconcile_provisions(t, gen(l0, dia_edicion), mode = "regenerate"))
}

cat("\n== 1. The index is absolute ==\n")
l <- renta()
g_a <- gen(l, as.Date("2025-11-01")); g_b <- gen(l, as.Date("2026-11-22"))
comun <- intersect(g_a$fecha_calculada, g_b$fecha_calculada)
ok("1.1 the two generations share dates", length(comun) > 0)
ok("1.2 a date has the same index on both days",
   identical(g_a$occurrence_index[match(comun, g_a$fecha_calculada)],
             g_b$occurrence_index[match(comun, g_b$fecha_calculada)]))
ok("1.3 index k is the k-th date from fecha_inicio (dec-2026 = 14)",
   g_b$occurrence_index[g_b$fecha_calculada == as.Date("2026-12-05")] == 14L)

for (caso in list(list(nombre = "one month later", hoy = as.Date("2025-12-10"), pagado_hasta = as.Date("2025-12-01")),
                  list(nombre = "one year later",  hoy = as.Date("2026-11-22"), pagado_hasta = as.Date("2026-11-01")))) {
  cat("\n== 2. Repair, edit", caso$nombre, "==\n")
  l0 <- l; l0$fecha_inicio <- as.Date(NA)
  t <- gen(l0, as.Date("2025-11-01"))                  # created as prod did
  t$estado[t$fecha_efectiva < caso$pagado_hasta] <- "converted"
  # one pending row edited by hand (amount), far ahead
  k_mano <- which(t$estado == "provisional")[3]
  t$amount_pago_override[k_mano] <- 777
  danada <- edicion_vieja(t, l, caso$hoy)
  fut <- meses(caso$hoy, caso$hoy + 365)
  ok("2.0 the damage is there: months from today without a provision",
     length(setdiff(fut, vivas(danada)$fecha_calculada)) > 0)
  pagadas_antes <- danada[danada$estado != "provisional", ]
  mano_antes <- danada[!is.na(danada$amount_pago_override), ]

  # the next save, with PV-1
  g <- gen(l, caso$hoy)
  r <- pasivos_reconcile_provisions(danada, g, mode = "regenerate", liability = l, liability_antes = l)
  rep <- aplicar(r)
  v <- vivas(rep)
  hasta <- max(g$fecha_calculada)
  fut <- meses(caso$hoy, hasta)
  ok("2.1 every month from today on has a provision", all(fut %in% v$fecha_calculada))
  ok("2.2 no month has two", !any(duplicated(v$fecha_calculada[v$fecha_calculada >= caso$hoy])))
  ok("2.3 no date is duplicated at all", !any(duplicated(v$fecha_calculada)))
  pagadas_despues <- rep[rep$id %in% pagadas_antes$id, ]
  pagadas_despues <- pagadas_despues[match(pagadas_antes$id, pagadas_despues$id), ]
  ok("2.4 paid / converted rows are identical (index too)",
     nrow(pagadas_despues) == nrow(pagadas_antes) &&
       isTRUE(all.equal(as.data.frame(pagadas_despues), as.data.frame(pagadas_antes), check.attributes = FALSE)))
  mano <- rep[rep$id %in% mano_antes$id, ]
  ok("2.5 the hand-edited row keeps its amount and date",
     nrow(mano) == 1L && mano$amount_pago_override == 777 &&
       identical(as.Date(mano$fecha_efectiva), as.Date(mano_antes$fecha_efectiva)))
  ok("2.6 no row disappears", all(danada$id %in% rep$id))
  pend <- v[v$estado == "provisional" & v$fecha_calculada >= caso$hoy & is.na(v$amount_pago_override), ]
  ok("2.7 each pending row is on its own index's date",
     all(pend$fecha_calculada == pasivos_recurrencia_fechas(l, hasta)[pend$occurrence_index]))

  cat("\n== 3. A second save changes nothing (", caso$nombre, ") ==\n")
  r2 <- pasivos_reconcile_provisions(rep, gen(l, caso$hoy), mode = "regenerate", liability = l, liability_antes = l)
  rep2 <- aplicar(r2)
  ok("3.1 nothing inserted", nrow(r2$insert) == 0L)
  ok("3.2 same ids, same dates",
     setequal(rep2$id, rep$id) &&
       identical(as.Date(rep2$fecha_calculada[match(rep$id, rep2$id)]), as.Date(rep$fecha_calculada)))
}

cat("\n== 4. Editing the day: 5 -> 10 ==\n")
hoy <- as.Date("2026-03-01")
t <- gen(l, as.Date("2025-11-01"))
t$estado[t$fecha_efectiva < hoy] <- "converted"
l10 <- renta(day = 10L)
r <- pasivos_reconcile_provisions(t, gen(l10, hoy), mode = "regenerate", liability = l10, liability_antes = l)
rep <- vivas(aplicar(r))
pend <- rep[rep$estado == "provisional", ]
ok("4.1 the pending ones moved to the 10th", nrow(pend) > 0 && all(format(pend$fecha_calculada, "%d") == "10"))
ok("4.2 none left on the 5th from today", !any(pend$fecha_calculada >= hoy & format(pend$fecha_calculada, "%d") == "05"))
ok("4.3 one per month", !any(duplicated(format(pend$fecha_calculada, "%Y-%m"))))
ok("4.4 the paid ones stay on the 5th", all(format(rep$fecha_calculada[rep$estado == "converted"], "%d") == "05"))

cat("\n== 5. Future start; no start ==\n")
lf <- renta(inicio = as.Date("2026-06-01"))
gf <- gen(lf, as.Date("2026-01-15"))
ok("5.1 nothing before a future fecha_inicio", nrow(gf) > 0 && min(gf$fecha_calculada) >= as.Date("2026-06-01"))
ok("5.2 its first one is index 1", gf$occurrence_index[which.min(gf$fecha_calculada)] == 1L)
ln <- l; ln$fecha_inicio <- as.Date(NA)
t <- gen(ln, as.Date("2025-11-01"))
t$estado[t$fecha_efectiva < as.Date("2025-12-01")] <- "converted"
r <- pasivos_reconcile_provisions(t, gen(ln, as.Date("2025-12-10")), mode = "regenerate", liability = ln, liability_antes = ln)
v <- vivas(aplicar(r))
ok("5.3 without fecha_inicio an edit no longer moves the pending ones",
   all(t$fecha_calculada[t$estado == "provisional" & t$fecha_calculada >= as.Date("2025-12-10")] %in%
         v$fecha_calculada[v$estado == "provisional"]))
ok("5.4 nor duplicates them", !any(duplicated(v$fecha_calculada)))

cat("\n== 6. The wizard passes the terms ==\n")
# (its fallback when the reconciliation fails: tests/test_pasivos_reconciliar_respaldo.R)
src <- paste(readLines("R/pasivos_wizard_module.R", warn = FALSE), collapse = "\n")
ok("6.1 the wizard passes the liability and its previous terms",
   grepl("liability = liab_row", src, fixed = TRUE) && grepl("liability_antes = if (!is.null(before_row)", src, fixed = TRUE))

s3_memoria_restaurar()
cat(sprintf("\nPV-1: %d PASS / %d FAIL\n", pass, fail))
if (fail > 0L) quit(status = 1L)
