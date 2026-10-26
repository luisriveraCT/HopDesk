# =============================================================================
# tests/test_policy_apply_no_data_loss.R
#
# "Aplicar políticas" is the only place in the app that DELETES manual date
# overrides (invoice_moves). Two defects made it destroy work nobody asked it
# to touch; the S3 version history of networks/invoice_moves.rds shows the
# damage -- drops of 475, 138, 73 and 48 rows in a single write during July
# and early August 2026.
#
#  1. scope defaulted to "all" -- the ONE destructive scope -- whenever
#     input$cmp_apply_scope was NULL. A Shiny radio that has not rendered
#     reports NULL, so a confirm could run the destructive path with nobody
#     ever choosing it.
#  2. The prune key was (Empresa, Moneda, Documento) while every other key in
#     the same function is (ledger, Empresa, Moneda, Documento). AR and AP
#     reuse document numbers, so an AR policy deleted the manual move of the
#     same-numbered AP invoice.
#
# Source-level assertions: this handler needs a live Shiny session, so the
# guarantees are pinned against the source rather than executed.
# =============================================================================

cat("\n== Aplicar políticas no destruye movimientos manuales ==\n")

.pass <- 0L; .fail <- 0L
.chk_true <- function(cond, label) {
  if (isTRUE(cond)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

src <- paste(readLines("R/settings/settings_companies.R", warn = FALSE), collapse = "\n")

# ── 1. El default nunca puede ser el scope destructivo ──────────────────────
.chk_true(grepl('isolate(input$cmp_apply_scope) %||% "new"', src, fixed = TRUE),
          'el handler de confirmacion cae en "new" (aditivo), no en "all"')
.chk_true(!grepl('isolate(input$cmp_apply_scope) %||% "all"', src, fixed = TRUE),
          'ya no queda ningun default a "all" en el handler')

# ── 2. La poda usa la llave completa, con ledger ────────────────────────────
.chk_true(grepl("pol_keys <- paste(new_moves$ledger, new_moves$Empresa", src, fixed = TRUE),
          "pol_keys incluye ledger")
.chk_true(grepl("man_keys <- paste(man_db$ledger, man_db$Empresa", src, fixed = TRUE),
          "man_keys incluye ledger")
.chk_true(!grepl('pol_keys <- paste(new_moves$Empresa, new_moves$Moneda', src, fixed = TRUE),
          "no queda ninguna pol_keys sin ledger")
.chk_true(!grepl('man_keys <- paste(man_db$Empresa, man_db$Moneda', src, fixed = TRUE),
          "no queda ninguna man_keys sin ledger")

# ── 3. save_moves() sigue siendo exclusivo del scope destructivo ────────────
# Si aparece una segunda llamada, alguien amplio el borrado y hay que revisarlo
# a mano antes de que llegue a produccion.
.chk_true(length(gregexpr("save_moves(", src, fixed = TRUE)[[1]]) == 1L,
          "save_moves() se llama exactamente una vez en todo el archivo")
.chk_true(grepl('if (scope == "all") {', src, fixed = TRUE),
          "el borrado sigue encerrado tras la guarda de scope == 'all'")

# ── 4. El camino automatico tras refresco SAP jamas borra ───────────────────
auto <- sub('.*Auto-aplicación de políticas tras refresco SAP.*', '', src)
blk  <- regmatches(src, regexpr("Keep all existing policy dates.*?truly_new", src))
.chk_true(length(blk) == 1 && !grepl("save_moves", blk, fixed = TRUE),
          "el bloque automatico post-SAP es aditivo: no llama save_moves()")

# ── 5. Precedencia: el movimiento manual gana sobre la politica ─────────────
dp <- paste(readLines("R/data_pipeline.R", warn = FALSE), collapse = "\n")
.chk_true(grepl("coalesce(FechaVenc_Proyectada, FechaVenc_Politica", dp, fixed = TRUE),
          "FechaEff prioriza el movimiento manual sobre la fecha de politica")

cat(sprintf("\n  %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_policy_apply_no_data_loss.R: ", .fail, " failure(s)")
