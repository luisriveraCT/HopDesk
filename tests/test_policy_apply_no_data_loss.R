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

# ── 2. La poda usa la llave completa: ledger y, desde T52b, la factura ──────
# The prune selects with .mudanza_alcanza() (R/data_pipeline.R), the readers'
# rule: ledger + Empresa + Moneda + Documento + the invoice's posting date.
# Executed here, not only read.
.chk_true(grepl("pruned <- man_db[!.mudanza_alcanza(man_db, new_moves, facturas), , drop = FALSE]", src, fixed = TRUE),
          "la poda selecciona con .mudanza_alcanza() (la regla de los lectores)")
.chk_true(!grepl('pol_keys <- paste(new_moves$Empresa, new_moves$Moneda', src, fixed = TRUE),
          "no queda ninguna pol_keys sin ledger")
.chk_true(!grepl('man_keys <- paste(man_db$Empresa, man_db$Moneda', src, fixed = TRUE),
          "no queda ninguna man_keys sin ledger")
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".politica_doc", ".politica_doc_unica", ".mudanza_doc", ".mudanza_fecha",
                                  ".mudanza_docs_unicos", ".mudanza_alcanza"))
    eval(e, globalenv())
{
  mov <- function(ledger, fecha) data.frame(ledger = ledger, Empresa = "NCS", Moneda = "MXN", Documento = "100",
                                            fecha_factura = as.Date(fecha), stringsAsFactors = FALSE)
  man  <- rbind(mov("AP", "2025-03-01"), mov("AP", "2026-10-01"))
  fact <- man
  .chk_true(identical(.mudanza_alcanza(man, mov("AR", "2026-10-01"), fact), c(FALSE, FALSE)),
            "una politica AR no poda el movimiento AP del mismo numero")
  .chk_true(identical(.mudanza_alcanza(man, mov("AP", "2026-10-01"), fact), c(FALSE, TRUE)),
            "la politica de la factura de 2026 no poda el movimiento de la de 2025 (mismo numero)")
  sin_fecha <- mov("AP", NA)
  .chk_true(identical(.mudanza_alcanza(sin_fecha, mov("AP", "2026-10-01"), fact), FALSE),
            "un movimiento sin fecha de un numero reusado no se poda (no llega a nadie)")
  .chk_true(identical(.mudanza_alcanza(sin_fecha, mov("AP", "2026-10-01"), fact[2, ]), TRUE),
            "un movimiento sin fecha de una sola factura se poda, como antes")
}

# ── 3. save_moves() sigue siendo exclusivo del scope destructivo ────────────
# Si aparece una segunda llamada, alguien amplio el borrado y hay que revisarlo
# a mano antes de que llegue a produccion.
.chk_true(length(gregexpr("save_moves(", src, fixed = TRUE)[[1]]) == 1L,
          "save_moves() se llama exactamente una vez en todo el archivo")
.chk_true(grepl('if (scope == "all") {', src, fixed = TRUE),
          "el borrado sigue encerrado tras la guarda de scope == 'all'")

# ── 4. El camino automatico tras refresco SAP jamas borra ───────────────────
# Since CM12a both triggers (SAP refresh, partner policies load) run one
# function, which adds on S3's current table through modificar_tabla.
blk  <- regmatches(src, regexpr("\\.auto_aplicar_politicas <- function.*?observeEvent\\(shared\\$sap_data\\(\\)", src))
.chk_true(length(blk) == 1 && !grepl("save_moves", blk, fixed = TRUE),
          "el bloque automatico post-SAP es aditivo: no llama save_moves()")
.chk_true(length(blk) == 1 && grepl("modificar_tabla(S3_KEYS$policy_moves", blk, fixed = TRUE) &&
            grepl(".policy_moves_sumar(fresco, new_moves, facturas)", blk, fixed = TRUE) &&
            !grepl("save_policy_moves(", blk, fixed = TRUE),
          "el bloque automatico suma sobre lo fresco de S3, nunca guarda la copia de la sesion")

# ── 5. Precedencia: el movimiento manual gana sobre la politica ─────────────
dp <- paste(readLines("R/data_pipeline.R", warn = FALSE), collapse = "\n")
.chk_true(grepl("coalesce(FechaVenc_Proyectada, FechaVenc_Politica", dp, fixed = TRUE),
          "FechaEff prioriza el movimiento manual sobre la fecha de politica")

cat(sprintf("\n  %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_policy_apply_no_data_loss.R: ", .fail, " failure(s)")
