# =============================================================================
# tests/test_pagar_hoy_annot_cache.R
# Agenda de Hoy's catalog annotation cache (.annotate_ap_cached), on the real
# pagarHoyServer in testServer, through the real payment preview and PPL
# download. Found 2026-11-01 (T06 inventory): the cache returned the WHOLE
# cached row, keyed on Parte/Documento/Codigo only, and nothing but
# link/unlink cleared it, so:
#   1. an Importe edited in the Calendar (.sync_staged) -> the preview and
#      the PPL file paid the OLD amount;
#   2. a CLABE corrected in Proveedores -> the PPL paid the OLD account;
#   3. a date moved -> the table's order (by the cached FechaVenc) no longer
#      matched the confirm handlers' order (by the fresh one);
#   4. an invoice taken out and re-staged as an abono -> the old id, the
#      full amount and no ABONO;
#   5. a session alias override (the ⇕ button) still re-annotates, and the
#      link/unlink handlers still clear the cache.
# Every check here fails on ab9a5b9. PH_MODULE=<path> runs it against another
# copy of R/pagar_hoy_module.R (to show that).
#
# No S3: the shared reactives are in memory, and the few helpers that would
# reach S3 or the audit log are stubbed below, before the server runs.
# Run from project root:  Rscript tests/test_pagar_hoy_annot_cache.R
# =============================================================================
cat("=== test_pagar_hoy_annot_cache ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
fmt_money <- function(x) paste0("$ ", formatC(x, format = "f", digits = 2, big.mark = ","))

for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/ppl_generator.R"))
  source(f)
# The raw SAP viewer the module makes once (search_module.R, T21).
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
source(Sys.getenv("PH_MODULE", "R/pagar_hoy_module.R"))

# Stubs (after sourcing, so they win over the module's own top-level helpers).
ultimo_ppl <- NULL; ultimo_modal <- NULL
.log_pago_generado <- function(shared, emp, moneda, cuenta_origen, ppl_rows, ...) ultimo_ppl <<- ppl_rows
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showModal <- function(ui, ...) ultimo_modal <<- as.character(ui)
removeModal <- function(...) invisible(NULL)
get_cuentas_ppl_data <- function(emp, cur, shared)
  data.frame(cuenta = "012180001111111111", banco = "012", alias = "Operativa", is_ppl_default = TRUE, stringsAsFactors = FALSE)
get_cuenta_ppl <- function(emp, cur, shared) "012180001111111111"

CLABE_ACME  <- "012345678901234567"; CLABE_ACME2 <- "072180009999999999"
CLABE_BETA  <- "014180002222222222"
catalogo <- function(clabe_acme = CLABE_ACME) tibble::tibble(
  nombre = c("ACME SA", "BETA SA", "ACME SA"), alias = c("ACME", "BETA", "ACME2"),
  clabe = c(clabe_acme, CLABE_BETA, CLABE_ACME2), medio_pago = "SPEI", banco = c("012", "014", "072"),
  Empresa = NA_character_)
fila <- function(id, parte, doc, imp, fecha, tipo = NA_character_) tibble::tibble(
  id = id, ledger = "AP", Empresa = "Networks", Moneda = "MXN", Documento = doc, Parte = parte,
  Codigo = "", Importe = imp, FechaVenc = as.Date(fecha), status = "pending", tipo_item = tipo,
  source = "manual", provision_id = NA_character_)

shared <- list(
  company_map = reactiveVal(c(NTS = "Networks")), proveedores_db = reactiveVal(catalogo()),
  parte_alias_map_db = reactiveVal(NULL),
  pagar_hoy_db = reactiveVal(fila("a", "ACME SA", "D1", 1000, "2026-11-01")),
  sap_data = reactiveVal(list()), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
  effective_client_id = function() "dev", current_user = function() "probe",
  home_client_id = function() "dev")

testServer(pagarHoyServer, args = list(shared = shared), {
  session$flushReact()
  vista_previa <- function() {
    ultimo_modal <<- NULL
    session$setInputs(dl_preview_Networks = runif(1)); ultimo_modal %||% ""
  }
  ppl <- function() { ultimo_ppl <<- NULL; f <- output$dl_ppl_Networks; list(filas = ultimo_ppl, texto = paste(readLines(f, warn = FALSE), collapse = "\n")) }

  # Warm the cache, as the table and the header do.
  p0 <- ppl()
  ok("baseline: the PPL pays 1000 to ACME's CLABE", identical(p0$filas$importe, 1000) && identical(p0$filas$clabe_dest, CLABE_ACME))

  # 1. Importe edited in the Calendar (same id, same Parte/Documento)
  shared$pagar_hoy_db(fila("a", "ACME SA", "D1", 1200, "2026-11-01")); session$flushReact()
  html <- vista_previa()
  ok("1: the preview shows the new amount (1,200.00), not the old one",
     grepl("1,200.00", html, fixed = TRUE) && !grepl("1,000.00", html, fixed = TRUE))
  p1 <- ppl()
  ok("1: the PPL pays the new amount (1200)", identical(p1$filas$importe, 1200))
  ok("1: and the PPL file's own bytes carry it (15 digits in centavos: 000000000120000)",
     grepl("000000000120000", p1$texto, fixed = TRUE) && !grepl("000000000100000", p1$texto, fixed = TRUE))

  # 2. CLABE corrected in Proveedores
  shared$proveedores_db(catalogo(clabe_acme = "021180003333333333")); session$flushReact()
  p2 <- ppl()
  ok("2: the PPL pays to the NEW CLABE", identical(p2$filas$clabe_dest, "021180003333333333"))
  ok("2: and the old CLABE is nowhere in the file", !grepl(CLABE_ACME, p2$texto, fixed = TRUE))

  # 3. Two rows; a's date moves past b's -> the order flips
  shared$proveedores_db(catalogo()); session$flushReact()
  dos <- dplyr::bind_rows(fila("a", "ACME SA", "D1", 1000, "2026-11-01"), fila("b", "BETA SA", "D2", 500, "2026-11-05"))
  shared$pagar_hoy_db(dos); session$flushReact()
  invisible(.annotate_ap_cached(dos, "Networks"))
  movida <- dos; movida$FechaVenc[1] <- as.Date("2026-11-20")
  shared$pagar_hoy_db(movida); session$flushReact()
  tabla    <- .annotate_ap_cached(movida, "Networks") |> dplyr::arrange(FechaVenc)
  confirma <- movida |> dplyr::arrange(FechaVenc)
  ok("3: the table's order (annotated) matches the confirm handlers' order (b, a)",
     identical(tabla$id, confirma$id) && identical(tabla$id, c("b", "a")))
  al_reves <- .annotate_ap_cached(movida[2:1, ], "Networks")
  ok("3: the same rows in another order: each keeps its own CLABE, by id",
     identical(al_reves$id, c("b", "a")) && identical(al_reves$clabe_dest, c(CLABE_BETA, CLABE_ACME)) &&
       identical(al_reves$Importe, c(500, 1000)))

  # 4. a taken out and re-staged as an abono of 400 (new id, same Parte/Documento)
  shared$pagar_hoy_db(fila("a", "ACME SA", "D1", 1000, "2026-11-01")); session$flushReact()
  invisible(ppl())
  shared$pagar_hoy_db(fila("c", "ACME SA", "D1", 400, "2026-11-01", tipo = "abono")); session$flushReact()
  an <- .annotate_ap_cached(shared$pagar_hoy_db(), "Networks")
  ok("4: the abono comes back as itself: new id, 400, tipo_item = abono",
     identical(an$id, "c") && identical(an$Importe, 400) && identical(an$tipo_item, "abono"))
  ok("4: the PPL pays 400, not the full 1000", identical(ppl()$filas$importe, 400))

  # 5. A session alias override (⇕) re-annotates; link/unlink still clear
  .session_alias_overrides(list("Networks|ACME SA" = paste0("ACME2|", CLABE_ACME2))); session$flushReact()
  ok("5: a session override points the PPL at the other account at once",
     identical(ppl()$filas$clabe_dest, CLABE_ACME2))
})

src <- readLines(Sys.getenv("PH_MODULE", "R/pagar_hoy_module.R"), warn = FALSE)
ok("5: link, unlink and cycle-alias still clear the cache (3 rm(list = ls(.annot_cache)))",
   sum(grepl("rm(list = ls(.annot_cache), envir = .annot_cache)", src, fixed = TRUE)) == 3L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
