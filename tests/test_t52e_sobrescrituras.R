# =============================================================================
# tests/test_t52e_sobrescrituras.R
# T52e: the ✎ overrides of a SAP invoice (sap_overrides.rds: Parte, Código --
# the CLABE's key --, Factura, Notas, Moneda, Importe) belong to ONE invoice,
# not to its document number.
#
# Before: keyed ledger|Empresa|Moneda|Documento, no posting date. With a
# number SAP reuses in another year, what was edited on the 2025 invoice
# (already paid) painted the 2026 one -- another supplier's Código, so
# another CLABE in the Agenda -- and two open invoices with the same number
# shared their edit. The save also wrote the session's copy (another
# session's edit lost) and a failed save still said "Cambios guardados".
#
# The rule (T52d's, closed by the integrator and the Coordinador):
#   - a dated override reaches its own invoice only (document + posting date);
#   - an undated one (before T52e) reaches its document only while it is ONE
#     invoice, and only when that invoice is not later than the override's
#     day (updated_at in the client's zone, hoy_en(); never as.Date(POSIXct));
#     without updated_at, the one-invoice rule alone.
#
# Run from project root:  Rscript tests/test_t52e_sobrescrituras.R
# =============================================================================
cat("=== test_t52e_sobrescrituras ===\n\n")
source("tests/helpers/s3_memoria.R")          # R/persistence.R over an in-memory S3
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key", "is_erp_sourced"))
    eval(e, globalenv())
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("R/zona.R"); source("R/monedas.R"); source("R/empresas_identidad.R")
source("R/data_pipeline.R"); source("R/policy_engine.R")
cargar <- function(archivo, nombres)
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
cargar("R/ledger_module.R", ".sap_ov_guardar")
s3_memoria_reset <- function() rm(list = ls(.mem, all.names = TRUE), envir = .mem)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q  <- function(expr) suppressMessages(suppressWarnings(expr))
s  <- function(expr) tryCatch(q(expr), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
FC <- "Fecha de contabilización"
VIEJA <- "CLABE-VIEJA-DELL"   # the Código an old override carries: in Pagar hoy it picks the bank account

# AP 1001 of NCS in MXN: the 2025 one (Dell, paid) and the 2026 one (HP).
fila_ap <- function(parte, codigo, saldo, fc, venc)
  tibble::tibble(Documento = "1001", Empresa = "NCS", Parte = parte, `Código de proveedor` = codigo,
                 Moneda = "MXN", `Saldo vencido` = saldo, Importe = saldo, `Fecha de contabilización` = as.Date(fc),
                 `Fecha de vencimiento` = as.Date(venc), check.names = FALSE)
f2025 <- fila_ap("Dell", "P-DELL", 100, "2025-03-01", "2025-04-01")
f2026 <- fila_ap("HP",   "P-HP",   200, "2026-10-01", "2026-11-01")

ov <- function(fecha = NA, at = "2025-03-05 18:00:00", doc = "1001", codigo = VIEJA, parte = "Dell", importe = 999,
               notas = NA_character_, quita_fecha = FALSE) {
  r <- tibble::tibble(id = paste("AP", "NCS", "MXN", doc, sep = "|"), ledger = "AP", Empresa = "NCS", Moneda = "MXN",
                      Documento = doc, Parte_override = parte, Codigo_override = codigo, Factura_override = NA_character_,
                      Notas_override = notas, Moneda_override = NA_character_, Importe_override = importe,
                      updated_by = "a", updated_at = as.POSIXct(at, tz = "UTC"), fecha_factura = as.Date(fecha))
  if (quita_fecha) r$fecha_factura <- NULL   # a row written before T52e has no such column
  r
}
lib <- function(raw, sap_ov) q(build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL, sap_ov = sap_ov))
codigo_de <- function(df, fc) df$Codigo[as.Date(df[[FC]]) == as.Date(fc)]
agenda_de <- function(df) q(agenda_filas_factura(dplyr::mutate(df, ledger = "AP"), "yo"))

# ── e · The reused number after the old one was paid: the CLABE stays behind ──
cat("-- e. reused number, the old CLABE --\n")
d <- lib(f2026, ov(quita_fecha = TRUE))                       # undated, written 2025-03-05; invoice of 2026-10-01
ok("e1: an undated override older than the invoice does not paint it (Código is the invoice's own)",
   identical(d$Codigo, "P-HP") && identical(d$Parte, "HP") && isFALSE(d$has_sap_override))
ok("e2: …nor its amount", identical(abs(d$Importe), 200))
a <- agenda_de(d)
ok("e3: …and the Agenda row carries the invoice's own Código and amount, never the old CLABE",
   nrow(a) == 1L && identical(a$Codigo, "P-HP") && !any(a$Codigo == VIEJA) && identical(as.numeric(a$Importe), 200))
d <- lib(f2026, ov(fecha = "2025-03-01"))                     # dated, of the 2025 invoice
ok("e4: a dated override of the 2025 invoice never reaches the 2026 one",
   identical(d$Codigo, "P-HP") && isFALSE(d$has_sap_override))

# ── Two open invoices with the same number ────────────────────────────────────
cat("-- two open invoices --\n")
d <- lib(bind_rows(f2025, f2026), ov(at = "2026-11-01 18:00:00", quita_fecha = TRUE))
ok("r1: an undated override on a document with two open invoices reaches neither",
   !any(d$Codigo == VIEJA) && !any(d$has_sap_override))
d <- lib(bind_rows(f2025, f2026), ov(fecha = "2026-10-01", codigo = "P-HP-2", parte = "HP SA", at = "2026-11-01 18:00:00"))
ok("r2: a dated override reaches only its own invoice of the two",
   identical(codigo_de(d, "2026-10-01"), "P-HP-2") && identical(codigo_de(d, "2025-03-01"), "P-DELL"))

# ── Nothing of today is lost ──────────────────────────────────────────────────
cat("-- old rows that still reach --\n")
d <- lib(f2026, ov(at = "2026-10-15 18:00:00", codigo = "P-HP-X", parte = "HP X", importe = 250, quita_fecha = TRUE))
ok("v1: an undated override of a one-invoice document, written after it, still paints it (as before)",
   identical(d$Codigo, "P-HP-X") && identical(d$Parte, "HP X") && identical(abs(d$Importe), 250) && isTRUE(d$has_sap_override))
o <- ov(codigo = "P-HP-X", quita_fecha = TRUE); o$updated_at <- as.POSIXct(NA)
ok("v2: without updated_at, only the one-invoice rule (it paints)", identical(lib(f2026, o)$Codigo, "P-HP-X"))
# The day's edge, in the client's zone: 2026-10-01 05:00 UTC is 2026-09-30 in Mexico City.
ok("v3: written 05:00 UTC on the invoice's day (the day before, in Mexico City): it does not paint",
   identical(lib(f2026, ov(at = "2026-10-01 05:00:00", codigo = "P-HP-X", quita_fecha = TRUE))$Codigo, "P-HP"))
ok("v4: written 07:00 UTC that day (the same day in Mexico City): it paints",
   identical(lib(f2026, ov(at = "2026-10-01 07:00:00", codigo = "P-HP-X", quita_fecha = TRUE))$Codigo, "P-HP-X"))

# ── a · identical() to cd2ea9b without reused numbers ─────────────────────────
cat("-- a. identical() to the code before T52e --\n")
viejo <- new.env(parent = globalenv())
fuente <- tempfile(fileext = ".R")
base_commit <- Sys.getenv("T52E_BASE", "cd2ea9b")
writeLines(system2("git", c("show", sprintf("%s:R/data_pipeline.R", base_commit)), stdout = TRUE), fuente, useBytes = TRUE)
q(sys.source(fuente, envir = viejo))
diez <- bind_rows(lapply(1:10, function(k) tibble::tibble(
  Documento = as.character(2000 + k), Empresa = if (k %% 2) "NCS" else "NTS", Parte = paste("P", k),
  `Código de proveedor` = paste0("C", k), Moneda = if (k %% 3) "MXN" else "USD", `Saldo vencido` = 100 * k, Importe = 100 * k,
  `Fecha de contabilización` = as.Date("2026-01-01") + k, `Fecha de vencimiento` = as.Date("2026-02-01") + k,
  check.names = FALSE)))
ovs <- bind_rows(lapply(c(1, 3, 4, 7, 9), function(k) {
  r <- ov(doc = as.character(2000 + k), codigo = paste0("C", k, "-X"), parte = paste("P", k, "X"),
          importe = if (k %% 2) 50 * k else NA_real_, at = "2026-06-01 12:00:00", quita_fecha = TRUE)
  r$Empresa <- if (k %% 2) "NCS" else "NTS"; r$Moneda <- if (k %% 3) "MXN" else "USD"; r
}))
nuevo <- lib(diez, ovs)
antes <- q(viejo$build_ledger_df(diez, "AP", empresa = NULL, moves_df = NULL, sap_ov = ovs))
ok("a1: ten one-invoice documents, five old overrides written after them: identical() to cd2ea9b",
   identical(nuevo, antes))
ok("a2: …and with no overrides at all",
   identical(lib(diez, NULL), q(viejo$build_ledger_df(diez, "AP", empresa = NULL, moves_df = NULL, sap_ov = NULL))))

# ── The Agenda's Referencia (pagar_hoy_module.R) ─────────────────────────────
cat("-- the Agenda's Referencia --\n")
# The function lives inside the module's server: take its definition from the
# parsed file (this branch's or cd2ea9b's) and run it on a stub `shared`. Since
# T52g it is .ph_referencias(emp, filas, overrides), one Referencia per Agenda
# row; cd2ea9b's is .ph_ref_lookup(emp, overrides), a map by Documento.
ref_lookup_de <- function(texto, shared, nombre = ".ph_referencias") {
  busca <- function(x) {
    if (is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(nombre))) return(x[[3]])
    if (is.call(x) || is.expression(x))
      for (k in seq_along(x)) if (is.call(x[[k]])) { r <- busca(x[[k]]); if (!is.null(r)) return(r) }
    NULL
  }
  def <- busca(parse(text = texto, keep.source = FALSE))
  stopifnot(!is.null(def))
  e <- new.env(parent = globalenv()); e$shared <- shared
  eval(def, e)
}
sh_ph <- function(ov_db) list(company_map = function() c(NCS = "NETWORKS CARGO"),
                              sap_data = function() list(AP = data.frame(Empresa = "NCS", Documento = c("1001", "2002"),
                                                                         Factura = c("F-2026", "F-2002"))),
                              sap_ov_db = function() ov_db, manual_inv = function() NULL)
agenda <- tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = c("1001", "2002"),
                         fecha_factura = as.Date(c("2026-10-01", "2026-01-02")))
ov_ph <- q(.normalize(bind_rows(
  ov(doc = "1001", codigo = NA_character_, at = "2025-03-05 18:00:00", quita_fecha = TRUE) |> dplyr::mutate(Factura_override = "F-DELL-2025"),
  ov(doc = "2002", codigo = NA_character_, at = "2026-06-01 18:00:00", quita_fecha = TRUE) |> dplyr::mutate(Factura_override = "F-2002-EDIT")),
  .schema_sap_overrides))
lm_txt <- readLines("R/pagar_hoy_module.R", warn = FALSE, encoding = "UTF-8")
lk_new <- s(stats::setNames(ref_lookup_de(lm_txt, sh_ph(ov_ph))("NCS", agenda, overrides = TRUE), agenda$Documento))
viejo_txt <- system2("git", c("show", sprintf("%s:R/pagar_hoy_module.R", base_commit)), stdout = TRUE)
lk_old <- s(ref_lookup_de(viejo_txt, sh_ph(ov_ph), ".ph_ref_lookup")("NCS", overrides = TRUE))
ok("p1: a reused number (1001, the old edit from before the 2026 invoice) keeps SAP's reference, never the old No. Factura",
   identical(unname(lk_new["1001"]), "F-2026"))
ok("p2: …where the code before T52e showed the old one (the bug)", identical(unname(lk_old["1001"]), "F-DELL-2025"))
ok("p3: a number that does not repeat (2002, edited after its invoice): identical() to cd2ea9b for what the Agenda shows",
   identical(unname(lk_new["2002"]), unname(lk_old["2002"])) && identical(unname(lk_new["2002"]), "F-2002-EDIT"))
ok("p4: without the Agenda rows (overrides = FALSE), the plain SAP map as before",
   identical(s(ref_lookup_de(lm_txt, sh_ph(ov_ph))("NCS", agenda)),
             unname(s(ref_lookup_de(viejo_txt, sh_ph(ov_ph), ".ph_ref_lookup")("NCS"))[agenda$Documento])))
# T52g (integrator, 2026-11-18): the override is applied per row, from the
# row's own origin.
dos_ov <- q(.normalize(bind_rows(
  ov(fecha = "2025-03-01", at = "2025-03-05 18:00:00") |> dplyr::mutate(Factura_override = "F-1001-2025"),
  ov(fecha = "2026-10-01", at = "2026-10-05 18:00:00") |> dplyr::mutate(Factura_override = "F-1001-2026")),
  .schema_sap_overrides))
dos_filas <- tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                            fecha_factura = as.Date(c("2025-03-01", "2026-10-01")))
ok("p5: two Agenda rows with the same number and different dates each take their own invoice's override",
   identical(s(ref_lookup_de(lm_txt, sh_ph(dos_ov))("NCS", dos_filas, overrides = TRUE)),
             c("F-1001-2025", "F-1001-2026")))
sh_man <- sh_ph(ov_ph)
sh_man$manual_inv <- function() tibble::tibble(id = "m-2002", ledger = "AP", Empresa = "NCS", Moneda = "MXN",
                                               Documento = "2002", Factura = "F-MAN-2002")
fila_man <- tibble::tibble(id = "m-2002", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "2002",
                           source = "manual", fecha_factura = as.Date("2026-01-02"))   # p3's date: the override reaches it
ok("p6: a manual row never takes a SAP override with its number (F-2002-EDIT): it keeps its manual's Factura",
   identical(s(ref_lookup_de(lm_txt, sh_man)("NCS", fila_man, overrides = TRUE)), "F-MAN-2002"))
ok("p7: …and with no manual found it stays empty (the caller shows the Documento), never the SAP override",
   identical(s(ref_lookup_de(lm_txt, sh_ph(ov_ph))("NCS", fila_man, overrides = TRUE)), ""))

# ── Writing: upsert_sap_override() per invoice ────────────────────────────────
cat("-- writing: upsert_sap_override() --\n")
pf  <- function(...) tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                                    fecha_factura = as.Date(c(...)))
dos <- pf("2025-03-01", "2026-10-01"); una <- pf("2026-10-01")
db0 <- q(.normalize(bind_rows(ov(quita_fecha = TRUE), ov(doc = "777", quita_fecha = TRUE)), .schema_sap_overrides))
nuevo_ov <- q(.normalize(ov(fecha = "2026-10-01", codigo = "P-HP-2", at = "2026-11-01 18:00:00"), .schema_sap_overrides))
r <- s(upsert_sap_override(db0, nuevo_ov, facturas = dos))
ok("w1: a reused number: editing the 2026 invoice keeps the undated row (it may be the 2025 one's)",
   sum(r$Documento == "1001" & is.na(r$fecha_factura)) == 1L && sum(r$Documento == "1001" & r$fecha_factura %in% as.Date("2026-10-01")) == 1L)
r <- s(upsert_sap_override(db0, nuevo_ov, facturas = una))
ok("w2: one invoice: its undated row is replaced, one row per invoice",
   sum(r$Documento == "1001") == 1L && identical(r$Codigo_override[r$Documento == "1001"], "P-HP-2"))
ok("w3: …and another document (777) is never touched", sum(r$Documento == "777") == 1L)
r2 <- s(upsert_sap_override(r, q(.normalize(ov(fecha = "2026-10-01", codigo = "P-HP-3"), .schema_sap_overrides)), facturas = una))
ok("w4: editing the same invoice again replaces its row", sum(r2$Documento == "1001") == 1L &&
     identical(r2$Codigo_override[r2$Documento == "1001"], "P-HP-3"))
ok("w5: the schema has fecha_factura (Date)", inherits(.schema_sap_overrides()$fecha_factura, "Date"))

# ── b · Two sessions, the same base: neither edit is lost ─────────────────────
cat("-- b. two sessions --\n")
s3_memoria_reset()
CID <- "t52e"
base <- q(.normalize(ov(doc = "777", quita_fecha = TRUE), .schema_sap_overrides))
s(save_sap_overrides(base, client_id = CID))
a_ov <- q(.normalize(ov(doc = "100", fecha = "2026-01-05", codigo = "A"), .schema_sap_overrides))
b_ov <- q(.normalize(ov(doc = "200", fecha = "2026-01-06", codigo = "B"), .schema_sap_overrides))
s(save_sap_overrides(upsert_sap_override(base, a_ov), client_id = CID, base = base))
s(save_sap_overrides(upsert_sap_override(base, b_ov), client_id = CID, base = base))
fin <- s(load_sap_overrides(CID))
ok("b1: session A's edit (100) and session B's (200), from the same base, both stay",
   all(c("100", "200", "777") %in% fin$Documento) && nrow(fin) == 3L)

# ── g · The ✎ save: a failure is neither applied nor announced ────────────────
cat("-- g. the ✎ save --\n")
avisos <- character(); bitacora <- character()
showNotification   <- function(ui, ..., type = "default") { avisos <<- c(avisos, type); invisible() }
log_action         <- function(..., action = "") { bitacora <<- c(bitacora, action); invisible() }
bump_sync_version  <- function(...) invisible()
tr <- function(k, ...) k
sesion <- function(db) { v <- db; list(sap_ov_db = function(x) if (missing(x)) v else v <<- x,
                                       effective_client_id = function() CID, current_user = function() "yo") }
s3_memoria_reset(); s(save_sap_overrides(base, client_id = CID))
sh <- sesion(base)
ok("g1: a good save returns TRUE and the session holds the saved table",
   isTRUE(s(.sap_ov_guardar(sh, a_ov, facturas = NULL))) && "100" %in% sh$sap_ov_db()$Documento)
real_save <- save_sap_overrides
save_sap_overrides <- function(...) stop("S3 no disponible")
sh <- sesion(base); avisos <- character(); bitacora <- character()
r <- s(.sap_ov_guardar(sh, b_ov, facturas = NULL))
save_sap_overrides <- real_save
ok("g2: a failed save returns FALSE", isFALSE(r))
ok("g3: …the session's copy is unchanged (the edit is not shown as if saved)", identical(sh$sap_ov_db(), base))
ok("g4: …the user sees an error", "error" %in% avisos)
ok("g5: …and the failure is logged", any(grepl("fall", bitacora)))

# ── f/g · The code: one reader, the ✎ passes the date and the base ────────────
cat("-- f/g. the code --\n")
archivos <- c(list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE), "app.R")
lineas <- unlist(lapply(archivos, function(f) { l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  paste0(f, ":", seq_along(l), ": ", l) }))
lineas <- lineas[!grepl("^[^:]+:[0-9]+:\\s*#", lineas)]
usa_ov <- grep("(Parte|Codigo|Factura|Notas|Moneda|Importe)_override\\b", lineas, value = TRUE)
fuera <- usa_ov[!grepl("^(R/data_pipeline\\.R|R/persistence\\.R|R/ledger_module\\.R|R/pagar_hoy_module\\.R):", usa_ov)]
ph <- readLines("R/pagar_hoy_module.R", warn = FALSE, encoding = "UTF-8")
ok("f4: the Agenda's Referencia (pagar_hoy_module.R) reads them through sobrescrituras_por_factura(), not by number",
   any(grepl("sobrescrituras_por_factura\\(", ph)) && !any(grepl("pares\\(ov, \"Factura_override\"\\)", ph)))
ok("f1: the override columns are read nowhere else (grep of R/ and app.R)", !length(fuera))
if (length(fuera)) cat(paste0("      ", fuera[1:min(5, length(fuera))]), sep = "\n")
ok("f2: build_ledger_df() reads them through sobrescrituras_por_factura(), not a join",
   any(grepl("^R/data_pipeline\\.R:.*sobrescrituras_por_factura\\(", lineas)) &&
     !any(grepl("^R/data_pipeline\\.R:.*left_join\\(result, ov_filt", lineas)))
lm <- readLines("R/ledger_module.R", warn = FALSE, encoding = "UTF-8")
ok("f3: the ✎'s prefill reads them through sobrescrituras_por_factura() too",
   any(grepl("existing_ov.*sobrescrituras_por_factura\\(|sobrescrituras_por_factura\\(.*existing", lm)) ||
     length(grep("sobrescrituras_por_factura\\(", lm)) >= 1L && !any(grepl("ov_db\\$Documento == row", lm)))
ok("g6: the ✎ saves through .sap_ov_guardar() (with the base), never save_sap_overrides() directly",
   { lmc <- lm[!grepl("^\\s*#", lm)]; todo <- paste(lmc, collapse = "\n")
     any(grepl("isTRUE\\(\\.sap_ov_guardar\\(", lmc)) && sum(grepl("save_sap_overrides\\(", lmc)) == 1L &&
       grepl("save_sap_overrides\\([^\n]*\n[^\n]*base *=", todo) })
ok("g7: the override row carries the invoice's posting date (fecha_factura)",
   any(grepl("fecha_factura *= *as.Date\\(row\\[\\[\"Fecha de contabilizaci", lm)) &&
     length(grep("fecha_factura *= *as.Date\\(row\\[\\[\"Fecha de contabilizaci", lm)) >= 2L)
ok("g8: 'Cambios guardados' only after a successful override save",
   { i <- grep("if \\(!isTRUE\\(\\.sap_ov_guardar\\(", lm); j <- grep("cambios_guardados", lm)
     length(i) == 1L && length(j) >= 1L && any(j > i) })

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
