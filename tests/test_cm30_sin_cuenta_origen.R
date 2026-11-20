# =============================================================================
# tests/test_cm30_sin_cuenta_origen.R
# CM30. With no ORIGIN account for a company and currency, get_cuenta_ppl()
# is "" and generate_ppl() zero-padded it: a PPL file debiting from
# "000000000000", logged as generated, with no word to the user. (The old
# bancos.rds fallback behind it is gone: it existed in no client.) Now:
#   1  one company (dl_ppl_<emp>): the modal says where to set the account up
#      and has no download button; content() writes only "# <notice>" and
#      logs nothing;
#   2  "Descargar todos": that company and currency is skipped before
#      anything is logged, the others still go in, the preview and a notice
#      say which; its rows cannot be included in a _002 either;
#   3  every pair without an account: no button, and no empty ZIP;
#   4  generate_ppl() stops on a blank or all-zeros origin (last net); the
#      destination CLABE in zeros of the _002 is untouched and its bytes are
#      the same as before CM30;
#   5  .get_cuenta_origen_legacy() reads only the session's bancos_cuentas.
# The real pagarHoyServer (testServer), the harness of test_t52g_ref_factura.
#
# Run from project root:  Rscript tests/test_cm30_sin_cuenta_origen.R
# =============================================================================
cat("=== test_cm30_sin_cuenta_origen ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
fmt_money <- function(x) paste0("$ ", formatC(x, format = "f", digits = 2, big.mark = ","))

for (e in parse("R/erp_connector_registry.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "is_erp_sourced"))
    eval(e, globalenv())
for (e in parse("R/data_pipeline.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".politica_doc_unica"))
    eval(e, globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/zona.R", "R/ppl_generator.R"))
  source(f)
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
source("R/agenda_filas.R")   # staged() reads agenda_pendientes() (T60)
source("R/pagar_hoy_module.R")
# .get_cuenta_origen_legacy() alone (section 5), from the file that has it.
for (e in parse("R/settings/settings_helpers.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".get_cuenta_origen_legacy"))
    eval(e, globalenv())

# Stubs (after the module). Every logged payment file is recorded.
PPL <- list(); ultimo_modal <- NULL; AVISOS <- character()
.log_pago_generado <- function(shared, emp, moneda, cuenta_origen, ppl_rows, ..., source_type = NA_character_) {
  PPL[[length(PPL) + 1L]] <<- list(tipo = source_type, moneda = moneda, origen = cuenta_origen)
  invisible(NULL)
}
LOGS <- list()
log_action <- function(user, module, action, ...) { LOGS[[length(LOGS) + 1L]] <<- list(action = action, args = list(...)); invisible(NULL) }
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(ui, ...) ultimo_modal <<- as.character(ui)
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
Sys.time <- function() as.POSIXct("2026-11-18 12:00:00", tz = "America/Mexico_City")

# The origin account per currency; "" = none set up for it.
ORIGEN <- c(MXN = "073543010201", USD = "013386156002")
get_cuenta_ppl <- function(emp, cur, shared) unname(ORIGEN[toupper(cur)]) %||% ""
get_cuentas_ppl_data <- function(emp, cur, shared) {
  c0 <- get_cuenta_ppl(emp, cur, shared)
  if (!nzchar(c0)) return(data.frame(cuenta = character(), banco = character(), alias = character(),
                                     is_ppl_default = logical(), stringsAsFactors = FALSE))
  data.frame(cuenta = c0, banco = "012", alias = "Operativa", is_ppl_default = TRUE, stringsAsFactors = FALSE)
}

EMP <- "Networks"
catalogo <- tibble::tibble(nombre = c("ACME SA", "SIN CLABE SA"), alias = c("ACME", "SINCLABE"),
                           clabe = c("012345678901234567", NA), medio_pago = "SPEI", banco = c("012", NA),
                           Empresa = NA_character_)
fila <- function(id, doc, moneda = "MXN", parte = "ACME SA", imp = 100)
  tibble::tibble(id = id, ledger = "AP", Empresa = EMP, Moneda = moneda, Documento = doc, Parte = parte,
                 Codigo = "", Importe = imp, FechaVenc = as.Date("2026-11-20"), status = "pending",
                 tipo_item = NA_character_, source = SAP_B1_ERP_TYPE, provision_id = NA_character_,
                 fecha_factura = as.Date("2026-11-01"))
AGENDA <- dplyr::bind_rows(fila("A1", "D1"), fila("A2", "D2", moneda = "USD", imp = 50),
                           fila("A3", "D3", parte = "SIN CLABE SA", imp = 30))   # A3: MXN, no CLABE
SAP <- data.frame(Empresa = "NTS", Documento = c("D1", "D2", "D3"), Factura = c("F-1", "F-2", "F-3"),
                  Parte = c("ACME SA", "ACME SA", "SIN CLABE SA"), Moneda = c("MXN", "USD", "MXN"),
                  `Fecha de contabilización` = as.Date("2026-11-01"), check.names = FALSE)

# Runs the one-company flow (MXN) or the ZIP; `incluir`: ids for the _002.
correr <- function(zip = TRUE, incluir = character()) {
  PPL <<- list(); LOGS <<- list(); AVISOS <<- character(); ultimo_modal <<- NULL
  res <- list()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(AGENDA),
    sap_data = reactiveVal(list(AP = SAP)), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))),
    home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    if (!zip) {
      session$setInputs(dl_preview_Networks = 1)
      res$modal <<- ultimo_modal %||% ""
      res$archivo <<- tryCatch(readLines(output$dl_ppl_Networks, warn = FALSE), error = function(e) paste("ERROR", conditionMessage(e)))
    } else {
      session$setInputs(preview_all = 1)
      res$vista <<- .descargar_vista()
      if (length(incluir)) {
        ids <- paste0(incluir, "#1")
        session$setInputs(`descargar_t-opciones` = list(ids = ids, valores = rep("incluir", length(ids))))
      }
      res$incluidos <<- .descargar_incluidos()
      res$boton <<- as.character(output$descargar_boton$html)
      dir <- tempfile(); dir.create(dir)
      res$zip <<- tryCatch({ utils::unzip(output$download_all, exdir = dir); sort(list.files(dir)) },
                           error = function(e) paste("ERROR", conditionMessage(e)))
    }
  })))
  res
}
aviso_ind <- function(cur) tr("tablas.pagos.sin_cuenta_origen", empresa = EMP, moneda = cur)
omitidas  <- function(lista) tr("tablas.pagos.omitidas_sin_origen", lista = lista)
f001 <- function(cur) sprintf("pagos_%s_%s_18112026_001.txt", EMP, cur)
f002 <- function(cur) sprintf("pagos_%s_%s_18112026_002.txt", EMP, cur)

# ── Control: with both accounts everything is as before ─────────────────────
ORIGEN <- c(MXN = "073543010201", USD = "013386156002")
r <- correr(zip = FALSE)
ok("control: one company with an account: its button is there, the file has the payment, it is logged",
   grepl("dl_ppl_Networks", r$modal, fixed = TRUE) && any(startsWith(r$archivo, "02")) &&
     length(Filter(function(p) identical(p$tipo, "individual"), PPL)) == 1L)
r <- correr(zip = TRUE, incluir = "A3")
ok("control: the ZIP has MXN and USD _001 and the MXN _002, no notice",
   identical(r$zip, sort(c(f001("MXN"), f001("USD"), f002("MXN")))) && !any(grepl(omitidas(""), AVISOS, fixed = TRUE)))

# ── 1. One company, no MXN origin account ───────────────────────────────────
ORIGEN <- c(MXN = "", USD = "013386156002")
r <- correr(zip = FALSE)
ok("1 the modal says where to set the origin account up, and has no download button",
   grepl(aviso_ind("MXN"), r$modal, fixed = TRUE) && !grepl("dl_ppl_Networks", r$modal, fixed = TRUE))
ok("1 content() writes only the notice line, no payment, nothing logged",
   identical(r$archivo, paste("#", aviso_ind("MXN"))) && !length(PPL))

# ── 2. ZIP: MXN has no origin account, USD has ──────────────────────────────
r <- correr(zip = TRUE, incluir = "A3")
ok("2 the preview marks the MXN rows 'sin origen', locked out", {
   v <- r$vista; m <- v[sub("#.*$", "", v$id) %in% c("A1", "A3"), ]
   all(m$motivo == "sin_origen") && all(m$zip == "omitir") && all(m$.bloqueada) })
ok("2 …and A3 (no CLABE, MXN) cannot be included in a _002", !"A3" %in% r$incluidos)
ok("2 the preview says which company and currency is skipped", grepl(omitidas(paste0(EMP, " (MXN)")), r$boton, fixed = TRUE))
ok("2 the ZIP has only the USD file", identical(r$zip, f001("USD")))
ok("2 only USD is logged (MXN skipped before .log_pago_generado), and no 'incluir_sin_cuenta'",
   identical(vapply(PPL, `[[`, "", "moneda"), "USD") &&
     !any(vapply(LOGS, function(l) identical(l$action, "incluir_sin_cuenta"), logical(1))))
ok("2 the user is told which was skipped", any(grepl(omitidas(paste0(EMP, " (MXN)")), AVISOS, fixed = TRUE)))

# ── 3. No origin account anywhere ───────────────────────────────────────────
ORIGEN <- c(MXN = "", USD = "")
r <- correr(zip = TRUE)
ok("3 no download button, and the preview says there is nothing to generate",
   !grepl("download_all", r$boton, fixed = TRUE) && grepl(tr("tablas.pagos.nada_que_descargar"), r$boton, fixed = TRUE))
ok("3 the ZIP itself is never an empty one: it stops with the notice, nothing logged",
   startsWith(r$zip[1], "ERROR") && grepl(tr("tablas.pagos.nada_que_descargar"), r$zip[1], fixed = TRUE) && !length(PPL))

# ── 4. generate_ppl()'s net ─────────────────────────────────────────────────
p <- data.frame(alias = "ACME", clabe_dest = "012345678901234567", banco_dest = "012", medio_pago = "SPI",
                importe = 100.5, concepto = "F-3", stringsAsFactors = FALSE)
para <- function(o) tryCatch({ generate_ppl(p, o, as.Date("2026-11-18"), 1L); "generado" }, error = function(e) "detenido")
ok("4 generate_ppl() stops on a blank, NA, NULL or all-zeros origin",
   identical(vapply(list("", NA, NULL, "000000000000", "  "), para, ""), rep("detenido", 5)))
md5 <- function(pp, c) { t <- tempfile(); write_ppl(generate_ppl(pp, "073543010201", as.Date("2026-11-18"), c), t); unname(tools::md5sum(t)) }
p002 <- data.frame(alias = "", clabe_dest = "000000000000000000", banco_dest = "", medio_pago = "SPI",
                   importe = c(8200, 3100), concepto = c("F-1", "F-2"), stringsAsFactors = FALSE)
# Reference MD5s from ppl_generator.R before CM30 (38b3425), same inputs.
ok("4 a _002 (destination CLABE in zeros) and a _001 are byte-identical to before CM30",
   identical(md5(p002, 2L), "abf214cace720f9eb09cfac0d46c041b") && identical(md5(p, 1L), "908be3af4d96a800ce70f7cf41219d5b"))

# ── 5. The origin account fallback reads only bancos_cuentas ────────────────
bc <- function(df) list(bancos_cuentas = function() df)
ok("5 .get_cuenta_origen_legacy(): the active bancos_cuentas account of the company",
   identical(.get_cuenta_origen_legacy("NTS", "MXN", bc(tibble::tibble(Empresa = c("NTS", "NTS"), activa = c(FALSE, TRUE),
                                                                     cuenta = c("111", " 222 ")))), "222"))
ok("5 …and \"\" without one; it never calls load_bancos() (which no longer exists)",
   identical(.get_cuenta_origen_legacy("NTS", "MXN", bc(tibble::tibble(Empresa = character(), activa = logical(),
                                                                     cuenta = character()))), "") &&
     identical(.get_cuenta_origen_legacy("NTS", "MXN", list()), "") &&
     !any(grepl("load_bancos", deparse(.get_cuenta_origen_legacy), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
