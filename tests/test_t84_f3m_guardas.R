# =============================================================================
# tests/test_t84_f3m_guardas.R
# T84 F3m: the importer's auto-conciliation (auto_match_agenda) stops making
# wrong links. Prod (networks, 2026-11-27) had 21 in the wrong direction, 68
# confirmations linked to 168 movements, 17 across currencies and 89 by the
# approximate level (73 with a different amount).
#   A  the guards: direction, currency (fail-closed), one use; the
#      approximate level no longer conciliates; the exact one as before.
#   B  the real bancosServer (testServer, in-memory S3): do_import passes the
#      accounts and the confirmations already linked.
# Run from the repo root:  Rscript tests/test_t84_f3m_guardas.R
# =============================================================================
cat("=== test_t84_f3m_guardas ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, det = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (r) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L; if (!is.null(det)) cat("      ", det, "\n") }
}

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced"))
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos"))
cargar("R/ui_components.R", c("hd_numero_revisar"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/bancos_parser.R", "R/empresas_identidad.R", "R/bancos_detect.R", "R/bancos_importadores.R", "R/bancos_persistence.R", "R/bancos_formatos.R",
            "R/bancos_module.R"))
  suppressMessages(source(f))

D <- as.Date("2026-11-20")
CTAS <- data.frame(cuenta_id = c("cM", "cU", "cP"), moneda = c("MXN", "USD", "MXP"))
mv <- function(id, cargo = 0, abono = 0, cuenta = "cM", fecha = D)
  tibble::tibble(id = id, cuenta_id = cuenta, fecha = fecha, cargo = cargo, abono = abono)
cf <- function(id, importe, tipo = "pago", moneda = "MXN", fecha = D, eliminado = FALSE)
  .normalize(tibble::tibble(confirmacion_id = paste0("c-", id), agenda_item_id = paste0("ag-", id), documento = paste0("F-", id),
                            importe = importe, moneda = moneda, fecha = fecha, tipo = tipo, eliminado = eliminado),
             .schema_bancos_confirmados)
am <- function(m, c, cuentas = CTAS, ligadas = character()) auto_match_agenda(m, c, cuentas = cuentas, ligadas = ligadas)

cat("== A. auto_match_agenda ==\n")
r <- am(mv("m1", cargo = 100), cf("1", 100))
ok("A1 exacto como antes: un cargo y un pago del mismo importe y fecha -> conciliado, documento y agenda_id",
   isTRUE(r$conciliado) && identical(r$doc_vinculado, "F-1") && identical(r$agenda_id, "ag-1"))
ok("A2 las columnas de salida son las de siempre (sin columnas nuevas)",
   identical(setdiff(names(r), names(mv("m1"))), c("conciliado", "doc_vinculado", "agenda_id")))
ok("A3 H1: un abono no casa con un pago", !am(mv("m1", abono = 100), cf("1", 100))$conciliado)
ok("A4 H1: un cargo no casa con un cobro", !am(mv("m1", cargo = 100), cf("1", 100, tipo = "cobro"))$conciliado)
ok("A5 un abono sí casa con un cobro", isTRUE(am(mv("m1", abono = 100), cf("1", 100, tipo = "cobro"))$conciliado))
ok("A6 una confirmación sin tipo no casa", !am(mv("m1", cargo = 100), cf("1", 100, tipo = NA_character_))$conciliado)
ok("A7 H3: cuenta en USD contra un pago en MXN no casa", !am(mv("m1", cargo = 100, cuenta = "cU"), cf("1", 100))$conciliado)
ok("A8 moneda normalizada: una cuenta en MXP casa con un pago en MXN", isTRUE(am(mv("m1", cargo = 100, cuenta = "cP"), cf("1", 100))$conciliado))
ok("A9 fail-closed: sin cuentas, con la cuenta que no está o sin moneda en la confirmación, nada casa",
   !am(mv("m1", cargo = 100), cf("1", 100), cuentas = NULL)$conciliado &&
     !auto_match_agenda(mv("m1", cargo = 100), cf("1", 100))$conciliado &&
     !am(mv("m1", cargo = 100, cuenta = "otra"), cf("1", 100))$conciliado &&
     !am(mv("m1", cargo = 100), cf("1", 100, moneda = NA_character_))$conciliado &&
     # both sides without currency: NA must not "equal" NA
     !am(mv("m1", cargo = 100), cf("1", 100, moneda = NA_character_), cuentas = NULL)$conciliado &&
     !am(mv("m1", cargo = 100, cuenta = "otra"), cf("1", 100, moneda = NA_character_))$conciliado)
r <- am(bind_rows(mv("m1", cargo = 50), mv("m2", cargo = 50)), cf("1", 50))
ok("A10 H2: dos cargos iguales el mismo día y una sola confirmación: casa solo uno", sum(r$conciliado) == 1L)
ok("A11 H2: una confirmación ya ligada a un movimiento guardado no vuelve a casar",
   !am(mv("m1", cargo = 50), cf("1", 50), ligadas = "ag-1")$conciliado)
ok("A12 el aproximado (±1 %, ±2 días) ya NO concilia: ni importe distinto ni fecha distinta",
   !am(mv("m1", cargo = 1000), cf("1", 1005))$conciliado && !am(mv("m1", cargo = 1000), cf("1", 1000, fecha = D + 1))$conciliado)
ok("A13 dos candidatas exactas: no se adivina", !am(mv("m1", cargo = 100), bind_rows(cf("1", 100), cf("2", 100)))$conciliado)
ok("A14 confirmaciones eliminadas no casan (CM10/C6)", !am(mv("m1", cargo = 100), cf("1", 100, eliminado = TRUE))$conciliado)
ok("A15 sin movimientos o sin confirmaciones: columnas presentes, nada conciliado",
   nrow(am(mv("m1", cargo = 1)[0, ], cf("1", 1))) == 0L && !am(mv("m1", cargo = 100), NULL)$conciliado)
mg <- tibble::tibble(conciliado = c(TRUE, TRUE, FALSE), eliminado = c(FALSE, TRUE, FALSE), agenda_id = c("ag-1", "ag-9", "ag-3"))
ok("A16 conciliacion_ligadas: los agenda_id de movimientos vigentes y conciliados", identical(conciliacion_ligadas(mg), "ag-1"))

cat("== B. bancosServer real ==\n")
log_action <- function(...) invisible(NULL)
avisos_actividad <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
removeModal <- function(...) invisible(NULL)
showModal <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE
puede_o_avisa <- function(...) TRUE
CID <- "acme"; MK <- paste0(CID, "/", S3_KEYS$bancos_movimientos)
ctas <- tibble::tibble(id = c("c1", "cu"), Empresa = "NTS", banco_id = "bb", Moneda = c("MXN", "USD"), alias = c("A1", "U1"),
                       cuenta = c("01", "02"), clabe_interbancaria = c("0301", "0302"), saldo_inicial = 0, activa = TRUE)
mov <- function(id, cargo, cuenta = "c1", extra = list())
  .normalize(do.call(tibble::tibble, utils::modifyList(list(id = id, cuenta_id = cuenta, fecha = D, hora = "10:00:00", recibo = id,
                 descripcion_raw = paste("Mov", id), tipo = "spei_out", parte = "PROV", concepto = "c", cargo = cargo, abono = 0,
                 saldo_banco = 1000, conciliado = FALSE, importado_at = as.POSIXct("2026-11-20 10:00:00"), fuente = "txt",
                 eliminado = FALSE), extra)), .schema_bancos_movimientos)
movs0 <- mov("m0", 100, extra = list(conciliado = TRUE, agenda_id = "ag-1"))
MOVS <- reactiveVal(movs0)
shared <- list(
  effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
  usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
  visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS,
  bancos_confirmados = reactiveVal(bind_rows(cf("1", 100), cf("2", 250), cf("3", 1000), cf("4", 70))),
  conciliacion_rv = reactiveVal(tibble::tibble()), ctas_cuentas = reactiveVal(ctas), papelera_rv = reactiveVal(NULL),
  abonos_db = reactiveVal(tibble::tibble()), manual_inv = reactiveVal(NULL), pasivos_provisions_db = reactiveVal(NULL),
  sap_data = reactiveVal(list()), empresas_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
testServer(bancosServer, args = list(shared = shared), {
  session$flushReact()
  s3_memoria_reset(); assign(MK, movs0, envir = .mem)
  pv <- bind_rows(mov("T1", 100), mov("T2", 250), mov("T3", 1004), mov("T4", 70, cuenta = "cu"))
  pv$id <- NULL; pv$recibo <- c("901", "902", "903", "904"); pv$conciliado <- NULL
  parsed_preview(pv)
  session$setInputs(import_cuenta = "", do_import = runif(1))
  m <- get(MK, envir = .mem); g <- function(r) m[m$recibo %in% r, ]
  ok("B1 importar: el 250 casa exacto con la 2 (do_import pasa las cuentas)",
     isTRUE(g("902")$conciliado) && identical(g("902")$agenda_id, "ag-2"))
  ok("B2 importar: el 100 NO casa con la 1, ya ligada a m0 (do_import pasa las ligadas)", !g("901")$conciliado)
  ok("B3 importar: el 1004 (aproximado a la 3) ya no se concilia", !g("903")$conciliado)
  ok("B4 importar: el 70 de la cuenta en USD no casa con el pago en MXN", !g("904")$conciliado)
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
quit(status = if (.fail > 0L) 1L else 0L)
