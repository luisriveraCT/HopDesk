# =============================================================================
# tests/test_bancos_excel_banbajio.R
# "Exportar (formato BanBajío)" writes the same file it wrote before T12a:
# the Libro's own Excel, used to reconcile and for accounting (Luis,
# 2026-11-03). The same movements go through bancosServer as it was at
# 3cecac2 (production before T12a, the module's own downloadHandler) and
# through today's (the format's entry in R/bancos_formatos.R), in
# testServer; the two files must match in columns, order, values, types,
# cell styles, column widths, row heights and the frozen header. When they
# don't, the test says what differs. Two cases: every account (Empresa and
# Moneda columns) and one account.
#
# Run from project root:  Rscript tests/test_bancos_excel_banbajio.R
# (needs git: the old module is read with `git show 3cecac2:...`).
# =============================================================================
cat("=== test_bancos_excel_banbajio ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
ANTES <- "3cecac2"

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced"))
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_persistence.R"))
  suppressMessages(source(f))
log_action <- function(...) invisible(NULL)
log_archivo_generado <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE
puede_o_avisa <- function(...) TRUE

# Each version of the module in its own environment.
modulo <- function(lineas, extra = character(0)) {
  env <- new.env(parent = globalenv())
  for (f in extra) sys.source(f, envir = env)
  tmp <- tempfile(fileext = ".R"); writeLines(lineas, tmp, useBytes = TRUE)
  sys.source(tmp, envir = env)
  env
}
viejo <- system2("git", c("show", paste0(ANTES, ":R/bancos_module.R")), stdout = TRUE)
ok(sprintf("the module as it was at %s is readable from git", ANTES), length(viejo) > 1000L)
env_antes <- modulo(enc2utf8(viejo))
env_hoy   <- modulo(readLines("R/bancos_module.R", warn = FALSE, encoding = "UTF-8"), "R/bancos_formatos.R")

# ── The same data for both ───────────────────────────────────────────────────
CID <- "acme"
ctas <- tibble::tibble(id = c("c1", "c2"), Empresa = c("NTS", "NCS"), banco_id = "bb", Moneda = c("MXN", "USD"),
                       alias = c("NTSBB", "NCSUSD"), cuenta = c("01", "02"), clabe_interbancaria = c("0301", "0302"),
                       saldo_inicial = 0, activa = TRUE)
mov <- function(i, cuenta, cargo = 0, abono = 0, tipo = "spei_out", saldo = 1e6 - i * 10)
  tibble::tibble(id = sprintf("M%03d", i), cuenta_id = cuenta, fecha = as.Date("2026-10-01") + i %/% 3,
                 hora = sprintf("%02d:%02d:00", 8 + i %% 10, i %% 60), recibo = as.character(500000 + i),
                 descripcion_raw = sprintf("SPEI | PROVEEDOR %02d SA | Recibo # %d", i, i), tipo = tipo,
                 parte = sprintf("PROVEEDOR %02d SA", i), rfc = NA_character_, referencia = NA_character_,
                 clave_rastreo = NA_character_, concepto = "c", cargo = cargo, abono = abono, saldo_banco = saldo,
                 conciliado = FALSE, doc_vinculado = NA_character_, agenda_id = NA_character_,
                 importado_at = as.POSIXct("2026-10-01 10:00:00"), fuente = "txt", eliminado = FALSE,
                 eliminado_por = NA_character_, eliminado_at = as.POSIXct(NA), notas = NA_character_)
movs0 <- .normalize(dplyr::bind_rows(
  lapply(1:30, function(i) mov(i, c("c1", "c2")[1 + i %% 2], cargo = if (i %% 3) 100 * i + 0.5 else 0,
                               abono = if (i %% 3) 0 else 250 * i)),
  mov(31, "c1", cargo = 12.5, tipo = "comision"),                 # hidden: commissions off
  mov(32, "c1", abono = 999, saldo = NA_real_)), .schema_bancos_movimientos)

exportar <- function(env, cuenta) {
  shared <- list(
    effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
    usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")),
    visible_initials = reactiveVal(NULL), bancos_movimientos = reactiveVal(movs0),
    bancos_confirmados = reactiveVal(.schema_bancos_confirmados()), conciliacion_rv = reactiveVal(tibble::tibble()),
    ctas_cuentas = reactiveVal(ctas), papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(tibble::tibble()),
    manual_inv = reactiveVal(NULL), pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()),
    empresas_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
  archivo <- NULL
  testServer(env$bancosServer, args = list(shared = shared), {
    session$setInputs(libro_cuenta = cuenta, libro_periodo = "todo", mostrar_comisiones = FALSE, flujo_filter = "todos")
    # testServer removes the download's own folder when it ends: keep a copy.
    archivo <<- tempfile(fileext = ".xlsx")
    file.copy(output$download_libro, archivo)
  })
  archivo
}

# ── What a file holds, as comparable pieces ──────────────────────────────────
partes <- function(archivo) {
  wb  <- openxlsx::loadWorkbook(archivo)
  est <- lapply(wb$styleObjects, function(so) {
    st <- so$style
    list(sheet = so$sheet, rows = sort(so$rows), cols = sort(so$cols),
         font = c(st$fontName, st$fontSize, st$fontColour, st$fontDecoration),
         fill = st$fill, border = c(st$borderTop, st$borderBottom, st$borderLeft, st$borderRight,
                                    st$borderTopColour, st$borderBottomColour),
         halign = st$halign, valign = st$valign, numFmt = st$numFmt)
  })
  list(hojas   = names(wb),
       datos   = openxlsx::read.xlsx(archivo, sheet = 1, check.names = FALSE),
       estilos = est[order(vapply(est, function(e) paste(e$rows[1], e$cols[1], e$numFmt$numFmt %||% "", e$font[3] %||% ""), ""))],
       anchos  = wb$colWidths, altos = wb$rowHeights, congelado = wb$worksheets[[1]]$sheetViews)
}
diferencias <- function(a, b) {
  d <- character(0)
  for (k in names(a)) if (!identical(a[[k]], b[[k]])) {
    d <- c(d, k)
    if (k == "datos") {
      if (!identical(names(a$datos), names(b$datos)))
        d <- c(d, sprintf("  columnas: %s  vs  %s", paste(names(a$datos), collapse = ","), paste(names(b$datos), collapse = ",")))
      else if (nrow(a$datos) != nrow(b$datos)) d <- c(d, sprintf("  filas: %d vs %d", nrow(a$datos), nrow(b$datos)))
      else for (cl in names(a$datos)) if (!identical(a$datos[[cl]], b$datos[[cl]]))
        d <- c(d, sprintf("  columna %s (tipo %s vs %s)", cl, class(a$datos[[cl]])[1], class(b$datos[[cl]])[1]))
    }
  }
  d
}

for (caso in list(c("__todas__", "every account"), c("c1", "one account (NTS MXN)"))) {
  fa <- exportar(env_antes, caso[1]); fh <- exportar(env_hoy, caso[1])
  ok(sprintf("%s: both versions write a file", caso[2]), !is.null(fa) && file.exists(fa) && !is.null(fh) && file.exists(fh))
  pa <- partes(fa); ph <- partes(fh)
  d <- diferencias(pa, ph)
  if (length(d)) cat("   differs in:\n", paste0("    ", d, "\n"), sep = "")
  ok(sprintf("%s: the same file as %s -- sheet, columns and order, values and types, cell styles, widths, heights, frozen header (%d rows)",
             caso[2], ANTES, nrow(ph$datos)), !length(d))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
