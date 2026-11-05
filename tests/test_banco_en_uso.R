# =============================================================================
# tests/test_banco_en_uso.R
# CM17 (Luis, 2026-11-05): a bank that some account uses is never deleted.
# The server refuses, writes nothing, and shows a copyable warning with every
# account that uses it (company, currency, last 4 digits or alias, active).
# Without can_manage_bancos it is refused before anything is read or shown.
#
# Runs the REAL settings_cuentas_observer in shiny::testServer over the
# in-memory S3 (the harness of test_ctas_lectura.R).
#
# Run from project root:  Rscript tests/test_banco_en_uso.R
# =============================================================================
cat("=== test_banco_en_uso ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "modificar_ctas_bancos", "modificar_ctas_cuentas",
                                          ".BANCOS_MX_CHOICES", "borrar_banco_si_libre",
                                          ".banco_en_uso_filas", ".banco_en_uso_texto"))
cargar("R/adjuntos_s3.R", c(".NUMERO_PURO", ".CELDA_PELIGRO", "neutralizar_celdas", "tabla_a_texto_copiable"))
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/settings/settings_cuentas.R"))
  suppressMessages(source(f))

AVISOS <- character(); MODALES <- list()
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible("id") }
showModal <- function(ui, ...) { MODALES[[length(MODALES) + 1L]] <<- ui; invisible(NULL) }
removeModal <- function(...) invisible(NULL)

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")
nivel <- function(t) list(user = t, tier = t, permisos = permisos_resueltos(t))
CON <- nivel("admin"); SIN <- nivel("analysis")
stopifnot(puede(CON, "can_manage_bancos"), !puede(SIN, "can_manage_bancos"))

k_b <- paste0(CID, "/", S3_KEYS$ctas_bancos)
k_c <- paste0(CID, "/", S3_KEYS$ctas_cuentas)
BANCOS0 <- tibble::tibble(id = c("b1", "b2"), nombre = c("BanBajío", "Santander"), clave = c("030", "014"))
cta <- function(id, banco, empresa = "NTS", moneda = "MXN", cuenta = "", clabe = "", alias = "", activa = TRUE)
  tibble::tibble(id = id, banco_id = banco, Empresa = empresa, razon_social = "X SA", rfc = "XXX010101AA1",
                 Moneda = moneda, alias = alias, cuenta = cuenta, clabe_interbancaria = clabe,
                 is_ppl_default = FALSE, saldo_inicial = 0, activa = activa)
# Three on b1 (one inactive, one with only CLABE, one with only an alias that
# would run as a formula in Excel) and one on b2.
CTAS0 <- rbind(cta("c1", "b1", cuenta = "0123456789"),
               cta("c2", "b1", empresa = "NCS", moneda = "USD", clabe = "030000000000004321", activa = FALSE),
               cta("c3", "b1", alias = "=HYPERLINK(1)"),
               cta("c4", "b2", cuenta = "5555"))

# Deletes the bank at row `fila` as user `u`, over S3 seeded by `sembrar`.
borrar <- function(u, fila, sembrar = function() { assign(k_b, BANCOS0, envir = .mem); assign(k_c, CTAS0, envir = .mem) }) {
  s3_memoria_reset(); AVISOS <<- character(); MODALES <<- list()
  sembrar()
  antes <- .mem_escrituras
  shared <- list(usuario = reactiveVal(u), current_user = function() u$user,
                 effective_client_id = function() CID, home_client_id = function() CID,
                 company_map = reactive(CMAP), ctas_cuentas = reactiveVal(CTAS0))
  testServer(function(input, output, session) settings_cuentas_observer(input, output, session, shared), {
    session$flushReact()
    session$setInputs(`bancos_cat_t-accion` = list(accion = "editar", ids = BANCOS0$id[fila], nonce = 1))
    session$setInputs(banco_delete = 1)
  })
  .mem_escrituras - antes
}
texto_modal <- function() paste(as.character(MODALES[[1]]), collapse = "\n")

# 1. A bank in use: refused, nothing written, the whole list shows
n <- borrar(CON, 1)
ok("in use: nothing written", n == 0L)
ok("in use: both banks still on S3", identical(get(k_b, envir = .mem)$id, c("b1", "b2")))
ok("in use: one warning modal, no 'deleted' notice", length(MODALES) == 1L && !"Banco eliminado." %in% AVISOS)
m <- texto_modal()
ok("in use: names the bank", grepl("BanBajío", m, fixed = TRUE))
ok("in use: says three accounts", grepl(tr_n("comun.banco_en_uso.explica", 3), m, fixed = TRUE))
ok("in use: last 4 of the account", grepl("…6789", m, fixed = TRUE))
ok("in use: last 4 of the CLABE when there is no account", grepl("…4321", m, fixed = TRUE))
ok("in use: the company's name, not its initials", grepl("Networks Crossdocking", m, fixed = TRUE))
ok("in use: the inactive account is listed too",
   grepl(tr("comun.banco_en_uso.linea", empresa = "Networks Crossdocking", moneda = "USD",
            cuenta = "…4321", activa = tr("comun.banco_en_uso.no")), m, fixed = TRUE))
ok("in use: the account of the other bank is not listed", !grepl("5555", m, fixed = TRUE))
ok("in use: has the Copiar button", grepl(tr("comun.copiar"), m, fixed = TRUE))

# 2. The copied text: header + one row per account, tab-separated, and a cell
#    that would run as a formula gets its apostrophe
filas <- .banco_en_uso_filas(CTAS0[CTAS0$banco_id == "b1", ], CMAP)
txt <- .banco_en_uso_texto(filas)
lineas <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
ok("copy text: header plus three rows", length(lineas) == 4L)
ok("copy text: four tab-separated columns per row", all(lengths(strsplit(lineas, "\t", fixed = TRUE)) == 4L))
ok("copy text: header from the dictionary", startsWith(lineas[1], tr("comun.banco_en_uso.col_empresa")))
ok("copy text: the formula alias is neutralized", grepl("\t'=HYPERLINK(1)\t", txt, fixed = TRUE))
ok("copy text: the modal carries that same text", grepl("&#39;=HYPERLINK(1)", m, fixed = TRUE) || grepl("'=HYPERLINK(1)", m, fixed = TRUE))

# 3. A bank no account uses is deleted, only it
n <- borrar(CON, 2, function() { assign(k_b, BANCOS0, envir = .mem); assign(k_c, CTAS0[CTAS0$banco_id == "b1", ], envir = .mem) })
ok("free bank: deleted, the other stays", identical(get(k_b, envir = .mem)$id, "b1"))
ok("free bank: no warning modal", length(MODALES) == 0L && "Banco eliminado." %in% AVISOS)

# 4. Without can_manage_bancos: refused, nothing written, no list shown
n <- borrar(SIN, 1)
ok("no permission: nothing written", n == 0L && identical(get(k_b, envir = .mem)$id, c("b1", "b2")))
ok("no permission: no list of accounts shown", length(MODALES) == 0L)

# 5. The accounts can't be read: nothing deleted, and said so
n <- borrar(CON, 2, function() {
  assign(k_b, BANCOS0, envir = .mem); assign(k_c, CTAS0, envir = .mem); .mem_falla <<- k_c })
.mem_falla <- character()
ok("accounts unreadable: nothing written", n == 0L && identical(get(k_b, envir = .mem)$id, c("b1", "b2")))
ok("accounts unreadable: the user is told", tr("comun.error_guardar") %in% AVISOS)

# 6. Every visible text of the modal is in both dictionaries
for (idioma in c("es", "en")) {
  d <- i18n_dict(idioma)
  ok(sprintf("dictionary %s has every comun.banco_en_uso key", idioma),
     all(c("comun.banco_en_uso.titulo", "comun.banco_en_uso.explica.uno", "comun.banco_en_uso.explica.otros",
           "comun.banco_en_uso.linea", "comun.banco_en_uso.col_empresa", "comun.banco_en_uso.col_moneda",
           "comun.banco_en_uso.col_cuenta", "comun.banco_en_uso.col_activa",
           "comun.banco_en_uso.si", "comun.banco_en_uso.no") %in% names(d)))
}

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
