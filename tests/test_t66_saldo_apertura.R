# =============================================================================
# tests/test_t66_saldo_apertura.R
# T66: the Agenda's "Saldo apertura" (R/pagar_hoy_module.R, bal_<emp>) is a
# hd_numero_input monto, set with hd_numero_update. On the real
# pagarHoyServer (testServer) and the real persistence over the in-memory S3
# (the harness of test_cm33_agenda_base.R):
#   1. the Observer 1 -> 2 lock: switching the currency loads the stored
#      value; its echo (the field sending the same number back) writes
#      nothing and logs nothing;
#   2. an edit saves and logs; a negative is accepted (an overdraft);
#   3. empty or a text (NA, which used to become 0 and be saved): one aviso,
#      nothing saved, nothing logged;
# Run from project root:  Rscript tests/test_t66_saldo_apertura.R
# Contraprueba: on 5e102e3 case 3 fails (NA saved as 0).
# =============================================================================
cat("=== test_t66_saldo_apertura ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
source("R/bancos_persistence.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", "is_erp_sourced")
cargar("R/data_pipeline.R", ".politica_doc_unica")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/zona.R", "R/ppl_generator.R"))
  source(f)
cargar("R/search_module.R", c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
cargar("R/ui_components.R", c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                              "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))
source("R/agenda_filas.R")
source("R/pagar_hoy_module.R")

LOGS <- character(); AVISOS <- character()
log_action <- function(user, module, action, ...) { LOGS <<- c(LOGS, action); invisible(NULL) }
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }

CID <- "dev"; EMP <- "Networks"
KSA <- paste0(CID, "/", s3_key_saldos_apertura_user("luis"))
SA0 <- list(Networks = list(MXN = 1000, USD = 50))
sa_s3 <- function() get(KSA, envir = .mem)

# Seeds S3, runs `clics` in a fresh session; gives the session's saldos at the end.
correr <- function(clics, falla = FALSE) {
  s3_memoria_reset(); assign(KSA, SA0, envir = .mem)
  LOGS <<- character(); AVISOS <<- character()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(NULL),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(.schema_pagar_hoy()),
    sap_data = reactiveVal(list(AP = NULL, AR = NULL)), sap_ov_db = reactiveVal(NULL),
    manual_inv = reactiveVal(NULL), bancos_confirmados = reactiveVal(tibble::tibble()),
    abonos_db = reactiveVal(NULL),
    effective_client_id = function() CID, current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list())),
    home_client_id = function() CID)
  fin <- NULL
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    do.call(session$setInputs, setNames(list("MXN"), paste0("bal_cur_", EMP)))
    session$flushReact()
    escrituras0 <- .mem_escrituras
    if (falla) .mem_falla_escritura <<- KSA
    clics(function(v) do.call(session$setInputs, setNames(list(v), paste0("bal_", EMP))),
          function(m) do.call(session$setInputs, setNames(list(m), paste0("bal_cur_", EMP))))
    .mem_falla_escritura <<- character(0)
    fin <<- list(sa = saldos_apertura(), escrituras = .mem_escrituras - escrituras0)
  })))
  fin
}

# 1. The lock: currency switch + the field's echo of the stored number
r <- correr(function(bal, moneda) { moneda("USD"); bal(50); moneda("MXN"); bal(1000) })
ok("1 lock: switching currency and its echo write nothing", r$escrituras == 0L && identical(sa_s3(), SA0))
ok("1 lock: …and log nothing, no aviso", !length(LOGS) && !length(AVISOS))

# 2. An edit saves; a negative too
r <- correr(function(bal, moneda) bal(2500.5))
ok("2 edit 2,500.50: saved and logged", identical(sa_s3()$Networks$MXN, 2500.5) && "editar_saldo_apertura" %in% LOGS)
r <- correr(function(bal, moneda) bal(-3000))
ok("2 a negative (-3,000, an overdraft): saved as is", identical(sa_s3()$Networks$MXN, -3000) &&
     identical(r$sa$Networks$MXN, -3000))

# 3. Empty or a text (NA)
r <- correr(function(bal, moneda) bal(NA_real_))
ok("3 NA: one aviso 'Saldo apertura: escribe un número.'", identical(AVISOS, "Saldo apertura: escribe un número."))
ok("3 NA: nothing saved, nothing logged, the session keeps 1,000",
   r$escrituras == 0L && identical(sa_s3(), SA0) && !length(LOGS) && identical(r$sa$Networks$MXN, 1000))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
