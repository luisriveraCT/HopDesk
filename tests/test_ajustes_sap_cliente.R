# =============================================================================
# tests/test_ajustes_sap_cliente.R
# T23 · 1: Ajustes reads the SAP snapshots (IC panel lookup, the scanner,
# Business Partners on open, "Actualizar" and after "Aplicar como IC") only
# with the session's own client. Its 10 reads used
# client_id = tryCatch(shared$effective_client_id(), error = NULL): with no
# client they read with NULL, which falls to the deployment's default key --
# another client's (or the shared) snapshots.
#   1. the client fails, is NULL or empty: no snapshot is read, and the user
#      is told -- for each of the five places;
#   2. with a client, every read carries it;
#   3. static: every load_sap_snapshot() in settings_hub.R goes through the
#      one reader, with its client.
#
# Run from project root:  Rscript tests/test_ajustes_sap_cliente.R
# =============================================================================
cat("=== test_ajustes_sap_cliente ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

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
cargar("R/data_pipeline.R", "scan_ic_candidates")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_cuentas.R", "R/settings/settings_bp.R", "R/settings/settings_hub.R"))
  suppressMessages(source(f))

AVISOS <- character(); LECTURAS <- list()
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
insertUI <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
proveedoresServer <- function(...) invisible(NULL)
proveedoresUI <- function(id) div()

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking")
USR  <- list(user = "admin", tier = "admin", permisos = permisos_resueltos("admin"))
REG0 <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
             companies = list(NTS = list(ar = "1027", ap = character())))
SNAP <- data.frame(Empresa = "Networks Trucking", `Código de cliente` = c("C1027", "C2001"),
                   Parte = c("NETWORKS CROSSDOCKING", "NETWORKS REALTORS"), RFC = c("NCS010101AA1", "NRS010101AA1"),
                   check.names = FALSE, stringsAsFactors = FALSE)
load_sap_snapshot <- function(ledger, client_id = NULL) {
  LECTURAS[[length(LECTURAS) + 1L]] <<- list(ledger = ledger, client_id = client_id)
  list(data = if (ledger == "AR") SNAP else NULL)
}

correr <- function(cliente, clics) {
  s3_memoria_reset(); AVISOS <<- character(); LECTURAS <<- list()
  assign(paste0(CID, "/", S3_KEYS$interco_v2), REG0, envir = .mem)
  shared <- list(usuario = reactiveVal(USR), current_user = function() "admin",
                 effective_client_id = cliente, home_client_id = function() CID,
                 company_map = reactive(CMAP), ctas_cuentas = reactiveVal(NULL),
                 interco_v2 = reactiveVal(REG0), proveedores_db = reactiveVal(NULL), sap_data = function() list())
  error <- tryCatch({
    testServer(function(input, output, session) settings_observers(input, output, session, shared),
               { session$flushReact(); clics(session) }); NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(error)) message("  ", error)
  list(error = error, lecturas = LECTURAS, avisos = AVISOS)
}
LUGARES <- list(
  "panel Intercompany (etiquetas de códigos)" = function(s) s$setInputs(stg_btn_interco = 1),
  "escáner"                                  = function(s) s$setInputs(scan_ic_btn = 1),
  "Business Partners al abrir"               = function(s) s$setInputs(stg_btn_catalogo = 1),
  "Business Partners · Actualizar"           = function(s) s$setInputs(bp_refresh_btn = 1))
aviso <- tr("tablas.ajustes.sap.sin_cliente")

# ── 1. No client: nothing read, the user is told ─────────────────────────────
SIN <- list("el cliente truena" = function() stop("sin sesión"), "cliente NULL" = function() NULL,
            "cliente vacío" = function() "")
for (caso in names(SIN)) for (lugar in names(LUGARES)) {
  r <- correr(SIN[[caso]], LUGARES[[lugar]])
  ok(sprintf("%s · %s: no lee ningún snapshot y avisa (%d lecturas)", caso, lugar, length(r$lecturas)),
     is.null(r$error) && !length(r$lecturas) && aviso %in% r$avisos)
}

# ── 2. With a client, every read carries it ──────────────────────────────────
for (lugar in names(LUGARES)) {
  r <- correr(function() CID, LUGARES[[lugar]])
  ok(sprintf("con cliente · %s: lee AR y AP con client_id = %s, sin avisar", lugar, CID),
     is.null(r$error) && length(r$lecturas) >= 2L &&
       all(vapply(r$lecturas, function(l) identical(l$client_id, CID), logical(1))) && !aviso %in% r$avisos)
}
# After "Aplicar como IC" the table re-reads the snapshots: also with the client.
r <- correr(function() CID, function(s) {
  s$setInputs(bp_refresh_btn = 1); n0 <<- length(LECTURAS)
  s$setInputs(`bp_sap_t-sel` = "NTS|AR|C2001"); s$setInputs(bp_apply_ic_btn = 1)
})
ok("después de “Aplicar como IC”, la relectura también lleva el cliente",
   is.null(r$error) && length(r$lecturas) > n0 &&
     all(vapply(r$lecturas, function(l) identical(l$client_id, CID), logical(1))))

# ── 2b. T23 · 2: Business Partners and the scanner read the same ────────────
# No snapshot on S3, but the session has its SAP data: both stand on it (only
# the scanner did; Business Partners said "no hay snapshots").
snap_real <- load_sap_snapshot
load_sap_snapshot <- function(ledger, client_id = NULL) {
  LECTURAS[[length(LECTURAS) + 1L]] <<- list(ledger = ledger, client_id = client_id); NULL
}
# What each place hands scan_ic_candidates() (the codes it will show).
scan_real <- scan_ic_candidates
VISTOS <- NULL
scan_ic_candidates <- function(sap_ar, sap_ap, ...) {
  r <- scan_real(sap_ar, sap_ap, ...); VISTOS <<- sort(r$code); r
}
codigos <- function(boton) {
  VISTOS <<- NULL
  s3_memoria_reset(); AVISOS <<- character(); LECTURAS <<- list()
  assign(paste0(CID, "/", S3_KEYS$interco_v2), REG0, envir = .mem)
  shared <- list(usuario = reactiveVal(USR), current_user = function() "admin",
                 effective_client_id = function() CID, home_client_id = function() CID,
                 company_map = reactive(CMAP), ctas_cuentas = reactiveVal(NULL),
                 interco_v2 = reactiveVal(REG0), proveedores_db = reactiveVal(NULL),
                 sap_data = function() list(AR = SNAP))
  tryCatch(testServer(function(input, output, session) settings_observers(input, output, session, shared), {
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(1), boton))
  }), error = function(e) message("  ERROR: ", conditionMessage(e)))
  VISTOS
}
bp <- codigos("bp_refresh_btn"); esc <- codigos("scan_ic_btn")
ok("sin snapshot, con datos de la sesión: Business Partners y el escáner ven los mismos códigos (C1027, C2001)",
   identical(bp, c("C1027", "C2001")) && identical(esc, bp))
load_sap_snapshot <- snap_real; scan_ic_candidates <- scan_real

# ── 3. Static ────────────────────────────────────────────────────────────────
src <- paste(readLines("R/settings/settings_hub.R", encoding = "UTF-8"), collapse = "\n")
llamadas <- regmatches(src, gregexpr("load_sap_snapshot\\([^)]*\\)", src))[[1]]
ok(sprintf("settings_hub.R: las %d llamadas a load_sap_snapshot() van con client_id = cid (del lector único)", length(llamadas)),
   length(llamadas) == 2L && all(grepl("client_id = cid", llamadas, fixed = TRUE)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
