# =============================================================================
# tests/test_ajustes_candidatos_hd.R
# Ajustes › Business Partners (bp_sap) and the IC scanner (escaner_ic) on the
# table engine (T16c, class D), one column definition for both:
#   1. the rows: id = initials|ledger|code, only the session's companies,
#      sorted by company, CxC before CxP, most invoices first; "IC ✓" on the
#      codes already in the registry (no pre-selection); En catálogo by RFC or
#      code; empty or NULL sources keep the columns;
#   2. one definition: the scanner's columns are Business Partners' without
#      En catálogo;
#   3. by id: a new scan puts another code ahead between marking and
#      "Aplicar", and the marked code is the one saved -- by position it saved
#      the code that took the row; a marked code the new scan no longer has is
#      dropped, and with nothing left nothing is written;
#   4. nothing marked: nothing written, the user is told (the pre-selected
#      codes used to be sent along).
#
# Run from project root:  Rscript tests/test_ajustes_candidatos_hd.R
# =============================================================================
cat("=== test_ajustes_candidatos_hd ===\n\n")
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

AVISOS <- character()
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
CMAP <- c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")
USR  <- list(user = "admin", tier = "admin", permisos = permisos_resueltos("admin"))
stopifnot(puede(USR, "can_manage_interco"))
REG0 <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
             companies = list(NTS = list(ar = "1027", ap = character())))

cand <- function(ini, ledger, code, n, is_ic = FALSE, rfc = NA_character_, nombre = paste("BP", code))
  tibble::tibble(initials = ini, ledger = ledger, code = code, nombre = nombre, rfc = rfc,
                 n_facturas = n, is_ic = is_ic)

# ── 1. The rows ──────────────────────────────────────────────────────────────
CANDS <- dplyr::bind_rows(
  cand("NTS", "AP", "P3003", 9),
  cand("NTS", "AR", "C2001", 2, rfc = "NRS010101AA1"),
  cand("NTS", "AR", "C1027", 5, is_ic = TRUE),
  cand("NCS", "AR", "C4004", 1, rfc = ""),
  cand("HDX", "AR", "C9999", 50))                         # not one of the session's companies
PROV <- data.frame(rfc = c("NRS010101AA1", ""), codigo = c("", "P3003"), stringsAsFactors = FALSE)
COLS <- c("id", "is_ic", "Empresa", "ledger", "codigo", "nombre", "rfc", "facturas", "en_catalogo")

f <- tryCatch(.ic_candidatos_filas(CANDS, CMAP, PROV), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
ok("id = iniciales|libro|código; solo las empresas de la sesión (HDX fuera)",
   identical(f$id, c("NCS|AR|C4004", "NTS|AR|C1027", "NTS|AR|C2001", "NTS|AP|P3003")))
ok("orden: empresa, CxC antes que CxP, más facturas primero", identical(f$facturas, c(1L, 5L, 2L, 9L)))
ok("“IC ✓” solo en el código que ya está en el registro (sin preselección)",
   identical(f$is_ic, c(NA, "ic", NA, NA)))
ok("En catálogo: por RFC y por código; RFC vacío queda sin RFC",
   identical(f$en_catalogo, c(NA, NA, "rfc", "codigo")) && is.na(f$rfc[1]))
ok("sin catálogo de proveedores: En catálogo vacío", all(is.na(.ic_candidatos_filas(CANDS, CMAP)$en_catalogo)))
for (caso in list(list("candidatos NULL", NULL), list("candidatos vacíos", CANDS[0, ]),
                  list("solo de otra empresa", CANDS[5, ])))
  ok(sprintf("%s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .ic_candidatos_filas(caso[[2]], CMAP); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 2. One column definition ─────────────────────────────────────────────────
campos <- function(cols) vapply(cols, function(c) c$campo, "")
ok("el escáner tiene las columnas de Business Partners sin En catálogo",
   identical(campos(.ic_candidatos_columnas()), setdiff(campos(.ic_candidatos_columnas(catalogo = TRUE)), "en_catalogo")) &&
     "en_catalogo" %in% campos(.ic_candidatos_columnas(catalogo = TRUE)))

# ── 3-4. On the real observers ───────────────────────────────────────────────
SNAP1 <- data.frame(Empresa = "Networks Trucking", `Código de cliente` = c("C1027", "C2001"),
                    Parte = c("NETWORKS CROSSDOCKING", "NETWORKS REALTORS"), RFC = c("NCS010101AA1", "NRS010101AA1"),
                    check.names = FALSE, stringsAsFactors = FALSE)
# The new scan: C0500 with more invoices goes ahead of C2001 (row 2 by position).
SNAP2 <- rbind(SNAP1, data.frame(Empresa = "Networks Trucking", `Código de cliente` = c("C0500", "C0500", "C0500"),
                                 Parte = "OTRA", RFC = "OTR010101AA1", check.names = FALSE, stringsAsFactors = FALSE))
SNAP <- SNAP1
load_sap_snapshot <- function(ledger, client_id = NULL) list(data = if (ledger == "AR") SNAP else NULL)

correr <- function(clics) {
  s3_memoria_reset(); AVISOS <<- character(); SNAP <<- SNAP1
  assign(paste0(CID, "/", S3_KEYS$interco_v2), REG0, envir = .mem)
  shared <- list(usuario = reactiveVal(USR), current_user = function() "admin",
                 effective_client_id = function() CID, home_client_id = function() CID,
                 company_map = reactive(CMAP), ctas_cuentas = reactiveVal(NULL),
                 interco_v2 = reactiveVal(REG0), proveedores_db = reactiveVal(NULL), sap_data = function() list())
  antes <- .mem_escrituras
  error <- tryCatch({
    testServer(function(input, output, session) settings_observers(input, output, session, shared),
               { session$flushReact(); clics(session) }); NULL
  }, error = function(e) conditionMessage(e))
  list(error = error, escrituras = .mem_escrituras - antes,
       ar = get0(paste0(CID, "/", S3_KEYS$interco_v2), envir = .mem)$companies$NTS$ar)
}

# Business Partners: marked C2001, then "Actualizar" brings C0500 ahead.
r <- correr(function(session) {
  session$setInputs(bp_refresh_btn = 1)
  session$setInputs(`bp_sap_t-sel` = "NTS|AR|C2001")
  SNAP <<- SNAP2; session$setInputs(bp_refresh_btn = 2)
  session$setInputs(bp_apply_ic_btn = 1)
})
if (!is.null(r$error)) message("  ", r$error)
ok("Business Partners: “Aplicar como IC” guarda el código marcado (2001), aunque otro escaneo puso C0500 antes",
   is.null(r$error) && r$escrituras >= 1L && "2001" %in% r$ar && !"0500" %in% r$ar)
ok("Business Partners: une con el registro (1027 sigue)", "1027" %in% r$ar)

# Scanner: the same, with "Escáner SAP" run again between marking and applying.
r <- correr(function(session) {
  session$setInputs(scan_ic_btn = 1)
  session$setInputs(`escaner_ic_t-sel` = "NTS|AR|C2001")
  SNAP <<- SNAP2; session$setInputs(scan_ic_btn = 2)
  session$setInputs(ic_scan_apply = 1)
})
if (!is.null(r$error)) message("  ", r$error)
ok("escáner: “Aplicar selección” guarda el código marcado (2001), no el que quedó en su fila (0500)",
   is.null(r$error) && r$escrituras >= 1L && "2001" %in% r$ar && !"0500" %in% r$ar)

# A marked code the new scan no longer has: dropped; nothing left, nothing written.
r <- correr(function(session) {
  session$setInputs(scan_ic_btn = 1)
  session$setInputs(`escaner_ic_t-sel` = "NTS|AR|C7777")
  session$setInputs(ic_scan_apply = 1)
})
ok("escáner: un código marcado que ya no está se descarta; sin nada, no escribe y avisa",
   is.null(r$error) && r$escrituras == 0L && any(grepl("No se seleccionaron", AVISOS)))

# Nothing marked: nothing written (the pre-selected codes used to go along).
r <- correr(function(session) {
  session$setInputs(bp_refresh_btn = 1)
  session$setInputs(bp_apply_ic_btn = 1)
})
ok("Business Partners sin nada marcado: no escribe y avisa",
   is.null(r$error) && r$escrituras == 0L && any(grepl("Selecciona al menos", AVISOS)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
