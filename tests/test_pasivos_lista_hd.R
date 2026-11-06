# =============================================================================
# tests/test_pasivos_lista_hd.R
# T18: the Lista de pasivos on hd_table (lista_pasivos, class D). Its rows,
# their buttons by estado and permission, ✎ handing the wizard the id, and
# every action acting on the row's id (never its position). Real module under
# testServer, in-memory S3. The saves themselves: test_pasivos_papelera.R.
# Run from project root:  Rscript tests/test_pasivos_lista_hd.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_list_module.R"))
  suppressMessages(source(f))
PERMITIDO <- TRUE
has_capability    <- function(user, cap) PERMITIDO
permiso_nombre    <- function(p) p
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
pasivos_log_audit <- function(...) invisible(NULL)
EDITADOS <- character(0)
pasivos_pedir_editar <- function(lid) EDITADOS <<- c(EDITADOS, lid)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
pasivo <- function(id, nombre, empresa, estado = "active", categoria = "regular", moneda = "MXN",
                   cotizado = "", flavor = NULL, monto = 100) {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = categoria, flavor = flavor, nombre = nombre, empresa = empresa, parte = paste("Parte", id),
    moneda_pago = moneda, cotizado_en = cotizado, recurrence_type = "monthly_day",
    recurrence_params = list(day = 15L), fecha_inicio = Sys.Date() - 40, amount_default = monto),
    mode = "create", user = "ana")
  l$id <- id; l$estado <- estado; l
}
liabs0 <- dplyr::bind_rows(
  pasivo("L1", "Renta bodega",  "NTS"),
  pasivo("L2", "Licencias",     "NCS", cotizado = "USD", monto = 250),
  pasivo("L3", "Mantenimiento", "NTS", estado = "paused"),
  pasivo("L4", "Seguro viejo",  "NCS", estado = "deleted")
)
provs0 <- dplyr::bind_rows(lapply(seq_len(nrow(liabs0)), function(i) {
  l <- liabs0[i, ]; l$estado <- "active"; pasivos_generate_provisions(l, window_start = Sys.Date() - 40)
}))
provs0 <- provs0[!(provs0$liability_id == "L3" & provs0$fecha_efectiva >= Sys.Date()), ]

nuevo_shared <- function(liabs = liabs0, provs = provs0, visibles = NULL) {
  L <- reactiveVal(liabs); P <- reactiveVal(provs); V <- reactiveVal(visibles)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P, visible_initials = V,
       effective_client_id = function() CID, current_user = function() "luis",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function(liabs = liabs0, provs = provs0) {
  s3_memoria_reset()
  save_pasivos_liabilities(liabs, client_id = CID)
  .s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
}
estado_s3 <- function(id) {
  l <- get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem); l$estado[l$id == id]
}

# ── L1: rows ─────────────────────────────────────────────────────────────────
sembrar(); sh <- nuevo_shared()
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()
  d <- lista_datos()
  ok(identical(sort(d$id), c("L1", "L2", "L3")), "L1: the Lista shows active and paused, not the deleted one")
  ok(identical(d$empresa, sort(d$empresa)), "L1: sorted by empresa (then categoria, nombre)")
  ok(identical(d$moneda[d$id == "L2"], "MXN (cotizado USD)"), "L1: Moneda says what it is quoted in")
  prox <- provs0[provs0$liability_id == "L1" & provs0$fecha_efectiva >= Sys.Date(), ]
  ok(identical(d$proxima_fecha[d$id == "L1"], min(prox$fecha_efectiva)) &&
       isTRUE(d$proxima_monto[d$id == "L1"] == 100), "L1: next provision = L1's earliest one from today on, 100")
  ok(is.na(d$proxima_fecha[d$id == "L3"]), "L1: the paused one has no next provision")
  ok(identical(d$.acciones[d$id == "L1"], "editar,inactivar,eliminar"), "L1: an active row: ✎, pause, ✕")
  ok(identical(d$.acciones[d$id == "L3"], "editar,reactivar,eliminar"), "L1: a paused row: ✎, resume, ✕")
})
sh <- nuevo_shared(visibles = "NCS")
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()
  ok(identical(lista_datos()$id, "L2"), "L1: only the companies the user may see")
})
PERMITIDO <- FALSE
testServer(pasivos_list_module_server, args = list(shared = nuevo_shared()), {
  session$flushReact()
  ok(all(lista_datos()$.acciones == ""), "L1: without can_manage_pasivos, no row has a button")
})
PERMITIDO <- TRUE
testServer(pasivos_list_module_server, args = list(shared = nuevo_shared(NULL, NULL)), {
  session$flushReact()
  ok(is.data.frame(lista_datos()) && !nrow(lista_datos()), "L1: NULL sources (startup): an empty Lista, no error")
})

# ── L2: ✎ hands the wizard the row's id ──────────────────────────────────────
testServer(pasivos_list_module_server, args = list(shared = nuevo_shared()), {
  session$flushReact()
  session$setInputs(`lista-accion` = list(accion = "editar", ids = "L2", nonce = runif(1)))
})
ok(identical(EDITADOS, "L2"), "L2: ✎ on L2 sends pasivos_edit_liability = L2")

# ── L3: by id, whatever moved: another session adds a liability that sorts
# first while this one has the Lista open; ✕ on L2 deletes L2 ──────────────
sembrar(); sh <- nuevo_shared()
testServer(pasivos_list_module_server, args = list(shared = sh), {
  session$flushReact()
  antes <- lista_datos()$id
  # The other session saved it (on S3) and the sync bus brought it here.
  # Since T22 the save works on S3's table, so L0 must be there as it would.
  con_l0 <- dplyr::bind_rows(pasivo("L0", "Agua", "AAA"), isolate(sh$pasivos_liabilities_db()))
  save_pasivos_liabilities(con_l0, client_id = CID)
  sh$pasivos_liabilities_db(con_l0)
  session$flushReact()
  ok(!identical(lista_datos()$id, antes) && identical(lista_datos()$id[1], "L0"), "L3: the rows moved (L0 sorts first)")
  session$setInputs(`lista-accion` = list(accion = "eliminar", ids = "L2", nonce = runif(1)))
  session$setInputs(plist_confirm_archive = 1)
})
ok(identical(estado_s3("L2"), "deleted") && identical(estado_s3("L1"), "active") && identical(estado_s3("L0"), "active"),
   "L3: ✕ on L2 deleted L2, and nothing else")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
