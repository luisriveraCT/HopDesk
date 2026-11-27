# =============================================================================
# tests/test_t99_pasivo_id_na.R
# T99. A liability whose id is NA is nobody's liability, and no read or save
# may let it take another liability's place. Before T99 `liabs$id == x`
# gave NA at that row, and `[` put an all-NA row there (a "ghost"):
#   A  the wizard's save (R/pasivos_wizard_module.R, `fresco$id != fila$id`)
#      turned a row with an NA id into a ghost on every edit of ANY other
#      liability: its data lost, its NA id kept;
#   B  the pencil (the wizard's edit observer) opened the ghost instead of the
#      liability when an NA row came first;
#   C  the Pasivos table (R/pasivos_table_pivot.R, provisions and converted
#      provisions' manual items) took the ghost as the row's liability: no
#      name, no categoria.
# (The Calendario's glue is in tests/test_t88_provisiones_vector.R.)
# On the in-memory S3 of tests/helpers/s3_memoria.R (never a real bucket).
# Run from project root:  Rscript --vanilla tests/test_t99_pasivo_id_na.R
# =============================================================================
cat("=== test_t99_pasivo_id_na ===\n\n")
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("R/monedas.R"); source("R/zona.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R",
            "R/pasivos_wizard_module.R", "R/adjuntos_s3.R", "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R", "R/adjuntos_cfdi.R", "R/adjuntos_pasivo_ui.R", "R/i18n.R", "R/pasivos_table_pivot.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/ui_components.R"))
  suppressMessages(source(f))
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name("strip_accents"))) eval(e)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(expr)))

CID <- "acme"
pasivo <- function(id, nombre, estado = "active") {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = nombre, empresa = "NG", parte = paste("Parte", nombre),
    moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
    fecha_inicio = as.Date("2026-08-01"), amount_default = 100), mode = "create", user = "ana")
  l$id <- id; l$estado <- estado; l
}
# The NA-id row FIRST, with its data: what a lost id would leave.
LIABS <- dplyr::bind_rows(pasivo(NA_character_, "Sin id"), pasivo("L1", "Renta"), pasivo("L2", "Licencias"))
en_s3 <- function() get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)
fantasmas <- function(l) sum(is.na(l$nombre) & is.na(l$empresa) & is.na(l$estado))

bumps <- character(); auditorias <- list()
bump_sync_version <- function(name, client_id = NULL) bumps <<- c(bumps, name)
avisos_actividad <- function(...) invisible(TRUE)
pasivos_log_audit <- function(...) { auditorias[[length(auditorias) + 1L]] <<- list(...); invisible(NULL) }
pasivos_edit_confirm_open <- function(...) invisible(NULL)
has_capability <- function(...) TRUE

sesion <- function(liabs = LIABS) {
  L <- reactiveVal(liabs); P <- reactiveVal(.schema_pasivos_provision())
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       effective_client_id = function() CID,
       suppress_ledger_prov_refresh = function(...) invisible(NULL),
       company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
       current_user = function() "beto")
}

# ── A: the wizard's save keeps the NA-id row as it is ─────────────────────────
cat("== A: the wizard's save ==\n")
s3_memoria_reset(); callado(save_pasivos_liabilities(LIABS, client_id = CID))
antes <- en_s3()
ok(sum(is.na(antes$id)) == 1L && identical(antes$nombre[is.na(antes$id)], "Sin id"),
   "A0: the seed is on S3 with its NA-id row (the case exists to be tested)")
sh <- sesion(antes)
draft <- pasivos_liability_to_draft(antes[!is.na(antes$id) & antes$id == "L1", ]); draft$amount_default <- 120
wiz <- list(mode = "edit", edit_target_id = "L1", draft = draft)
res <- NULL
callado(testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, sh, session, "beto"), {}))
desp <- en_s3()
ok(isTRUE(res$ok) && isTRUE(desp$amount_default[!is.na(desp$id) & desp$id == "L1"] == 120),
   "A1: editing L1 saves L1 with 120")
ok(fantasmas(desp) == 0L, "A2: ...and leaves no all-NA (ghost) row on S3")
sin_id <- desp[is.na(desp$id), , drop = FALSE]
ok(nrow(sin_id) == 1L && identical(sin_id$nombre, "Sin id") && identical(sin_id$empresa, "NG") &&
     identical(sin_id$estado, "active"),
   "A3: ...the NA-id row is still there with its data (nombre, empresa, estado)")
ok(nrow(desp) == nrow(antes) && identical(sort(desp$id[!is.na(desp$id)]), c("L1", "L2")),
   "A4: ...no row added or lost: L1, L2 and the NA-id row")

# ── B: the pencil opens the liability, never the ghost ────────────────────────
cat("\n== B: the pencil (edit observer) ==\n")
abierto <- NULL
.a_borrador <- pasivos_liability_to_draft
pasivos_liability_to_draft <- function(row) { abierto <<- row; .a_borrador(row) }
shB <- c(sesion(LIABS), list(moneda_base_cliente = function() list(moneda = "MXN"),
                             empresas_db = function() character(), moneda_catalogo = function() NULL))
errB <- tryCatch({
  callado(testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, shB), {
    # The observer has ignoreInit = TRUE: the first flush is its init run.
    session$setInputs(pasivos_edit_liability = "")
    session$setInputs(pasivos_edit_liability = "L2")
  })); NULL
}, error = function(e) conditionMessage(e))
pasivos_liability_to_draft <- .a_borrador
if (!is.null(errB)) cat("  (testServer: ", errB, ")\n")
ok(!is.null(abierto) && nrow(abierto) == 1L && identical(abierto$id, "L2") && identical(abierto$nombre, "Licencias"),
   "B1: with an NA-id row first, the pencil on L2 opens L2 (Licencias), not a ghost")

# ── C: the Pasivos table takes the liability, never the ghost ─────────────────
cat("\n== C: the Pasivos table ==\n")
FILTROS <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"),
                subcategorias = character(0), currency = "All", search = "", granularity = "day")
BANCOS <- data.frame(provision_id = character(), confirmacion_id = character(), eliminado = logical(),
                     stringsAsFactors = FALSE)
prov <- function(id, lid, estado = "provisional", fecha = Sys.Date() + 5) {
  p <- .schema_pasivos_provision()[NA_integer_, ][1, ]
  p$id <- id; p$liability_id <- lid; p$origin <- "rule"; p$occurrence_index <- 1L; p$estado <- estado
  p$fecha_calculada <- fecha; p$fecha_efectiva <- fecha; p$empresa <- "NG"; p$parte <- "Parte"
  p$moneda_pago <- "MXN"; p$amount_pago <- 100; p
}
P <- dplyr::bind_rows(prov("p1", "L1"), prov("p2", "L2", "converted", Sys.Date() + 7))
MI <- tibble::tibble(id = "mi2", provision_id = "p2", ledger = "AP", Importe = 100, FechaVenc = Sys.Date() + 7)
largo <- tryCatch(suppressMessages(pasivos_table_build_long(P, MI, BANCOS, LIABS, FILTROS, fx = NULL)),
                  error = function(e) { cat("  (build_long: ", conditionMessage(e), ")\n"); NULL })
fila_de <- function(kind) if (is.null(largo)) NULL else largo[largo$cell_kind == kind, , drop = FALSE]
pv <- fila_de("provision"); mi <- fila_de("manual_item")
ok(!is.null(pv) && nrow(pv) == 1L && identical(pv$row_id, "L1") && grepl("^Renta", pv$row_label) &&
     identical(pv$row_categoria, "regular"),
   "C1: with an NA-id row first, L1's provision shows under L1 (Renta, regular)")
ok(!is.null(mi) && nrow(mi) == 1L && identical(mi$row_id, "L2") && identical(mi$row_label, "Licencias") &&
     identical(mi$row_categoria, "regular"),
   "C2: ...and L2's converted provision (its manual item) under L2 (Licencias, regular)")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
