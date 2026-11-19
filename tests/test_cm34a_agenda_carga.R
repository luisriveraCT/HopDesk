# =============================================================================
# tests/test_cm34a_agenda_carga.R
# CM34a. Where the session loads the Agenda, a failed read must never become
# an empty or a foreign Agenda, nor a write.
#   1  Client switch (app.R, the [CTX] pagar_hoy block): a failed read of the
#      new client's Agenda left ph NULL and pagar_hoy_db() still holding the
#      PREVIOUS client's rows under the new client -- shown, and the base of
#      a confirm or Vaciar. Now the Agenda is emptied first; on a failed read
#      it stays empty with a notice, and nothing of the previous client is
#      ever shown or saved under the new one.
# The real app.R blocks (found by a marker, evaluated here) on the real
# persistence over the in-memory S3.
#
# Run from project root:  Rscript tests/test_cm34a_agenda_carga.R
# =============================================================================
cat("=== test_cm34a_agenda_carga ===\n\n")
suppressPackageStartupMessages({ library(dplyr) })
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
suppressMessages(source("R/i18n.R"))
for (e in parse("R/erp_connector_registry.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# The first local({...}) in app.R whose code contains `marca`.
bloque_app <- function(marca) {
  encontrado <- NULL
  recorrer <- function(e) {
    if (!is.null(encontrado) || !is.call(e)) return(invisible())
    if (identical(e[[1]], as.name("local")) &&
        grepl(marca, paste(deparse(e, width.cutoff = 500L), collapse = "\n"), fixed = TRUE)) {
      encontrado <<- e; return(invisible())
    }
    # e[[i]] straight into is.call(): an empty argument (x[, 1]) bound to a
    # variable would error when evaluated.
    for (i in seq_along(e)[-1]) if (is.call(e[[i]])) recorrer(e[[i]])
  }
  for (e in parse("app.R", keep.source = FALSE, encoding = "UTF-8")) recorrer(e)
  if (is.null(encontrado)) stop("block not found in app.R: ", marca)
  encontrado
}

# The session around those blocks.
PH <- NULL
pagar_hoy_db <- function(x) if (missing(x)) PH else { PH <<- x; invisible(x) }
current_user <- function() "luis"
AVISOS <- character()
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
bump_sync_version <- function(...) invisible(TRUE)

fila <- function(id, emp) tibble::tibble(id = id, ledger = "AP", Empresa = emp, Moneda = "MXN",
                                         Documento = paste0("D-", id), Parte = "ACME SA", Codigo = "",
                                         Importe = 100, FechaVenc = as.Date("2026-11-20"), status = "pending",
                                         tipo_item = NA_character_, source = SAP_B1_ERP_TYPE)
llave <- function(cid, k) paste0(cid, "/", S3_KEYS[[k]])
poner <- function(cid, k, x) assign(llave(cid, k), x, envir = .mem)
leer  <- function(cid, k) get(llave(cid, k), envir = .mem)
sync_on <- function(cid) poner(cid, "agenda_sync_config",
                               .normalize(tibble::tibble(is_enabled = TRUE), .schema_agenda_sync_config))
de_a <- function() sum(PH$Empresa %in% "A-EMP")

# Client A in session (its Agenda shown); client B has its own shared Agenda.
preparar_salto <- function() {
  s3_memoria_reset(); AVISOS <<- character()
  sync_on("cli-a"); sync_on("cli-b")
  poner("cli-a", "pagar_hoy_sync", .normalize(dplyr::bind_rows(fila("A1", "A-EMP"), fila("A2", "A-EMP")), .schema_pagar_hoy))
  poner("cli-b", "pagar_hoy_sync", .normalize(fila("B1", "B-EMP"), .schema_pagar_hoy))
  PH <<- .normalize(dplyr::bind_rows(fila("A1", "A-EMP"), fila("A2", "A-EMP")), .schema_pagar_hoy)
}
SALTO <- bloque_app("[CTX]   pagar_hoy OK")
saltar_a <- function(cid) { env <- new.env(); env$effective_cid <- cid; eval(SALTO, env) }

# ── 1. Client switch ─────────────────────────────────────────────────────
preparar_salto()
saltar_a("cli-b")
ok("1 control: switching A -> B shows B's Agenda, nothing of A", identical(PH$id, "B1") && de_a() == 0L)

preparar_salto()
.mem_falla <- llave("cli-b", "pagar_hoy_sync")
n <- .mem_escrituras
saltar_a("cli-b")
ok("1 B's Agenda cannot be read: no row of client A is left in the session", de_a() == 0L)
ok("1 …the notice says the Agenda could not be read, and nothing was written",
   tr("tablas.agenda.aviso.no_leida") %in% AVISOS && identical(.mem_escrituras, n))

preparar_salto()
.mem_falla <- llave("cli-b", "agenda_sync_config")
saltar_a("cli-b")
ok("1 B's sync config cannot be read: no row of client A is left either (and no per-user guess)",
   de_a() == 0L && tr("tablas.agenda.aviso.no_leida") %in% AVISOS)

# The load RAISES (an unexpected error, e.g. .s3_key refusing the key):
# before CM34a ph stayed NULL and the session kept client A's Agenda.
preparar_salto()
.s3_read_with_real <- .s3_read_with
.s3_read_with <- function(key, client_id = NULL) {
  if (identical(tolower(client_id %||% ""), "cli-b")) stop("simulated unexpected read error")
  .s3_read_with_real(key, client_id)
}
saltar_a("cli-b")
.s3_read_with <- .s3_read_with_real
ok("1 the load raises an error: the session does NOT keep client A's Agenda under B (the leak)",
   de_a() == 0L && tr("tablas.agenda.aviso.no_leida") %in% AVISOS)

preparar_salto()
.mem_falla <- llave("cli-b", "agenda_sync_config")
saltar_a("cli-b")

# What that session would save for B afterwards (Vaciar / confirm rebase on
# S3 with the session copy as base) never carries A's rows.
.mem_falla <- character()
antes_b <- leer("cli-b", "pagar_hoy_sync")
invisible(suppressMessages(save_pagar_hoy(PH, "luis", client_id = "cli-b", base = PH)))
ok("1 a save for B from that session afterwards writes no row of A into B's Agenda",
   !any(leer("cli-b", "pagar_hoy_sync")$Empresa %in% "A-EMP") &&
     identical(sort(leer("cli-b", "pagar_hoy_sync")$id), sort(antes_b$id)))

# ── 2. Login (a1): the Agenda block of Phase 2 ─────────────────────────────
# Client "net": a live shared Agenda (S1, S2) and an old legacy pagar_hoy.rds
# (L1) that the first-run migration would copy over it.
CARGA <- bloque_app(".merge_early_adds")
iniciar <- function() { env <- new.env(); env$cid_lo <- "net"; eval(CARGA, env) }
preparar_login <- function(config = TRUE, sync = TRUE, early = NULL) {
  s3_memoria_reset(); AVISOS <<- character(); .mem_falla <<- character(); .mem_prohibido <<- character()
  if (config) sync_on("net")
  if (sync) poner("net", "pagar_hoy_sync", .normalize(dplyr::bind_rows(fila("S1", "N"), fila("S2", "N")), .schema_pagar_hoy))
  poner("net", "pagar_hoy", .normalize(fila("L1", "N"), .schema_pagar_hoy))
  PH <<- early
}
ids_sync <- function() sort(leer("net", "pagar_hoy_sync")$id)
config_on <- function() exists(llave("net", "agenda_sync_config"), envir = .mem) &&
  isTRUE(.normalize(leer("net", "agenda_sync_config"), .schema_agenda_sync_config)$is_enabled[1])
no_leida <- function() tr("tablas.agenda.aviso.no_leida") %in% AVISOS

preparar_login()
iniciar()
ok("2 control: sync on, the login shows the shared Agenda", identical(sort(PH$id), c("S1", "S2")))

preparar_login()
.mem_falla <- llave("net", "agenda_sync_config")
n <- .mem_escrituras
iniciar()
ok("2 the sync config cannot be read: the shared Agenda is NOT overwritten (no migration)",
   identical(ids_sync(), c("S1", "S2")))
ok("2 …nothing at all is written, and the user is told",
   identical(.mem_escrituras, n) && no_leida())

preparar_login(config = FALSE)
iniciar()
ok("2 no config but the shared Agenda exists: it is loaded, never overwritten by the legacy file",
   identical(ids_sync(), c("S1", "S2")) && identical(sort(PH$id), c("S1", "S2")) && config_on())

preparar_login(config = FALSE, sync = FALSE)
iniciar()
ok("2 control: a real first run (no config, no sync file) migrates the legacy Agenda and turns sync on",
   identical(ids_sync(), "L1") && config_on())

preparar_login(config = FALSE, sync = FALSE)
.mem_prohibido <- llave("net", "pagar_hoy_sync")
n <- .mem_escrituras
iniciar()
ok("2 no config and S3 cannot say whether the sync file exists (403): nothing migrated or written, notice",
   identical(.mem_escrituras, n) && no_leida())

preparar_login(config = FALSE, sync = FALSE)
.mem_falla <- llave("net", "pagar_hoy")
n <- .mem_escrituras
iniciar()
ok("2 first run but the legacy file cannot be read: nothing migrated or written, notice",
   identical(.mem_escrituras, n) && no_leida() && !config_on())

early <- .normalize(fila("E1", "N"), .schema_pagar_hoy)
preparar_login(early = early)
.mem_falla <- llave("net", "pagar_hoy_sync")
n <- .mem_escrituras
iniciar()
ok("2 sync on but the shared Agenda cannot be read: the early-added row is not saved over it, notice",
   identical(.mem_escrituras, n) && no_leida())
.mem_falla <- character()
ok("2 …and the shared Agenda is intact", identical(ids_sync(), c("S1", "S2")))

preparar_login(early = early)
iniciar()
ok("2 control: an early-added row is saved, rebased on the shared Agenda",
   identical(ids_sync(), c("E1", "S1", "S2")))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
