# =============================================================================
# tests/test_pasivos_liabilities_seguro.R
# T22: pasivos_liabilities saved safely. On the in-memory S3 of
# tests/helpers/s3_memoria.R (never a real bucket).
#   A  load_pasivos_liabilities() keeps created_at / updated_at (they came back
#      NA, and each save of the session's copy wrote NA for every liability)
# Run from project root:  Rscript tests/test_pasivos_liabilities_seguro.R
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_list_module.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(expr)))

CID <- "acme"
CREADO   <- as.POSIXct("2026-08-01 10:00:00")
CAMBIADO <- as.POSIXct("2026-09-01 12:30:00")
pasivo <- function(id, nombre, estado = "active") {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = nombre, empresa = "NG", parte = paste("Parte", id),
    moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
    fecha_inicio = as.Date("2026-08-01"), amount_default = 100), mode = "create", user = "ana")
  l$id <- id; l$estado <- estado; l$created_at <- CREADO; l$updated_at <- CAMBIADO; l
}
LIABS0 <- dplyr::bind_rows(pasivo("L1", "Renta"), pasivo("L2", "Licencias"), pasivo("L3", "Seguro", "paused"))
sembrar <- function(liabs = LIABS0) { s3_memoria_reset(); callado(save_pasivos_liabilities(liabs, client_id = CID)) }
en_s3 <- function() get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)   # raw, as stored

# ── A: the loader keeps the timestamps ────────────────────────────────────────
sembrar()
l <- load_pasivos_liabilities(client_id = CID)
ok(inherits(l$created_at, "POSIXct") && all(as.numeric(l$created_at) == as.numeric(CREADO)) &&
     all(as.numeric(l$updated_at) == as.numeric(CAMBIADO)),
   "A1: load_pasivos_liabilities() keeps created_at and updated_at")
callado(save_pasivos_liabilities(l, client_id = CID))
ok(all(as.numeric(en_s3()$created_at) == as.numeric(CREADO)),
   "A2: saving the loaded copy writes them back as they were (no NA on S3)")
ok(identical(l$fecha_inicio, rep(as.Date("2026-08-01"), 3L)) && is.list(l$recurrence_params),
   "A3: Date and list columns load as before")

# A4: a row whose timestamp is NA stays NA; the others keep theirs.
x <- LIABS0; x$updated_at[2] <- NA
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(is.na(l$updated_at[2]) && all(as.numeric(l$updated_at[-2]) == as.numeric(CAMBIADO)),
   "A4: an NA timestamp stays NA, the others keep theirs")

# A5: an old table without the timestamp columns, and one where they are all NA
# (logical, as the bug wrote them), load as POSIXct NA.
x <- LIABS0; x$created_at <- NULL; x$updated_at <- NA
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(inherits(l$created_at, "POSIXct") && all(is.na(l$created_at)) &&
     inherits(l$updated_at, "POSIXct") && all(is.na(l$updated_at)),
   "A5: missing or all-NA timestamp columns load as POSIXct NA")

# A6: a timestamp stored as text loads as that time.
x <- LIABS0; x$created_at <- format(CREADO, "%Y-%m-%d %H:%M:%S")
sembrar(x); l <- load_pasivos_liabilities(client_id = CID)
ok(all(as.numeric(l$created_at) == as.numeric(CREADO)), "A6: a timestamp stored as text loads as that time")

# ── B: Pausar, Borrar and Recuperar save on S3 as it is now ───────────────────
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
bumps <- character(); auditorias <- list(); avisos <- list()
bump_sync_version <- function(name, client_id = NULL) bumps <<- c(bumps, name)
avisos_actividad <- function(...) invisible(TRUE)   # the wizard's hook (C); avisos.R is not loaded here
pasivos_log_audit <- function(...) { auditorias[[length(auditorias) + 1L]] <<- list(...); invisible(NULL) }
trace("showNotification", where = asNamespace("shiny"), print = FALSE,
      tracer = quote(avisos[[length(avisos) + 1L]] <<- list(texto = paste(ui, collapse = ""), tipo = type[1])))
NO_GUARDADO <- tr("tablas.pasivos.no_guardado")
aviso_error <- function() any(vapply(avisos, function(a) identical(a$texto, NO_GUARDADO) && identical(a$tipo, "error"), logical(1)))

PROVS0 <- dplyr::bind_rows(lapply(seq_len(nrow(LIABS0)), function(i)
  pasivos_generate_provisions(LIABS0[i, ], window_start = Sys.Date())))
sembrar_todo <- function(liabs = LIABS0, provs = PROVS0) {
  sembrar(liabs); callado(.s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID))
  bumps <<- character(); auditorias <<- list(); avisos <<- list()
}
# A session's copies (what it loaded, possibly old by now).
sesion <- function(liabs = LIABS0, provs = PROVS0) {
  L <- reactiveVal(liabs); P <- reactiveVal(provs)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       effective_client_id = function() CID,
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
correr <- function(f) testServer(function(input, output, session) NULL, { f() })
fila_s3 <- function(id) { l <- en_s3(); l[l$id == id, , drop = FALSE] }
provs_s3 <- function() load_pasivos_provisions(client_id = CID)
futuras <- function(p, lid) sum(p$liability_id == lid & p$estado == "provisional" & p$fecha_efectiva >= Sys.Date())

ACCIONES <- list(
  pausar    = list(f = function(sh) pasivos_cambiar_pausa(sh, "L1", "beto", "paused"), lid = "L1", queda = "paused"),
  reanudar  = list(f = function(sh) pasivos_cambiar_pausa(sh, "L3", "beto", "active"), lid = "L3", queda = "active"),
  borrar    = list(f = function(sh) pasivos_borrar(sh, "L1", "beto"),                  lid = "L1", queda = "deleted"),
  recuperar = list(f = function(sh) pasivos_recuperar(sh, "L4", "beto"),               lid = "L4", queda = "active"))
LIABS_B <- dplyr::bind_rows(LIABS0, pasivo("L4", "Viejo", "deleted"))

# B1: each one changes its row on S3, and only that row; the others keep
# their timestamps; the session takes S3's table; one bump; its audit entry.
for (nm in names(ACCIONES)) {
  a <- ACCIONES[[nm]]
  sembrar_todo(LIABS_B); sh <- sesion(LIABS_B)
  correr(function() a$f(sh))
  s3 <- en_s3(); fila <- s3[s3$id == a$lid, ]
  otros <- s3[s3$id != a$lid, ]
  ok(identical(fila$estado, a$queda) && identical(fila$updated_by, "beto") &&
       as.numeric(fila$created_at) == as.numeric(CREADO),
     sprintf("B1 %s: %s is %s on S3, updated by beto, its created_at kept", nm, a$lid, a$queda))
  ok(identical(otros$estado, LIABS_B$estado[LIABS_B$id != a$lid]) &&
       all(as.numeric(otros$updated_at) == as.numeric(CAMBIADO)),
     sprintf("B1 %s: the other liabilities are as they were, timestamps included", nm))
  ok(identical(isolate(sh$pasivos_liabilities_db())$estado, s3$estado) && "pasivos_liabilities_db" %in% bumps &&
       length(auditorias) >= 1L && !aviso_error(),
     sprintf("B1 %s: the session holds S3's table, one bump, audited, no error", nm))
  if (nm == "pausar")
    ok(futuras(provs_s3(), "L1") == 0L && futuras(PROVS0, "L1") > 0L,
       "B1 pausar: L1's future provisional provisions left S3 (as before)")
}

# B2 (two sessions): B paused L2 meanwhile; A, whose copy still has L2
# active, deletes L1. L2 stays paused on S3, and A's session now has it so.
x <- LIABS_B; x$estado[x$id == "L2"] <- "paused"
sembrar_todo(x); sh <- sesion(LIABS_B)
correr(function() pasivos_borrar(sh, "L1", "ana"))
ok(identical(fila_s3("L2")$estado, "paused") && identical(fila_s3("L1")$estado, "deleted"),
   "B2: A deletes L1 and B's pause of L2 stays (A's old copy said active)")
ok(identical(isolate(sh$pasivos_liabilities_db())$estado[isolate(sh$pasivos_liabilities_db())$id == "L2"], "paused"),
   "B2: A's session now shows L2 paused")

# B3: another session created L9 (on S3, not in A's copy). A pauses L1: L9
# stays, and the write is not refused.
x <- dplyr::bind_rows(LIABS_B, pasivo("L9", "Nuevo de otra sesión"))
sembrar_todo(x); sh <- sesion(LIABS_B)
correr(function() pasivos_cambiar_pausa(sh, "L1", "ana", "paused"))
ok("L9" %in% en_s3()$id && identical(fila_s3("L1")$estado, "paused") && !aviso_error(),
   "B3: A pauses L1 and L9 (created by another session) is still on S3")

# B4: the decision is taken on S3's row. Another session already restored L4:
# A's Recuperar (its copy says deleted) changes nothing and logs nothing.
x <- LIABS_B; x$estado[x$id == "L4"] <- "active"
sembrar_todo(x); sh <- sesion(LIABS_B); escrituras <- .mem_escrituras
correr(function() pasivos_recuperar(sh, "L4", "ana"))
ok(.mem_escrituras == escrituras && !length(auditorias) && !length(bumps),
   "B4: restoring what S3 already has active writes nothing, logs nothing, no bump")
ok(identical(isolate(sh$pasivos_liabilities_db())$estado[isolate(sh$pasivos_liabilities_db())$id == "L4"], "active"),
   "B4: and A's session now shows it active")

# B5-B6: a failed read, then a failed write: nothing on S3, the session as it
# was, no bump, no audit, no provisions removed, and it says so.
guardar_real <- save_pasivos_liabilities
for (falla in c("lectura", "escritura")) for (nm in names(ACCIONES)) {
  a <- ACCIONES[[nm]]
  sembrar_todo(LIABS_B); sh <- sesion(LIABS_B); antes <- en_s3(); escrituras <- .mem_escrituras
  if (falla == "lectura") {
    .mem_falla <- .s3_key(S3_KEYS$pasivos_liabilities, client_id = CID)
    for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
  } else save_pasivos_liabilities <- function(...) stop("simulated S3 write failure")
  correr(function() a$f(sh))
  .mem_falla <- character(); save_pasivos_liabilities <- guardar_real
  ok(identical(en_s3(), antes) && .mem_escrituras == escrituras,
     sprintf("B5 %s, failed %s: nothing written", nm, falla))
  ok(identical(isolate(sh$pasivos_liabilities_db()), LIABS_B) && identical(isolate(sh$pasivos_provisions_db()), PROVS0) &&
       !length(bumps) && !length(auditorias),
     sprintf("B5 %s, failed %s: the session as it was, no bump, no audit", nm, falla))
  ok(aviso_error() && !any(vapply(avisos, function(v) identical(v$tipo, "message"), logical(1))),
     sprintf("B5 %s, failed %s: it says it was not saved, and nothing says it was", nm, falla))
}

# B7: pausing saves the liability but the provisions' save fails: S3 keeps
# them, the session too, and it says so (the liability is paused).
sembrar_todo(LIABS_B); sh <- sesion(LIABS_B)
guardar_p <- save_pasivos_provisions
save_pasivos_provisions <- function(...) stop("simulated S3 write failure")
correr(function() pasivos_cambiar_pausa(sh, "L1", "ana", "paused"))
save_pasivos_provisions <- guardar_p
ok(identical(fila_s3("L1")$estado, "paused") && futuras(provs_s3(), "L1") == futuras(PROVS0, "L1") &&
     identical(isolate(sh$pasivos_provisions_db()), PROVS0),
   "B7: the provisions' save fails: they stay on S3 and in the session")
ok(any(vapply(avisos, function(v) identical(v$texto, tr("tablas.pasivos.provisiones_no_quitadas")) && identical(v$tipo, "error"), logical(1))) &&
     !any(vapply(auditorias, function(v) identical(v$action_type, "provision.deleted"), logical(1))),
   "B7: it says the provisions were not removed, and logs none as deleted")
# ── C: the wizard saves the liability on S3 as it is now ──────────────────────
pasivos_edit_confirm_open <- function(...) invisible(NULL)
sesion_wiz <- function(liabs = LIABS_B, provs = PROVS0) {
  s <- sesion(liabs, provs)
  c(s, list(company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
            current_user = function() "beto"))
}
asistente <- function(sh, lid = "L1", monto = 120, mode = "edit") {
  base <- if (mode == "edit") LIABS_B[LIABS_B$id == lid, ] else pasivo("LX", "Nuevo")
  draft <- pasivos_liability_to_draft(base); draft$amount_default <- monto
  wiz <- list(mode = mode, edit_target_id = if (mode == "edit") lid, draft = draft)
  res <- NULL
  testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, sh, session, "beto"), {})
  res
}

# C1: editing L1 changes L1 on S3 (amount 120, creator kept) and nothing
# else; the session holds S3's table; its provisions are saved.
sembrar_todo(LIABS_B); sh <- sesion_wiz()
res <- asistente(sh)
l1 <- fila_s3("L1"); otros <- en_s3()[en_s3()$id != "L1", ]
ok(isTRUE(res$ok) && isTRUE(l1$amount_default == 120) && identical(l1$created_by, "ana") &&
     as.numeric(l1$created_at) == as.numeric(CREADO),
   "C1: the wizard saves L1 with 120, its creator and creation time kept")
ok(all(as.numeric(otros$updated_at) == as.numeric(CAMBIADO)) && setequal(otros$id, c("L2", "L3", "L4")),
   "C1: the other liabilities are as they were, timestamps included")
ok(identical(sort(isolate(sh$pasivos_liabilities_db())$id), sort(en_s3()$id)) && "pasivos_liabilities_db" %in% bumps &&
     "pasivos_provisions_db" %in% bumps && any(provs_s3()$liability_id == "L1" & provs_s3()$amount_pago == 120),
   "C1: the session holds S3's table; liabilities and provisions bumped; L1's provisions take 120")

# C2 (two sessions): B paused L2; A (old copy: L2 active) edits L1. L2 stays paused.
x <- LIABS_B; x$estado[x$id == "L2"] <- "paused"
sembrar_todo(x); sh <- sesion_wiz()
asistente(sh)
ok(identical(fila_s3("L2")$estado, "paused") && isTRUE(fila_s3("L1")$amount_default == 120),
   "C2: A edits L1 and B's pause of L2 stays")

# C3: another session created L9; A creates LX: L9 stays, and so does LX.
sembrar_todo(dplyr::bind_rows(LIABS_B, pasivo("L9", "De otra sesión"))); sh <- sesion_wiz()
res <- asistente(sh, mode = "create")
ok(isTRUE(res$ok) && "L9" %in% en_s3()$id && sum(en_s3()$nombre == "Nuevo") == 1L && nrow(en_s3()) == nrow(LIABS_B) + 2L,
   "C3: A creates a liability (a new id) and L9 (another session's) is still on S3")

# C4: another session deleted L1 meanwhile; A's copy has it active. A's edit
# is refused (decided on S3's row) and writes nothing.
x <- LIABS_B; x$estado[x$id == "L1"] <- "deleted"
sembrar_todo(x); sh <- sesion_wiz(); escrituras <- .mem_escrituras
res <- asistente(sh)
ok(!isTRUE(res$ok) && identical(res$msg, tr("tablas.pasivos.no_editar_borrado")) && .mem_escrituras == escrituras &&
     identical(fila_s3("L1")$estado, "deleted"),
   "C4: editing a liability another session deleted is refused, nothing written")

# C5: a failed read, then a failed write: nothing written, the session as it
# was, no bump, it says so.
for (falla in c("lectura", "escritura")) {
  sembrar_todo(LIABS_B); sh <- sesion_wiz(); antes <- en_s3(); escrituras <- .mem_escrituras
  if (falla == "lectura") {
    .mem_falla <- .s3_key(S3_KEYS$pasivos_liabilities, client_id = CID)
    for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
  } else save_pasivos_liabilities <- function(...) stop("simulated S3 write failure")
  res <- asistente(sh)
  .mem_falla <- character(); save_pasivos_liabilities <- guardar_real
  ok(!isTRUE(res$ok) && identical(res$msg, NO_GUARDADO) && identical(en_s3(), antes) && .mem_escrituras == escrituras,
     sprintf("C5 failed %s: not saved, says so, nothing written", falla))
  ok(identical(isolate(sh$pasivos_liabilities_db()), LIABS_B) && identical(isolate(sh$pasivos_provisions_db()), PROVS0) &&
       !length(bumps) && !length(auditorias),
     sprintf("C5 failed %s: the session as it was, no bump, no audit", falla))
}

# C6: the liability saves but its provisions' save fails: S3's provisions and
# the session's stay, no provisions bump, and it says so (not "guardado").
sembrar_todo(LIABS_B); sh <- sesion_wiz()
save_pasivos_provisions <- function(...) stop("simulated S3 write failure")
res <- asistente(sh)
save_pasivos_provisions <- guardar_p
ok(isTRUE(fila_s3("L1")$amount_default == 120) && identical(isolate(sh$pasivos_provisions_db()), PROVS0) &&
     !"pasivos_provisions_db" %in% bumps && !any(provs_s3()$amount_pago == 120),
   "C6: the provisions' save fails: L1 is saved, its provisions are not (S3 and the session as they were)")
ok(any(vapply(avisos, function(v) identical(v$texto, tr("tablas.pasivos.provisiones_no_guardadas")) && identical(v$tipo, "error"), logical(1))) &&
     !any(vapply(avisos, function(v) grepl("provisiones generadas", v$texto), logical(1))),
   "C6: it says the provisions were not saved, never \"N provisiones generadas\"")
# ── D: the notices come from the dictionary (es and en) ───────────────────────
dijo <- function(clave, ...) any(vapply(avisos, function(v) identical(v$texto, tr(clave, ...)), logical(1)))
sembrar_todo(LIABS_B); sh <- sesion(LIABS_B)
correr(function() pasivos_cambiar_pausa(sh, "L1", "beto", "paused"))
ok(dijo("tablas.pasivos.pausado_ok"), "D1: pausing says tablas.pasivos.pausado_ok")
avisos <- list(); correr(function() pasivos_cambiar_pausa(sh, "L1", "beto", "active"))
ok(dijo("tablas.pasivos.reanudado_ok"), "D1: resuming says tablas.pasivos.reanudado_ok")
sembrar_todo(LIABS_B); sh <- sesion_wiz(); asistente(sh)
ok(any(vapply(avisos, function(v) grepl("^Pasivo guardado\\. [0-9]+ provisiones generadas\\.$", v$texto), logical(1))) &&
     !dijo("tablas.pasivos.guardado_generadas", n = -1),
   "D2: the wizard says tablas.pasivos.guardado_generadas with its count")
claves_d <- c("hay_errores", "error_construir", "sin_filas_generadas", "error_generar", "guardado_conflictos",
              "guardado_generadas", "sin_permiso_crear", "sin_permiso_editar", "no_encontrado", "error_inesperado",
              "pausado_ok", "reanudado_ok", "no_guardado", "provisiones_no_quitadas", "provisiones_no_guardadas")
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
k <- paste0("tablas.pasivos.", claves_d)
ok(all(k %in% names(es)) && all(k %in% names(en)) && !any(unlist(es[k]) == unlist(en[k])),
   "D3: every T22 key is in es and en, translated")
codigo <- c(readLines("R/pasivos_wizard_module.R", encoding = "UTF-8"), readLines("R/pasivos_list_module.R", encoding = "UTF-8"))
ok(!any(grepl('showNotification\\(\\s*"', codigo)) && !any(grepl('msg = (paste0\\()?"', codigo)),
   "D4: no notice or error message written by hand in the wizard and the Lista")
untrace("showNotification", where = asNamespace("shiny"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
