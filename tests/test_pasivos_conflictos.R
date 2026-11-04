# =============================================================================
# tests/test_pasivos_conflictos.R
# T18a. Editing a liability whose future provision was edited by hand (an
# override the new terms would change) opens the edit-confirm panel. Until
# 2026-11-04 the wizard save left that provision out of what it wrote, so it
# was gone from S3 before the panel opened, and every decision -- "Mantener"
# included -- found nothing to act on. Real wizard save and real decisions,
# on in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_pasivos_conflictos.R
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
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_list_module.R"))
  suppressMessages(source(f))
has_capability    <- function(user, cap) TRUE
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
pasivos_log_audit <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date(), amount_default = 100), mode = "create", user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date())
P1 <- provs0$id[3]                                   # a future one, edited by hand
provs0$amount_pago_override[provs0$id == P1] <- 150

# The session's copies, as reactive values; the wizard reads and writes them.
nuevo_shared <- function(liab = liab0, provs = provs0) {
  L <- reactiveVal(liab); P <- reactiveVal(provs)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo"),
       holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function(liab = liab0, provs = provs0) {
  s3_memoria_reset()
  save_pasivos_liabilities(liab, client_id = CID)
  .s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
}
en_s3 <- function() load_pasivos_provisions(client_id = CID)

# Save the wizard on L1 with a new amount (120); capture what the panel gets.
abierto <- NULL
pasivos_edit_confirm_open <- function(session, conflicts, replacement, shared, liability_id)
  abierto <<- list(conflicts = conflicts, replacement = replacement, liability_id = liability_id)
guardar_asistente <- function(shared, liab = liab0, user = "ana") {
  draft <- pasivos_liability_to_draft(liab); draft$amount_default <- 120
  wiz <- list(mode = "edit", edit_target_id = liab$id, draft = draft)
  res <- NULL
  testServer(function(input, output, session) {
    res <<- .pasivos_wizard_save(wiz, shared, session, user)
  }, {})
  res
}

# ── A1: the save keeps the conflict as it was ─────────────────────────────────
sembrar(); abierto <- NULL
sh <- nuevo_shared()
res <- guardar_asistente(sh)
ok(isTRUE(res$ok), "A1: the wizard save succeeds")
ok(identical(abierto$conflicts$id, P1), "A1: the panel opens with P1 as the one conflict")
s3 <- en_s3()
ok(P1 %in% s3$id, "A1: P1 is still on S3 after the wizard save")
ok(isTRUE(s3$amount_pago_override[s3$id == P1] == 150), "A1: P1 keeps its hand-edited 150")
ok(P1 %in% isolate(sh$pasivos_provisions_db())$id, "A1: and in the session's copy")
otra <- s3[s3$id == provs0$id[4], ]
ok(isTRUE(otra$amount_pago == 120), "A1: a provision without override takes the new 120")

# ── A2: the panel's suggested value is P1's generated row ─────────────────────
rep <- abierto$replacement
ok(identical(rep$id, P1), "A2: the replacement carries P1's id")
ok(isTRUE(rep$amount_pago == 120), "A2: and the generated amount, 120")

decidir <- function(dec, custom = NULL) {
  sembrar(); abierto <<- NULL
  sh <- nuevo_shared(); guardar_asistente(sh)
  d <- setNames(list(dec), P1)
  if (!is.null(custom)) d[[paste0(P1, "_custom")]] <- custom
  .pec_apply_decisions(abierto$conflicts, abierto$replacement, d, sh, "ana")
  r <- en_s3(); r[r$id == P1, , drop = FALSE]
}

# ── A3: each decision ─────────────────────────────────────────────────────────
r <- decidir("mantener")
ok(nrow(r) == 1L && isTRUE(r$amount_pago_override == 150) && isTRUE(r$amount_pago == 100),
   "A3: Mantener leaves P1 exactly as it was (override 150)")
r <- decidir("aplicar")
ok(nrow(r) == 1L && is.na(r$amount_pago_override) && isTRUE(r$amount_pago == 120),
   "A3: Aplicar gives P1 the generated 120 and clears the override")
r <- decidir("personalizar", custom = "135")
ok(nrow(r) == 1L && isTRUE(r$amount_pago_override == 135),
   "A3: Personalizar 135 sets P1's override to 135")
# Cancelar writes nothing: after the save alone P1 is as it was (A1).

# ── B: two sessions, each with its own panel ──────────────────────────────────
# Real pasivos_edit_confirm_open() again (A stubbed it to capture its input).
sembrar(); abierto <- NULL
sh <- nuevo_shared(); guardar_asistente(sh)          # B's conflict: P1 of L1
conf_B <- abierto$conflicts; rep_B <- abierto$replacement
suppressMessages(source("R/pasivos_edit_confirm_module.R"))

sesion <- function() {
  s <- MockShinySession$new()
  withReactiveDomain(s, setup_pasivos_edit_confirm(s$input, s$output, s, nuevo_shared()))
  s
}
A <- sesion(); B <- sesion()
withReactiveDomain(B, pasivos_edit_confirm_open(B, conf_B, rep_B, sh, "L1"))
B$flushReact()
ok(identical(isolate(B$userData$pec_state$conflicts$id), P1),
   "B1: B's panel holds B's conflict")
ok(is.null(isolate(A$userData$pec_state$conflicts)),
   "B1: A's panel is still empty after B opens one")

# A clicks "Aplicar todos: Nuevo valor" and "Guardar decisiones": with nothing
# open in A, nothing may be written -- above all not B's conflict.
antes <- .mem_escrituras
A$setInputs(pec_all_nuevo = 1); A$setInputs(pec_save = 1)
r <- en_s3(); r <- r[r$id == P1, , drop = FALSE]
ok(.mem_escrituras == antes, "B2: A's Guardar decisiones writes nothing")
ok(isTRUE(r$amount_pago_override == 150), "B2: B's P1 keeps its 150 after A's save")

# B decides on its own conflict: that one does apply.
B$setInputs(pec_all_nuevo = 1); B$setInputs(pec_save = 1)
r <- en_s3(); r <- r[r$id == P1, , drop = FALSE]
ok(is.na(r$amount_pago_override) && isTRUE(r$amount_pago == 120),
   "B3: B's Guardar decisiones applies B's choice to P1")

# ── C: editing keeps estado and creator; no ✎ on an archived row ─────────────
pasivos_edit_confirm_open <- function(...) invisible(NULL)
# What the save wrote, read raw: load_pasivos_liabilities() turns every
# POSIXct column into NA (methods::as() with a two-class target; reported
# apart in T18a, not changed here), so the creation time is checked as stored.
liab_en_s3 <- function(id) {
  l <- get(paste0(CID, "/", S3_KEYS$pasivos_liabilities), envir = .mem)
  l[l$id == id, , drop = FALSE]
}
creado <- Sys.time() - 90 * 86400                 # local time, as the app writes it

# C1: a paused liability (pausing removed its future provisions) edited by beto
pausado <- liab0; pausado$id <- "L2"; pausado$estado <- "paused"
pausado$created_by <- "ana"; pausado$created_at <- creado
sembrar(pausado, provs0[0, ])
res <- guardar_asistente(nuevo_shared(pausado, provs0[0, ]), pausado, user = "beto")
l <- liab_en_s3("L2")
ok(isTRUE(res$ok), "C1: editing a paused liability saves")
ok(identical(l$estado, "paused"), "C1: and it stays paused")
ok(isTRUE(l$amount_default == 120), "C1: with its new amount")
ok(identical(l$created_by, "ana") && isTRUE(as.numeric(l$created_at) == as.numeric(creado)),
   "C1: its creator and creation time are kept")
ok(identical(l$updated_by, "beto"), "C1: beto is the one who updated it")
ok(!nrow(en_s3()), "C1: no provisions are generated for it while paused")

# C2: an active one edited by beto keeps its creator (and stays active)
activo <- liab0; activo$created_by <- "ana"; activo$created_at <- creado
sembrar(activo)
guardar_asistente(nuevo_shared(activo), activo, user = "beto")
l <- liab_en_s3("L1")
ok(identical(l$estado, "active") && identical(l$created_by, "ana"),
   "C2: an edited active liability stays active, created by ana")

# C3: an archived one is not saved by the wizard at all
archivado <- liab0; archivado$id <- "L3"; archivado$estado <- "deleted"
sembrar(archivado, provs0[0, ])
antes <- .mem_escrituras
res <- guardar_asistente(nuevo_shared(archivado, provs0[0, ]), archivado, user = "beto")
ok(!isTRUE(res$ok) && .mem_escrituras == antes && identical(liab_en_s3("L3")$estado, "deleted"),
   "C3: the wizard refuses to save an archived liability, and writes nothing")

# C4: ✎ on the active row; none for the archived one (since T18 it is not in
# the Lista at all: it is in the Papelera, whose only button is ↶)
liabs_lista <- dplyr::bind_rows(liab0, archivado)
testServer(pasivos_list_module_server, args = list(shared = list(
  pasivos_liabilities_db = function() liabs_lista,
  pasivos_provisions_db  = function() provs0[0, ],
  visible_initials       = function() NULL,
  current_user           = function() "ana")), {
  session$flushReact()
  df <- lista_datos()
  ok(!"L3" %in% df$id, "C4: no ✎ on the archived row (it is not in the Lista)")
  ok(grepl("editar", df$.acciones[df$id == "L1"]), "C4: ✎ still on the active row")
})
testServer(pasivos_papelera_server, args = list(shared = list(
  pasivos_liabilities_db = function() liabs_lista,
  pasivos_provisions_db  = function() provs0[0, ],
  visible_initials       = function() NULL,
  current_user           = function() "ana")), {
  session$flushReact()
  ok(identical(papelera_datos()$.acciones[papelera_datos()$id == "L3"], "deshacer"),
     "C4: in the Papelera the archived one has ↶ only, no ✎")
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
