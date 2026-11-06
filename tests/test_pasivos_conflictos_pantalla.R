# =============================================================================
# tests/test_pasivos_conflictos_pantalla.R
# T20 step 0: what the edit-confirm panel ("Conflictos al editar un pasivo")
# shows and does today, before it moves to hd_table. Real wizard save on
# in-memory S3 (tests/helpers/s3_memoria.R), real panel server under
# testServer. Two conflicts of L1 after its amount goes 100 -> 120:
#   P1  amount edited by hand to 150 (an amount conflict)
#   P2  date moved by hand 3 days later, amount untouched (a date conflict)
# Run from project root:  Rscript tests/test_pasivos_conflictos_pantalla.R
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
            "R/pasivos_wizard_module.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
has_capability    <- function(user, cap) TRUE
pasivos_autoriza  <- function(...) TRUE
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
pasivos_log_audit <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

# Every notification the panel shows: its text and type.
avisos <- list()
trace("showNotification", where = asNamespace("shiny"), print = FALSE,
      tracer = quote(avisos[[length(avisos) + 1L]] <<- list(texto = paste(ui, collapse = ""), tipo = type[1])))
ultimo_aviso <- function() if (length(avisos)) avisos[[length(avisos)]] else list(texto = "", tipo = "")

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date(), amount_default = 100), mode = "create", user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date())
P1 <- provs0$id[3]; P2 <- provs0$id[5]
provs0$amount_pago_override[provs0$id == P1] <- 150
F2_MANO <- provs0$fecha_efectiva[provs0$id == P2] + 3L
provs0$fecha_efectiva[provs0$id == P2]          <- F2_MANO
provs0$fecha_efectiva_override[provs0$id == P2] <- F2_MANO
F1 <- provs0$fecha_efectiva[provs0$id == P1]

nuevo_shared <- function(provs = provs0) {
  L <- reactiveVal(liab0); P <- reactiveVal(provs)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo"),
       holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function() {
  s3_memoria_reset()
  save_pasivos_liabilities(liab0, client_id = CID)
  .s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID)
}
en_s3 <- function(id) { p <- load_pasivos_provisions(client_id = CID); p[p$id == id, , drop = FALSE] }

# The wizard's real save; what it hands the panel is kept in `abierto`.
abierto <- NULL
pasivos_edit_confirm_open <- function(session, conflicts, replacement, shared, liability_id)
  abierto <<- list(conflicts = conflicts, replacement = replacement, liability_id = liability_id)
guardar_asistente <- function(shared) {
  draft <- pasivos_liability_to_draft(liab0); draft$amount_default <- 120
  wiz <- list(mode = "edit", edit_target_id = "L1", draft = draft)
  testServer(function(input, output, session) .pasivos_wizard_save(wiz, shared, session, "ana"), {})
}
sembrar(); guardar_asistente(nuevo_shared())
CONF <- abierto$conflicts; REPL <- abierto$replacement
ok(setequal(CONF$id, c(P1, P2)), "S0: the wizard save hands the panel P1 (amount) and P2 (date)")
suppressMessages(source("R/pasivos_edit_confirm_module.R"))   # the real open again


# ── What the panel shows: one row per conflict, by id ─────────────────────────
# Since T20 the panel is hd_table "conflictos_pasivo": its rows are
# pasivos_conflictos_filas() and its columns .pec_columnas(). Until 80bfa1a
# (step 0) this read the HTML of .pec_render_conflicts(); the [CAMBIA] below
# say what changed and why.
fecha_txt  <- function(d) col_fecha("f")$valores(d)$mostrar
COLS <- .pec_columnas()
titulos <- vapply(COLS, function(c) .hd_texto(c$titulo), character(1))
op_col  <- Filter(function(c) identical(c$tipo, "opcion"), COLS)[[1]]

# [CAMBIA] H1 (T20): "Acción" (three radios with a number box) is now
# Decisión (Nuevo / Mantener / Otro, opcion_fila) and Otro monto (edicion_celda).
ok(identical(unname(titulos), c("Fecha", "Antes", "Nuevo (sugerido)", "Decisión", "Otro monto")),
   "H1: columns Fecha, Antes, Nuevo (sugerido), Decisión, Otro monto")
v <- pasivos_conflictos_filas(CONF, REPL)
ok(nrow(v) == 2L && setequal(v$prov_id, c(P1, P2)) && identical(v$id, paste0("1:", v$prov_id)),
   "H2: one row per conflict, P1 and P2, by provision id")
ok(identical(v$fecha, sort(v$fecha)), "H2: sorted by date")
r1 <- v[v$prov_id == P1, ]; r2 <- v[v$prov_id == P2, ]
G1 <- as.Date(REPL$fecha_efectiva[REPL$id == P1]); G2 <- as.Date(REPL$fecha_efectiva[REPL$id == P2])
# [CAMBIA] H3 (T20): Antes and Nuevo are "fecha · monto" (they were amounts only).
ok(identical(r1$fecha, F1) && identical(r1$antes, paste(fecha_txt(F1), "· $150.00")) &&
     identical(r1$nuevo, paste(fecha_txt(G1), "· $120.00")),
   "H3: P1 shows its date, Antes <date> · $150.00 (the hand edit) and Nuevo <date> · $120.00")
ok(identical(v$decision, c("mantener", "mantener")) && all(is.na(v$otro)),
   "H4: every row starts on Mantener, with no Otro amount")
ok(identical(unname(vapply(names(op_col$opciones), function(o) .hd_texto(op_col$opciones[[o]]), character(1))),
             c("Nuevo", "Mantener", "Otro")) && identical(op_col$burbuja, c(aplicar = "nuevo")),
   "H5: [CAMBIA] options Nuevo / Mantener / Otro (T17a's names); Nuevo's bubble is the suggested value")
# [CAMBIA] H6 (T20, hallazgo 1, view only): the date conflict shows both dates.
ok(identical(r2$fecha, F2_MANO) && identical(r2$antes, paste(fecha_txt(F2_MANO), "· $100.00")) &&
     identical(r2$nuevo, paste(fecha_txt(G2), "· $120.00")) && !identical(G2, F2_MANO),
   "H6: P2 (a date conflict) shows its hand date in Antes and the generated one in Nuevo")

# ── What the panel does (its server, one session) ─────────────────────────────
NO_MONTO <- function(n) tr("tablas.pasivos.conflictos.otro_sin_monto", n = n)
mensajes <- list()
panel <- function(expr_fn) {
  sh <- nuevo_shared(load_pasivos_provisions(client_id = CID))
  testServer(function(input, output, session) setup_pasivos_edit_confirm(input, output, session, sh), {
    session$sendCustomMessage <- function(type, message)
      mensajes[[length(mensajes) + 1L]] <<- list(type = type, message = message)
    session$flushReact()
    expr_fn(session, sh)
  })
}
abrir <- function(session, sh) {
  pasivos_edit_confirm_open(session, CONF, REPL, sh, "L1"); session$flushReact()
}
# The table's row id for a provision, in the panel open now.
fila <- function(session, pid) paste0(shiny::isolate(session$userData$pec_state$apertura), ":", pid)
# What the browser sends: each row's choice and, for Otro, what was typed.
decidir <- function(session, opciones, montos = list(), ids = NULL) {
  ids_op <- ids %||% vapply(names(opciones), function(p) fila(session, p), character(1))
  l <- list(`pec_tabla-opciones` = list(ids = as.list(unname(ids_op)), valores = unname(opciones)))
  if (length(montos))
    l[["pec_tabla-ediciones"]] <- list(ids = as.list(vapply(names(montos), function(p) fila(session, p), character(1))),
                                       valores = unname(montos))
  do.call(session$setInputs, l)
}

# What the audit entry records, per provision (pasivos_log_audit's `after`).
auditorias <- list()
pasivos_log_audit <- function(...) { auditorias[[length(auditorias) + 1L]] <<- list(...); invisible(NULL) }
decision_auditada <- function(pid) {
  d <- auditorias[[length(auditorias)]]$after$decisions
  Filter(function(x) identical(x$provision_id, pid), d)[[1]]
}
G2_GEN <- as.Date(REPL$fecha_efectiva[REPL$id == P2])

# D1: Nuevo on both -- P1 takes 120 and loses its override.
# [CAMBIA] (Luis, T20 option A): Nuevo on P2 (a date moved by hand) takes the
# generated amount AND date, and clears the date override. Until 28b3f3d it
# kept the hand date and its override.
sembrar(); auditorias <- list()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar", "aplicar"), c(P1, P2)))
  session$setInputs(pec_save = 1)
})
p1 <- en_s3(P1); p2 <- en_s3(P2)
ok(isTRUE(p1$amount_pago == 120) && is.na(p1$amount_pago_override), "D1: Nuevo on P1 -> 120, override cleared")
ok(isTRUE(p2$amount_pago == 120) && identical(p2$fecha_efectiva, G2_GEN) && is.na(p2$fecha_efectiva_override),
   "D1: Nuevo on P2 -> 120 and the generated date; its date override is cleared")
a2 <- decision_auditada(P2)
ok(identical(a2$decision, "aplicar") && identical(a2$fecha_antes, format(F2_MANO)) &&
     identical(a2$fecha_despues, format(G2_GEN)),
   "D1: the audit entry records P2's date before (the hand one) and after (the generated one)")
ok(identical(decision_auditada(P1)$fecha_antes, format(F1)) && identical(decision_auditada(P1)$fecha_despues, format(F1)),
   "D1: and P1's, the same before and after (only its amount was in conflict)")

# D2: Otro 135 sets P1's override; P2, left on Mantener, keeps its hand date
# and its override (and its amount).
sembrar(); auditorias <- list()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("personalizar"), P1), setNames(list(135), P1))
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago_override == 135), "D2: Otro 135 -> P1's override 135")
p2 <- en_s3(P2)
ok(identical(p2$fecha_efectiva, F2_MANO) && identical(p2$fecha_efectiva_override, F2_MANO) && isTRUE(p2$amount_pago == 100),
   "D2: P2, left on Mantener, keeps its hand date, its override and its amount")
ok(identical(decision_auditada(P2)$fecha_antes, format(F2_MANO)) && identical(decision_auditada(P2)$fecha_despues, format(F2_MANO)),
   "D2: the audit records P2's date unchanged")

# D2b (option A): Otro on P2 changes only its amount; Antes' date (the hand
# one) and its override stay.
sembrar()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("personalizar"), P2), setNames(list(90), P2))
  session$setInputs(pec_save = 1)
})
p2 <- en_s3(P2)
ok(isTRUE(p2$amount_pago_override == 90) && identical(p2$fecha_efectiva, F2_MANO) &&
     identical(p2$fecha_efectiva_override, F2_MANO),
   "D2b: Otro 90 on P2 -> its amount 90, its hand date and override stay")

# [CAMBIA] D3 (T20, hallazgo 4): Otro without a valid amount saves nothing --
# not even the other rows -- keeps the panel open and says so. It used to
# save the rest and say "Decisiones guardadas.".
cerrados <- 0L
trace("removeModal", where = asNamespace("shiny"), print = FALSE, tracer = quote(cerrados <<- cerrados + 1L))
for (caso in list(list(nombre = "nothing typed", montos = list()),
                  list(nombre = "a negative amount", montos = setNames(list(-5), P1)))) {
  sembrar(); avisos <- list(); cerrados <- 0L
  panel(function(session, sh) {
    abrir(session, sh); escrituras <- .mem_escrituras
    decidir(session, setNames(list("personalizar", "aplicar"), c(P1, P2)), caso$montos)
    session$setInputs(pec_save = 1)
    ok(.mem_escrituras == escrituras, paste0("D3: Otro with ", caso$nombre, " writes nothing"))
  })
  ok(isTRUE(en_s3(P1)$amount_pago_override == 150) && identical(en_s3(P2)$fecha_efectiva_override, F2_MANO) &&
       isTRUE(en_s3(P2)$amount_pago == 100),
     paste0("D3: with ", caso$nombre, ", P1 and P2 (on Nuevo) are as they were"))
  ok(identical(ultimo_aviso()$texto, NO_MONTO(1)) && identical(ultimo_aviso()$tipo, "warning") && cerrados == 0L,
     paste0("D3: with ", caso$nombre, ", it says an Otro amount is missing and the panel stays open"))
}

# D4: an amount typed on a row left on Nuevo doesn't count (al_escribir).
sembrar()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar"), P1), setNames(list(135), P1))
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago == 120) && is.na(en_s3(P1)$amount_pago_override),
   "D4: 135 typed but Nuevo chosen -> P1 takes Nuevo's 120")

# D5: "Aplicar todos: Nuevo" sets every row's choice in the browser; the
# browser's answer is what is saved.
sembrar(); mensajes <- list()
panel(function(session, sh) {
  abrir(session, sh); session$setInputs(pec_all_nuevo = 1)
  m <- Filter(function(x) identical(x$type, "hdt-opciones"), mensajes)
  ok(length(m) == 1L && identical(m[[1]]$message$opcion, "aplicar") && is.null(m[[1]]$message$ids) &&
       identical(m[[1]]$message$id, "pec_tabla-hdt"),
     "D5: Aplicar todos: Nuevo puts Nuevo on every row of the table")
  decidir(session, setNames(list("aplicar", "aplicar"), c(P1, P2)))
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago == 120) && is.na(en_s3(P1)$amount_pago_override),
   "D5: and Guardar applies it: P1 takes 120")

# [CAMBIA] D6 (T20, hallazgo 3): opening the panel again starts on Mantener.
# A choice the browser still holds from the cancelled panel ("1:<P1>") can't
# land on the new one ("2:<P1>"). It used to come back and be saved.
sembrar(); avisos <- list()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar"), P1))
  session$setInputs(pec_cancel = 1); session$setInputs(pec_cancel_yes = 1)
  abrir(session, sh)
  ok(identical(shiny::isolate(session$userData$pec_state$apertura), 2L), "D6: the panel is opened a second time")
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago_override == 150), "D6: Guardar after opening again keeps P1 at 150 (Mantener)")

# D7 (T20): "Continuar revisando" gives the panel back with its choices: Nuevo
# on P1, Otro 135 on P2, and Guardar saves them without choosing again.
sembrar()
panel(function(session, sh) {
  abrir(session, sh)
  decidir(session, setNames(list("aplicar", "personalizar"), c(P1, P2)), setNames(list(135), P2))
  session$setInputs(pec_cancel = 1); session$setInputs(pec_cancel_no = 1)
  st <- session$userData$pec_state
  f <- pasivos_conflictos_filas(shiny::isolate(st$conflicts), shiny::isolate(st$replacement),
                                shiny::isolate(st$apertura), shiny::isolate(st$restaurado))
  ok(identical(f$decision[f$prov_id == P1], "aplicar") && identical(f$decision[f$prov_id == P2], "personalizar") &&
       isTRUE(f$otro[f$prov_id == P2] == 135) && all(startsWith(f$id, "2:")),
     "D7: Continuar revisando shows Nuevo on P1 and Otro 135 on P2 again")
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago == 120) && isTRUE(en_s3(P2)$amount_pago_override == 135),
   "D7: and Guardar saves them: P1 120, P2 135")
untrace("removeModal", where = asNamespace("shiny"))
# ── The save (regla 17) ───────────────────────────────────────────────────────
# [CAMBIA] G1-G2 (T20, regla 17). Until 80bfa1a a failed write still said
# "Decisiones guardadas." and put the change in the session's copy, and a
# failed read replaced the session's provisions with an empty table. Now
# either one saves nothing, leaves the session as it was, keeps the panel
# open (no removeModal: the choices stay) and says so.
cerrados <- 0L
trace("removeModal", where = asNamespace("shiny"), print = FALSE, tracer = quote(cerrados <<- cerrados + 1L))
NO_GUARDADO <- tr("tablas.pasivos.conflictos.no_guardado")

# G1: a failed write.
sembrar(); avisos <- list(); cerrados <- 0L
guardar_real <- save_pasivos_provisions
save_pasivos_provisions <- function(...) stop("simulated S3 write failure")
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar"), P1))
  session$setInputs(pec_save = 1)
  p <- shiny::isolate(sh$pasivos_provisions_db())
  ok(isTRUE(p$amount_pago_override[p$id == P1] == 150), "G1: the session's copy is as it was (150)")
})
save_pasivos_provisions <- guardar_real
ok(isTRUE(en_s3(P1)$amount_pago_override == 150), "G1: S3 has nothing (the write failed)")
ok(identical(ultimo_aviso()$texto, NO_GUARDADO) && identical(ultimo_aviso()$tipo, "error"),
   "G1: it says the decisions were not saved")
ok(!any(vapply(avisos, function(a) identical(a$texto, "Decisiones guardadas."), logical(1))),
   "G1: and never \"Decisiones guardadas.\"")
ok(cerrados == 0L, "G1: the panel stays open")

# G2: a failed read.
sembrar(); avisos <- list(); cerrados <- 0L; antes <- NULL
panel(function(session, sh) {
  abrir(session, sh)
  antes <<- shiny::isolate(sh$pasivos_provisions_db())
  .mem_falla <<- .s3_key(S3_KEYS$pasivos_provisions, client_id = CID)
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
  escrituras <- .mem_escrituras
  decidir(session, setNames(list("aplicar"), P1)); session$setInputs(pec_save = 1)
  .mem_falla <<- character()
  ok(identical(shiny::isolate(sh$pasivos_provisions_db()), antes), "G2: a failed read leaves the session's provisions as they were")
  ok(.mem_escrituras == escrituras, "G2: and writes nothing")
})
ok(identical(ultimo_aviso()$texto, NO_GUARDADO) && cerrados == 0L, "G2: it says so and the panel stays open")

# G3: the save that works still closes the panel and says so.
sembrar(); avisos <- list(); cerrados <- 0L
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar"), P1)); session$setInputs(pec_save = 1)
})
ok(identical(ultimo_aviso()$texto, "Decisiones guardadas.") && cerrados == 1L && isTRUE(en_s3(P1)$amount_pago == 120),
   "G3: a save that works writes, closes the panel and says \"Decisiones guardadas.\"")
untrace("removeModal", where = asNamespace("shiny"))

untrace("showNotification", where = asNamespace("shiny"))
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
