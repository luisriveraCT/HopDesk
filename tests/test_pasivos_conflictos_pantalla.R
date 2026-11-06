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
# pantalla(conf, repl, decisions): data.frame(id, fecha, antes, nuevo,
# decision, burbuja_nuevo) as the person sees it.
pantalla <- function(conf, repl, decisions = list()) {
  h <- xml2::read_html(as.character(.pec_render_conflicts(conf, repl, decisions)))
  filas <- xml2::xml_find_all(h, "//tbody/tr")
  do.call(rbind, lapply(filas, function(tr) {
    td <- xml2::xml_text(xml2::xml_find_all(tr, "./td"), trim = TRUE)
    marcado <- xml2::xml_attr(xml2::xml_find_first(tr, ".//input[@type='radio'][@checked]"), "value")
    etiq <- xml2::xml_text(xml2::xml_find_first(tr, ".//input[@value='aplicar']/following-sibling::label"))
    id <- sub("^pec_dec_", "", xml2::xml_attr(xml2::xml_find_first(tr, ".//input[@type='radio']"), "name"))
    data.frame(id = id, fecha = td[1], antes = td[2], nuevo = td[3],
               decision = marcado, burbuja_nuevo = etiq, stringsAsFactors = FALSE)
  }))
}
encabezados <- function() {
  h <- xml2::read_html(as.character(.pec_render_conflicts(CONF, REPL, list())))
  xml2::xml_text(xml2::xml_find_all(h, "//thead//th"))
}

ok(identical(encabezados(), c("Fecha", "Antes", "Nuevo (sugerido)", "Acción")),
   "H1: columns Fecha, Antes, Nuevo (sugerido), Acción")
v <- pantalla(CONF, REPL)
ok(nrow(v) == 2L && setequal(v$id, c(P1, P2)), "H2: one row per conflict, P1 and P2")
r1 <- v[v$id == P1, ]; r2 <- v[v$id == P2, ]
ok(identical(r1$fecha, format(F1)) && identical(r1$antes, "150.00") && identical(r1$nuevo, "120.00"),
   "H3: P1 shows its date, Antes 150 (the hand edit) and Nuevo 120")
ok(identical(r1$decision, "mantener") && identical(r2$decision, "mantener"),
   "H4: every row starts on Mantener")
ok(identical(r1$burbuja_nuevo, "Aplicar 120.00"), "H5: the Aplicar option names the suggested 120")
# Hallazgo 1 (today): the date conflict shows only amounts -- its hand date,
# and the generated date nowhere.
ok(identical(r2$fecha, format(F2_MANO)) && identical(r2$antes, "100.00") && identical(r2$nuevo, "120.00"),
   "H6: P2 (a date conflict) shows its hand date and only amounts, 100 -> 120")
v <- pantalla(CONF, REPL, setNames(list("aplicar"), P1))
ok(identical(v$decision[v$id == P1], "aplicar"), "H7: a chosen option is the one marked")

# ── What the panel does (its server, one session) ─────────────────────────────
k <- function(id) gsub("-", "_", id)
panel <- function(expr_fn) {
  sh <- nuevo_shared(load_pasivos_provisions(client_id = CID))
  testServer(function(input, output, session) setup_pasivos_edit_confirm(input, output, session, sh), {
    session$flushReact()
    expr_fn(session, sh)
  })
}
abrir <- function(session, sh) {
  pasivos_edit_confirm_open(session, CONF, REPL, sh, "L1"); session$flushReact()
}
decidir <- function(session, valores) {
  for (id in names(valores)) {
    v <- valores[[id]]
    l <- setNames(list(v[[1]]), paste0("pec_decision_", k(id)))
    if (length(v) > 1L) l[[paste0("pec_custom_", k(id))]] <- v[[2]]
    do.call(session$setInputs, l)
  }
}

# D1: Nuevo on both -- P1 takes 120 and loses its override; P2 takes 120 but
# keeps its hand date and its date override (the save is unchanged in T20).
sembrar()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar", "aplicar"), c(P1, P2)))
  session$setInputs(pec_save = 1)
})
p1 <- en_s3(P1); p2 <- en_s3(P2)
ok(isTRUE(p1$amount_pago == 120) && is.na(p1$amount_pago_override), "D1: Nuevo on P1 -> 120, override cleared")
ok(isTRUE(p2$amount_pago == 120) && identical(p2$fecha_efectiva, F2_MANO) &&
     identical(p2$fecha_efectiva_override, F2_MANO),
   "D1: Nuevo on P2 -> 120, its hand date and date override stay")

# D2: Mantener (nothing chosen) writes P1 as it was; Personalizar 135 sets it.
sembrar()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list(list("personalizar", "135")), P1))
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago_override == 135), "D2: Personalizar 135 -> P1's override 135")
ok(identical(en_s3(P2)$fecha_efectiva_override, F2_MANO), "D2: P2, left on Mantener, is as it was")

# D3 (today, hallazgo 4): Personalizar with nothing typed writes nothing and
# still says the decisions were saved.
sembrar(); avisos <- list()
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list(list("personalizar", "")), P1))
  session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago_override == 150), "D3: Personalizar with nothing typed leaves P1 at 150")
ok(identical(ultimo_aviso()$texto, "Decisiones guardadas."), "D3: and says \"Decisiones guardadas.\"")

# D4: "Aplicar todos: Nuevo valor" then Guardar.
sembrar()
panel(function(session, sh) {
  abrir(session, sh); session$setInputs(pec_all_nuevo = 1); session$setInputs(pec_save = 1)
})
ok(isTRUE(en_s3(P1)$amount_pago == 120) && is.na(en_s3(P1)$amount_pago_override),
   "D4: Aplicar todos: Nuevo valor -> P1 takes 120")

# D5 (today, hallazgo 3): cancelling and opening again keeps the old choice.
sembrar()
v <- local({
  panel(function(session, sh) {
    abrir(session, sh); decidir(session, setNames(list("aplicar"), P1))
    session$setInputs(pec_cancel = 1); session$setInputs(pec_cancel_yes = 1)
    abrir(session, sh); session$setInputs(pec_save = 1)
  }); en_s3(P1) })
ok(isTRUE(v$amount_pago == 120), "D5: after Cancelar and opening again, Guardar applies the old Aplicar on P1")

# ── The save (regla 17) ───────────────────────────────────────────────────────
# G1 (today): a failed write still says "Decisiones guardadas.".
sembrar(); avisos <- list()
guardar_real <- save_pasivos_provisions
save_pasivos_provisions <- function(...) stop("simulated S3 write failure")
panel(function(session, sh) {
  abrir(session, sh); decidir(session, setNames(list("aplicar"), P1))
  session$setInputs(pec_save = 1)
  p <- shiny::isolate(sh$pasivos_provisions_db())
  ok(is.na(p$amount_pago_override[p$id == P1]), "G1: the session's copy takes the change anyway")
})
save_pasivos_provisions <- guardar_real
ok(isTRUE(en_s3(P1)$amount_pago_override == 150), "G1: S3 has nothing (the write failed)")
ok(identical(ultimo_aviso()$texto, "Decisiones guardadas."), "G1: yet it says \"Decisiones guardadas.\"")

# G2 (today): a failed read empties the session's provisions.
sembrar()
panel(function(session, sh) {
  abrir(session, sh)
  .mem_falla <<- .s3_key(S3_KEYS$pasivos_provisions, client_id = CID)
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
  decidir(session, setNames(list("aplicar"), P1)); session$setInputs(pec_save = 1)
  .mem_falla <<- character()
  ok(!nrow(shiny::isolate(sh$pasivos_provisions_db())), "G2: a failed read leaves the session with no provisions")
})

untrace("showNotification", where = asNamespace("shiny"))
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
