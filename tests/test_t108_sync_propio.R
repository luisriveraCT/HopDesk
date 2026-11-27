# =============================================================================
# tests/test_t108_sync_propio.R
# T108 (2): a session's own save does not recompute the app a second time,
# and no other session's change is ever lost.
#
# The sync bus (R/sync_bus.R) rereads a table when its version moves, and now
# invalidates it only when the content changed (.sync_mismo_contenido():
# plain data frames ordered by id, identical()). A save leaves the saving
# session holding exactly what a load returns (pasivos_liabilities_como_se_leen(),
# the provisions written after the rebase), so its own bump finds nothing new.
#
#   U  the comparison: same rows in another order = same; one value = different
#   S1 A saves; its poll rereads and invalidates nothing; A's copy is identical()
#      to load_*() (the frecuencia_pago NA -> "mensual" of the loader included)
#   S2 B saved something else first; then A saves: A holds both changes
#   S3 A saves; B saves after: A's poll reloads and A holds both
#   S4 a writer that keeps its own row order: its own bump recomputes nothing
#   M  the cost of comparing ~1,600 provisions
# Real wizard save, real sync bus, in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_t108_sync_propio.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
raiz <- normalizePath(".", winslash = "/")
source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/global.R", c("fmt_money", "%||%"))
cargar("R/data_pipeline.R", c("etiquetas_por_factura", ".etiquetas_doc_unica", ".politica_doc", ".politica_doc_unica",
                              "parse_sap_date", "ensure_dates", "ensure_documento", "ensure_moneda",
                              "standardize_party", "guess_doc_col", "guess_col", "clean_str"))
sys.source("R/zona.R", envir = globalenv())
sys.source("R/monedas.R", envir = globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R", "R/sync_bus.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
assign("avisos_actividad",  function(...) invisible(TRUE), envir = globalenv())
assign("pasivos_log_audit", function(...) invisible(NULL), envir = globalenv())
assign("has_capability", function(user, cap) TRUE, envir = globalenv())
assign("pasivos_autoriza", function(...) TRUE, envir = globalenv())
assign("pasivos_edit_confirm_open", function(...) invisible(NULL), envir = globalenv())
if (!exists(".s3_missing_cache")) .s3_missing_cache <- new.env(parent = emptyenv())
if (!exists(".s3_preload_cache")) .s3_preload_cache <- new.env(parent = emptyenv())

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressMessages(invisible(utils::capture.output(r <- force(expr))));

# ── U: the comparison ─────────────────────────────────────────────────────────
d <- tibble::tibble(id = c("b", "a", "c"), x = c(2, 1, 3), l = list(1, NULL, "z"))
ok(.sync_mismo_contenido(d, d[c(3, 1, 2), ]), "U las mismas filas en otro orden son el mismo contenido")
d2 <- d; d2$x[2] <- 9
ok(!.sync_mismo_contenido(d, d2[c(2, 3, 1), ]), "U un valor distinto es otro contenido, aunque cambie el orden")
ok(!.sync_mismo_contenido(d, d[1:2, ]), "U una fila de menos es otro contenido")
ok(!.sync_mismo_contenido(1, 2) && .sync_mismo_contenido(1, 1), "U sin id (una señal): identical() tal cual")

# ── The data: three regular liabilities, one with frecuencia_pago NA ───────────
CID <- "acme"; hoy <- Sys.Date()
reg <- function(id, monto, dia) {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = paste("Pasivo", id), empresa = "NG", parte = paste("Parte", id),
    moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = dia),
    fecha_inicio = hoy - 60, amount_default = monto), mode = "create", user = "ana")
  l$id <- id; l
}
liabs0 <- dplyr::bind_rows(reg("L1", 1000, 5L), reg("L2", 2000, 10L), reg("L3", 3000, 15L))
provs0 <- dplyr::bind_rows(lapply(seq_len(nrow(liabs0)), function(i) pasivos_generate_provisions(liabs0[i, ], window_start = hoy - 60)))

# A session with the real sync bus; a save is the wizard's real one.
LOG <- new.env()
sembrar <- function() {
  s3_memoria_reset()
  .SYNC_REGISTRY <<- list(); .GlobalEnv$.sync_versions <- list()
  register_synced("pasivos_provisions_db",  S3_KEYS$pasivos_provisions,  function(client_id = NULL) load_pasivos_provisions(client_id = client_id %||% CID))
  register_synced("pasivos_liabilities_db", S3_KEYS$pasivos_liabilities, function(client_id = NULL) load_pasivos_liabilities(client_id = client_id %||% CID))
  quieto(save_pasivos_liabilities(liabs0, client_id = CID))
  quieto(.s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID))
  LOG$inv <- c(L = 0L, P = 0L)
}
sesion_a <- function(cuerpo) testServer(function(input, output, session) {
  L <- reactiveVal(load_pasivos_liabilities(client_id = CID)); P <- reactiveVal(load_pasivos_provisions(client_id = CID))
  sh <- list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
             company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
             effective_client_id = function() CID, current_user = function() "ana",
             suppress_ledger_prov_refresh = function(...) invisible(NULL))
  session$userData$sh <- sh
  observe({ L(); LOG$inv["L"] <- LOG$inv["L"] + 1L })
  observe({ P(); LOG$inv["P"] <- LOG$inv["P"] + 1L })
  setup_sync_bus(session, sh, poll_ms = 8000, active_client_rv = function() CID)
}, { session$flushReact(); session$elapse(8100); session$flushReact(); cuerpo(session) })
guardar_a <- function(session, lid, monto) {
  sh <- session$userData$sh
  d <- pasivos_liability_to_draft(isolate(sh$pasivos_liabilities_db())[isolate(sh$pasivos_liabilities_db())$id == lid, ])
  d$amount_default <- monto
  quieto(isolate(.pasivos_wizard_save(list(mode = "edit", edit_target_id = lid, draft = d), sh, session, "ana")))
  session$flushReact()
}
# B: another session (or process) renames a liability through the same path.
guardar_b <- function(lid, nombre) {
  quieto(modificar_pasivos_liabilities(CID, function(f) { f$nombre[f$id == lid] <- nombre; f }))
  quieto(bump_sync_version("pasivos_liabilities_db", client_id = CID))
}
sondear <- function(session) { session$elapse(8100); session$flushReact() }
copia <- function(session) list(L = isolate(session$userData$sh$pasivos_liabilities_db()),
                                P = isolate(session$userData$sh$pasivos_provisions_db()))

# ── S1 ────────────────────────────────────────────────────────────────────────
sembrar()
sesion_a(function(session) {
  guardar_a(session, "L1", 1500)
  tras <- LOG$inv
  sondear(session)
  ok(identical(LOG$inv, tras), sprintf("S1 el sondeo de la propia sesión no invalida nada (L %d→%d, P %d→%d)",
                                       tras["L"], LOG$inv["L"], tras["P"], LOG$inv["P"]))
  c <- copia(session)
  ok(identical(c$L, load_pasivos_liabilities(client_id = CID)), "S1 la copia de liabilities es identical() a load_pasivos_liabilities()")
  ok(identical(c$P, load_pasivos_provisions(client_id = CID)), "S1 la copia de provisions es identical() a load_pasivos_provisions()")
  ok(identical(c$L$frecuencia_pago[c$L$id == "L1"], "mensual"), "S1 la frecuencia del pasivo editado es la del cargador (\"mensual\"), no NA")
  ok(isTRUE(c$L$amount_default[c$L$id == "L1"] == 1500), "S1 y tiene la edición")
})

# ── S2: B first, then A ───────────────────────────────────────────────────────
sembrar()
sesion_a(function(session) {
  guardar_b("L2", "Renombrado por B")
  guardar_a(session, "L1", 1500)
  sondear(session)
  c <- copia(session)
  ok(identical(c$L$nombre[c$L$id == "L2"], "Renombrado por B") && isTRUE(c$L$amount_default[c$L$id == "L1"] == 1500),
     "S2 B guardó antes: A ve su edición y la de B")
  ok(identical(c$L, load_pasivos_liabilities(client_id = CID)), "S2 y su copia es identical() a load_pasivos_liabilities()")
})

# ── S3: A first, then B ───────────────────────────────────────────────────────
sembrar()
sesion_a(function(session) {
  guardar_a(session, "L1", 1500)
  antes <- LOG$inv["L"]
  guardar_b("L3", "Renombrado por B después")
  sondear(session)
  c <- copia(session)
  ok(LOG$inv["L"] > antes, "S3 el bump de B recarga a A (sí invalida)")
  ok(identical(c$L$nombre[c$L$id == "L3"], "Renombrado por B después") && isTRUE(c$L$amount_default[c$L$id == "L1"] == 1500),
     "S3 A ve su edición y la de B")
  ok(identical(c$L, load_pasivos_liabilities(client_id = CID)), "S3 y su copia es identical() a load_pasivos_liabilities()")
})

# ── S4: a writer that keeps its own row order (the Lista, pasivos_module) ─────
# Its copy has the content on S3 in another order; its own bump must not
# recompute the app (the reload alone would, its order differs).
sembrar()
sesion_a(function(session) {
  sh <- session$userData$sh
  p <- isolate(sh$pasivos_provisions_db()); sh$pasivos_provisions_db(p[rev(seq_len(nrow(p))), ]); session$flushReact()
  quieto(bump_sync_version("pasivos_provisions_db", client_id = CID))
  tras <- LOG$inv
  sondear(session)
  ok(identical(LOG$inv, tras), sprintf("S4 mismo contenido en otro orden: el bump propio no invalida (P %d→%d)", tras["P"], LOG$inv["P"]))
})

# ── M: the cost of the comparison on networks' size ───────────────────────────
grande <- dplyr::bind_rows(replicate(ceiling(1600 / nrow(provs0)), provs0, simplify = FALSE))[1:1600, ]
grande$id <- sprintf("P%05d", sample.int(99999, 1600))
otra <- grande[sample.int(nrow(grande)), ]
t <- system.time(for (i in 1:20) .sync_mismo_contenido(grande, otra))[["elapsed"]] / 20
cat(sprintf("  INFO comparar 1,600 provisiones: %.1f ms\n", t * 1000))
ok(.sync_mismo_contenido(grande, otra), "M 1,600 provisiones barajadas: mismo contenido")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
