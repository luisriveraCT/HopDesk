# =============================================================================
# tests/test_cm38_salto_cliente.R
# CM38: a staff member jumps from client A to client B and a read fails. The
# session never keeps client A's value under client B:
#   - the reactive gets an empty copy (the table with no rows, its columns
#     kept; group_config its empty schema), never A's rows;
#   - its label goes to ctx_no_cargadas(); one notice for the whole jump and
#     a log line with the labels;
#   - the loaders that turn a failed read into an empty table (empresas,
#     group config, policy catalog, alias map) are read strictly first;
#   - a SAP snapshot that exists but cannot be read is marked too;
#   - the saves that still write the session's copy (alias map, policy
#     catalog) refuse while their table is marked.
#
# The jump observer is taken from app.R as it is and run with its I/O stubbed.
# Run from project root:  Rscript tests/test_cm38_salto_cliente.R
# Counter-proof: HD_APP_R=<master's app.R> fails the jump checks (A's rows stay).
# =============================================================================
cat("=== test_cm38_salto_cliente ===\n\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── The jump observer, from app.R ────────────────────────────────────────────
app <- Sys.getenv("HD_APP_R", "app.R")
manejador <- NULL
buscar <- function(e) {
  if (!is.null(manejador) || !is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("observeEvent")) && length(e) >= 3 &&
      identical(e[[2]], quote(jump_client_id())) && ".ctx_load" %in% all.names(e[[3]])) {
    manejador <<- e[[3]]; return(invisible())
  }
  for (i in seq_along(e)[-1])   # an empty argument (x[, 1]) is skipped, never touched
    if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) buscar(e[[i]])
}
for (e in parse(app, keep.source = FALSE, encoding = "UTF-8")) buscar(e)
ok(!is.null(manejador), "the context-switch observer (with .ctx_load) found in app.R")

# A reactiveVal stand-in: rv() reads, rv(x) writes (NULL included).
rv_falso <- function(inicial = NULL) {
  valor <- inicial
  function(x) { if (missing(x)) return(valor); valor <<- x; invisible(x) }
}
tabla <- function(cliente) data.frame(cliente = cliente, v = 1, stringsAsFactors = FALSE)

REACTIVOS <- c("bancos_confirmados_db", "ctas_cuentas", "bancos_movimientos_db", "proveedores_db",
               "empresas_db", "grupos_db", "pasivos_provisions_db", "pasivos_liabilities_db", "papelera_rv",
               "notes_df", "tags_db", "moves_db", "policy_catalog_db", "forecasting_series_observations_db",
               "forecasting_metrics_db", "forecasting_subscriptions_db", "forecasting_manual_curves_db",
               "conciliacion_rv", "parte_alias_map_db", "manual_inv", "abonos_db", "sap_ov_db",
               "bancos_cuentas_db", "partner_policies_db", "holiday_overrides_db", "policy_moves_db",
               "proveedores_inactivos_db")
CARGADORES <- c("load_bancos_confirmados", "load_ctas_cuentas", "load_bancos_movimientos", "load_proveedores",
                "load_empresas", "load_grupos", "load_pasivos_provisions", "load_pasivos_liabilities",
                "load_papelera", "load_notes", "load_tags", "load_moves", "load_policy_catalog",
                "load_forecasting_series_observations", "load_forecasting_metrics",
                "load_forecasting_subscriptions", "load_forecasting_manual_curves", "load_conciliacion",
                "load_parte_alias_map", "load_manual", "load_abonos", "load_sap_overrides",
                "load_bancos_cuentas", "load_partner_policies", "load_holiday_overrides",
                "load_policy_moves", "load_proveedores_inactivos")

# One jump from A to B. falla_carga: loaders that stop(); falla_estricta: S3
# keys whose strict read stop()s (their loader would swallow it); falla_sap:
# ledgers whose snapshot exists but cannot be read.
saltar <- function(falla_carga = character(), falla_estricta = character(), falla_sap = character()) {
  ent <- new.env(parent = globalenv())
  for (r in REACTIVOS) assign(r, rv_falso(tabla("A")), envir = ent)
  ent$group_config_rv <- rv_falso(list(group_name = "Grupo A"))
  ent$pagar_hoy_db <- rv_falso(tabla("A")); ent$interco_v2 <- rv_falso(list(cliente = "A"))
  ent$sap_data <- rv_falso(list(AR = tabla("A"), AP = tabla("A")))
  ent$sap_snapshot_info <- rv_falso(NULL)
  ent$ctx_no_cargadas <- rv_falso(c("de_un_salto_anterior"))
  ent$tema_cargado <- rv_falso(0L)   # TEMA 5a: the jump says the new client's theme is known
  for (f in CARGADORES) local({
    nombre <- f
    assign(nombre, function(client_id = NULL, ...) {
      if (nombre %in% falla_carga) stop("S3 caído")
      tabla(client_id)
    }, envir = ent)
  })
  ent$load_group_config <- function(client_id = NULL) list(group_name = paste("Grupo", client_id))
  ent$load_interco_v2 <- function(client_id = NULL) list(cliente = client_id)
  ent$.schema_group_config <- function() list(group_name = "")
  ent$.schema_pagar_hoy <- function() data.frame()
  ent$safe_load_pagar_hoy <- function(username, client_id) tabla(client_id)
  ent$S3_KEYS <- list(empresas = "empresas.rds", policy_catalog = "policy_catalog.rds",
                      parte_alias_map = "parte_alias_map.rds", group_config = "group_config.rds")
  ent$.s3_leer_para_escribir <- function(key, client_id) {
    if (key %in% falla_estricta) stop("could not read ", key)
    NULL
  }
  ent$.load_sap_snapshot_for_client <- function(ledger, client_id) {
    if (ledger %in% falla_sap) stop("could not read the snapshot")
    list(data = tabla(client_id), saved_at = Sys.time())
  }
  ent$.sap_cache_set <- function(...) invisible(NULL)
  ent$isolate <- function(x) x
  ent$jump_client_id <- function() "B"; ent$effective_client_id <- function() "B"
  ent$home_client_id <- function() "hd-admin"; ent$current_user <- function() "staff"
  avisos <- list(); registro <- list()
  ent$showNotification <- function(texto, ...) { avisos[[length(avisos) + 1L]] <<- list(texto = texto, ...); invisible() }
  ent$tr <- function(clave, ...) clave
  ent$tr_n <- function(clave, n, ...) paste0(clave, "|", n)
  ent$log_action <- function(...) { registro[[length(registro) + 1L]] <<- list(...); invisible() }
  # T87: the jump's entry/exit (tests/test_t87_catalogo.R covers it) and the
  # state it keeps: the client left, and a forced exit's reason.
  ent$t87_registrar_salto <- function(...) invisible(TRUE)
  ent$.salto_anterior <- "A"
  if (!exists("session", envir = ent, inherits = FALSE)) ent$session <- list(userData = new.env())
  error <- tryCatch({ suppressMessages(eval(manejador, ent)); NULL }, error = function(e) conditionMessage(e))
  list(ent = ent, avisos = avisos, registro = registro, error = error)
}

if (!is.null(manejador)) {
  # ── 1. A jump with no failures ────────────────────────────────────────────
  s <- saltar()
  ok(is.null(s$error), paste("a clean jump runs:", s$error %||% "ok"))
  ok(all(vapply(REACTIVOS, function(r) identical(get(r, s$ent)()$cliente, "B"), logical(1))),
     "clean jump: every table is client B's")
  ok(identical(s$ent$ctx_no_cargadas(), character()), "clean jump: nothing marked (the previous jump's mark is cleared)")
  ok(!length(s$avisos), "clean jump: no notice")

  # ── 2. Reads that fail: never A's value ───────────────────────────────────
  s <- saltar(falla_carga = c("load_moves", "load_manual"),
              falla_estricta = c("empresas.rds", "group_config.rds", "policy_catalog.rds", "parte_alias_map.rds"))
  ok(is.null(s$error), paste("a jump with failed reads runs:", s$error %||% "ok"))
  vacias <- c("moves_db", "manual_inv", "empresas_db", "policy_catalog_db", "parte_alias_map_db")
  ok(all(vapply(vacias, function(r) { x <- get(r, s$ent)(); is.data.frame(x) && !nrow(x) }, logical(1))),
     "failed reads: the table is empty (no row of client A's)")
  ok(identical(names(s$ent$moves_db()), names(tabla("A"))),
     "failed reads: the empty table keeps its columns (readers still work)")
  ok(identical(s$ent$group_config_rv(), list(group_name = "")),
     "failed group config: its empty schema, never client A's name and logo")
  ok(identical(s$ent$proveedores_db()$cliente, "B"), "the tables that were read are client B's")
  ok(setequal(s$ent$ctx_no_cargadas(),
              c("moves", "manual_inv", "empresas", "group_config", "policy_catalog", "parte_alias_map")),
     "every failed table is marked in ctx_no_cargadas, and only those")
  errores <- Filter(function(a) identical(a$type, "error"), s$avisos)
  ok(length(errores) == 1L && identical(errores[[1]]$texto, "comun.ctx.no_cargado|6"),
     "one notice for the whole jump, with how many tables")
  lf <- Filter(function(r) identical(r$action, "ctx_load_fallido"), s$registro)
  ok(length(lf) == 1L && identical(lf[[1]]$client_id, "B") && grepl("moves", lf[[1]]$description),
     "one log line for client B with the labels")

  # ── 3. A SAP snapshot that exists but cannot be read ──────────────────────
  s <- saltar(falla_sap = "AR")
  ok(is.null(s$error) && is.null(s$ent$sap_data()$AR) && identical(s$ent$sap_data()$AP$cliente, "B"),
     "SAP: the unreadable snapshot is NULL (never A's); the other one is B's")
  ok(identical(s$ent$ctx_no_cargadas(), "sap_ar"), "SAP: marked as sap_ar")
}

# ── 4. The session-copy saves refuse while their table is marked ─────────────
bloque <- function(archivo, desde, hasta) {
  src <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  i <- grep(desde, src, fixed = TRUE)[1]
  if (is.na(i)) return(character())
  j <- grep(hasta, src[i:length(src)], fixed = TRUE)[1] + i - 1L
  if (is.na(i) || is.na(j)) character() else src[i:j]
}
guarda <- function(lineas, etiqueta) {
  g <- grep(sprintf('"%s" %%in%% tryCatch(isolate(shared$ctx_no_cargadas())', etiqueta), lineas, fixed = TRUE)[1]
  s <- grep("modificar_", lineas, fixed = TRUE)[1]   # CM34c: the write is a modificar_*() call
  !is.na(g) && !is.na(s) && g < s && any(grepl("comun.ctx.guardar_bloqueado", lineas[g:(g + 2L)], fixed = TRUE))
}
ok(guarda(bloque("R/pagar_hoy_module.R", "observeEvent(input$ph_link_supplier", "modificar_parte_alias_map("), "parte_alias_map"),
   "alias link: refuses before saving when the alias map was not read")
ok(guarda(bloque("R/pagar_hoy_module.R", "observeEvent(input$ph_unlink_supplier", "modificar_parte_alias_map("), "parte_alias_map"),
   "alias unlink: refuses before saving when the alias map was not read")
# T120: the save is politica_guardar() (Configuración and the Pasivos wizard).
ok(guarda(bloque("R/settings/settings_policies.R", "politica_guardar <- function(", "modificar_policy_catalog("), "policy_catalog"),
   "policy save: refuses before saving when the catalog was not read")
ok(guarda(bloque("R/settings/settings_policies.R", "observeEvent(input$pol_delete_confirm", "modificar_policy_catalog("), "policy_catalog"),
   "policy delete: refuses before saving when the catalog was not read")

# The guard itself: with no ctx_no_cargadas in `shared` (old fixtures) it lets
# the save through; with the label marked it stops.
cond <- function(shared, etiqueta) etiqueta %in% tryCatch(isolate(shared$ctx_no_cargadas()), error = function(e) character())
isolate <- function(x) x
ok(!cond(list(), "policy_catalog"), "guard: a `shared` without ctx_no_cargadas never blocks")
ok(cond(list(ctx_no_cargadas = function() "policy_catalog"), "policy_catalog"), "guard: a marked table blocks")

# ── 5. The texts, in both languages ──────────────────────────────────────────
for (idioma in c("es", "en")) {
  d <- jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))
  ok(all(c("comun.ctx.no_cargado.uno", "comun.ctx.no_cargado.otros", "comun.ctx.guardar_bloqueado") %in% names(d)) &&
       grepl("{n}", d[["comun.ctx.no_cargado.otros"]], fixed = TRUE),
     sprintf("i18n %s: the CM38 notices exist", idioma))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
