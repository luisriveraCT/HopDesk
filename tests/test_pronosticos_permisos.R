# =============================================================================
# tests/test_pronosticos_permisos.R
# T26 · 3: Pronósticos' writers check their permission on the server
# (puede_o_avisa), with the levels Luis chose (as can_manage_policies: Admin
# and Finanzas, plus hopdesk / dev; staff has "*"):
#   can_refresh_forecasting             -> Actualizar (btn_refresh)
#   can_refresh_forecasting_all         -> Actualizar todas (btn_refresh_all)
#   can_edit_forecasting_subscriptions  -> editar (row action) + sub_save,
#                                          + Global + global_sub_save
# Before, the three .FC_CAP_* constants existed and nobody read them: any
# user could refresh the series and change the subscriptions that set the
# rates Pasivos uses for its amounts.
# The real module in testServer, on the in-memory S3 of tests/helpers/s3_memoria.R,
# with each level's real permissions (permisos_resueltos()):
#   N  analysis and compras: told no, nothing is fetched or saved;
#   S  finance (and admin): refresh and edit go through;
#   R  the levels in R/tier_registry.R are the ones Luis chose.
# Run from project root:  Rscript tests/test_pronosticos_permisos.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/forecasting_schemas.R", "R/forecasting_persistence.R",
            "R/forecasting_module.R"))
  suppressMessages(source(f))
.i18n_dir <- file.path("www", "i18n")
log_action <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

# What the module reaches outside itself.
traidas <- 0L
fcs_fetch_and_store <- function(metric_id, ...) { traidas <<- traidas + 1L; 2L }
fcs_flush_caches    <- function() invisible(NULL)
load_forecasting_series_observations <- function(client_id = NULL) .schema_forecasting_series_observations()
load_forecasting_methods <- function() tibble::tibble(method_id = c("spot", "yesterday_spot"),
                                                      label = c("Spot", "Spot de ayer"), kind = "deterministic_rule",
                                                      apply_fn = "f", description = "", params_schema = list(list(), list()),
                                                      active = TRUE)
avisos <- character()
local({
  ns_shiny <- asNamespace("shiny")
  unlockBinding("showNotification", ns_shiny)
  assign("showNotification", function(ui, ...) { avisos <<- c(avisos, paste(as.character(ui), collapse = " ")); invisible(NULL) },
         envir = ns_shiny)
})

CID <- "acme"
KEY <- paste0(CID, "/", S3_KEYS$forecasting_subscriptions)
t0  <- as.POSIXct("2026-11-01 10:00:00")
fila <- function(id, metrica, metodo = "spot")
  tibble::tibble(subscription_id = id, consumer_type = "global_default", consumer_id = NA_character_, metric_id = metrica,
                 method_id = metodo, method_params = list(list()), active = TRUE, created_by = "ana",
                 created_at = t0, updated_at = t0, notes = NA_character_)
SUBS0 <- dplyr::bind_rows(fila("s1", "usd_mxn_fix"), fila("s2", "tiie_28"))
METRICAS <- tibble::tibble(metric_id = c("usd_mxn_fix", "tiie_28", "cetes"), label = c("USD", "TIIE", "CETES"),
                           active = TRUE)
en_s3 <- function() get(KEY, envir = .mem)

# One session of `nivel` presses everything once.
probar <- function(nivel) {
  s3_memoria_reset(); invisible(utils::capture.output(save_forecasting_subscriptions(SUBS0, client_id = CID)))
  traidas <<- 0L; avisos <<- character(); antes <- .mem_escrituras
  usr <- list(user = paste0("u_", nivel), tier = nivel, permisos = permisos_resueltos(nivel))
  shared <- list(
    forecasting_metrics_db             = reactiveVal(METRICAS),
    forecasting_subscriptions_db       = reactiveVal(SUBS0),
    forecasting_series_observations_db = reactiveVal(.schema_forecasting_series_observations()),
    effective_client_id = function() CID, home_client_id = function() CID,
    current_user = function() usr$user, usuario = reactiveVal(usr))
  r <- tryCatch({
    invisible(utils::capture.output(suppressMessages(suppressWarnings(
      testServer(forecastingServer, args = list(shared = shared), {
        session$flushReact()   # observers created first, or ignoreInit eats the first click
        session$setInputs(metric_sel = "usd_mxn_fix", period_sel = "30", btn_refresh = 1)
        session$setInputs(btn_refresh_all = 1)
        session$setInputs(`subs_t-accion` = list(accion = "editar", ids = list("s1"), nonce = 1))
        session$setInputs(sub_method_new = "yesterday_spot", sub_save = 1)
        session$setInputs(btn_add_global_sub = 1)
        session$setInputs(global_sub_metric = "cetes", global_sub_method = "spot", global_sub_save = 1)
      })))))
    "ok"
  }, error = function(e) conditionMessage(e))
  s <- en_s3()
  list(r = r, traidas = traidas, escrituras = .mem_escrituras - antes, s = s, avisos = avisos,
       denegados = sum(grepl(sub("%s.*$", "", tr("permisos.denegado", permiso = "%s")), avisos, fixed = TRUE)))
}

# ── N: no permission ─────────────────────────────────────────────────────────
for (nivel in c("analysis", "compras")) {
  x <- probar(nivel)
  ok(identical(x$r, "ok") && x$traidas == 0L, sprintf("N %s: Actualizar y Actualizar todas no traen nada", nivel))
  ok(x$escrituras == 0L && identical(x$s$method_id, c("spot", "spot")) && nrow(x$s) == 2L,
     sprintf("N %s: editar y + Global no guardan nada", nivel))
  ok(x$denegados >= 5L, sprintf("N %s: se le avisa que no tiene permiso (%d avisos)", nivel, x$denegados))
}

# ── S: with permission ───────────────────────────────────────────────────────
for (nivel in c("finance", "admin")) {
  x <- probar(nivel)
  ok(identical(x$r, "ok") && x$traidas == 1L + nrow(METRICAS),
     sprintf("S %s: Actualizar trae una serie y Actualizar todas las %d", nivel, nrow(METRICAS)))
  ok(x$escrituras == 2L && x$s$method_id[x$s$subscription_id == "s1"] == "yesterday_spot" &&
       nrow(x$s) == 3L && "cetes" %in% x$s$metric_id,
     sprintf("S %s: editar cambia el método y + Global agrega la suya", nivel))
  ok(x$denegados == 0L, sprintf("S %s: sin avisos de permiso", nivel))
}

# ── V: what each level sees ──────────────────────────────────────────────────
# Without the permission the buttons that write are hidden (shinyjs::toggle)
# and the subscriptions' rows carry no pencil (.acciones); a double click
# opens nothing and says nothing.
toggles <- list()
local({
  ns_js <- asNamespace("shinyjs")
  unlockBinding("toggle", ns_js)
  assign("toggle", function(id = NULL, ..., condition = NULL) { toggles[[id]] <<- condition; invisible(NULL) }, envir = ns_js)
})
vista <- function(nivel) {
  toggles <<- list(); avisos <<- character()
  usr <- list(user = paste0("u_", nivel), tier = nivel, permisos = permisos_resueltos(nivel))
  shared <- list(
    forecasting_metrics_db             = reactiveVal(METRICAS),
    forecasting_subscriptions_db       = reactiveVal(SUBS0),
    forecasting_series_observations_db = reactiveVal(.schema_forecasting_series_observations()),
    effective_client_id = function() CID, home_client_id = function() CID,
    current_user = function() usr$user, usuario = reactiveVal(usr))
  acciones <- NULL; editor <- NULL
  invisible(utils::capture.output(suppressMessages(suppressWarnings(
    testServer(forecastingServer, args = list(shared = shared), {
      session$flushReact()
      acciones <<- tryCatch(.subs_tabla()$.acciones, error = function(e) NA)
      session$setInputs(`subs_t-accion` = list(accion = "detalle", ids = list("s1"), nonce = 1))
      editor <<- sub_editando()
    })))))
  list(toggles = toggles, acciones = acciones, editor = editor, avisos = length(avisos))
}
BOTONES <- c("btn_refresh", "btn_refresh_all", "btn_add_global_sub")
for (nivel in c("analysis", "compras")) {
  v <- vista(nivel)
  ok(all(BOTONES %in% names(v$toggles)) && !any(unlist(v$toggles[BOTONES])),
     sprintf("V %s: Actualizar, Actualizar todas y + Global quedan escondidos", nivel))
  ok(identical(unique(v$acciones), ""), sprintf("V %s: las suscripciones no traen lápiz", nivel))
  ok(is.null(v$editor) && v$avisos == 0L, sprintf("V %s: doble clic no abre el editor ni avisa", nivel))
}
v <- vista("finance")
ok(all(BOTONES %in% names(v$toggles)) && all(unlist(v$toggles[BOTONES])), "V finance: ve los 3 botones")
ok(identical(unique(v$acciones), "editar") && identical(v$editor, "s1"),
   "V finance: lápiz en cada suscripción y el doble clic abre el editor")

# ── R: the levels ────────────────────────────────────────────────────────────
NUEVOS <- c("can_refresh_forecasting", "can_refresh_forecasting_all", "can_edit_forecasting_subscriptions")
con <- vapply(c("hopdesk", "dev", "admin", "finance", "analysis", "compras"),
              function(n) { p <- permisos_resueltos(n)
                             all(vapply(NUEVOS, function(k) k %in% names(p) && isTRUE(p[[k]]), logical(1))) }, logical(1))
ok(identical(unname(con), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)),
   "R hopdesk, dev, admin y finance los tienen; analysis y compras no (como can_manage_policies)")
ok(all(vapply(NUEVOS, function(k) identical(PERMISOS[[k]]$grupo, "tesoreria") && !permiso_interno(k), logical(1))),
   "R grupo tesoreria y no internos (un cliente sí los ve)")

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
