# =============================================================================
# tests/helpers/pronosticos_fixture_app.R
# The Pronósticos tab (R/forecasting_module.R) over fixed methods, metrics,
# subscriptions and fetch log, with a simulated `shared` -- no S3. Saves keep
# the subscriptions in memory (and fail on purpose with PRO_FALLA = 1);
# fcs_fetch_and_store() adds one "ok" row per metric to the in-memory fetch
# log, as a real fetch would. Every save and log_action() call is listed in
# #registro as one line:
#   guardar <subscription_id>:<method_id> | ...     log <action> <target_id>
# #otra_sesion plays "another session": it puts a new subscription FIRST in
# shared$forecasting_subscriptions_db (every position shifts by one), as the
# sync bus would.
#
# The fetch log has 250 rows (more than the old 200 cut), dates on both sides
# of the dd/mm text trap (30/09 and 30/10), one row with no fetch_id and two
# that share one.
#
# PRO_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("PRO_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(shinyjs) })
cargar("R/ui_components.R", "app_styles")
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/forecasting_schemas.R", "R/forecasting_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── What the module reaches outside itself, recorded ────────────────────────
registro <- reactiveVal(character(0))
anotar   <- function(x) registro(c(registro(), x))
falla    <- identical(Sys.getenv("PRO_FALLA"), "1")
almacen  <- new.env()

t0 <- as.POSIXct("2026-10-31 23:00:00")
metodos <- tibble::tibble(
  method_id = c("yesterday_spot", "spot", "manual_curve"),
  label = c("Spot de ayer", "Spot", "Curva manual"),
  kind = c("deterministic_rule", "deterministic_rule", "statistical"),
  apply_fn = c("fc_yesterday", "fc_spot", "fc_manual"),
  description = c("Usa el último valor publicado antes de hoy.",
                  "Usa el valor del día.",
                  paste("Curva capturada a mano por el usuario, valor por valor, para métricas sin fuente pública",
                        "o cuando la fuente no alcanza el horizonte que se necesita en Pasivos.")),
  params_schema = list(list(), list(), list()), active = TRUE)
metricas <- tibble::tibble(
  metric_id = c("usd_mxn_fix", "tiie_28", "inpc"), label = c("USD/MXN FIX", "TIIE 28d", "INPC"),
  category = c("fx", "interest_rate", "inflation"), unit = c("MXN", "%", "pts"), is_legal_metric = FALSE,
  primary_source_id = "banxico", fallback_source_ids = list(list(), list(), list()),
  fetch_params = list(list(), list(), list()), default_method_id = "yesterday_spot", active = TRUE,
  created_at = t0, notes = NA_character_)
sub <- function(id, tipo, consumidor, metrica, metodo, activa = TRUE)
  tibble::tibble(subscription_id = id, consumer_type = tipo, consumer_id = consumidor, metric_id = metrica,
                 method_id = metodo, method_params = list(list()), active = activa, created_by = "luis",
                 created_at = t0, updated_at = t0, notes = NA_character_)
almacen$subs <- dplyr::bind_rows(
  sub("s-usd",  "global_default",    NA,        "usd_mxn_fix", "yesterday_spot"),
  sub("s-tiie", "global_default",    NA,        "tiie_28",     "spot"),
  sub("s-psv",  "pasivos_liability", "PSV-001", "inpc",        "yesterday_spot"),
  sub("s-off",  "global_default",    NA,        "inpc",        "spot", activa = FALSE))

n <- 250L
almacen$log <- tibble::tibble(
  fetch_id     = sprintf("f-%03d", seq_len(n)),
  metric_id    = rep(c("usd_mxn_fix", "tiie_28", "inpc"), length.out = n),
  source_id    = rep(c("banxico", "fred"), length.out = n),
  # f-001 is the newest; one row every 6 h going back.
  attempted_at = t0 - (seq_len(n) - 1L) * 6 * 3600,
  status       = rep(c("ok", "ok", "no_data", "http_error", "timeout"), length.out = n),
  rows_added   = as.integer(rep(c(9L, 10L, 0L, 0L, 0L), length.out = n)),
  duration_ms  = as.integer(seq_len(n) * 7L),
  error_msg    = NA_character_,
  triggered_by = rep(c("scheduled", "manual_ui", "first_use"), length.out = n))
almacen$log$error_msg[almacen$log$status == "http_error"] <-
  "HTTP 503 Service Unavailable desde la fuente; se reintentará en la siguiente corrida programada del día"
almacen$log$fetch_id[5]  <- NA_character_    # a row written before fetch_id existed
almacen$log$fetch_id[7]  <- "f-006"          # two rows that share one

assign("load_forecasting_methods", function() metodos, envir = globalenv())
assign("load_forecasting_metrics", function(client_id = NULL) metricas, envir = globalenv())
assign("load_forecasting_series_observations", function(client_id = NULL) .schema_forecasting_series_observations(),
       envir = globalenv())
assign("load_forecasting_subscriptions", function(client_id = NULL) almacen$subs, envir = globalenv())
assign("save_forecasting_subscriptions", function(df, client_id = NULL) {
  if (falla) stop("S3 no disponible (fixture)")
  almacen$subs <- df
  anotar(paste("guardar", paste(sprintf("%s:%s", df$subscription_id, df$method_id), collapse = " | ")))
  invisible(NULL)
}, envir = globalenv())
# The module's writes go through it (T26): the change on the subscriptions as
# they are now, saved only if something changed. PRO_FALLA = 1 fails it.
assign("modificar_forecasting_subscriptions", function(client_id, cambio) {
  if (falla) stop("S3 no disponible (fixture)")
  nuevo <- cambio(almacen$subs)
  if (identical(nuevo, almacen$subs)) return(invisible(nuevo))
  save_forecasting_subscriptions(nuevo, client_id = client_id)
  invisible(nuevo)
}, envir = globalenv())
assign("load_forecasting_fetch_log", function() almacen$log, envir = globalenv())
assign("fcs_flush_caches", function() invisible(NULL), envir = globalenv())
assign("fcs_fetch_and_store", function(metric_id, ...) {
  almacen$log <- dplyr::bind_rows(tibble::tibble(
    fetch_id = paste0("nuevo-", metric_id, "-", length(registro())), metric_id = metric_id, source_id = "banxico",
    attempted_at = Sys.time(), status = "ok", rows_added = 3L, duration_ms = 42L, error_msg = NA_character_,
    triggered_by = "manual_ui"), almacen$log)
  3L
}, envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, ...)
  anotar(sprintf("log %s %s", action, target_id %||% "")), envir = globalenv())

# HD_ARRANQUE_NULL = 1: the sources start NULL, as before S3 loads at
# startup (tests/test_hd_table_arranque_null.R; incident 2026-11-03).
if (identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")) { metricas <- NULL; almacen$subs <- NULL; almacen$log <- NULL }

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  div(class = "d-flex gap-2 p-2", actionButton("otra_sesion", "Otra sesión: agrega una suscripción al principio")),
  forecastingUI("fc"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:200px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  subs_db <- reactiveVal(almacen$subs)
  shared <- list(
    forecasting_metrics_db             = reactiveVal(metricas),
    forecasting_subscriptions_db       = subs_db,
    forecasting_series_observations_db = reactiveVal(.schema_forecasting_series_observations()),
    effective_client_id = function() "fixture",
    home_client_id      = function() "fixture",
    current_user        = function() "luis"
  )
  forecastingServer("fc", shared)
  observeEvent(input$otra_sesion, {
    almacen$subs <- dplyr::bind_rows(sub("s-new", "global_default", NA, "inpc", "spot"), almacen$subs)
    subs_db(almacen$subs)
  })
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
