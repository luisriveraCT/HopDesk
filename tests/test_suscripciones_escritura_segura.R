# =============================================================================
# tests/test_suscripciones_escritura_segura.R
# T26 · 2: the forecasting subscriptions are written on S3's table as it is
# now, through modificar_forecasting_subscriptions(). Their three writers --
# editing one (sub_save), "+ Global" (global_sub_save) and the start-up seed
# (forecasting_seed_if_empty) -- read with load_forecasting_subscriptions(),
# which turns a failed read into an empty table: "+ Global" then saved only
# the new one over all the others, and the seed took the failure for "no
# subscriptions" and wrote the defaults over the client's.
# On the in-memory S3 of tests/helpers/s3_memoria.R.
#   A  modificar_forecasting_subscriptions(): a failed read writes nothing and
#      the error reaches the caller; a good read keeps what another session
#      added; no change = no write; no client = nothing read or written;
#   B  the seed: a failed read writes nothing; a really empty table is seeded;
#      a table with rows is left alone;
#   C  the module's "+ Global" (testServer): a failed read writes nothing and
#      the user is told; a good read adds it to what is on S3 now;
#   D  the module's edit (sub_save) goes through the strict write.
# Run from project root:  Rscript tests/test_suscripciones_escritura_segura.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/forecasting_schemas.R", "R/forecasting_persistence.R", "R/forecasting_metric_registry.R",
            "R/forecasting_module.R"))
  if (file.exists(f)) suppressMessages(source(f))
.i18n_dir <- file.path("www", "i18n")
log_action <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(suppressWarnings(expr))))
intento <- function(expr) tryCatch({ callado(expr); NULL }, error = function(e) conditionMessage(e))

CID <- "acme"
KEY <- paste0(CID, "/", S3_KEYS$forecasting_subscriptions)
t0  <- as.POSIXct("2026-11-01 10:00:00")
sub <- function(id, metrica, metodo = "spot", tipo = "global_default")
  tibble::tibble(subscription_id = id, consumer_type = tipo, consumer_id = NA_character_, metric_id = metrica,
                 method_id = metodo, method_params = list(list()), active = TRUE, created_by = "ana",
                 created_at = t0, updated_at = t0, notes = NA_character_)
SUBS0 <- dplyr::bind_rows(sub("s1", "usd_mxn_fix"), sub("s2", "tiie_28"), sub("s3", "inpc", tipo = "pasivos_liability"))
sembrar <- function() { s3_memoria_reset(); callado(save_forecasting_subscriptions(SUBS0, client_id = CID)) }
en_s3   <- function() if (exists(KEY, envir = .mem)) get(KEY, envir = .mem) else NULL
ids_s3  <- function() sort(as.character(en_s3()$subscription_id))
falla_lectura <- function() .mem_falla <<- KEY
existe_mod <- exists("modificar_forecasting_subscriptions", mode = "function")

# ── A: the strict write ──────────────────────────────────────────────────────
if (!existe_mod) {
  ok(FALSE, "A modificar_forecasting_subscriptions() existe")
} else {
  sembrar(); antes <- .mem_escrituras; falla_lectura()
  err <- intento(modificar_forecasting_subscriptions(CID, function(s) dplyr::bind_rows(s, sub("s9", "cetes"))))
  .mem_falla <- character()
  ok(!is.null(err) && .mem_escrituras == antes && identical(ids_s3(), c("s1", "s2", "s3")),
     "A1 lectura fallida: no escribe, las 3 suscripciones siguen y el error llega al llamador")

  sembrar()
  assign(KEY, dplyr::bind_rows(en_s3(), .fcp_pack_list_col(sub("s4", "sofr"), "method_params")), envir = .mem)
  callado(modificar_forecasting_subscriptions(CID, function(s) dplyr::bind_rows(s, sub("s9", "cetes"))))
  ok(identical(ids_s3(), c("s1", "s2", "s3", "s4", "s9")),
     "A2 lectura buena: agrega sobre lo fresco y conserva la que otra sesión agregó")

  sembrar(); antes <- .mem_escrituras
  callado(modificar_forecasting_subscriptions(CID, function(s) s))
  ok(.mem_escrituras == antes, "A3 sin cambio: no escribe")

  sembrar(); antes <- .mem_escrituras
  err <- intento(modificar_forecasting_subscriptions(NULL, function(s) dplyr::bind_rows(s, sub("s9", "cetes"))))
  ok(!is.null(err) && .mem_escrituras == antes, "A4 sin client_id: no lee ni escribe (nunca la llave por defecto)")

  s3_memoria_reset()
  callado(modificar_forecasting_subscriptions(CID, function(s) dplyr::bind_rows(s, sub("s9", "cetes"))))
  ok(identical(ids_s3(), "s9"), "A5 sin archivo todavía (cliente nuevo): parte de la tabla vacía y guarda")
}

# ── B: the start-up seed ─────────────────────────────────────────────────────
METRICAS <- tibble::tibble(metric_id = c("usd_mxn_fix", "tiie_28"), default_method_id = "yesterday_spot")
load_forecasting_metrics <- function(client_id = NULL) METRICAS
load_forecasting_sources <- function() tibble::tibble(x = 1)
load_forecasting_methods <- function() tibble::tibble(x = 1)

sembrar(); falla_lectura()
err <- intento(forecasting_seed_if_empty(client_id = CID))
.mem_falla <- character()
ok(identical(ids_s3(), c("s1", "s2", "s3")),
   "B1 siembra con la lectura fallida: no escribe los valores por defecto sobre las del cliente")
ok(!is.null(err), "B2 siembra con la lectura fallida: el error sube (el arranque lo registra como warning)")

s3_memoria_reset()
callado(forecasting_seed_if_empty(client_id = CID))
s <- en_s3()
ok(!is.null(s) && nrow(s) == 2L && all(s$consumer_type == "global_default"),
   "B3 tabla de verdad vacía: siembra las globales por defecto")

sembrar(); antes <- .mem_escrituras
callado(forecasting_seed_if_empty(client_id = CID))
ok(.mem_escrituras == antes && identical(ids_s3(), c("s1", "s2", "s3")), "B4 tabla con filas: no la toca")

# ── C: the module's "+ Global", in testServer ────────────────────────────────
avisos <- character()
showNotification <- function(ui, ...) { avisos <<- c(avisos, as.character(ui)); invisible(NULL) }
assign("showNotification", showNotification, envir = globalenv())
local({
  ns_shiny <- asNamespace("shiny")
  unlockBinding("showNotification", ns_shiny)
  assign("showNotification", function(ui, ...) { avisos <<- c(avisos, as.character(ui)); invisible(NULL) }, envir = ns_shiny)
})
hacer_shared <- function() list(
  forecasting_metrics_db             = reactiveVal(tibble::tibble(metric_id = c("usd_mxn_fix", "tiie_28", "cetes"),
                                                                  label = c("USD", "TIIE", "CETES"), active = TRUE)),
  forecasting_subscriptions_db       = reactiveVal(SUBS0),
  forecasting_series_observations_db = reactiveVal(.schema_forecasting_series_observations()),
  effective_client_id = function() CID,
  home_client_id      = function() CID,
  current_user        = function() "ana",
  # A Finanzas user, with that level's real permissions (T26 · 3).
  usuario             = reactiveVal(list(user = "ana", tier = "finance", permisos = permisos_resueltos("finance"))))
crear_global <- function(fallar) {
  sembrar(); if (fallar) falla_lectura()
  antes <- .mem_escrituras; avisos <<- character()
  r <- tryCatch({
    callado(testServer(forecastingServer, args = list(shared = hacer_shared()), {
      session$flushReact()   # observers created first, or ignoreInit eats the click
      session$setInputs(global_sub_metric = "cetes", global_sub_method = "spot", global_sub_save = 1)
    }))
    "ok"
  }, error = function(e) conditionMessage(e))
  .mem_falla <<- character()
  list(r = r, escrituras = .mem_escrituras - antes, avisos = avisos)
}
x <- crear_global(fallar = TRUE)
ok(identical(x$r, "ok") && x$escrituras == 0L && identical(ids_s3(), c("s1", "s2", "s3")),
   sprintf("C1 + Global con la lectura fallida: no escribe y las 3 siguen (antes guardaba solo la nueva) [%s]", x$r))
ok(any(grepl(tr("comun.error_guardar"), x$avisos, fixed = TRUE)), "C2 + Global con la lectura fallida: avisa al usuario")
x <- crear_global(fallar = FALSE)
s <- en_s3()
ok(identical(x$r, "ok") && x$escrituras == 1L && nrow(s) == 4L && "cetes" %in% s$metric_id &&
     all(c("s1", "s2", "s3") %in% s$subscription_id),
   "C3 + Global con lectura buena: agrega la nueva y conserva las 3")

# ── D: the edit goes through the strict write ───────────────────────────────
mod <- paste(readLines("R/forecasting_module.R", encoding = "UTF-8"), collapse = "\n")
guardar <- regmatches(mod, regexpr("observeEvent\\(input\\$sub_save,[\\s\\S]*?\\n    \\}, ignoreInit", mod, perl = TRUE))
ok(length(guardar) == 1L && grepl(".guardar_subs(function(subs)", guardar, fixed = TRUE) &&
     !grepl("load_forecasting_subscriptions", guardar, fixed = TRUE),
   "D sub_save cambia por id sobre lo fresco (sin leer con load_forecasting_subscriptions)")

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
