# =============================================================================
# tests/test_t72c_sistema.R
# T72-C: the system writes of the Forecasting catalog and Banxico's FIX are
# logged as what they are -- evento_sistema() -- not as "sin_evento". On the
# in-memory S3 (tests/helpers/s3_memoria.R) with the REAL .s3_write(), so each
# event goes through .t72_registrar() as in the app; log_action() keeps what
# it is given. The integrador's conditions (requisiciones/bitacora.md,
# 2026-11-21, T72-C):
#   1  the wrappers without evento= behave as before (a sin_evento entry);
#   2  the entry text has dates or catalog metric names, never a rate or a
#      client's data;
#   4  idempotent, in the 3 places: a second call with nothing new neither
#      writes nor logs (.fx_fix_bajar, forecasting_completar_catalogo,
#      forecasting_seed_if_empty);
#   6  the text a client sees for the seed (printed here, for the LISTO).
# Run from project root:  Rscript tests/test_t72c_sistema.R
# Contraprueba: on 711bf20 every system write is a sin_evento entry, and the
# seed adds its hand-written seed_catalog.
# =============================================================================
cat("=== test_t72c_sistema ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
for (f in c("R/forecasting_schemas.R", "R/forecasting_persistence.R", "R/forecasting_metric_registry.R", "R/fx_banxico.R"))
  suppressMessages(source(f))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressMessages(suppressWarnings(expr))
L <- new.env(); L$x <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$x[[length(L$x) + 1L]] <- list(user = user, module = module, action = action, description = description,
                                  metadata = metadata, client_id = client_id)
  invisible(TRUE)
}
acciones <- function() vapply(L$x, function(e) paste(e$user, e$module, e$action), "")
texto <- function(x) paste(capture.output(str(x, vec.len = 100, nchar.max = 2000)), collapse = "\n")
CID <- "networks"

# ── 1. A wrapper without evento= is as before ──────────────────────────────
s3_memoria_reset(); L$x <- list()
quieto(save_forecasting_metrics(.seed_forecasting_metrics(), client_id = CID))
ok("1 save_forecasting_metrics() without evento=: written, one sistema/registro/sin_evento entry, as before",
   exists(paste0(CID, "/", S3_KEYS$forecasting_metrics), envir = .mem) && identical(acciones(), "sistema registro sin_evento"))

# ── Forecasting: the seed of an empty client ───────────────────────────────
s3_memoria_reset(); L$x <- list()
quieto(forecasting_seed_if_empty(client_id = CID))
a <- acciones()
ok("seed: every write is a sistema:forecasting / sembrar_catalogo event; no sin_evento, no hand-written seed_catalog",
   length(a) >= 2L && all(a == "sistema:forecasting forecasting sembrar_catalogo"))
cat("   texts of the seed (what a client's Actividad shows):\n")
for (e in L$x) cat("     ", e$client_id %||% "?", "|", e$user, "|", e$action, "|", e$description, "\n")
semilla_desc <- vapply(L$x, `[[`, "", "description")
ok("2 the seed's texts name the catalog parts only (no client data)",
   all(grepl("^Catálogo de pronósticos: (métricas|fuentes|métodos|suscripciones) iniciales \\[tabla=", semilla_desc)))
n1 <- .mem_escrituras; L$x <- list()
quieto(forecasting_seed_if_empty(client_id = CID))
ok("4 seed, a second time with nothing new: no write, no entry", .mem_escrituras == n1 && !length(L$x))

# ── Forecasting: a client seeded before PRIME (completar) ──────────────────
s3_memoria_reset(); L$x <- list()
m <- .seed_forecasting_metrics(); m <- m[m$metric_id != "prime", , drop = FALSE]
quieto(save_forecasting_metrics(m, client_id = CID))
quieto(save_forecasting_subscriptions(.seed_forecasting_global_subscriptions(m), client_id = CID))
L$x <- list(); n0 <- .mem_escrituras
nuevas <- quieto(forecasting_completar_catalogo(CID))
a <- acciones()
ok("completar: PRIME added; metrics and subscriptions written as 2 sistema:forecasting events, no sin_evento",
   identical(nuevas, "prime") && identical(a, rep("sistema:forecasting forecasting sembrar_catalogo", 2L)))
# save_forecasting_* write with .s3_write() directly: the entry carries the
# table and its row counts (13 -> 14), not a difference by id (T72-B: a
# direct .s3_write has no table before it in hand).
ok("2 completar: a fixed text with the table and its counts, no client data",
   all(grepl("^Catálogo de pronósticos: se agregaron métricas nuevas de la siembra \\[tabla=forecasting_(metrics|subscriptions)\\.rds filas 13 -> 14\\]$",
             vapply(L$x, `[[`, "", "description"))))
cat("   texts of completar:\n"); for (e in L$x) cat("     ", e$client_id, "|", e$user, "|", e$description, "\n")
n1 <- .mem_escrituras; L$x <- list()
nuevas2 <- quieto(forecasting_completar_catalogo(CID))
ok("4 completar, a second time: nothing added, no write, no entry",
   identical(nuevas2, character()) && .mem_escrituras == n1 && !length(L$x))

# ── Banxico's FIX ──────────────────────────────────────────────────────────
Sys.setenv(BANXICO_TOKEN = "token-de-prueba")
AHORA <- as.POSIXct("2026-11-20 18:00:00", tz = "UTC")
TASA <- c(18.123456, 18.234567)
fcs_fetch_banxico <- function(metric, date_from, date_to)
  list(status = "ok", rows = data.frame(fecha = as.Date(c("2026-11-19", "2026-11-20")), value = TASA))
s3_memoria_reset(); L$x <- list()
r <- quieto(.fx_fix_bajar(AHORA))
ok("FIX: written, ONE sistema:banxico / fx_fix event, no sin_evento",
   is.data.frame(r) && nrow(r) == 2L && identical(acciones(), "sistema:banxico banxico fx_fix"))
fx_txt <- if (length(L$x)) texto(L$x[[1]]) else ""
ok("2 FIX: the entry has the date and never a rate (18.12 / 18.23)",
   grepl("FIX de Banxico hasta el 2026-11-20", fx_txt, fixed = TRUE) && !grepl("18[.,]12|18[.,]23", fx_txt))
cat("   text of the FIX:", if (length(L$x)) L$x[[1]]$description else "-", "\n")
n1 <- .mem_escrituras; L$x <- list()
quieto(.fx_fix_bajar(AHORA))
ok("4 FIX, a second time the same day (no new date): no write, no entry", .mem_escrituras == n1 && !length(L$x))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
