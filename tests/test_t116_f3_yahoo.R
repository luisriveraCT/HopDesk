# =============================================================================
# tests/test_t116_f3_yahoo.R
# T116 F3: Yahoo Finance retired (R/forecasting_metric_registry.R). In-memory S3.
#   1  the seed: yahoo inactive; ecb and nyfed active with their adapters; no
#      metric reads yahoo; every metric's sources have their keys
#   2  the deployment's sources table: the new sources added, yahoo made
#      inactive (the row stays); idempotent; an unchanged table not written;
#      a failed read writes nothing
#   3  each client's catalog: only the metrics still as the old seed left
#      them move; an edited one is not touched, and Forecasting lists it as
#      without an active source; idempotent; a failed read writes nothing
#   4  forecasting_seed_if_empty() does both
#   5  the guardian: no adapter, no quantmod, no source() of Yahoo left
#
# Run from project root:  Rscript tests/test_t116_f3_yahoo.R
# =============================================================================
cat("=== test_t116_f3_yahoo ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
for (f in c("R/forecasting_schemas.R", "R/forecasting_persistence.R", "R/forecasting_metric_registry.R",
            "R/forecasting_sources.R", "R/forecasting_source_banxico.R", "R/forecasting_source_fred.R",
            "R/forecasting_source_ecb.R", "R/forecasting_source_nyfed.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
q <- function(expr) suppressWarnings(suppressMessages(expr))
CID <- "cli-t116"
uuid_ok <- requireNamespace("uuid", quietly = TRUE)

# The tables as a deployment seeded before T116 F3 left them.
fuentes_viejas <- function() {
  s <- .seed_forecasting_sources()
  s <- s[!s$source_id %in% c("ecb", "nyfed"), ]
  s$active[s$source_id == "yahoo"] <- TRUE; s$notes[s$source_id == "yahoo"] <- NA_character_
  s
}
TICKERS <- c(eur_usd = "EURUSD=X", eur_mxn = "EURMXN=X", gbp_usd = "GBPUSD=X",
             gbp_mxn = "GBPMXN=X", jpy_mxn = "JPYMXN=X", cad_mxn = "CADMXN=X")
metricas_viejas <- function() {
  m <- .seed_forecasting_metrics()
  m <- m[!m$metric_id %in% c("fed_funds", "estr", "cetes28"), ]
  for (id in names(TICKERS)) {
    i <- which(m$metric_id == id)
    m$primary_source_id[i] <- "yahoo"; m$fallback_source_ids[i] <- list(list())
    m$fetch_params[i] <- list(list(ticker = TICKERS[[id]]))
  }
  m
}
fuentes_s3  <- function() q(load_forecasting_sources())
metricas_s3 <- function() q(load_forecasting_metrics(client_id = CID))
ticker_de <- function(m, id) { fp <- m$fetch_params[[which(m$metric_id == id)]]; if (is.list(fp[[1]])) fp <- fp[[1]]; fp$ticker %||% NA }
fp_de <- function(m, id) { fp <- m$fetch_params[[which(m$metric_id == id)]]; if (length(fp) && is.list(fp[[1]])) fp[[1]] else fp }

cat("== 1. La semilla ==\n")
s <- .seed_forecasting_sources()
ok("1.1 yahoo inactiva, con su nota; ecb y nyfed activas",
   identical(s$active[s$source_id == "yahoo"], FALSE) && identical(s$notes[s$source_id == "yahoo"], FCS_YAHOO_RETIRADA) &&
     all(s$active[s$source_id %in% c("ecb", "nyfed")]))
ok("1.2 cada fuente activa tiene su adaptador (menos la entrada manual)",
   all(vapply(s$adapter_fn[s$active & s$source_id != "manual"], exists, logical(1), mode = "function")))
m <- .seed_forecasting_metrics()
ok("1.3 ninguna métrica lee yahoo, ni de respaldo",
   !any(m$primary_source_id == "yahoo") && !any(unlist(m$fallback_source_ids) == "yahoo"))
llaves_de <- list(banxico = "serie", fred = "serie", ecb = c("flujo", "llave"), nyfed = c("tipo", "tasa"))
faltan <- unlist(lapply(seq_len(nrow(m)), function(i) {
  fp <- m$fetch_params[[i]]
  for (src in c(m$primary_source_id[i], unlist(m$fallback_source_ids[[i]])))
    if (!all(llaves_de[[src]] %in% names(fp))) return(paste(m$metric_id[i], src))
  NULL
}))
ok("1.4 fetch_params trae las llaves de cada fuente de la métrica (primaria y respaldos)", !length(faltan), paste(faltan, collapse = ", "))
ok("1.5 las nuevas: fed_funds (nyfed, respaldo fred), estr (ecb), cetes28 (banxico SF43936, semanal: spot)",
   identical(m$primary_source_id[m$metric_id == "fed_funds"], "nyfed") &&
     identical(unlist(m$fallback_source_ids[m$metric_id == "fed_funds"]), "fred") &&
     identical(m$primary_source_id[m$metric_id == "estr"], "ecb") &&
     identical(fp_de(m, "cetes28")$serie, "SF43936") && identical(m$default_method_id[m$metric_id == "cetes28"], "spot"))
ok("1.6 SOFR: FRED y de respaldo la Fed de NY", identical(unlist(m$fallback_source_ids[m$metric_id == "sofr"]), "nyfed"))

cat("== 2. La tabla de fuentes del despliegue ==\n")
s3_memoria_reset()
q(save_forecasting_sources(fuentes_viejas()))
n0 <- .mem_escrituras
cambio <- q(forecasting_completar_fuentes())
f <- fuentes_s3()
ok("2.1 agrega ecb y nyfed y retira yahoo", setequal(cambio, c("ecb", "nyfed", "yahoo")) &&
     all(c("ecb", "nyfed") %in% f$source_id) && identical(f$active[f$source_id == "yahoo"], FALSE) &&
     identical(f$notes[f$source_id == "yahoo"], FCS_YAHOO_RETIRADA), paste(cambio, collapse = ","))
ok("2.2 la fila de yahoo se queda (las observaciones viejas la nombran)", sum(f$source_id == "yahoo") == 1L)
ok("2.3 escribió una vez", .mem_escrituras - n0 == 1L)
n1 <- .mem_escrituras
ok("2.4 otra vez: nada cambia, nada se escribe", length(q(forecasting_completar_fuentes())) == 0L && .mem_escrituras == n1)
s3_memoria_reset()
ok("2.5 tabla vacía (se siembra entera en otro lado): no escribe", { n <- .mem_escrituras; length(q(forecasting_completar_fuentes())) == 0L && .mem_escrituras == n })
s3_memoria_reset()
q(save_forecasting_sources(fuentes_viejas()))
llave_f <- .s3_key(S3_KEYS$forecasting_sources)
assign(".mem_prohibido", c(get0(".mem_prohibido", ifnotfound = character()), llave_f), envir = globalenv())
n2 <- .mem_escrituras
ok("2.6 lectura fallida: error y nada escrito", inherits(tryCatch(q(forecasting_completar_fuentes()), error = function(e) e), "error") &&
     .mem_escrituras == n2)
assign(".mem_prohibido", setdiff(.mem_prohibido, llave_f), envir = globalenv())

cat("== 3. El catálogo de cada cliente ==\n")
s3_memoria_reset()
viejas <- metricas_viejas()
viejas$fetch_params[viejas$metric_id == "gbp_mxn"] <- list(list(ticker = "GBPMXN=X", nota = "editada"))
q(save_forecasting_metrics(viejas, client_id = CID))
n0 <- .mem_escrituras
movidas <- q(forecasting_migrar_yahoo(CID))
m2 <- metricas_s3()
ok("3.1 se mueven las 5 que siguen como las dejó la semilla; gbp_mxn (editada) no",
   setequal(movidas, setdiff(names(TICKERS), "gbp_mxn")), paste(movidas, collapse = ","))
ok("3.2 eur_mxn → banxico SF46410; eur_usd → ecb; gbp_usd → ecb cruzado",
   identical(m2$primary_source_id[m2$metric_id == "eur_mxn"], "banxico") && identical(fp_de(m2, "eur_mxn")$serie, "SF46410") &&
     identical(m2$primary_source_id[m2$metric_id == "eur_usd"], "ecb") &&
     identical(fp_de(m2, "gbp_usd")$divide, "D.GBP.EUR.SP00.A"))
ok("3.3 la editada queda igual: yahoo y su ticker", identical(m2$primary_source_id[m2$metric_id == "gbp_mxn"], "yahoo") &&
     identical(fp_de(m2, "gbp_mxn")$nota, "editada"))
ok("3.4 las demás métricas no cambian (usd_mxn, tiie28, sofr)",
   all(vapply(c("usd_mxn", "tiie28", "sofr"), function(id) identical(fp_de(m2, id), fp_de(viejas, id)), logical(1))))
ok("3.5 escribió una vez", .mem_escrituras - n0 == 1L)
n1 <- .mem_escrituras
ok("3.6 otra vez: nada se mueve ni se escribe", !length(q(forecasting_migrar_yahoo(CID))) && .mem_escrituras == n1)
sin <- forecasting_metricas_sin_fuente(m2, .seed_forecasting_sources())
ok("3.7 Forecasting la lista como sin fuente activa (solo a ella)", identical(sin$metric_id, "gbp_mxn") && nzchar(sin$label))
ok("3.8 sin_fuente: una métrica inactiva no cuenta; una con respaldo activo tampoco",
   { m3 <- m2; m3$active[m3$metric_id == "gbp_mxn"] <- FALSE; !nrow(forecasting_metricas_sin_fuente(m3, .seed_forecasting_sources())) })
s3_memoria_reset()
q(save_forecasting_metrics(metricas_viejas(), client_id = CID))
llave_m <- .s3_key(S3_KEYS$forecasting_metrics, client_id = CID)
assign(".mem_prohibido", c(get0(".mem_prohibido", ifnotfound = character()), llave_m), envir = globalenv())
n2 <- .mem_escrituras
ok("3.9 lectura fallida: error y nada escrito", inherits(tryCatch(q(forecasting_migrar_yahoo(CID)), error = function(e) e), "error") &&
     .mem_escrituras == n2)
assign(".mem_prohibido", setdiff(.mem_prohibido, llave_m), envir = globalenv())
fc <- paste(readLines("R/forecasting_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("3.10 Forecasting lo dice: output$metrica_sin_fuente con tablas.pronosticos.sin_fuente",
   grepl("output$metrica_sin_fuente", fc, fixed = TRUE) && grepl("forecasting_metricas_sin_fuente(", fc, fixed = TRUE) &&
     grepl('"tablas.pronosticos.sin_fuente"', fc, fixed = TRUE))

cat("== 4. La siembra hace las dos ==\n")
if (uuid_ok) {
  s3_memoria_reset()
  q(save_forecasting_sources(fuentes_viejas()))
  q(save_forecasting_metrics(metricas_viejas(), client_id = CID))
  q(save_forecasting_methods(.seed_forecasting_methods()))
  q(save_forecasting_subscriptions(.seed_forecasting_global_subscriptions(metricas_viejas()), client_id = CID))
  q(forecasting_seed_if_empty(client_id = CID))
  f <- fuentes_s3(); m4 <- metricas_s3()
  ok("4.1 tras la siembra: yahoo inactiva, ecb y nyfed dadas de alta", identical(f$active[f$source_id == "yahoo"], FALSE) &&
       all(c("ecb", "nyfed") %in% f$source_id))
  ok("4.2 ...y ninguna métrica del cliente lee yahoo (las nuevas también llegaron)",
     !any(m4$primary_source_id == "yahoo") && all(c("fed_funds", "estr", "cetes28") %in% m4$metric_id))
} else cat(" (sin el paquete uuid: sección 4 omitida)\n")

cat("== 6. Las migraciones dejan su entrada de sistema en el log ==\n")
# The scanner (test_log_action_completeness_scan) trusts their callers to
# log: this is the net -- remove the evento and it falls.
L <- new.env(); L$x <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$x[[length(L$x) + 1L]] <- list(user = user, module = module, action = action, description = description)
  invisible(TRUE)
}
entradas <- function() vapply(L$x, function(e) paste(e$user, e$module, e$action, "|", e$description), "")
s3_memoria_reset()
q(save_forecasting_sources(fuentes_viejas()))
q(save_forecasting_metrics(metricas_viejas(), client_id = CID))
L$x <- list()
q(forecasting_completar_fuentes())
ok("6.1 completar_fuentes: una entrada sistema:forecasting sembrar_catalogo con su texto",
   identical(entradas(), "sistema:forecasting forecasting sembrar_catalogo | Catálogo de fuentes: BCE y Fed de NY agregadas, Yahoo retirada"),
   paste(entradas(), collapse = " / "))
L$x <- list()
q(forecasting_migrar_yahoo(CID))
ok("6.2 migrar_yahoo: una entrada sistema:forecasting sembrar_catalogo con su texto",
   identical(entradas(), "sistema:forecasting forecasting sembrar_catalogo | Catálogo de pronósticos: métricas de Yahoo pasan a Banxico o al BCE"),
   paste(entradas(), collapse = " / "))
L$x <- list()
q(forecasting_completar_fuentes()); q(forecasting_migrar_yahoo(CID))
ok("6.3 otra vez, sin nada que cambiar: ninguna entrada", !length(L$x))
rm(log_action)

cat("== 5. La guardiana ==\n")
codigo <- function(f) { x <- readLines(f, warn = FALSE, encoding = "UTF-8"); x[!grepl("^\\s*#", x)] }
archivos <- c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE),
              list.files("scripts", "\\.R$", full.names = TRUE, recursive = TRUE))
hallados <- Filter(function(f) any(grepl("quantmod::|fcs_fetch_yahoo\\(|forecasting_source_yahoo", codigo(f))), archivos)
ok("5.1 ni quantmod, ni una llamada a fcs_fetch_yahoo(), ni un source() de Yahoo en el código", !length(hallados),
   paste(hallados, collapse = ", "))
ok("5.2 R/forecasting_source_yahoo.R no existe", !file.exists("R/forecasting_source_yahoo.R"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
