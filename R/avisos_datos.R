# =============================================================================
# R/avisos_datos.R
# Frente D: the numbers of the weekly emails, from what the app left in S3.
#
# The job is not a Shiny session, so it builds its own `shared`: a list of
# functions that return each table read from S3 for one client, the calling
# convention build_export_combined_df() (R/cashflow_export_module.R) expects.
# The rows are then the Calendar's and Exportar's own (open, no confirmed, no
# papelera, intercompany out, with FechaEff): the pipeline is reused, never
# copied, and that module is not touched.
#
# Never a half-read client (integrador, 2026-11-12). Each key is read first
# with .s3_leer_para_escribir(): it reads, or S3 says it does not exist; any
# other answer (a failed read, a 403) stops. The values go to the preload
# cache, so the app's own load_*() then read exactly that. A client is left
# out of the week's emails when
#   omitido_lectura    a key could not be read;
#   omitido_sin_sap    a SAP snapshot (AR or AP) is missing;
#   omitido_sap_viejo  the oldest snapshot is over AVISOS_SAP_DIAS_MAX days
#                      old (the job never asks SAP: it reads what the app
#                      saved; Panorama's "Neblina", lenient for the weekend);
#   omitido_calculo    the pipeline failed: build_export_combined_df() turns
#                      a ledger's error into a warning and a NULL ledger, and
#                      an email with $0 of cobros is worse than none.
#
# Nothing here writes: load_empresas() may backfill empresas.rds, so the
# company map is built from the table as read. The job only writes under
# <cliente>/avisos/ (its role allows nothing else).
# Never prints: the job runs in a public repo's logs (see avisos_job.R).
# =============================================================================

AVISOS_SAP_DIAS_MAX <- 3
AVISOS_TOP_N        <- 5L

# The keys build_export_combined_df() reads through `shared`.
.avisos_llaves_ledger <- function() unname(unlist(c(
  S3_KEYS_CRITICAL[c("sap_snap_ar", "sap_snap_ap", "empresas")],
  S3_KEYS[c("moves", "manual", "abonos", "policy_moves", "sap_overrides", "pasivos_provisions",
            "pasivos_liabilities", "bancos_confirmados", "papelera", "interco_v2")])))

# Strict read of every key into the preload cache. stop()s on the first key
# S3 cannot answer for; a key S3 says does not exist stays out (its load_*()
# returns the empty table, as in the app).
avisos_precargar <- function(cid) {
  for (k in .avisos_llaves_ledger()) {
    x <- .s3_leer_para_escribir(k, cid)
    if (!is.null(x)) assign(.s3_key(k, client_id = cid), x, envir = .s3_preload_cache)
  }
  invisible(TRUE)
}

avisos_soltar <- function(cid) {
  llaves <- vapply(.avisos_llaves_ledger(), function(k) .s3_key(k, client_id = cid), character(1))
  for (env in list(.s3_preload_cache, .s3_missing_cache))
    suppressWarnings(rm(list = intersect(llaves, ls(env)), envir = env))
  invisible(TRUE)
}

# The job's `shared` for one client, from the preloaded tables. Also
# carries sap_guardado: when each snapshot (AR, AP) was saved, NA if missing.
avisos_shared <- function(cid) {
  snaps <- list(AR = load_sap_snapshot("AR", cid), AP = load_sap_snapshot("AP", cid))
  cmap  <- .company_map_from_empresas(
    .normalize(.s3_read_with(S3_KEYS_CRITICAL$empresas, client_id = cid), .schema_empresas))
  t <- list(moves = load_moves(cid), manual = load_manual(cid), abonos = load_abonos(cid),
            policy = load_policy_moves(cid), sap_ov = load_sap_overrides(cid),
            provs = load_pasivos_provisions(cid), liabs = load_pasivos_liabilities(cid),
            bancos = load_bancos_confirmados(cid), papelera = load_papelera(cid),
            interco = load_interco_v2(cid))
  guardado <- vapply(snaps, function(s) if (is.null(s$saved_at)) NA_real_ else as.numeric(s$saved_at), numeric(1))
  list(
    company_map            = function() cmap,
    sap_data               = function() list(AR = snaps$AR$data, AP = snaps$AP$data),
    moves_db               = function() t$moves,
    manual_inv             = function() t$manual,
    abonos_db              = function() t$abonos,
    policy_moves_db        = function() t$policy,
    sap_ov_db              = function() t$sap_ov,
    pasivos_provisions_db  = function() t$provs,
    pasivos_liabilities_db = function() t$liabs,
    bancos_confirmados     = function() t$bancos,
    papelera_rv            = function() t$papelera,
    interco_v2             = function() t$interco,
    sap_guardado           = as.POSIXct(guardado, origin = "1970-01-01", tz = "UTC"))
}

# list(ok, motivo, fecha): fecha = the oldest snapshot, what the email says.
avisos_sap_estado <- function(guardado, ahora = Sys.time()) {
  if (!length(guardado) || anyNA(guardado)) return(list(ok = FALSE, motivo = "omitido_sin_sap", fecha = NULL))
  fecha <- min(guardado)
  if (as.numeric(difftime(ahora, fecha, units = "days")) > AVISOS_SAP_DIAS_MAX)
    return(list(ok = FALSE, motivo = "omitido_sap_viejo", fecha = fecha))
  list(ok = TRUE, motivo = NA_character_, fecha = fecha)
}

# The combined AR + AP rows, or NULL when the pipeline failed. Its own
# warning ("build_export_combined_df: AR pipeline error: ...") is a failure;
# every other warning and message is silenced, never printed.
avisos_ledger <- function(sh) {
  falla <- FALSE
  df <- tryCatch(withCallingHandlers(
    build_export_combined_df(sh, "exclude"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "build_export_combined_df:")) falla <<- TRUE
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")),
    error = function(e) { falla <<- TRUE; NULL })
  if (falla || is.null(df)) return(NULL)
  df
}

# Everything for one client, or why it is left out:
# list(ok, motivo, shared, df, sap_fecha). The preload is dropped at the end.
avisos_cliente_datos <- function(cid, ahora = Sys.time()) {
  on.exit(avisos_soltar(cid), add = TRUE)
  no <- function(motivo, fecha = NULL) list(ok = FALSE, motivo = motivo, shared = NULL, df = NULL, sap_fecha = fecha)
  leido <- tryCatch(suppressMessages(suppressWarnings(avisos_precargar(cid))), error = function(e) FALSE)
  if (!isTRUE(leido)) return(no("omitido_lectura"))
  sh <- tryCatch(suppressMessages(suppressWarnings(avisos_shared(cid))), error = function(e) NULL)
  if (is.null(sh)) return(no("omitido_lectura"))
  sap <- avisos_sap_estado(sh$sap_guardado, ahora)
  if (!sap$ok) return(no(sap$motivo, sap$fecha))
  df <- avisos_ledger(sh)
  if (is.null(df)) return(no("omitido_calculo", sap$fecha))
  list(ok = TRUE, motivo = NA_character_, shared = sh, df = df, sap_fecha = sap$fecha)
}

# ── Lunes ────────────────────────────────────────────────────────────────────

.avisos_monto <- function(df) abs(suppressWarnings(as.numeric(df[["Saldo vencido"]])))

# Rows whose effective date falls in the week of `lunes` (Monday to Sunday),
# with an amount and a currency: a row without them is never counted.
.avisos_semana <- function(df, lunes) {
  if (is.null(df) || !nrow(df) || !"FechaEff" %in% names(df)) return(df[0, , drop = FALSE])
  f <- as.Date(df$FechaEff); m <- .avisos_monto(df)
  df[!is.na(f) & f >= lunes & f <= lunes + 6L & !is.na(m) & m > 0 &
       !is.na(df$Moneda) & nzchar(trimws(df$Moneda)), , drop = FALSE]
}
.avisos_aprox <- function(df) if ("aprox" %in% names(df)) df$aprox %in% TRUE else rep(FALSE, nrow(df))

# Named numeric by moneda, one per currency (B's .pan_por_moneda: never
# added across currencies), MXN first.
.avisos_por_moneda <- function(df) {
  t <- .pan_por_moneda(df$Moneda, .avisos_monto(df))
  stats::setNames(t$total, t$moneda)
}

# The currencies whose total holds an approximate amount (a provision quoted
# in USD, at today's FIX): that total is shown with "≈" (banxico-fix).
.avisos_monedas_aprox <- function(df) unique(toupper(trimws(df$Moneda[.avisos_aprox(df)])))

# The AVISOS_TOP_N largest rows: contraparte, empresa (its initials, from the
# client's company map), fecha, monto, moneda, aprox.
.avisos_top <- function(df, cmap) {
  vacio <- data.frame(contraparte = character(), empresa = character(), fecha = as.Date(character()),
                      monto = numeric(), moneda = character(), aprox = logical(), stringsAsFactors = FALSE)
  if (!nrow(df)) return(vacio)
  monto <- .avisos_monto(df)
  o <- utils::head(order(-monto, as.Date(df$FechaEff)), AVISOS_TOP_N)
  emp <- as.character(df$Empresa[o])
  ini <- names(cmap)[match(emp, unname(cmap))]
  parte <- as.character(df$Parte[o])
  data.frame(contraparte = ifelse(is.na(parte) | !nzchar(trimws(parte)), "—", parte),
             empresa = ifelse(is.na(ini), ifelse(is.na(emp), "", emp), ini),
             fecha = as.Date(df$FechaEff[o]), monto = monto[o], moneda = as.character(df$Moneda[o]),
             aprox = .avisos_aprox(df)[o], stringsAsFactors = FALSE)
}

# The client's CxC % past due in the base currency on `lunes`, and its
# change since the Monday before (NA without that week), from metricas.rds.
correo_vencidos <- function(serie, lunes, moneda_base = "MXN") {
  serie <- .normalize(serie, .schema_metricas)
  pct <- function(d) {
    s <- serie[serie$lunes == d & serie$moneda == moneda_base, , drop = FALSE]
    a <- sum(s$saldo_abierto)
    if (!nrow(s) || a <= 0) NA_real_ else 100 * sum(s$saldo_vencido) / a
  }
  hoy <- pct(as.Date(lunes)); antes <- pct(as.Date(lunes) - 7L)
  list(pct = hoy, delta = if (is.na(hoy) || is.na(antes)) NA_real_ else hoy - antes)
}

# The Monday email's numbers for one client (correo_lunes_html()'s `d`,
# without the per-recipient parts). `df` = avisos_ledger(), `sh` its shared,
# `serie` = metricas.rds after this Monday's rows, `alertas` = umbral_evaluar().
correo_datos_lunes <- function(df, sh, lunes, serie, alertas, sap_fecha, url = "", moneda_base = "MXN") {
  lunes <- as.Date(lunes)
  sem <- .avisos_semana(df, lunes)
  ar <- sem[sem$Tipo %in% "AR", , drop = FALSE]
  ap <- sem[sem$Tipo %in% "AP", , drop = FALSE]
  cmap <- sh$company_map()
  pas <- panorama_pasivos_pronto(sh$pasivos_provisions_db(), lunes, 6L)
  venc <- correo_vencidos(serie, lunes, moneda_base)
  list(lunes = lunes,
       cobros = .avisos_por_moneda(ar), pagos = .avisos_por_moneda(ap),
       cobros_aprox = .avisos_monedas_aprox(ar), pagos_aprox = .avisos_monedas_aprox(ap),
       pasivos_n = as.integer(sum(pas$totales$n)), pasivos = stats::setNames(pas$totales$total, pas$totales$moneda),
       vencidos_pct = venc$pct, vencidos_delta = venc$delta,
       alertas = alertas,
       top_cobros = .avisos_top(ar, cmap), top_pagos = .avisos_top(ap, cmap),
       datos_sap = sap_fecha, moneda_base = moneda_base, url = url)
}
