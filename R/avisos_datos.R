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
#   omitido_sap_viejo  the oldest snapshot is stale by sap_viejo() (R/
#                      panorama_datos.R: before today - SAP_DIAS_VIEJO
#                      calendar days), the same rule as Panorama's "Neblina"
#                      (T46; the job never asks SAP: it reads what the app
#                      saved);
#   omitido_calculo    the pipeline failed: build_export_combined_df() turns
#                      a ledger's error into a warning and a NULL ledger, and
#                      an email with $0 of cobros is worse than none.
#
# Nothing here writes: load_empresas() may backfill empresas.rds, so the
# company map is built from the table as read. The job only writes under
# <cliente>/avisos/ (its role allows nothing else).
# Never prints: the job runs in a public repo's logs (see avisos_job.R).
# =============================================================================

AVISOS_TOP_N        <- 10L   # Monday's top cobros and pagos (Luis, T45)
AVISOS_TOP_VIERNES  <- 5L    # Friday: what stayed pending, next week's flows
AVISOS_CONTEXTO     <- 4L    # weeks of context on Monday: 4 real before, 4 scheduled after
# The real weeks show only when the confirmations in HopDesk cover this
# share of what the bank statement paid in those 4 weeks (base currency,
# internal movements out): measured in prod on 2026-11-17 (P3), networks
# confirmed 0% -- bars of "real" would have shown nothing as if nothing
# moved. They switch on by themselves once the coverage is reached.
AVISOS_COBERTURA_MIN  <- 0.70
AVISOS_BANCO_INTERNOS <- c("traspaso", "cambio", "plazo", "comision")

# ── Moneda base del cliente (T45, H1) ────────────────────────────────────────
# The client's base currency from its group config, as the app reads it
# (moneda_base_resolver(), R/monedas.R). The job's role reads the whole
# bucket; if the read still fails, the default is used and `leida` says so,
# so the job can count it -- never a client left out for this.
avisos_moneda_base <- function(cid) {
  cfg <- tryCatch(.s3_leer_para_escribir(S3_KEYS$group_config, cid), error = function(e) e)
  if (inherits(cfg, "error")) return(list(moneda = MONEDA_BASE_OMISION, leida = FALSE))
  list(moneda = moneda_base_resolver(if (is.list(cfg)) cfg$moneda_base else NULL)$moneda, leida = TRUE)
}

# Rate of each currency against the base, for ordering the tops only (an
# amount is always shown in its own currency): c(MXN = 1, USD = 18.4).
# Banxico's figures through fx_tipo() (R/fx_tipos.R); a currency without a
# source is left out, never made up. `fx`: fx_fuentes_hoy() (NULL in tests).
avisos_tipos <- function(monedas, base, fx = NULL) {
  m <- unique(toupper(trimws(as.character(monedas)))); m <- m[!is.na(m) & nzchar(m)]
  v <- vapply(m, function(x) fx_tipo(x, base, fix = fx$fix, divisas = fx$divisas)$valor, numeric(1))
  v <- stats::setNames(as.numeric(v), m)
  c(stats::setNames(1, base), v[!is.na(v) & v > 0 & names(v) != base])
}

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
avisos_sap_estado <- function(guardado, ahora = Sys.time(), tz = UMBRAL_TZ) {
  if (!length(guardado) || anyNA(guardado)) return(list(ok = FALSE, motivo = "omitido_sin_sap", fecha = NULL))
  fecha <- min(guardado)
  if (sap_viejo(fecha, ahora, tz))
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
.avisos_semana <- function(df, lunes) .avisos_entre(df, lunes, lunes + 6L)

.avisos_entre <- function(df, desde, hasta) {
  if (is.null(df) || !nrow(df) || !"FechaEff" %in% names(df)) return(df[0, , drop = FALSE])
  f <- as.Date(df$FechaEff); m <- .avisos_monto(df)
  df[!is.na(f) & f >= desde & f <= hasta & !is.na(m) & m > 0 &
       !is.na(df$Moneda) & nzchar(trimws(df$Moneda)), , drop = FALSE]
}

# Sum of the base currency's rows only (charts never convert currencies).
.avisos_en_base <- function(df, base) {
  if (is.null(df) || !nrow(df)) return(0)
  sum(.avisos_monto(df)[toupper(trimws(df$Moneda)) %in% base])
}

# Rows of a ledger that come from Pasivos (a provision).
.avisos_es_provision <- function(df)
  if ("source" %in% names(df)) df$source %in% "provision" else rep(FALSE, nrow(df))
.avisos_aprox <- function(df) if ("aprox" %in% names(df)) df$aprox %in% TRUE else rep(FALSE, nrow(df))

# Named numeric by moneda, one per currency (B's .pan_por_moneda: never
# added across currencies), the client's base first (T39).
.avisos_por_moneda <- function(df, base = MONEDA_BASE_OMISION) {
  t <- .pan_por_moneda(df$Moneda, .avisos_monto(df), base)
  stats::setNames(t$total, t$moneda)
}

# The currencies whose total holds an approximate amount (a provision quoted
# in USD, at today's FIX): that total is shown with "≈" (banxico-fix).
.avisos_monedas_aprox <- function(df) unique(toupper(trimws(df$Moneda[.avisos_aprox(df)])))

# The `n` largest rows: contraparte, empresa (its initials, from the client's
# company map), fecha, monto, moneda, aprox, tipo (AR/AP). Ordered by the
# amount's equivalent in the base (`tipos`, avisos_tipos(); Luis, T45 P2):
# each amount is still shown in its own currency. A row whose currency has
# no rate is left out of the order, and attr "sin_tipo" counts them by
# currency so the email can say so. Without `tipos`, only the base ranks.
.avisos_top <- function(df, cmap, n = AVISOS_TOP_N, tipos = NULL, base = MONEDA_BASE_OMISION) {
  vacio <- data.frame(contraparte = character(), empresa = character(), fecha = as.Date(character()),
                      monto = numeric(), moneda = character(), aprox = logical(), tipo = character(),
                      stringsAsFactors = FALSE)
  attr(vacio, "sin_tipo") <- stats::setNames(integer(), character())
  if (is.null(df) || !nrow(df)) return(vacio)
  if (is.null(tipos)) tipos <- stats::setNames(1, base)
  monto <- .avisos_monto(df)
  mon <- toupper(trimws(as.character(df$Moneda)))
  tasa <- unname(tipos[mon]); tasa[is.na(tasa)] <- NA_real_
  sin <- is.na(tasa)
  o <- which(!sin)
  o <- utils::head(o[order(-(monto[o] * tasa[o]), as.Date(df$FechaEff[o]))], n)
  emp <- as.character(df$Empresa[o])
  ini <- names(cmap)[match(emp, unname(cmap))]
  parte <- as.character(df$Parte[o])
  out <- data.frame(contraparte = ifelse(is.na(parte) | !nzchar(trimws(parte)), "—", parte),
                    empresa = ifelse(is.na(ini), ifelse(is.na(emp), "", emp), ini),
                    fecha = as.Date(df$FechaEff[o]), monto = monto[o], moneda = as.character(df$Moneda[o]),
                    aprox = .avisos_aprox(df)[o], tipo = as.character(df$Tipo[o]), stringsAsFactors = FALSE)
  t <- table(mon[sin])
  attr(out, "sin_tipo") <- stats::setNames(as.integer(t), names(t))
  out
}

# The client's CxC % past due in the base currency on `lunes`, and its
# change since the Monday before (NA without that week), from metricas.rds.
correo_vencidos <- function(serie, lunes, moneda_base = MONEDA_BASE_OMISION) {
  serie <- .normalize(serie, .schema_metricas)
  pct <- function(d) {
    s <- serie[serie$lunes == d & serie$moneda == moneda_base, , drop = FALSE]
    a <- sum(s$saldo_abierto)
    if (!nrow(s) || a <= 0) NA_real_ else 100 * sum(s$saldo_vencido) / a
  }
  hoy <- pct(as.Date(lunes)); antes <- pct(as.Date(lunes) - 7L)
  list(pct = hoy, delta = if (is.na(hoy) || is.na(antes)) NA_real_ else hoy - antes)
}

# The client's CxC past due on `lunes`, by currency (metricas.rds), base first.
.avisos_vencido_monto <- function(serie, lunes, base) {
  serie <- .normalize(serie, .schema_metricas)
  s <- serie[serie$lunes == as.Date(lunes) & serie$saldo_vencido > 0, , drop = FALSE]
  if (!nrow(s)) return(stats::setNames(numeric(), character()))
  t <- .pan_por_moneda(s$moneda, s$saldo_vencido, base)
  stats::setNames(t$total, t$moneda)
}

# What really moved in a past week, base currency only: the confirmations
# still in force (confirmaciones_vigentes(), never !eliminado alone) dated in
# that week. Only what was confirmed in HopDesk -- the email says so.
.avisos_real_semana <- function(conf, lunes, base) {
  vacio <- c(cobros = 0, pagos = 0, n = 0)
  if (is.null(conf) || !is.data.frame(conf) || !nrow(conf)) return(vacio)
  f <- as.Date(conf$fecha)
  s <- conf[!is.na(f) & f >= lunes & f <= lunes + 6L & toupper(trimws(conf$moneda)) %in% base, , drop = FALSE]
  imp <- abs(suppressWarnings(as.numeric(s$importe))); imp[is.na(imp)] <- 0
  c(cobros = sum(imp[s$tipo %in% "cobro"]), pagos = sum(imp[s$tipo %in% "pago"]), n = nrow(s))
}

# The client's bank statement, for the coverage: data.frame(fecha, tipo,
# cargo, abono, moneda), the currency from the movement's account
# (ctas_cuentas, then the older bancos_cuentas). NULL when unreadable: the
# real weeks then stay off. Only ids and currencies of the accounts are kept.
avisos_banco <- function(cid) {
  movs <- tryCatch(suppressMessages(load_bancos_movimientos(client_id = cid)), error = function(e) NULL)
  if (!is.data.frame(movs) || !nrow(movs)) return(NULL)
  par <- function(df, id, mon) if (is.data.frame(df) && all(c(id, mon) %in% names(df)))
    stats::setNames(toupper(trimws(as.character(df[[mon]]))), as.character(df[[id]])) else character()
  ctas <- tryCatch(.s3_leer_para_escribir(S3_KEYS$ctas_cuentas, cid), error = function(e) NULL)
  bc <- tryCatch(suppressMessages(load_bancos_cuentas(client_id = cid)), error = function(e) NULL)
  mon <- c(par(ctas, "id", "Moneda"), par(bc, "cuenta_id", "moneda")); mon <- mon[!duplicated(names(mon))]
  data.frame(fecha = as.Date(movs$fecha), tipo = as.character(movs$tipo),
             cargo = suppressWarnings(as.numeric(movs$cargo)), abono = suppressWarnings(as.numeric(movs$abono)),
             moneda = unname(mon[as.character(movs$cuenta_id)]), stringsAsFactors = FALSE)
}

# Share of the bank's payments (debits, internal movements out, base
# currency) of the AVISOS_CONTEXTO weeks before `lunes` that HopDesk has as
# confirmed payments (intercompany out): the measure of
# scripts/diagnose_cobertura_confirmaciones_readonly.R (pct_pagos). NA
# without a statement for those weeks.
.avisos_cobertura <- function(banco, conf, lunes, base, interco = NULL, cmap = character(), k = AVISOS_CONTEXTO) {
  if (!is.data.frame(banco) || !nrow(banco)) return(NA_real_)
  desde <- lunes - 7L * k; hasta <- lunes - 1L
  b <- banco[!is.na(banco$fecha) & banco$fecha >= desde & banco$fecha <= hasta & banco$moneda %in% base &
               !(banco$tipo %in% AVISOS_BANCO_INTERNOS) & !is.na(banco$cargo) & banco$cargo > 0, , drop = FALSE]
  den <- sum(b$cargo)
  if (den <= 0) return(NA_real_)
  vig <- confirmaciones_vigentes(conf)
  if (!is.data.frame(vig) || !nrow(vig)) return(0)
  vig <- vig[!.avisos_es_ic_conf(vig, interco, cmap), , drop = FALSE]
  fv <- as.Date(vig$fecha)
  p <- vig[vig$tipo %in% "pago" & toupper(trimws(vig$moneda)) %in% base & !is.na(fv) & fv >= desde & fv <= hasta, , drop = FALSE]
  min(1, sum(abs(as.numeric(p$importe)), na.rm = TRUE) / den)
}

# Monday's context: AVISOS_CONTEXTO weeks before (real: confirmed in
# HopDesk), this week and AVISOS_CONTEXTO after (scheduled today), in the
# base currency. data.frame(lunes, cobros, pagos, clase = "real" | "esta" |
# "programado"). The real weeks show only with `mostrar_real` (the coverage,
# above) and when one of them has a confirmation. Intercompany
# confirmations are left out, as from the ledger (they are not the group's
# cash flow).
.avisos_contexto <- function(df, conf, lunes, base, k = AVISOS_CONTEXTO, interco = NULL, cmap = character(),
                             mostrar_real = TRUE) {
  vig <- confirmaciones_vigentes(conf)
  if (is.data.frame(vig) && nrow(vig)) vig <- vig[!.avisos_es_ic_conf(vig, interco, cmap), , drop = FALSE]
  pasadas <- lapply(rev(seq_len(k)), function(i) {
    l <- lunes - 7L * i; r <- .avisos_real_semana(vig, l, base)
    data.frame(lunes = l, cobros = r[["cobros"]], pagos = r[["pagos"]], clase = "real", n = r[["n"]])
  })
  prog <- lapply(0:k, function(i) {
    l <- lunes + 7L * i; s <- .avisos_semana(df, l)
    data.frame(lunes = l, cobros = .avisos_en_base(s[s$Tipo %in% "AR", , drop = FALSE], base),
               pagos = .avisos_en_base(s[s$Tipo %in% "AP", , drop = FALSE], base),
               clase = if (i == 0) "esta" else "programado", n = NA_real_)
  })
  pas <- do.call(rbind, pasadas)
  out <- if (isTRUE(mostrar_real) && sum(pas$n) > 0) rbind(pas, do.call(rbind, prog)) else do.call(rbind, prog)
  out$n <- NULL
  out
}

# Monday to Friday of the week, base currency: data.frame(fecha, cobros,
# pagos). A weekend with rows adds its days, so the bars add up to the cards.
.avisos_por_dia <- function(ar, ap, lunes, base) {
  dias <- lunes + 0:6
  suma <- function(x, d) .avisos_en_base(x[as.Date(x$FechaEff) == d, , drop = FALSE], base)
  out <- data.frame(fecha = dias, cobros = vapply(dias, function(d) suma(ar, d), numeric(1)),
                    pagos = vapply(dias, function(d) suma(ap, d), numeric(1)))
  out[seq_len(7) <= 5 | out$cobros > 0 | out$pagos > 0, , drop = FALSE]
}

# The Monday email's numbers for one client (correo_lunes_html()'s `d`,
# without the per-recipient parts). `df` = avisos_ledger(), `sh` its shared,
# `serie` = metricas.rds after this Monday's rows, `alertas` = umbral_evaluar(),
# `tipos` = avisos_tipos() (orders the tops; NULL: only the base ranks),
# `banco` = avisos_banco() (the coverage that switches the real weeks on).
correo_datos_lunes <- function(df, sh, lunes, serie, alertas, sap_fecha, url = "",
                               moneda_base = MONEDA_BASE_OMISION, tipos = NULL, banco = NULL) {
  lunes <- as.Date(lunes)
  sem <- .avisos_semana(df, lunes)
  ar <- sem[sem$Tipo %in% "AR", , drop = FALSE]
  ap <- sem[sem$Tipo %in% "AP", , drop = FALSE]
  # Pasivos are the provisions among this week's AP rows: a part of "por
  # pagar", so the line inside the card always adds up to it (T45).
  prov <- ap[.avisos_es_provision(ap), , drop = FALSE]
  # Collections past due with no new date are not in this week (their
  # FechaEff already went by): the card says how much (T45 P1).
  todo_ar <- if (is.null(df) || !nrow(df)) df else df[df$Tipo %in% "AR", , drop = FALSE]
  atras <- .avisos_entre(todo_ar, as.Date("1900-01-01"), lunes - 1L)
  cmap <- sh$company_map()
  venc <- correo_vencidos(serie, lunes, moneda_base)
  top_c <- .avisos_top(ar, cmap, AVISOS_TOP_N, tipos, moneda_base)
  top_p <- .avisos_top(ap, cmap, AVISOS_TOP_N, tipos, moneda_base)
  cob <- .avisos_cobertura(banco, sh$bancos_confirmados(), lunes, moneda_base, sh$interco_v2(), cmap)
  list(lunes = lunes,
       cobros = .avisos_por_moneda(ar, moneda_base), pagos = .avisos_por_moneda(ap, moneda_base),
       cobros_n = nrow(ar), pagos_n = nrow(ap),
       cobros_aprox = .avisos_monedas_aprox(ar), pagos_aprox = .avisos_monedas_aprox(ap),
       pasivos_n = nrow(prov), pasivos = .avisos_por_moneda(prov, moneda_base),
       cobros_atrasados = .avisos_por_moneda(atras, moneda_base),
       vencidos_pct = venc$pct, vencidos_delta = venc$delta,
       vencido = .avisos_vencido_monto(serie, lunes, moneda_base),
       por_dia = .avisos_por_dia(ar, ap, lunes, moneda_base),
       cobertura = cob,
       contexto = .avisos_contexto(df, sh$bancos_confirmados(), lunes, moneda_base,
                                   interco = sh$interco_v2(), cmap = cmap,
                                   mostrar_real = !is.na(cob) && cob >= AVISOS_COBERTURA_MIN),
       alertas = alertas,
       top_cobros = top_c, top_pagos = top_p,
       sin_tipo = .avisos_sumar_sin_tipo(top_c, top_p),
       datos_sap = sap_fecha, moneda_base = moneda_base, url = url)
}

# Rows left out of the tops for lack of an exchange rate, by currency.
.avisos_sumar_sin_tipo <- function(...) {
  x <- unlist(lapply(list(...), function(t) attr(t, "sin_tipo")))
  if (!length(x)) return(stats::setNames(integer(), character()))
  t <- tapply(x, names(x), sum)
  stats::setNames(as.integer(t), names(t))
}

# ── Por destinatario ─────────────────────────────────────────────────────────

CORREO_MAX_SOLICITUDES <- 2L

# "También esta semana" for one recipient (a row of correo_destinatarios()):
# their open solicitudes and the client's destacados, from the avisos THIS
# recipient may see -- avisos_visibles() first, as Panorama does, then B's
# panorama_mis_solicitudes() / panorama_destacados(). Never the unfiltered
# list: an aviso for Dev must not reach Admin by email (integrador).
correo_tambien <- function(avisos_df, dest, cid) {
  u <- list(user = dest$usuario, tier = dest$tier, permisos = dest$permisos[[1]])
  vis <- avisos_visibles(cid, u, avisos_df)
  if (!nrow(vis)) return(list())
  sol <- utils::head(panorama_mis_solicitudes(vis, dest$usuario), CORREO_MAX_SOLICITUDES)
  des <- panorama_destacados(vis)
  c(lapply(sol$titulo, function(t) list(tipo = "solicitud", texto = t)),
    lapply(des$titulo, function(t) list(tipo = "comunicado", texto = t)))
}

# ── Semana: lo que el lunes esperaba ─────────────────────────────────────────
# Monday keeps the week's rows in <cliente>/avisos/semana.rds, so Friday
# measures against what Monday expected, not against a Monday recomputed
# after the week changed it. Only the columns compute_confirmed_flags()
# reads. A list, written whole each Monday (no rows to merge), after a
# strict read: an unreadable key is never overwritten blind.

.SEMANA_KEY <- "avisos/semana.rds"
# Parte (Friday's "quedó pendiente" names the counterpart) and provision_id
# (a provision paid in the week is tracked by it) since T45.
.SEMANA_COLS <- c("Tipo", "Empresa", "Documento", "Parte", "Moneda", "Fecha de contabilización",
                  "Saldo_original", "Saldo vencido", "source", "id", "provision_id", "FechaEff")

correo_semana_guardar <- function(cid, df, d) {
  sem <- .avisos_semana(df, d$lunes)
  # sap_fecha: the data Monday planned with; Friday closes only on newer data.
  obj <- list(lunes = as.Date(d$lunes), moneda_base = d$moneda_base %||% MONEDA_BASE_OMISION,
              vencidos_pct = d$vencidos_pct, sap_fecha = d$datos_sap,
              filas = sem[, intersect(.SEMANA_COLS, names(sem)), drop = FALSE])
  .s3_leer_para_escribir(.SEMANA_KEY, cid)
  .s3_write(obj, .SEMANA_KEY, client_id = cid)
  invisible(obj)
}

# This week's Monday record, or why there is none: list(ok, motivo, semana).
correo_semana_leer <- function(cid, viernes) {
  x <- tryCatch(.s3_leer_para_escribir(.SEMANA_KEY, cid), error = function(e) e)
  if (inherits(x, "error")) return(list(ok = FALSE, motivo = "omitido_lectura", semana = NULL))
  if (!is.list(x) || is.null(x$lunes) || !identical(as.Date(x$lunes), umbral_lunes(viernes)))
    return(list(ok = FALSE, motivo = "omitido_sin_lunes", semana = NULL))
  list(ok = TRUE, motivo = NA_character_, semana = x)
}

# ── Viernes ──────────────────────────────────────────────────────────────────

# Which of Monday's rows are confirmed now, by the app's own rule
# (compute_confirmed_flags: bancos_confirmados and the papelera), whether it
# flags the row or drops it (a confirmed manual row).
.avisos_confirmadas <- function(filas, ledger, bancos, papelera) {
  if (!nrow(filas)) return(logical())
  filas$.fila <- seq_len(nrow(filas))
  r <- compute_confirmed_flags(filas, ledger, bancos, papelera)
  !(filas$.fila %in% r$.fila[!(r$confirmed %in% TRUE)])
}

# ── Viernes: cada partida del plan, clasificada (T45 v2, Luis 2026-11-17) ───
# Treasury and CxC move dates all the time, for good reasons: a % against
# Monday's snapshot counted a well-reasoned reschedule as a failure and one
# large item could move it alone. So each committed row of Monday's plan is
# classified by its state at the close (the date of Friday's SAP data), with
# its amount and currency. A row may split: a balance that went down counts
# as collected / paid by the difference, the rest by its date.
#   hecho         confirmed in HopDesk (the app's rule; an abono on a manual row)
#   liquidado     settled in the ERP: no longer open, or its balance went down,
#                 and nobody confirmed it in HopDesk. In Networks that IS the
#                 real collection or payment (P3: almost nothing is confirmed),
#                 so it counts as collected / paid, in a second tone. The SAP
#                 snapshot only carries open items (no DocStatus, no cancelled
#                 flag): a payment and a cancellation look alike, and the
#                 legend says "pago o cancelación"
#   reprogramado  still open with a new date outside the week (and to when)
#   por_vencer    still open and not due yet at the close (this weekend, or
#                 after a mid-week close)
#   atrasado      still open, its date passed by the close, no new date
#   cambio        its amount went up, or a HopDesk row (manual, provision)
#                 was cancelled or deleted
# Apart from the plan: what came in or went out outside it, the intercompany
# confirmations (never group cash flow) and the plans that are not committed
# (P12: provisions behind a requisition, or manual ones with no liability).
# Without fresh SAP data since Monday there is no close at all (P3, punto 3).

AVISOS_CONCENTRACION    <- 0.40   # one item over 40% of what was collected / paid
AVISOS_RACHA_MIN        <- 3L     # "moved N weeks in a row" from 3
AVISOS_PARTIDAS_SEMANAS <- 8L     # weeks kept in semana_partidas.rds
AVISOS_CLASES_CIERRE    <- c("hecho", "liquidado", "reprogramado", "por_vencer", "atrasado", "cambio")
AVISOS_CLASES_PAGADAS   <- c("hecho", "liquidado")

# Which of Monday's provision rows were paid in the week: the provision is
# now "item_confirmed" (converted and its item confirmed in Bancos,
# pasivos_provision_item_confirmed()), or a confirmation in force carries its
# provision_id.
.avisos_provisiones_pagadas <- function(pid, provs, conf) {
  pid <- as.character(pid)
  provs <- if (is.data.frame(provs)) provs else data.frame(id = character(), estado = character())
  hechas <- as.character(provs$id[provs$estado %in% "item_confirmed"])
  vig <- confirmaciones_vigentes(conf)
  por_conf <- if (is.data.frame(vig) && "provision_id" %in% names(vig)) as.character(vig$provision_id) else character()
  !is.na(pid) & (pid %in% hechas | pid %in% por_conf)
}

# A provision of the plan is committed when it belongs to a liability (a
# credit, a lease: a firm date). One behind a requisition (a quote) or a
# manual one with neither is a plan (P12). `req_ids`: requisiciones.rds's
# provision_id; NULL when unknown (then every one without a liability is
# "manual").
.avisos_provision_clase <- function(pid, provs, req_ids = NULL) {
  pid <- as.character(pid)
  liab <- if (is.data.frame(provs) && "liability_id" %in% names(provs))
    as.character(provs$liability_id)[match(pid, as.character(provs$id))] else rep(NA_character_, length(pid))
  ifelse(!is.na(pid) & pid %in% as.character(req_ids %||% character()), "requisicion",
         ifelse(is.na(liab) | !nzchar(liab), "manual", "comprometida"))
}

# The provision_id of every requisition (for .avisos_provision_clase()), or
# NULL when the table cannot be read: the job then counts them as manual.
avisos_requisiciones_provisiones <- function(cid) {
  x <- tryCatch(.s3_leer_para_escribir("requisiciones.rds", cid), error = function(e) NULL)
  if (!is.data.frame(x) || !"provision_id" %in% names(x)) return(NULL)
  p <- as.character(x$provision_id)
  unique(p[!is.na(p) & nzchar(p)])
}

# A row's identity on Monday and on Friday: the provision when it has one
# (a converted provision's item keeps it), else the invoice's identity --
# ledger, empresa, documento, moneda and posting date, the idea of
# agenda_filas_factura(): the ERP never changes them when the balance moves.
.avisos_llave <- function(df) {
  if (is.null(df) || !nrow(df)) return(character())
  col <- function(n) if (n %in% names(df)) as.character(df[[n]]) else rep(NA_character_, nrow(df))
  pid <- col("provision_id")
  ifelse(!is.na(pid) & nzchar(pid), paste0("prov:", pid),
         paste(col("Tipo"), toupper(trimws(col("Empresa"))), trimws(col("Documento")),
               toupper(trimws(col("Moneda"))), col("Fecha de contabilización"), sep = "|"))
}

# Monday's committed rows against the open rows at the close (`df`, the
# ledger: confirmed rows are already out of it). `hecho`: confirmed flags;
# `corte`: the date of the close (Friday, or the day of the last SAP data).
# Returns the pieces: data.frame(fila, clase, monto, fecha_nueva) -- one per
# row, two when its balance went down.
.avisos_clasificar <- function(filas, df, hecho, corte, lunes) {
  vacio <- data.frame(fila = integer(), clase = character(), monto = numeric(), fecha_nueva = as.Date(character()))
  if (!nrow(filas)) return(vacio)
  i <- match(.avisos_llave(filas), .avisos_llave(df))
  hay <- !is.null(df) && nrow(df)
  f_hoy <- if (hay) as.Date(df$FechaEff)[i] else rep(as.Date(NA), nrow(filas))
  m_hoy <- if (hay) .avisos_monto(df)[i] else rep(NA_real_, nrow(filas))
  m_lun <- .avisos_monto(filas)
  de_erp <- !(filas$source %in% c("manual", "provision"))
  por_fecha <- function(f) ifelse(!is.na(f) & f > lunes + 6L, "reprogramado",
                           ifelse(!is.na(f) & f > corte, "por_vencer", "atrasado"))
  piezas <- lapply(seq_len(nrow(filas)), function(k) {
    p <- function(clase, monto, fn = as.Date(NA)) data.frame(fila = k, clase = clase, monto = monto, fecha_nueva = fn)
    if (hecho[k]) return(p("hecho", m_lun[k]))
    if (is.na(i[k])) return(p(if (de_erp[k]) "liquidado" else "cambio", m_lun[k]))
    clase_f <- por_fecha(f_hoy[k]); fn <- if (clase_f == "reprogramado") f_hoy[k] else as.Date(NA)
    if (!is.na(m_hoy[k]) && m_hoy[k] > m_lun[k] + 0.005) return(p("cambio", m_lun[k]))
    if (!is.na(m_hoy[k]) && m_hoy[k] < m_lun[k] - 0.005)
      return(rbind(p(if (de_erp[k]) "liquidado" else "hecho", m_lun[k] - m_hoy[k]), p(clase_f, m_hoy[k], fn)))
    p(clase_f, m_lun[k], fn)
  })
  out <- do.call(rbind, piezas)
  out$fecha_nueva <- as.Date(out$fecha_nueva, origin = "1970-01-01")
  out
}

# One side (AR or AP) of the close: by currency, each class's amount and
# count (of pieces). list(<moneda> = data.frame(clase, monto, n)), base first.
.avisos_composicion <- function(pz, base) {
  if (!nrow(pz)) return(list())
  mon <- toupper(trimws(pz$Moneda)); m <- .avisos_monto(pz)
  orden <- c(intersect(base, unique(mon)), sort(setdiff(unique(mon), base)))
  stats::setNames(lapply(orden, function(x) {
    s <- mon == x
    data.frame(clase = AVISOS_CLASES_CIERRE,
               monto = vapply(AVISOS_CLASES_CIERRE, function(k) sum(m[s & pz$clase == k]), numeric(1)),
               n = vapply(AVISOS_CLASES_CIERRE, function(k) sum(s & pz$clase == k), integer(1)),
               stringsAsFactors = FALSE)
  }), orden)
}

# One item over AVISOS_CONCENTRACION of what was collected / paid (HopDesk +
# ERP) in a currency, with 2+ items (one alone is trivially 100%).
.avisos_concentracion <- function(pz, tipo) {
  h <- pz[pz$clase %in% AVISOS_CLASES_PAGADAS & pz$Tipo %in% tipo, , drop = FALSE]
  if (!nrow(h)) return(NULL)
  m <- tapply(.avisos_monto(h), h$fila, sum); f1 <- h[match(names(m), h$fila), , drop = FALSE]
  if (length(m) < 2L) return(NULL)
  mon <- toupper(trimws(f1$Moneda))
  out <- lapply(unique(mon), function(x) {
    s <- which(mon == x); tot <- sum(m[s]); j <- s[which.max(m[s])]
    if (length(s) < 2L || tot <= 0 || m[j] / tot <= AVISOS_CONCENTRACION) return(NULL)
    parte <- as.character(f1$Parte[j])
    list(tipo = tipo, contraparte = if (is.na(parte) || !nzchar(trimws(parte))) "—" else parte,
         moneda = x, pct = 100 * unname(m[j]) / tot)
  })
  Filter(Negate(is.null), out)
}

# Which confirmations are intercompany: the ledger's own detection
# (apply_ic_filter() as the app's "exclude": the interco v2 registry's
# CardCodes and RFCs, or the company names when none are registered), by
# side -- cobro against the AR codes, pago against the AP ones.
.avisos_es_ic_conf <- function(conf, interco = NULL, cmap = character()) {
  es_ic <- rep(FALSE, if (is.data.frame(conf)) nrow(conf) else 0L)
  if (!length(es_ic) || is.null(interco)) return(es_ic)
  conf$.fila <- seq_len(nrow(conf))
  rfcs <- unique(toupper(trimws(unname(interco$rfcs %||% character()))))
  for (lado in c("AR", "AP")) {
    x <- conf[conf$tipo %in% (if (lado == "AR") "cobro" else "pago"), , drop = FALSE]
    if (!nrow(x)) next
    col <- if (lado == "AR") "Código de cliente" else "Código de proveedor"
    x[[col]] <- if ("codigo" %in% names(x)) as.character(x$codigo) else NA_character_
    x$Parte <- if ("parte" %in% names(x)) as.character(x$parte) else NA_character_
    queda <- suppressMessages(apply_ic_filter(x, mode = "exclude", code_col = col,
                                              ic_codes = build_ic_fullcodes(interco, lado),
                                              ic_rfcs = rfcs[nzchar(rfcs)], cmap = cmap))
    es_ic[setdiff(x$.fila, queda$.fila)] <- TRUE
  }
  es_ic
}

# Confirmations in force dated in the week that are not one of Monday's rows
# (same identity, or the provision_id): outside the plan (P6). Intercompany
# ones go apart: they inflate both sides without being the group's cash flow.
# list(cobros, pagos, ic_n, ic) amounts by currency.
.avisos_fuera_del_plan <- function(conf, filas, lunes, base, interco = NULL, cmap = character()) {
  vacio <- stats::setNames(numeric(), character())
  nada <- list(cobros = vacio, pagos = vacio, cobros_n = 0L, pagos_n = 0L, ic = vacio, ic_n = 0L)
  vig <- confirmaciones_vigentes(conf)
  if (is.null(vig) || !is.data.frame(vig) || !nrow(vig)) return(nada)
  f <- as.Date(vig$fecha)
  s <- vig[!is.na(f) & f >= lunes & f <= lunes + 6L, , drop = FALSE]
  if (!nrow(s)) return(nada)
  llave <- function(e, d, m) paste(toupper(trimws(e)), trimws(d), toupper(trimws(m)), sep = "\x1f")
  plan <- llave(filas$Empresa, filas$Documento, filas$Moneda)
  pid_plan <- if ("provision_id" %in% names(filas)) filas$provision_id[!is.na(filas$provision_id)] else character()
  pid <- if ("provision_id" %in% names(s)) as.character(s$provision_id) else rep(NA_character_, nrow(s))
  fuera <- s[!(llave(s$empresa, s$documento, s$moneda) %in% plan) & !(!is.na(pid) & pid %in% pid_plan), , drop = FALSE]
  if (!nrow(fuera)) return(nada)
  es_ic <- .avisos_es_ic_conf(fuera, interco, cmap)
  suma <- function(x) {
    if (!nrow(x)) return(vacio)
    r <- .pan_por_moneda(toupper(trimws(x$moneda)), abs(as.numeric(x$importe)), base)
    stats::setNames(r$total, r$moneda)
  }
  no_ic <- fuera[!es_ic, , drop = FALSE]
  list(cobros = suma(no_ic[no_ic$tipo %in% "cobro", , drop = FALSE]),
       pagos = suma(no_ic[no_ic$tipo %in% "pago", , drop = FALSE]),
       cobros_n = sum(no_ic$tipo %in% "cobro"), pagos_n = sum(no_ic$tipo %in% "pago"),
       ic = suma(fuera[es_ic, , drop = FALSE]), ic_n = sum(es_ic))
}

# The largest pieces of one class and side, for a list (.avisos_top() plus
# the new date of a rescheduled one).
.avisos_top_clase <- function(pz, clase, tipo, cmap, tipos, base, n = AVISOS_TOP_VIERNES) {
  s <- pz[pz$clase %in% clase & pz$Tipo %in% tipo, , drop = FALSE]
  t <- .avisos_top(s, cmap, n, tipos, base)
  if (nrow(t)) {
    i <- match(paste(t$contraparte, t$fecha, t$monto, t$moneda),
               paste(ifelse(is.na(s$Parte) | !nzchar(trimws(s$Parte)), "—", s$Parte), as.Date(s$FechaEff),
                     .avisos_monto(s), s$Moneda))
    t$fecha_nueva <- s$fecha_nueva[i]
  } else t$fecha_nueva <- as.Date(character())
  t
}

# Is there a close this week? Friday's SAP data must be newer than the data
# Monday planned with (semana$sap_fecha; for a semana.rds without it, newer
# than Monday's date). list(ok, corte): corte = the close's date, Friday or
# the day of the last data when it is earlier (a mid-week refresh).
correo_corte <- function(semana, sap_fecha, viernes, tz = UMBRAL_TZ) {
  viernes <- as.Date(viernes); lunes <- umbral_lunes(viernes)
  if (is.null(sap_fecha) || !length(sap_fecha) || is.na(sap_fecha[1])) return(list(ok = FALSE, corte = NA))
  dia <- as.Date(format(as.POSIXct(sap_fecha[1], tz = "UTC"), tz = tz, "%Y-%m-%d"))
  previo <- semana$sap_fecha
  fresco <- if (!is.null(previo) && length(previo) && !is.na(previo[1])) as.numeric(sap_fecha[1]) > as.numeric(previo[1])
            else dia > lunes
  list(ok = isTRUE(fresco), corte = min(dia, viernes))
}

# The Friday email's numbers (correo_viernes_html()'s `d`, without logros and
# radar). `semana`: Monday's record (semana.rds); `df`: Friday's ledger;
# `tipos`: avisos_tipos(); `serie`: metricas.rds; `historial`,
# `partidas`: the past weeks (semana_historial.rds, semana_partidas.rds).
# An empty table is "no history yet"; NULL is "it could not be read this
# week", and the email says that instead -- never "no history" when there
# may be one (Coordinador, T45). `req_ids`: avisos_requisiciones_provisiones().
correo_datos_viernes <- function(semana, df, sh, viernes, sap_fecha, url = "", tipos = NULL,
                                 serie = NULL, historial = .schema_semana_historial(),
                                 partidas = .schema_semana_partidas(), req_ids = NULL) {
  base <- semana$moneda_base %||% MONEDA_BASE_OMISION
  viernes <- as.Date(viernes); lunes <- umbral_lunes(viernes)
  cmap <- sh$company_map()
  sig <- .avisos_semana(df, lunes + 7L)
  sig_ar <- sig[sig$Tipo %in% "AR", , drop = FALSE]; sig_ap <- sig[sig$Tipo %in% "AP", , drop = FALSE]
  d <- list(viernes = viernes, lunes = lunes, moneda_base = base,
            siguiente = list(lunes = lunes + 7L,
                             cobros = .avisos_por_moneda(sig_ar, base), pagos = .avisos_por_moneda(sig_ap, base),
                             cobros_n = nrow(sig_ar), pagos_n = nrow(sig_ap),
                             top = .avisos_top(sig, cmap, AVISOS_TOP_VIERNES, tipos, base)),
            logros = NULL, radar = NULL, datos_sap = sap_fecha, url = url)
  corte <- correo_corte(semana, sap_fecha, viernes)
  # Without SAP data newer than Monday's there is no close: the email says
  # so and shows only the week ahead (punto 3).
  if (!corte$ok) return(c(d, list(sin_cierre = TRUE, pagos_conf = 0L, pagos_total = 0L)))
  d$sin_cierre <- FALSE; d$corte <- corte$corte

  f <- semana$filas
  for (n in c("Parte", "provision_id", "source")) if (!n %in% names(f)) f[[n]] <- rep(NA_character_, nrow(f))
  bancos <- sh$bancos_confirmados(); pap <- sh$papelera_rv(); provs <- sh$pasivos_provisions_db()

  # Plans that are not committed leave the close (P12).
  es_prov <- .avisos_es_provision(f)
  pclase <- rep(NA_character_, nrow(f))
  if (any(es_prov)) pclase[es_prov] <- .avisos_provision_clase(f$provision_id[es_prov], provs, req_ids)
  es_plan <- es_prov & pclase %in% c("requisicion", "manual")
  pl <- f[es_plan, , drop = FALSE]
  d$planes <- list(n = nrow(pl), requisicion = sum(pclase[es_plan] == "requisicion"),
                   manual = sum(pclase[es_plan] == "manual"), montos = .avisos_por_moneda(pl, base),
                   pagados = sum(.avisos_provisiones_pagadas(pl$provision_id, provs, bancos)))

  cm <- f[!es_plan, , drop = FALSE]
  hecho <- rep(FALSE, nrow(cm))
  es_p <- .avisos_es_provision(cm)
  ar <- cm$Tipo %in% "AR"; ap_doc <- cm$Tipo %in% "AP" & !es_p
  if (any(ar)) hecho[ar] <- .avisos_confirmadas(cm[ar, , drop = FALSE], "AR", bancos, pap)
  if (any(ap_doc)) hecho[ap_doc] <- .avisos_confirmadas(cm[ap_doc, , drop = FALSE], "AP", bancos, pap)
  if (any(es_p)) hecho[es_p] <- .avisos_provisiones_pagadas(cm$provision_id[es_p], provs, bancos)
  cl <- .avisos_clasificar(cm, df, hecho, corte$corte, lunes)
  pz <- cm[cl$fila, , drop = FALSE]
  pz$`Saldo vencido` <- cl$monto; pz$clase <- cl$clase; pz$fecha_nueva <- cl$fecha_nueva; pz$fila <- cl$fila

  cobros <- pz[pz$Tipo %in% "AR", , drop = FALSE]; pagos <- pz[pz$Tipo %in% "AP", , drop = FALSE]
  en_base <- function(x, k) sum(.avisos_monto(x)[x$clase %in% k & toupper(trimws(x$Moneda)) %in% base])
  # A row is "done" when every piece of it was collected / paid.
  hechas <- function(x) { if (!nrow(x)) return(0L); t <- tapply(x$clase %in% AVISOS_CLASES_PAGADAS, x$fila, all); sum(t) }

  ar_hoy <- if (is.null(df) || !nrow(df)) df else df[df$Tipo %in% "AR", , drop = FALSE]
  m <- umbral_metricas(ar_hoy, viernes, sh$interco_v2(), cmap)
  ahora <- correo_vencidos(m, lunes, base)$pct
  antes <- semana$vencidos_pct %||% NA_real_

  d <- c(d, list(
    cobros = .avisos_composicion(cobros, base), pagos = .avisos_composicion(pagos, base),
    # The headline's facts, base currency: collected / paid = HopDesk + ERP.
    titular = list(cobrado = en_base(cobros, AVISOS_CLASES_PAGADAS), cobros_reprog = en_base(cobros, "reprogramado"),
                   cobros_atrasado = en_base(cobros, "atrasado"), pagado = en_base(pagos, AVISOS_CLASES_PAGADAS),
                   pagos_reprog = en_base(pagos, "reprogramado"), pagos_atrasado = en_base(pagos, "atrasado"),
                   hay_cobros = nrow(cobros) > 0, hay_pagos = nrow(pagos) > 0),
    atrasados_cobros = .avisos_top_clase(pz, "atrasado", "AR", cmap, tipos, base),
    atrasados_pagos  = .avisos_top_clase(pz, "atrasado", "AP", cmap, tipos, base),
    reprogramados = .avisos_top_clase(pz, "reprogramado", c("AR", "AP"), cmap, tipos, base),
    concentracion = c(.avisos_concentracion(pz, "AR"), .avisos_concentracion(pz, "AP")),
    fuera = .avisos_fuera_del_plan(bancos, f, lunes, base, sh$interco_v2(), cmap),
    # For the logros (R/avisos_logros.R): committed rows done, and atrasado.
    pagos_conf = as.integer(hechas(pagos)), pagos_total = length(unique(pagos$fila)),
    cobros_atrasados_n = as.integer(sum(cobros$clase == "atrasado")), cobros_total = length(unique(cobros$fila)),
    vencidos_pct = ahora,
    vencidos_delta = if (is.na(ahora) || is.na(antes)) NA_real_ else ahora - antes,
    metricas = m))   # Friday's per-empresa rows, for the logros (R/avisos_logros.R)
  d$partidas_fila <- correo_partidas_filas(pz, lunes)
  d$racha <- correo_racha(d$partidas_fila, partidas, lunes, cmap, tipos, base)
  d$desempeno <- correo_desempeno(d, historial)
  d
}

# ── Historiales del viernes (T45) ────────────────────────────────────────────
# semana.rds keeps only this week. Each Friday adds, under <cliente>/avisos/:
#   semana_historial.rds  one row per week (base currency): what stayed
#                         atrasado, for "Desempeño";
#   semana_partidas.rds   one row per committed item and week (its class and
#                         new date), the last AVISOS_PARTIDAS_SEMANAS weeks,
#                         for "se ha movido varias semanas seguidas".
# Both start empty at the first Friday after T45: the past is never made
# up, and the email says when there is no history yet. Strict read,
# replace this week's rows, write (modificar_tabla): a failed read never
# overwrites a history.

.HISTORIAL_KEY <- "avisos/semana_historial.rds"
.PARTIDAS_KEY  <- "avisos/semana_partidas.rds"

.schema_semana_historial <- function() tibble::tibble(
  lunes = as.Date(character()), moneda = character(),
  atrasado_cobros = numeric(), atrasado_cobros_n = integer(),
  atrasado_pagos = numeric(), atrasado_pagos_n = integer(), vencidos_pct = numeric())

.schema_semana_partidas <- function() tibble::tibble(
  lunes = as.Date(character()), llave = character(), tipo = character(), clase = character(),
  parte = character(), empresa = character(), moneda = character(), monto = numeric(),
  fecha = as.Date(character()), fecha_nueva = as.Date(character()))

correo_historial_fila <- function(d) {
  b <- d$moneda_base
  pieza <- function(lado, k) { x <- d[[lado]][[b]]; if (is.null(x)) 0 else x[[k]][x$clase == "atrasado"] }
  tibble::tibble(lunes = as.Date(d$lunes), moneda = b,
                 atrasado_cobros = as.numeric(pieza("cobros", "monto")), atrasado_cobros_n = as.integer(pieza("cobros", "n")),
                 atrasado_pagos = as.numeric(pieza("pagos", "monto")), atrasado_pagos_n = as.integer(pieza("pagos", "n")),
                 vencidos_pct = as.numeric(d$vencidos_pct %||% NA_real_))
}

correo_historial_guardar <- function(cid, d) {
  fila <- correo_historial_fila(d)
  modificar_tabla(.HISTORIAL_KEY, cid, .schema_semana_historial, function(h)
    dplyr::arrange(dplyr::bind_rows(h[!(h$lunes == fila$lunes & h$moneda == fila$moneda), , drop = FALSE], fila),
                   .data$lunes))
}

correo_partidas_filas <- function(c, lunes) {
  if (is.null(c) || !nrow(c)) return(.schema_semana_partidas())
  tibble::tibble(lunes = as.Date(lunes), llave = .avisos_llave(c), tipo = as.character(c$Tipo),
                 clase = as.character(c$clase), parte = as.character(c$Parte), empresa = as.character(c$Empresa),
                 moneda = as.character(c$Moneda), monto = .avisos_monto(c), fecha = as.Date(c$FechaEff),
                 fecha_nueva = as.Date(c$fecha_nueva))
}

correo_partidas_guardar <- function(cid, filas, lunes) {
  lunes <- as.Date(lunes)
  modificar_tabla(.PARTIDAS_KEY, cid, .schema_semana_partidas, function(h) {
    h <- h[h$lunes != lunes & h$lunes > lunes - 7L * AVISOS_PARTIDAS_SEMANAS, , drop = FALSE]
    dplyr::arrange(dplyr::bind_rows(h, filas), .data$lunes)
  }, permitir_reduccion = TRUE)   # weeks older than the window leave
}

# Items rescheduled again this week that were rescheduled the weeks right
# before too: AVISOS_RACHA_MIN weeks in a row or more. list(sin_historia,
# ilegible, filas = data.frame like .avisos_top() plus semanas and
# fecha_nueva). `partidas` NULL: the history could not be read (ilegible).
correo_racha <- function(hoy, partidas, lunes, cmap, tipos, base) {
  if (is.null(partidas)) return(list(sin_historia = FALSE, ilegible = TRUE, filas = NULL, minimo = AVISOS_RACHA_MIN))
  p <- .normalize(partidas, .schema_semana_partidas)
  sin <- !nrow(p[p$lunes < as.Date(lunes), , drop = FALSE])
  r <- hoy[hoy$clase == "reprogramado", , drop = FALSE]
  vacio <- list(sin_historia = sin, filas = NULL, minimo = AVISOS_RACHA_MIN)
  if (sin || !nrow(r)) return(vacio)
  semanas <- vapply(r$llave, function(k) {
    n <- 1L
    while (any(p$llave == k & p$lunes == as.Date(lunes) - 7L * n & p$clase == "reprogramado")) n <- n + 1L
    n
  }, integer(1))
  r <- r[semanas >= AVISOS_RACHA_MIN, , drop = FALSE]; semanas <- semanas[semanas >= AVISOS_RACHA_MIN]
  if (!nrow(r)) return(vacio)
  o <- utils::head(order(-semanas, -r$monto), AVISOS_TOP_VIERNES)
  emp <- r$empresa[o]; ini <- names(cmap)[match(emp, unname(cmap))]
  list(sin_historia = FALSE, minimo = AVISOS_RACHA_MIN, filas = data.frame(
    contraparte = ifelse(is.na(r$parte[o]) | !nzchar(trimws(r$parte[o])), "—", r$parte[o]),
    empresa = ifelse(is.na(ini), ifelse(is.na(emp), "", emp), ini), fecha = r$fecha[o],
    monto = r$monto[o], moneda = r$moneda[o], aprox = FALSE, tipo = r$tipo[o],
    fecha_nueva = r$fecha_nueva[o], semanas = semanas[o], stringsAsFactors = FALSE))
}

# "Desempeño": what stayed atrasado (amount and count, base currency)
# against last week and the median of the 4 before -- never a %
# (rescheduling for a good reason is not a failure). NA where there is no
# history; `sin_historia` says so, `ilegible` when it could not be read.
correo_desempeno <- function(d, historial = .schema_semana_historial()) {
  b <- d$moneda_base; lunes <- as.Date(d$lunes)
  ilegible <- is.null(historial)
  h <- .normalize(historial, .schema_semana_historial)
  h <- h[h$moneda %in% b & h$lunes < lunes, , drop = FALSE]
  h <- h[order(h$lunes, decreasing = TRUE), , drop = FALSE]
  hoy <- correo_historial_fila(d)
  linea <- function(m, n, hm, hn) list(
    hoy = m, hoy_n = n,
    semana_pasada = if (nrow(h) && h$lunes[1] == lunes - 7L) hm[1] else NA_real_,
    semana_pasada_n = if (nrow(h) && h$lunes[1] == lunes - 7L) hn[1] else NA_integer_,
    mediana = if (nrow(h)) stats::median(utils::head(hm, 4L)) else NA_real_)
  list(sin_historia = !ilegible && !nrow(h), ilegible = ilegible,
       cobros = linea(hoy$atrasado_cobros, hoy$atrasado_cobros_n, h$atrasado_cobros, h$atrasado_cobros_n),
       pagos = linea(hoy$atrasado_pagos, hoy$atrasado_pagos_n, h$atrasado_pagos, h$atrasado_pagos_n))
}
