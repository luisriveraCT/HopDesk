# =============================================================================
# R/avisos_umbrales.R
# Umbrales (frente D): turn an unusual weekly change into an automatic aviso.
#
# What is measured: the % of the open receivables (CxC) that is past due, per
# empresa and moneda, once a week (Monday). Each Monday the job adds one row
# per empresa and moneda to <cliente>/avisos/metricas.rds; nobody kept this
# history before, so the series starts cold.
#
# The rule, per empresa and moneda, with Δ = this week's % minus last week's
# and H = up to the 12 previous weekly Δ:
#   1. history: H has at least 4 values (a Δ needs two consecutive Mondays);
#   2. not a small empresa: 10+ open invoices and 5%+ of the client's open
#      balance in that moneda (with 6 invoices one invoice moves 16 pts);
#   3. unusual for that empresa: |Δ - median(H)| >= max(3 * MAD(H), 1.0 pts),
#      MAD scaled to a standard deviation (R's mad()), so one odd week does
#      not inflate it, and the 1.0 pts floor keeps a very steady empresa from
#      alerting on 0.3 pts.
# Rises and falls both alert (Luis, 2026-11-11); a fall is also a Friday
# logro. The direction has to agree with the sign of Δ itself, so the text
# never says "subieron" about a week where the % went down.
#
# Never prints: the job runs in a public repo's logs (see avisos_job.R).
# =============================================================================

UMBRAL_HISTORIA_MIN <- 4L     # Δ needed before any alert
UMBRAL_VENTANA      <- 12L    # previous Δ used as the empresa's usual change
UMBRAL_K            <- 3      # how many robust deviations count as unusual
UMBRAL_PISO_PTS     <- 1.0    # never alert below this many points
UMBRAL_FACTURAS_MIN <- 10L    # smaller empresas are noise
UMBRAL_PARTE_MIN    <- 0.05   # share of the client's open balance in that moneda
UMBRAL_TZ           <- "America/Mexico_City"
.METRICAS_KEY       <- "avisos/metricas.rds"

.schema_metricas <- function() tibble::tibble(
  semana        = character(),   # ISO week, "2026-W47"
  lunes         = as.Date(character()),
  empresa       = character(),
  moneda        = character(),
  saldo_abierto = numeric(),
  saldo_vencido = numeric(),
  facturas      = integer()
)

umbral_semana <- function(fecha) format(as.Date(fecha), "%G-W%V")
umbral_lunes  <- function(fecha) {
  d <- as.Date(fecha)
  d - (as.integer(format(d, "%u")) - 1L)
}

# Sunday 23:59:59 in Mexico of the week of `fecha`, in UTC: when the
# week's avisos expire.
umbral_fin_semana <- function(fecha) {
  dom <- umbral_lunes(fecha) + 6L
  as.POSIXct(format(as.POSIXct(paste(dom, "23:59:59"), tz = UMBRAL_TZ), tz = "UTC"), tz = "UTC")
}

# Intercompany CxC is left out (integrador, 2026-11-11): a past-due invoice
# between group companies is no collection risk and would move the % for
# nothing. The same detection the app applies with ic_mode = "exclude"
# (cashflow export): the interco v2 registry's CardCodes and RFCs through
# apply_ic_filter(), which falls back to the company names when none are
# registered. `interco` = load_interco_v2(cid), `cmap` = the client's
# initials -> name map.
umbral_ar_sin_ic <- function(ar_df, interco, cmap) {
  if (is.null(ar_df) || !is.data.frame(ar_df) || !nrow(ar_df)) return(ar_df)
  rfcs  <- unique(toupper(trimws(unname(interco$rfcs %||% character()))))
  codes <- build_ic_fullcodes(interco, "AR")
  apply_ic_filter(ar_df, mode = "exclude", code_col = "Código de cliente",
                  ic_codes = codes, ic_rfcs = rfcs[nzchar(rfcs)], cmap = cmap)
}

# One row per empresa and moneda from the AR ledger (build_ledger_df(..., "AR")),
# intercompany out: open balance = `Saldo vencido` (what is still owed,
# after abonos), past due when its contractual due date (FechaVenc_Original)
# is before `hoy`. `interco` and `cmap` are required, so no caller can
# forget the IC filter.
umbral_metricas <- function(ar_df, hoy, interco, cmap) {
  hoy <- as.Date(hoy)
  vacio <- .schema_metricas()
  ar_df <- umbral_ar_sin_ic(ar_df, interco, cmap)
  if (is.null(ar_df) || !is.data.frame(ar_df) || !nrow(ar_df)) return(vacio)
  df <- tibble::tibble(
    empresa = as.character(ar_df$Empresa),
    moneda  = as.character(ar_df$Moneda),
    saldo   = suppressWarnings(as.numeric(ar_df$`Saldo vencido`)),
    vence   = as.Date(ar_df$FechaVenc_Original))
  df <- df[!is.na(df$empresa) & !is.na(df$moneda) & !is.na(df$saldo) & df$saldo > 0, , drop = FALSE]
  if (!nrow(df)) return(vacio)
  out <- df |>
    dplyr::group_by(.data$empresa, .data$moneda) |>
    dplyr::summarise(
      saldo_abierto = sum(.data$saldo),
      saldo_vencido = sum(.data$saldo[!is.na(.data$vence) & .data$vence < hoy]),
      facturas      = dplyr::n(),
      .groups = "drop")
  tibble::tibble(semana = umbral_semana(hoy), lunes = umbral_lunes(hoy),
                 empresa = out$empresa, moneda = out$moneda,
                 saldo_abierto = out$saldo_abierto, saldo_vencido = out$saldo_vencido,
                 facturas = as.integer(out$facturas))
}

# The series with this week's rows in place of any earlier run of the same
# week: running Monday twice never adds a second row.
umbral_serie_agregar <- function(serie, nuevas) {
  serie <- .normalize(serie, .schema_metricas)
  if (!nrow(nuevas)) return(serie)
  quitar <- paste(serie$semana, serie$empresa, serie$moneda) %in%
            paste(nuevas$semana, nuevas$empresa, nuevas$moneda)
  dplyr::arrange(dplyr::bind_rows(serie[!quitar, , drop = FALSE], nuevas),
                 .data$lunes, .data$empresa, .data$moneda)
}

# Strict read, add, write (persistence's modificar_tabla). A failed read
# stops: it is never an empty series.
umbral_serie_guardar <- function(client_id, nuevas) {
  modificar_tabla(.METRICAS_KEY, client_id, .schema_metricas,
                  function(serie) umbral_serie_agregar(serie, nuevas))
}

.pct <- function(vencido, abierto) ifelse(abierto > 0, 100 * vencido / abierto, NA_real_)

# Alerts for the week of `fecha`, one row per empresa and moneda that passes
# the three conditions. Pure: the tests feed it invented series.
umbral_evaluar <- function(serie, fecha) {
  serie <- .normalize(serie, .schema_metricas)
  vacio <- tibble::tibble(semana = character(), empresa = character(), moneda = character(),
                          pct = numeric(), delta = numeric(), umbral = numeric(),
                          direccion = character())
  lunes <- umbral_lunes(fecha)
  semana_df <- serie[serie$lunes == lunes, , drop = FALSE]
  if (!nrow(semana_df)) return(vacio)
  total_moneda <- tapply(semana_df$saldo_abierto, semana_df$moneda, sum)
  filas <- lapply(seq_len(nrow(semana_df)), function(i) {
    act <- semana_df[i, ]
    # 2. small empresa
    if (act$facturas < UMBRAL_FACTURAS_MIN) return(NULL)
    if (act$saldo_abierto < UMBRAL_PARTE_MIN * total_moneda[[act$moneda]]) return(NULL)
    s <- serie[serie$empresa == act$empresa & serie$moneda == act$moneda & serie$lunes <= lunes, , drop = FALSE]
    s <- s[order(s$lunes), , drop = FALSE]
    p <- .pct(s$saldo_vencido, s$saldo_abierto)
    n <- nrow(s)
    if (n < 2L) return(NULL)
    # Δ only between consecutive Mondays: a gap is not a weekly change.
    consec <- as.integer(diff(s$lunes)) == 7L
    d <- diff(p)
    if (!consec[n - 1L] || is.na(d[n - 1L])) return(NULL)
    delta <- d[n - 1L]
    previos <- d[seq_len(n - 2L)][consec[seq_len(n - 2L)]]
    previos <- utils::tail(previos[!is.na(previos)], UMBRAL_VENTANA)
    # 1. history
    if (length(previos) < UMBRAL_HISTORIA_MIN) return(NULL)
    # 3. unusual for this empresa
    med    <- stats::median(previos)
    umbral <- max(UMBRAL_K * stats::mad(previos), UMBRAL_PISO_PTS)
    if (abs(delta - med) < umbral) return(NULL)
    if (sign(delta) == 0 || sign(delta) != sign(delta - med)) return(NULL)
    tibble::tibble(semana = act$semana, empresa = act$empresa, moneda = act$moneda,
                   pct = p[n], delta = delta, umbral = umbral,
                   direccion = if (delta > 0) "sube" else "baja")
  })
  dplyr::bind_rows(vacio, dplyr::bind_rows(filas))
}

# Client tiers that see CxC: the Calendario is not fixed to AP for them.
# From the registry, so Compras (calendario = "AP") never gets a CxC alert.
umbral_niveles_cxc <- function() {
  Filter(function(t) is.null(nivel_calendario(t)), niveles_cliente())
}

# The aviso for one alert row. Text in Spanish, as every stored aviso.
umbral_aviso <- function(alerta, fecha) {
  dir <- alerta$direccion
  list(
    tipo      = "automatico",
    titulo    = tr(paste0("avisos.umbral.cxc_", dir, ".titulo"), idioma = "es",
                   empresa = alerta$empresa, moneda = alerta$moneda,
                   pts = sprintf("%.1f", abs(alerta$delta))),
    cuerpo    = tr(paste0("avisos.umbral.cxc_", dir, ".cuerpo"), idioma = "es",
                   pct = sprintf("%.1f", alerta$pct)),
    severidad = if (identical(dir, "sube")) "atencion" else "info",
    niveles   = umbral_niveles_cxc(),
    clave     = paste("umbral", "cxc_vencidos", dir, alerta$empresa, alerta$moneda, alerta$semana, sep = ":"),
    vence     = umbral_fin_semana(fecha))
}

# Publish the week's alerts. avisos_agregar() dedups by clave, so a second
# run of the same Monday refreshes instead of duplicating. Returns how many.
umbral_publicar <- function(client_id, alertas, fecha) {
  for (i in seq_len(nrow(alertas)))
    avisos_agregar(client_id, umbral_aviso(alertas[i, ], fecha), por = "sistema")
  nrow(alertas)
}
