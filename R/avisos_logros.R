# =============================================================================
# R/avisos_logros.R
# Frente D: the Friday email's "Logros de la semana" and "En el radar", from
# the catalog Luis approved (requisiciones/D_mockup_correos.html, 2026-11-11).
# Each line appears only when the app's own data prove it that week; never
# as an amount (Friday has none), only %, points, counts and days.
#
# Implemented here (what the job already computes):
#   logros  1 strong fall (Monday's umbral rule)       alta    CxC
#           2 lowest of 12 weeks (8+ stored)           alta    CxC
#           3 an empresa closed with no past due       alta    CxC
#           4 every payment of the week confirmed      media
#           6 100% of the expected collections         media   CxC
#          14 third weekly fall in a row (client)      normal  CxC
#   radar   1 unusual rise (Monday's umbral rule)              CxC
#           3 next week payments over collections 1.3+ to 1    CxC
#           6 one supplier with 35%+ of next week's payments
#           7 this week's payments still unconfirmed
#           9 SAP data over 2 business days old
# Still to come, they need data the job does not read yet: logros 5, 7, 8,
# 9, 10, 11, 12, 13 (requisiciones, bank movements, collection days, aging,
# the email history for streaks) and radar 2, 4, 5, 8. Ties go by catalog
# order until the email history exists (the catalog's "rarest first").
#
# An item: tipo ("logro" | "radar"), n (catalog number), prioridad (1 alta,
# 2 media, 3 normal), cxc, and params for its text. Texts are built per
# recipient (language; Compras gets no CxC line) by correo_items_texto().
# =============================================================================

LOGRO_SEMANAS_MIN      <- 12L    # logro 2: the window
LOGRO_SEMANAS_GUARDADAS <- 8L    # logro 2: weeks stored before it may say "lowest"
RADAR_RAZON_MIN        <- 1.3    # radar 3
RADAR_CONCENTRACION    <- 35     # radar 6, %
RADAR_SAP_DIAS_HABILES <- 2L     # radar 9

.item <- function(tipo, n, prioridad, cxc, ...)
  list(tipo = tipo, n = as.integer(n), prioridad = as.integer(prioridad), cxc = cxc, params = list(...))

# Business days (Monday to Friday) after `desde` up to `hasta`.
.dias_habiles <- function(desde, hasta) {
  desde <- as.Date(desde); hasta <- as.Date(hasta)
  if (is.na(desde) || is.na(hasta) || hasta <= desde) return(0L)
  d <- seq(desde + 1L, hasta, by = "day")
  sum(as.integer(format(d, "%u")) <= 5L)
}

# Client-level % past due (base currency) on each of the last `n` Mondays up
# to `lunes`, oldest first; NA for a Monday with no rows.
.pct_cliente_semanas <- function(serie, lunes, n, base) {
  lunes <- as.Date(lunes)
  vapply(rev(seq_len(n) - 1L), function(k) correo_vencidos(serie, lunes - 7L * k, base)$pct, numeric(1))
}

# Logros and radar items for one client's Friday.
#   serie  metricas.rds (Monday rows, this week's included)
#   m_hoy  umbral_metricas() of Friday's CxC rows (per empresa and moneda)
#   dv     correo_datos_viernes()
#   df     Friday's rows (avisos_ledger), for next week's radar lines
correo_items_viernes <- function(serie, m_hoy, dv, df, base = "MXN") {
  viernes <- as.Date(dv$viernes); lunes <- umbral_lunes(viernes)
  serie <- .normalize(serie, .schema_metricas)
  items <- list()
  pts <- function(x) sprintf("%.1f", abs(x))

  # Logros 1 and radar 1: Monday's umbral rule, both directions.
  al <- umbral_evaluar(serie, viernes)
  for (i in seq_len(nrow(al))) {
    items[[length(items) + 1L]] <- if (al$direccion[i] == "baja")
      .item("logro", 1, 1, TRUE, empresa = al$empresa[i], moneda = al$moneda[i], pts = pts(al$delta[i]))
    else .item("radar", 1, NA, TRUE, empresa = al$empresa[i], moneda = al$moneda[i])
  }

  # Logro 2: this Monday's % is below every other of the last 12, with 8+ stored.
  hoy <- serie[serie$lunes == lunes, , drop = FALSE]
  for (i in seq_len(nrow(hoy))) {
    s <- serie[serie$empresa == hoy$empresa[i] & serie$moneda == hoy$moneda[i] &
                 serie$lunes <= lunes & serie$lunes > lunes - 7L * LOGRO_SEMANAS_MIN, , drop = FALSE]
    p <- .pct(s$saldo_vencido, s$saldo_abierto)
    act <- p[s$lunes == lunes]; otros <- p[s$lunes != lunes]
    if (nrow(s) >= LOGRO_SEMANAS_GUARDADAS && length(act) == 1L && !is.na(act) &&
        length(otros) && all(!is.na(otros)) && act < min(otros))
      items[[length(items) + 1L]] <- .item("logro", 2, 1, TRUE, empresa = hoy$empresa[i], moneda = hoy$moneda[i])
  }

  # Logro 3: an empresa with past due on Monday and none on Friday.
  if (nrow(hoy) && !is.null(m_hoy) && nrow(m_hoy)) {
    for (e in unique(hoy$empresa)) {
      lun <- sum(hoy$saldo_vencido[hoy$empresa == e])
      vie <- m_hoy[m_hoy$empresa == e, , drop = FALSE]
      if (lun > 0 && nrow(vie) && sum(vie$saldo_vencido) == 0)
        items[[length(items) + 1L]] <- .item("logro", 3, 1, TRUE, empresa = e)
    }
  }

  # Logro 4 and radar 7: this week's payments.
  total <- as.integer(dv$pagos_total %||% 0L); conf <- as.integer(dv$pagos_conf %||% 0L)
  if (total > 0L && conf >= total) items[[length(items) + 1L]] <- .item("logro", 4, 2, FALSE)
  if (total > conf) items[[length(items) + 1L]] <- .item("radar", 7, NA, FALSE, pagos = total - conf)

  # Logro 6: every expected collection came in.
  if (!is.null(dv$cobros_pct) && !is.na(dv$cobros_pct) && dv$cobros_pct >= 100)
    items[[length(items) + 1L]] <- .item("logro", 6, 2, TRUE)

  # Logro 14: three consecutive weekly falls, client level (none has to be strong).
  p4 <- .pct_cliente_semanas(serie, lunes, 4L, base)
  if (all(!is.na(p4)) && all(diff(p4) < 0))
    items[[length(items) + 1L]] <- .item("logro", 14, 3, TRUE)

  # Radar 3 and 6: next week's rows, base currency.
  sig <- .avisos_semana(df, lunes + 7L)
  sig <- sig[toupper(trimws(sig$Moneda)) %in% base, , drop = FALSE]
  cob <- sum(.avisos_monto(sig[sig$Tipo %in% "AR", , drop = FALSE]))
  ap  <- sig[sig$Tipo %in% "AP", , drop = FALSE]
  pag <- sum(.avisos_monto(ap))
  if (cob > 0 && pag / cob >= RADAR_RAZON_MIN)
    items[[length(items) + 1L]] <- .item("radar", 3, NA, TRUE, razon = sprintf("%.1f", pag / cob))
  if (pag > 0) {
    parte <- ifelse(is.na(ap$Parte) | !nzchar(trimws(ap$Parte)), ap$Documento, ap$Parte)
    mayor <- max(tapply(.avisos_monto(ap), parte, sum))
    if (100 * mayor / pag >= RADAR_CONCENTRACION)
      items[[length(items) + 1L]] <- .item("radar", 6, NA, FALSE, pct = round(100 * mayor / pag))
  }

  # Radar 9: SAP data over 2 business days old on Friday.
  if (!is.null(dv$datos_sap)) {
    dias <- .dias_habiles(as.Date(dv$datos_sap, tz = UMBRAL_TZ), viernes)
    if (dias > RADAR_SAP_DIAS_HABILES) items[[length(items) + 1L]] <- .item("radar", 9, NA, FALSE, dias = dias)
  }
  items
}

# The lines of one kind for one recipient, in their language: no CxC line
# for a recipient without CxC; logros by priority, then catalog order;
# radar by catalog order; at most `max`.
correo_items_texto <- function(items, tipo, dest, max) {
  idioma <- dest$idioma %||% "es"; cxc <- isTRUE(dest$ver_cxc)
  x <- Filter(function(it) identical(it$tipo, tipo) && (cxc || !isTRUE(it$cxc)), items %||% list())
  if (!length(x)) return(character())
  pr <- vapply(x, function(it) if (is.na(it$prioridad)) 9L else it$prioridad, integer(1))
  n  <- vapply(x, function(it) it$n, integer(1))
  x  <- utils::head(x[order(pr, n)], max)
  vapply(x, function(it) do.call(tr, c(list(paste0("avisos.correo.", tipo, ".", it$n), idioma = idioma), it$params)),
         character(1))
}
