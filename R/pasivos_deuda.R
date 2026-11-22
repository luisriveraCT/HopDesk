# =============================================================================
# R/pasivos_deuda.R — "Costo de la deuda" (T78; spec: the Coordinador's, in
# hopdesk_coord/PRIORIDADES_20261119.md §T78, approved paso 0 T78_paso0.md)
#
# Only the debt that charges interest, in two groups apart: financiamientos
# (financiero, but not arrendamiento puro: it charges no interest) and
# tarjetas. Regular payments are not debt. By currency, never adding
# currencies; taxes (IVA) never inside a rate or a cost.
#   - The rate a user captures is already EFFECTIVE annual (the engine does
#     r = (1 + t)^(1/m) - 1): EAR = the captured rate (+ spread for a variable
#     one); the nominal rate compounded m times a year is derived,
#     m * ((1 + EAR)^(1/m) - 1).
#   - Costo efectivo = XIRR with the real dates: + the principal on its start
#     date, - the opening fees, - every scheduled payment (capital + interest
#     + fees, no taxes). Without fees it equals the EAR; with them it goes up.
#   - A tarjeta has no rate nor capital balance in HopDesk: its debt is what
#     its pending provisions owe; rate and costs "—", out of the weighted
#     cost, interest 0.
# Pure functions first (pasivos_deuda*()), then the sub-tab's module. Nothing
# at the top level calls another file (regla 29).
# =============================================================================

# The group of a liability, or NA when it is not interest-bearing debt.
pasivos_deuda_grupo <- function(l) {
  cat <- as.character(l$categoria[1] %||% "")
  if (identical(cat, "tarjeta")) return("tarjeta")
  if (identical(cat, "financiero") && !identical(as.character(l$flavor[1] %||% ""), "arrendamiento_puro")) return("financiamiento")
  NA_character_
}

# One liability that can't be worked out (incomplete terms, a missing
# function) is left out and says so in the log, never silently.
.deuda_intento <- function(expr, l) tryCatch(expr, error = function(e) {
  message("[pasivos_deuda] ", as.character(l$id[1] %||% "?"), ": ", conditionMessage(e))
  NULL
})

# XIRR: the effective annual rate r with sum(flujo / (1 + r)^(dias/365)) = 0.
# NA when the flows have no sign change or no root is found.
.deuda_xirr <- function(fechas, flujos) {
  ok <- !is.na(fechas) & !is.na(flujos) & flujos != 0
  fechas <- as.Date(fechas[ok]); flujos <- flujos[ok]
  if (length(flujos) < 2L || !(any(flujos > 0) && any(flujos < 0))) return(NA_real_)
  t <- as.numeric(fechas - min(fechas)) / 365
  vpn <- function(r) sum(flujos / (1 + r)^t)
  r <- tryCatch(stats::uniroot(vpn, c(-0.99, 10), tol = 1e-10)$root, error = function(e) NA_real_)
  r
}

# One liability's row: data.frame(id, nombre, acreedor, grupo, tipo, moneda,
# saldo, ear, nominal, m, costo_efectivo, plazo_meses, prox_fecha, prox_monto,
# int12, cap12, impuestos12). NULL when it is not interest-bearing debt or owes
# nothing.
pasivos_deuda_pasivo <- function(l, provs = NULL, hoy = hoy_en()) {
  hoy <- as.Date(hoy)
  grupo <- pasivos_deuda_grupo(l)
  if (is.na(grupo)) return(NULL)
  un_ano <- seq(hoy, by = "1 year", length.out = 2L)[2]
  base <- data.frame(id = as.character(l$id[1]), nombre = as.character(l$nombre[1] %||% ""),
                     acreedor = as.character(l$parte[1] %||% ""), grupo = grupo,
                     tipo = as.character(if (identical(grupo, "tarjeta")) "tarjeta" else l$flavor[1] %||% ""),
                     stringsAsFactors = FALSE)
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

  if (identical(grupo, "tarjeta")) {
    p <- if (is.data.frame(provs) && nrow(provs))
      provs[!is.na(provs$liability_id) & provs$liability_id == l$id[1] & provs$estado %in% "provisional", , drop = FALSE] else NULL
    if (is.null(p) || !nrow(p)) return(NULL)
    monto <- ifelse(!is.na(p$amount_pago_override), p$amount_pago_override, p$amount_pago)
    f <- as.Date(ifelse(!is.na(p$fecha_efectiva_override), as.character(p$fecha_efectiva_override), as.character(p$fecha_efectiva)))
    m <- as.character(p$moneda_pago); m[is.na(m)] <- as.character(l$moneda_pago[1] %||% NA_character_)
    out <- lapply(sort(unique(m)), function(mo) {
      k <- m == mo; fk <- f[k]; mk <- monto[k]
      prox <- if (any(fk >= hoy, na.rm = TRUE)) which(fk == min(fk[fk >= hoy], na.rm = TRUE))[1] else NA_integer_
      cbind(base, data.frame(moneda = mo, saldo = sum(num(mk)), ear = NA_real_, nominal = NA_real_, m = NA_integer_,
        costo_efectivo = NA_real_, plazo_meses = NA_real_,
        prox_fecha = if (is.na(prox)) as.Date(NA) else fk[prox], prox_monto = if (is.na(prox)) NA_real_ else mk[prox],
        int12 = 0, cap12 = sum(num(mk[fk < un_ano])), impuestos12 = 0, stringsAsFactors = FALSE))
    })
    return(do.call(rbind, out))
  }

  # financiamiento
  s <- pasivos_generate_schedule(l, today = hoy)
  if (!nrow(s)) return(NULL)
  mo <- as.character(l$cotizado_en[1] %||% NA_character_)
  if (is.na(mo) || !nzchar(mo)) mo <- as.character(l$moneda_pago[1] %||% NA_character_)
  sm <- if ("moneda" %in% names(s)) as.character(s$moneda) else rep(NA_character_, nrow(s))
  sm[is.na(sm) | !nzchar(sm)] <- mo
  pagos <- s$origin %in% c("regular", "residual")
  fut <- pagos & !s$historico
  if (!any(fut)) return(NULL)
  f <- as.Date(s$fecha)
  saldo <- sum(num(s$capital[fut]))
  m <- .periods_per_year(as.character(l$frecuencia_pago[1] %||% "mensual"))
  ear <- suppressWarnings(as.numeric(l$tasa_actual[1]))
  if (!startsWith(as.character(l$tasa_tipo[1] %||% "fija"), "fija"))
    ear <- ear + num(l$tasa_spread[1]) / 100
  nominal <- if (is.na(ear)) NA_real_ else m * ((1 + ear / 100)^(1 / m) - 1) * 100
  en12 <- fut & f < un_ano
  imp <- if ("impuestos" %in% names(s)) num(s$impuestos) else rep(0, nrow(s))
  # XIRR in the schedule's currency: + principal on the start date, - opening
  # fees in that currency, - every payment (no taxes).
  fees <- s$origin %in% "initial_fee" & sm == mo
  flujos <- c(num(l$principal_original[1]), -num(s$fees[fees]), -num(s$total[pagos]))
  fechas <- c(as.Date(l$fecha_inicio[1]), f[fees], f[pagos])
  xirr <- .deuda_xirr(fechas, flujos)
  primero <- which(fut)[order(f[fut])][1]
  cbind(base, data.frame(moneda = mo, saldo = saldo, ear = ear, nominal = nominal, m = m,
    costo_efectivo = if (is.na(xirr)) NA_real_ else xirr * 100,
    plazo_meses = round(as.numeric(max(f[fut]) - hoy) / 30.4375),
    prox_fecha = f[primero], prox_monto = num(s$total[primero]),
    int12 = sum(num(s$interes[en12])), cap12 = sum(num(s$capital[en12])), impuestos12 = sum(imp[en12]),
    stringsAsFactors = FALSE))
}

# Every liability's row, the summary per currency and the next 12 months'
# debt service. liabs/provs: the session's tables.
pasivos_deuda <- function(liabs, provs = NULL, hoy = hoy_en()) {
  hoy <- as.Date(hoy)
  vacio_p <- data.frame(id = character(), nombre = character(), acreedor = character(), grupo = character(),
    tipo = character(), moneda = character(), saldo = numeric(), ear = numeric(), nominal = numeric(), m = integer(),
    costo_efectivo = numeric(), plazo_meses = numeric(), prox_fecha = as.Date(character()), prox_monto = numeric(),
    int12 = numeric(), cap12 = numeric(), impuestos12 = numeric(), stringsAsFactors = FALSE)
  filas <- if (is.data.frame(liabs) && nrow(liabs)) {
    vivos <- liabs[liabs$estado %in% "active", , drop = FALSE]
    l <- lapply(seq_len(nrow(vivos)), function(i)
      .deuda_intento(pasivos_deuda_pasivo(vivos[i, , drop = FALSE], provs, hoy), vivos[i, , drop = FALSE]))
    l <- Filter(Negate(is.null), l)
    if (length(l)) do.call(rbind, l) else vacio_p
  } else vacio_p
  list(pasivos = filas, resumen = pasivos_deuda_resumen(filas), servicio = pasivos_deuda_servicio(liabs, provs, hoy),
       regulares = pasivos_deuda_regulares(liabs, hoy))
}

# Regular payments are not debt: only their cost per year by currency (T77's
# pasivos_costeo), for one line below. anual NA = unknown (a payment with no
# amount yet), never 0.
pasivos_deuda_regulares <- function(liabs, hoy = hoy_en()) {
  vacio <- data.frame(moneda = character(), anual = numeric(), stringsAsFactors = FALSE)
  if (!is.data.frame(liabs) || !nrow(liabs)) return(vacio)
  k <- which(liabs$estado %in% "active" & liabs$categoria %in% "regular")
  f <- lapply(k, function(i) .deuda_intento(pasivos_costeo(liabs[i, , drop = FALSE], hoy)$filas, liabs[i, , drop = FALSE]))
  f <- do.call(rbind, Filter(function(x) is.data.frame(x) && nrow(x), f))
  if (is.null(f)) return(vacio)
  do.call(rbind, lapply(sort(unique(f$moneda)), function(mo) {
    a <- f$anual[f$moneda == mo]
    data.frame(moneda = mo, anual = if (any(is.na(a))) NA_real_ else sum(a), stringsAsFactors = FALSE)
  }))
}

# Per currency: total debt (and per group), the cost weighted by balance
# (only liabilities with a rate), interest of the next 12 months and the share
# of the debt due in them.
pasivos_deuda_resumen <- function(filas) {
  if (!nrow(filas)) return(data.frame(moneda = character(), deuda = numeric(), deuda_fin = numeric(), deuda_tar = numeric(),
    ponderado = numeric(), int12 = numeric(), pct12 = numeric(), stringsAsFactors = FALSE))
  do.call(rbind, lapply(sort(unique(filas$moneda)), function(mo) {
    f <- filas[filas$moneda == mo, , drop = FALSE]
    con <- !is.na(f$ear) & f$saldo > 0
    data.frame(moneda = mo, deuda = sum(f$saldo), deuda_fin = sum(f$saldo[f$grupo == "financiamiento"]),
      deuda_tar = sum(f$saldo[f$grupo == "tarjeta"]),
      ponderado = if (any(con)) sum(f$ear[con] * f$saldo[con]) / sum(f$saldo[con]) else NA_real_,
      int12 = sum(f$int12), pct12 = if (sum(f$saldo) > 0) 100 * sum(f$cap12) / sum(f$saldo) else NA_real_,
      stringsAsFactors = FALSE)
  }))
}

# The next 12 months' debt service, per currency: rows Capital and Interés
# (grouped by currency, the group's row is the total), columns b1..b12 by
# month and Total. Tarjetas add their pending provisions as capital.
pasivos_deuda_servicio <- function(liabs, provs = NULL, hoy = hoy_en()) {
  hoy <- as.Date(hoy)
  vacio <- data.frame(id = character(), Moneda = character(), Concepto = character(), stringsAsFactors = FALSE)
  for (j in 1:12) vacio[[paste0("b", j)]] <- numeric()
  vacio$Total <- numeric()
  if (length(hoy) != 1L || is.na(hoy)) return(list(datos = vacio, meses = as.Date(character())))
  meses <- seq(as.Date(format(hoy, "%Y-%m-01")), by = "1 month", length.out = 12L)
  fin <- seq(meses[12], by = "1 month", length.out = 2L)[2]
  mov <- list()
  if (is.data.frame(liabs) && nrow(liabs)) for (i in which(liabs$estado %in% "active")) {
    l <- liabs[i, , drop = FALSE]; g <- pasivos_deuda_grupo(l)
    if (is.na(g)) next
    if (identical(g, "financiamiento")) {
      s <- .deuda_intento(pasivos_generate_schedule(l, today = hoy), l)
      if (is.null(s) || !nrow(s)) next
      mo <- as.character(l$cotizado_en[1] %||% NA_character_)
      if (is.na(mo) || !nzchar(mo)) mo <- as.character(l$moneda_pago[1] %||% NA_character_)
      k <- s$origin %in% c("regular", "residual") & !s$historico & as.Date(s$fecha) >= hoy & as.Date(s$fecha) < fin
      if (any(k)) mov[[length(mov) + 1L]] <- data.frame(moneda = mo, fecha = as.Date(s$fecha[k]),
        capital = as.numeric(s$capital[k]), interes = as.numeric(s$interes[k]), stringsAsFactors = FALSE)
    } else if (is.data.frame(provs) && nrow(provs)) {
      p <- provs[!is.na(provs$liability_id) & provs$liability_id == l$id[1] & provs$estado %in% "provisional", , drop = FALSE]
      if (!nrow(p)) next
      f <- as.Date(ifelse(!is.na(p$fecha_efectiva_override), as.character(p$fecha_efectiva_override), as.character(p$fecha_efectiva)))
      monto <- ifelse(!is.na(p$amount_pago_override), p$amount_pago_override, p$amount_pago)
      k <- !is.na(f) & f < fin   # overdue ones fall in the first month
      if (any(k)) mov[[length(mov) + 1L]] <- data.frame(moneda = as.character(p$moneda_pago[k]),
        fecha = pmax(f[k], meses[1]), capital = as.numeric(monto[k]), interes = 0, stringsAsFactors = FALSE)
    }
  }
  if (!length(mov)) return(list(datos = vacio, meses = meses))
  mv <- do.call(rbind, mov)
  mv$b <- findInterval(mv$fecha, meses)
  filas <- list()
  for (mo in sort(unique(mv$moneda))) for (con in c("capital", "interes")) {
    x <- mv[mv$moneda == mo, , drop = FALSE]
    v <- vapply(1:12, function(j) sum(x[[con]][x$b == j], na.rm = TRUE), numeric(1))
    fila <- data.frame(id = paste(mo, con, sep = "|"), Moneda = mo,
                       Concepto = tr(paste0("tablas.pasivos.deuda.concepto_", con)), stringsAsFactors = FALSE)
    for (j in 1:12) fila[[paste0("b", j)]] <- v[j]
    fila$Total <- sum(v)
    filas[[length(filas) + 1L]] <- fila
  }
  list(datos = do.call(rbind, filas), meses = meses)
}


# The currencies present, the base first, then by how many rows (T39, rule 2).
.deuda_monedas <- function(monedas, base = NA_character_) {
  intersect(monedas_en_datos(monedas, base), unique(monedas))
}

# ── The sub-tab ────────────────────────────────────────────────────────────────
pasivos_deuda_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pasivos-deuda p-2",
    shiny::uiOutput(ns("tarjetas")),
    shiny::div(class = "fw-semibold mt-3 mb-1", tr("tablas.pasivos.deuda.titulo_tabla")),
    hd_table_ui(ns("tabla")),
    shiny::div(class = "fw-semibold mt-3 mb-1", tr("tablas.pasivos.deuda.titulo_servicio")),
    hd_table_ui(ns("servicio")),
    shiny::uiOutput(ns("pie")))
}

# base: a reactive with the client's base currency (its currency first).
pasivos_deuda_server <- function(id, shared, base = function() MONEDA_BASE_OMISION) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (is.data.frame(liabs) && !is.null(allowed) && "empresa" %in% names(liabs))
        liabs <- liabs[liabs$empresa %in% allowed, , drop = FALSE]
      # A failure leaves the panel empty and says why (never the session:
      # Pasivos' other sub-tabs keep working).
      tryCatch(pasivos_deuda(liabs, provs, hoy_en()), error = function(e) {
        message("[pasivos_deuda] ", conditionMessage(e))
        pasivos_deuda(NULL, NULL, as.Date(NA))
      })
    })
    pct <- function(x) if (is.na(x)) "—" else paste0(formatC(x, format = "f", digits = 2), " %")

    output$tarjetas <- shiny::renderUI({
      r <- datos()$resumen
      if (!nrow(r)) return(shiny::div(class = "text-muted small", tr("tablas.pasivos.deuda.sin_deuda")))
      r <- r[match(.deuda_monedas(r$moneda, base()), r$moneda), , drop = FALSE]
      dato <- function(clave, valor) shiny::div(class = "me-4 mb-1",
        shiny::div(class = "text-muted small", tr(clave)), shiny::div(class = "fw-semibold", valor))
      shiny::div(class = "d-flex flex-wrap gap-2",
        lapply(seq_len(nrow(r)), function(i) {
          mo <- r$moneda[i]
          shiny::div(class = "card p-2 pasivos-deuda-tarjeta",
            shiny::div(class = "fw-semibold mb-1", mo),
            shiny::div(class = "d-flex flex-wrap",
              dato("tablas.pasivos.deuda.deuda_total", monto_moneda(r$deuda[i], mo)),
              dato("tablas.pasivos.deuda.ponderado", pct(r$ponderado[i])),
              dato("tablas.pasivos.deuda.int12", monto_moneda(r$int12[i], mo)),
              dato("tablas.pasivos.deuda.pct12", pct(r$pct12[i]))),
            shiny::div(class = "text-muted small",
              tr("tablas.pasivos.deuda.por_grupo", fin = monto_moneda(r$deuda_fin[i], mo),
                 tar = monto_moneda(r$deuda_tar[i], mo))))
        }))
    })

    filas_tabla <- shiny::reactive({
      p <- datos()$pasivos
      tipo <- vapply(p$tipo, function(t) {
        clave <- if (identical(t, "tarjeta")) "tablas.pasivos.cat.tarjeta" else paste0("tablas.pasivos.flavor.", t)
        if (is.null(i18n_dict("es")[[clave]])) tr("tablas.pasivos.flavor.otro") else tr(clave)
      }, character(1), USE.NAMES = FALSE)
      data.frame(id = paste(p$id, p$moneda, sep = "|"),
        Acreedor = ifelse(nzchar(p$acreedor), p$acreedor, p$nombre), Pasivo = p$nombre,
        Grupo = vapply(p$grupo, function(g) tr(paste0("tablas.pasivos.deuda.grupo_", g)), character(1), USE.NAMES = FALSE),
        Tipo = tipo, Moneda = p$moneda, Saldo = p$saldo, Nominal = p$nominal, EAR = p$ear, Efectivo = p$costo_efectivo,
        Plazo = p$plazo_meses, ProxFecha = p$prox_fecha, ProxMonto = p$prox_monto, Int12 = p$int12,
        stringsAsFactors = FALSE)
    })
    # Nominal and Plazo are optional (shown from the columns menu): with them
    # the table doesn't fit Luis's 1293 px window (T78, measured in Chrome).
    hd_table_server("tabla", tabla = "deuda_pasivos", datos = filas_tabla, piezas = "columnas_opc",
      secciones = list(list(campo = "Moneda", orden = function() base()), list(campo = "Grupo")),
      columnas = list(
        col_texto("Acreedor", "tablas.pasivos.deuda.col.acreedor"),
        col_texto("Pasivo", "tablas.pasivos.deuda.col.pasivo"),
        col_texto("Tipo", "tablas.pasivos.deuda.col.tipo"),
        col_dinero("Saldo", "tablas.pasivos.deuda.col.saldo", moneda = "Moneda"),
        col_numero("Nominal", "tablas.pasivos.deuda.col.nominal", decimales = 2L, opcional = TRUE, visible = FALSE),
        col_numero("EAR", "tablas.pasivos.deuda.col.ear", decimales = 2L),
        col_numero("Efectivo", "tablas.pasivos.deuda.col.efectivo", decimales = 2L),
        col_numero("Plazo", "tablas.pasivos.deuda.col.plazo", opcional = TRUE, visible = FALSE),
        col_fecha("ProxFecha", "tablas.pasivos.deuda.col.prox_fecha"),
        col_dinero("ProxMonto", "tablas.pasivos.deuda.col.prox_monto", moneda = "Moneda", sumar = FALSE),
        col_dinero("Int12", "tablas.pasivos.deuda.col.int12", moneda = "Moneda")),
      vacio = "tablas.pasivos.deuda.sin_deuda")

    servicio <- shiny::reactive({
      s <- datos()$servicio
      d <- s$datos
      if (nrow(d)) d <- d[order(match(d$Moneda, .deuda_monedas(d$Moneda, base()))), , drop = FALSE]
      list(datos = d, meses = s$meses)
    })
    hd_table_server("servicio", tabla = "servicio_deuda", datos = shiny::reactive(servicio()$datos),
      piezas = c("sumas_columnas", "dinero_compacto", "expandir"),
      grupo = "Moneda",
      columnas = shiny::reactive({
        meses <- servicio()$meses
        etiqueta <- function(d) paste(tr(paste0("comun.mes.", as.integer(format(d, "%m")))), format(d, "%Y"))
        c(list(col_texto("Concepto", "tablas.pasivos.deuda.col.concepto", fija = TRUE)),
          lapply(seq_along(meses), function(j)
            col_dinero(paste0("b", j), etiqueta(meses[j]), moneda = "Moneda", formato = "compacto", cero = "vacio")),
          list(col_dinero("Total", "tablas.flujo.total", moneda = "Moneda", formato = "entero", cero = "vacio")))
      }),
      vacio = "tablas.pasivos.deuda.sin_deuda")

    output$pie <- shiny::renderUI({
      p <- datos()$pasivos
      shiny::div(class = "text-muted small mt-2",
        shiny::div(tr("tablas.pasivos.deuda.nota_tasa")),
        if (any(p$grupo == "tarjeta")) shiny::div(tr("tablas.pasivos.deuda.nota_tarjetas")),
        shiny::div(tr("tablas.pasivos.deuda.nota_impuestos")),
        {
          rg <- datos()$regulares
          if (nrow(rg)) {
            rg <- rg[match(.deuda_monedas(rg$moneda, base()), rg$moneda), , drop = FALSE]
            montos <- ifelse(is.na(rg$anual), paste(rg$moneda, tr("tablas.pasivos.deuda.desconocido")),
                             vapply(seq_len(nrow(rg)), function(i) monto_moneda(rg$anual[i], rg$moneda[i]), character(1)))
            shiny::div(tr("tablas.pasivos.deuda.nota_regulares", montos = paste(montos, collapse = " · ")))
          }
        })
    })
  })
}
