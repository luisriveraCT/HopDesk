# =============================================================================
# R/panorama_module.R
# Panorama (front B1): the home screen. Saludo, SAP status, Clima de hoy,
# Destacados, Hoy (payments, collections, liabilities), Mis solicitudes,
# Lo que ha pasado (with the bell), Tu equipo hoy and La Cinta.
#
# Everything it shows comes from R/panorama_datos.R (pure) over data the
# session already holds, plus avisos_visibles() -- filtered on the server, so
# each user only ever receives the avisos meant for their tier, permissions
# and name. Styles: www/hd_marca.css (brand tokens, root class .hd-marca) and
# www/panorama.css (root class .hd-panorama); nothing leaks to other tabs.
#
# panoramaServer() arguments:
#   activa   reactive TRUE while the Panorama tab is open: S3 is read only then
#   ir_a     function(tab) that opens a navbar tab (app.R owns the navbar)
#   tz       reactive with the browser's time zone
# shared$fx_fix(): the Banxico FIX for La Cinta -- NULL until its connector
# exists (separate task), and then NULL whenever there is no figure: nothing
# is shown rather than a wrong number.
# =============================================================================

PANORAMA_REFRESCO_MS <- 60000L

panoramaUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "hd-marca hd-panorama", id = ns("raiz"),
    # Publicar (B2): its own view in the same tab; the server shows it only to
    # can_publish_panorama.
    shiny::div(id = ns("vista_publicar"), class = "pan-pagina", style = "display:none",
      panoramaPublicarUI(ns("pub"))),
    shiny::div(class = "pan-pagina", id = ns("vista_inicio"),
      shiny::uiOutput(ns("encabezado")),
      shiny::uiOutput(ns("sap_franja")),
      shiny::div(class = "pan-rejilla",
        shiny::div(class = "pan-col-a",
          shiny::uiOutput(ns("clima")),
          shiny::uiOutput(ns("destacados"))),
        shiny::div(class = "pan-col-b",
          shiny::uiOutput(ns("hoy")),
          shiny::uiOutput(ns("solicitudes")))),
      shiny::div(class = "pan-rejilla pan-rejilla--feed",
        shiny::div(class = "pan-col-a", id = ns("feed_ancla"), shiny::uiOutput(ns("feed"))),
        shiny::div(class = "pan-col-b", shiny::uiOutput(ns("equipo"))))),
    shiny::uiOutput(ns("cinta")))
}

panoramaServer <- function(id, shared, activa = shiny::reactive(TRUE), ir_a = function(tab) NULL,
                           tz = shiny::reactive("America/Mexico_City")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cid <- shiny::reactive(tryCatch(shared$effective_client_id(), error = function(e) NULL))
    usr <- shiny::reactive(tryCatch(shared$usuario(), error = function(e) NULL))
    zona <- shiny::reactive(tz() %||% "America/Mexico_City")
    hoy  <- shiny::reactive({ reloj(); as.Date(Sys.time(), tz = zona()) })
    reloj <- shiny::reactiveTimer(PANORAMA_REFRESCO_MS)
    version <- shiny::reactiveVal(0L)

    # ── Avisos (S3 only while the tab is open) ────────────────────────────────
    visibles <- shiny::reactive({
      if (!isTRUE(activa())) return(NULL)
      reloj(); version()
      c <- cid(); u <- usr()
      if (is.null(c) || !nzchar(c) || is.null(u)) return(NULL)
      tryCatch(avisos_visibles(c, u), error = function(e) {
        message("[panorama] avisos_visibles falló: ", conditionMessage(e)); NULL
      })
    })

    # Last visit: read once per session, the first time the tab opens, and
    # moved to now at that point. "New" in this session = since that visit.
    # Staff inside a client never write into the client's preferences.
    visita <- shiny::reactiveVal(NULL)
    visita_leida <- FALSE
    shiny::observe({
      if (!isTRUE(activa()) || visita_leida) return()
      c <- cid(); u <- usr()
      if (is.null(c) || !nzchar(c) || is.null(u) || !nzchar(u$user %||% "")) return()
      visita_leida <<- TRUE
      p <- tryCatch(prefs_leer(c, u$user), error = function(e) NULL)
      visita(p$ultima_visita)
      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      if (!isTRUE(info$is_staff))
        tryCatch(prefs_guardar(c, u$user, list(ultima_visita = format(Sys.time(), tz = "UTC", usetz = TRUE))),
                 error = function(e) message("[panorama] prefs_guardar falló: ", conditionMessage(e)))
    })

    feed <- shiny::reactive({
      v <- visibles(); if (is.null(v)) return(panorama_feed(NULL))
      panorama_feed(v, visita())
    })
    mis_sol <- shiny::reactive({
      v <- visibles(); u <- usr()
      if (is.null(v) || is.null(u)) return(NULL)
      panorama_mis_solicitudes(v, u$user)
    })

    # ── Data already in the session ───────────────────────────────────────────
    pagos  <- shiny::reactive(panorama_pagos_hoy(tryCatch(shared$pagar_hoy_db(), error = function(e) NULL), hoy()))
    ver_cxc <- shiny::reactive(is.null(nivel_calendario(tryCatch(shared$tier_efectivo(), error = function(e) NULL))))
    cobros <- shiny::reactive({
      if (!ver_cxc()) return(NULL)
      f <- shared$df_combined_AR
      panorama_cobros_hoy(if (is.function(f)) tryCatch(f(), error = function(e) NULL) else NULL, hoy())
    })
    pasivos <- shiny::reactive(panorama_pasivos_pronto(
      tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL), hoy()))
    sap <- shiny::reactive({ reloj(); panorama_sap_estado(tryCatch(shared$sap_snapshot_info(), error = function(e) NULL)) })

    # ── Encabezado ────────────────────────────────────────────────────────────
    output$encabezado <- shiny::renderUI({
      u <- usr(); info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      nombre <- strsplit(trimws(info$name %||% u$user %||% ""), "\\s+")[[1]][1] %||% ""
      ahora <- Sys.time(); reloj()
      s <- sap()
      linea_sap <- if (isTRUE(s$cargado))
        tr("panorama.sap.al", fecha = .pan_fecha_hora(s$ts, zona())) else tr("panorama.sap.sin_datos")
      n_nuevos <- sum(feed()$nuevo)
      boton_publicar <- if (puede_pub()) shiny::tags$button(
        type = "button", class = "pan-boton",
        onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("abrir_publicar")),
        shiny::icon("pen-to-square"), " ", tr("panorama.publicar.abrir"))
      shiny::div(class = "pan-encabezado",
        shiny::div(
          shiny::div(class = "pan-kicker", tr("panorama.kicker", fecha = panorama_fecha_larga(hoy()))),
          shiny::h1(class = "pan-saludo",
                    tr(panorama_saludo(format(ahora, "%H", tz = zona()))), shiny::tags$br(),
                    shiny::tags$em(nombre)),
          shiny::div(class = "pan-sap-linea",
            shiny::tags$span(class = paste("pan-punto", if (isTRUE(s$viejo)) "pan-punto--viejo")),
            linea_sap)),
        shiny::div(class = "pan-encabezado-acciones",
          boton_publicar,
          shiny::tags$button(
            type = "button", class = "pan-campana", id = ns("campana"),
            `aria-label` = tr("panorama.campana.ayuda", n = as.character(n_nuevos)),
            onclick = sprintf("hdPanorama.irA('%s')", ns("feed_ancla")),
            shiny::icon("bell"),
            if (n_nuevos > 0) shiny::tags$span(class = "pan-campana-n", n_nuevos))))
    })

    # ── Publicar (B2) ─────────────────────────────────────────────────────────
    puede_pub <- shiny::reactive(isTRUE(tryCatch(puede(usr(), "can_publish_panorama"), error = function(e) FALSE)))
    modo <- shiny::reactiveVal("inicio")
    shiny::observeEvent(input$abrir_publicar, {
      if (!puede_o_avisa(shared, "can_publish_panorama")) return()
      modo("publicar")
    }, ignoreInit = TRUE)
    # Losing the permission (or a staff "ver como") closes it.
    shiny::observe({ if (!puede_pub() && shiny::isolate(modo()) == "publicar") modo("inicio") })
    shiny::observe({
      pub <- identical(modo(), "publicar")
      shinyjs::toggle("vista_publicar", condition = pub)
      shinyjs::toggle("vista_inicio", condition = !pub)
    })
    panoramaPublicarServer("pub", shared,
      abierta     = shiny::reactive(isTRUE(activa()) && identical(modo(), "publicar") && puede_pub()),
      al_volver   = function() modo("inicio"),
      al_publicar = function() version(version() + 1L),
      tz          = zona)

    output$sap_franja <- shiny::renderUI({
      s <- sap()
      if (!isTRUE(s$cargado) || !isTRUE(s$viejo)) return(NULL)
      shiny::div(class = "pan-franja", role = "status",
        shiny::div(shiny::tags$strong(tr("panorama.sap.viejo_titulo")), " ",
                   tr("panorama.sap.viejo_texto", fecha = .pan_fecha_hora(s$ts, zona()))),
        shiny::tags$button(type = "button", class = "pan-boton pan-boton--claro",
                           onclick = "hdPanorama.refrescarSap()", tr("panorama.sap.refrescar")))
    })

    # ── Clima de hoy ──────────────────────────────────────────────────────────
    output$clima <- shiny::renderUI({
      f <- feed(); ms <- mis_sol(); p <- pagos()
      cl <- panorama_clima(f, n_solicitudes = if (is.null(ms)) 0L else nrow(ms),
                           n_pagos_hoy = sum(p$n), sap = sap())
      icono <- switch(cl$clima, despejado = "sun", medio = "cloud-sun", nublado = "cloud", neblina = "smog")
      cuenta <- if (cl$clima == "neblina") tr("panorama.clima.cuenta_sap") else
        tr_n("panorama.clima.pendientes", cl$pendientes,
             urgentes = if (cl$urgentes > 0) tr_n("panorama.clima.urgentes", cl$urgentes) else tr("panorama.clima.ninguno_urgente"))
      shiny::div(class = paste0("pan-tarjeta pan-clima pan-clima--", cl$clima),
        shiny::div(class = "pan-etiqueta", tr("panorama.clima.titulo")),
        shiny::div(class = "pan-clima-fila",
          shiny::span(class = "pan-clima-icono", `aria-hidden` = "true", shiny::icon(icono)),
          shiny::div(
            shiny::div(class = "pan-clima-nombre", tr(paste0("panorama.clima.", cl$clima))),
            shiny::div(class = "pan-clima-cuenta", cuenta))),
        shiny::p(class = "pan-clima-linea", tr(paste0("panorama.clima.linea.", cl$clima))),
        shiny::tags$ul(class = "pan-clima-razones",
          lapply(cl$razones, function(r) shiny::tags$li(do.call(tr, c(list(r$clave), r$params))))))
    })

    # ── Destacados ────────────────────────────────────────────────────────────
    output$destacados <- shiny::renderUI({
      v <- visibles(); d <- if (is.null(v)) NULL else panorama_destacados(v)
      if (is.null(d) || !nrow(d)) {
        return(shiny::div(class = "pan-tarjeta pan-destacado pan-tono-salvia pan-destacado--vacio",
          shiny::div(class = "pan-etiqueta", tr("panorama.destacados.titulo_vacio")),
          shiny::h2(class = "pan-destacado-titulo", tr("panorama.destacados.vacio_titulo")),
          shiny::p(tr("panorama.destacados.vacio_texto"))))
      }
      n <- nrow(d)
      shiny::div(class = "pan-carrusel", `data-total` = n,
        lapply(seq_len(n), function(i) {
          a <- d[i, ]
          tono <- switch(a$clase, recordatorio = "ambar", reconocimiento = "lila", "salvia")
          shiny::tags$article(
            class = paste("pan-tarjeta pan-destacado", paste0("pan-tono-", tono), if (i == 1) "activa"),
            `aria-roledescription` = "slide",
            `aria-label` = tr("panorama.destacados.n_de", i = as.character(i), n = as.character(n)),
            shiny::div(class = "pan-destacado-cabeza",
              shiny::span(class = "pan-tag", tr(paste0("panorama.clase.", a$clase))),
              shiny::span(class = "pan-etiqueta", tr("panorama.destacados.contador", i = as.character(i), n = as.character(n)))),
            shiny::h2(class = "pan-destacado-titulo", a$titulo),
            if (nzchar(a$cuerpo %||% "")) shiny::p(class = "pan-destacado-cuerpo", a$cuerpo),
            shiny::div(class = "pan-destacado-pie",
              shiny::span(class = "pan-autor",
                tr("panorama.destacados.autor", autor = a$creado_por %||% "",
                   cuando = panorama_cuando(a$creado, tz = zona()))),
              if (!is.na(a$enlace) && nzchar(a$enlace))
                shiny::tags$button(type = "button", class = "pan-boton",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                    ns("ir"), htmltools::htmlEscape(a$enlace, attribute = TRUE)),
                  tr("panorama.abrir"))))
        }),
        if (n > 1) shiny::div(class = "pan-puntos",
          lapply(seq_len(n), function(i) shiny::tags$button(
            type = "button", class = paste("pan-punto-nav", if (i == 1) "activo"), `data-i` = i - 1L,
            `aria-label` = tr("panorama.destacados.ver", i = as.character(i))))))
    })

    # ── Hoy ───────────────────────────────────────────────────────────────────
    output$hoy <- shiny::renderUI({
      pp <- pasivos()
      tarjetas <- list(
        .pan_tarjeta_hoy(tr("panorama.hoy.pagos"), pagos(),
          tr_n("panorama.hoy.pagos_n", sum(pagos()$n)), "PH", ns),
        if (ver_cxc()) .pan_tarjeta_hoy(tr("panorama.hoy.cobros"), cobros(),
          tr_n("panorama.hoy.cobros_n", sum(cobros()$n)), "CAL", ns),
        .pan_tarjeta_hoy(tr("panorama.hoy.pasivos", n = as.character(PANORAMA_DIAS_PASIVOS)), pp$totales,
          if (nrow(pp$totales))
            tr_n("panorama.hoy.pasivos_n", sum(pp$totales$n),
                 fecha = paste(tr(paste0("panorama.dia.", as.integer(format(pp$primera, "%u")))),
                               as.integer(format(pp$primera, "%d"))))
          else tr("panorama.hoy.pasivos_ninguno"), "PSV", ns))
      shiny::div(class = "pan-hoy", Filter(Negate(is.null), tarjetas))
    })

    # ── Mis solicitudes ───────────────────────────────────────────────────────
    output$solicitudes <- shiny::renderUI({
      s <- mis_sol(); if (is.null(s) || !nrow(s)) return(NULL)
      shiny::div(class = "pan-tarjeta pan-solicitudes pan-tono-lila",
        shiny::div(class = "pan-etiqueta", tr("panorama.solicitudes.titulo")),
        lapply(seq_len(min(nrow(s), 3L)), function(i) {
          a <- s[i, ]
          shiny::div(class = "pan-solicitud",
            shiny::div(
              shiny::div(class = "pan-solicitud-titulo", a$titulo),
              shiny::div(class = "pan-meta",
                if (!is.na(a$vence)) tr("panorama.solicitudes.de_vence", autor = a$creado_por %||% "",
                                        fecha = .pan_fecha_corta(a$vence, zona()))
                else tr("panorama.solicitudes.de", autor = a$creado_por %||% ""))),
            shiny::tags$button(type = "button", class = "pan-boton",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                ns("hecho"), htmltools::htmlEscape(a$id, attribute = TRUE)),
              tr("panorama.solicitudes.hecho")))
        }))
    })

    shiny::observeEvent(input$hecho, {
      c <- cid(); u <- usr(); id <- input$hecho
      if (is.null(c) || is.null(u) || !is.character(id) || !nzchar(id)) return()
      # Only one of this user's own open solicitudes: the id comes from the
      # browser, so it is checked against what the server showed them.
      s <- mis_sol()
      if (is.null(s) || !id %in% s$id) return()
      ok <- tryCatch({ avisos_marcar_hecho(c, id, u$user); TRUE }, error = function(e) FALSE)
      if (ok) version(version() + 1L)
      else shiny::showNotification(tr("panorama.solicitudes.error"), type = "error")
    }, ignoreInit = TRUE)

    # A tab a Destacado or a feed item points to. Only tab values: anything
    # else is ignored.
    shiny::observeEvent(input$ir, {
      tab <- input$ir
      if (is.character(tab) && length(tab) == 1L && grepl("^[A-Z]{2,6}$", tab)) ir_a(tab)
    }, ignoreInit = TRUE)

    # ── Lo que ha pasado ──────────────────────────────────────────────────────
    output$feed <- shiny::renderUI({
      f <- feed()
      uv <- visita()
      cuenta <- function(t) if (t == "todo") nrow(f) else sum(f$tipo_feed == t)
      chips <- c("todo", PANORAMA_TIPOS_FEED)
      shiny::div(class = "pan-tarjeta pan-feed",
        shiny::div(class = "pan-feed-cabeza",
          shiny::h2(class = "pan-titulo-seccion", tr("panorama.feed.titulo")),
          shiny::div(class = "pan-meta",
            if (!is.null(uv) && !is.na(suppressWarnings(as.POSIXct(uv, tz = "UTC"))))
              tr("panorama.feed.desde", cuando = panorama_cuando(as.POSIXct(uv, tz = "UTC"), tz = zona()))
            else tr("panorama.feed.primera_vez"))),
        shiny::div(class = "pan-chips", role = "group", `aria-label` = tr("panorama.feed.filtro"),
          lapply(chips, function(t) shiny::tags$button(
            type = "button", class = "pan-chip", `data-tipo` = t,
            `aria-pressed` = if (t == "todo") "true" else "false",
            tr(paste0("panorama.feed.chip.", t)),
            shiny::span(class = "pan-chip-n", cuenta(t))))),
        if (!nrow(f)) shiny::p(class = "pan-vacio", tr("panorama.feed.vacio"))
        else shiny::tags$ul(class = "pan-lista",
          lapply(seq_len(min(nrow(f), 30L)), function(i) {
            a <- f[i, ]
            icono <- switch(a$tipo_feed, ale = "triangle-exclamation", act = "user-check",
                            sol = "clipboard-check", "bullhorn")
            meta <- c(tr(paste0("panorama.feed.tipo.", a$tipo_feed)),
                      if (nzchar(a$cuerpo %||% "")) a$cuerpo,
                      if ((a$contador %||% 1L) > 1L) tr("panorama.feed.veces", n = as.character(a$contador)))
            enlace <- if (!is.na(a$enlace) && grepl("^[A-Z]{2,6}$", a$enlace)) a$enlace else NULL
            shiny::tags$li(class = paste("pan-item", if (isTRUE(a$nuevo)) "pan-item--nuevo"),
              `data-tipo` = a$tipo_feed,
              shiny::span(class = "pan-item-nuevo", `aria-hidden` = "true"),
              shiny::span(class = paste0("pan-item-icono pan-icono-", a$tipo_feed), shiny::icon(icono)),
              shiny::div(class = "pan-item-texto",
                if (is.null(enlace)) shiny::div(class = "pan-item-titulo", a$titulo)
                else shiny::tags$button(type = "button", class = "pan-item-titulo pan-enlace",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("ir"), enlace),
                  a$titulo),
                shiny::div(class = "pan-meta", paste(meta, collapse = " · "))),
              shiny::span(class = "pan-item-hora", panorama_cuando(a$cuando, tz = zona())),
              if (isTRUE(a$nuevo)) shiny::span(class = "visually-hidden", tr("panorama.feed.nuevo")))
          })))
    })

    # ── Tu equipo hoy ─────────────────────────────────────────────────────────
    output$equipo <- shiny::renderUI({
      v <- visibles(); e <- panorama_equipo_hoy(v, hoy(), zona())
      if (!length(e$personas)) {
        return(shiny::div(class = "pan-tarjeta pan-equipo",
          shiny::div(class = "pan-etiqueta", tr("panorama.equipo.titulo")),
          shiny::p(class = "pan-vacio", tr("panorama.equipo.vacio"))))
      }
      nombres <- e$personas
      shiny::div(class = "pan-tarjeta pan-equipo",
        shiny::div(class = "pan-etiqueta", tr("panorama.equipo.titulo")),
        shiny::div(class = "pan-avatares",
          lapply(utils::head(nombres, 4), function(n) shiny::span(class = "pan-avatar", title = n, .pan_iniciales(n)))),
        shiny::p(class = "pan-equipo-linea",
          tr_n("panorama.equipo.linea", e$acciones,
               personas = .pan_lista_nombres(nombres))))
    })

    # ── La Cinta ──────────────────────────────────────────────────────────────
    output$cinta <- shiny::renderUI({
      f <- feed()
      items <- list()
      add <- function(badge, texto, delta = NULL, tono = NULL)
        items[[length(items) + 1L]] <<- list(badge = badge, texto = texto, delta = delta, tono = tono)
      if (nrow(f)) {
        for (i in which(f$clase_ok %in% "recordatorio")[seq_len(min(1L, sum(f$clase_ok %in% "recordatorio")))])
          add(tr("panorama.clase.recordatorio"), f$titulo[i], tono = "fuerte")
        for (i in utils::head(which(f$tipo_feed == "ale"), 2L)) add(tr("panorama.cinta.alerta"), f$titulo[i])
      }
      p <- pagos(); cb <- cobros()
      if (nrow(p) || (!is.null(cb) && nrow(cb)))
        add(tr("panorama.cinta.hoy"), paste(c(
          if (nrow(p)) tr("panorama.cinta.pagos", monto = .pan_monto_corto(p)),
          if (!is.null(cb) && nrow(cb)) tr("panorama.cinta.cobros", monto = .pan_monto_corto(cb))),
          collapse = " · "))
      fx <- tryCatch(if (is.function(shared$fx_fix)) shared$fx_fix() else NULL, error = function(e) NULL)
      if (is.list(fx) && is.numeric(fx$valor) && length(fx$valor) == 1L && !is.na(fx$valor) && fx$valor > 0) {
        d <- suppressWarnings(as.numeric(fx$cambio_pct %||% NA))
        add(tr("panorama.cinta.banxico"), tr("panorama.cinta.fix", valor = formatC(fx$valor, format = "f", digits = 4)),
            delta = if (!is.na(d)) list(texto = sprintf("%s %s%%", if (d >= 0) "▲" else "▼",
                                                       formatC(abs(d), format = "f", digits = 2)),
                                        sube = d >= 0))
      }
      if (nrow(f)) {
        for (i in utils::head(which(f$tipo_feed == "act"), 2L)) add(tr("panorama.cinta.equipo"), f$titulo[i])
        for (i in utils::head(which(f$tipo_feed == "com" & f$clase_ok %in% "comunicado"), 1L))
          add(tr("panorama.clase.comunicado"), f$titulo[i])
      }
      if (!length(items)) return(NULL)
      pieza <- function(it) shiny::span(class = "pan-cinta-item",
        shiny::span(class = paste("pan-cinta-badge", if (identical(it$tono, "fuerte")) "pan-cinta-badge--fuerte"), it$badge),
        shiny::span(it$texto),
        if (!is.null(it$delta)) shiny::span(class = paste("pan-cinta-delta", if (it$delta$sube) "sube" else "baja"),
                                            it$delta$texto))
      shiny::div(class = "pan-cinta", role = "marquee", `aria-label` = tr("panorama.cinta.titulo"),
        shiny::span(class = "pan-cinta-nombre", tr("panorama.cinta.titulo")),
        shiny::div(class = "pan-cinta-ventana",
          shiny::div(class = "pan-cinta-pista",
            shiny::div(class = "pan-cinta-tramo", lapply(items, pieza)),
            # A second copy, hidden from screen readers, makes the loop seamless.
            shiny::div(class = "pan-cinta-tramo", `aria-hidden` = "true", lapply(items, pieza)))))
    })

    invisible(list(refrescar = function() version(version() + 1L)))
  })
}

# ── Helpers (drawing only) ───────────────────────────────────────────────────

.pan_fecha_hora <- function(ts, tz) {
  if (is.null(ts) || is.na(ts)) return("")
  paste0(as.integer(format(ts, "%d", tz = tz)), " ",
         tr(paste0("comun.mes.", as.integer(format(ts, "%m", tz = tz)))), ", ",
         format(ts, "%H:%M", tz = tz))
}

.pan_fecha_corta <- function(ts, tz) {
  d <- as.Date(ts, tz = tz)
  paste(tr(paste0("panorama.dia.", as.integer(format(d, "%u")))), as.integer(format(d, "%d")))
}

.pan_iniciales <- function(n) {
  p <- strsplit(trimws(n), "[[:space:]._-]+")[[1]]
  toupper(paste(substr(utils::head(p[nzchar(p)], 2), 1, 1), collapse = ""))
}

.pan_lista_nombres <- function(n) {
  n <- utils::head(n, 3)
  if (length(n) <= 1) return(n %||% "")
  paste(paste(n[-length(n)], collapse = ", "), tr("panorama.y"), n[length(n)])
}

# MXN (or the first currency) in full; others as "+ N más" in the Cinta.
.pan_monto_corto <- function(t) {
  base <- paste0(panorama_monto(t$total[1]), if (t$moneda[1] != "MXN") paste0(" ", t$moneda[1]))
  if (nrow(t) > 1) paste(base, tr("panorama.cinta.mas_monedas", n = as.character(nrow(t) - 1L))) else base
}

# A Hoy card: the base currency large, any other currency underneath (never
# added together), the count line and the tab it opens.
.pan_tarjeta_hoy <- function(titulo, totales, linea, tab, ns) {
  vacio <- is.null(totales) || !nrow(totales)
  principal <- if (vacio) panorama_monto(0) else
    paste0(panorama_monto(totales$total[1]), if (totales$moneda[1] != "MXN") paste0(" ", totales$moneda[1]))
  shiny::tags$button(
    type = "button", class = paste("pan-tarjeta pan-hoy-tarjeta", if (vacio) "pan-hoy-tarjeta--vacia"),
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("ir"), tab),
    shiny::div(class = "pan-etiqueta", titulo),
    shiny::div(class = "pan-cifra", principal),
    if (!vacio && nrow(totales) > 1) shiny::div(class = "pan-cifra-otras",
      lapply(2:nrow(totales), function(i)
        shiny::div(paste0("+ ", panorama_monto(totales$total[i]), " ", totales$moneda[i])))),
    shiny::div(class = "pan-meta", linea, " →"))
}
