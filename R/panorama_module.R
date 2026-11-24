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
    # The client's base currency (T39): first in every total, and the one
    # whose amounts carry no code.
    base_cli <- shiny::reactive(tryCatch(shared$moneda_base_cliente()$moneda, error = function(e) NULL) %||%
                                  MONEDA_BASE_OMISION)
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
      v <- tryCatch(avisos_visibles(c, u), error = function(e) {
        message("[panorama] avisos_visibles falló: ", conditionMessage(e)); NULL
      })
      # T80b: the weekly email not gone out, computed on read (R/avisos_correo_vigia.R).
      staff <- isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
      vig <- tryCatch(avisos_visibles(c, u, df = correo_vigia_avisos(c, u, staff = staff)), error = function(e) NULL)
      if (!is.null(v) && !is.null(vig) && nrow(vig)) v <- dplyr::bind_rows(vig, v)
      v
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
      # The previous visit is read FIRST: it is what "new since your last
      # visit" means in this session. Only then is the new one written --
      # after the first screen is drawn (T88-3: the write is off the sign-in
      # path), with the time of this visit, taken now.
      p <- tryCatch(prefs_leer(c, u$user), error = function(e) NULL)
      visita(p$ultima_visita)
      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      if (!isTRUE(info$is_staff)) {
        ahora_visita <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        session$onFlushed(function()
          tryCatch(prefs_guardar(c, u$user, list(ultima_visita = ahora_visita)),
                   error = function(e) message("[panorama] prefs_guardar falló: ", conditionMessage(e))),
          once = TRUE)
      }
    })

    feed <- shiny::reactive({
      v <- visibles(); if (is.null(v)) return(panorama_feed(NULL))
      # T68: the activity this reader takes, by their preference (after the
      # permission avisos_visibles() already applied).
      fl <- ad_filtro()
      if (!is.null(fl)) v <- avisos_de_filtrar_feed(v, fl$pref, fl$cod)
      panorama_feed(v, visita(), usuario = usr()$user)
    })
    mis_sol <- shiny::reactive({
      v <- visibles(); u <- usr()
      if (is.null(v) || is.null(u)) return(NULL)
      panorama_mis_solicitudes(v, u$user)
    })

    # ── Data already in the session ───────────────────────────────────────────
    pagos  <- shiny::reactive(panorama_pagos_hoy(tryCatch(shared$pagar_hoy_db(), error = function(e) NULL), hoy(),
                                                 base = base_cli()))
    tier_ef <- shiny::reactive(tryCatch(shared$tier_efectivo(), error = function(e) NULL))
    ver_cxc <- shiny::reactive(nivel_ve_libro(tier_ef(), "AR"))
    ver_cxp <- shiny::reactive(nivel_ve_libro(tier_ef(), "AP"))   # T89
    # T89: the ledger's rows as the Calendar counts them (IC and confirmed
    # out, panorama_filas_vigentes()); NULL when the ledger cannot be read.
    vigentes <- function(ledger) {
      f <- shared[[paste0("df_combined_", ledger)]]
      tryCatch(panorama_filas_vigentes(if (is.function(f)) f() else NULL, ledger, shared),
               error = function(e) { message("[panorama] filas ", ledger, ": ", conditionMessage(e)); NULL })
    }
    filas_ar <- shiny::reactive(if (ver_cxc()) vigentes("AR") else NULL)
    filas_ap <- shiny::reactive(if (ver_cxp()) vigentes("AP") else NULL)
    cobros <- shiny::reactive({
      if (!ver_cxc()) return(NULL)
      panorama_cobros_hoy(filas_ar(), hoy(), base = base_cli())
    })
    cobros_atrasados <- shiny::reactive({
      if (!ver_cxc()) return(NULL)
      panorama_cobros_atrasados(filas_ar(), hoy(), base = base_cli())
    })
    # T89: "Por pagar hoy", the ERP's supplier invoices due today (the twin of
    # Por cobrar hoy; the Agenda's is "Programado para pagar hoy").
    por_pagar <- shiny::reactive({
      if (!ver_cxp()) return(NULL)
      panorama_cobros_hoy(filas_ap(), hoy(), base = base_cli())
    })
    por_pagar_atrasados <- shiny::reactive({
      if (!ver_cxp()) return(NULL)
      panorama_cobros_atrasados(filas_ap(), hoy(), base = base_cli())
    })
    pasivos <- shiny::reactive(panorama_pasivos_pronto(
      tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL), hoy(), base = base_cli()))
    sap <- shiny::reactive({ reloj(); panorama_sap_estado(tryCatch(shared$sap_snapshot_info(), error = function(e) NULL),
                                                          tz = zona()) })

    # ── Encabezado ────────────────────────────────────────────────────────────
    output$encabezado <- shiny::renderUI({
      u <- usr(); info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      nombre <- strsplit(trimws(info$name %||% u$user %||% ""), "\\s+")[[1]][1] %||% ""
      ahora <- Sys.time(); reloj()
      s <- sap()
      # Always on screen, with or without neblina (T46).
      linea_sap <- sap_al_texto(if (isTRUE(s$cargado)) s$ts, zona())
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
                   tr("panorama.sap.viejo_texto", n = as.character(SAP_DIAS_VIEJO))),
        shiny::tags$button(type = "button", class = "pan-boton pan-boton--claro",
                           onclick = "hdPanorama.refrescarSap()", tr("panorama.sap.refrescar")))
    })

    # ── Clima de hoy ──────────────────────────────────────────────────────────
    output$clima <- shiny::renderUI({
      f <- feed(); ms <- mis_sol(); p <- pagos()
      cl <- panorama_clima(f, n_solicitudes = if (is.null(ms)) 0L else nrow(ms),
                           n_pagos_hoy = sum(p$n), sap = sap())
      icono <- switch(cl$clima, despejado = "sun", medio = "cloud-sun", nublado = "cloud", neblina = "smog")
      # Each thing by its name, never one "pendientes" adding payments and
      # solicitudes (T46b, E2).
      n_sol <- if (is.null(ms)) 0L else nrow(ms)
      partes <- c(if (sum(p$n) > 0) tr_n("panorama.clima.cuenta.pagos", sum(p$n)),
                  if (n_sol > 0) tr_n("panorama.clima.cuenta.solicitudes", n_sol))
      urgentes <- if (cl$urgentes > 0) tr_n("panorama.clima.cuenta.urgentes", cl$urgentes)
                  else tr("panorama.clima.cuenta.ninguno_urgente")
      cuenta <- if (cl$clima == "neblina") tr("panorama.clima.cuenta_sap") else
        if (!length(partes)) tr("panorama.clima.cuenta.nada", urgentes = urgentes) else
          tr("panorama.clima.cuenta.texto", urgentes = urgentes,
             que = paste(partes, collapse = paste0(" ", tr("panorama.y"), " ")))
      shiny::div(class = paste0("pan-tarjeta pan-clima pan-clima--", cl$clima),
        shiny::div(class = "pan-etiqueta", tr("panorama.clima.titulo")),
        shiny::div(class = "pan-explica pan-explica--clima", tr("panorama.clima.explica")),
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
      # The weekly emails' words (semana.tarjeta.*, T45) where the measure is
      # the same; another measure, another word (T46b): the Agenda's payments
      # are "Programado para pagar hoy", not the ledger's "Por pagar".
      # T89 order: the ERP's pair (Por cobrar hoy, Por pagar hoy: "vencen
      # hoy"), then what the team already scheduled (the Agenda), then what
      # is coming (Pasivos). A tier that does not see a ledger does not get
      # its card (nivel_ve_libro()).
      pp <- pasivos()
      excluye <- function(atras) {
        if (is.null(atras) || !nrow(atras) || !any(atras$total > 0)) return(NULL)
        a <- atras[atras$total > 0, , drop = FALSE]
        paste(tr("semana.tarjeta.por_cobrar.excluye",
                 monto = paste(monto_moneda(a$total, a$moneda), collapse = " · ")),
              "·", tr("semana.tarjeta.por_cobrar.excluye_ver", destino = tr("panorama.destino.ven")))
      }
      tarjetas <- list(
        if (ver_cxc()) .pan_tarjeta_hoy(tr("panorama.hoy.por_cobrar"), cobros(),
          tr_n("semana.tarjeta.por_cobrar.n", sum(cobros()$n)), "CAL", ns, base = base_cli(),
          explica = tr("semana.tarjeta.por_cobrar.explica", rango = tr("panorama.hoy.rango")),
          extra = excluye(cobros_atrasados())),
        if (ver_cxp()) .pan_tarjeta_hoy(tr("panorama.hoy.por_pagar"), por_pagar(),
          tr_n("semana.tarjeta.por_cobrar.n", sum(por_pagar()$n)), "CAL", ns, base = base_cli(),
          explica = tr("panorama.hoy.por_pagar_explica", rango = tr("panorama.hoy.rango")),
          extra = excluye(por_pagar_atrasados())),
        .pan_tarjeta_hoy(tr("panorama.hoy.programado"), pagos(),
          tr_n("semana.tarjeta.por_pagar.n", sum(pagos()$n)), "PH", ns, base = base_cli(),
          explica = tr("panorama.hoy.programado_explica"),
          extra = .pan_desglose_texto(panorama_pagos_desglose(
            tryCatch(shared$pagar_hoy_db(), error = function(e) NULL), hoy(), base = base_cli()))),
        .pan_tarjeta_hoy(tr("panorama.hoy.pasivos_titulo", n = as.character(PANORAMA_DIAS_PASIVOS)), pp$totales,
          if (nrow(pp$totales))
            tr_n("panorama.hoy.pasivos_n", sum(pp$totales$n), fecha = .pan_fecha_corta(pp$primera, zona()))
          else tr("panorama.hoy.pasivos_ninguno"), "PSV", ns, base = base_cli(),
          explica = tr("panorama.hoy.pasivos_explica", fecha = .pan_fecha_corta(hoy() + PANORAMA_DIAS_PASIVOS, zona()))))
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
    # T81: a public note's aviso (NOTA:<year>-<month>) goes to the Calendar
    # and opens the notes panel in that month (notes_handlers, input$abrir_notas).
    shiny::observeEvent(input$ir, {
      tab <- input$ir
      if (!panorama_enlace_ok(tab)) return()
      mes <- notas_enlace_mes(tab)
      if (is.null(mes)) return(ir_a(tab))
      ir_a("CAL")
      shinyjs::runjs(sprintf("Shiny.setInputValue('abrir_notas', '%s', {priority: 'event'});",
                             format(mes, "NOTA:%Y-%m")))
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
          # T68: "De quién recibo avisos", for whoever has at least one kind of notice.
          if (length(tryCatch(avisos_de_canales_de(usr()), error = function(e) character())))
            shiny::tags$button(type = "button", class = "pan-feed-ajustes", title = tr("panorama.avisos_de.titulo"),
              onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("req_avisos_abrir")),
              shiny::icon("gear"), " ", tr("panorama.avisos_de.boton")),
          shiny::div(class = "pan-meta",
            if (!is.null(uv) && !is.na(suppressWarnings(as.POSIXct(uv, tz = "UTC"))))
              tr("panorama.feed.desde", cuando = panorama_cuando(as.POSIXct(uv, tz = "UTC"), tz = zona()))
            else tr("panorama.feed.primera_vez"))),
        shiny::div(class = "pan-chips", role = "group", `aria-label` = tr("panorama.feed.filtro"),
          lapply(chips, function(t) shiny::tags$button(
            type = "button", class = "pan-chip", `data-tipo` = t,
            `aria-pressed` = if (t == "todo") "true" else "false",
            title = if (t == "ale") tr("panorama.feed.chip.ale_explica"),
            tr(paste0("panorama.feed.chip.", t)),
            shiny::span(class = "pan-chip-n", cuenta(t))))),
        if (!nrow(f)) shiny::p(class = "pan-vacio", tr("panorama.feed.vacio"))
        else shiny::tags$ul(class = "pan-lista",
          lapply(seq_len(min(nrow(f), 30L)), function(i) {
            a <- f[i, ]
            icono <- switch(a$tipo_feed, ale = "triangle-exclamation", act = "user-check",
                            sol = "clipboard-check", req = "clipboard-list", "bullhorn")
            # A published recordatorio or reconocimiento says so (its chip is
            # still Comunicados).
            etiqueta <- if (a$tipo_feed == "com" && isTRUE(a$clase_ok %in% c("recordatorio", "reconocimiento")))
              paste0("panorama.clase.", a$clase_ok) else paste0("panorama.feed.tipo.", a$tipo_feed)
            meta <- c(tr(etiqueta),
                      if (nzchar(a$cuerpo %||% "")) a$cuerpo,
                      if ((a$contador %||% 1L) > 1L) tr("panorama.feed.veces", n = as.character(a$contador)))
            if (grepl("^nota\\|", a$clave %||% "")) icono <- "note-sticky"   # T81
            enlace <- if (panorama_enlace_ok(a$enlace)) a$enlace else NULL
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

    # ── De quién recibo avisos (T68) ──────────────────────────────────────────
    # One panel, on the side of who receives (it replaces T62's "Mis avisos"
    # and the "Alertas del equipo" matrix): "Lo mío, siempre", and a grid of
    # people x kinds of notice (hd_table avisos_de, col_casilla) with search,
    # groups by tier or job title, Todos / Ninguno / Solo yo and "Ver: todas
    # / elegidas". Columns by permission of each ACTION (avisos_de_canales_de);
    # the server trims the channels again when saving. Saved whole with
    # prefs_reemplazar() as datos$avisos_de (base + exceptions). Staff inside
    # a client see it, never save it.
    ad <- shiny::reactiveValues(estado = NULL, canales = character(), ver = "todas", agrupar = "nivel", yo = NA_character_)
    ad_staff <- function() isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
    ad_datos <- shiny::reactive({
      e <- ad$estado
      shiny::req(is.data.frame(e))
      e$grupo <- switch(ad$agrupar, nivel = e$nivel, puesto = e$puesto, rep("", nrow(e)))
      if (identical(ad$ver, "elegidas") && length(ad$canales))
        e <- e[rowSums(as.matrix(e[, ad$canales, drop = FALSE])) > 0 | e$id %in% ad$yo, , drop = FALSE]
      e
    })
    ad_tabla <- hd_table_server("avisos_de", tabla = "avisos_de", datos = ad_datos, grupo = "grupo",
      textos = list(contador = "panorama.avisos_de.contador"),
      columnas = shiny::reactive(c(
        list(col_texto("nombre", "panorama.avisos_de.persona"), col_texto("puesto", "panorama.avisos_de.puesto")),
        lapply(ad$canales, function(c) col_casilla(c, paste0("panorama.avisos_de.canal.", c))))))
    # The grid as it stands: the data plus what was ticked (t$casillas()).
    ad_actual <- function() {
      e <- shiny::isolate(ad$estado)
      cs <- tryCatch(shiny::isolate(ad_tabla$casillas()), error = function(err) NULL)
      if (is.data.frame(e) && is.data.frame(cs) && nrow(cs)) {
        k <- match(cs$id, e$id)
        for (c in intersect(shiny::isolate(ad$canales), names(cs))) e[[c]][k[!is.na(k)]] <- cs[[c]][!is.na(k)]
      }
      e
    }
    ad_poner <- function(e) { ad$estado <- e; ad_tabla$limpiar_casillas() }

    shiny::observeEvent(input$req_avisos_abrir, {
      c <- cid(); u <- usr()
      if (is.null(c) || !nzchar(c) || is.null(u)) return()
      staff <- ad_staff()
      casa <- if (staff) tryCatch(shared$home_client_id(), error = function(e) NULL) else NULL
      if (staff && (is.null(casa) || !nzchar(casa))) return()
      canales <- avisos_de_canales_de(u)
      if (staff) canales <- setdiff(canales, "req")   # requisitions reach the client's own users
      if (!length(canales)) return()
      leido <- tryCatch({
        usuarios <- auth_load_usuarios(client_id = c)
        cod <- avisos_de_codigos(usuarios)
        pref <- if (staff) avisos_de_staff_de(prefs_leer(casa, u$user), c) else {
          matriz <- tryCatch(alertas_leer(c), error = function(e) NULL)   # only to work out a preference never saved
          avisos_de_de(prefs_leer(c, u$user), matriz, cod, codigos = req_avisos_personas(usuarios)$id)
        }
        yo <- unname(cod[tolower(u$user)])
        list(filas = avisos_de_filas(usuarios, pref, canales, yo_code = yo %||% NA_character_), pref = pref, yo = yo)
      }, error = function(e) NULL)
      if (is.null(leido)) { shiny::showNotification(tr("panorama.req_avisos.error_leer"), type = "error"); return() }
      ad$canales <- canales; ad$ver <- "todas"; ad$agrupar <- "nivel"; ad$yo <- leido$yo %||% NA_character_
      ad_poner(leido$filas)
      # hd_modal(): the window for a table (almost the whole window; the grid
      # in window mode grows to its bottom and never pushes the footer out).
      shiny::showModal(hd_modal(
        titulo = tr("panorama.avisos_de.titulo"),
        pie = shiny::tagList(shiny::modalButton(tr("comun.cancelar")),
          shiny::actionButton(ns("ad_guardar"), tr("comun.guardar"), class = "btn btn-primary")),
        shiny::div(class = "pan-avisos-de",
          if (!staff) shiny::checkboxInput(ns("ad_mio"), tr("panorama.avisos_de.mio"), value = isTRUE(leido$pref$mio), width = "100%"),
          shiny::p(class = "text-muted small mb-2", tr(if (staff) "panorama.avisos_de.staff_nota" else "panorama.avisos_de.mio_ayuda")),
          # Inline: the modal lives outside .hd-panorama, and panorama.css keeps
          # its rules inside it (test_t49_notas_cinta).
          shiny::div(class = "pan-avisos-de-barra",
            style = "display:flex; flex-wrap:wrap; align-items:center; gap:var(--hd-esp-2) var(--hd-esp-4); margin-bottom:var(--hd-esp-2);",
            shiny::div(class = "btn-group btn-group-sm", role = "group",
              shiny::actionButton(ns("ad_todos"), tr("panorama.avisos_de.todos"), class = "btn btn-outline-secondary"),
              shiny::actionButton(ns("ad_ninguno"), tr("panorama.avisos_de.ninguno"), class = "btn btn-outline-secondary"),
              shiny::actionButton(ns("ad_solo_yo"), tr("panorama.avisos_de.solo_yo"), class = "btn btn-outline-secondary")),
            shiny::radioButtons(ns("ad_ver"), NULL, inline = TRUE, selected = "todas",
              choiceValues = c("todas", "elegidas"),
              choiceNames = list(tr("panorama.avisos_de.ver_todas"), tr("panorama.avisos_de.ver_elegidas"))),
            shiny::radioButtons(ns("ad_agrupar"), NULL, inline = TRUE, selected = "nivel",
              choiceValues = c("nivel", "puesto", "ninguno"),
              choiceNames = list(tr("panorama.avisos_de.por_nivel"), tr("panorama.avisos_de.por_puesto"),
                                 tr("panorama.avisos_de.sin_agrupar")))),
          shiny::div(class = "pan-avisos-de-tabla", hd_table_ui(ns("avisos_de"), alto = "ventana")))))
    }, ignoreInit = TRUE)

    ad_todas <- function(valor) {
      e <- ad_actual(); if (!is.data.frame(e)) return()
      for (c in shiny::isolate(ad$canales)) e[[c]] <- rep(valor, nrow(e))
      ad_poner(e)
    }
    shiny::observeEvent(input$ad_todos,   ad_todas(TRUE),  ignoreInit = TRUE)
    shiny::observeEvent(input$ad_ninguno, ad_todas(FALSE), ignoreInit = TRUE)
    shiny::observeEvent(input$ad_solo_yo, {   # "Solo yo": nobody's, with "Lo mío" on
      ad_todas(FALSE)
      shiny::updateCheckboxInput(session, "ad_mio", value = TRUE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$ad_ver, {
      if (identical(input$ad_ver, shiny::isolate(ad$ver))) return()
      e <- ad_actual(); ad$ver <- input$ad_ver; ad_poner(e)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$ad_agrupar, {
      if (identical(input$ad_agrupar, shiny::isolate(ad$agrupar))) return()
      e <- ad_actual(); ad$agrupar <- input$ad_agrupar; ad_poner(e)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ad_guardar, {
      c <- cid(); u <- usr()
      if (is.null(c) || !nzchar(c) || is.null(u)) return()
      e <- ad_actual()
      if (!is.data.frame(e)) return()
      # Staff: in their home client, never in this one's folder.
      casa <- if (ad_staff()) tryCatch(shared$home_client_id(), error = function(e) NULL) else NULL
      if (ad_staff() && (is.null(casa) || !nzchar(casa))) return()
      ok <- tryCatch({ avisos_de_guardar(c, u, e, isTRUE(input$ad_mio), casa = casa); TRUE },
                     error = function(err) { message("[panorama] avisos_de no se guardó: ", conditionMessage(err)); FALSE })
      if (ok) {
        ad_version(ad_version() + 1L)
        shiny::removeModal()
        shiny::showNotification(tr("panorama.req_avisos.guardado"), type = "message")
      } else shiny::showNotification(tr("panorama.req_avisos.error_guardar"), type = "error")
    }, ignoreInit = TRUE)

    # The feed's activity by this reader's preference (after the permission,
    # never instead of it): read when the tab opens and after saving.
    ad_version <- shiny::reactiveVal(0L)
    ad_filtro <- shiny::reactive({
      if (!isTRUE(activa())) return(NULL)
      ad_version()
      c <- cid(); u <- usr()
      if (is.null(c) || !nzchar(c) || is.null(u)) return(NULL)
      tryCatch({
        usuarios <- auth_load_usuarios(client_id = c)
        cod <- avisos_de_codigos(usuarios)
        pref <- if (ad_staff()) avisos_de_staff_de(prefs_leer(shared$home_client_id(), u$user), c)
                else avisos_de_de(prefs_leer(c, u$user), tryCatch(alertas_leer(c), error = function(e) NULL), cod)
        list(pref = pref, cod = cod)
      }, error = function(e) { message("[panorama] avisos_de: no se pudo leer la preferencia; el feed no se filtra"); NULL })
    })

    # ── Pagada / pago anulado de requisiciones (T62) ──────────────────────────
    # Published by the session of whoever confirmed or annulled: the
    # confirmations this session's user made (confirmado_por) or annulled
    # (eliminado_por) since it started, of a provision, as they reach the
    # session. Nothing is read in any other session; repetir = FALSE (and
    # `hechas`) keep each event to one notice.
    inicio_sesion <- Sys.time()
    hechas <- character()
    shiny::observe({
      cf <- tryCatch(if (is.function(shared$bancos_confirmados)) shared$bancos_confirmados() else NULL,
                     error = function(e) NULL)
      u <- shiny::isolate(usr()); c <- shiny::isolate(cid())
      if (!is.data.frame(cf) || !nrow(cf) || is.null(u) || is.null(c) || !nzchar(c)) return()
      col <- function(n) if (n %in% names(cf)) cf[[n]] else rep(NA, nrow(cf))
      yo  <- tolower(trimws(u$user %||% ""))
      con <- !is.na(col("provision_id")) & nzchar(as.character(col("provision_id")))
      pago <- con & tolower(trimws(as.character(col("confirmado_por")))) %in% yo &
        !is.na(col("confirmado_at")) & as.POSIXct(col("confirmado_at")) >= inicio_sesion
      anul <- con & col("eliminado") %in% TRUE & tolower(trimws(as.character(col("eliminado_por")))) %in% yo &
        !is.na(col("eliminado_at")) & as.POSIXct(col("eliminado_at")) >= inicio_sesion
      llaves <- c(paste0(col("confirmacion_id")[pago], "|pagada"), paste0(col("confirmacion_id")[anul], "|anulada"))
      nuevas <- setdiff(llaves, hechas)
      if (!length(nuevas)) return()
      hechas <<- c(hechas, nuevas)
      req_publicar_pagos(c, unique(sub("\\|(pagada|anulada)$", "", nuevas)), u$user)
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

# The Cinta: the first two currencies in full (the base first, as
# .pan_por_moneda() orders them), "y N más" from the third. Every figure
# carries its code (T46).
.pan_monto_corto <- function(t) {
  txt <- paste(monto_moneda(utils::head(t$total, 2L), utils::head(t$moneda, 2L)), collapse = ", ")
  if (nrow(t) > 2) paste(txt, tr("panorama.cinta.mas_monedas", n = as.character(nrow(t) - 2L))) else txt
}

# A Hoy card: the label, the first currency (the base when there is any)
# large, any other currency underneath (never added together), what the
# figure is (`explica`) with its count, an optional `extra` line, and the tab
# it opens on a line of its own (T46b: it read as part of the data). Every
# figure carries its code, the empty card the base's (T46).
# The line under "Programado para pagar hoy" (T60): "Incluye 2 vencidos
# (300.00 MXN) · 1 anticipado (2,000.00 MXN)", each part with its amounts by
# currency (monto_moneda(), never added up across currencies). NULL when the
# Agenda holds nothing overdue nor ahead.
.pan_desglose_texto <- function(d) {
  parte <- function(t, clave) if (!is.null(t) && nrow(t))
    tr_n(clave, sum(t$n), monto = paste(monto_moneda(t$total, t$moneda), collapse = " · "))
  partes <- c(parte(d$vencidos, "panorama.hoy.desglose.vencidos"), parte(d$anticipados, "panorama.hoy.desglose.anticipados"))
  if (!length(partes)) return(NULL)
  tr("panorama.hoy.desglose", partes = paste(partes, collapse = " · "))
}

.pan_tarjeta_hoy <- function(titulo, totales, linea, tab, ns, base = MONEDA_BASE_OMISION,
                             explica = NULL, extra = NULL) {
  vacio <- is.null(totales) || !nrow(totales)
  principal <- if (vacio) monto_moneda(0, base) else monto_moneda(totales$total[1], totales$moneda[1])
  shiny::tags$button(
    type = "button", class = paste("pan-tarjeta pan-hoy-tarjeta", if (vacio) "pan-hoy-tarjeta--vacia"),
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("ir"), tab),
    shiny::div(class = "pan-etiqueta", titulo),
    shiny::div(class = "pan-cifra", principal),
    if (!vacio && nrow(totales) > 1) shiny::div(class = "pan-cifra-otras",
      lapply(2:nrow(totales), function(i)
        shiny::div(paste0("+ ", monto_moneda(totales$total[i], totales$moneda[i]))))),
    shiny::div(class = "pan-explica", paste(c(explica, linea), collapse = " · ")),
    if (length(extra) && nzchar(extra)) shiny::div(class = "pan-explica pan-explica--extra", extra),
    shiny::div(class = "pan-ver", tr("panorama.hoy.ver", destino = tr(paste0("panorama.destino.", tolower(tab))))))
}
