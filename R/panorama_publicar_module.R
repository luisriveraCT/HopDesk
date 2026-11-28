# =============================================================================
# R/panorama_publicar_module.R
# Panorama › Publicar (front B2). Only with can_publish_panorama, checked on
# the server at every write; the button that opens it is only offered to
# those who hold it.
#
#   Nueva publicación  kind, title, message, audience, deadline, button and a
#                      live preview of the Destacado and the Cinta line
#   Publicadas         the live publications (hd_table "avisos_publicados")
#                      with Desactivar: ends its visibility today, never deletes
#   (T68: the "Alertas del equipo" tab is gone: each person chooses whose
#   notices they receive, Panorama's "De quién recibo avisos".)
#
# Audience (plan "Audiencia por niveles"): a bar of the client tiers; a
# solicitud goes to named people instead. The last tiers chosen are kept in
# the publisher's preferences (prefs$audiencia) and the bar opens with them.
# =============================================================================

panoramaPublicarUI <- function(id) {
  ns <- shiny::NS(id)
  tipo_txt <- function(t) tr(paste0("panorama.publicar.tipo.", t))
  shiny::div(class = "pan-publicar",
    shiny::div(class = "pan-publicar-cabeza",
      shiny::tags$button(`data-hd-espera` = "auto", type = "button", class = "pan-boton pan-boton--claro",
        onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("volver")),
        "← ", tr("panorama.publicar.volver")),
      shiny::div(class = "pan-kicker", tr("panorama.publicar.kicker"))),
    shiny::div(class = "pan-pestanas", role = "tablist",
      shiny::tags$button(type = "button", role = "tab", class = "pan-pestana", `data-seccion` = ns("s_nueva"), id = ns("tab_nueva"),
                         `aria-selected` = "true", tr("panorama.publicar.nueva")),
      shiny::tags$button(type = "button", role = "tab", class = "pan-pestana", `data-seccion` = ns("s_publicadas"), id = ns("tab_publicadas"),
                         `aria-selected` = "false", tr("panorama.publicar.publicadas"),
                         shiny::span(class = "pan-chip-n", shiny::textOutput(ns("n_vigentes"), inline = TRUE)))),

    shiny::div(id = ns("s_nueva"), class = "pan-seccion",
      shiny::div(class = "pan-rejilla",
        shiny::div(class = "pan-col-a",
          shiny::div(class = "pan-tarjeta pan-form",
            shiny::div(class = "pan-etiqueta", tr("panorama.publicar.redactar")),
            shiny::uiOutput(ns("tipos")),
            shiny::div(class = "pan-campo",
              shiny::tags$label(`for` = ns("titulo"), class = "pan-etiqueta", tr("panorama.publicar.titulo")),
              shiny::tags$input(id = ns("titulo"), type = "text", class = "form-control shiny-input-text",
                                maxlength = PANORAMA_TITULO_MAX, `data-contador` = ns("titulo_n")),
              shiny::span(class = "pan-contador", id = ns("titulo_n"), paste0("0 / ", PANORAMA_TITULO_MAX))),
            shiny::div(class = "pan-campo",
              shiny::tags$label(`for` = ns("mensaje"), class = "pan-etiqueta", tr("panorama.publicar.mensaje")),
              shiny::tags$textarea(id = ns("mensaje"), class = "form-control shiny-input-textarea", rows = 4,
                                   maxlength = PANORAMA_MENSAJE_MAX, `data-contador` = ns("mensaje_n")),
              shiny::span(class = "pan-contador", id = ns("mensaje_n"), paste0("0 / ", PANORAMA_MENSAJE_MAX))),
            shiny::uiOutput(ns("para")),
            shiny::div(class = "pan-campos-fila",
              shiny::div(class = "pan-campo",
                shiny::uiOutput(ns("vence_ui"))),
              shiny::div(class = "pan-campo",
                shiny::selectInput(ns("enlace"), tr("panorama.publicar.boton"),
                  choices = c(stats::setNames("", tr("panorama.publicar.sin_boton")),
                              stats::setNames(PANORAMA_ENLACES,
                                              vapply(PANORAMA_ENLACES, function(t) tr(paste0("panorama.publicar.ir.", tolower(t))),
                                                     character(1)))))))) ),
        shiny::div(class = "pan-col-b",
          shiny::uiOutput(ns("vista")),
          shiny::div(class = "pan-tarjeta pan-enviar",
            shiny::uiOutput(ns("edicion")),   # T91b: "Editando …" and Cancelar
            shiny::uiOutput(ns("alcance")),
            shiny::tags$button(type = "button", id = ns("publicar"), class = "pan-boton pan-boton--grande action-button",
                               tr("panorama.publicar.publicar")))))),

    shiny::div(id = ns("s_publicadas"), class = "pan-seccion", hidden = NA,
      shiny::div(class = "pan-tarjeta", hd_table_ui(ns("tabla")))))
}

# T91a: of the Publicadas rows a user asked to take down (`filas`, with the
# author in `por`), the ones they may: their own always; someone else's only
# with can_moderate_panorama. Pure, so the server's rule is tested without a
# browser. list(filas = the allowed rows, rechazadas = how many were not).
publicadas_retirables <- function(filas, usuario) {
  if (is.null(filas) || !nrow(filas)) return(list(filas = filas, rechazadas = 0L))
  ajenas <- !(tolower(trimws(filas$por)) %in% tolower(trimws(usuario$user %||% "")))
  if (!any(ajenas) || isTRUE(tryCatch(puede(usuario, "can_moderate_panorama"), error = function(e) FALSE)))
    return(list(filas = filas, rechazadas = 0L))
  list(filas = filas[!ajenas, , drop = FALSE], rechazadas = sum(ajenas))
}

# T91b: of the rows a user asked to edit, the ones they may -- their own,
# and never a note (notes are edited in Notas del mes). Not even a moderator
# edits someone else's. list(filas, rechazadas).
publicadas_editables <- function(filas, usuario) {
  if (is.null(filas) || !nrow(filas)) return(list(filas = filas, rechazadas = 0L))
  ok <- tolower(trimws(filas$por)) %in% tolower(trimws(usuario$user %||% "")) & !(filas$tipo %in% "nota")
  list(filas = filas[ok, , drop = FALSE], rechazadas = sum(!ok))
}

panoramaPublicarServer <- function(id, shared, abierta = shiny::reactive(TRUE), al_volver = function() NULL,
                                   al_publicar = function() NULL, tz = shiny::reactive("America/Mexico_City")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cid <- shiny::reactive(tryCatch(shared$effective_client_id(), error = function(e) NULL))
    usr <- shiny::reactive(tryCatch(shared$usuario(), error = function(e) NULL))
    es_staff <- shiny::reactive(isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE)))
    version <- shiny::reactiveVal(0L)
    todos <- niveles_cliente()

    shiny::observeEvent(input$volver, al_volver(), ignoreInit = TRUE)

    # T91b: Publicar opens for everyone (Mis publicaciones); only "Nueva"
    # needs can_publish_panorama -- its tab and section are not there without
    # it (the server checks it again when publishing).
    puede_pub <- shiny::reactive(isTRUE(tryCatch(puede(usr(), "can_publish_panorama"), error = function(e) FALSE)))
    .ver_seccion <- function(cual) shinyjs::runjs(sprintf(
      "(function(){ var n = document.getElementById('%s'), p = document.getElementById('%s');
         if (n) n.hidden = %s; if (p) p.hidden = %s;
         ['%s','%s'].forEach(function(id, i){ var b = document.getElementById(id);
           if (b) b.setAttribute('aria-selected', (i === 0) === %s ? 'true' : 'false'); });
         $(window).trigger('resize'); })();",
      ns("s_nueva"), ns("s_publicadas"), if (cual == "nueva") "false" else "true", if (cual == "nueva") "true" else "false",
      ns("tab_nueva"), ns("tab_publicadas"), if (cual == "nueva") "true" else "false"))
    shiny::observe({
      if (!isTRUE(abierta())) return()
      p <- puede_pub()
      shinyjs::toggle("tab_nueva", condition = p)
      if (!p && is.null(shiny::isolate(editando()))) .ver_seccion("publicadas")
    })

    # T91b: the publication being edited (NULL = a new one): list(id, tipo,
    # titulo). Publicar then saves the edit in place (avisos_editar).
    editando <- shiny::reactiveVal(NULL)
    output$edicion <- shiny::renderUI({
      e <- editando(); if (is.null(e)) return(NULL)
      shiny::div(class = "pan-edicion",
        shiny::div(class = "pan-etiqueta", tr("panorama.publicar.editando", titulo = e$titulo)),
        if (identical(e$tipo, "solicitud")) shiny::div(class = "pan-meta", tr("panorama.publicar.editar_responsables")),
        shiny::tags$button(`data-hd-espera` = "no", type = "button", class = "pan-boton pan-boton--claro",
          onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("cancelar_edicion")),
          tr("panorama.publicar.cancelar_edicion")))
    })
    .limpiar_formulario <- function() {
      session$sendInputMessage("titulo", list(value = ""))
      session$sendInputMessage("mensaje", list(value = ""))
      shiny::updateSelectizeInput(session, "responsables", selected = character())
    }
    shiny::observeEvent(input$cancelar_edicion, {
      editando(NULL); .limpiar_formulario(); .ver_seccion("publicadas")
    }, ignoreInit = TRUE)

    # ── Kind ──────────────────────────────────────────────────────────────────
    tipo <- shiny::reactiveVal("comunicado")
    shiny::observeEvent(input$tipo, {
      if (isTRUE(input$tipo %in% PANORAMA_TIPOS_PUBLICAR)) tipo(input$tipo)
    }, ignoreInit = TRUE)
    output$tipos <- shiny::renderUI({
      t <- tipo()
      shiny::div(class = "pan-tipos", role = "radiogroup", `aria-label` = tr("panorama.publicar.tipo_ayuda"),
        lapply(PANORAMA_TIPOS_PUBLICAR, function(x) shiny::tags$button(`data-hd-espera` = "no",
          type = "button", role = "radio", `aria-checked` = if (x == t) "true" else "false",
          class = paste("pan-tipo", paste0("pan-tipo--", x)),
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("tipo"), x),
          shiny::span(class = "pan-tag", tr(paste0("panorama.publicar.tipo.", x))),
          shiny::span(class = "pan-tipo-ayuda", tr(paste0("panorama.publicar.tipo_ayuda.", x))))))
    })

    # ── Audience ──────────────────────────────────────────────────────────────
    audiencia <- shiny::reactive({
      c <- cid(); if (is.null(c) || !isTRUE(abierta())) return(NULL)
      version()
      tryCatch(audiencia_niveles(c), error = function(e) NULL)
    })
    estado <- shiny::reactiveVal(NULL)
    shiny::observe({
      if (!isTRUE(abierta()) || !is.null(estado())) return()
      c <- cid(); u <- usr(); if (is.null(c) || is.null(u)) return()
      p <- tryCatch(prefs_leer(c, u$user), error = function(e) NULL)
      shiny::isolate(estado(audiencia_estado(unlist(p$audiencia), todos)))
    })
    shiny::observeEvent(input$nivel, {
      e <- estado(); if (is.null(e)) return()
      estado(audiencia_click(e, input$nivel))
    }, ignoreInit = TRUE)
    seleccion <- shiny::reactive({ e <- estado(); if (is.null(e)) character() else audiencia_seleccion(e, todos) })

    responsables_posibles <- shiny::reactive({
      c <- cid(); if (is.null(c) || !isTRUE(abierta())) return(NULL)
      tryCatch(alertas_usuarios(c), error = function(e) NULL)
    })

    output$para <- shiny::renderUI({
      if (identical(tipo(), "solicitud")) {
        r <- responsables_posibles()
        opciones <- if (is.null(r) || !nrow(r)) character() else stats::setNames(r$usuario, r$nombre)
        return(shiny::div(class = "pan-campo",
          shiny::selectizeInput(ns("responsables"), tr("panorama.publicar.responsables"),
                                choices = opciones, multiple = TRUE,
                                selected = shiny::isolate(input$responsables),
                                options = list(placeholder = tr("panorama.publicar.responsables_ayuda")))))
      }
      e <- estado(); a <- audiencia()
      if (is.null(e)) return(NULL)
      personas <- if (is.null(a)) rep(NA_integer_, length(todos)) else a$personas[match(todos, a$nivel)]
      shiny::div(class = "pan-campo",
        shiny::div(class = "pan-etiqueta", tr("panorama.publicar.para")),
        shiny::div(class = "pan-niveles", role = "group", `aria-label` = tr("panorama.publicar.para"),
          lapply(seq_along(todos), function(i) {
            dentro <- i >= e$desde && i <= e$hasta
            omit <- i %in% e$omitidos
            estado_txt <- if (!dentro) "fuera" else if (omit) "omitido" else "incluido"
            shiny::tags$button(`data-hd-espera` = "no",
              type = "button",
              class = paste("pan-nivel", paste0("pan-nivel--", estado_txt),
                            if (i == e$desde) "pan-nivel--desde", if (i == e$hasta) "pan-nivel--hasta",
                            if (isTRUE(personas[i] == 0)) "pan-nivel--vacio"),
              `aria-pressed` = if (dentro && !omit) "true" else "false",
              `aria-label` = tr(paste0("panorama.publicar.nivel_", estado_txt),
                                nivel = nivel_nombre(todos[i]),
                                n = if (is.na(personas[i])) "?" else as.character(personas[i])),
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("nivel"), i),
              shiny::span(class = "pan-nivel-nombre", nivel_nombre(todos[i])),
              shiny::span(class = "pan-nivel-n",
                          tr("panorama.publicar.personas", n = if (is.na(personas[i])) "?" else as.character(personas[i]))))
          })),
        shiny::div(class = "pan-meta", tr("panorama.publicar.para_ayuda")))
    })

    alcance_n <- shiny::reactive({
      if (identical(tipo(), "solicitud")) return(length(input$responsables %||% character()))
      a <- audiencia(); if (is.null(a)) return(NA_integer_)
      sum(a$personas[a$nivel %in% seleccion()])
    })
    output$alcance <- shiny::renderUI({
      n <- alcance_n()
      a_quien <- if (identical(tipo(), "solicitud")) tr("panorama.publicar.a_personas") else {
        e <- estado(); if (is.null(e)) "" else audiencia_resumen(e, todos) }
      shiny::div(class = "pan-alcance",
        shiny::div(class = "pan-etiqueta", tr("panorama.publicar.llegara")),
        shiny::div(class = "pan-cifra", if (is.na(n)) "—" else tr_n("panorama.publicar.alcance", n)),
        shiny::div(class = "pan-meta", a_quien))
    })

    output$vence_ui <- shiny::renderUI({
      con_fecha <- tipo() %in% c("recordatorio", "solicitud")
      shiny::dateInput(ns("vence"),
        if (con_fecha) tr("panorama.publicar.fecha_limite") else tr("panorama.publicar.visible_hasta"),
        value = shiny::isolate(input$vence) %||% (as.Date(Sys.time(), tz = tz()) + 7),
        min = as.Date(Sys.time(), tz = tz()), language = hd_idioma(), format = "dd/mm/yyyy")
    })

    # ── Preview ───────────────────────────────────────────────────────────────
    output$vista <- shiny::renderUI({
      t <- tipo()
      tono <- switch(t, recordatorio = "ambar", urgente = "ambar", solicitud = "lila", reconocimiento = "lila", "salvia")
      titulo <- trimws(input$titulo %||% ""); mensaje <- trimws(input$mensaje %||% "")
      enlace <- input$enlace %||% ""
      shiny::div(class = "pan-vista",
        shiny::div(class = "pan-etiqueta", tr("panorama.publicar.asi_destacados")),
        shiny::tags$article(class = paste("pan-tarjeta pan-destacado activa", paste0("pan-tono-", tono)),
          shiny::div(class = "pan-destacado-cabeza",
            shiny::span(class = "pan-tag", tr(paste0("panorama.publicar.tipo.", t)))),
          shiny::h2(class = "pan-destacado-titulo",
                    if (nzchar(titulo)) titulo else tr("panorama.publicar.titulo_vacio")),
          if (nzchar(mensaje)) shiny::p(class = "pan-destacado-cuerpo", mensaje),
          shiny::div(class = "pan-destacado-pie",
            shiny::span(class = "pan-autor", tr("panorama.publicar.autor_vista")),
            if (nzchar(enlace)) shiny::span(class = "pan-boton", tr(paste0("panorama.publicar.ir.", tolower(enlace)))))),
        shiny::div(class = "pan-etiqueta", tr("panorama.publicar.y_cinta")),
        shiny::div(class = "pan-cinta pan-cinta--vista",
          shiny::span(class = paste("pan-cinta-badge", if (t %in% c("recordatorio", "urgente")) "pan-cinta-badge--fuerte"),
                      tr(paste0("panorama.publicar.tipo.", t))),
          shiny::span(if (nzchar(titulo)) titulo else tr("panorama.publicar.titulo_vacio"))))
    })

    # ── Publish ───────────────────────────────────────────────────────────────
    shiny::observeEvent(input$publicar, {
      # T91b: editing one's own publication needs no can_publish_panorama
      # (only authorship, checked again in avisos_editar).
      if (!is.null(editando())) { .guardar_edicion(); return() }
      if (!puede_o_avisa(shared, "can_publish_panorama")) return()
      c <- cid(); u <- usr(); if (is.null(c) || is.null(u)) return()
      r <- responsables_posibles()
      p <- panorama_publicacion(
        tipo(), input$titulo, input$mensaje, niveles = seleccion(),
        responsables = input$responsables, vence = input$vence %||% NA, enlace = input$enlace,
        vivos = if (is.null(r)) character() else r$usuario, tz = tz())
      if (!isTRUE(p$ok)) {
        shiny::showNotification(tr(p$error), type = "warning", duration = 6)
        return()
      }
      res <- tryCatch({
        if (identical(tipo(), "solicitud")) {
          a <- p$aviso; a$clave <- paste0("sol:", uuid::UUIDgenerate()); a$autor <- u$user
          avisos_personales(c, p$responsables, a)
        } else avisos_agregar(c, p$aviso, por = u$user)
        TRUE
      }, error = function(e) e)
      tryCatch(log_action(
        user = u$user, module = "panorama",
        action = if (isTRUE(res)) "publicar" else "publicar_fallido",
        description = if (isTRUE(res)) sprintf("Publicó %s: %s", tipo(), trimws(input$titulo %||% ""))
                      else paste0("Falló al publicar: ", conditionMessage(res)),
        target_id = NA_character_,
        metadata = list(tipo = tipo(), niveles = if (identical(tipo(), "solicitud")) NULL else p$aviso$niveles,
                        responsables = p$responsables),
        client_id = c,
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      ), error = function(e) NULL)
      if (!isTRUE(res)) {
        message("[panorama] publicar falló: ", conditionMessage(res))
        shiny::showNotification(tr("comun.error_guardar"), type = "error", duration = 6)
        return()   # the form keeps what was written
      }
      if (!identical(tipo(), "solicitud") && !es_staff())
        tryCatch(prefs_guardar(c, u$user, list(audiencia = p$aviso$niveles)),
                 error = function(e) message("[panorama] prefs_guardar falló: ", conditionMessage(e)))
      session$sendInputMessage("titulo", list(value = ""))
      session$sendInputMessage("mensaje", list(value = ""))
      shiny::updateSelectizeInput(session, "responsables", selected = character())
      version(version() + 1L)
      al_publicar()
      shiny::showNotification(tr("panorama.publicar.publicado"), type = "message", duration = 4)
    }, ignoreInit = TRUE)

    # T91b: saves the edit in place. The form's checks are Publicar's
    # (panorama_publicacion); the kind never changes; a solicitud keeps its
    # recipients. Logged with the text and audience before and after.
    .guardar_edicion <- function() {
      e <- editando(); c <- cid(); u <- usr(); if (is.null(e) || is.null(c) || is.null(u)) return()
      p <- panorama_publicacion(
        e$tipo, input$titulo, input$mensaje, niveles = seleccion(),
        responsables = e$responsables, vence = input$vence %||% NA, enlace = NA,
        vivos = e$responsables, tz = tz())
      if (!isTRUE(p$ok)) { shiny::showNotification(tr(p$error), type = "warning", duration = 6); return() }
      cambios <- list(titulo = p$aviso$titulo, cuerpo = p$aviso$cuerpo, vence = p$aviso$vence,
                      niveles = if (!identical(e$tipo, "solicitud")) p$aviso$niveles)
      # avisos_editar logs the edit itself (editar_publicacion, before/after);
      # here only a failure is logged.
      casa <- tryCatch(shared$home_client_id(), error = function(e) NULL)
      res <- tryCatch(avisos_editar(c, e$id, cambios, por = u$user, viewer_home = casa), error = function(err) err)
      ok <- !inherits(res, "error")
      if (!ok) tryCatch(log_action(
        user = u$user, module = "panorama", action = "editar_publicacion_fallido",
        description = paste0("Falló al editar: ", conditionMessage(res)),
        target_id = e$id, metadata = list(tipo = e$tipo),
        client_id = c, viewer_home_client_id = casa
      ), error = function(e) NULL)
      if (!ok) {
        message("[panorama] editar falló: ", conditionMessage(res))
        shiny::showNotification(tr("comun.error_guardar"), type = "error", duration = 6)
        return()   # the form keeps what was written
      }
      editando(NULL); .limpiar_formulario(); .ver_seccion("publicadas")
      version(version() + 1L); al_publicar()
      shiny::showNotification(tr(if (isTRUE(res$cambiado)) "panorama.publicar.editada" else "panorama.publicar.sin_cambios"),
                              type = "message", duration = 4)
    }

    # ── Publicadas ────────────────────────────────────────────────────────────
    publicadas <- shiny::reactive({
      version()
      vacia <- panorama_publicadas(NULL)
      if (!isTRUE(abierta())) return(vacia)   # T91b: one's own, with or without can_publish_panorama
      c <- cid(); if (is.null(c)) return(vacia)
      # A failed read stop()s: the table shows its "could not load" notice,
      # never an empty list that looks like nothing is published.
      # T91a: one's own publications; everyone's only with can_moderate_panorama.
      p <- panorama_publicadas(avisos_leer(c))
      if (isTRUE(tryCatch(puede(usr(), "can_moderate_panorama"), error = function(e) FALSE))) p
      else p[tolower(trimws(p$por)) %in% tolower(usr()$user %||% ""), , drop = FALSE]
    })
    output$n_vigentes <- shiny::renderText(tryCatch(nrow(publicadas()), error = function(e) ""))
    tipo_mapa <- list(
      comunicado     = list(texto = "panorama.publicar.tipo.comunicado",     tono = "info"),
      recordatorio   = list(texto = "panorama.publicar.tipo.recordatorio",   tono = "aviso"),
      solicitud      = list(texto = "panorama.publicar.tipo.solicitud",      tono = "morado"),
      reconocimiento = list(texto = "panorama.publicar.tipo.reconocimiento", tono = "exito"),
      urgente        = list(texto = "panorama.publicar.tipo.urgente",        tono = "peligro"),
      nota           = list(texto = "panorama.publicar.tipo.nota",           tono = "info"))   # T91b
    tabla <- hd_table_server(
      "tabla", tabla = "avisos_publicados", datos = publicadas,
      columnas = list(
        col_insignia("tipo", "panorama.publicar.col.tipo", mapa = tipo_mapa),
        col_texto("titulo", "panorama.publicar.col.titulo"),
        col_texto("para", "panorama.publicar.col.para"),
        col_texto("por", "panorama.publicar.col.por"),
        col_fecha_hora("creado", "panorama.publicar.col.creado"),
        col_fecha_hora("vence", "panorama.publicar.col.vence"),
        col_fecha_hora("editado_en", "panorama.publicar.col.editado")),   # T91b
      acciones_fila = c("editar", "eliminar"),   # T91b: Editar (own) and Borrar (own, or moderator)
      vacio = "panorama.publicar.vacio")

    shiny::observeEvent(tabla$accion(), {
      a <- tabla$accion()
      if (!isTRUE(a$accion %in% c("editar", "eliminar")) || !length(a$ids)) return()
      c <- cid(); u <- usr(); if (is.null(c) || is.null(u)) return()
      d <- publicadas()
      filas <- d[d$id %in% a$ids, , drop = FALSE]
      if (!nrow(filas)) return()
      if (identical(a$accion, "editar")) { .abrir_edicion(filas[1, , drop = FALSE], c, u); return() }
      # T91b: a note is deleted in Notas del mes (its note and its aviso).
      if (any(filas$tipo %in% "nota")) {
        shiny::showNotification(tr("panorama.publicar.nota_en_notas"), type = "warning", duration = 6)
        filas <- filas[!(filas$tipo %in% "nota"), , drop = FALSE]
        if (!nrow(filas)) return()
      }
      # T91a, checked on the server row by row (the ids come from the
      # browser): someone else's publication only with can_moderate_panorama.
      r <- publicadas_retirables(filas, u)
      if (r$rechazadas > 0L)
        shiny::showNotification(tr("panorama.publicar.solo_propias"), type = "warning", duration = 6)
      filas <- r$filas
      if (!nrow(filas)) return()
      ids <- unlist(strsplit(filas$ids, ",", fixed = TRUE))
      res <- tryCatch({ for (i in ids) avisos_retirar(c, i, u$user); TRUE }, error = function(e) e)
      tryCatch(log_action(
        user = u$user, module = "panorama",
        action = if (isTRUE(res)) "desactivar_aviso" else "desactivar_aviso_fallido",
        description = if (isTRUE(res)) paste("Desactivó:", paste(filas$titulo, collapse = "; "))
                      else paste0("Falló al desactivar: ", conditionMessage(res)),
        target_id = paste(filas$id, collapse = ","), metadata = list(ids = ids),
        client_id = c,
        viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
      ), error = function(e) NULL)
      if (!isTRUE(res)) {
        shiny::showNotification(tr("comun.error_guardar"), type = "error", duration = 6)
      } else {
        shiny::showNotification(tr_n("panorama.publicar.desactivados", nrow(filas)), type = "message", duration = 4)
      }
      version(version() + 1L)
      al_publicar()
    }, ignoreInit = TRUE)

    # T91b: opens the form on one's own publication (checked on the server:
    # the id comes from the browser), with what it says now.
    .abrir_edicion <- function(fila, c, u) {
      r <- publicadas_editables(fila, u)
      if (r$rechazadas > 0L) {
        shiny::showNotification(tr(if (fila$tipo %in% "nota") "panorama.publicar.nota_en_notas" else "panorama.publicar.solo_autor_edita"),
                                type = "warning", duration = 6)
        return()
      }
      av <- tryCatch(avisos_leer(c), error = function(e) NULL)
      if (is.null(av)) { shiny::showNotification(tr("comun.error_guardar"), type = "error"); return() }
      ids <- unlist(strsplit(fila$ids, ",", fixed = TRUE))
      x <- av[av$id %in% ids, , drop = FALSE]
      if (!nrow(x)) return()
      t <- if (identical(fila$tipo, "solicitud")) "solicitud" else if (x$severidad[1] %in% "urgente") "urgente" else avisos_clase(x[1, ])
      editando(list(id = fila$id, tipo = t, titulo = x$titulo[1],
                    responsables = unique(unlist(lapply(x$usuarios, .de_json)))))
      tipo(t)
      session$sendInputMessage("titulo", list(value = x$titulo[1] %||% ""))
      session$sendInputMessage("mensaje", list(value = x$cuerpo[1] %||% ""))
      if (!is.na(x$vence[1])) shiny::updateDateInput(session, "vence", value = as.Date(x$vence[1], tz = tz()))
      if (!identical(t, "solicitud")) estado(audiencia_estado(.de_json(x$niveles[1]), todos))
      .ver_seccion("nueva")   # the form, even for an author who no longer publishes
    }

    # T91b (the T106 pattern): the form's outputs keep drawing while their
    # section is hidden -- an edit fills them from Mis publicaciones, where
    # the form is not shown, and a suspended output came back stale (the
    # "Editando" band empty, the preview "Escribe un titulo"; Chrome).
    for (o in c("edicion", "vista", "tipos", "para", "alcance", "vence_ui"))
      shiny::outputOptions(output, o, suspendWhenHidden = FALSE)

    invisible(list(tabla = tabla))
  })
}
