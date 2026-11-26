# =============================================================================
# R/empresas_module.R
# Panel "Empresas" — gestión de empresas del grupo.
# Visible para dev y admin.
# Companies are the source of truth for initials/names throughout the app.
# Soft delete only — companies are never truly removed from storage.
# =============================================================================

# ── Helpers ───────────────────────────────────────────────────────────────────

# The group's logo (group_config.rds: logo_raw + logo_ext) as an <img> of
# `alto` px, inline (base64), for Ajustes › Configuración del Grupo and the
# "Empresa" line of a new invitation (T58). NULL without a logo.
logo_grupo_img <- function(logo_raw, logo_ext = "png", alto = 32) {
  if (is.null(logo_raw) || !is.raw(logo_raw) || !length(logo_raw)) return(NULL)
  mime <- if (tolower(logo_ext %||% "png") %in% c("jpg", "jpeg")) "image/jpeg" else "image/png"
  htmltools::tags$img(src = paste0("data:", mime, ";base64,", base64enc::base64encode(logo_raw)),
                      alt = "", class = "logo-grupo",
                      style = sprintf("max-height:%dpx;max-width:%dpx;object-fit:contain;", as.integer(alto), 4L * as.integer(alto)))
}

.next_empresa_code <- function(all_e) {
  if (is.null(all_e) || !nrow(all_e) || !"account_code" %in% names(all_e))
    return("E0001")
  codes <- all_e$account_code
  nums  <- suppressWarnings(as.integer(sub("^E0*", "", codes[grepl("^E\\d+$", codes)])))
  nums  <- nums[!is.na(nums)]
  if (!length(nums)) return("E0001")
  sprintf("E%04d", max(nums) + 1L)
}

# iniciales_anteriores after a COMPLETE rename OLD -> NEW (CM29): OLD goes in
# and NEW comes out, so a chain A -> B -> C keeps A and B, and going back
# B -> A takes A out and puts B in. Written only when the cascade finished:
# with hd_renombrar_incompleto the company keeps OLD and nothing is added.
.iniciales_anteriores_tras <- function(x, old_ini, new_ini) {
  a <- .iniciales_anteriores_de(x)[[1]]
  a <- setdiff(unique(c(a, toupper(trimws(old_ini)))), toupper(trimws(new_ini)))
  as.character(jsonlite::toJSON(a, auto_unbox = FALSE))
}

# Initials another company had before (every row but `id`'s own). Taking
# them is refused: what tables still hold under them would be translated to
# that other company (iniciales_vigentes()).
.iniciales_de_antes_ajenas <- function(all_e, id = NULL) {
  if (is.null(all_e) || !nrow(all_e) || !"iniciales_anteriores" %in% names(all_e)) return(character(0))
  otras <- if (is.null(id)) rep(TRUE, nrow(all_e)) else as.character(all_e$id) != id
  unique(unlist(.iniciales_anteriores_de(all_e$iniciales_anteriores[otras])))
}

# A new company checked against S3's table as it is now (`fresco`, CM39), not
# only against the session's copy: two sessions creating the same initials
# at once both passed the copy's check and both were written. Stops with a
# classed condition (nothing is written); the caller tells the user.
.empresa_rechazo <- function(clase, ...)
  stop(structure(class = c(clase, "empresa_rechazada", "error", "condition"),
                 c(list(message = clase, call = NULL), list(...))))
# CM48a: the nombre_corto IS the Empresa of every SAP invoice, and nine
# tables match on it (CM47). Two companies sharing one would cross their
# confirmations, overrides, moves and abonos. Compared without case or
# spaces, against EVERY row -- a deleted company's stored rows still carry
# its name, and a new company with it would inherit them.
.nombre_corto_llave <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  tolower(gsub("[[:space:]]+", "", x))
}
.nombre_corto_ocupado <- function(empresas, nombre) {
  !is.null(empresas) && nrow(empresas) && "nombre_corto" %in% names(empresas) &&
    nzchar(.nombre_corto_llave(nombre)) &&
    .nombre_corto_llave(nombre) %in% .nombre_corto_llave(empresas$nombre_corto)
}

.validar_alta_empresa <- function(initials, nombre_corto = NULL) function(fresco) {
  if (nrow(fresco) && initials %in% toupper(trimws(fresco$initials)))
    .empresa_rechazo("empresa_iniciales_repetidas")
  if (!is.null(nombre_corto) && .nombre_corto_ocupado(fresco, nombre_corto))   # CM48a
    .empresa_rechazo("empresa_nombre_corto_repetido", nombre_corto = nombre_corto)
  if (initials %in% .iniciales_de_antes_ajenas(fresco))   # CM29
    .empresa_rechazo("empresa_iniciales_de_antes")
  if (initials %in% .reservas_renombre(fresco))            # CM40: a rename in progress holds them
    .empresa_rechazo("empresa_iniciales_reservadas", iniciales = initials)
  invisible(TRUE)
}

# ── Renaming, in three steps (CM40) ──────────────────────────────────────────
# 1. reservar_renombre_empresa(): NEW is reserved on the company's row in
#    empresas.rds (renombre_pendiente), checked on S3's table as it is now;
# 2. rename_empresa_initials() (R/persistence.R): the cascade, which runs only
#    on a reserved OLD -> NEW;
# 3. confirmar_renombre_empresa() (or the edit's save, which applies the same
#    .empresa_aplicar_campos()): initials NEW, OLD among the earlier ones,
#    the reservation cleared.
# Two sessions renaming two companies to the same NEW, or one company to two,
# used to pass the same check and run their cascades over each other. Now
# the second is refused at step 1, before any table is touched. A cascade
# that fails leaves the reservation: the same rename again (Reintentar)
# finishes it; nothing is undone.

# The initials reserved by renames in progress (upper case), every row or all
# but `id`'s.
.reservas_renombre <- function(fresco, id = NULL) {
  if (!is.data.frame(fresco) || !nrow(fresco) || !"renombre_pendiente" %in% names(fresco)) return(character(0))
  otras <- if (is.null(id)) rep(TRUE, nrow(fresco)) else as.character(fresco$id) != id
  p <- toupper(trimws(fresco$renombre_pendiente[otras]))
  unique(p[!is.na(p) & nzchar(p)])
}

# Step 1. Refused (an "empresa_rechazada" condition, nothing written) when:
# the company is gone; it is being renamed to other initials
# (empresa_renombre_en_curso, $iniciales); its initials are no longer OLD
# (empresa_renombrada_otra_sesion, $iniciales); NEW is another company's
# initials or reserved by another rename (empresa_iniciales_reservadas);
# NEW is another company's earlier initials (empresa_iniciales_de_antes).
# The same reservation again (a retry) writes nothing and goes on. Returns
# empresas.rds as written.
reservar_renombre_empresa <- function(id, old_ini, new_ini, client_id) {
  old_ini <- toupper(trimws(old_ini)); new_ini <- toupper(trimws(new_ini))
  modificar_tabla(S3_KEYS$empresas, client_id, .schema_empresas, function(fresco) {
    f <- which(as.character(fresco$id) == id)
    if (length(f) != 1L) .empresa_rechazo("empresa_no_existe")
    ini  <- toupper(trimws(fresco$initials))
    pend <- toupper(trimws(fresco$renombre_pendiente)); pend[is.na(pend)] <- ""
    if (nzchar(pend[f])) {
      if (pend[f] == new_ini && ini[f] %in% c(old_ini, new_ini)) return(fresco)   # a retry
      .empresa_rechazo("empresa_renombre_en_curso", iniciales = pend[f])
    }
    if (!identical(ini[f], old_ini)) .empresa_rechazo("empresa_renombrada_otra_sesion", iniciales = ini[f])
    if (new_ini %in% ini[-f] || new_ini %in% .reservas_renombre(fresco, id))
      .empresa_rechazo("empresa_iniciales_reservadas", iniciales = new_ini)
    if (new_ini %in% .iniciales_de_antes_ajenas(fresco, id)) .empresa_rechazo("empresa_iniciales_de_antes")
    fresco$renombre_pendiente[f] <- new_ini
    fresco
  })
}

# Step 3, outside the edit panel (the edit's save does the same through
# .save_and_broadcast()). Returns empresas.rds as written.
confirmar_renombre_empresa <- function(id, old_ini, new_ini, client_id) {
  aplicar <- .empresa_aplicar_campos(id, list(), toupper(trimws(old_ini)), toupper(trimws(new_ini)))
  modificar_tabla(S3_KEYS$empresas, client_id, .schema_empresas, function(fresco) aplicar(fresco, fresco))
}

# The new company's account_code made free against S3's table (CM39): it was
# computed on the session's copy, so two creates at once -- even with
# different initials -- took the same E000N. The user never chose it, so a
# taken code is moved to the next free one, not refused. Pure.
.codigo_libre_empresa <- function(id) function(nuevo, fresco) {
  i <- which(as.character(nuevo$id) == id)
  usados <- fresco$account_code[!is.na(fresco$account_code)]
  if (length(i) == 1L && nuevo$account_code[i] %in% usados)
    nuevo$account_code[i] <- .next_empresa_code(fresco)
  nuevo
}

# The fields an edit changed (CM40): each value from the form that differs
# from the row the panel was drawn with (`fila`), as it would be saved.
# Guardar applies only these onto S3's current row: the whole row of the
# session's copy used to win, and a copy loaded before another session's
# rename wrote the old initials back (every table already under the new ones).
# CM47: the nombre_corto IS the Empresa of every SAP invoice (sap_api.R,
# company_map_rv()), and nine tables store it to match against it
# (bancos_confirmados, papelera, pagar_hoy, moves, policy_moves, tags,
# sap_overrides, abonos, manual_inv). No cascade follows an edit, so changing
# it reopened confirmations and lost moves, overrides and abonos. It is set
# at creation and fixed afterwards; this tells whether a save would change it.
.nombre_corto_cambia <- function(fila, valor) {
  valor <- if (length(valor) && !is.na(valor[1])) trimws(as.character(valor[1])) else ""
  "nombre_corto" %in% names(.empresa_campos_editados(fila, list(nombre_corto = valor)))
}

.empresa_campos_editados <- function(fila, valores) {
  igual <- function(a, b) identical(if (length(a) && !is.na(a)) as.character(a) else "",
                                    if (length(b) && !is.na(b)) as.character(b) else "")
  alt <- function(x) tryCatch(as.character(jsonlite::toJSON(as.character(jsonlite::fromJSON(x %||% "[]")),
                                                            auto_unbox = FALSE)),
                              error = function(e) "[]")
  antes <- list(razon_social = trimws(fila$razon_social %||% ""),
                nombre_corto = trimws(fila$nombre_corto %||% ""),
                rfc          = toupper(trimws(fila$rfc %||% "")),
                activa       = isTRUE(fila$activa),
                nombres_alt  = alt(fila$nombres_alt))
  valores$nombres_alt <- alt(valores$nombres_alt)
  valores[vapply(names(valores), function(k) !igual(valores[[k]], antes[[k]]), logical(1))]
}

# ajustar(nuevo, fresco) for an edit (CM40): the company's row is S3's
# current one with the edited fields set -- and, for a rename OLD -> NEW,
# the initials and its earlier ones (CM29) computed on S3's row, not the
# copy's. A company no longer in empresas.rds stops it, nothing written.
.empresa_aplicar_campos <- function(id, cambios, old_ini = NULL, new_ini = NULL) function(nuevo, fresco) {
  i <- which(as.character(nuevo$id) == id); f <- which(as.character(fresco$id) == id)
  if (length(i) != 1L || length(f) != 1L)
    stop("[EMP] the company is no longer in empresas.rds; nothing was saved", call. = FALSE)
  fila <- fresco[f, , drop = FALSE]
  for (k in names(cambios)) fila[[k]] <- cambios[[k]]
  if (!is.null(new_ini) && !identical(old_ini, new_ini)) {
    fila$initials <- new_ini
    fila$iniciales_anteriores <- .iniciales_anteriores_tras(fila$iniciales_anteriores, old_ini, new_ini)
    fila$renombre_pendiente <- NA_character_   # confirmed (CM40)
  }
  fila$updated_at <- as.character(Sys.time())
  nuevo[i, names(fila)] <- fila
  nuevo
}

# ── UI ────────────────────────────────────────────────────────────────────────

empresasUI <- function(id) {
  ns <- NS(id)

  div(
    class = "p-3 hd-scroll-pestana",   # T56: room at the end for "Notas del mes" (R/ui_components.R)
    style = "overflow-y: auto; max-height: calc(100vh - var(--hd-barra-alto) - 17px);",   # the bar's height (T51)

    # Header — same structure as tiers module
    div(
      class = "tiers-header",
      icon("building", class = "fa-2x"),
      div(
        tags$h4(class = "mb-0 fw-bold", "Empresas del Grupo"),
        tags$p(class = "mb-0 small opacity-75",
               "Razones sociales, iniciales y datos de las empresas")
      ),
      uiOutput(ns("header_badge"))
    ),

    div(
      class = "mt-3",
      fluidRow(
        # ── Left: table ──────────────────────────────────────────────────────
        column(
          width = 7,
          div(
            class = "card border-0 shadow-sm",
            div(
              class = "card-header bg-white py-2 d-flex align-items-center justify-content-between",
              tags$span(class = "fw-semibold",
                        tagList(icon("building"), " Empresas registradas")),
              actionButton(ns("btn_new_empresa"),
                           tagList(icon("circle-plus"), " Nueva empresa"),
                           class = "btn btn-sm btn-outline-primary")
            ),

            # ── Create form (hidden by default) ──────────────────────────────
            shinyjs::hidden(
              div(
                id = ns("create_form_wrap"),
                class = "card-body border-bottom",
                style = "background:var(--hd-superficie-2);",
                tags$h6(class = "fw-semibold mb-3", style = "color:var(--hd-primario);",
                        tagList(icon("circle-plus"), " Nueva empresa")),
                fluidRow(
                  column(3,
                    textInput(ns("new_initials"), "Iniciales",
                              placeholder = "Ej: NCS")
                  ),
                  column(9,
                    textInput(ns("new_razon_social"), "Razón Social",
                              placeholder = "Ej: Networks Crossdocking Services S de RL de CV")
                  )
                ),
                fluidRow(
                  column(6,
                    textInput(ns("new_nombre_corto"), "Nombre corto para mostrar",
                              placeholder = "Ej: Networks Crossdocking")
                  ),
                  column(6,
                    textInput(ns("new_rfc"), "RFC",
                              placeholder = "Ej: NCS123456AB1")
                  )
                ),
                textAreaInput(
                  ns("new_nombres_alt"),
                  "Nombres alternativos",
                  placeholder = "Uno por línea — variaciones del nombre para búsquedas y reportes",
                  rows = 2,
                  width = "100%"
                ),
                div(
                  class = "d-flex align-items-center justify-content-between mt-2",
                  checkboxInput(ns("new_activa"), "Empresa activa", value = TRUE),
                  div(
                    class = "d-flex gap-2",
                    actionButton(ns("btn_save_new_empresa"),
                                 tagList(icon("floppy-disk"), " Crear empresa"),
                                 class = "btn btn-primary btn-sm"),
                    actionButton(ns("btn_cancel_new_empresa"), "Cancelar",
                                 class = "btn btn-outline-secondary btn-sm")
                  )
                )
              )
            ),

            div(class = "card-body p-0",
                hd_table_ui(ns("tabla"), alto = "auto"))
          )
        ),

        # ── Right: edit panel ─────────────────────────────────────────────────
        column(width = 5, uiOutput(ns("edit_panel_ui")))
      ),

      div(
        class = "alert alert-light border small mt-3",
        icon("circle-info", class = "text-primary"),
        tags$strong(" Empresas del grupo: "),
        "Las ", tags$strong("iniciales"), " identifican a cada empresa en todo el sistema. ",
        "Puedes modificarlas desde el panel de edición — el cambio se propagará automáticamente ",
        "a cuentas bancarias, proveedores y mapas de intercompañía. ",
        "Una empresa desactivada queda oculta, pero sus datos históricos se conservan para auditoría."
      ),

      uiOutput(ns("group_config_section"))
    )
  )
}

# ── Edit panel ────────────────────────────────────────────────────────────────

.empresas_edit_panel_ui <- function(ns, row) {
  nombres_alt_text <- tryCatch(
    paste(jsonlite::fromJSON(row$nombres_alt %||% "[]"), collapse = "\n"),
    error = function(e) ""
  )

  div(
    class = "card border-0 shadow-sm",
    div(
      class = "card-header py-3",
      style = "background: linear-gradient(120deg,var(--hd-leg-oscuro-fondo-2-1a1a2e) 0%,var(--hd-leg-oscuro-fondo-16213e) 100%); color:var(--hd-texto-inverso); border-radius:8px 8px 0 0;",
      div(
        class = "d-flex align-items-center gap-3",
        div(
          class = "rounded-circle d-flex align-items-center justify-content-center flex-shrink-0",
          style = "width:42px; height:42px; background:color-mix(in srgb, var(--hd-texto-inverso) 15%, transparent); font-size:1.1rem;",
          icon("building-pen")
        ),
        div(
          tags$p(class = "mb-0",
                 style = "font-size:.7rem; opacity:.65; text-transform:uppercase; letter-spacing:.8px;",
                 "Editando empresa"),
          tags$h5(class = "mb-0 fw-bold", style = "font-size:1.05rem;",
                  row$nombre_corto %||% row$razon_social %||% row$initials),
          div(
            class = "d-flex align-items-center gap-2 mt-1",
            tags$span(
              style = "font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; background:var(--hd-primario); border:1px solid color-mix(in srgb, var(--hd-texto-inverso) 30%, transparent); padding:2px 8px; border-radius:20px;",
              row$initials
            ),
            tags$span(style = "font-size:.75rem; opacity:.6;", row$account_code %||% "")
          )
        )
      )
    ),
    div(
      class = "card-body p-3",

      # A rename in progress (CM40): its cascade or its confirmation failed.
      # Reintentar finishes that same rename; nothing else is offered.
      if (!is.na(row$renombre_pendiente %||% NA) && nzchar(trimws(row$renombre_pendiente)))
        div(class = "alert alert-warning d-flex align-items-center gap-2 py-2",
            tags$span(tr("tablas.empresas.renombre_pendiente_detalle",
                         antes = row$initials, despues = row$renombre_pendiente)),
            actionButton(ns("btn_reintentar_renombre"), tr("tablas.empresas.reintentar_renombre"),
                         class = "btn btn-warning btn-sm ms-auto")),

      textInput(ns("edit_razon_social"), "Razón Social",
                value = row$razon_social %||% "", width = "100%"),

      fluidRow(
        column(7,
          # CM47: fixed once the company exists (see .nombre_corto_cambia()).
          shinyjs::disabled(textInput(ns("edit_nombre_corto"), "Nombre corto",
                                      value = row$nombre_corto %||% "", width = "100%")),
          tags$small(class = "text-muted d-block mb-2", tr("tablas.empresas.nombre_corto_fijo"))
        ),
        column(5,
          textInput(ns("edit_rfc"), "RFC",
                    value = row$rfc %||% "", width = "100%")
        )
      ),

      textAreaInput(
        ns("edit_nombres_alt"),
        "Nombres alternativos",
        value       = nombres_alt_text,
        placeholder = "Uno por línea",
        rows        = 3,
        width       = "100%"
      ),

      fluidRow(
        column(4,
          textInput(ns("edit_initials"), "Iniciales",
                    value = row$initials, width = "100%"),
          # The initials this form was drawn with (CM40), sent from the same
          # page as edit_initials: a rename is "edit_initials differs from
          # these", never from the session's copy, which another session's
          # rename may have reloaded before the page is drawn again.
          div(style = "display:none",
              textInput(ns("edit_ini_apertura"), NULL, value = row$initials))
        ),
        column(8,
          div(class = "mt-4 pt-1",
              checkboxInput(ns("edit_activa"), "Empresa activa",
                            value = isTRUE(row$activa)))
        )
      ),

      div(
        class = "d-flex gap-2",
        actionButton(ns("btn_save_edit"),
                     tagList(icon("floppy-disk"), " Guardar cambios"),
                     class = "btn btn-primary btn-sm"),
        actionButton(ns("btn_cancel_edit"), "Cancelar",
                     class = "btn btn-outline-secondary btn-sm")
      )
    )
  )
}

# Is the session's copy (`base`) another client's? (CM38b) A company's id is
# a UUID made when it is created, and a company is never removed from
# empresas.rds (deleting marks it). So a copy with companies none of which
# is in S3's current table (`fresco`) is not this client's -- or so stale it
# cannot be rebased. An empty copy (a first company) is fine.
.empresas_copia_ajena <- function(base, fresco) {
  ids <- as.character(base$id); ids <- ids[!is.na(ids) & nzchar(ids)]
  length(ids) > 0L && !any(ids %in% as.character(fresco$id))
}

# ── Server ────────────────────────────────────────────────────────────────────

empresasServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_tier  <- reactive({
      tryCatch(shared$current_user_info()$tier, error = function(e) "")
    })
    # Authorized if tier is dev/admin OR if the user has the can_manage_empresas
    # permission explicitly granted (allows future per-user overrides).
    req_authorized <- reactive({
      puede(shared$usuario(), "can_manage_empresas")
    })

    # ── Header badge ──────────────────────────────────────────────────────────
    output$header_badge <- renderUI({
      tier <- current_tier()
      if (!nzchar(tier)) return(NULL)
      tags$span(class = "ms-auto tiers-dev-badge", toupper(tier))
    })

    # ── Data layer ────────────────────────────────────────────────────────────
    # CM38b: one copy only, shared$empresas_db(), which the context switch
    # reloads. A module-local copy filled on save (.empresas_local) outlived
    # a jump: staff saved in A, jumped to B, saw A's companies and the next
    # save wrote A's list into B/empresas.rds.
    all_empresas_rv <- reactive({
      req(req_authorized())
      # Consume shared$empresas_db — populated by the context-switch observer.
      # Never read from S3 directly here: that would bypass the active client
      # context and always read from the env-var prefix (hd-admin).
      shared$empresas_db()
    })

    # Save an action's result (`df`: the copy it started from, changed).
    # Rebased on S3's current table (CM38b): only the rows this action added,
    # changed or removed are applied, every other row stays as S3 holds it;
    # never the session's whole copy. A copy that is not this client's
    # (.empresas_copia_ajena()) stops it with nothing written. Throws on any
    # failure; callers notify and say nothing was saved.
    # `validar(fresco)` (CM39) checks the action against S3's current table
    # before anything is written; when it refuses (an "empresa_rechazada"
    # condition), the session is shown what S3 holds -- the other session's
    # company included -- and the condition goes on to the caller.
    # `ajustar(nuevo, fresco)` (CM39) may change the action's rows against
    # S3's table before the rebase (a new company's account_code). Returns
    # the table written: what the caller reports comes from there.
    .save_and_broadcast <- function(df, validar = NULL, ajustar = NULL) {
      cid   <- shared$effective_client_id()
      base  <- .normalize(isolate(all_empresas_rv()) %||% .schema_empresas(), .schema_empresas)
      nuevo <- .normalize(df, .schema_empresas)
      visto <- NULL
      guardado <- withCallingHandlers(
        modificar_tabla(S3_KEYS$empresas, cid, .schema_empresas, function(fresco) {
          if (.empresas_copia_ajena(base, fresco))
            stop("[EMP] the session's companies are not this client's; nothing was saved", call. = FALSE)
          visto <<- fresco
          if (is.function(validar)) validar(fresco)
          propio <- if (is.function(ajustar)) .normalize(ajustar(nuevo, fresco), .schema_empresas) else nuevo
          rebase_por_id(propio, base, fresco, "id")
        }),
        empresa_rechazada = function(e)
          if (!is.null(visto) && !is.null(shared$empresas_db) && is.function(shared$empresas_db))
            shared$empresas_db(visto))
      bump_sync_version("empresas_db", client_id = cid)
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db))
        shared$empresas_db(guardado)   # what S3 holds now, other sessions' rows included
      invisible(guardado)
    }

    # Visible (non-deleted) companies, filtered to the user's allowed initials.
    empresas_rv <- reactive({
      df <- all_empresas_rv()
      if (is.null(df)) return(.schema_empresas())
      if (nrow(df) > 0 && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      # Respect group-based company visibility (NULL = no filter; character(0) = none)
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed) && nrow(df) > 0 && "initials" %in% names(df))
        df <- df[df$initials %in% allowed, , drop = FALSE]
      df
    })

    # ── Companies table (hd_table) ────────────────────────────────────────────
    # Row buttons and the double click bring the company's id. The edit panel
    # and the deactivate step keep that id, never a position in empresas_rv():
    # another session's change can reorder or shrink that list while the panel
    # is open, and a position would then point at a different company.
    empresas_tabla <- reactive({
      req(req_authorized())
      empresas <- empresas_rv()
      req(!is.null(empresas))
      # Stage 3: the ERP button only when the ERP feature itself is visible in
      # this context (see erp_ui_visible()). Ordinary client-facing
      # functionality; hidden for a staff-at-home session exactly like
      # FINANCIAL_TABS hides Calendario/Vencidos/etc. for that same context.
      acciones <- c("editar", if (isTRUE(erp_ui_visible())) "erp", "desactivar")
      data.frame(
        id           = as.character(empresas$id),
        account_code = empresas$account_code,
        initials     = empresas$initials,
        razon_social = empresas$razon_social,
        rfc          = empresas$rfc,
        # A rename in progress (CM40) shows first: it needs Reintentar.
        estado       = ifelse(!is.na(empresas$renombre_pendiente) & nzchar(trimws(empresas$renombre_pendiente)),
                              "renombre_pendiente",
                              ifelse(!is.na(empresas$activa) & empresas$activa == TRUE, "activa", "inactiva")),
        .acciones    = rep(paste(acciones, collapse = ","), nrow(empresas)),
        stringsAsFactors = FALSE
      )
    })

    tabla <- hd_table_server(
      "tabla", tabla = "empresas", datos = empresas_tabla,
      columnas = list(
        col_texto("account_code", "tablas.empresas.codigo", recorte = FALSE),
        col_texto("initials",     "tablas.empresas.iniciales", recorte = FALSE),
        col_texto("razon_social", "tablas.empresas.razon_social"),
        col_texto("rfc",          "tablas.empresas.rfc", recorte = FALSE),
        col_insignia("estado",    "tablas.empresas.estado", mapa = list(
          activa   = list(texto = "tablas.empresas.activa",   tono = "exito"),
          inactiva = list(texto = "tablas.empresas.inactiva", tono = "neutro"),
          renombre_pendiente = list(texto = "tablas.empresas.renombre_pendiente", tono = "aviso")))
      ),
      acciones_fila = c("editar", "erp", "desactivar"),
      filtros       = list(list(campo = "estado")),
      vacio         = "tablas.empresas.vacio"
    )

    observeEvent(tabla$accion(), {
      a  <- tabla$accion()
      id <- a$ids[1]
      if (is.null(id) || is.na(id)) return()
      switch(a$accion,
        editar     = ,
        detalle    = selected_id(id),
        erp        = .abrir_erp(id),
        desactivar = .desactivar_empresa(id),
        NULL)
    })

    # ── New empresa form ──────────────────────────────────────────────────────
    observeEvent(input$btn_new_empresa,        { shinyjs::toggle("create_form_wrap") })
    observeEvent(input$btn_cancel_new_empresa, { shinyjs::hide("create_form_wrap")  })

    observeEvent(input$btn_save_new_empresa, {
      initials     <- toupper(trimws(input$new_initials     %||% ""))
      razon_social <- trimws(input$new_razon_social %||% "")
      nombre_corto <- trimws(input$new_nombre_corto %||% "")
      rfc          <- toupper(trimws(input$new_rfc          %||% ""))

      if (!nzchar(initials) || !grepl("^[A-Z0-9]+$", initials)) {
        showNotification(tr("tablas.empresas.aviso.iniciales_invalidas"),
                         type = "error"); return()
      }
      if (!nzchar(razon_social)) {
        showNotification(tr("tablas.empresas.aviso.razon_social_obligatoria"), type = "error"); return()
      }

      all_e <- all_empresas_rv()
      if (!is.null(all_e) && nrow(all_e) &&
          initials %in% toupper(trimws(all_e$initials))) {
        showNotification(tr("tablas.empresas.aviso.iniciales_existe"), type = "error"); return()
      }
      if (initials %in% .iniciales_de_antes_ajenas(all_e)) {   # CM29
        showNotification(tr("tablas.empresas.aviso.iniciales_de_antes"), type = "error"); return()
      }
      # CM48a: the name the company will carry (razón social when left empty).
      nombre_final <- if (nzchar(nombre_corto)) nombre_corto else razon_social
      if (.nombre_corto_ocupado(all_e, nombre_final)) {
        showNotification(tr("tablas.empresas.aviso.nombre_corto_existe", nombre = nombre_final),
                         type = "error", duration = 8); return()
      }

      alt_lines <- trimws(strsplit(input$new_nombres_alt %||% "", "\n")[[1]])
      alt_lines <- alt_lines[nzchar(alt_lines)]

      new_row <- data.frame(
        id           = uuid::UUIDgenerate(),
        account_code = .next_empresa_code(all_e),
        initials     = initials,
        razon_social = razon_social,
        nombre_corto = nombre_final,
        nombres_alt  = as.character(jsonlite::toJSON(alt_lines, auto_unbox = FALSE)),
        rfc          = rfc,
        activa       = isTRUE(input$new_activa),
        deleted      = FALSE,
        deleted_at   = NA_character_,
        created_at   = as.character(Sys.time()),
        updated_at   = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )

      updated <- dplyr::bind_rows(all_e, new_row)
      # CM39: the checks above saw the session's copy; they are made again on
      # S3's table as the company is written. Refused there (another session
      # took the initials meanwhile): nothing written, the form stays open,
      # the session already shows that other company.
      guardado <- NULL
      saved <- tryCatch({
        guardado <- .save_and_broadcast(updated, validar = .validar_alta_empresa(initials, nombre_final),
                                        ajustar = .codigo_libre_empresa(new_row$id))
        "ok"
      },
                        empresa_iniciales_repetidas = function(e) "repetidas",
                        empresa_iniciales_de_antes  = function(e) "de_antes",
                        empresa_iniciales_reservadas = function(e) "reservadas",
                        empresa_nombre_corto_repetido = function(e) "nombre_repetido",
                        error = function(e) { message("[EMP] create failed: ", e$message); "error" })
      if (saved == "repetidas") {
        showNotification(tr("tablas.empresas.aviso.iniciales_recien_dadas", iniciales = initials),
                         type = "error", duration = 8)
        return()
      }
      if (saved == "nombre_repetido") {   # CM48a: another session just took the name
        showNotification(tr("tablas.empresas.aviso.nombre_corto_existe", nombre = nombre_final),
                         type = "error", duration = 8)
        return()
      }
      if (saved == "reservadas") {   # a rename in progress holds them (CM40)
        showNotification(tr("tablas.empresas.aviso.iniciales_reservadas", iniciales = initials),
                         type = "error", duration = 8)
        return()
      }
      if (saved == "de_antes") {
        showNotification(tr("tablas.empresas.aviso.iniciales_de_antes"), type = "error"); return()
      }
      if (saved != "ok") {
        showNotification(
          tr("tablas.empresas.aviso.no_guardada"),
          type = "error", duration = 6)
        return()
      }
      # The code written, which may not be the one computed on the copy (CM39).
      codigo <- as.character(guardado$account_code[as.character(guardado$id) == new_row$id][1])
      shinyjs::hide("create_form_wrap")
      updateTextInput(session, "new_initials",     value = "")
      updateTextInput(session, "new_razon_social", value = "")
      updateTextInput(session, "new_nombre_corto", value = "")
      updateTextInput(session, "new_rfc",          value = "")
      showNotification(
        tr("tablas.empresas.aviso.creada", iniciales = initials, codigo = codigo),
        type = "message", duration = 3)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "empresas",
        action      = "crear_empresa",
        description = paste0("Empresa creada: ", initials, " (", codigo, ") — ", razon_social),
        target_id   = new_row$id,
        metadata    = list(initials = initials, account_code = codigo, razon_social = razon_social),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    })

    # ── Edit panel ────────────────────────────────────────────────────────────
    # The id of the company being edited (from the table), never its position.
    selected_id <- reactiveVal(NULL)

    observeEvent(input$btn_cancel_edit, { selected_id(NULL) })

    # The visible company with that id, or a 0-row frame once it's gone
    # (deactivated or hidden by another session): the panel then closes.
    .empresa_por_id <- function(id) {
      empresas <- empresas_rv()
      if (is.null(id) || is.null(empresas)) return(empresas[0, , drop = FALSE])
      empresas[as.character(empresas$id) == id, , drop = FALSE]
    }

    # The row the edit panel was drawn with (CM40): Guardar applies only the
    # fields that differ from it, onto S3's current row.
    fila_form <- reactiveVal(NULL)
    output$edit_panel_ui <- renderUI({
      row <- .empresa_por_id(selected_id())
      req(nrow(row) == 1)
      fila_form(row)
      .empresas_edit_panel_ui(ns, row)
    })

    # The session shown what S3 holds now (after a refused step).
    .refrescar_empresas <- function() tryCatch(
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db))
        shared$empresas_db(leer_tabla_para_escribir(S3_KEYS$empresas, shared$effective_client_id(), .schema_empresas)),
      error = function(e) message("[EMP] reload empresas: ", conditionMessage(e)))

    # Steps 1 and 2 of a rename OLD -> NEW (CM40): reserve NEW on S3's table,
    # then the cascade. TRUE when both are done; otherwise the user was told
    # why and nothing more is to be done (a refused reservation wrote
    # nothing; a cascade that failed keeps the reservation for Reintentar).
    .renombrar <- function(sel_id, old_ini, new_ini) {
      cid <- shared$effective_client_id()
      reserva <- tryCatch(reservar_renombre_empresa(sel_id, old_ini, new_ini, cid),
                          empresa_rechazada = function(e) e,
                          error = function(e) { message("[EMP rename] reserve: ", conditionMessage(e)); e })
      if (inherits(reserva, "error")) {
        aviso <- if (inherits(reserva, "empresa_renombre_en_curso"))
          tr("tablas.empresas.aviso.renombre_en_curso", iniciales = reserva$iniciales)
        else if (inherits(reserva, "empresa_renombrada_otra_sesion"))
          tr("tablas.empresas.aviso.renombrada_otra_sesion", iniciales = reserva$iniciales)
        else if (inherits(reserva, "empresa_iniciales_reservadas"))
          tr("tablas.empresas.aviso.iniciales_reservadas", iniciales = reserva$iniciales)
        else if (inherits(reserva, "empresa_iniciales_de_antes"))
          tr("tablas.empresas.aviso.iniciales_de_antes")
        else tr("tablas.empresas.aviso.cambios_no_guardados")
        showNotification(aviso, type = "error", duration = 8)
        if (inherits(reserva, "empresa_rechazada")) .refrescar_empresas()
        return(FALSE)
      }
      bump_sync_version("empresas_db", client_id = cid)   # other sessions see the reservation
      if (!is.null(shared$empresas_db) && is.function(shared$empresas_db)) shared$empresas_db(reserva)
      tryCatch(log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "empresas",
        action      = "reservar_renombre",
        description = sprintf("Reserva de iniciales %s → %s", old_ini, new_ini),
        target_id   = sel_id,
        metadata    = list(old_initials = old_ini, new_initials = new_ini),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      ), error = function(e) message("[EMP] log reservar_renombre: ", conditionMessage(e)))

      # A step that failed (hd_renombrar_incompleto, CM26) leaves this
      # company with its old initials and the reservation: Reintentar (or
      # saving the same edit again) finishes the tables left. The cascade is
      # recorded either way, with which tables were done and which failed
      # (initials and table keys only).
      cascade <- tryCatch(list(hechos = rename_empresa_initials(old_ini, new_ini, client_id = cid)),
                          error = function(e) e)
      if (inherits(cascade, "error")) {
        if (inherits(cascade, "hd_renombrar_incompleto")) {
          showNotification(tr("tablas.empresas.aviso.renombrar_incompleto", antes = old_ini, despues = new_ini,
                              tablas = paste(cascade$fallidos, collapse = ", ")),
                           type = "error", duration = 10)
        } else {
          showNotification(tr("tablas.empresas.aviso.error_cascada", error = conditionMessage(cascade)),
                           type = "error", duration = 8)
        }
      }
      if (inherits(cascade, "hd_renombrar_incompleto") || !inherits(cascade, "error"))
        tryCatch(log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = "empresas",
          action      = "renombrar_iniciales",
          description = sprintf("Cambio de iniciales %s → %s: %s", old_ini, new_ini,
                                if (inherits(cascade, "error")) "incompleto" else "completo"),
          target_id   = sel_id,
          metadata    = list(old_initials = old_ini, new_initials = new_ini,
                             hechas = cascade$hechos %||% character(0),
                             fallidas = if (inherits(cascade, "error")) cascade$fallidos else character(0)),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) message("[EMP] log renombrar_iniciales: ", conditionMessage(e)))
      !inherits(cascade, "error")
    }

    # Step 3 and the edit's other fields: set on S3's current row (CM40); a
    # rename's initials confirmed there, its reservation cleared. If this
    # write fails, the reservation stays and the same save (or Reintentar)
    # finishes it.
    .confirmar_edicion <- function(sel_id, cambios, old_ini, new_ini) {
      renombra <- !identical(old_ini, new_ini)
      saved <- tryCatch({ .save_and_broadcast(isolate(all_empresas_rv()), ajustar = .empresa_aplicar_campos(
                            sel_id, cambios, old_ini, if (renombra) new_ini)); TRUE },
                        error = function(e) { message("[EMP] edit failed: ", e$message); FALSE })
      if (!saved) {
        showNotification(tr("tablas.empresas.aviso.cambios_no_guardados"),
                         type = "error", duration = 6)
        return(FALSE)
      }

      # After initials rename: reload in-memory reactive vals that were updated
      # in S3 by rename_empresa_initials(), so all modules see the new initials
      # immediately without requiring a page refresh.
      if (renombra) {
        tryCatch(log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = "empresas",
          action      = "confirmar_renombre",
          description = sprintf("Iniciales confirmadas %s → %s", old_ini, new_ini),
          target_id   = sel_id,
          metadata    = list(old_initials = old_ini, new_initials = new_ini),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) message("[EMP] log confirmar_renombre: ", conditionMessage(e)))
        tryCatch(
          if (!is.null(shared$ctas_cuentas) && is.function(shared$ctas_cuentas))
            shared$ctas_cuentas(load_ctas_cuentas(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload ctas_cuentas: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$interco_v2) && is.function(shared$interco_v2))
            shared$interco_v2(
              load_interco_v2(client_id = shared$effective_client_id()) %||%
                list(ar_prefix = "C", ap_prefix = "P", companies = list())
            ),
          error = function(e) message("[EMP rename] reload interco_v2: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$proveedores_db) && is.function(shared$proveedores_db))
            shared$proveedores_db(load_proveedores(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload proveedores: ", e$message)
        )
        tryCatch(
          if (!is.null(shared$parte_alias_map_db) && is.function(shared$parte_alias_map_db))
            shared$parte_alias_map_db(load_parte_alias_map(client_id = shared$effective_client_id())),
          error = function(e) message("[EMP rename] reload parte_alias_map: ", e$message)
        )
      }

      selected_id(NULL)
      showNotification(
        if (renombra)
          tr("tablas.empresas.aviso.iniciales_actualizadas", antes = old_ini, despues = new_ini)
        else
          tr("tablas.empresas.aviso.actualizada"),
        type = "message", duration = 3)
      tryCatch(log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "empresas",
        action      = "editar_empresa",
        description = if (renombra) paste0("Empresa editada: iniciales '", old_ini, "' → '", new_ini, "'")
                      else paste0("Empresa editada: ", new_ini),
        target_id   = sel_id,
        metadata    = list(old_initials = old_ini, new_initials = new_ini),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      ), error = function(e) message("[EMP] log editar_empresa: ", conditionMessage(e)))
      TRUE
    }

    observeEvent(input$btn_save_edit, {
      sel_id <- selected_id()
      row    <- .empresa_por_id(sel_id)
      req(nrow(row) == 1)

      # CM40: what the form showed, not the session's copy now. A rename is
      # edit_initials differing from the initials the page was drawn with
      # (sent from the page); the other fields are compared with the row the
      # panel was drawn with. Without the page's value (an older page), the
      # drawn row, then the copy.
      form <- isolate(fila_form())
      if (is.null(form) || !identical(as.character(form$id), as.character(sel_id))) form <- row
      # CM47: the server refuses a nombre_corto change whatever the page sent,
      # BEFORE any rename cascade runs (no half-done edit).
      if (.nombre_corto_cambia(form, trimws(input$edit_nombre_corto %||% form$nombre_corto %||% ""))) {
        showNotification(tr("tablas.empresas.aviso.nombre_corto_fijo"), type = "error")
        return()
      }
      apertura <- trimws(input$edit_ini_apertura %||% "")
      old_ini <- if (nzchar(apertura)) apertura else trimws(form$initials)
      new_ini <- toupper(trimws(input$edit_initials %||% ""))

      if (!nzchar(new_ini) || !grepl("^[A-Z0-9]+$", new_ini)) {
        showNotification(tr("tablas.empresas.aviso.iniciales_invalidas"),
                         type = "error"); return()
      }

      all_e    <- all_empresas_rv()
      full_idx <- which(as.character(all_e$id) == sel_id)
      req(length(full_idx) == 1)

      # Cascade-rename across all related S3 tables when initials change
      if (new_ini != old_ini) {
        # The company was renamed by another session since this form was
        # drawn: renaming it from initials it no longer has would run the
        # cascade on the wrong ones (CM40).
        if (!identical(toupper(trimws(row$initials)), toupper(old_ini))) {
          showNotification(tr("tablas.empresas.aviso.renombrada_otra_sesion", iniciales = trimws(row$initials)),
                           type = "error", duration = 8)
          return()
        }
        others <- all_e[-full_idx, , drop = FALSE]
        if (new_ini %in% toupper(trimws(others$initials[!is.na(others$initials)]))) {
          showNotification(tr("tablas.empresas.aviso.iniciales_otra"), type = "error")
          return()
        }
        # Another company's earlier initials: never (CM29). Its own are fine
        # (going back B -> A).
        if (new_ini %in% .iniciales_de_antes_ajenas(all_e, sel_id)) {
          showNotification(tr("tablas.empresas.aviso.iniciales_de_antes"), type = "error")
          return()
        }
        # Reserve on S3's table, then the cascade (CM40).
        if (!.renombrar(sel_id, old_ini, new_ini)) return()
      }

      alt_lines <- trimws(strsplit(input$edit_nombres_alt %||% "", "\n")[[1]])
      alt_lines <- alt_lines[nzchar(alt_lines)]

      # CM40: only the fields this form changed, set on S3's current row (the
      # initials, for a rename, with their earlier ones computed there too).
      cambios <- .empresa_campos_editados(form, list(
        razon_social = trimws(input$edit_razon_social %||% ""),
        nombre_corto = trimws(input$edit_nombre_corto %||% form$nombre_corto %||% ""),
        rfc          = toupper(trimws(input$edit_rfc  %||% "")),
        activa       = isTRUE(input$edit_activa),
        nombres_alt  = as.character(jsonlite::toJSON(alt_lines, auto_unbox = FALSE))))
      .confirmar_edicion(sel_id, cambios, old_ini, new_ini)
    })

    # Reintentar (CM40): a company with a rename in progress (a cascade or a
    # confirmation that failed) finishes that same rename -- idempotent: the
    # reservation is its own, the tables already done find no OLD. Only that
    # rename; cancelling it is not offered.
    observeEvent(input$btn_reintentar_renombre, {
      sel_id <- selected_id()
      row    <- .empresa_por_id(sel_id)
      req(nrow(row) == 1)
      pend <- toupper(trimws(row$renombre_pendiente %||% ""))
      if (is.na(pend) || !nzchar(pend)) return()
      old_ini <- trimws(row$initials)
      # Initials already NEW (only the confirmation was left): clear it.
      if (identical(toupper(old_ini), pend))
        return(invisible(.confirmar_edicion(sel_id, list(renombre_pendiente = NA_character_), pend, pend)))
      if (!.renombrar(sel_id, old_ini, pend)) return()
      .confirmar_edicion(sel_id, list(), old_ini, pend)
    })

    # ── Delete (soft) ─────────────────────────────────────────────────────────
    # The table's ⊘ confirms in its own bar before the action arrives, so this
    # runs on an already confirmed id.
    .desactivar_empresa <- function(id) {
      row <- .empresa_por_id(id)
      if (nrow(row) != 1) return()
      target_initials <- row$initials

      all_e    <- all_empresas_rv()
      row_idx  <- which(as.character(all_e$id) == id)

      if (length(row_idx) == 1) {
        all_e[row_idx, "deleted"]    <- TRUE
        all_e[row_idx, "deleted_at"] <- as.character(Sys.time())
        all_e[row_idx, "activa"]     <- FALSE
        all_e[row_idx, "updated_at"] <- as.character(Sys.time())
      }

      saved <- tryCatch({ .save_and_broadcast(all_e); TRUE },
                        error = function(e) { message("[EMP] delete failed: ", e$message); FALSE })
      if (!saved) {
        showNotification(tr("tablas.empresas.aviso.cambio_no_guardado"),
                         type = "error", duration = 6)
        return()
      }
      if (identical(selected_id(), id)) selected_id(NULL)
      showNotification(tr("tablas.empresas.aviso.desactivada", iniciales = target_initials),
                       type = "message", duration = 4)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = "empresas",
        action      = "desactivar_empresa",
        description = paste0("Empresa desactivada: ", target_initials),
        target_id   = if (length(row_idx) == 1) all_e[row_idx, "id"] else NA_character_,
        metadata    = list(initials = target_initials),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

    # ── Stage 3: ERP connections ───────────────────────────────────────────────
    # Per-company ERP credentials. See docs/saas_rebuild/STAGE_3_ERP_CREDENTIALS.md.
    #
    # Visibility (spec §5): FINANCIAL_TABS-style, not Stage 2's staff-only-tab
    # hiding — this is ordinary client-facing functionality. Invisible for a
    # staff-at-home session (nothing to configure, hd-admin has no
    # companies), visible for a native client session and for staff mid-jump.
    erp_is_staff <- reactive(isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE)))
    erp_jumped   <- reactive(!is.null(tryCatch(shared$jump_client_id(), error = function(e) NULL)))

    erp_ui_visible <- reactive(if (erp_is_staff()) erp_jumped() else TRUE)

    # Server-side access gate (spec §5 "Access") — the actual enforcement.
    # Every handler below calls this itself; the UI only hides the option.
    erp_access <- reactive(erp_access_allowed(current_tier(), erp_is_staff(), erp_jumped(), shared$usuario()))

    .reject_erp_action <- function() {
      showNotification(tr("tablas.empresas.aviso.erp_sin_permiso"),
                       type = "error", duration = 5)
    }

    .erp_connections_local <- reactiveVal(NULL)

    # NOTE: .erp_connections_local() is a DISPLAY cache only — it may contain
    # rows just flagged deleted=TRUE (the write path needs the full set to
    # persist that flag correctly). load_erp_connections() already excludes
    # deleted rows on a fresh read, so this reactive re-applies the same
    # filter to the cached path too — otherwise a just-deleted row keeps
    # showing in the UI after the very first mutation in a session, even
    # though it really was removed from what's active.
    all_erp_rv <- reactive({
      ovr <- .erp_connections_local()
      df <- if (!is.null(ovr)) {
        ovr
      } else {
        req(erp_ui_visible())
        tryCatch(load_erp_connections(shared$effective_client_id()),
                error = function(e) { message("[EMP ERP] load failed: ", e$message); .schema_erp_connections() })
      }
      if (!is.null(df) && nrow(df) && "deleted" %in% names(df))
        df <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
      df
    })

    # Every mutation (save/test/delete/restore) MUST go through this, never
    # save all_erp_rv() directly. all_erp_rv()/
    # .erp_connections_local() are a per-session display cache — if a write
    # ever merged its change into that cached snapshot instead of the live
    # S3 state, a snapshot that was even briefly stale (a slow reactive
    # flush, a second tab, another session) would silently overwrite S3 with
    # a smaller set of rows and drop whatever wasn't in that snapshot — with
    # no "deleted" flag, no trace, just gone. This function re-fetches the
    # current live state immediately before merging in `mutate_fn`'s change
    # and writing, so the write can never regress rows that changed since
    # this session's own copy was cached.
    # CM31: the reread is the WHOLE table, trash included, and strict (a
    # failed read stops the save; it is never an empty table). `mutate_fn`
    # sees only the active connections and the rows in the trash are written
    # back exactly as read -- before, every save erased the trash. Restore
    # (`include_deleted`, Stage 3b) is the one mutation that sees them all.
    # save_erp_connections() rebases on `fresh`, so a row another session
    # changed between this read and the write is kept.
    .erp_mutate_and_save <- function(mutate_fn, include_deleted = FALSE) {
      cid <- shared$effective_client_id()
      fresh <- leer_erp_connections_para_escribir(cid)   # stop()s on a failed read
      updated <- if (include_deleted) {
        mutate_fn(fresh)
      } else {
        borrada <- !is.na(fresh$deleted) & fresh$deleted
        dplyr::bind_rows(mutate_fn(fresh[!borrada, , drop = FALSE]), fresh[borrada, , drop = FALSE])
      }
      save_erp_connections(updated, client_id = cid, base = fresh)
      .erp_connections_local(updated)
      invisible(updated)
    }

    erp_modal <- reactiveValues(company = NULL, form_open = FALSE, editing_id = NULL,
                                confirm_delete_id = NULL, show_deleted = FALSE)

    # Deleted connections for the currently-open company's modal — a raw
    # read (bypassing load_erp_connections()'s own deleted-row filter),
    # scoped to just this company, feeding the "Ver eliminadas" / Restaurar
    # UI (Stage 3b §2).
    deleted_erp_rows <- reactive({
      req(erp_modal$company)
      cid <- shared$effective_client_id()
      raw <- tryCatch(.normalize(.s3_read_with(S3_KEYS$erp_connections, client_id = cid), .schema_erp_connections),
                      error = function(e) .schema_erp_connections())
      if (!nrow(raw)) return(raw)
      is_deleted  <- !is.na(raw$deleted) & raw$deleted == TRUE
      has_company <- vapply(raw$company_initials, function(x) {
        inits <- tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))
        isTRUE(erp_modal$company %in% inits)
      }, logical(1))
      raw[is_deleted & has_company, , drop = FALSE]
    })

    company_erp_rows <- reactive({
      req(erp_modal$company)
      df <- all_erp_rv()
      if (is.null(df) || !nrow(df)) return(df)
      has_company <- vapply(df$company_initials, function(x) {
        inits <- tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))
        isTRUE(erp_modal$company %in% inits)
      }, logical(1))
      df[has_company, , drop = FALSE]
    })

    editing_erp_row <- reactive({
      id <- erp_modal$editing_id
      req(!is.null(id))
      rows <- company_erp_rows()
      req(!is.null(rows) && nrow(rows))
      match_row <- rows[rows$id == id, , drop = FALSE]
      req(nrow(match_row) == 1)
      match_row
    })

    # Blocks browser autofill on a rendered input by patching the actual
    # <input> element (not the wrapping div textInput()/passwordInput()
    # return). Plain autocomplete="off" is routinely ignored by Chrome for
    # login-shaped fields, so password fields get "new-password" instead —
    # the one value Chrome actually honors for suppressing saved-credential
    # suggestions. Needed because a text-input-next-to-password-input pair
    # on the same origin as this app's own login screen gets autofilled with
    # the logged-in user's own Hopdesk credentials by the browser itself —
    # not a server-side leak, but confusing and worth blocking outright.
    .erp_no_autofill <- function(tag, password = FALSE) {
      htmltools::tagQuery(tag)$find("input")$
        addAttrs(autocomplete = if (password) "new-password" else "off")$
        allTags()
    }

    # `secret` = TRUE for fields that go into secrets_encrypted (prefix "secret") —
    # these are NEVER pre-filled with the real value, even on edit, per the
    # "leave blank to keep unchanged" convention. Non-secret config fields
    # (prefix "cfg") are ordinary settings and MUST show their real current
    # value on edit — leaving them blank-with-a-hint like the secret fields
    # would silently wipe the URL/company/etc. on save if the user didn't
    # notice and retype them.
    .erp_render_field <- function(prefix, field, value = NULL, secret = FALSE) {
      input_id <- ns(paste0(prefix, "_", field$name))
      if (identical(field$type, "checkbox")) {
        checkboxInput(input_id, field$label, value = if (is.null(value)) TRUE else isTRUE(value))
      } else if (secret) {
        if (identical(field$type, "password")) {
          .erp_no_autofill(passwordInput(input_id, field$label, value = "",
                        placeholder = if (!is.null(value)) "•••••• (dejar en blanco para no modificar)" else ""),
                        password = TRUE)
        } else {
          .erp_no_autofill(textInput(input_id, field$label, value = "",
                    placeholder = if (!is.null(value)) "(dejar en blanco para no modificar)" else ""))
        }
      } else {
        .erp_no_autofill(textInput(input_id, field$label, value = if (is.null(value)) "" else as.character(value)))
      }
    }

    output$erp_modal_body <- renderUI({
      req(erp_modal$company)
      rows <- company_erp_rows()

      # Inline delete confirmation — NOT a nested showModal(), which would
      # silently replace this "Conexiones ERP" modal (Shiny shows only one
      # modal at a time) and strand the user without a way back to the list.
      if (!is.null(erp_modal$confirm_delete_id)) {
        target <- if (!is.null(rows) && nrow(rows)) rows[rows$id == erp_modal$confirm_delete_id, , drop = FALSE] else rows[0, ]
        return(tagList(
          tags$p("Vas a eliminar la conexión:"),
          tags$div(class = "alert alert-warning py-2",
                   tags$strong(if (nrow(target)) target$label[1] else "")),
          tags$p(class = "text-muted small",
                 icon("circle-info", class = "me-1"),
                 " Esta acción no se puede deshacer desde la interfaz. Tendrás que volver a ",
                 "capturar las credenciales si la necesitas de nuevo."),
          div(class = "d-flex gap-2",
              actionButton(ns("btn_confirm_erp_delete"), tagList(icon("trash"), " Sí, eliminar"),
                          class = "btn btn-danger btn-sm"),
              actionButton(ns("btn_cancel_erp_delete"), "Cancelar", class = "btn btn-outline-secondary btn-sm"))
        ))
      }

      # Source indicator (Stage 3b §3) — small, unobtrusive, pulled straight
      # from whatever .sap_creds() itself last logged for this company, not a
      # new tracking mechanism.
      src <- tryCatch(sap_creds_source(erp_modal$company), error = function(e) NULL)
      source_indicator_ui <- if (is.null(src)) {
        tags$p(class = "text-muted small mb-2",
               icon("circle-info", class = "me-1"),
               "Fuente de credenciales: sin datos todavía en esta sesión.")
      } else {
        label <- if (identical(src$source, "store")) "Conexión guardada" else "Variable de entorno (respaldo)"
        color <- if (identical(src$source, "store")) "success" else "warning"
        tags$p(class = "text-muted small mb-2",
               icon("circle-info", class = "me-1"),
               "Fuente de la última conexión SAP: ",
               tags$span(class = paste0("badge bg-", color), label),
               sprintf(" (%s)", format(src$at, "%H:%M:%S")))
      }

      list_ui <- if (is.null(rows) || !nrow(rows)) {
        tags$p(class = "text-muted small", "Sin conexiones configuradas todavía.")
      } else {
        tagList(lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, ]
          badge <- if (isTRUE(r$last_test_result == "ok")) list(color = "success", text = "OK")
                   else if (isTRUE(r$last_test_result == "error")) list(color = "danger", text = "Error")
                   else list(color = "secondary", text = "Sin probar")
          div(
            class = "d-flex align-items-center justify-content-between border rounded p-2 mb-2",
            div(
              tags$strong(r$label), " ",
              tags$span(class = paste0("badge bg-", badge$color), badge$text),
              tags$div(class = "text-muted small", erp_connector_label(r$erp_type))
            ),
            div(
              class = "d-flex gap-1",
              tags$button(`data-hd-espera` = "auto", class = "btn btn-sm btn-outline-primary", title = "Editar",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_edit_click"), r$id),
                icon("pen")),
              tags$button(`data-hd-espera` = "auto", class = "btn btn-sm btn-outline-secondary", title = "Probar conexión",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_test_click"), r$id),
                icon("plug")),
              tags$button(`data-hd-espera` = "auto", class = "btn btn-sm btn-outline-danger", title = "Eliminar",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("erp_del_click"), r$id),
                icon("trash"))
            )
          )
        }))
      }

      form_ui <- if (isTRUE(erp_modal$form_open)) {
        editing <- tryCatch(editing_erp_row(), error = function(e) NULL)
        erp_type <- if (!is.null(editing)) editing$erp_type[1] else "sap_b1_service_layer"
        cfg <- if (!is.null(editing)) tryCatch(jsonlite::fromJSON(editing$config[1]), error = function(e) list()) else list()

        tagList(
          tags$hr(),
          tags$h6(class = "fw-semibold", if (!is.null(editing)) "Editar conexión" else "Nueva conexión"),
          .erp_no_autofill(textInput(ns("erp_label"), "Nombre",
                    value = if (!is.null(editing)) editing$label[1] else "",
                    placeholder = "Ej: SAP — Networks Group (NG)")),
          lapply(erp_connector_config_fields(erp_type), function(f) .erp_render_field("cfg", f, cfg[[f$name]])),
          tags$p(class = "text-muted small mb-1 mt-2", "Credenciales"),
          lapply(erp_connector_secret_fields(erp_type), function(f)
            .erp_render_field("secret", f, value = if (!is.null(editing)) TRUE else NULL, secret = TRUE)),
          div(
            class = "d-flex gap-2 mt-2",
            actionButton(ns("btn_save_erp"), tagList(icon("floppy-disk"), " Guardar"),
                        class = "btn btn-primary btn-sm"),
            actionButton(ns("btn_cancel_erp_form"), "Cancelar", class = "btn btn-outline-secondary btn-sm")
          )
        )
      } else {
        actionButton(ns("btn_new_erp"), tagList(icon("circle-plus"), " Nueva conexión"),
                    class = "btn btn-outline-primary btn-sm mt-2")
      }

      # Restore section (Stage 3b §2) — "we really do not want any
      # connection to get lost." Only shown when there's actually something
      # deleted for this company, to keep the modal uncluttered otherwise.
      del_rows <- deleted_erp_rows()
      restore_ui <- if (!is.null(del_rows) && nrow(del_rows) > 0) {
        tagList(
          tags$hr(),
          actionButton(ns("btn_toggle_deleted"),
                      tagList(icon(if (isTRUE(erp_modal$show_deleted)) "eye-slash" else "eye"),
                              if (isTRUE(erp_modal$show_deleted)) " Ocultar eliminadas"
                              else paste0(" Ver eliminadas (", nrow(del_rows), ")")),
                      class = "btn btn-outline-secondary btn-sm"),
          if (isTRUE(erp_modal$show_deleted)) {
            tagList(lapply(seq_len(nrow(del_rows)), function(i) {
              r <- del_rows[i, ]
              div(
                class = "d-flex align-items-center justify-content-between border rounded p-2 mt-2",
                style = "opacity: 0.75;",
                div(
                  tags$strong(r$label), " ",
                  tags$span(class = "badge bg-secondary", "Eliminada"),
                  tags$div(class = "text-muted small", paste0("Eliminada: ", r$deleted_at))
                ),
                tags$button(`data-hd-espera` = "auto", class = "btn btn-sm btn-outline-success", title = "Restaurar",
                  onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                    ns("erp_restore_click"), r$id),
                  tagList(icon("rotate-left"), " Restaurar"))
              )
            }))
          } else NULL
        )
      } else NULL

      tagList(source_indicator_ui, list_ui, restore_ui, form_ui)
    })

    # The table's 🔌 brings the company's id; the modal works by initials.
    .abrir_erp <- function(id) {
      req(erp_ui_visible())
      row <- .empresa_por_id(id)
      if (nrow(row) != 1) return()
      ini <- row$initials
      erp_modal$company           <- ini
      erp_modal$form_open         <- FALSE
      erp_modal$editing_id        <- NULL
      erp_modal$confirm_delete_id <- NULL
      erp_modal$show_deleted      <- FALSE
      showModal(modalDialog(
        title = tagList(icon("plug"), paste0(" Conexiones ERP — ", ini)),
        uiOutput(ns("erp_modal_body")),
        footer = modalButton("Cerrar"),
        size = "m", easyClose = TRUE
      ))
    }

    observeEvent(input$btn_new_erp, {
      req(erp_access())
      erp_modal$editing_id <- NULL; erp_modal$form_open <- TRUE; erp_modal$confirm_delete_id <- NULL
    })
    observeEvent(input$erp_edit_click, {
      req(erp_access())
      erp_modal$editing_id <- input$erp_edit_click; erp_modal$form_open <- TRUE; erp_modal$confirm_delete_id <- NULL
    })
    observeEvent(input$btn_cancel_erp_form, {
      erp_modal$form_open <- FALSE; erp_modal$editing_id <- NULL
    })

    observeEvent(input$btn_save_erp, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      editing  <- tryCatch(editing_erp_row(), error = function(e) NULL)
      erp_type <- if (!is.null(editing)) editing$erp_type[1] else "sap_b1_service_layer"
      label    <- trimws(input$erp_label %||% "")
      if (!nzchar(label)) { showNotification(tr("tablas.empresas.aviso.erp_nombre_obligatorio"), type = "error"); return() }

      cfg_fields <- erp_connector_config_fields(erp_type)
      config <- setNames(lapply(cfg_fields, function(f) {
        v <- input[[paste0("cfg_", f$name)]]
        if (identical(f$type, "checkbox")) isTRUE(v) else trimws(v %||% "")
      }), vapply(cfg_fields, function(f) f$name, character(1)))

      secret_fields  <- erp_connector_secret_fields(erp_type)
      secret_inputs  <- lapply(secret_fields, function(f) trimws(input[[paste0("secret_", f$name)]] %||% ""))
      names(secret_inputs) <- vapply(secret_fields, function(f) f$name, character(1))
      any_secret_filled <- any(vapply(secret_inputs, nzchar, logical(1)))
      all_secret_filled <- all(vapply(secret_inputs, nzchar, logical(1)))

      if (is.null(editing) && !all_secret_filled) {
        showNotification(tr("tablas.empresas.aviso.erp_credenciales_completas"), type = "error"); return()
      }
      if (any_secret_filled && !all_secret_filled) {
        showNotification(tr("tablas.empresas.aviso.erp_credenciales_todas_o_ninguna"),
                         type = "error", duration = 6)
        return()
      }

      secrets_encrypted <- if (!is.null(editing) && !any_secret_filled) {
        editing$secrets_encrypted[1]
      } else {
        encrypt_secret(secret_inputs)
      }

      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      who  <- info$account_code %||% info$user %||% "unknown"
      now  <- as.character(Sys.time())
      cid  <- shared$effective_client_id()
      erp_company <- erp_modal$company
      editing_id  <- if (!is.null(editing)) editing$id[1] else NULL

      saved <- tryCatch({
        .erp_mutate_and_save(function(all_df) {
          if (!is.null(editing_id)) {
            idx <- which(all_df$id == editing_id)
            if (length(idx) != 1)
              stop("Esta conexión ya no existe — puede haber sido eliminada en otra sesión. Cierra y vuelve a abrir el panel.")
            all_df[idx, "label"]             <- label
            all_df[idx, "config"]            <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
            all_df[idx, "secrets_encrypted"] <- secrets_encrypted
            all_df[idx, "updated_at"]        <- now
            all_df[idx, "updated_by"]        <- who
            all_df
          } else {
            new_row <- tibble::tibble(
              id = uuid::UUIDgenerate(), client_id = cid, label = label, erp_type = erp_type,
              company_initials = as.character(jsonlite::toJSON(erp_company, auto_unbox = FALSE)),
              config = as.character(jsonlite::toJSON(config, auto_unbox = TRUE)),
              secrets_encrypted = secrets_encrypted,
              created_at = now, created_by = who, updated_at = now, updated_by = who,
              last_tested_at = NA_character_, last_test_result = NA_character_, last_test_message = NA_character_,
              active = TRUE, deleted = FALSE, deleted_at = NA_character_
            )
            dplyr::bind_rows(all_df, new_row)
          }
        })
        TRUE
      }, error = function(e) {
        message("[EMP ERP] save failed: ", e$message)
        showNotification(conditionMessage(e), type = "error", duration = 8)
        FALSE
      })
      if (!saved) return()

      erp_modal$form_open <- FALSE
      erp_modal$editing_id <- NULL
      showNotification(tr("tablas.empresas.aviso.erp_guardada", nombre = label), type = "message", duration = 3)
    })

    observeEvent(input$erp_test_click, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      test_id <- input$erp_test_click
      cid <- shared$effective_client_id()
      current <- tryCatch(load_erp_connections(cid), error = function(e) NULL)
      if (is.null(current) || !any(current$id == test_id)) {
        showNotification(tr("tablas.empresas.aviso.erp_ya_no_existe"),
                         type = "error", duration = 6)
        return()
      }
      row <- current[current$id == test_id, , drop = FALSE]

      config  <- tryCatch(jsonlite::fromJSON(row$config[1]), error = function(e) list())
      secrets <- tryCatch(decrypt_secret(row$secrets_encrypted[1]),
                          error = function(e) { list(.decrypt_error = conditionMessage(e)) })

      result <- if (!is.null(secrets$.decrypt_error)) {
        list(ok = FALSE, message = paste0("No se pudo descifrar la credencial: ", secrets$.decrypt_error))
      } else {
        tryCatch(run_erp_connection_test(row$erp_type[1], config, secrets),
                error = function(e) list(ok = FALSE, message = paste0("Error inesperado: ", conditionMessage(e))))
      }

      tryCatch({
        .erp_mutate_and_save(function(all_df) {
          idx <- which(all_df$id == test_id)
          if (length(idx) != 1) stop("La conexión fue eliminada mientras se probaba — resultado no guardado.")
          all_df[idx, "last_tested_at"]    <- as.character(Sys.time())
          all_df[idx, "last_test_result"]  <- if (isTRUE(result$ok)) "ok" else "error"
          all_df[idx, "last_test_message"] <- result$message %||% ""
          all_df
        })
      }, error = function(e) {
        message("[EMP ERP] test-result save failed: ", e$message)
        # T74: the test ran; only storing its result failed -- say so.
        showNotification(tr("comun.error_guardar"), type = "warning", duration = NULL)
      })

      showNotification(result$message %||% (if (isTRUE(result$ok)) "OK" else "Error"),
                       type = if (isTRUE(result$ok)) "message" else "error", duration = 6)
    })

    observeEvent(input$erp_del_click, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }
      all_df <- all_erp_rv()
      req(any(all_df$id == input$erp_del_click))
      erp_modal$confirm_delete_id <- input$erp_del_click
    })

    observeEvent(input$btn_cancel_erp_delete, {
      erp_modal$confirm_delete_id <- NULL
    })

    observeEvent(input$btn_confirm_erp_delete, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      id <- erp_modal$confirm_delete_id
      req(!is.null(id))

      saved <- tryCatch({
        .erp_mutate_and_save(function(all_df) {
          idx <- which(all_df$id == id)
          if (length(idx) != 1) stop("Esta conexión ya no existe (fue eliminada en otra sesión).")
          all_df[idx, "deleted"]    <- TRUE
          all_df[idx, "deleted_at"] <- as.character(Sys.time())
          all_df[idx, "active"]     <- FALSE
          all_df
        })
        TRUE
      }, error = function(e) {
        message("[EMP ERP] delete failed: ", e$message)
        showNotification(conditionMessage(e), type = "error", duration = 6)
        FALSE
      })
      erp_modal$confirm_delete_id <- NULL
      if (!saved) return()
      showNotification(tr("tablas.empresas.aviso.erp_eliminada"), type = "message", duration = 3)
    })

    # ── Restore (Stage 3b §2) ──────────────────────────────────────────────────

    observeEvent(input$btn_toggle_deleted, {
      erp_modal$show_deleted <- !isTRUE(erp_modal$show_deleted)
    })

    observeEvent(input$erp_restore_click, {
      if (!isTRUE(erp_access())) { .reject_erp_action(); return() }

      id <- input$erp_restore_click
      saved <- tryCatch({
        .erp_mutate_and_save(function(all_df) {
          idx <- which(all_df$id == id)
          if (length(idx) != 1) stop("Esta conexión ya no existe.")
          all_df[idx, "deleted"]    <- FALSE
          all_df[idx, "deleted_at"] <- NA_character_
          all_df[idx, "active"]     <- TRUE
          all_df
        }, include_deleted = TRUE)
        TRUE
      }, error = function(e) {
        message("[EMP ERP] restore failed: ", e$message)
        showNotification(conditionMessage(e), type = "error", duration = 6)
        FALSE
      })
      if (!saved) return()
      showNotification(
        tr("tablas.empresas.aviso.erp_restaurada"),
        type = "message", duration = 6)
    })

    # ── Group configuration section (dev only) ────────────────────────────────

    output$group_config_section <- renderUI({
      req(req_authorized())
      if (!puede(shared$usuario(), "can_manage_group_branding")) return(NULL)

      cfg <- tryCatch(shared$group_config(), error = function(e) NULL) %||%
             list(group_name = "Networks Group", logo_raw = NULL, logo_ext = "png")
      has_logo <- !is.null(cfg$logo_raw) && length(cfg$logo_raw) > 0

      tagList(
        tags$hr(class = "mt-4"),
        div(
          class = "card border-0 shadow-sm mt-3",
          div(
            class = "card-header bg-white py-2 d-flex align-items-center gap-2",
            icon("gear", class = "text-primary"),
            tags$span(class = "fw-semibold", "Configuración del Grupo"),
            tags$span(class = "badge bg-primary ms-1 small", "DEV")
          ),
          div(
            class = "card-body",
            fluidRow(
              column(8,
                tags$label("Nombre del grupo", class = "form-label small fw-semibold"),
                tags$p(class = "text-muted small mb-1",
                       "Aparece en la portada del reporte Word y como prefijo del nombre de archivo."),
                textInput(ns("group_name_input"), NULL,
                          value = cfg$group_name %||% "Networks Group",
                          placeholder = "Ej: Networks Group",
                          width = "100%")
              )
            ),
            fluidRow(
              column(8,
                tags$label("Logo del grupo", class = "form-label small fw-semibold"),
                tags$p(class = "text-muted small mb-1",
                       "PNG o JPG, máx. 2 MB. Aparece en la portada del reporte Word."),
                fileInput(ns("group_logo_upload"), NULL,
                          accept = c("image/png", "image/jpeg", ".png", ".jpg", ".jpeg"),
                          buttonLabel = "Elegir imagen…",
                          width = "100%")
              ),
              # T58: the saved logo itself, so one can see which it is (the
              # one the reports carry), not just that there is one.
              if (has_logo) {
                column(4,
                  div(class = "mt-4 pt-1 d-flex align-items-center gap-2",
                      logo_grupo_img(cfg$logo_raw, cfg$logo_ext, alto = 64),
                      tags$span(class = "small text-muted", tr("tablas.empresas.logo_guardado")))
              ) } else NULL
            ),
            # Monedas fase 1: the client's presentation currency (R/monedas.R).
            # Empty = the default (MONEDA_BASE_OMISION).
            {
              base <- tryCatch(shared$moneda_base_cliente(), error = function(e) NULL) %||%
                      moneda_base_resolver(NA)
              datos <- tryCatch(monedas_en_datos(monedas_de_sap(shared$sap_data())), error = function(e) character())
              idioma <- hd_idioma()
              fluidRow(
                column(8,
                  tags$label(tr("comun.monedas.base.titulo"), class = "form-label small fw-semibold"),
                  tags$p(class = "text-muted small mb-1", tr("comun.monedas.base.ayuda")),
                  selectizeInput(ns("moneda_base_input"), NULL,
                                 choices  = c(stats::setNames("", tr("comun.monedas.base.omision_opcion",
                                                                     moneda = MONEDA_BASE_OMISION)),
                                              monedas_opciones(primero = datos, idioma = idioma)),
                                 selected = if (identical(base$origen, "ajuste")) base$moneda else "",
                                 width = "100%"),
                  if (identical(base$origen, "omision"))
                    tags$p(class = "small text-muted mb-0",
                           icon("circle-info"), " ",
                           tr("comun.monedas.base.aviso_omision", moneda = base$moneda))
                )
              )
            },
            # TEMA stage 5: the client's theme, for every user of it (screen and
            # reports). Clients see the offered themes; HopDesk staff all of
            # them. The one it has now always shows, so saving never drops it.
            {
              actual  <- if (tema_valido(cfg$tema)) tolower(trimws(cfg$tema)) else ""
              ofrec   <- temas_ofrecidos(erp_is_staff())
              valores <- c("", union(ofrec, if (nzchar(actual)) actual))
              muestra <- function(t) tags$span(class = "d-inline-flex gap-1 me-2 align-middle",
                lapply(c("barra-fondo", "primario", "fondo", "acento"), function(r)
                  tags$span(class = "d-inline-block rounded-circle border hd-muestra-tema",   # a theme's preview: its own colours on purpose (TEMA 6-6b)
                            style = sprintf("width:14px;height:14px;background:%s;", tema_color(r, t)))))
              nombres <- lapply(valores, function(t) {
                if (!nzchar(t)) return(tagList(muestra(HD_TEMA_OMISION),
                  tr("tablas.empresas.tema.omision", tema = tr(HD_TEMAS[[HD_TEMA_OMISION]]$nombre))))
                tagList(muestra(t), tr(HD_TEMAS[[t]]$nombre),
                        if (!isTRUE(HD_TEMAS[[t]]$ofrecer))
                          tags$span(class = "badge text-bg-light border ms-2 small", tr("tablas.empresas.tema.solo_staff")))
              })
              fluidRow(
                column(8,
                  tags$label(tr("tablas.empresas.tema.titulo"), class = "form-label small fw-semibold"),
                  tags$p(class = "text-muted small mb-1", tr("tablas.empresas.tema.ayuda")),
                  radioButtons(ns("tema_input"), NULL, choiceNames = nombres, choiceValues = valores,
                               selected = actual, width = "100%")
                )
              )
            },
            actionButton(ns("btn_save_group_config"),
                         tagList(icon("floppy-disk"), " Guardar configuración"),
                         class = "btn btn-sm btn-primary mt-2")
          )
        )
      )
    })

    observeEvent(input$btn_save_group_config, {
      # The card is drawn only with the permission; the save checks it again.
      if (!puede(shared$usuario(), "can_manage_group_branding")) return()
      name <- trimws(input$group_name_input %||% "")
      if (!nzchar(name)) {
        showNotification(tr("tablas.empresas.aviso.grupo_nombre_vacio"), type = "warning")
        return()
      }
      if (nchar(name) > 100) {
        showNotification(tr("tablas.empresas.aviso.grupo_nombre_largo"), type = "warning")
        return()
      }

      # CM36: the copy this form started from is the save's `base`; only what
      # this session changed is written over S3's current config.
      base <- tryCatch(shared$group_config(), error = function(e) NULL) %||% .schema_group_config()
      cfg  <- base
      cfg$group_name <- name
      # "" = automatic; anything else must be a catalog code (never trust the
      # browser's value).
      mb <- toupper(trimws(input$moneda_base_input %||% ""))
      if (nzchar(mb) && !moneda_valida(mb)) {
        showNotification(tr("comun.monedas.base.invalida"), type = "error")
        return()
      }
      cfg$moneda_base <- if (nzchar(mb)) mb else NA_character_

      # TEMA stage 5: "" = the system's default (NA); anything else must be in
      # the registry AND offered to this person -- or be the one the client
      # already has. Never trust the browser's value. No input (the card was
      # not drawn): the theme stays as it is.
      if (!is.null(input$tema_input)) {
        t <- tolower(trimws(input$tema_input))
        actual <- if (tema_valido(base$tema)) tolower(trimws(base$tema)) else NA_character_
        if (nzchar(t) && (!tema_valido(t) ||
                          !(t %in% temas_ofrecidos(erp_is_staff()) || identical(t, actual)))) {
          showNotification(tr("tablas.empresas.tema.invalido"), type = "error")
          return()
        }
        cfg$tema <- if (nzchar(t)) t else NA_character_
      }

      logo_file <- input$group_logo_upload
      if (!is.null(logo_file) && file.exists(logo_file$datapath)) {
        ext <- tolower(tools::file_ext(logo_file$name))
        if (!ext %in% c("png", "jpg", "jpeg")) {
          showNotification(tr("tablas.empresas.aviso.logo_formato"), type = "error")
          return()
        }
        size_mb <- file.size(logo_file$datapath) / 1024 / 1024
        if (size_mb > 2) {
          showNotification(tr("tablas.empresas.aviso.logo_grande"), type = "error")
          return()
        }
        cfg$logo_raw <- readBin(logo_file$datapath, "raw",
                                n = file.size(logo_file$datapath))
        cfg$logo_ext <- if (ext == "jpeg") "jpg" else ext
      }

      tryCatch({
        # T72: the write logs itself (one entry, what changed field by field,
        # the client written and the viewer's home), only once S3 has it.
        evento <- list(
          usuario     = tryCatch(shared$current_user(), error = function(e) "system"),
          modulo      = "empresas",
          accion      = "guardar_config_grupo",
          descripcion = paste0("Configuración del grupo actualizada: nombre='", name, "'",
                               if (!is.null(logo_file)) ", logo actualizado" else ""),
          home        = tryCatch(shared$home_client_id(), error = function(e) NULL))
        guardado <- modificar_group_config(function(fresco) .group_config_rebase(cfg, base, fresco),
                                           client_id = shared$effective_client_id(), evento = evento)
        shared$group_config(guardado)   # what S3 holds now, other sessions' changes included
        showNotification(tr("tablas.empresas.aviso.grupo_guardado"),
                         type = "message", duration = 4)
      }, error = function(e) {
        showNotification(tr("tablas.empresas.aviso.error_guardar", error = conditionMessage(e)),
                         type = "error", duration = 6)
      })
    })

  })
}
