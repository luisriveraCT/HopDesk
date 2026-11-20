# =============================================================================
# R/requisiciones_persistence.R
# Requisiciones (front E): what Compras sends Tesorería to pay, with its
# evidence, before it has a date. Data only; the screen is
# R/requisiciones_module.R.
#
# Every function takes client_id explicitly and fails closed without one.
# Every write goes through modificar_tabla(): it re-reads S3 strictly and
# applies this action's change to that fresh copy, so two sessions writing
# at once never drop each other's rows, and a failed read never becomes an
# empty table that overwrites the file. A failed save stop()s; the caller
# never announces it as done.
#
# Tables (one file each per client; none ever loses rows -- trash is a mark):
#   requisiciones.rds              one row per requisition
#   requisiciones_carpetas.rds     folders (left panel), optional budget
#   requisiciones_comentarios.rds  comments
#   requisiciones_leidos.rds       per user and requisition, read up to when
#   requisiciones_evidencia.rds    attachments and pasted tables (metadata);
#                                  the bytes live in <cliente>/adjuntos/...
#   requisiciones_historial.rds    what happened to each requisition
# Evidence and comments live in sibling tables, not in requisiciones.rds:
# the list reads one small table, there are no list columns for .normalize()
# to break, and two people adding evidence to the same requisition at once
# each add their own row instead of racing for one cell.
#
# The stage is deduced on read, never stored (requisiciones_vista_en()):
#   sin_provision · provisionada · en_pago · pagada · papelera
# "pagada" comes only from etapa_de_provision_en(), i.e. a live confirmation
# in Bancos carrying the provision_id. A paid requisition leaves every view
# (incluir_pagadas = FALSE) but its row, evidence and comments stay in S3 and
# requisicion_de_provision() still finds it (read-only, from the history).
# =============================================================================

REQ_KEYS <- list(
  requisiciones = "requisiciones.rds",
  carpetas      = "requisiciones_carpetas.rds",
  comentarios   = "requisiciones_comentarios.rds",
  leidos        = "requisiciones_leidos.rds",
  evidencia     = "requisiciones_evidencia.rds",
  historial     = "requisiciones_historial.rds"
)

REQ_ETAPAS          <- c("sin_provision", "provisionada", "en_pago", "pagada", "papelera")
REQ_ETAPAS_VISIBLES <- c("sin_provision", "provisionada", "en_pago", "papelera")

REQ_MAX_TITULO     <- 200L
REQ_MAX_NOTAS      <- 2000L
REQ_MAX_COMENTARIO <- 2000L
REQ_COLOR_DEFECTO  <- "#607d8b"
.REQ_TZ            <- "America/Mexico_City"

.req_t0 <- function() as.POSIXct(character(), tz = "UTC")

.schema_requisiciones <- function() tibble::tibble(
  id              = character(),
  folio           = character(),   # REQ-AAAA-0001, assigned on the fresh table
  titulo          = character(),
  empresa         = character(),
  proveedor       = character(),
  monto           = numeric(),     # estimate; the shown amount is deduced
  moneda          = character(),
  notas           = character(),
  prioridad       = integer(),     # 1-10, optional; 10 = highest
  carpeta_id      = character(),
  creado_por      = character(),
  creado          = .req_t0(),
  actualizado_por = character(),
  actualizado     = .req_t0(),
  provision_id    = character(),   # FK to pasivos_provisions; NA = sin provisión
  ligada_por      = character(),
  ligada_en       = .req_t0(),
  papelera_por    = character(),
  papelera_en     = .req_t0()
)

.schema_req_carpetas <- function() tibble::tibble(
  id           = character(),
  padre_id     = character(),      # T61: parent folder id; NA = top level
  empresas     = character(),      # T61: JSON array of company ids; NA = every company
  nombre       = character(),
  color        = character(),      # #rrggbb
  presupuesto  = numeric(),        # optional estimated budget (costing a project)
  moneda       = character(),
  creado_por   = character(),
  creado       = .req_t0(),
  movido_por   = character(),      # T61: who last moved it, and when
  movido       = .req_t0(),
  papelera_por = character(),
  papelera_en  = .req_t0()
)

.schema_req_comentarios <- function() tibble::tibble(
  id             = character(),
  requisicion_id = character(),
  autor          = character(),
  creado         = .req_t0(),
  texto          = character()
)

.schema_req_leidos <- function() tibble::tibble(
  id             = character(),    # "<usuario>|<requisicion_id>"
  usuario        = character(),
  requisicion_id = character(),
  leido_hasta    = .req_t0()
)

.schema_req_evidencia <- function() tibble::tibble(
  id               = character(),
  requisicion_id   = character(),
  tipo             = character(),  # "adjunto" | "tabla"
  nombre           = character(),  # original file name, shown only (escaped)
  titulo           = character(),  # pasted table title
  ext              = character(),  # stored format: png jpg pdf xlsx json
  mime             = character(),
  es_imagen        = logical(),
  tamano           = numeric(),    # bytes stored
  ancho            = integer(),
  alto             = integer(),
  llave            = character(),  # full S3 key, built by .adj_llave()
  filas            = integer(),    # pasted table: rows kept
  columnas         = integer(),
  filas_pegadas    = integer(),    # rows/cols that were pasted (before the trim)
  columnas_pegadas = integer(),
  encabezado       = logical(),    # first row is a header
  creado_por       = character(),
  creado           = .req_t0(),
  papelera_por     = character(),
  papelera_en      = .req_t0()
)

.schema_req_historial <- function() tibble::tibble(
  id             = character(),
  requisicion_id = character(),
  accion         = character(),    # creada editada movida papelera restaurada ligada
                                   # comentario evidencia evidencia_papelera evidencia_restaurada
  detalle        = character(),
  por            = character(),
  en             = .req_t0()
)

.REQ_SCHEMAS <- list(
  requisiciones = .schema_requisiciones, carpetas = .schema_req_carpetas,
  comentarios = .schema_req_comentarios, leidos = .schema_req_leidos,
  evidencia = .schema_req_evidencia, historial = .schema_req_historial)

# ── Helpers ──────────────────────────────────────────────────────────────────

.req_cid <- function(client_id) {
  cid <- tolower(trimws(client_id %||% ""))
  if (length(cid) != 1L || is.na(cid) || !nzchar(cid))
    stop("[requisiciones] client_id es obligatorio", call. = FALSE)
  cid
}

.req_ahora <- function() as.POSIXct(Sys.time(), tz = "UTC")
.req_hoy   <- function() as.Date(format(Sys.time(), tz = .REQ_TZ))

# Strict read: the table as S3 holds it now, the empty schema only when S3
# says the file does not exist, stop() when it could not be read.
.req_leer <- function(tabla, cid) leer_tabla_para_escribir(REQ_KEYS[[tabla]], cid, .REQ_SCHEMAS[[tabla]])

.req_modificar <- function(tabla, cid, cambio)
  modificar_tabla(REQ_KEYS[[tabla]], cid, .REQ_SCHEMAS[[tabla]], cambio)

.req_txt <- function(x) {
  x <- if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
  if (is.na(x)) return(NA_character_)
  x <- trimws(gsub("[\u0001-\u0008\u000B\u000C\u000E-\u001F\u007F]", "", x))
  if (!nzchar(x)) NA_character_ else x
}

# Optional amount: NA, or a finite number >= 0.
.req_monto <- function(x, clave) {
  if (is.null(x) || !length(x) || (length(x) == 1L && (is.na(x) || identical(trimws(as.character(x)), ""))))
    return(NA_real_)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(v) || !is.finite(v) || v < 0) .req_error(clave)
  v
}

.req_prioridad <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || identical(trimws(as.character(x[[1]])), ""))
    return(NA_integer_)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(v) || v != round(v) || v < 1 || v > 10) .req_error("requisiciones.error.prioridad")
  as.integer(v)
}

.req_moneda <- function(x) {
  m <- toupper(.req_txt(x) %||% NA_character_)
  if (is.na(m)) return(NA_character_)
  if (!grepl("^[A-Z]{3}$", m)) .req_error("requisiciones.error.moneda")
  m
}

# Validated fields of a requisition. `parcial` = an edit: only the fields
# present in `datos` are checked and returned.
.REQ_EDITABLES <- c("titulo", "empresa", "proveedor", "monto", "moneda", "notas", "prioridad", "carpeta_id")

.req_validar <- function(datos, parcial = FALSE) {
  datos <- as.list(datos)
  campos <- if (parcial) intersect(names(datos), .REQ_EDITABLES) else .REQ_EDITABLES
  out <- list()
  for (k in campos) {
    v <- datos[[k]]
    out[[k]] <- switch(k,
      titulo = {
        t <- .req_txt(v)
        if (is.na(t) || nchar(t) > REQ_MAX_TITULO) .req_error("requisiciones.error.titulo", max = REQ_MAX_TITULO)
        t
      },
      empresa = {
        t <- .req_txt(v)
        if (is.na(t) || nchar(t) > 100L) .req_error("requisiciones.error.empresa")
        t
      },
      proveedor = {
        t <- .req_txt(v)
        if (!is.na(t) && nchar(t) > REQ_MAX_TITULO) .req_error("requisiciones.error.proveedor", max = REQ_MAX_TITULO)
        t
      },
      monto     = .req_monto(v, "requisiciones.error.monto"),
      moneda    = .req_moneda(v),
      notas     = {
        t <- if (is.null(v) || !length(v) || is.na(v[[1]])) NA_character_ else as.character(v[[1]])
        if (!is.na(t)) { t <- gsub("[\u0001-\u0008\u000B\u000C\u000E-\u001F\u007F]", "", t); if (!nzchar(trimws(t))) t <- NA_character_ }
        if (!is.na(t) && nchar(t) > REQ_MAX_NOTAS) .req_error("requisiciones.error.notas", max = REQ_MAX_NOTAS)
        t
      },
      prioridad  = .req_prioridad(v),
      carpeta_id = .req_txt(v))
  }
  # An amount needs its currency (a currency alone is harmless).
  if ("monto" %in% names(out) && !is.na(out$monto) &&
      is.na(out$moneda %||% datos$moneda_actual %||% NA_character_))
    .req_error("requisiciones.error.moneda")
  out
}

.req_fila <- function(df, id) {
  i <- which(df$id == id)
  if (!length(i)) .req_error("requisiciones.error.no_existe")
  i[1]
}

# ── Empresa contra carpeta (T61, Luis) ───────────────────────────────────────
# A requisition's company is one of its folder's (General, carpeta_id NA, has
# every company). Compared by company id (empresa_id_de(),
# R/empresas_identidad.R), never by text. With no company in the catalog
# there is nothing to compare against: only the folder is checked, as before.

# The client's company catalog, strict.
.req_empresas <- function(cid) leer_tabla_para_escribir(S3_KEYS$empresas, cid, .schema_empresas)

.req_catalogo_vacio <- function(empresas) !any(!(empresas$deleted %in% TRUE) & !is.na(empresas$id))

# TRUE where company `emp_id` is one of folder `carpeta_id`'s (on a tree).
.req_empresa_cabe <- function(emp_id, carpeta_id, arbol, empresas) {
  vapply(seq_along(emp_id), function(k) {
    if (is.na(carpeta_id[k])) return(TRUE)
    j <- match(carpeta_id[k], arbol$id)
    if (is.na(j)) return(TRUE)            # its folder is gone: shown in General
    e <- .req_carpeta_empresas(arbol$empresas[j], existentes = empresas$id)
    is.null(e) || emp_id[k] %in% e
  }, logical(1))
}

# The folder must be live and the company one of its own. `carpetas` is the
# folder table as just read. Returns the text to store: the company's current
# initials (one format, whatever the text was), or the text itself with an
# empty catalog.
.req_empresa_en_carpeta <- function(texto, carpeta_id, carpetas, empresas) {
  a <- requisiciones_carpetas_arbol(carpetas)
  if (!is.na(carpeta_id) && !any(a$id == carpeta_id & !a$en_papelera)) .req_error("requisiciones.error.carpeta")
  if (.req_catalogo_vacio(empresas)) return(texto)
  id <- empresa_id_de(texto, empresas)
  if (is.na(id)) .req_error("requisiciones.error.empresa_no_reconocida")
  if (!.req_empresa_cabe(id, carpeta_id, a, empresas)) .req_error("requisiciones.error.empresa_fuera_carpeta")
  empresas$initials[match(id, empresas$id)]
}

# Liabilities, strict: a deleted one's provisional provisions are "borrada" (T18).
.req_liabs <- function(cid) leer_tabla_para_escribir(S3_KEYS$pasivos_liabilities, cid, .schema_pasivos_liability)

# Stage of the provision(s) behind requisitions, from strict reads.
.req_etapa_provision <- function(cid, provision_id) {
  provision_id <- as.character(provision_id)
  if (!length(provision_id) || all(is.na(provision_id))) return(rep(NA_character_, length(provision_id)))
  provs <- leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, cid, .schema_pasivos_provision)
  conf  <- leer_tabla_para_escribir(S3_KEYS$bancos_confirmados, cid, .schema_bancos_confirmados)
  etapa_de_provision_en(provs, conf, provision_id, liabs = .req_liabs(cid))
}

# A requisition that can still be changed: exists, not paid, not in the trash.
.req_editable <- function(cid, id, permitir_papelera = FALSE) {
  reqs <- .req_leer("requisiciones", cid)
  fila <- reqs[.req_fila(reqs, id), , drop = FALSE]
  if (!is.na(fila$provision_id) && identical(.req_etapa_provision(cid, fila$provision_id), "pagada"))
    .req_error("requisiciones.error.pagada")
  if (!permitir_papelera && !is.na(fila$papelera_en)) .req_error("requisiciones.error.en_papelera")
  fila
}

# History rows. Written after the action itself succeeded; a failure here is
# logged, never turned into a failure of the action the user already saved.
.req_historial <- function(cid, requisicion_ids, accion, por, detalle = NA_character_) {
  if (!length(requisicion_ids)) return(invisible(FALSE))
  ahora <- .req_ahora()
  ok <- tryCatch({
    .req_modificar("historial", cid, function(df) dplyr::bind_rows(df, tibble::tibble(
      id = vapply(seq_along(requisicion_ids), function(i) uuid::UUIDgenerate(), character(1)),
      requisicion_id = as.character(requisicion_ids), accion = accion,
      detalle = as.character(detalle), por = por %||% NA_character_, en = ahora)))
    TRUE
  }, error = function(e) {
    message("[requisiciones] historial no se guardó (", accion, "): ", conditionMessage(e)); FALSE })
  invisible(ok)
}

# Next folio for `anio` on the fresh table: REQ-AAAA-0001, -0002, ...
.req_siguiente_folio <- function(folios, anio) {
  pref <- paste0("REQ-", anio, "-")
  nums <- suppressWarnings(as.integer(sub(pref, "", folios[startsWith(folios %||% character(), pref)], fixed = TRUE)))
  sprintf("%s%04d", pref, max(c(0L, nums), na.rm = TRUE) + 1L)
}

# ── Requisiciones ────────────────────────────────────────────────────────────

# datos: list(titulo, empresa, proveedor, monto, moneda, notas, prioridad,
#             carpeta_id, provision_id). Returns list(id, folio).
# provision_id is for "Agregar evidencia a una provisión": the requisition is
# born linked; refused if another requisition already has that provision.
requisicion_crear <- function(client_id, datos, por) {
  cid <- .req_cid(client_id)
  v <- .req_validar(datos)
  provision_id <- .req_txt(datos$provision_id)
  if (!is.na(provision_id)) .req_provision_libre(cid, provision_id)
  empresas <- .req_empresas(cid)
  id <- uuid::UUIDgenerate(); ahora <- .req_ahora(); folio <- NA_character_
  .req_modificar("requisiciones", cid, function(df) {
    if (!is.na(provision_id) && any(df$provision_id %in% provision_id))
      .req_error("requisiciones.error.provision_ocupada")
    # T61: live folder and one of its companies, on the folder table as read now.
    v$empresa <<- .req_empresa_en_carpeta(v$empresa, v$carpeta_id, .req_leer("carpetas", cid), empresas)
    folio <<- .req_siguiente_folio(df$folio, format(ahora, "%Y", tz = .REQ_TZ))
    dplyr::bind_rows(df, tibble::tibble(
      id = id, folio = folio, titulo = v$titulo, empresa = v$empresa, proveedor = v$proveedor,
      monto = v$monto, moneda = v$moneda, notas = v$notas, prioridad = v$prioridad,
      carpeta_id = v$carpeta_id, creado_por = por, creado = ahora,
      actualizado_por = por, actualizado = ahora,
      provision_id = provision_id,
      ligada_por = if (is.na(provision_id)) NA_character_ else por,
      ligada_en = if (is.na(provision_id)) as.POSIXct(NA, tz = "UTC") else ahora))
  })
  .req_historial(cid, id, "creada", por, folio)
  if (!is.na(provision_id)) .req_historial(cid, id, "ligada", por, provision_id)
  list(id = id, folio = folio)
}

# cambios: any of titulo, empresa, proveedor, monto, moneda, notas, prioridad,
# carpeta_id. Other names are ignored. Paid or trashed requisitions refuse.
requisicion_editar <- function(client_id, id, cambios, por) {
  cid <- .req_cid(client_id)
  actual <- .req_editable(cid, id)
  cambios <- as.list(cambios)
  if (!is.null(cambios$monto) && is.null(cambios$moneda)) cambios$moneda_actual <- actual$moneda
  v <- .req_validar(cambios, parcial = TRUE)
  if (!length(v)) return(invisible(FALSE))
  # T61: changing the company or the folder checks the pair; editing anything
  # else never forces a requisition already outside its folder to change.
  revisa <- any(c("empresa", "carpeta_id") %in% names(v))
  empresas <- if (revisa) .req_empresas(cid)
  ahora <- .req_ahora(); cambiados <- character()
  .req_modificar("requisiciones", cid, function(df) {
    i <- .req_fila(df, id)
    if (revisa) {
      emp <- .req_empresa_en_carpeta(v$empresa %||% df$empresa[i],
                                     if ("carpeta_id" %in% names(v)) v$carpeta_id else df$carpeta_id[i],
                                     .req_leer("carpetas", cid), empresas)
      if ("empresa" %in% names(v)) v$empresa <- emp
    }
    for (k in names(v)) {
      antes <- df[[k]][i]
      if (!identical(antes, v[[k]]) && !(is.na(antes) && is.na(v[[k]]))) {
        df[[k]][i] <- v[[k]]; cambiados <<- c(cambiados, k)
      }
    }
    if (length(cambiados)) { df$actualizado_por[i] <- por; df$actualizado[i] <- ahora }
    df
  })
  if (length(cambiados)) .req_historial(cid, id, "editada", por, paste(cambiados, collapse = ","))
  invisible(length(cambiados) > 0L)
}

# "Mover a carpeta…" on several rows; carpeta_id NA = no folder.
requisiciones_mover <- function(client_id, ids, carpeta_id, por) {
  cid <- .req_cid(client_id)
  carpeta_id <- .req_txt(carpeta_id)
  empresas <- .req_empresas(cid)
  ahora <- .req_ahora(); movidas <- character()
  .req_modificar("requisiciones", cid, function(df) {
    # %in% matches NA with NA, so this also covers "no folder".
    i <- which(df$id %in% ids & is.na(df$papelera_en) & !(df$carpeta_id %in% carpeta_id))
    # T61: every one must be of a company of the destination; if any is not,
    # none moves (the message says how many to correct first).
    a <- requisiciones_carpetas_arbol(.req_leer("carpetas", cid))
    if (!is.na(carpeta_id) && !any(a$id == carpeta_id & !a$en_papelera)) .req_error("requisiciones.error.carpeta")
    if (length(i) && !.req_catalogo_vacio(empresas)) {
      eid <- empresa_id_de(df$empresa[i], empresas)
      ok <- !is.na(eid) & .req_empresa_cabe(eid, rep(carpeta_id, length(i)), a, empresas)
      if (any(!ok)) .req_error("requisiciones.error.empresa_fuera_mover", n = sum(!ok))
    }
    df$carpeta_id[i] <- carpeta_id; df$actualizado_por[i] <- por; df$actualizado[i] <- ahora
    movidas <<- df$id[i]
    df
  })
  .req_historial(cid, movidas, "movida", por, carpeta_id)
  invisible(movidas)
}

# Which of `filas` (requisition rows) this user may send to the trash:
# their own with can_trash_own_requisiciones, others' with
# can_trash_others_requisiciones. The screen still asks puede_o_avisa().
requisiciones_puede_papelera <- function(usuario, filas) {
  if (is.null(usuario) || !nrow(filas)) return(logical(nrow(filas)))
  propia <- tolower(filas$creado_por %||% "") == tolower(usuario$user %||% "")
  ifelse(propia, puede(usuario, "can_trash_own_requisiciones"),
                 puede(usuario, "can_trash_others_requisiciones"))
}

# Trash is a mark: the row, its evidence and comments stay.
requisiciones_papelera <- function(client_id, ids, por) {
  cid <- .req_cid(client_id); ahora <- .req_ahora(); hechas <- character()
  .req_modificar("requisiciones", cid, function(df) {
    i <- which(df$id %in% ids & is.na(df$papelera_en))
    df$papelera_por[i] <- por; df$papelera_en[i] <- ahora
    hechas <<- df$id[i]
    df
  })
  .req_historial(cid, hechas, "papelera", por)
  invisible(hechas)
}

requisiciones_restaurar <- function(client_id, ids, por) {
  cid <- .req_cid(client_id); hechas <- character()
  .req_modificar("requisiciones", cid, function(df) {
    i <- which(df$id %in% ids & !is.na(df$papelera_en))
    df$papelera_por[i] <- NA_character_; df$papelera_en[i] <- as.POSIXct(NA, tz = "UTC")
    hechas <<- df$id[i]
    df
  })
  .req_historial(cid, hechas, "restaurada", por)
  invisible(hechas)
}

# A provision may hold at most one requisition, and must exist and be alive.
.req_provision_libre <- function(cid, provision_id) {
  etapa <- .req_etapa_provision(cid, provision_id)
  if (is.na(etapa) || etapa == "borrada") .req_error("requisiciones.error.provision_no_existe")
  reqs <- .req_leer("requisiciones", cid)
  if (any(reqs$provision_id %in% provision_id)) .req_error("requisiciones.error.provision_ocupada")
  invisible(etapa)
}

# After "Crear provisión" saves: link the new provision. Refused if the
# requisition already has a live provision or another requisition holds it.
requisicion_ligar_provision <- function(client_id, id, provision_id, por) {
  cid <- .req_cid(client_id)
  provision_id <- .req_txt(provision_id)
  if (is.na(provision_id)) .req_error("requisiciones.error.provision_no_existe")
  actual <- .req_editable(cid, id)
  if (!is.na(actual$provision_id) &&
      !(.req_etapa_provision(cid, actual$provision_id) %in% c(NA_character_, "borrada")))
    .req_error("requisiciones.error.ya_ligada")
  .req_provision_libre(cid, provision_id)
  ahora <- .req_ahora()
  .req_modificar("requisiciones", cid, function(df) {
    i <- .req_fila(df, id)
    if (any(df$provision_id[-i] %in% provision_id)) .req_error("requisiciones.error.provision_ocupada")
    if (!identical(df$provision_id[i], actual$provision_id) && !(is.na(df$provision_id[i]) && is.na(actual$provision_id)))
      .req_error("requisiciones.error.ya_ligada")
    df$provision_id[i] <- provision_id; df$ligada_por[i] <- por; df$ligada_en[i] <- ahora
    df$actualizado_por[i] <- por; df$actualizado[i] <- ahora
    df
  })
  .req_historial(cid, id, "ligada", por, provision_id)
  invisible(TRUE)
}

# ── Carpetas ─────────────────────────────────────────────────────────────────
# T61: folders inside folders, up to REQ_CARPETA_MAX_NIVEL levels, each tied
# to companies. One flat table: `id` is the only key, `padre_id` the parent
# (NA = top level), `empresas` a JSON array of company ids (NA = every
# company, also those added later). Never paths or visible names as keys:
# renaming or moving changes one cell and nothing inside it. Files written
# before T61 have neither column; .normalize() reads them as NA (top level,
# every company) and nothing is migrated.
# "General" is virtual: a requisition with carpeta_id NA. It has every
# company, is never hidden and is never written to S3 (no sentinel row).
# Reading never trusts the tree: a cycle, a parent that does not exist or a
# branch deeper than the limit (a race, a hand edit, old data) is read with
# that folder at the top level -- every walk ends and no folder disappears.

REQ_CARPETA_MAX_NIVEL <- 5L

# empresas cell -> company ids, or NULL = every company. Empty, unreadable or
# broken JSON = every company; so is a list none of whose ids is still in
# `existentes` (when given), so that a folder never becomes invisible for good.
.req_carpeta_empresas <- function(x, existentes = NULL) {
  x <- if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  ids <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
  ids <- if (is.character(ids)) unique(ids[!is.na(ids) & nzchar(ids)]) else character()
  if (!length(ids)) return(NULL)
  if (!is.null(existentes) && !any(ids %in% existentes)) return(NULL)
  ids
}

# The tree as it is read, from the whole table (trashed rows too). Adds:
#   padre_ef     the parent actually used (NA = top level)
#   nivel        1 = top level
#   ruta         "A › B › C"
#   en_papelera  this folder or one of its ancestors is in the trash
# Rows in tree order: each parent before its children, siblings in creation
# order. Pure; every loop is bounded by the number of rows.
requisiciones_carpetas_arbol <- function(carpetas) {
  n <- nrow(carpetas); ids <- carpetas$id
  if (!n) {
    carpetas$padre_ef <- character(); carpetas$nivel <- integer()
    carpetas$ruta <- character(); carpetas$en_papelera <- logical()
    return(carpetas)
  }
  creado <- as.numeric(carpetas$creado)
  padre <- as.character(carpetas$padre_id)
  padre[!(padre %in% ids) | (!is.na(padre) & padre == ids)] <- NA_character_   # orphan or itself
  arriba <- function(i) match(padre[i], ids)       # NA at the top
  # Cycles: the member created first goes to the top. Each pass removes one
  # link, so there are at most n passes.
  for (pasada in seq_len(n)) {
    roto <- FALSE
    for (i in seq_len(n)) {
      visto <- integer(); j <- i
      while (!is.na(j) && !(j %in% visto)) { visto <- c(visto, j); j <- arriba(j) }
      if (!is.na(j)) {
        ciclo <- visto[match(j, visto):length(visto)]
        padre[ciclo[order(creado[ciclo], ids[ciclo])[1]]] <- NA_character_
        roto <- TRUE; break
      }
    }
    if (!roto) break
  }
  nivel_de <- function() vapply(seq_len(n), function(i) {
    k <- 1L; j <- arriba(i)
    while (!is.na(j) && k <= n) { k <- k + 1L; j <- arriba(j) }
    k
  }, integer(1))
  # Too deep: the first folder past the limit goes to the top, with its branch.
  for (pasada in seq_len(n)) {
    nv <- nivel_de(); malo <- which(nv == REQ_CARPETA_MAX_NIVEL + 1L)
    if (!length(malo)) break
    padre[malo] <- NA_character_
  }
  nv <- nivel_de()
  papelera <- !is.na(carpetas$papelera_en)
  ruta <- character(n); en_pap <- logical(n)
  for (i in seq_len(n)) {
    cad <- i; j <- arriba(i)
    while (!is.na(j)) { cad <- c(j, cad); j <- arriba(j) }
    ruta[i] <- paste(carpetas$nombre[cad], collapse = " › ")
    en_pap[i] <- any(papelera[cad])
  }
  orden <- integer()
  visita <- function(p) {
    h <- which(if (is.na(p)) is.na(padre) else padre %in% ids[p])
    for (k in h[order(creado[h], ids[h])]) if (!(k %in% orden)) { orden <<- c(orden, k); visita(k) }
  }
  visita(NA_integer_)
  orden <- c(orden, setdiff(seq_len(n), orden))   # never needed; never lose a row
  carpetas$padre_ef <- padre; carpetas$nivel <- nv; carpetas$ruta <- ruta; carpetas$en_papelera <- en_pap
  carpetas[orden, , drop = FALSE]
}

# Ids of `id` and everything under it (on a tree from requisiciones_carpetas_arbol()).
.req_carpeta_subarbol <- function(arbol, id) {
  dentro <- id
  repeat {
    nuevos <- setdiff(arbol$id[arbol$padre_ef %in% dentro], dentro)
    if (!length(nuevos)) break
    dentro <- c(dentro, nuevos)
  }
  dentro
}

# Which folders show with the company bubbles: TRUE where the folder is for
# every company or shares one with `activas` (company ids), and its parent
# shows. activas NULL = no filter. `existentes` = every company id in the
# catalog (see .req_carpeta_empresas()). On a tree in tree order.
requisiciones_carpetas_visibles <- function(arbol, activas, existentes = NULL) {
  n <- nrow(arbol)
  if (is.null(activas)) return(rep(TRUE, n))
  vis <- vapply(seq_len(n), function(i) {
    e <- .req_carpeta_empresas(arbol$empresas[i], existentes)
    is.null(e) || any(e %in% activas)
  }, logical(1))
  for (i in seq_len(n)) {
    j <- match(arbol$padre_ef[i], arbol$id)
    if (!is.na(j)) vis[i] <- vis[i] && vis[j]
  }
  vis
}

.req_carpeta_validar <- function(datos, parcial = FALSE) {
  datos <- as.list(datos); out <- list()
  if (!parcial || "nombre" %in% names(datos)) {
    n <- .req_txt(datos$nombre)
    if (is.na(n) || nchar(n) > 60L) .req_error("requisiciones.error.carpeta_nombre")
    out$nombre <- n
  }
  if (!parcial || "color" %in% names(datos)) {
    col <- .req_txt(datos$color) %||% NA_character_
    if (is.na(col)) col <- REQ_COLOR_DEFECTO
    if (!grepl("^#[0-9A-Fa-f]{6}$", col)) .req_error("requisiciones.error.color")
    out$color <- tolower(col)
  }
  if (!parcial || "presupuesto" %in% names(datos))
    out$presupuesto <- .req_monto(datos$presupuesto, "requisiciones.error.presupuesto")
  if (!parcial || "moneda" %in% names(datos)) out$moneda <- .req_moneda(datos$moneda)
  if (!is.null(out$presupuesto) && !is.na(out$presupuesto) && is.na(out$moneda %||% datos$moneda_actual %||% NA))
    .req_error("requisiciones.error.moneda")
  # empresas: NA / absent = every company; otherwise at least one id.
  if (!parcial || "empresas" %in% names(datos)) {
    e <- datos$empresas
    out$empresas <- if (is.null(e) || (length(e) == 1L && is.na(e))) NA_character_ else {
      e <- unique(as.character(e)); e <- e[!is.na(e) & nzchar(e)]
      if (!length(e)) .req_error("requisiciones.error.carpeta_empresas")
      as.character(jsonlite::toJSON(sort(e)))
    }
  }
  out
}

# Same name (case aside) among the live folders with the same parent.
.req_carpeta_repetida <- function(arbol, nombre, padre, salvo = NA_character_) {
  any(!arbol$en_papelera & arbol$padre_ef %in% padre & tolower(arbol$nombre) == tolower(nombre) &
        !(arbol$id %in% salvo))
}

# A folder can go under `padre` (NA = top level): it exists and is live, it
# is not `mueve` nor under it, and the branch fits in REQ_CARPETA_MAX_NIVEL.
.req_carpeta_destino <- function(arbol, padre, mueve = NULL) {
  nivel_padre <- 0L
  if (!is.na(padre)) {
    j <- match(padre, arbol$id)
    if (is.na(j) || arbol$en_papelera[j]) .req_error("requisiciones.error.carpeta_padre")
    nivel_padre <- arbol$nivel[j]
  }
  alto <- 1L
  if (!is.null(mueve)) {
    sub <- .req_carpeta_subarbol(arbol, mueve)
    if (!is.na(padre) && padre %in% sub) .req_error("requisiciones.error.carpeta_ciclo")
    alto <- max(arbol$nivel[arbol$id %in% sub]) - arbol$nivel[match(mueve, arbol$id)] + 1L
  }
  if (nivel_padre + alto > REQ_CARPETA_MAX_NIVEL)
    .req_error("requisiciones.error.carpeta_nivel", max = REQ_CARPETA_MAX_NIVEL)
  invisible(TRUE)
}

# datos: list(nombre, color, presupuesto, moneda, empresas, padre_id).
# Returns the id. Every check runs on the freshly read table.
requisiciones_carpeta_crear <- function(client_id, datos, por) {
  cid <- .req_cid(client_id)
  datos <- as.list(datos)
  v <- .req_carpeta_validar(datos)
  padre <- .req_txt(datos$padre_id)
  id <- uuid::UUIDgenerate(); ahora <- .req_ahora()
  .req_modificar("carpetas", cid, function(df) {
    a <- requisiciones_carpetas_arbol(df)
    .req_carpeta_destino(a, padre)
    if (.req_carpeta_repetida(a, v$nombre, padre)) .req_error("requisiciones.error.carpeta_repetida")
    dplyr::bind_rows(df, tibble::tibble(
      id = id, padre_id = padre, empresas = v$empresas,
      nombre = v$nombre, color = v$color, presupuesto = v$presupuesto, moneda = v$moneda,
      creado_por = por, creado = ahora, movido_por = NA_character_, movido = as.POSIXct(NA, tz = "UTC"),
      papelera_por = NA_character_, papelera_en = as.POSIXct(NA, tz = "UTC")))
  })
  id
}

# cambios: any of nombre, color, presupuesto, moneda, empresas, padre_id
# (padre_id present = move it there; NA = top level). Moving touches no
# requisition: what is inside goes along.
requisiciones_carpeta_editar <- function(client_id, id, cambios, por) {
  cid <- .req_cid(client_id)
  cambios <- as.list(cambios)
  mueve <- "padre_id" %in% names(cambios)
  padre <- if (mueve) .req_txt(cambios$padre_id) else NULL
  ahora <- .req_ahora()
  .req_modificar("carpetas", cid, function(df) {
    i <- .req_fila(df, id)
    a <- requisiciones_carpetas_arbol(df)
    if (!is.null(cambios$presupuesto) && is.null(cambios$moneda)) cambios$moneda_actual <- df$moneda[i]
    v <- .req_carpeta_validar(cambios, parcial = TRUE)
    padre_antes <- a$padre_ef[match(id, a$id)]
    se_mueve <- mueve && !identical(padre, padre_antes)
    if (se_mueve) .req_carpeta_destino(a, padre, mueve = id)
    nombre <- v$nombre %||% df$nombre[i]
    if ((!is.null(v$nombre) || se_mueve) &&
        .req_carpeta_repetida(a, nombre, if (se_mueve) padre else padre_antes, salvo = id))
      .req_error("requisiciones.error.carpeta_repetida")
    for (k in names(v)) df[[k]][i] <- v[[k]]
    if (se_mueve) { df$padre_id[i] <- padre; df$movido_por[i] <- por; df$movido[i] <- ahora }
    df
  })
  invisible(TRUE)
}

# A folder holding live requisitions -- in it or anywhere under it -- cannot
# go to the trash (Luis, 2026-11-04): they must be moved or trashed first.
# Trashed and paid ones do not count. Only this folder is marked; what is
# under it is hidden with it and comes back with it.
requisiciones_carpeta_papelera <- function(client_id, id, por) {
  cid <- .req_cid(client_id)
  reqs <- .req_leer("requisiciones", cid)
  vivas <- reqs[!is.na(reqs$carpeta_id) & is.na(reqs$papelera_en), , drop = FALSE]
  if (nrow(vivas)) vivas <- vivas[!(.req_etapa_provision(cid, vivas$provision_id) %in% "pagada"), , drop = FALSE]
  ahora <- .req_ahora()
  .req_modificar("carpetas", cid, function(df) {
    i <- .req_fila(df, id)
    n <- sum(vivas$carpeta_id %in% .req_carpeta_subarbol(requisiciones_carpetas_arbol(df), id))
    if (n) .req_error("requisiciones.error.carpeta_con_requisiciones", n = n)
    if (is.na(df$papelera_en[i])) { df$papelera_por[i] <- por; df$papelera_en[i] <- ahora }
    df
  })
  invisible(TRUE)
}

# Back with everything under it. If its parent is still in the trash it goes
# under the nearest live ancestor (or to the top), in the same write.
requisiciones_carpeta_restaurar <- function(client_id, id, por) {
  cid <- .req_cid(client_id)
  ahora <- .req_ahora()
  .req_modificar("carpetas", cid, function(df) {
    i <- .req_fila(df, id)
    a <- requisiciones_carpetas_arbol(df)
    antes <- a$padre_ef[match(id, a$id)]; p <- antes
    while (!is.na(p) && a$en_papelera[match(p, a$id)]) p <- a$padre_ef[match(p, a$id)]
    if (.req_carpeta_repetida(a, df$nombre[i], p, salvo = id)) .req_error("requisiciones.error.carpeta_repetida")
    df$papelera_por[i] <- NA_character_; df$papelera_en[i] <- as.POSIXct(NA, tz = "UTC")
    if (!identical(p, antes)) { df$padre_id[i] <- p; df$movido_por[i] <- por; df$movido[i] <- ahora }
    df
  })
  invisible(TRUE)
}

# Live folders, in tree order, with what each holds counting everything
# under it: n_requisiciones (visible ones), comprometido (the shown amounts
# in the folder's own currency -- never two currencies summed) and
# n_otra_moneda (those with an amount in another currency: not summed, not
# hidden either; the panel says so).
requisiciones_carpetas_resumen <- function(carpetas, vista) {
  a <- requisiciones_carpetas_arbol(carpetas)
  cs <- a[!a$en_papelera, , drop = FALSE]
  v <- vista[!(vista$etapa %in% c("pagada", "papelera")), , drop = FALSE]
  cs$n_requisiciones <- integer(nrow(cs)); cs$comprometido <- numeric(nrow(cs)); cs$n_otra_moneda <- integer(nrow(cs))
  for (i in seq_len(nrow(cs))) {
    x <- v[v$carpeta_id %in% .req_carpeta_subarbol(cs, cs$id[i]), , drop = FALSE]
    cs$n_requisiciones[i] <- nrow(x)
    misma <- x$moneda_mostrada %in% cs$moneda[i]
    cs$comprometido[i] <- sum(x$monto_mostrado[misma], na.rm = TRUE)
    cs$n_otra_moneda[i] <- sum(!misma & !is.na(x$monto_mostrado))
  }
  cs
}

# Folders, strict read, in tree order (requisiciones_carpetas_arbol()):
# live only, unless incluir_papelera.
requisiciones_carpetas <- function(client_id, incluir_papelera = FALSE) {
  a <- requisiciones_carpetas_arbol(.req_leer("carpetas", .req_cid(client_id)))
  a[incluir_papelera | !a$en_papelera, , drop = FALSE]
}

# ── Comentarios ──────────────────────────────────────────────────────────────

# Saves the comment, marks it read for its author, and notifies in Panorama
# (avisos_personales(): the requisition's creator and everyone who commented
# before, never the author). Returns list(id, aviso_ok): the comment is saved
# even if the notice fails.
requisicion_comentar <- function(client_id, requisicion_id, texto, por) {
  cid <- .req_cid(client_id)
  t <- if (is.null(texto) || !length(texto) || is.na(texto[[1]])) "" else as.character(texto[[1]])
  t <- trimws(gsub("[\u0001-\u0008\u000B\u000C\u000E-\u001F\u007F]", "", t))
  if (!nzchar(t) || nchar(t) > REQ_MAX_COMENTARIO) .req_error("requisiciones.error.comentario", max = REQ_MAX_COMENTARIO)
  fila <- .req_editable(cid, requisicion_id)
  id <- uuid::UUIDgenerate(); ahora <- .req_ahora(); previos <- character()
  .req_modificar("comentarios", cid, function(df) {
    previos <<- df$autor[df$requisicion_id %in% requisicion_id]
    dplyr::bind_rows(df, tibble::tibble(id = id, requisicion_id = requisicion_id,
                                        autor = por, creado = ahora, texto = t))
  })
  tryCatch(requisiciones_marcar_leido(cid, requisicion_id, por, hasta = ahora),
           error = function(e) message("[requisiciones] marca de leído falló: ", conditionMessage(e)))
  .req_historial(cid, requisicion_id, "comentario", por, id)
  destino <- unique(stats::na.omit(c(fila$creado_por, previos)))
  aviso_ok <- tryCatch({
    avisos_personales(cid, destino, list(
      titulo = tr("requisiciones.aviso.comentario", folio = fila$folio),
      cuerpo = if (nchar(t) > 140L) paste0(substr(t, 1L, 140L), "…") else t,
      severidad = "info", clave = paste0("req:", requisicion_id),
      enlace = "requisiciones", autor = por))
    TRUE
  }, error = function(e) { message("[requisiciones] aviso de comentario falló: ", conditionMessage(e)); FALSE })
  list(id = id, aviso_ok = aviso_ok)
}

requisicion_comentarios <- function(client_id, requisicion_id) {
  df <- .req_leer("comentarios", .req_cid(client_id))
  df <- df[df$requisicion_id %in% requisicion_id, , drop = FALSE]
  df[order(df$creado), , drop = FALSE]
}

requisiciones_marcar_leido <- function(client_id, requisicion_id, usuario, hasta = .req_ahora()) {
  cid <- .req_cid(client_id)
  u <- tolower(usuario %||% "")
  if (!nzchar(u)) stop("[requisiciones] usuario es obligatorio", call. = FALSE)
  llave <- paste0(u, "|", requisicion_id)
  .req_modificar("leidos", cid, function(df) {
    i <- which(df$id == llave)
    if (length(i)) {
      if (is.na(df$leido_hasta[i[1]]) || df$leido_hasta[i[1]] < hasta) df$leido_hasta[i[1]] <- hasta
      df
    } else dplyr::bind_rows(df, tibble::tibble(id = llave, usuario = u,
                                               requisicion_id = requisicion_id, leido_hasta = hasta))
  })
  invisible(TRUE)
}

# ── Evidencia ────────────────────────────────────────────────────────────────

.req_evid_vivas <- function(df, requisicion_id, tipo)
  sum(df$requisicion_id %in% requisicion_id & df$tipo %in% tipo & is.na(df$papelera_en))

# preparado: the result of adjunto_preparar() (R/adjuntos_s3.R). The bytes go
# to S3 first under a server-made key; the row is added only once they are
# there. Returns the evidence id.
evidencia_adjuntar <- function(client_id, requisicion_id, preparado, por) {
  cid <- .req_cid(client_id)
  .req_editable(cid, requisicion_id)
  if (.req_evid_vivas(.req_leer("evidencia", cid), requisicion_id, "adjunto") >= ADJ_MAX_POR_REQ)
    .req_error("requisiciones.evidencia.error_limite_adjuntos", n = ADJ_MAX_POR_REQ)
  id <- uuid::UUIDgenerate()
  llave <- .adj_llave(cid, requisicion_id, id, preparado$ext)
  .adj_s3_poner(preparado$bytes, llave, preparado$mime)
  ahora <- .req_ahora()
  .req_modificar("evidencia", cid, function(df) {
    if (.req_evid_vivas(df, requisicion_id, "adjunto") >= ADJ_MAX_POR_REQ)
      .req_error("requisiciones.evidencia.error_limite_adjuntos", n = ADJ_MAX_POR_REQ)
    dplyr::bind_rows(df, tibble::tibble(
      id = id, requisicion_id = requisicion_id, tipo = "adjunto", nombre = preparado$nombre,
      ext = preparado$ext, mime = preparado$mime, es_imagen = isTRUE(preparado$es_imagen),
      tamano = as.numeric(preparado$tamano), ancho = as.integer(preparado$ancho),
      alto = as.integer(preparado$alto), llave = llave, creado_por = por, creado = ahora))
  })
  .req_historial(cid, requisicion_id, "evidencia", por, id)
  id
}

# preparada: the result of tabla_pegada_preparar(), after the preview.
# encabezado: what the user left on the "first row is header" switch.
evidencia_tabla_agregar <- function(client_id, requisicion_id, preparada, titulo, encabezado, por) {
  cid <- .req_cid(client_id)
  .req_editable(cid, requisicion_id)
  t <- .req_txt(titulo)
  if (!is.na(t) && nchar(t) > REQ_MAX_TITULO) t <- substr(t, 1L, REQ_MAX_TITULO)
  if (.req_evid_vivas(.req_leer("evidencia", cid), requisicion_id, "tabla") >= TABLA_MAX_POR_REQ)
    .req_error("requisiciones.evidencia.error_limite_tablas", n = TABLA_MAX_POR_REQ)
  celdas <- preparada$celdas
  if (!is.matrix(celdas) || nrow(celdas) > TABLA_MAX_FILAS || ncol(celdas) > TABLA_MAX_COLS)
    .req_error("requisiciones.tabla.error_vacia")
  id <- uuid::UUIDgenerate()
  llave <- .adj_llave(cid, requisicion_id, id, "json")
  # No auto_unbox: a one-column row must stay an array.
  cuerpo <- jsonlite::toJSON(list(celdas = lapply(seq_len(nrow(celdas)), function(r) unname(celdas[r, ])),
                                  encabezado = isTRUE(encabezado)), auto_unbox = FALSE)
  .adj_s3_poner(charToRaw(enc2utf8(as.character(cuerpo))), llave, "application/json")
  ahora <- .req_ahora()
  .req_modificar("evidencia", cid, function(df) {
    if (.req_evid_vivas(df, requisicion_id, "tabla") >= TABLA_MAX_POR_REQ)
      .req_error("requisiciones.evidencia.error_limite_tablas", n = TABLA_MAX_POR_REQ)
    dplyr::bind_rows(df, tibble::tibble(
      id = id, requisicion_id = requisicion_id, tipo = "tabla", titulo = t, ext = "json",
      mime = "application/json", es_imagen = FALSE, tamano = as.numeric(nchar(cuerpo, type = "bytes")),
      llave = llave, filas = as.integer(nrow(celdas)), columnas = as.integer(ncol(celdas)),
      filas_pegadas = as.integer(preparada$filas_pegadas), columnas_pegadas = as.integer(preparada$columnas_pegadas),
      encabezado = isTRUE(encabezado), creado_por = por, creado = ahora))
  })
  .req_historial(cid, requisicion_id, "evidencia", por, id)
  id
}

# Evidence of a requisition (live only unless incluir_papelera).
requisicion_evidencia <- function(client_id, requisicion_id, incluir_papelera = FALSE) {
  df <- .req_leer("evidencia", .req_cid(client_id))
  df <- df[df$requisicion_id %in% requisicion_id & (incluir_papelera | is.na(df$papelera_en)), , drop = FALSE]
  df[order(df$creado), , drop = FALSE]
}

# Removing evidence sends it to the trash; the S3 object stays.
evidencia_papelera <- function(client_id, evid_id, por) {
  cid <- .req_cid(client_id)
  ev <- .req_leer("evidencia", cid)
  fila <- ev[.req_fila(ev, evid_id), , drop = FALSE]
  .req_editable(cid, fila$requisicion_id)
  .req_modificar("evidencia", cid, function(df) {
    i <- .req_fila(df, evid_id)
    if (is.na(df$papelera_en[i])) { df$papelera_por[i] <- por; df$papelera_en[i] <- .req_ahora() }
    df
  })
  .req_historial(cid, fila$requisicion_id, "evidencia_papelera", por, evid_id)
  invisible(TRUE)
}

evidencia_restaurar <- function(client_id, evid_id, por) {
  cid <- .req_cid(client_id)
  ev <- .req_leer("evidencia", cid)
  fila <- ev[.req_fila(ev, evid_id), , drop = FALSE]
  .req_editable(cid, fila$requisicion_id)
  .req_modificar("evidencia", cid, function(df) {
    i <- .req_fila(df, evid_id)
    if (is.na(df$papelera_en[i])) return(df)
    tope <- if (df$tipo[i] == "tabla") TABLA_MAX_POR_REQ else ADJ_MAX_POR_REQ
    if (.req_evid_vivas(df, df$requisicion_id[i], df$tipo[i]) >= tope)
      .req_error(if (df$tipo[i] == "tabla") "requisiciones.evidencia.error_limite_tablas"
                 else "requisiciones.evidencia.error_limite_adjuntos", n = tope)
    df$papelera_por[i] <- NA_character_; df$papelera_en[i] <- as.POSIXct(NA, tz = "UTC")
    df
  })
  .req_historial(cid, fila$requisicion_id, "evidencia_restaurada", por, evid_id)
  invisible(TRUE)
}

# Bytes of an attachment, for display (images, already re-encoded) or
# download (PDF, XLSX). The key is the one stored, built by the server, and
# must sit under this client's prefix.
evidencia_bytes <- function(client_id, evid_id) {
  cid <- .req_cid(client_id)
  ev <- .req_leer("evidencia", cid)
  fila <- ev[.req_fila(ev, evid_id), , drop = FALSE]
  if (!startsWith(fila$llave, paste0(cid, "/adjuntos/"))) stop("[requisiciones] llave ajena", call. = FALSE)
  list(bytes = .adj_s3_traer(fila$llave), mime = fila$mime, nombre = fila$nombre, ext = fila$ext)
}

# A stored pasted table: list(celdas = character matrix, encabezado).
evidencia_tabla <- function(client_id, evid_id) {
  b <- evidencia_bytes(client_id, evid_id)
  x <- jsonlite::fromJSON(rawToChar(b$bytes), simplifyVector = FALSE)
  filas <- lapply(x$celdas, function(r) vapply(r, function(v) if (is.null(v)) "" else as.character(v), character(1)))
  nc <- max(c(0L, lengths(filas)))
  celdas <- matrix("", nrow = length(filas), ncol = nc)
  for (r in seq_along(filas)) celdas[r, seq_along(filas[[r]])] <- filas[[r]]
  Encoding(celdas) <- "UTF-8"
  list(celdas = celdas, encabezado = isTRUE(unlist(x$encabezado)[1]))
}

requisicion_historial <- function(client_id, requisicion_id) {
  df <- .req_leer("historial", .req_cid(client_id))
  df <- df[df$requisicion_id %in% requisicion_id, , drop = FALSE]
  df[order(df$en), , drop = FALSE]
}

# ── Stage, amount and counts: deduced on read ────────────────────────────────

# Pure core. Every input is a normalized table; `usuario` (a username) adds
# the unread-comment count. One row per requisition with:
#   etapa, provision_perdida (it had a provision that no longer exists),
#   monto_mostrado / moneda_mostrada / monto_fuente: the firmest figure, never
#     two summed -- confirmed payment -> item -> provision -> estimate,
#   fecha_provision, dias_sin_provision, carpeta_nombre / carpeta_ruta /
#     carpeta_color,
#   empresa_id, empresa_aviso (T61, with `empresas`, the catalog): NA, or
#     "no_reconocida" (the text names no company) / "fuera_carpeta" (a
#     company that is not one of its folder's). Only marked, never rewritten.
#   n_adjuntos, n_imagenes, n_tablas, primera_imagen_id,
#   n_comentarios, comentarios_sin_leer.
requisiciones_vista_en <- function(reqs, provs, conf, manual, carpetas, evid, coms,
                                   leidos = NULL, usuario = NULL, hoy = .req_hoy(), liabs = NULL,
                                   empresas = NULL) {
  n <- nrow(reqs)
  pid <- reqs$provision_id
  ep <- rep(NA_character_, n)
  con <- !is.na(pid)
  if (any(con)) ep[con] <- etapa_de_provision_en(provs, conf, pid[con], liabs = liabs)
  etapa <- ifelse(ep %in% "pagada", "pagada",
           ifelse(!is.na(reqs$papelera_en), "papelera",
           ifelse(ep %in% c("provisionada", "en_pago"), ep, "sin_provision")))
  reqs$etapa <- etapa
  reqs$provision_perdida <- con & !(ep %in% c("provisionada", "en_pago", "pagada"))

  # Firmest amount.
  monto <- reqs$monto; moneda <- reqs$moneda; fuente <- ifelse(is.na(monto), NA_character_, "estimado")
  ip <- match(pid, provs$id)
  m_prov <- dplyr::coalesce(provs$amount_pago_override[ip], provs$amount_pago[ip])
  usa <- con & !is.na(ip) & !is.na(m_prov) & !(ep %in% c(NA_character_, "borrada"))
  monto[usa] <- m_prov[usa]; moneda[usa] <- provs$moneda_pago[ip][usa]; fuente[usa] <- "provision"
  if (nrow(manual) && "provision_id" %in% names(manual)) {
    im <- match(pid, manual$provision_id)
    usa <- con & !is.na(im) & !is.na(manual$Importe[im]) & !(ep %in% c(NA_character_, "borrada"))
    monto[usa] <- manual$Importe[im][usa]; moneda[usa] <- manual$Moneda[im][usa]; fuente[usa] <- "item"
  }
  vivas <- confirmaciones_vigentes(conf)
  if (!is.null(vivas) && nrow(vivas)) {
    vivas <- vivas[!is.na(vivas$provision_id), , drop = FALSE]
    vivas <- vivas[order(vivas$confirmado_at, decreasing = TRUE), , drop = FALSE]
    ic <- match(pid, vivas$provision_id)
    usa <- con & !is.na(ic) & !is.na(vivas$importe[ic])
    monto[usa] <- vivas$importe[ic][usa]; moneda[usa] <- vivas$moneda[ic][usa]; fuente[usa] <- "pago"
  }
  reqs$monto_mostrado <- monto; reqs$moneda_mostrada <- moneda; reqs$monto_fuente <- fuente

  f_prov <- dplyr::coalesce(provs$fecha_efectiva_override[ip], provs$fecha_efectiva[ip], provs$fecha_calculada[ip])
  f_prov[!(etapa %in% c("provisionada", "en_pago", "pagada"))] <- as.Date(NA)
  reqs$fecha_provision <- f_prov
  creado_dia <- as.Date(format(reqs$creado, tz = .REQ_TZ))
  reqs$dias_sin_provision <- ifelse(etapa == "sin_provision", as.integer(hoy - creado_dia), NA_integer_)

  ca <- requisiciones_carpetas_arbol(carpetas); cv <- ca[!ca$en_papelera, , drop = FALSE]
  icar <- match(reqs$carpeta_id, cv$id)
  reqs$carpeta_nombre <- cv$nombre[icar]
  reqs$carpeta_ruta   <- cv$ruta[icar]
  reqs$carpeta_color  <- cv$color[icar]
  reqs$empresa_id <- NA_character_; reqs$empresa_aviso <- NA_character_
  if (!is.null(empresas) && !.req_catalogo_vacio(empresas) && n) {
    reqs$empresa_id <- empresa_id_de(reqs$empresa, empresas)
    cabe <- .req_empresa_cabe(reqs$empresa_id, ifelse(is.na(icar), NA_character_, reqs$carpeta_id), ca, empresas)
    reqs$empresa_aviso <- ifelse(is.na(reqs$empresa_id), "no_reconocida", ifelse(cabe, NA_character_, "fuera_carpeta"))
  }

  ev <- evid[is.na(evid$papelera_en), , drop = FALSE]
  ev <- ev[order(ev$creado), , drop = FALSE]
  cuenta <- function(filtro) as.integer(vapply(reqs$id, function(k) sum(ev$requisicion_id == k & filtro), numeric(1)))
  reqs$n_adjuntos <- cuenta(ev$tipo %in% "adjunto")
  reqs$n_imagenes <- cuenta(ev$tipo %in% "adjunto" & ev$es_imagen %in% TRUE)
  reqs$n_tablas   <- cuenta(ev$tipo %in% "tabla")
  img <- ev[ev$tipo %in% "adjunto" & ev$es_imagen %in% TRUE, , drop = FALSE]
  reqs$primera_imagen_id <- img$id[match(reqs$id, img$requisicion_id)]

  reqs$n_comentarios <- as.integer(vapply(reqs$id, function(k) sum(coms$requisicion_id == k), numeric(1)))
  reqs$comentarios_sin_leer <- 0L
  if (!is.null(usuario) && nzchar(usuario %||% "")) {
    u <- tolower(usuario)
    lh <- if (is.null(leidos)) .req_t0() else leidos$leido_hasta[match(paste0(u, "|", reqs$id), leidos$id)]
    if (!length(lh)) lh <- rep(as.POSIXct(NA, tz = "UTC"), n)
    reqs$comentarios_sin_leer <- as.integer(vapply(seq_len(n), function(i) {
      k <- coms$requisicion_id == reqs$id[i] & tolower(coms$autor) != u
      if (!is.na(lh[i])) k <- k & coms$creado > lh[i]
      sum(k)
    }, numeric(1)))
  }
  reqs
}

# Everything the list needs, from strict reads (a table that cannot be read
# stop()s; the screen says so instead of showing an empty list). Paid
# requisitions are left out unless incluir_pagadas.
requisiciones_vista <- function(client_id, usuario = NULL, incluir_pagadas = FALSE) {
  cid <- .req_cid(client_id)
  v <- requisiciones_vista_en(
    reqs     = .req_leer("requisiciones", cid),
    provs    = leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, cid, .schema_pasivos_provision),
    conf     = leer_tabla_para_escribir(S3_KEYS$bancos_confirmados, cid, .schema_bancos_confirmados),
    manual   = leer_tabla_para_escribir(S3_KEYS$manual, cid, .schema_manual),
    carpetas = .req_leer("carpetas", cid),
    evid     = .req_leer("evidencia", cid),
    coms     = .req_leer("comentarios", cid),
    leidos   = if (is.null(usuario)) NULL else .req_leer("leidos", cid),
    liabs    = .req_liabs(cid),
    usuario  = usuario,
    empresas = .req_empresas(cid))
  if (!incluir_pagadas) v <- v[v$etapa != "pagada", , drop = FALSE]
  v
}

# For Calendario, Pasivos and the confirmation history: the requisition(s)
# behind these provisions, paid and trashed ones included (read-only there).
# Vectorized; zero rows when none.
requisicion_de_provision <- function(client_id, provision_id) {
  provision_id <- as.character(provision_id)
  v <- requisiciones_vista(client_id, incluir_pagadas = TRUE)
  v[!is.na(v$provision_id) & v$provision_id %in% provision_id, , drop = FALSE]
}

# For "Agregar evidencia a una provisión": provisions with no requisition
# yet, alive (provisionada, en_pago, or pagada -- a paid one may need its
# evidence too), with the firmest date and amount the provision itself has.
# Strict reads.
provisiones_para_evidencia <- function(client_id) {
  cid <- .req_cid(client_id)
  provs <- leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, cid, .schema_pasivos_provision)
  conf  <- leer_tabla_para_escribir(S3_KEYS$bancos_confirmados, cid, .schema_bancos_confirmados)
  reqs  <- .req_leer("requisiciones", cid)
  provs <- provs[!is.na(provs$id) & !(provs$id %in% reqs$provision_id), , drop = FALSE]
  provs$etapa <- etapa_de_provision_en(provs, conf, provs$id, liabs = .req_liabs(cid))
  provs <- provs[provs$etapa %in% c("provisionada", "en_pago", "pagada"), , drop = FALSE]
  tibble::tibble(
    id         = provs$id,
    fecha      = dplyr::coalesce(provs$fecha_efectiva_override, provs$fecha_efectiva, provs$fecha_calculada),
    empresa    = provs$empresa,
    parte      = provs$parte,
    referencia = dplyr::coalesce(provs$referencia, provs$documento, provs$notas),
    monto      = dplyr::coalesce(provs$amount_pago_override, provs$amount_pago),
    moneda     = provs$moneda_pago,
    etapa      = provs$etapa)
}

# One requisition per provision chosen, as if created from the tab: title
# from the provision's reference (or its counterpart), linked from birth.
# Returns the ids created; stops at the first failure (those before it stay).
requisiciones_desde_provisiones <- function(client_id, provision_ids, carpeta_id, por) {
  cid <- .req_cid(client_id)
  disp <- provisiones_para_evidencia(cid)
  disp <- disp[disp$id %in% provision_ids, , drop = FALSE]
  creadas <- character()
  for (i in seq_len(nrow(disp))) {
    p <- disp[i, ]
    # T61: a provision with no company cannot say whose it is.
    if (is.na(.req_txt(p$empresa))) .req_error("requisiciones.error.empresa_no_reconocida")
    titulo <- .req_txt(p$referencia) %||% NA_character_
    if (is.na(titulo)) titulo <- .req_txt(p$parte) %||% NA_character_
    if (is.na(titulo)) titulo <- p$id
    r <- requisicion_crear(cid, list(
      titulo = substr(titulo, 1L, REQ_MAX_TITULO), empresa = p$empresa, proveedor = p$parte,
      monto = p$monto, moneda = p$moneda, carpeta_id = carpeta_id, provision_id = p$id), por)
    creadas <- c(creadas, r$id)
  }
  creadas
}

# Counts for the paper clip: one row per requisition given (rows of
# requisiciones_vista() / requisicion_de_provision()).
adjuntos_resumen <- function(requisicion) {
  if (is.null(requisicion) || !nrow(requisicion))
    return(tibble::tibble(id = character(), adjuntos = integer(), imagenes = integer(),
                          tablas = integer(), total = integer(), primera_imagen_id = character()))
  tibble::tibble(
    id = requisicion$id,
    adjuntos = as.integer(requisicion$n_adjuntos %||% 0L),
    imagenes = as.integer(requisicion$n_imagenes %||% 0L),
    tablas = as.integer(requisicion$n_tablas %||% 0L),
    total = as.integer((requisicion$n_adjuntos %||% 0L) + (requisicion$n_tablas %||% 0L)),
    primera_imagen_id = as.character(requisicion$primera_imagen_id %||% NA_character_))
}

# ── Liga con Pasivos y Calendario (E3) ───────────────────────────────────────

# What the provision form starts with when "Crear provisión" is pressed on a
# requisition (a row of requisiciones_vista()). Only the requisition's own
# data; the form decides what it keeps and the person can change all of it.
# NA where the requisition has nothing.
requisicion_prefill_provision <- function(fila) {
  txt <- function(x) if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(trimws(x[1]))) NA_character_ else trimws(x[1])
  num <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) NA_real_ else as.numeric(x[1])
  folio <- txt(fila$folio); titulo <- txt(fila$titulo)
  list(
    requisicion_id = txt(fila$id),
    folio          = folio,
    empresa        = txt(fila$empresa),
    parte          = txt(fila$proveedor),
    monto          = num(fila$monto),
    moneda         = txt(fila$moneda),
    referencia     = substr(paste(stats::na.omit(c(folio, titulo)), collapse = " · "), 1L, REQ_MAX_TITULO),
    notas          = txt(fila$notas))
}

# Who may open a requisition's evidence read-only from Calendario, Pasivos or
# the confirmation history: only whoever sees Requisiciones (Luis,
# 2026-11-10). Without it the clip is not even shown.
requisiciones_puede_ver_evidencia <- function(usuario) puede(usuario, "can_view_requisiciones")

# The clip for other tabs' rows (Calendario, Pasivos, confirmation history):
# per provision with a live (not trashed) requisition, how many pieces of
# evidence it has. Cheap: two strict reads. A provision without a
# requisition has no row.
requisiciones_clip_por_provision <- function(client_id, provision_ids) {
  cid <- .req_cid(client_id)
  vacia <- tibble::tibble(provision_id = character(), requisicion_id = character(), total = integer())
  pids <- unique(stats::na.omit(as.character(provision_ids)))
  if (!length(pids)) return(vacia)
  reqs <- .req_leer("requisiciones", cid)
  reqs <- reqs[!is.na(reqs$provision_id) & reqs$provision_id %in% pids & is.na(reqs$papelera_en), , drop = FALSE]
  reqs <- reqs[!duplicated(reqs$provision_id), , drop = FALSE]
  if (!nrow(reqs)) return(vacia)
  ev <- .req_leer("evidencia", cid)
  ev <- ev[is.na(ev$papelera_en) & ev$requisicion_id %in% reqs$id, , drop = FALSE]
  tibble::tibble(provision_id = reqs$provision_id, requisicion_id = reqs$id,
                 total = as.integer(vapply(reqs$id, function(k) sum(ev$requisicion_id == k), numeric(1))))
}

# What shared$req_clip() answers: only for whoever sees Requisiciones (Luis,
# 2026-11-10), and never an error -- no client, no permission or a failed
# read is "no clip" (the other tab must keep working).
requisiciones_clip <- function(client_id, usuario, provision_ids) {
  vacia <- tibble::tibble(provision_id = character(), requisicion_id = character(), total = integer())
  if (!requisiciones_puede_ver_evidencia(usuario)) return(vacia)
  if (is.null(client_id) || !length(client_id) || is.na(client_id[1]) || !nzchar(client_id[1])) return(vacia)
  tryCatch(requisiciones_clip_por_provision(client_id[1], provision_ids), error = function(e) {
    message("[requisiciones] clip: ", conditionMessage(e))
    vacia
  })
}

# Translated stage name, for badges and chips.
requisiciones_etapa_nombre <- function(etapa)
  vapply(etapa, function(e) tr(paste0("requisiciones.etapa.", e)), character(1), USE.NAMES = FALSE)
