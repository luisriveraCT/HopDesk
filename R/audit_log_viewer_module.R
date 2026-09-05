# =============================================================================
# R/audit_log_viewer_module.R
# Stage 4 Part B — one shared filter/viewer component, reused by both the
# Actividad tab (mode = "own_client") and the Bitácora Global tab
# (mode = "multi_client"). Mouse's explicit instruction: "I want the filters
# to be the same for everyone who gets access to the logs."
#
# This module is the only caller of read_audit_log_scoped() (R/app_audit.R) —
# it never calls read_audit_log() directly (Stage 4 Part C railguard).
#
# auditLogViewerServer(id, shared, mode, allowed_client_ids = NULL,
#                       include_staff_log = FALSE)
#   mode = "own_client"    — no client selector; renders effective_client_id()'s
#                            own log. Used by Actividad.
#   mode = "multi_client"  — renders a client selector built from
#                            allowed_client_ids (already resolved by the
#                            caller), plus a "Hopdesk (interno)" option when
#                            include_staff_log is TRUE. Used by Bitácora Global.
#
# allowed_client_ids / include_staff_log may each be passed as a plain value
# OR as a zero-arg reactive/function — the caller's viewer-permission
# resolution depends on shared$current_user_info(), which isn't reliably
# resolved at the single synchronous instant a module is wired up, so both
# params are resolved lazily, at render/fetch time, via .resolve_param().
# =============================================================================

# =============================================================================
# Stage 2 of the Actividad audit overhaul (2026-08-26): a click on any row's
# Descripción cell opens a detail modal. Payment confirmations and policy
# applies get bespoke, information-dense rendering (Mouse's explicit
# priority: "if it was a payment I need full information about it"); login/
# logout get their own small bespoke views; every other module/action falls
# through to a generic key/value dump of its metadata JSON, so every log type
# is covered without needing one bespoke renderer per action. Pure data/
# classification helpers are kept separate from the tag-building ones so the
# former are unit-testable without a running Shiny session (see
# tests/test_activity_stage2_detail_modal.R).
# =============================================================================

# Classifies one audit row into which bespoke (or generic) detail view its
# popup should use. Pure function -- no UI, no reactive context.
.audit_detail_kind <- function(module, action) {
  # %||% only substitutes on NULL, not NA -- an explicit NA module/action
  # must be normalized to "" too, or module == "auth" && action == "login"
  # below evaluates to NA && NA (neither side is FALSE, so && can't
  # short-circuit) and `if (NA)` throws "missing value where TRUE/FALSE
  # needed" instead of falling through to the generic classification.
  module <- as.character(module %||% "")
  action <- as.character(action %||% "")
  if (is.na(module)) module <- ""
  if (is.na(action)) action <- ""
  if (module == "pagar_hoy" && action %in% c("confirmar_pago", "confirmar_pago_incompleto"))
    return("payment_pago")
  if (module == "pagar_hoy" && action %in% c("confirmar_cobro", "confirmar_cobro_incompleto"))
    return("payment_cobro")
  if (module == "settings_companies" && action %in% c("aplicar_politicas", "auto_aplicar_politicas"))
    return("policy")
  if (module == "pagar_hoy" && action == "generar_archivo_pago") return("payment_file")
  if (module == "pagar_hoy" && action == "generar_archivo_pago_zip") return("payment_file_zip")
  if (module == "cashflow_export" &&
      action %in% c("exportar_cashflow_pdf", "exportar_cashflow_word", "exportar_cashflow_excel"))
    return("cashflow_export")
  if (module == "auth" && action == "login")  return("login")
  if (module == "auth" && action == "logout") return("logout")
  # Bancos activity-logging enrichment, Stage 2 (2026-09-03):
  # the 4 bulk Bancos actions (import, bulk-delete, session reassign,
  # session-delete) carry a row-level movimientos summary
  # (.bancos_audit_rows_summary(), R/bancos_persistence.R) that the generic
  # key/value dump would render as an unreadable unlist()'d blob -- these get
  # a real table instead. The single-row undo/delete-confirmation actions
  # deliberately do NOT get a bespoke kind here: their metadata is a handful
  # of scalar fields, which the generic view already renders cleanly.
  if (module == "bancos" &&
      action %in% c("eliminar_movimientos", "reasignar_sesion",
                    "eliminar_sesion", "importar_movimientos"))
    return("bancos_movimientos")
  if (module == "bancos" && action %in% c("editar_movimiento_manual", "agregar_movimiento_manual"))
    return("bancos_manual_edit")
  # Stage 3 of the Bancos/Pasivos activity-logging extension (2026-09-03):
  # pasivos_log_audit() (R/pasivos_audit.R) already dual-writes every one of
  # its ~20 action types into this same app_audit log, with a consistent
  # metadata shape (target_kind, empresa, before, after) regardless of
  # action type -- so ONE generic before/after diff view (unlike Bancos'
  # per-action bespoke tables) covers all of them, rather than the previous
  # unlabeled unlist()-flattened blob the plain generic view produced for a
  # nested list.
  if (module == "pasivos") return("pasivos_diff")
  # Reporte activity logging (2026-09-04): "aplicar_filtros" carries the same
  # before/after shape as pasivos entries (arbitrary field set, no fixed
  # label map) -- reuses the same generic diff renderer rather than a
  # module-specific copy. Exports get their own small bespoke view since
  # they're a distinct kind of event (a document actually left the app),
  # not a settings change.
  if (module == "reporte" && action == "aplicar_filtros") return("reporte_filtros")
  if (module == "reporte" && action %in% c("exportar_html", "exportar_pdf")) return("reporte_export")
  # tier_config's before/after is a list of ~15-20 boolean permission flags
  # per tier -- same generic diff shape, same reuse (Grupo activity-logging
  # audit, 2026-09-04).
  if (module == "tiers" && action == "guardar_tier_config") return("tiers_config_diff")
  "generic"
}

# Parses one row's metadata JSON string back into a plain list. Never
# errors -- an unparseable/empty/NA value returns an empty list, same
# never-throw convention as pagos_generados_persistence.R's parse_pago_*().
.audit_parse_metadata <- function(metadata_json) {
  metadata_json <- metadata_json[1]
  if (is.null(metadata_json) || is.na(metadata_json) || !nzchar(metadata_json)) return(list())
  parsed <- tryCatch(jsonlite::fromJSON(metadata_json, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(parsed)) return(list())
  if (is.list(parsed)) parsed else as.list(parsed)
}

# Normalizes a payment-detail metadata field (the "pagos"/"cobros" array
# written by .pago_audit_detalle(), R/pagar_hoy_module.R) to a data frame
# with a fixed column set, regardless of whether it arrived as NULL, a
# single row, or many -- same defensive shape as
# pagos_generados_persistence.R's parse_pago_beneficiarios().
.audit_normalize_pagos <- function(x) {
  empty <- data.frame(parte = character(), documento = character(), importe = double(),
                      moneda = character(), cuenta_id = character(), fecha = character(),
                      stringsAsFactors = FALSE)
  if (is.null(x) || (is.list(x) && !length(x))) return(empty)
  df <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(empty)
  for (col in names(empty)) if (!col %in% names(df)) df[[col]] <- empty[[col]][NA_integer_]
  df[, names(empty), drop = FALSE]
}

# ── Per-kind detail bodies (tagList builders) ────────────────────────────────
.audit_detail_payment <- function(meta, key, label) {
  det    <- .audit_normalize_pagos(meta[[key]])
  fecha  <- meta$fecha_pago %||% meta$fecha_cobro %||% NA_character_
  header <- div(class = "mb-2 small",
    tags$div(tags$strong("Empresa: "),  meta$empresa   %||% ""),
    tags$div(tags$strong("Moneda: "),   meta$moneda    %||% ""),
    tags$div(tags$strong("Cuenta: "),   meta$cuenta_id %||% ""),
    tags$div(tags$strong("Fecha: "),    if (!is.na(fecha)) fecha else "")
  )
  if (!nrow(det)) {
    tagList(header, tags$p(class = "text-muted small",
                           sprintf("Sin detalle de %s registrado.", tolower(label))))
  } else {
    tagList(header,
      tags$table(class = "table table-sm table-hover small",
        tags$thead(class = "table-light",
          tags$tr(tags$th("Socio"), tags$th("Documento"),
                  tags$th(class = "text-end", "Importe"), tags$th("Moneda"))),
        tags$tbody(
          lapply(seq_len(nrow(det)), function(i)
            tags$tr(
              tags$td(det$parte[i]),
              tags$td(det$documento[i]),
              tags$td(class = "text-end",
                      formatC(det$importe[i], format = "f", digits = 2, big.mark = ",")),
              tags$td(det$moneda[i])
            ))
        )
      )
    )
  }
}

.audit_detail_policy <- function(meta) {
  pol_names <- meta$policy_names %||% character(0)
  partes    <- meta$partes       %||% character(0)
  tagList(
    tags$div(tags$strong("Política(s) aplicada(s): "),
             if (length(pol_names)) paste(pol_names, collapse = ", ") else "—"),
    tags$div(tags$strong("Documentos calculados: "), as.character(meta$n_moves %||% "")),
    tags$div(tags$strong("Socios afectados: "),
             if (length(partes)) paste(partes, collapse = ", ") else "—")
  )
}

# Follow-up (2026-08-27): Calendario's cash flow export -- PDF, Word, and
# Excel all now log here (module="cashflow_export"; PDF already did, Word/
# Excel were dual-writing only to "Ver archivos de pago", never to this
# general log). Basic info only, matching .audit_detail_policy()'s level of
# detail above -- there's no beneficiary/bank-level data in a cash flow
# report the way there is for a payment file, so a full receipt-style view
# isn't warranted here.
.CF_IC_LABELS <- c(exclude = "Sin transacciones entre compañías",
                   include = "Con transacciones entre compañías",
                   only    = "Sólo transacciones entre compañías")
.CF_FORMATO_LABELS <- c(pdf = "PDF", word = "Word", excel = "Excel")

.audit_detail_cashflow_export <- function(meta) {
  empresas <- meta$empresas %||% character(0)
  ic_lbl   <- unname(.CF_IC_LABELS[as.character(meta$ic_mode %||% "")])
  fmt_lbl  <- unname(.CF_FORMATO_LABELS[as.character(meta$formato %||% "")])
  tagList(
    tags$div(tags$strong("Rango: "),
             sprintf("%s — %s", meta$date_from %||% "—", meta$date_to %||% "—")),
    tags$div(tags$strong("Empresas: "),
             if (length(empresas)) paste(empresas, collapse = ", ") else "—"),
    tags$div(tags$strong("Intercompañía: "), if (is.na(ic_lbl)) (meta$ic_mode %||% "—") else ic_lbl),
    tags$div(tags$strong("Modo de moneda: "), meta$currency_mode %||% "—"),
    tags$div(tags$strong("Formato: "), if (is.na(fmt_lbl)) (meta$formato %||% "—") else fmt_lbl)
  )
}

# Normalizes the "beneficiarios" metadata field written by
# .log_pago_generado() (R/pagar_hoy_module.R) -- same defensive shape as
# .audit_normalize_pagos(), different column set (this is a bank-transfer
# beneficiary, not an invoice).
.audit_normalize_beneficiarios <- function(x) {
  empty <- data.frame(alias = character(), clabe = character(), banco = character(),
                      importe = double(), concepto = character(), stringsAsFactors = FALSE)
  if (is.null(x) || (is.list(x) && !length(x))) return(empty)
  df <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(empty)
  for (col in names(empty)) if (!col %in% names(df)) df[[col]] <- empty[[col]][NA_integer_]
  df[, names(empty), drop = FALSE]
}

# Stage 4 of the Actividad audit overhaul (2026-08-26): a bank payment file
# (PPL) generation -- real CLABEs, real transfer instructions -- gets its own
# bespoke view, matching what R/pagos_audit_viewer_module.R already shows
# (and reusing its own mask_clabe() for consistency): full beneficiary
# breakdown, CLABE masked to its last 4 digits, plus origin account/bank.
# Shared beneficiary table -- one payment file's worth of rows, CLABE always
# masked. Used both by the single-file view and, per bundled file, by the
# ZIP/bulk aggregate view below.
.audit_beneficiarios_table <- function(det) {
  if (!nrow(det)) return(tags$p(class = "text-muted small", "Sin beneficiarios registrados."))
  tags$table(class = "table table-sm table-hover small",
    tags$thead(class = "table-light",
      tags$tr(tags$th("Beneficiario"), tags$th("CLABE"), tags$th("Banco"),
              tags$th(class = "text-end", "Importe"), tags$th("Concepto"))),
    tags$tbody(
      lapply(seq_len(nrow(det)), function(i)
        tags$tr(
          tags$td(det$alias[i]),
          tags$td(mask_clabe(det$clabe[i])),
          tags$td(det$banco[i]),
          tags$td(class = "text-end",
                  formatC(det$importe[i], format = "f", digits = 2, big.mark = ",")),
          tags$td(det$concepto[i])
        ))
    )
  )
}

.audit_detail_payment_file <- function(meta) {
  det <- .audit_normalize_beneficiarios(meta$beneficiarios)
  source_label <- switch(as.character(meta$source %||% ""),
                         individual = "Individual", zip_bulk = "ZIP (todos)",
                         as.character(meta$source %||% "—"))
  header <- div(class = "mb-2 small",
    tags$div(tags$strong("Empresa: "), meta$empresa %||% ""),
    tags$div(tags$strong("Moneda: "), meta$moneda %||% ""),
    tags$div(tags$strong("Cuenta origen: "), meta$cuenta_origen %||% ""),
    tags$div(tags$strong("Banco origen: "), meta$banco_origen %||% "—"),
    tags$div(tags$strong("Total: "),
             formatC(as.numeric(meta$total %||% 0), format = "f", digits = 2, big.mark = ","),
             sprintf(" (%s pago(s))", as.character(meta$n_pagos %||% nrow(det)))),
    tags$div(tags$strong("Origen: "), source_label),
    tags$div(tags$strong("Archivo: "), meta$archivo %||% "")
  )
  if (!nrow(det)) {
    tagList(header, .audit_beneficiarios_table(det))
  } else {
    tagList(header,
      tags$p(class = "small text-muted mb-1", icon("circle-info"),
             " Las cuentas se muestran enmascaradas; el recibo completo (con CLABE sin enmascarar) ",
             "está disponible en \"Ver archivos de pago\"."),
      .audit_beneficiarios_table(det)
    )
  }
}

# Stage 4 follow-up (2026-08-27): the "Descargar todos" bulk/ZIP action's
# aggregate entry -- one summary header for the whole click, then every
# bundled file's own full beneficiary detail (same information density as a
# single-company download, per Mouse's explicit ask), not just a file count.
.audit_detail_payment_file_zip <- function(meta) {
  archivos <- meta$archivos
  archivos_list <- if (is.data.frame(archivos))
    lapply(seq_len(nrow(archivos)), function(i) as.list(archivos[i, , drop = FALSE]))
  else if (is.list(archivos)) archivos
  else list()

  header <- div(class = "mb-3 small",
    tags$div(tags$strong("Archivos generados: "), as.character(meta$n_archivos %||% length(archivos_list))),
    tags$div(tags$strong("Empresas: "),
             paste(meta$empresas %||% character(0), collapse = ", ")),
    tags$div(tags$strong("Pagos totales: "), as.character(meta$n_pagos_total %||% "")),
    tags$div(tags$strong("Total general: "),
             formatC(as.numeric(meta$total_general %||% 0), format = "f", digits = 2, big.mark = ","))
  )

  if (!length(archivos_list))
    return(tagList(header, tags$p(class = "text-muted small", "Sin archivos registrados.")))

  blocks <- lapply(seq_along(archivos_list), function(i) {
    a   <- archivos_list[[i]]
    det <- .audit_normalize_beneficiarios(a$beneficiarios)
    tagList(
      tags$hr(),
      tags$div(class = "small fw-semibold mb-1",
               sprintf("Archivo %d de %d — %s (%s)", i, length(archivos_list),
                       a$empresa %||% "", a$moneda %||% "")),
      div(class = "mb-1 small",
        tags$div(tags$strong("Cuenta origen: "), a$cuenta_origen %||% "",
                 " · ", tags$strong("Banco: "), a$banco_origen %||% "—"),
        tags$div(tags$strong("Total: "),
                 formatC(as.numeric(a$total %||% 0), format = "f", digits = 2, big.mark = ","),
                 sprintf(" (%s pago(s))", as.character(a$n_pagos %||% nrow(det)))),
        tags$div(tags$strong("Archivo: "), a$archivo %||% "")
      ),
      .audit_beneficiarios_table(det)
    )
  })

  tagList(
    header,
    tags$p(class = "small text-muted mb-1", icon("circle-info"),
           " Las cuentas se muestran enmascaradas; el recibo completo de cada archivo ",
           "está disponible en \"Ver archivos de pago\"."),
    blocks
  )
}

.audit_detail_login <- function(meta) {
  tagList(
    tags$div(tags$strong("Usuario: "), meta$user %||% ""),
    tags$div(tags$strong("Nivel (tier): "), meta$tier %||% ""),
    tags$div(tags$strong("Personal Hopdesk: "), if (isTRUE(meta$is_staff)) "Sí" else "No")
  )
}

.audit_detail_logout <- function(meta) {
  dur_val <- suppressWarnings(as.numeric(meta$duration_seconds %||% NA_real_))
  tagList(
    tags$div(tags$strong("Usuario: "), meta$user %||% ""),
    tags$div(tags$strong("Duración activa: "),
             if (!is.na(dur_val)) format_session_duration(dur_val) else "Desconocida"),
    tags$div(tags$strong("Inicio de sesión: "), meta$login_ts  %||% "Desconocido"),
    tags$div(tags$strong("Cierre de sesión: "), meta$logout_ts %||% "")
  )
}

# Normalizes the "rows" metadata field written by
# .bancos_audit_rows_summary() (R/bancos_persistence.R) -- same defensive
# shape as .audit_normalize_pagos()/.audit_normalize_beneficiarios().
.audit_normalize_bancos_rows <- function(x) {
  empty <- data.frame(fecha = character(), parte = character(), concepto = character(),
                      tipo = character(), cargo = double(), abono = double(),
                      stringsAsFactors = FALSE)
  if (is.null(x) || (is.list(x) && !length(x))) return(empty)
  # x arrives as a proper data.frame when it round-tripped through
  # jsonlite::fromJSON(simplifyVector = TRUE) (the real popup path -- a JSON
  # array of objects auto-simplifies to a data.frame there), but as a plain
  # list-of-lists when a caller (e.g. a test) hands it over directly without
  # that round trip. as.data.frame() on a list-of-lists treats each element
  # as a COLUMN, not a row -- dplyr::bind_rows() handles both shapes alike.
  df <- tryCatch(dplyr::bind_rows(x), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(empty)
  for (col in names(empty)) if (!col %in% names(df)) df[[col]] <- empty[[col]][NA_integer_]
  df[, names(empty), drop = FALSE]
}

.audit_detail_bancos_movimientos <- function(meta) {
  det     <- .audit_normalize_bancos_rows(meta$rows)
  n_total <- suppressWarnings(as.integer(meta$n_total %||% nrow(det)))
  header  <- if (isTRUE(meta$truncated))
    tags$p(class = "small text-muted mb-1", icon("circle-info"),
           sprintf(" Mostrando los primeros %d de %d movimiento(s).", nrow(det), n_total))
  else NULL
  if (!nrow(det)) {
    tagList(header, tags$p(class = "text-muted small", "Sin detalle de movimientos registrado."))
  } else {
    tagList(header,
      tags$table(class = "table table-sm table-hover small",
        tags$thead(class = "table-light",
          tags$tr(tags$th("Fecha"), tags$th("Parte"), tags$th("Concepto"), tags$th("Tipo"),
                  tags$th(class = "text-end", "Cargo"), tags$th(class = "text-end", "Abono"))),
        tags$tbody(
          lapply(seq_len(nrow(det)), function(i)
            tags$tr(
              tags$td(det$fecha[i]),
              tags$td(det$parte[i]),
              tags$td(det$concepto[i]),
              tags$td(det$tipo[i]),
              tags$td(class = "text-end",
                      if (isTRUE(det$cargo[i] > 0)) formatC(det$cargo[i], format = "f", digits = 2, big.mark = ",") else ""),
              tags$td(class = "text-end",
                      if (isTRUE(det$abono[i] > 0)) formatC(det$abono[i], format = "f", digits = 2, big.mark = ",") else "")
            ))
        )
      )
    )
  }
}

# editar_movimiento_manual carries before/after (a genuine edit diff);
# agregar_movimiento_manual has no "before" (there's nothing to diff against)
# -- shown as a flat summary of the new row's fields instead.
.audit_detail_bancos_manual_edit <- function(meta) {
  after <- meta$after
  if (is.null(after)) {
    return(tagList(
      tags$div(tags$strong("Cuenta: "), meta$cuenta_id %||% ""),
      tags$div(tags$strong("Tipo: "), meta$tipo %||% ""),
      tags$div(tags$strong("Cargo: "), formatC(as.numeric(meta$cargo %||% 0), format = "f", digits = 2, big.mark = ",")),
      tags$div(tags$strong("Abono: "), formatC(as.numeric(meta$abono %||% 0), format = "f", digits = 2, big.mark = ","))
    ))
  }
  before <- meta$before %||% list()
  fields <- c(cuenta_id = "Cuenta", fecha = "Fecha", tipo = "Tipo", parte = "Parte",
             concepto = "Concepto", cargo = "Cargo", abono = "Abono", notas = "Notas")
  rows <- Filter(Negate(is.null), lapply(names(fields), function(f) {
    b <- before[[f]]; a <- after[[f]]
    b_s <- if (is.null(b) || is.na(b)) "" else as.character(b)
    a_s <- if (is.null(a) || is.na(a)) "" else as.character(a)
    if (identical(b_s, a_s)) return(NULL)
    tags$tr(tags$td(fields[[f]]), tags$td(b_s), tags$td(a_s))
  }))
  if (!length(rows))
    return(tags$p(class = "text-muted small", "Sin cambios de valor detectados."))
  tags$table(class = "table table-sm table-hover small",
    tags$thead(class = "table-light", tags$tr(tags$th("Campo"), tags$th("Antes"), tags$th("Después"))),
    tags$tbody(rows)
  )
}

# Every pasivos_log_audit() call passes the same metadata shape
# (target_kind, empresa, before, after) regardless of its ~20 action types,
# whose before/after field sets are themselves wildly heterogeneous
# (a liability's ~40 columns vs. a provision's ~20 vs. an estimate's 5) --
# so this renders a generic, field-agnostic before/after diff (only
# CHANGED fields, like .audit_detail_bancos_manual_edit() but without a
# fixed field-label map), rather than one bespoke view per action type.
# Reused as-is (not a copy) by the "reporte" module's aplicar_filtros action
# (Stage: Reporte/Grupo activity logging, 2026-09-04) -- same shape
# (before/after, arbitrary field set), same need, no reason for a second
# implementation. Named generically for that reason, not after either
# module.
.audit_generic_fmt_val <- function(v) {
  if (is.null(v)) return("")
  if (is.list(v)) return(paste(unlist(v), collapse = ", "))
  if (!length(v)) return("")
  if (length(v) == 1 && is.na(v)) return("")
  paste(as.character(v), collapse = ", ")
}

.audit_detail_generic_diff <- function(meta) {
  target_kind <- as.character(meta$target_kind %||% "")
  empresa     <- as.character(meta$empresa %||% "")
  header <- div(class = "mb-2 small",
    if (nzchar(target_kind)) tags$div(tags$strong("Tipo: "), target_kind) else NULL,
    if (nzchar(empresa) && !identical(empresa, "NA")) tags$div(tags$strong("Empresa: "), empresa) else NULL
  )
  before <- meta$before
  after  <- meta$after

  # "id" duplicates the target row's own id, already shown one line above as
  # "ID destino" (target_id) in the modal's common header -- every
  # liability/provision row's before/after carries it (schema PK), so
  # without this it appeared as a redundant, uninformative diff row on
  # nearly every liability/provision entry (found while probing this
  # renderer against a real liability row, not a hand-simplified test list).
  before <- before[setdiff(names(before), "id")]
  after  <- after[setdiff(names(after), "id")]

  if (is.null(before)) {
    if (!length(after))
      return(tagList(header, tags$p(class = "text-muted small", "Sin detalle adicional registrado.")))
    rows <- lapply(names(after), function(k)
      tags$tr(tags$td(class = "fw-semibold", k), tags$td(.audit_generic_fmt_val(after[[k]]))))
    return(tagList(header, tags$table(class = "table table-sm small", tags$tbody(rows))))
  }

  all_fields <- union(names(before), names(after))
  diff_rows <- Filter(Negate(is.null), lapply(all_fields, function(f) {
    b <- .audit_generic_fmt_val(before[[f]])
    a <- .audit_generic_fmt_val(after[[f]])
    if (identical(b, a)) return(NULL)
    tags$tr(tags$td(f), tags$td(b), tags$td(a))
  }))
  if (!length(diff_rows))
    return(tagList(header, tags$p(class = "text-muted small", "Sin cambios de valor detectados.")))
  tagList(header,
    tags$table(class = "table table-sm table-hover small",
      tags$thead(class = "table-light", tags$tr(tags$th("Campo"), tags$th("Antes"), tags$th("Después"))),
      tags$tbody(diff_rows)
    )
  )
}

# Reporte export (.log_reporte_export(), R/reporte_module.R) -- a financial
# summary report actually left the app as a file, so this gets its own
# small view (format, coverage, totals) rather than the generic diff -- an
# export has no "before", it's a one-shot event, not a state change.
.audit_detail_reporte_export <- function(meta) {
  empresas <- meta$empresas %||% character(0)
  tagList(
    tags$div(tags$strong("Formato: "), toupper(as.character(meta$formato %||% ""))),
    tags$div(tags$strong("Rango: "),
             sprintf("%s — %s", meta$desde %||% "—", meta$hasta %||% "—")),
    tags$div(tags$strong("Empresas: "),
             if (length(empresas)) paste(empresas, collapse = ", ") else "—"),
    tags$div(tags$strong("Cuentas incluidas: "), as.character(meta$n_cuentas %||% "")),
    tags$div(tags$strong("Saldo total: "),
             formatC(as.numeric(meta$total_saldo %||% 0), format = "f", digits = 2, big.mark = ",")),
    if (!is.null(meta$nota) && !is.na(meta$nota) && nzchar(meta$nota %||% ""))
      tags$div(class = "text-muted small mt-1", icon("circle-info"), " ", meta$nota)
    else NULL
  )
}

# Generic fallback -- covers every module/action this popup has no bespoke
# view for, as a plain key/value dump of its metadata. Same shape as
# pagos_audit_viewer_module.R's non-"pago_ppl" detail rendering.
.audit_detail_generic <- function(meta) {
  if (!length(meta)) return(tags$p(class = "text-muted small", "Sin detalle adicional registrado."))
  tags$table(class = "table table-sm small",
    tags$tbody(
      lapply(names(meta), function(k)
        tags$tr(tags$td(class = "fw-semibold", k),
                tags$td(paste(unlist(meta[[k]]), collapse = ", "))))
    )
  )
}

# Assembles the full modal body for one raw audit row (a 1-row slice of
# read_audit_log_scoped()'s output -- NOT the formatted display data.frame).
# mode: "own_client" | "multi_client" -- multi_client also shows which client
# the row belongs to, since Bitácora Global spans several.
.audit_detail_modal_body <- function(row, mode = "own_client") {
  meta <- .audit_parse_metadata(row$metadata[1])
  kind <- .audit_detail_kind(row$module[1], row$action[1])
  target_id <- row$target_id[1]
  common <- div(class = "mb-3 small",
    tags$div(tags$strong("Usuario: "), row$user[1] %||% ""),
    tags$div(tags$strong("Fecha: "), format(as.POSIXct(row$ts[1]), "%d/%m/%Y %H:%M:%S")),
    tags$div(tags$strong("Módulo: "), row$module[1] %||% ""),
    tags$div(tags$strong("Acción: "), row$action[1] %||% ""),
    if (identical(mode, "multi_client") && "client_id" %in% names(row))
      tags$div(tags$strong("Cliente: "), row$client_id[1] %||% ""),
    if (!is.na(target_id) && nzchar(target_id %||% ""))
      tags$div(tags$strong("ID destino: "), target_id)
  )
  tagList(common, .audit_detail_body_by_kind(kind, meta))
}

# The type-specific body alone, without the "common" header (Usuario/Fecha/
# Módulo/Acción/Cliente/ID destino) -- factored out so Stage 3's PDF export
# can reuse the exact same bespoke/generic renderers for each entry's inline
# detail block without repeating fields the PDF's table row already shows.
.audit_detail_body_by_kind <- function(kind, meta) {
  switch(kind,
    payment_pago  = .audit_detail_payment(meta, "pagos",  "Pago(s)"),
    payment_cobro = .audit_detail_payment(meta, "cobros", "Cobro(s)"),
    payment_file  = .audit_detail_payment_file(meta),
    payment_file_zip = .audit_detail_payment_file_zip(meta),
    cashflow_export = .audit_detail_cashflow_export(meta),
    policy        = .audit_detail_policy(meta),
    login         = .audit_detail_login(meta),
    logout        = .audit_detail_logout(meta),
    bancos_movimientos = .audit_detail_bancos_movimientos(meta),
    bancos_manual_edit  = .audit_detail_bancos_manual_edit(meta),
    pasivos_diff  = .audit_detail_generic_diff(meta),
    reporte_filtros = .audit_detail_generic_diff(meta),
    tiers_config_diff = .audit_detail_generic_diff(meta),
    reporte_export  = .audit_detail_reporte_export(meta),
    .audit_detail_generic(meta)
  )
}

# =============================================================================
# Stage 3 of the Actividad audit overhaul (2026-08-26): PDF export of the
# currently filtered log, segmented into one section per Módulo for
# readability, each containing BOTH a compact table (exactly what's on
# screen) AND, per entry, the same bespoke/generic detail body Stage 2's
# popup shows -- an auditor working from a printed/downloaded copy sees
# everything a live click would have shown, with nothing left one click
# away. Modeled on R/cashflow_export_module.R's generate_cashflow_html_doc()
# + pagedown::chrome_print() pattern (the proven "export button" this
# feature was asked to learn from), and on
# R/pagos_generados_persistence.R's "Generado automáticamente por HopDesk a
# partir de un registro permanente e inmutable" authenticity footer for the
# same "this is an official, untampered document" framing here. No image
# logo exists in the repo yet (confirmed while scoping this stage) -- Mouse's
# explicit call: a text wordmark header for now, swap in a real logo image
# later without changing this structure.
#
# generate_audit_log_html_doc() is a PLAIN function (no reactives/session) --
# same design as generate_cashflow_html_doc() -- so it's directly unit-
# testable (tests/test_activity_stage3_pdf_export.R) without pagedown/Chrome
# even being installed; only the downloadHandler wiring below needs those.
# =============================================================================

.AUDIT_PDF_CSS <- "
  @page { size: A4 portrait; margin: 1.5cm 1.4cm; }
  body { font-family: Arial, Calibri, sans-serif; font-size: 9pt; margin: 0; color:#222; }
  .pb { page-break-before: always; }
  .cover { text-align:center; padding: 60px 50px 30px; }
  .cover .wordmark { font-size:30pt; font-weight:bold; letter-spacing:2px; color:#1E3A5F; }
  .cover h1 { font-size:16pt; color:#1E3A5F; font-weight:bold; margin:6px 0 4px; }
  .cover h2 { font-size:11pt; color:#444; font-weight:normal; margin:0 0 20px; }
  .meta-tbl { width:100%; border-collapse:collapse; font-size:9pt; margin:16px auto; max-width:520px; }
  .meta-tbl td { padding:3px 8px; border-bottom:1px solid #E0E0E0; }
  .meta-tbl .k { text-align:right; font-weight:bold; color:#333; width:42%; }
  .meta-tbl .v { text-align:left; }
  .conf { font-size:8pt; font-style:italic; color:#888; margin-top:24px; }
  .sec-hdr { background:#1E3A5F; color:#fff; font-weight:bold; font-size:10pt; padding:5px 10px; margin: 16px 0 6px; }
  table.audit-tbl { width:100%; border-collapse:collapse; font-size:8pt; margin-bottom:10px; table-layout:fixed; }
  table.audit-tbl th, table.audit-tbl td { border:1px solid #ccc; padding:3px 5px; text-align:left;
                                           word-wrap:break-word; overflow-wrap:break-word; }
  table.audit-tbl th { background:#f2f2f2; }
  .entry-card { border:1px solid #ddd; border-radius:3px; padding:6px 8px; margin-bottom:6px; font-size:8.5pt; }
  .entry-card .ehdr { font-weight:bold; color:#1E3A5F; margin-bottom:2px; }
  .entry-card .edesc { margin-bottom:4px; }
  .entry-card table { width:100%; border-collapse:collapse; font-size:8pt; margin-top:2px; }
  .entry-card table th, .entry-card table td { border:1px solid #ddd; padding:2px 4px; }
  .no-data { text-align:center; font-style:italic; color:#888; padding:10px; }
  .footer-note { font-size:7.5pt; color:#888; margin-top:6px; }
"

# Per-módulo entry counts, in alphabetical módulo order -- a stable,
# predictable order for a document meant to be compared run-over-run (unlike
# "most active first", which would reorder sections between two exports of
# overlapping ranges just because volume shifted).
.audit_module_summary <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame(module = character(), n = integer()))
  mods <- ifelse(is.na(df$module) | !nzchar(df$module), "(sin módulo)", df$module)
  tab  <- sort(table(mods))
  data.frame(module = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)[
    order(names(tab)), , drop = FALSE]
}

# One entry's "card": the same info its table row shows (Hora/Usuario/Acción/
# Descripción/ID destino) plus its Stage-2 detail body -- Módulo is omitted
# here since it's already this section's own header.
.audit_pdf_entry_card <- function(row) {
  meta <- .audit_parse_metadata(row$metadata[1])
  kind <- .audit_detail_kind(row$module[1], row$action[1])
  target_id <- row$target_id[1]
  tags$div(class = "entry-card",
    tags$div(class = "ehdr",
             sprintf("%s · %s · %s", format(as.POSIXct(row$ts[1]), "%d/%m/%Y %H:%M:%S"),
                     row$user[1] %||% "", row$action[1] %||% "")),
    tags$div(class = "edesc", row$description[1] %||% ""),
    if (!is.na(target_id) && nzchar(target_id %||% ""))
      tags$div(class = "edesc", tags$strong("ID destino: "), target_id),
    .audit_detail_body_by_kind(kind, meta)
  )
}

# Builds the full printable HTML document for the currently filtered log and
# writes it to a temp file (same return convention as
# generate_cashflow_html_doc()). export_df: the RAW rows as returned by
# read_audit_log_scoped()/filtered() (ts/user/module/action/description/
# target_id/metadata/client_id) -- NOT the display-formatted data.frame.
# meta_info: list(generated_by, date_from, date_to, modulo_filter,
#   accion_filter, usuario_filter, client_label, mode).
generate_audit_log_html_doc <- function(export_df, meta_info = list()) {
  df <- export_df
  if (is.null(df) || !is.data.frame(df)) df <- .APP_AUDIT_SCHEMA()
  df <- df[order(
    ifelse(is.na(df$module), "", df$module),
    df$ts, method = "radix", decreasing = c(FALSE, TRUE)
  ), , drop = FALSE]

  fmt_filter <- function(x) if (is.null(x) || identical(x, "Todos") || !nzchar(x %||% "")) "Todos" else x
  meta_rows <- list(
    c("Rango exportado", sprintf("%s — %s",
      format(as.POSIXct(meta_info$date_from %||% Sys.time()), "%d/%m/%Y %H:%M"),
      format(as.POSIXct(meta_info$date_to   %||% Sys.time()), "%d/%m/%Y %H:%M"))),
    c("Módulo (filtro)",  fmt_filter(meta_info$modulo_filter)),
    c("Acción (filtro)",  fmt_filter(meta_info$accion_filter)),
    c("Usuario (filtro)", if (nzchar(meta_info$usuario_filter %||% "")) meta_info$usuario_filter else "Todos"),
    c("Cliente",          meta_info$client_label %||% "—"),
    c("Generado por",     meta_info$generated_by %||% "—"),
    c("Fecha de generación", format(Sys.time(), "%d/%m/%Y %H:%M:%S")),
    c("Total de entradas", as.character(nrow(df)))
  )
  meta_html <- tags$table(class = "meta-tbl",
    lapply(meta_rows, function(r) tags$tr(tags$td(class = "k", r[1]), tags$td(class = "v", r[2]))))

  summ <- .audit_module_summary(df)
  summary_html <- if (!nrow(summ)) NULL else
    tags$table(class = "audit-tbl", style = "max-width:520px; margin:10px auto;",
      tags$thead(tags$tr(tags$th("Módulo"), tags$th("Entradas"))),
      tags$tbody(lapply(seq_len(nrow(summ)), function(i)
        tags$tr(tags$td(summ$module[i]), tags$td(as.character(summ$n[i]))))))

  cover <- tags$div(class = "cover pb",
    tags$div(class = "wordmark", "HOPDESK"),
    tags$h1("Registro Oficial de Actividad"),
    tags$h2("Documento generado automáticamente por el sistema"),
    meta_html,
    summary_html,
    tags$div(class = "conf",
      "Este documento refleja exactamente lo registrado en la bitácora del sistema al momento de su ",
      "generación, a partir de un registro permanente e inmutable. Cualquier alteración posterior a su ",
      "descarga no queda reflejada aquí.")
  )

  mods <- unique(ifelse(is.na(df$module) | !nzchar(df$module), "(sin módulo)", df$module))
  mods <- sort(mods)
  sections <- lapply(mods, function(m) {
    sub <- df[ifelse(is.na(df$module) | !nzchar(df$module), "(sin módulo)", df$module) == m, , drop = FALSE]
    quick_tbl <- tags$table(class = "audit-tbl",
      tags$colgroup(tags$col(style="width:15%"), tags$col(style="width:12%"),
                    tags$col(style="width:16%"), tags$col(style="width:42%"), tags$col(style="width:15%")),
      tags$thead(tags$tr(tags$th("Hora"), tags$th("Usuario"), tags$th("Acción"),
                         tags$th("Descripción"), tags$th("ID destino"))),
      tags$tbody(lapply(seq_len(nrow(sub)), function(i)
        tags$tr(
          tags$td(format(as.POSIXct(sub$ts[i]), "%d/%m/%Y %H:%M:%S")),
          tags$td(sub$user[i] %||% ""),
          tags$td(sub$action[i] %||% ""),
          tags$td(sub$description[i] %||% ""),
          tags$td(if (!is.na(sub$target_id[i])) sub$target_id[i] else "")
        )))
    )
    cards <- tagList(lapply(seq_len(nrow(sub)), function(i) .audit_pdf_entry_card(sub[i, , drop = FALSE])))
    tagList(
      tags$div(class = "pb"),
      tags$div(class = "sec-hdr", sprintf("%s (%d entrada%s)", m, nrow(sub), if (nrow(sub) == 1) "" else "s")),
      quick_tbl,
      cards
    )
  })

  if (!nrow(df)) sections <- list(tagList(
    tags$div(class = "pb"),
    tags$div(class = "sec-hdr", "Sin entradas"),
    tags$div(class = "no-data", "No hay entradas en el rango/filtros exportados.")
  ))

  html_doc <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("Registro Oficial de Actividad — HopDesk"),
      tags$style(HTML(.AUDIT_PDF_CSS))
    ),
    tags$body(cover, sections)
  )

  tmp_html <- tempfile(fileext = ".html")
  htmltools::save_html(html_doc, tmp_html, background = "white")
  tmp_html
}

.resolve_param <- function(x) if (is.function(x)) x() else x

auditLogViewerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel"))
}

# Resolve the *viewer's own* effective permissions: their tier default plus
# their own permisos override, looked up in their home client's usuarios.rds
# (never the jumped/effective client — a viewer's own permission grants live
# in their home folder regardless of where they're currently looking).
.audit_viewer_resolve_perms <- function(shared) {
  info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
  tier <- info$tier %||% ""
  home_cid <- tryCatch(shared$home_client_id(), error = function(e) NULL)
  home_cid <- home_cid %||% info$client_id %||% tolower(Sys.getenv("CLIENT_ID"))
  username <- tolower(trimws(info$user %||% ""))

  overrides_json <- "{}"
  if (nzchar(username)) {
    u <- tryCatch(auth_load_usuarios(client_id = home_cid), error = function(e) NULL)
    if (!is.null(u) && is.data.frame(u) && nrow(u)) {
      row <- u[tolower(trimws(u$username)) == username, , drop = FALSE]
      if (nrow(row)) overrides_json <- row$permisos[1] %||% "{}"
    }
  }
  auth_resolve_perms(tier, overrides_json)
}

auditLogViewerServer <- function(id, shared, mode,
                                 allowed_client_ids = NULL,
                                 include_staff_log = FALSE) {
  stopifnot(mode %in% c("own_client", "multi_client"))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    viewer_perms <- reactive({ .audit_viewer_resolve_perms(shared) })

    viewer_home_cid <- reactive({
      info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
      cid  <- tryCatch(shared$home_client_id(), error = function(e) NULL)
      cid %||% info$client_id %||% tolower(Sys.getenv("CLIENT_ID"))
    })

    viewer_is_staff <- reactive({
      isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
    })

    # Named vector of selectable clients — multi_client mode only.
    client_choices <- reactive({
      if (!identical(mode, "multi_client")) return(NULL)
      base <- .resolve_param(allowed_client_ids)
      if (is.null(base)) base <- character(0)
      if (isTRUE(.resolve_param(include_staff_log)))
        base <- c("Hopdesk (interno)" = "hd-admin", base)
      base
    })

    # NULL (not an error) while the multi_client selector hasn't rendered yet —
    # fetched() below treats that as "nothing to show", never a permission
    # denial, so a transient render-order gap can't masquerade as one.
    requested_client_id <- reactive({
      if (identical(mode, "own_client")) {
        tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% viewer_home_cid()
      } else {
        input$audit_client_sel
      }
    })

    refresh_tick <- reactiveVal(0L)
    observeEvent(input$audit_refresh, { refresh_tick(refresh_tick() + 1L) }, ignoreInit = TRUE)

    # Live auto-refresh (the 20s poll below) is opt-in, default OFF. It used
    # to run unconditionally, which rebuilt the whole DT widget (and its own
    # client-side search box + page position) every 20 seconds regardless of
    # whether anyone was actively using the filters -- reported 2026-08-25 as
    # "the table seems to refresh or restart constantly" and filters
    # "randomly" resetting. The manual refresh button (audit_refresh, above)
    # already covers "show me what changed"; this toggle is for the rarer
    # case of wanting to watch the log update live while it's open.
    live_refresh_on <- reactiveVal(FALSE)
    observeEvent(input$audit_live_toggle, {
      live_refresh_on(!isTRUE(live_refresh_on()))
      msg <- if (isTRUE(live_refresh_on()))
        "Actualización en vivo activada" else "Actualización en vivo desactivada"
      shinyjs::html(id = "live_toggle_msg", html = msg)
      shinyjs::show("live_toggle_msg")
      shinyjs::delay(1800, shinyjs::hide("live_toggle_msg"))
    }, ignoreInit = TRUE)

    # Rendered as its own tiny uiOutput (not inside output$panel) so toggling
    # it never forces the surrounding toolbar/filters/table to re-render --
    # exactly the kind of unrelated full-rebuild this fix exists to avoid.
    output$live_toggle_btn <- renderUI({
      on <- isTRUE(live_refresh_on())
      actionButton(
        ns("audit_live_toggle"),
        icon(if (on) "toggle-on" else "toggle-off"),
        class = paste("btn btn-sm", if (on) "btn-outline-success" else "btn-outline-secondary"),
        title = if (on)
          "Actualización en vivo: activada (cada 20s) — clic para desactivar"
        else
          "Actualización en vivo: desactivada — usa el botón de actualizar, o clic aquí para activarla"
      )
    })

    # ── Fetch — the only call site for read_audit_log_scoped() ────────────────
    fetched <- reactive({
      refresh_tick()
      # Only poll for cross-session updates while live-refresh is explicitly
      # on (see live_refresh_on above) -- invalidateLater() must be called
      # unconditionally from inside the reactive on the branch that wants it,
      # not skipped via an early return, so it correctly stops re-scheduling
      # itself the instant the toggle is switched off.
      if (isTRUE(live_refresh_on())) invalidateLater(20000)
      tryCatch(shared$refresh_signal(), error = function(e) NULL)  # optional: caller-side "just acted" signal

      cid <- requested_client_id()
      if (is.null(cid) || !nzchar(cid)) return(NULL)

      rng <- input$audit_range
      since <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[1]])) rng[[1]] else Sys.time() - 7 * 86400
      until <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[2]])) rng[[2]] else Sys.time()

      tryCatch(
        read_audit_log_scoped(
          requested_client_id         = cid,
          viewer_is_staff             = viewer_is_staff(),
          viewer_can_view_client_logs = isTRUE(viewer_perms()$can_view_client_audit_logs),
          viewer_can_view_staff_log   = isTRUE(viewer_perms()$can_view_staff_audit_log),
          viewer_home_client_id       = viewer_home_cid(),
          since = since, until = until
        ),
        error = function(e) e
      )
    })

    # Hopdesk's own jump/context-switch bookkeeping rows (logged by app.R's
    # context-switch observer as module="clientes", action in
    # {client_access, external_access}) are dual-written into a client's own
    # folder so hd-admin's audit trail stays complete, but aren't meaningful
    # "activity" from that client's point of view — a native client viewer's
    # own Actividad shows substantive changes only. Staff/principal (and
    # Bitácora Global, always multi_client) still see every row.
    .JUMP_BOOKKEEPING_MODULE  <- "clientes"
    .JUMP_BOOKKEEPING_ACTIONS <- c("client_access", "external_access")

    visible_fetched <- reactive({
      df <- fetched()
      if (inherits(df, "error") || is.null(df) || !nrow(df)) return(df)
      if (identical(mode, "own_client") && !viewer_is_staff()) {
        hide <- !is.na(df$module) & df$module == .JUMP_BOOKKEEPING_MODULE &
                !is.na(df$action) & df$action %in% .JUMP_BOOKKEEPING_ACTIONS
        df <- df[!hide, , drop = FALSE]
      }
      df
    })

    # Módulo / Acción choices derived from whatever the current range returned.
    observeEvent(visible_fetched(), {
      df <- visible_fetched()
      if (inherits(df, "error") || is.null(df)) return()
      mods <- sort(unique(df$module[!is.na(df$module) & nzchar(df$module)]))
      accs <- sort(unique(df$action[!is.na(df$action) & nzchar(df$action)]))
      updateSelectInput(session, "audit_mod_sel",
                        choices = c("Todos", mods),
                        selected = if (isTRUE(input$audit_mod_sel %in% mods)) input$audit_mod_sel else "Todos")
      updateSelectInput(session, "audit_accion_sel",
                        choices = c("Todos", accs),
                        selected = if (isTRUE(input$audit_accion_sel %in% accs)) input$audit_accion_sel else "Todos")
    })

    filtered <- reactive({
      df <- visible_fetched()
      if (inherits(df, "error") || is.null(df) || !nrow(df)) return(df)
      if (!is.null(input$audit_mod_sel) && !identical(input$audit_mod_sel, "Todos"))
        df <- df[!is.na(df$module) & df$module == input$audit_mod_sel, , drop = FALSE]
      if (!is.null(input$audit_accion_sel) && !identical(input$audit_accion_sel, "Todos"))
        df <- df[!is.na(df$action) & df$action == input$audit_accion_sel, , drop = FALSE]
      u_filter <- trimws(input$audit_user_filter %||% "")
      if (nzchar(u_filter))
        df <- df[!is.na(df$user) & grepl(u_filter, df$user, ignore.case = TRUE), , drop = FALSE]
      df
    })

    # ── UI ──────────────────────────────────────────────────────────────────
    output$panel <- renderUI({
      if (identical(mode, "multi_client") && !isTRUE(viewer_perms()$can_view_client_audit_logs)) {
        return(div(class = "alert alert-warning mt-3", icon("lock"),
                   " Requiere permiso ", tags$code("can_view_client_audit_logs"), "."))
      }

      choices <- client_choices()

      tagList(
        tags$style(HTML(paste0(
          ".audit-desc-clickable { cursor: pointer; }
           .audit-desc-clickable:hover { text-decoration: underline; color: #0d6efd; }",
          .AUDIT_STICKY_CSS
        ))),
        div(
          id = ns("sticky_scope"),
          div(
            id = ns("sticky_toolbar"), class = "audit-sticky-toolbar",
            div(
              class = "d-flex align-items-end gap-2 flex-wrap",
              if (identical(mode, "multi_client"))
                div(style = "min-width:170px;",
                    tags$label("Cliente", class = "form-label small fw-semibold mb-1"),
                    selectInput(ns("audit_client_sel"), NULL,
                               choices  = choices,
                               selected = if (length(choices)) choices[[1]] else NULL,
                               width    = "100%")),
              div(style = "min-width:330px;",
                  tags$label("Rango (fecha y hora)", class = "form-label small fw-semibold mb-1"),
                  shinyWidgets::airDatepickerInput(
                    ns("audit_range"), label = NULL,
                    range = TRUE, timepicker = TRUE,
                    value = c(Sys.time() - 7 * 86400, Sys.time()),
                    dateFormat = "dd/MM/yy HH:mm",
                    language = "es",
                    update_on = "close", width = "100%")),
              div(style = "min-width:130px;",
                  tags$label("Módulo", class = "form-label small fw-semibold mb-1"),
                  selectInput(ns("audit_mod_sel"), NULL, choices = c("Todos"), width = "100%")),
              div(style = "min-width:130px;",
                  tags$label("Acción", class = "form-label small fw-semibold mb-1"),
                  selectInput(ns("audit_accion_sel"), NULL, choices = c("Todos"), width = "100%")),
              div(style = "min-width:150px; flex:1;",
                  tags$label("Usuario", class = "form-label small fw-semibold mb-1"),
                  textInput(ns("audit_user_filter"), NULL,
                           placeholder = "Filtrar por usuario...", width = "100%")),
              div(class = "d-flex gap-2 align-items-center",
                  actionButton(ns("audit_refresh"), icon("rotate"),
                              class = "btn btn-sm btn-outline-secondary", title = "Actualizar"),
                  uiOutput(ns("live_toggle_btn"), inline = TRUE),
                  downloadButton(ns("audit_export_pdf"), " Exportar PDF",
                                class = "btn btn-sm btn-outline-primary",
                                icon = icon("file-pdf"),
                                title = "Exportar a PDF el rango y filtros actuales"),
                  # Bare text, no background/border/box -- a quick transient
                  # confirmation when the live-refresh toggle is clicked, shown
                  # briefly then auto-hidden. Not a showNotification() toast
                  # (those always render as a bordered card) and not styled as
                  # a button/badge -- literally just the message text.
                  shinyjs::hidden(
                    tags$span(id = ns("live_toggle_msg"),
                              style = "font-size:0.8rem; color:#6c757d;")
                  ))
            )
          ),
          div(class = "audit-sticky-table", uiOutput(ns("audit_results")))
        ),
        audit_sticky_js(ns("sticky_scope"))
      )
    })

    # Swaps between the results table and a permission-denied alert. A plain
    # reactive() re-evaluates on every upstream invalidation regardless of
    # whether its RETURN VALUE actually changed -- Shiny does not dedupe by
    # value on its own. Reading fetched() directly here meant this whole
    # renderUI (and therefore the dataTableOutput() container it returns)
    # was torn down and rebuilt on every date-range/filter interaction, even
    # when the error/no-error state never changed -- destroying the DT
    # widget's own sort/search/page state each time (reported 2026-08-25/26
    # as "just opening the [date range] clears everything"). error_state is
    # a reactiveVal, only ever updated when the boolean actually flips, so
    # this renderUI -- and the DT container it creates -- now only reruns
    # on a genuine permission-state change (e.g. a staff jump), never on an
    # ordinary data refresh.
    error_state <- reactiveVal(FALSE)
    observe({
      new_err <- inherits(fetched(), "error")
      if (!identical(new_err, isolate(error_state()))) error_state(new_err)
    })

    output$audit_results <- renderUI({
      if (isTRUE(error_state())) {
        return(div(class = "alert alert-warning mt-2", icon("lock"),
                   " No tienes permiso para ver esta bitácora."))
      }
      DT::dataTableOutput(ns("audit_tbl"))
    })

    .audit_cols <- if (identical(mode, "multi_client"))
      c(Hora = "ts", Usuario = "user", Cliente = "client_id", `Módulo` = "module",
       `Acción` = "action", `Descripción` = "description", `ID destino` = "target_id")
    else
      c(Hora = "ts", Usuario = "user", `Módulo` = "module",
       `Acción` = "action", `Descripción` = "description", `ID destino` = "target_id")

    # 0-based column index of "Descripción" -- rownames=FALSE below, so DT's
    # cell_clicked$col (also 0-based) lines up directly with this.
    .audit_desc_col0 <- which(names(.audit_cols) == "Descripción") - 1L

    .audit_display_df <- function(df) {
      if (inherits(df, "error") || is.null(df) || !nrow(df)) {
        return(as.data.frame(
          setNames(lapply(names(.audit_cols), function(x) character()), names(.audit_cols)),
          check.names = FALSE, stringsAsFactors = FALSE
        ))
      }
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      out <- data.frame(Hora = format(as.POSIXct(df$ts), "%d/%m/%Y %H:%M:%S"),
                        stringsAsFactors = FALSE)
      for (label in names(.audit_cols)[-1]) {
        src <- .audit_cols[[label]]
        out[[label]] <- ifelse(is.na(df[[src]]), "", df[[src]])
      }
      out
    }

    # Rendered ONCE (isolate()) -- the table's structure/columns never
    # change after that. All subsequent data changes go through the
    # dataTableProxy below via replaceData(), which updates the rows in
    # place client-side without touching the widget's own sort order,
    # search-box text, or current page (the exact state a full rebuild was
    # wiping every time).
    output$audit_tbl <- DT::renderDataTable({
      isolate(.audit_display_df(filtered()))
    }, options = list(
      pageLength = 25, dom = "ftp", order = list(),
      # Stage 2: visual affordance only (cursor + underline-on-hover) --
      # the actual click handling is input$audit_tbl_cell_clicked below,
      # which DT emits for a cell click regardless of this className.
      columnDefs = list(list(targets = .audit_desc_col0, className = "audit-desc-clickable")),
      # search = "" -- .AUDIT_STICKY_CSS (R/audit_sticky_ui.R) replaces the
      # "Filtrar:" text label with a magnifying-glass icon baked into the
      # input box itself; leaving this non-empty would show both.
      language = list(
        search      = "",
        zeroRecords = "Sin registros",
        info        = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
        infoEmpty   = "Sin registros",
        paginate    = list(previous = "Anterior", `next` = "Siguiente")
      )
    ), selection = "none", rownames = FALSE)

    audit_tbl_proxy <- DT::dataTableProxy("audit_tbl")
    observe({
      DT::replaceData(audit_tbl_proxy, .audit_display_df(filtered()),
                      resetPaging = FALSE, rownames = FALSE)
    })

    # ── Stage 2: click on a Descripción cell -> detail modal ────────────────
    # input$<id>_cell_clicked is DT's own passive click-report input, fired
    # on any cell click independent of `selection` (kept "none" above -- no
    # row-highlight UI, this table is pure click-for-detail). $row is the
    # index into the data as originally handed to the table/replaceData(),
    # NOT the live sorted display position -- same convention this codebase
    # already relies on for input$..._rows_selected elsewhere (e.g. the AP/AR
    # confirm tables in R/pagar_hoy_module.R). .audit_display_df()'s own
    # ts-descending sort is reproduced here so the index lines up with what
    # was actually rendered.
    observeEvent(input$audit_tbl_cell_clicked, {
      info <- input$audit_tbl_cell_clicked
      if (is.null(info) || is.null(info$row) || is.null(info$col)) return()
      if (!identical(as.integer(info$col), as.integer(.audit_desc_col0))) return()
      raw <- filtered()
      if (inherits(raw, "error") || is.null(raw) || !nrow(raw)) return()
      raw_sorted <- raw[order(raw$ts, decreasing = TRUE), , drop = FALSE]
      r <- as.integer(info$row)
      if (is.na(r) || r < 1L || r > nrow(raw_sorted)) return()
      row_data <- raw_sorted[r, , drop = FALSE]
      showModal(modalDialog(
        title = tagList(icon("circle-info"), " Detalle de la actividad"),
        size = "l", easyClose = TRUE,
        .audit_detail_modal_body(row_data, mode),
        footer = modalButton("Cerrar")
      ))
    })

    # ── Stage 3: PDF export of the currently filtered log ───────────────────
    # Same pattern as R/cashflow_export_module.R's btn_export_pdf: build a
    # self-contained HTML doc, print it with pagedown::chrome_print(), fall
    # back to raw HTML with a warning if pagedown/Chrome isn't available --
    # identical resilience to R/pagos_audit_viewer_module.R's dl_pago_pdf.
    output$audit_export_pdf <- downloadHandler(
      filename = function() {
        rng   <- input$audit_range
        from  <- format(if (!is.null(rng) && length(rng) == 2) rng[[1]] else Sys.time() - 7 * 86400, "%d%b%Y")
        to    <- format(if (!is.null(rng) && length(rng) == 2) rng[[2]] else Sys.time(), "%d%b%Y")
        label <- if (identical(mode, "multi_client"))
          (input$audit_client_sel %||% "grupo")
        else
          tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% "hopdesk"
        label <- gsub("[^A-Za-z0-9]", "", label)
        paste0("Actividad_HopDesk_", if (nzchar(label)) label else "reporte", "_", from, "-", to, ".pdf")
      },
      content = function(file) {
        showNotification(
          tagList(tags$span(class = "spinner-border spinner-border-sm me-1"),
                  "Generando PDF… (puede tomar unos segundos)"),
          id = "audit_export_notif", type = "message", duration = NULL)
        on.exit(removeNotification("audit_export_notif"), add = TRUE)

        export_df <- filtered()
        if (inherits(export_df, "error") || is.null(export_df)) export_df <- export_df[0, ]

        rng <- input$audit_range
        user_info <- tryCatch(shared$current_user_info(), error = function(e) NULL)
        client_label <- if (identical(mode, "multi_client"))
          (input$audit_client_sel %||% "—")
        else
          tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% "—"

        html_path <- tryCatch(
          generate_audit_log_html_doc(export_df, list(
            generated_by   = user_info$name %||% user_info$user %||% "—",
            date_from      = if (!is.null(rng) && length(rng) == 2) rng[[1]] else Sys.time() - 7 * 86400,
            date_to        = if (!is.null(rng) && length(rng) == 2) rng[[2]] else Sys.time(),
            modulo_filter  = input$audit_mod_sel,
            accion_filter  = input$audit_accion_sel,
            usuario_filter = input$audit_user_filter,
            client_label   = client_label
          )),
          error = function(e) {
            showNotification(paste("Error generando el documento:", conditionMessage(e)),
                             type = "error", duration = 10); NULL
          })
        if (is.null(html_path)) stop("Error al generar el documento HTML.")

        if (!requireNamespace("pagedown", quietly = TRUE)) {
          file.copy(html_path, file, overwrite = TRUE)
          showNotification(
            tagList(tags$strong("Instala pagedown primero:"), tags$br(),
                   tags$code('install.packages("pagedown")'), tags$br(),
                   tags$small("Se descargó el reporte como HTML mientras tanto.")),
            type = "warning", duration = 15)
          return(invisible(NULL))
        }

        chrome_extra <- if (.Platform$OS.type == "unix")
          c("--no-sandbox", "--disable-setuid-sandbox", "--disable-dev-shm-usage") else character(0)

        tryCatch({
          pagedown::chrome_print(
            input = html_path, output = file, timeout = 120,
            extra_args = chrome_extra,
            options = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                          paperWidth = 8.27, paperHeight = 11.69,
                          marginTop = 0.59, marginBottom = 0.59, marginLeft = 0.55, marginRight = 0.55)
          )
        }, error = function(e) {
          file.copy(html_path, file, overwrite = TRUE)
          showNotification(
            tagList(tags$strong("No se pudo generar el PDF (¿Chrome instalado?):"), tags$br(),
                   tags$code(conditionMessage(e)), tags$br(),
                   tags$small("Se descargó el reporte como HTML en su lugar.")),
            type = "warning", duration = 15)
        })

        log_action(
          user = tryCatch(shared$current_user(), error = function(e) NULL) %||% user_info$user %||% "unknown",
          module = "audit_export", action = "exportar_actividad_pdf",
          description = sprintf("Exportación de Actividad a PDF: %d entrada(s)", nrow(export_df)),
          target_id = client_label,
          metadata = list(n_entradas = nrow(export_df), modulo_filter = input$audit_mod_sel,
                          accion_filter = input$audit_accion_sel, usuario_filter = input$audit_user_filter,
                          mode = mode),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        )
      }
    )
  })
}
