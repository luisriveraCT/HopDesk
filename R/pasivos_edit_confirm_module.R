# =============================================================================
# R/pasivos_edit_confirm_module.R
# Stage 4: Edit-confirmation panel for reconciliation conflicts.
#
# Opened after the wizard saves an edit (pasivos_wizard_module.R) when
# pasivos_reconcile_provisions() returns conflicts: future provisions edited
# by hand that the new terms would change.
#
# The table is the engine's (hd_table "conflictos_pasivo", class E, T20): one
# row per conflict, by its provision id, with the choice per row (opcion_fila:
# Nuevo / Mantener / Otro) and the amount for "Otro" typed in its cell
# (edicion_celda; typing picks "Otro").
#
# Public entry point:
#   pasivos_edit_confirm_open(session, conflicts, replacement, shared, liability_id)
# Setup call (once from app.R):
#   setup_pasivos_edit_confirm(input, output, session, shared)
# =============================================================================

# ── Open entry point ──────────────────────────────────────────────────────────
# `conflicts` and `replacement` are tibbles from pasivos_reconcile_provisions()
# and pasivos_conflictos_reemplazo().
pasivos_edit_confirm_open <- function(session, conflicts, replacement,
                                       shared, liability_id) {
  # The panel's state lives in this session, set up by
  # setup_pasivos_edit_confirm(). It used to be an environment of the package,
  # one for every session of the R process, so a user could see (and save
  # decisions on) another user's conflicts (found 2026-11-04, T18a).
  st <- session$userData$pec_state
  if (is.null(st))
    stop("pasivos_edit_confirm_open(): setup_pasivos_edit_confirm() did not run in this session",
         call. = FALSE)
  st$conflicts    <- conflicts
  st$replacement  <- replacement
  st$liability_id <- liability_id
  # Every opening starts on "Mantener" (T20): nothing chosen in an earlier
  # panel -- cancelled, or another liability's -- carries over.
  st$restaurado   <- NULL
  st$apertura     <- (shiny::isolate(st$apertura) %||% 0L) + 1L

  shiny::showModal(.pec_modal_ui())
}

# ── Modal UI ──────────────────────────────────────────────────────────────────
.pec_modal_ui <- function() {
  shiny::modalDialog(
    title     = tr("tablas.pasivos.conflictos.titulo"),
    size      = "l",
    easyClose = FALSE,
    shiny::div(class = "mb-3 text-muted small", tr("tablas.pasivos.conflictos.ayuda")),
    hd_table_ui("pec_tabla", alto = "320px"),
    shiny::div(
      class = "mt-2 d-flex gap-2",
      shiny::actionButton("pec_all_mantener", tr("tablas.pasivos.conflictos.todos_mantener"),
                          class = "btn btn-outline-secondary btn-sm"),
      shiny::actionButton("pec_all_nuevo",    tr("tablas.pasivos.conflictos.todos_nuevo"),
                          class = "btn btn-outline-primary btn-sm")
    ),
    footer = shiny::tagList(
      shiny::actionButton("pec_cancel", tr("tablas.pasivos.conflictos.cancelar"), class = "btn btn-secondary"),
      shiny::actionButton("pec_save",   tr("tablas.pasivos.conflictos.guardar"),  class = "btn btn-primary")
    )
  )
}

# ── The table ─────────────────────────────────────────────────────────────────
.pec_columnas <- function() list(
  col_fecha("fecha", "tablas.pasivos.conflictos.col.fecha"),
  col_texto("antes", "tablas.pasivos.conflictos.col.antes", recorte = FALSE),
  col_texto("nuevo", "tablas.pasivos.conflictos.col.nuevo", recorte = FALSE),
  col_opcion("decision", "tablas.pasivos.conflictos.col.decision",
             opciones = c(aplicar      = "tablas.pasivos.conflictos.op.nuevo",
                          mantener     = "tablas.pasivos.conflictos.op.mantener",
                          personalizar = "tablas.pasivos.conflictos.op.otro"),
             al_escribir = "personalizar", burbuja = c(aplicar = "nuevo")),
  col_dinero("otro", "tablas.pasivos.conflictos.col.otro", sumar = FALSE, editable = list(min = 0))
)

# "fecha · monto", in the engine's own date and money formats. Antes and Nuevo
# show the date too (T20): a provision whose date was moved by hand is a
# conflict even when its amount wasn't touched, and showing amounts alone made
# it look like an amount conflict.
.pec_fecha_monto <- function(fecha, monto) {
  f <- col_fecha("f")$valores(fecha)$mostrar
  m <- col_dinero("m")$valores(monto)$mostrar
  f[!nzchar(f)] <- "—"; m[!nzchar(m)] <- "—"
  paste(f, m, sep = " · ")
}

# The table's rows: one per conflict, sorted by date. `id` is the opening's
# number and the provision's id ("<apertura>:<prov_id>"), so a choice the
# browser still holds from an earlier panel never lands on this one; prov_id
# is what is saved. restaurado: list(decision = c(<prov_id> = op),
# otro = c(<prov_id> = amount)), what the person had chosen before
# "Continuar revisando" rebuilt the panel.
pasivos_conflictos_filas <- function(conflicts, replacement, apertura = 1L, restaurado = NULL) {
  vacia <- data.frame(id = character(0), prov_id = character(0), fecha = as.Date(character(0)),
                      antes = character(0), nuevo = character(0), decision = character(0),
                      otro = numeric(0), Moneda = character(0), stringsAsFactors = FALSE)
  if (is.null(conflicts) || !nrow(conflicts)) return(vacia)
  conflicts <- as.data.frame(conflicts, stringsAsFactors = FALSE)
  pid <- as.character(conflicts$id)
  monto_antes <- vapply(seq_len(nrow(conflicts)), function(i)
    tryCatch(as.numeric(pasivos_pago_amount(conflicts[i, , drop = FALSE])), error = function(e) NA_real_), numeric(1))
  k <- if (!is.null(replacement) && nrow(replacement)) match(pid, as.character(replacement$id)) else rep(NA_integer_, length(pid))
  rep <- if (!is.null(replacement) && nrow(replacement)) as.data.frame(replacement, stringsAsFactors = FALSE) else NULL
  fecha_nueva <- if (is.null(rep)) as.Date(rep(NA, length(pid))) else as.Date(rep$fecha_efectiva[k])
  monto_nuevo <- vapply(seq_along(pid), function(i)
    if (is.na(k[i])) NA_real_ else tryCatch(as.numeric(pasivos_pago_amount(rep[k[i], , drop = FALSE])),
                                            error = function(e) NA_real_), numeric(1))
  dec <- rep("mantener", length(pid))
  otro <- rep(NA_real_, length(pid))
  if (!is.null(restaurado)) {
    d <- restaurado$decision[pid]; dec[!is.na(d)] <- d[!is.na(d)]
    o <- restaurado$otro[pid];     otro[!is.na(o)] <- o[!is.na(o)]
  }
  out <- data.frame(
    id       = paste0(apertura, ":", pid),
    prov_id  = pid,
    fecha    = as.Date(conflicts$fecha_efectiva),
    antes    = .pec_fecha_monto(as.Date(conflicts$fecha_efectiva), monto_antes),
    nuevo    = .pec_fecha_monto(fecha_nueva, monto_nuevo),
    decision = unname(dec),
    otro     = unname(otro),
    Moneda   = as.character(conflicts$moneda_pago %||% "MXN"),
    stringsAsFactors = FALSE)
  out[order(out$fecha, out$prov_id), , drop = FALSE]
}

# Each conflict's decision, by provision id, from the table as it is now:
# the row's choice (t$opciones()) and, for "Otro", the amount typed in its
# cell (t$ediciones(); one restored by "Continuar revisando" counts until it is
# typed over). Returns list(decisions = <what .pec_apply_decisions() takes>,
# sin_monto = prov_ids on "Otro" without a valid amount).
pasivos_conflictos_decisiones <- function(filas, opciones, ediciones) {
  decisions <- list(); sin_monto <- character(0)
  if (is.null(filas) || !nrow(filas)) return(list(decisions = decisions, sin_monto = sin_monto))
  for (i in seq_len(nrow(filas))) {
    id <- filas$id[i]; pid <- filas$prov_id[i]
    op <- if (!is.null(opciones) && nrow(opciones)) opciones$opcion[match(id, opciones$id)] else NA_character_
    if (is.na(op)) op <- filas$decision[i]
    decisions[[pid]] <- op
    if (!identical(op, "personalizar")) next
    e <- if (!is.null(ediciones) && nrow(ediciones)) ediciones[ediciones$id == id, , drop = FALSE] else NULL
    valor <- if (!is.null(e) && nrow(e)) { if (isTRUE(e$valido[1])) e$valor[1] else NA_real_ }
             else if (!is.na(filas$otro[i]) && filas$otro[i] >= 0) filas$otro[i] else NA_real_
    if (is.na(valor)) sin_monto <- c(sin_monto, pid)
    else decisions[[paste0(pid, "_custom")]] <- valor
  }
  list(decisions = decisions, sin_monto = sin_monto)
}

# ── Apply decisions ────────────────────────────────────────────────────────────
.pec_apply_decisions <- function(conflicts, replacement, decisions, shared, user,
                                 liability_id = NA_character_) {
  # Strict read (T20): a read that fails stop()s here. It used to come back as
  # an empty table, and the session's provisions were replaced by it below.
  provs <- leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, shared$effective_client_id(),
                                    .schema_pasivos_provision)
  provs_base <- provs   # CM09: the save applies only these decisions on S3 as it is now

  # Per-provision decision detail for the audit entry (2026-09-03) -- this
  # previously logged only a bare conflict count, even though the real
  # per-provision decisions (aplicar/mantener/personalizar, and the value
  # each one resolved to) were fully in scope in this loop.
  decision_log <- list()

  for (i in seq_len(nrow(conflicts))) {
    prov_id <- conflicts$id[i]
    dec     <- decisions[[prov_id]] %||% "mantener"
    idx     <- which(provs$id == prov_id)
    if (!length(idx)) next
    decision_log[[length(decision_log) + 1L]] <- list(
      provision_id = prov_id, decision = dec,
      custom_value = if (identical(dec, "personalizar"))
        as.numeric(decisions[[paste0(prov_id, "_custom")]] %||% NA) else NA_real_
    )

    if (identical(dec, "aplicar")) {
      # Clear the override, apply generated value
      rep_row <- if (!is.null(replacement) && nrow(replacement))
        replacement[replacement$id == prov_id, , drop = FALSE]
      else NULL

      if (!is.null(rep_row) && nrow(rep_row)) {
        provs$amount_pago[idx[1]]          <- rep_row$amount_pago[1]
        provs$amount_cotizado[idx[1]]      <- rep_row$amount_cotizado[1]
        provs$amount_pago_override[idx[1]] <- NA_real_
        provs$last_edited_by[idx[1]]       <- user
        provs$last_edited_at[idx[1]]       <- Sys.time()
      }

    } else if (identical(dec, "personalizar")) {
      custom_val <- as.numeric(decisions[[paste0(prov_id, "_custom")]] %||% NA)
      if (!is.na(custom_val)) {
        provs$amount_pago_override[idx[1]] <- custom_val
        provs$last_edited_by[idx[1]]       <- user
        provs$last_edited_at[idx[1]]       <- Sys.time()
      }
    }
    # "mantener" → do nothing (keep existing override)
  }

  # Regla 17: a failed save stop()s before the session's copy, the bump or the
  # audit (it used to be swallowed, and the panel said "saved" anyway).
  save_pasivos_provisions(provs, client_id = shared$effective_client_id(), base = provs_base)
  tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
  shared$pasivos_provisions_db(provs)
  bump_sync_version("pasivos_provisions_db", client_id = shared$effective_client_id())

  tryCatch(pasivos_log_audit(
    action_type = "bulk.aplicar_pagos_futuros",
    user        = user,
    target_kind = "bulk",
    target_id   = liability_id %||% NA_character_,
    after       = list(n_conflicts = nrow(conflicts), decisions = decision_log),
    notes       = sprintf("Edit-confirm: %d conflicts resolved", nrow(conflicts)),
    client_id   = shared$effective_client_id()
  ), error = function(e) NULL)
}

# ── Setup ─────────────────────────────────────────────────────────────────────
setup_pasivos_edit_confirm <- function(input, output, session, shared) {

  # This session's open panel: its conflicts, their generated rows and the
  # liability (pasivos_edit_confirm_open() fills it); apertura numbers each
  # opening, restaurado holds the choices "Continuar revisando" gives back.
  st <- shiny::reactiveValues(conflicts = NULL, replacement = NULL, liability_id = NULL,
                              apertura = 0L, restaurado = NULL)
  session$userData$pec_state <- st

  filas <- shiny::reactive(
    pasivos_conflictos_filas(st$conflicts, st$replacement, st$apertura, st$restaurado))
  t <- hd_table_server("pec_tabla", tabla = "conflictos_pasivo", datos = filas,
                       columnas = .pec_columnas(), piezas = c("opcion_fila", "edicion_celda"),
                       vacio = "tablas.pasivos.conflictos.vacio")

  # Mass choices: the table's, in the browser (the person sees them and can
  # still change any row before saving).
  shiny::observeEvent(input$pec_all_mantener, ignoreInit = TRUE, {
    if (!nrow(filas())) return()
    t$poner_opcion("mantener")
  })
  shiny::observeEvent(input$pec_all_nuevo, ignoreInit = TRUE, {
    if (!nrow(filas())) return()
    t$poner_opcion("aplicar")
  })

  # Save decisions
  shiny::observeEvent(input$pec_save, ignoreInit = TRUE, {
    if (!pasivos_autoriza("pasivos.edit_liability")) return()
    conf <- st$conflicts
    repl <- st$replacement
    if (is.null(conf) || !nrow(conf)) { shiny::removeModal(); return() }

    user <- tryCatch(shared$current_user(), error = function(e) "system")
    d <- pasivos_conflictos_decisiones(filas(), t$opciones(), t$ediciones())
    # "Otro" needs its amount (T20): nothing is saved while one is missing or
    # not valid (it used to save the rest and say "saved").
    if (length(d$sin_monto)) {
      shiny::showNotification(tr("tablas.pasivos.conflictos.otro_sin_monto", n = length(d$sin_monto)),
                              type = "warning", duration = 6)
      return()
    }

    # Saved, or nothing: on a failed read or write the panel stays open with
    # the choices as they are, so the person can try again (regla 17).
    guardado <- tryCatch({
      .pec_apply_decisions(conf, repl, d$decisions, shared, user, liability_id = st$liability_id)
      TRUE
    }, error = function(e) {
      message("pasivos_edit_confirm: decisions not saved: ", conditionMessage(e))
      shiny::showNotification(tr("tablas.pasivos.conflictos.no_guardado"), type = "error", duration = 8)
      FALSE
    })
    if (!guardado) return()
    shiny::removeModal()
    shiny::showNotification(tr("tablas.pasivos.conflictos.guardado"), type = "message", duration = 3)
  })

  # Cancel: confirm and default all to "mantener" (no writes). The table goes
  # away with the panel, so what the person chose is kept here first.
  shiny::observeEvent(input$pec_cancel, ignoreInit = TRUE, {
    f <- filas()
    if (nrow(f)) {
      d <- pasivos_conflictos_decisiones(f, t$opciones(), t$ediciones())
      dec  <- vapply(f$prov_id, function(p) d$decisions[[p]] %||% "mantener", character(1))
      otro <- vapply(f$prov_id, function(p) as.numeric(d$decisions[[paste0(p, "_custom")]] %||% NA_real_), numeric(1))
      st$restaurado_pendiente <- list(decision = setNames(dec, f$prov_id), otro = setNames(otro, f$prov_id))
    }
    shiny::showModal(shiny::modalDialog(
      tr("tablas.pasivos.conflictos.cancelar_pregunta"),
      footer = shiny::tagList(
        shiny::actionButton("pec_cancel_no",  tr("tablas.pasivos.conflictos.seguir"),        class = "btn btn-secondary"),
        shiny::actionButton("pec_cancel_yes", tr("tablas.pasivos.conflictos.mantener_todo"), class = "btn btn-danger")
      ),
      size = "s"
    ))
  })

  # "Continuar revisando": the panel again, with the choices it had (T20).
  # A new opening number, so the browser's old choices can't land on it.
  shiny::observeEvent(input$pec_cancel_no, ignoreInit = TRUE, {
    st$restaurado <- st$restaurado_pendiente
    st$apertura   <- st$apertura + 1L
    shiny::removeModal()
    shiny::showModal(.pec_modal_ui())
  })

  shiny::observeEvent(input$pec_cancel_yes, ignoreInit = TRUE, {
    shiny::removeModal()
    shiny::showNotification(tr("tablas.pasivos.conflictos.cancelado"), type = "message", duration = 3)
  })

  invisible(NULL)
}
