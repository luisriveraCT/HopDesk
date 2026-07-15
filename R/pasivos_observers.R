# =============================================================================
# R/pasivos_observers.R
# Reactive observers that close the Pasivos lifecycle automatically when
# bancos_confirmados changes (confirmation and reversal events).
# =============================================================================

# Vector-safe TRUE check — guards against the bare-isTRUE bug on logical vectors.
isTRUE_safe <- function(x) !is.na(x) & x

# Wire up the bancos_confirmados watcher.
# Call once from app.R server after setup_abono_browse().
#
# Watches shared$bancos_confirmados() for rows with provision_id set.
# On confirmation: calls pasivos_provision_item_confirmed().
# On reversal:     calls pasivos_provision_revive() and removes orphaned manual_inv row.
#
# Note: bancos_confirmados schema uses 'confirmacion_id' (not 'id') as the row key.
#
# Investigation findings (Fix 1A):
# 1. first_run guard is required: without it, any rows already-eliminado=TRUE at
#    startup are treated as new reversal events and incorrectly trigger revive().
#    On first run we snapshot current state into seen-sets without firing lifecycle
#    functions; reversals that happened while the user was logged out are not replayed.
# 2. shared$bancos_confirmados() is the same reactive the reversal handlers write to
#    (bancos_module.R:2736 shared$bancos_confirmados(conf)). Identity is correct.
# 3. isTRUE_safe = !is.na(x) & x. Already correct — no fix needed.
# 4. eliminado is logical() in schema; load_bancos_confirmados normalises with
#    ifelse(is.na(df$eliminado), FALSE, df$eliminado). Round-trip preserves type.
setup_pasivos_observers <- function(input, output, session, shared) {

  rv <- shiny::reactiveValues(
    confirmed_seen = character(0),  # confirmacion_ids handled for confirmation
    reversed_seen  = character(0),  # confirmacion_ids handled for reversal
    first_run      = TRUE           # suppress historical replay on startup
  )

  shiny::observe({
    bc <- shared$bancos_confirmados()
    if (is.null(bc) || !nrow(bc)) return()
    if (!"provision_id" %in% names(bc)) return()
    if (!"confirmacion_id" %in% names(bc)) return()

    user <- tryCatch(shared$current_user(), error = function(e) "system")

    # On first run: snapshot current state into seen-sets WITHOUT firing any
    # lifecycle events.  Reversals/confirmations that happened while the user
    # was logged out are not replayed — we only act on events observed live.
    if (rv$first_run) {
      rv$confirmed_seen <- bc$confirmacion_id[
        !is.na(bc$provision_id) & nzchar(bc$provision_id) &
        !isTRUE_safe(bc$eliminado)
      ]
      rv$reversed_seen <- bc$confirmacion_id[
        !is.na(bc$provision_id) & nzchar(bc$provision_id) &
        isTRUE_safe(bc$eliminado)
      ]
      rv$first_run <- FALSE
      return()
    }

    # ---- Confirmation event detection ----------------------------------------
    # New rows: provision_id non-NA/non-empty, eliminado FALSE/NA, not yet handled.
    new_confirms <- bc[
      !is.na(bc$provision_id) & nzchar(bc$provision_id) &
      !isTRUE_safe(bc$eliminado) &
      !(bc$confirmacion_id %in% rv$confirmed_seen),
      , drop = FALSE
    ]

    if (nrow(new_confirms)) {
      for (i in seq_len(nrow(new_confirms))) {
        prov_id <- new_confirms$provision_id[i]
        bc_id   <- new_confirms$confirmacion_id[i]

        tryCatch(
          pasivos_provision_item_confirmed(
            provision_id   = prov_id,
            bancos_conf_id = bc_id,
            user           = user,
            client_id      = shared$active_client_id()
          ),
          error = function(e) {
            warning("[pasivos] failed to mark provision confirmed: ", conditionMessage(e))
          }
        )
      }
      rv$confirmed_seen <- c(rv$confirmed_seen, new_confirms$confirmacion_id)
      # Fix 3A: refresh reactive so calendar reflects item_confirmed state change.
      tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
      tryCatch({
        shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$active_client_id()))
      }, error = function(e) NULL)
    }

    # ---- Reversal event detection --------------------------------------------
    # Rows: provision_id non-NA, eliminado IS TRUE, not yet handled.
    new_reversals <- bc[
      !is.na(bc$provision_id) & nzchar(bc$provision_id) &
      isTRUE_safe(bc$eliminado) &
      !(bc$confirmacion_id %in% rv$reversed_seen),
      , drop = FALSE
    ]

    if (nrow(new_reversals)) {
      for (i in seq_len(nrow(new_reversals))) {
        prov_id <- new_reversals$provision_id[i]

        tryCatch({
          # Read manual_inv_id from the provision BEFORE reviving clears FK fields.
          provs    <- load_pasivos_provisions(client_id = shared$active_client_id())
          prov_row <- provs[provs$id == prov_id, , drop = FALSE]
          mi_id    <- if (nrow(prov_row)) prov_row$manual_inv_id[1] else NA_character_

          pasivos_provision_revive(provision_id = prov_id, user = user,
                                   client_id = shared$active_client_id())

          # Refresh provision reactive so the AP calendar shows the revived item.
          tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
          tryCatch({
            shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$active_client_id()))
          }, error = function(e) NULL)

          # Remove the orphaned manual_inv row so the calendar doesn't show both
          # the revived provision and the now-stale converted item.
          if (!is.na(mi_id) && nzchar(mi_id %||% "")) {
            mi <- shared$manual_inv()
            mi <- mi[mi$id != mi_id, , drop = FALSE]
            shared$manual_inv(mi)
            save_manual(mi, client_id = shared$active_client_id())
          }
        }, error = function(e) {
          warning("[pasivos] failed to revive provision: ", conditionMessage(e))
        })
      }
      rv$reversed_seen <- c(rv$reversed_seen, new_reversals$confirmacion_id)
    }
  })

  invisible(NULL)
}
