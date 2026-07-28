# =============================================================================
# R/pasivos_observers.R
# Reactive observers that close the Pasivos lifecycle automatically when
# bancos_confirmados changes (confirmation events).
#
# Ledger-integrity master plan, 2026-07-23: the REVERSAL side of this file
# was removed. It used to revive the provision (estado -> "provisional",
# clearing manual_inv_id/pagar_hoy_id) and delete the derived manual_inv row
# whenever a provision-linked confirmation was undone — racing independently
# against undo_conf() (R/bancos_module.R), which reacts to the exact same
# bancos_confirmados.eliminado flip. Per Mouse's explicit correction:
# recovering a CONFIRMED item always recovers the ITEM, regardless of
# whether it originated from a converted provision — a provision itself is
# never confirmed (it can never reach Agenda), so there is no "revive the
# provision" step to run here at all. undo_conf() now restores the item
# losslessly on its own (same mechanism as any other manual entry); this
# file reviving the provision on top of that would just re-delete the
# row undo_conf() just restored. See docs/LEDGER_INTEGRITY_MASTER_PLAN.md
# Stage 5 for the full reasoning.
#
# Known open question, flagged to Mouse rather than guessed at: the
# provision's own `estado` now stays at "item_confirmed" after its item's
# confirmation is undone, since nothing reverts it anymore. Whether that
# should transition to something else (not "provisional" — the item still
# exists, just unconfirmed) is a real product decision, not yet made.
# =============================================================================

# Vector-safe TRUE check — guards against the bare-isTRUE bug on logical vectors.
isTRUE_safe <- function(x) !is.na(x) & x

# Wire up the bancos_confirmados watcher.
# Call once from app.R server after setup_abono_browse().
#
# Watches shared$bancos_confirmados() for rows with provision_id set.
# On confirmation: calls pasivos_provision_item_confirmed().
# (No longer reacts to reversal at all -- see the file header.)
#
# Note: bancos_confirmados schema uses 'confirmacion_id' (not 'id') as the row key.
#
# Investigation findings (Fix 1A):
# 1. first_run guard is required: without it, any rows already-eliminado=TRUE at
#    startup would have been treated as new reversal events (before reversal
#    handling was removed entirely, see file header) and incorrectly trigger
#    revive(). On first run we snapshot current state into confirmed_seen
#    without firing any lifecycle functions.
# 2. shared$bancos_confirmados() is the same reactive the confirm handler writes to
#    (bancos_module.R:2736 shared$bancos_confirmados(conf)). Identity is correct.
# 3. isTRUE_safe = !is.na(x) & x. Already correct — no fix needed.
# 4. eliminado is logical() in schema; load_bancos_confirmados normalises with
#    ifelse(is.na(df$eliminado), FALSE, df$eliminado). Round-trip preserves type.
setup_pasivos_observers <- function(input, output, session, shared) {

  rv <- shiny::reactiveValues(
    confirmed_seen = character(0),  # confirmacion_ids handled for confirmation
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
            client_id      = shared$effective_client_id()
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
        shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$effective_client_id()))
      }, error = function(e) NULL)
    }

    # Reversal event detection was removed here -- see the file header
    # comment. undo_conf() (R/bancos_module.R) now handles the entire
    # recovery of a provision-derived confirmation on its own, the same way
    # it handles any other manual entry.
  })

  invisible(NULL)
}
