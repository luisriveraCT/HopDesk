# =============================================================================
# R/pasivos_table_styles.R
# Stage 3: the CSS the Pasivos module still uses, and its currency-badge colours.
# Pure functions — no I/O, no reactives.
# =============================================================================

# ── pasivos_table_css ─────────────────────────────────────────────────────────
# Returns a <style> tag with the CSS the Pasivos module still uses.
# Called once from pasivos_table_module_ui() or appended to app_styles().
pasivos_table_css <- function() {
  shiny::tags$style(shiny::HTML("
/* The Pasivos grid is drawn by hd_table (T17b); the old table's rules are
   gone (TEMA-P1). Only the window buttons (Antes / Después) remain. */
.pasivos-extend-btn {
  font-size: 12px;
  padding: 2px 8px;
}
"))
}

# Badge colors by position in .pasivos_cur_order() (T33, Luis: no currency
# written by hand): the base is blue, the next two green and purple, the
# rest gray. A client in MXN with MXN, USD and EUR keeps today's colors.
.PASIVOS_CUR_COLORES <- c("#2563eb", "#16a34a", "#7c3aed")
.PASIVOS_CUR_GRIS    <- "#6b7280"
.pasivos_cur_color <- function(cur, orden) {
  k <- match(cur, orden)
  if (length(k) != 1L || is.na(k) || k > length(.PASIVOS_CUR_COLORES)) .PASIVOS_CUR_GRIS else .PASIVOS_CUR_COLORES[k]
}

