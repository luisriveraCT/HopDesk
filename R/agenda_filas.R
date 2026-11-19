# =============================================================================
# R/agenda_filas.R -- the Agenda de Hoy's rows, in one place (T60)
#
# Every figure that Panorama shows from the Agenda must equal its twin in the
# Agenda itself. Both read their rows through agenda_pendientes(): the Agenda's
# staged() (R/pagar_hoy_module.R) and panorama_pagos_hoy() (R/panorama_datos.R).
# A card that wants fewer rows filters what this returns (by ledger, by
# tipo_item); it never writes its own criterion again. Before T60 Panorama
# kept its own copy, with a FechaVenc <= today filter the Agenda never had: an
# item due after today sat in the Agenda and never reached Panorama.
#
# What is in: every pending row, whatever its due date (overdue, today or
# ahead) and whatever its origin (ERP, manual, Pasivos, Requisiciones).
# Partial payments (tipo_item "abono", Abono Parcial, T25) are in too, with
# their own positive Importe, as the Agenda's own total counts them: rule 25
# ("abonos parciales: fuera por ahora") stops building that feature further,
# it does not hide what is already staged. Confirmed and cancelled rows are
# out. Amounts keep their sign: nothing here takes abs().
# =============================================================================

agenda_pendientes <- function(ph, ledger = NULL) {
  if (is.null(ph) || !is.data.frame(ph)) return(NULL)
  if (!nrow(ph) || !"status" %in% names(ph)) return(ph[0, , drop = FALSE])
  keep <- ph$status %in% "pending"
  if (!is.null(ledger)) keep <- keep & ph$ledger %in% ledger
  ph[keep, , drop = FALSE]
}
