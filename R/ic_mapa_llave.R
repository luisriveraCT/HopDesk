# =============================================================================
# R/ic_mapa_llave.R
# Which invoice a Mapa IC payment pays.
#
# The Mapa lists the CREDITOR's AR invoices, but the payment is the DEBTOR's
# AP. Sent as-is (the creditor's DocNum, no Codigo), confirming it in the
# Agenda closed nothing: the debtor's invoice stayed open in Vencidos and the
# Calendario, ready to be paid again (T13 trace, 2026-11-03: 0 of 642).
# DocNums are per company and never cross; the creditor's folio does: AR
# Factura == the supplier reference the debtor captured, AP Factura (561 of
# 642 in dev; the other 81 have no AP yet).
# =============================================================================

# The debtor's AP invoice for one Mapa plan item (from_ini pays to_ini for the
# AR invoice item$Factura, in item$Moneda): same pair, same currency, same
# folio. NULL unless exactly one invoice matches -- a payment that would
# close nothing, or the wrong one, is not sent.
ic_mapa_ap_deudor <- function(item, ap, cmap) {
  fac <- toupper(trimws(as.character(item$Factura %||% "")))
  if (length(fac) != 1L || is.na(fac) || !nzchar(fac)) return(NULL)
  if (!is.data.frame(ap) || !nrow(ap) || !all(c(".ini", "Moneda", "Factura", "Parte", "Documento") %in% names(ap)))
    return(NULL)
  cand <- ap[ap$.ini %in% item$from_ini & ap$Moneda %in% item$Moneda &
               toupper(trimws(as.character(ap$Factura))) %in% fac, , drop = FALSE]
  if (nrow(cand)) cand <- cand[.tm_parte_to_ini(cand$Parte, cmap) %in% item$to_ini, , drop = FALSE]
  if (nrow(cand) != 1L) return(NULL)
  amt <- if ("Saldo vencido" %in% names(cand)) "Saldo vencido" else "DocTotal"
  cod <- if ("Codigo" %in% names(cand)) cand[["Codigo"]] else cand[["Código de proveedor"]]
  venc <- cand[["Fecha de vencimiento"]]
  list(Documento = as.character(cand$Documento),
       Codigo    = trimws(as.character(cod %||% "")),
       Importe   = as.numeric(cand[[amt]]),
       FechaVenc = if (is.null(venc)) NA_character_ else as.character(as.Date(venc)))
}
