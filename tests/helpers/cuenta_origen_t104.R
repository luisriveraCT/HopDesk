# =============================================================================
# tests/helpers/cuenta_origen_t104.R
# T104: the origin-account functions of a payment confirmation
# (R/pagos_audit_viewer_module.R: cuenta_foto, cuenta_confirmacion_texto,
# cuenta_id_de_numero) for a test that loads only some files -- the bank's
# report (R/bancos_respuesta.R), the Agenda's Confirmar
# (R/pagar_hoy_module.R) and Actividad (R/audit_log_viewer_module.R) call
# them. Only their definitions; tr() must exist (R/i18n.R) to show
# "No registrada".
#   source("tests/helpers/cuenta_origen_t104.R")
# CTAS_T104 / BANCOS_T104: a made-up catalog (NCS, MXN, 073543010201, the
# number the T93 fixtures' batches and reports carry).
# =============================================================================
local({
  nombres <- c(".cuenta_digitos", "cuenta_foto", "cuenta_confirmacion_texto", "cuenta_id_de_numero")
  hechos <- character()
  for (e in parse("R/pagos_audit_viewer_module.R", keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) {
      eval(e, globalenv()); hechos <- c(hechos, as.character(e[[2]]))
    }
  falta <- setdiff(nombres, hechos)
  if (length(falta)) stop("tests/helpers/cuenta_origen_t104.R: not in R/pagos_audit_viewer_module.R: ",
                          paste(falta, collapse = ", "), call. = FALSE)
})
CTAS_T104 <- data.frame(id = c("CT-NCS-MXN", "CT-NCS-USD"), banco_id = "BB", Empresa = "NCS", razon_social = "",
                        rfc = "", Moneda = c("MXN", "USD"), alias = c("NCSBB", "NCSUSD"),
                        cuenta = c("73543010201", "73543010999"), clabe_interbancaria = "", is_ppl_default = TRUE,
                        saldo_inicial = 0, activa = TRUE, stringsAsFactors = FALSE)
BANCOS_T104 <- data.frame(id = "BB", nombre = "BanBajío", stringsAsFactors = FALSE)
