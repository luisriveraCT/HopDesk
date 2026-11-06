# =============================================================================
# tests/helpers/abono_adaptador.R
# Abono parcial (T25) seen through one neutral shape. Step 0b recorded the
# hand-made HTML table of master with it; since the migration the same cases
# read the engine's rows (setup_abono_browse()'s facturas(), the data of
# hd_table "abono_parcial"):
#   ab_rejilla(datos) -> tibble(orden, Documento, Referencia, Empresa, Moneda,
#       Parte, Codigo, FechaVenc, Vence, Saldo, SaldoTxt, Grupo)
#     one row per invoice in the order painted: currency bands (MXN, USD,
#     EUR), and within one a group's invoices together where the group first
#     appears; Grupo "Parte|Empresa|Moneda" when 2 or more share it, else NA.
#   ab_grupos(datos)  -> tibble(Grupo, n)
#   ab_vacio(datos)   -> why it is empty (the text), or NA.
# =============================================================================
ab_rejilla <- function(datos) {
  if (is.null(datos) || !nrow(datos)) return(NULL)
  cols <- .ab_columnas()
  campo <- vapply(cols, `[[`, "", "campo")
  texto <- function(c, x) cols[[match(c, campo)]]$valores(x)$mostrar
  clave <- paste(datos$Parte, datos$Empresa, datos$Moneda, sep = "|")
  n <- table(clave)[clave]
  banda <- match(datos$Moneda, CURRENCIES)
  primera <- match(clave, clave)                          # where its group first appears
  o <- order(banda, primera, seq_len(nrow(datos)))
  d <- datos[o, , drop = FALSE]; cl <- clave[o]; nn <- as.integer(n[o])
  tibble::tibble(
    orden = seq_len(nrow(d)), Documento = d$Documento, Referencia = d$Referencia, Empresa = d$Empresa,
    Moneda = d$Moneda, Parte = d$Parte, Codigo = d$Codigo, FechaVenc = as.character(d$Vence),
    Vence = texto("Vence", d$Vence), Saldo = d$Saldo, SaldoTxt = texto("Saldo", d$Saldo),
    Grupo = ifelse(nn >= 2L, cl, NA_character_))
}

ab_grupos <- function(datos) {
  g <- ab_rejilla(datos)
  g <- g[!is.na(g$Grupo), ]
  t <- table(g$Grupo)
  tibble::tibble(Grupo = names(t), n = as.integer(t))
}

ab_vacio <- function(datos) {
  m <- attr(datos, "motivo")
  if (is.null(m)) NA_character_ else tr(m)
}
