# =============================================================================
# tests/helpers/abono_adaptador.R
# Abono parcial (T25) seen through one neutral shape, so the same
# characterization runs on today's hand-made HTML table and, after the
# migration, on the engine's rows:
#   ab_rejilla_viejo(html) -> tibble(orden, Documento, Referencia, DocAparte,
#       Empresa, Moneda, Parte, Codigo, FechaVenc, Vence, Saldo, SaldoTxt, Grupo)
#     one row per invoice in the order painted; Grupo "Parte|Empresa|Moneda"
#     or NA when the invoice stands alone.
#   ab_grupos_viejo(html) -> tibble(Grupo, n, TotalTxt) of the group rows.
#   ab_vacio_viejo(html)  -> the empty-state text, or NA.
#   ab_payload_viejo(rejilla, importes) -> what today's JS sends as
#     input$ab_rows for the invoices named in `importes` (Documento = amount).
# =============================================================================
.ab_nbsp <- intToUtf8(160L)
.ab_attr <- function(tag, nombre) {
  m <- regmatches(tag, regexpr(sprintf(' %s="[^"]*"', nombre), tag))
  if (!length(m)) return(NA_character_)
  v <- sub(sprintf('^ %s="(.*)"$', nombre), "\\1", m)
  xml2::xml_text(xml2::read_html(paste0("<p>", v, "</p>")))
}

ab_html <- function(salida) if (is.list(salida)) salida$html else as.character(salida)

ab_rejilla_viejo <- function(html) {
  html <- ab_html(html)
  filas <- regmatches(html, gregexpr('<tr class="ab-(subrow|standalone)"[^>]*>.*?</tr>', html, perl = TRUE))[[1]]
  grupos <- ab_grupos_viejo(html)
  if (!length(filas)) return(NULL)
  do.call(rbind, lapply(seq_along(filas), function(k) {
    tr <- filas[k]
    chk <- regmatches(tr, regexpr('<input type="checkbox" class="ab-check[^>]*>', tr))
    celdas <- regmatches(tr, gregexpr("<td[^>]*>.*?</td>", tr, perl = TRUE))[[1]]
    texto <- function(x) trimws(gsub(.ab_nbsp, " ", xml2::xml_text(xml2::read_html(paste0("<div>", x, "</div>")))))
    gid <- .ab_attr(tr, "data-gid")
    tibble::tibble(
      orden = k, Documento = .ab_attr(chk, "data-documento"), Referencia = .ab_attr(chk, "data-ref"),
      DocAparte = grepl('class="ab-doc">Doc', celdas[2], fixed = TRUE),
      Empresa = .ab_attr(chk, "data-empresa"), Moneda = .ab_attr(chk, "data-moneda"),
      Parte = .ab_attr(chk, "data-parte"), Codigo = .ab_attr(chk, "data-codigo"),
      FechaVenc = .ab_attr(chk, "data-fecha-venc"), Vence = texto(celdas[3]), Saldo = as.numeric(.ab_attr(chk, "data-saldo")), SaldoTxt = texto(celdas[4]),
      Grupo = if (is.na(gid)) NA_character_ else grupos$Grupo[grupos$gid == gid])
  }))
}

ab_grupos_viejo <- function(html) {
  html <- ab_html(html)
  g <- regmatches(html, gregexpr('<tr class="ab-group-row"[^>]*>.*?</tr>', html, perl = TRUE))[[1]]
  if (!length(g)) return(tibble::tibble(gid = character(), Grupo = character(), n = integer(), TotalTxt = character()))
  do.call(rbind, lapply(g, function(tr) {
    celdas <- regmatches(tr, gregexpr("<td[^>]*>.*?</td>", tr, perl = TRUE))[[1]]
    texto <- function(x) trimws(gsub(.ab_nbsp, " ", xml2::xml_text(xml2::read_html(paste0("<div>", x, "</div>")))))
    parte <- texto(regmatches(tr, regexpr("<strong[^>]*>.*?</strong>", tr, perl = TRUE)))
    badges <- regmatches(tr, gregexpr('<span class="badge[^"]*"[^>]*>[^<]*</span>', tr))[[1]]
    tot <- texto(celdas[3])
    tibble::tibble(gid = .ab_attr(tr, "data-gid"),
                   Grupo = paste(parte, texto(badges[2]), sub(" .*$", "", tot), sep = "|"),
                   n = as.integer(texto(badges[1])), TotalTxt = tot)
  }))
}

ab_vacio_viejo <- function(html) {
  html <- ab_html(html)
  m <- regmatches(html, regexpr('<div class="ab-empty">[^<]*</div>', html))
  if (!length(m)) NA_character_ else sub('^<div class="ab-empty">(.*)</div>$', "\\1", m)
}

ab_payload_viejo <- function(rejilla, importes) {
  lapply(names(importes), function(doc) {
    r <- rejilla[rejilla$Documento == doc, ][1, ]
    list(idx = as.character(r$orden), empresa = r$Empresa, moneda = r$Moneda, documento = r$Documento,
         parte = r$Parte, codigo = r$Codigo, referencia = r$Referencia,
         fecha_venc = r$FechaVenc, saldo = r$Saldo, importe = unname(importes[[doc]]))
  })
}
