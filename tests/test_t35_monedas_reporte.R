# =============================================================================
# tests/test_t35_monedas_reporte.R  (monedas fase 3, T35)
# Reporte without currency codes written by hand. The report used to be two
# fixed columns, MXN and USD: total boxes, group movements, the pulse's
# blocks, colors. Now its currencies come from the data, the client's base
# first; a letter page holds two, the rest are named in a note.
#
#   A. Networks Group (MXN base, MXN/USD accounts, MXN/USD/EUR documents):
#      the report HTML (4 pages) and the settings summary are byte-for-byte
#      a5c696a's (tests/fixtures/t35/, with the classes mxn->mon1, usd->mon2).
#   B. A COP client with COP/USD/EUR: COP and USD get the boxes, EUR the note.
#   C. Only MXN: one box, no "USD $0.00", no note.
#   D. SAP without a currency column: the documents are in the base.
#   E. The note in English.
#
# Contraprueba: on a5c696a (git worktree + this file, the fixture and its
# references) B, C, D and E fail; A passes (it is the reference).
# Run from the project root:  Rscript tests/test_t35_monedas_reporte.R
# =============================================================================
cat("=== test_t35_monedas_reporte ===\n\n")
source("tests/helpers/reporte_fixture.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# The references were written with "\n"; a Windows checkout (core.autocrlf)
# gives them "\r\n". The report itself never has "\r", so it is dropped.
leer <- function(p) {
  x <- rawToChar(readBin(p, "raw", file.size(p))); Encoding(x) <- "UTF-8"
  gsub("\r\n", "\n", x, fixed = TRUE)
}
cuenta <- function(patron, x) lengths(regmatches(x, gregexpr(patron, x, fixed = TRUE)))
tiene <- function(x, ...) all(vapply(c(...), function(p) grepl(p, x, fixed = TRUE), logical(1)))
nada  <- function(x, ...) !any(vapply(c(...), function(p) grepl(p, x, fixed = TRUE), logical(1)))

# ── A. Networks Group: identical to a5c696a ─────────────────────────────────
ng <- reporte_ng()
a  <- reporte_correr(ng$cuentas, ng$movs, ng$sap)
ref <- leer("tests/fixtures/t35/ng_reporte_a5c696a.html")
# [CAMBIA TEMA 4b] The report's colours come from the theme registry: written
# another way ("#FFFFFF" for "#fff", tema_mezcla()'s "rgba(30, 58, 95, 0.08)")
# and, where a colour was within ΔE 2 of its role, the role's. This test is
# about the report's structure, words and amounts, so both sides compare with
# every colour replaced by one mark; tests/test_tema_reporte_igual.R checks
# the colours themselves, one by one.
norm_col <- function(x) {
  x <- gsub("rgba?\\([^)]*\\)", "§color", x, perl = TRUE)
  gsub("(?<![&\\w])#(?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])", "§color", x, perl = TRUE)
}
a$html <- norm_col(a$html); ref <- norm_col(ref)
for (k in c("tot-box mxn", "tot-box usd", ".tot-box.mxn{", ".tot-box.usd{", "group-col-mxn",
            "group-col-usd", "pulse-mxn-label", "pulse-usd-label"))
  ref <- gsub(k, sub("usd", "mon2", sub("mxn", "mon1", k, fixed = TRUE), fixed = TRUE), ref, fixed = TRUE)
# [CAMBIA TEMA 4b] The report's words come from the dictionary (Spanish here):
# its English titles, labels and IN/OUT in Spanish, the accents as UTF-8
# instead of entities, one "Reporte financiero", the months from comun.mes.*
# ("nov", not the server locale's "Nov") and the chart's days from
# tablas.flujo.dia.* ("L 16", not "Mon 16"). The reference is translated by
# exactly this list; any other change still fails A1.
for (e in list(c("&oacute;", "ó"), c("&aacute;", "á"), c("&iacute;", "í"), c("&Iacute;", "Í"),
               c("Reporte Financiero", "Reporte financiero"), c("Financial Summary Report", "Resumen financiero"),
               c(">Company<", ">Empresa<"), c("NG Finances", "NG Finanzas"), c("Total Balance ", "Saldo total "),
               c(">Dir.<", ">Sentido<"), c(">SPEI IN<", ">SPEI recibido<"), c(">SPEI OUT<", ">SPEI enviado<"),
               c(">IN<", ">Abono<"), c(">OUT<", ">Cargo<"),
               c("Daily Forward Cash Flow Pulse", "Pulso diario de flujo de efectivo"),
               c("Síntesis AP/AR", "Síntesis CxP/CxC")))
  ref <- gsub(e[1], e[2], ref, fixed = TRUE)
for (m in 1:12)
  ref <- gsub(paste0("([0-9]{2}) ", month.abb[m], "\\b"), paste0("\\1 ", tr(paste0("comun.mes.", m), idioma = "es")), ref, perl = TRUE)
dias_en <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
for (k in 1:7)
  ref <- gsub(paste0(">", dias_en[k], " ([0-9]{2})<"), paste0(">", tr(paste0("tablas.flujo.dia.", k), idioma = "es"), " \\1<"), ref, perl = TRUE)
# [CAMBIA T55b] The amounts are monto_moneda()'s now ("282,000.00 MXN",
# "−2,300.00", "&minus;14,500.00", the axis "25K"), and each Pulse chart has
# its currency as the axis title. The rest is still a5c696a's, byte for byte:
# split into tags and texts, the two sides differ only where both are an
# amount -- the old one with "$", the new one without -- and the new one only
# adds the axis titles.
tokens <- function(s) { s <- gsub("(<[^>]+>)", "\n\\1\n", s); s <- trimws(unlist(strsplit(s, "\n"))); s[nzchar(s)] }
monto_viejo <- "^[+-]?\\$-?[0-9][0-9,]*(\\.[0-9]+)?k?$"
monto_nuevo <- paste0("^(\\+|", intToUtf8(0x2212L), "|&minus;)?[0-9][0-9,]*(\\.[0-9]+)?[KM]?( [A-Z]{3})?$")
TITULO_EJE  <- norm_col('<text x="2" y="6.0" font-size="5.5" font-weight="600" fill="#6b7280">')
solo_montos <- function(nuevo, viejo) {
  n <- tokens(enc2utf8(nuevo)); v <- tokens(enc2utf8(viejo))
  k <- which(n == TITULO_EJE)                                 # the axis titles: <text>, its code, </text>
  if (length(k)) n <- n[-c(k, k + 1L, k + 2L)]
  if (length(n) != length(v)) return(FALSE)
  d <- which(n != v)
  length(d) > 0L && all(grepl(monto_viejo, v[d])) && all(grepl(monto_nuevo, n[d]))
}
ok("A1: NG's report (4 pages) is a5c696a's (classes mxn/usd -> mon1/mon2) with the amounts as the only change [T55b]",
   solo_montos(a$html, ref))
ok("A2: NG's settings summary (Total MXN, Total USD, Cuentas, Período) is a5c696a's with the amounts as the only change [T55b]",
   solo_montos(a$resumen, leer("tests/fixtures/t35/ng_resumen_a5c696a.html")))
ok("A1c: …and the rule catches anything else: one word changed, or an amount moved to another place, fails it",
   !solo_montos(sub("Cobros esperados", "Cobros previstos", a$html, fixed = TRUE), ref) &&
     !solo_montos(sub("282,000.00 MXN", "Total", a$html, fixed = TRUE), ref))
ok("A1b: no \"$\" before a figure anywhere in the report; each Pulse chart titles its axis with its currency (MXN, USD)",
   !grepl("\\$ ?-?[0-9]", a$html) && cuenta(paste0(TITULO_EJE, "MXN</text>"), a$html) >= 1L &&
     cuenta(paste0(TITULO_EJE, "USD</text>"), a$html) >= 1L)
ok("A3: NG's pulse still names EUR in its note, with 'se muestran solo MXN y USD'",
   tiene(a$html, "Monedas adicionales detectadas: EUR — se muestran solo MXN y USD."))

# [CAMBIA TEMA 4b] The box's title comes from the dictionary ("Saldo total" in Spanish).
TB <- function(m) paste(tr("tablas.reporte.doc.saldo_total"), m)

# ── B. A COP client with COP, USD and EUR ───────────────────────────────────
cop <- ng
cop$cuentas$Moneda <- c("COP", "USD", "USD", "EUR")   # COP base; USD 2 accounts, EUR 1
cop$sap$AR$Moneda  <- c("COP", "USD", "COP", "COP", "USD", "EUR")
cop$sap$AP$Moneda  <- c("COP", "USD", "COP", "USD", "EUR")
b <- reporte_correr(cop$cuentas, cop$movs, cop$sap, base = "COP")
ok("B1: the boxes are COP (first, blue) and USD (second, green)",
   tiene(b$html, '<div class="tot-box mon1">', TB("COP"), '<div class="tot-box mon2">', TB("USD")) &&
     regexpr(TB("COP"), b$html) < regexpr(TB("USD"), b$html))
ok("B2: no box for EUR, and none for MXN (the client has none)",
   nada(b$html, TB("EUR"), TB("MXN"), "tot-box mxn", "tot-box usd"))
ok("B3: page 1 names EUR in the note: 'se muestran solo COP y USD'",
   tiene(b$html, "Monedas adicionales detectadas: EUR — se muestran solo COP y USD."))
ok("B4: the EUR account still gets its card (every account shows)", tiene(b$html, "NRS USD — EUR"))
ok("B5: the pulse's blocks are COP and USD, on both weeks",
   cuenta('pulse-currency-label pulse-mon1-label">COP<', b$html) == 2L &&
     cuenta('pulse-currency-label pulse-mon2-label">USD<', b$html) == 2L &&
     nada(b$html, 'pulse-mon1-label">MXN<', 'pulse-mon2-label">MXN<'))
ok("B6: the settings summary gives one total per currency, COP first, EUR too",
   tiene(b$resumen, "Total COP", "Total USD", "Total EUR") && nada(b$resumen, "Total MXN") &&
     regexpr("Total COP", b$resumen) < regexpr("Total USD", b$resumen))

# ── C. Only MXN ─────────────────────────────────────────────────────────────
mx <- ng
mx$cuentas <- mx$cuentas[mx$cuentas$Moneda == "MXN", ]
mx$movs <- mx$movs[mx$movs$cuenta_id %in% mx$cuentas$id, ]
mx$sap$AR <- mx$sap$AR[mx$sap$AR$Moneda == "MXN", ]
mx$sap$AP <- mx$sap$AP[mx$sap$AP$Moneda == "MXN", ]
c_ <- reporte_correr(mx$cuentas, mx$movs, mx$sap, base = "MXN")
ok("C1: one box, MXN; no 'Total Balance USD' at $0.00", cuenta('<div class="tot-box mon', c_$html) == 1L &&
     tiene(c_$html, TB("MXN")) && nada(c_$html, TB("USD")))
ok("C2: no note, and the pulse has only the MXN block", nada(c_$html, "Monedas adicionales") &&
     cuenta('<span class="pulse-currency-label', c_$html) == 2L && nada(c_$html, 'pulse-mon2-label">'))
ok("C3: the settings summary has Total MXN and no Total USD", tiene(c_$resumen, "Total MXN") && nada(c_$resumen, "Total USD"))

# ── D. SAP without a currency column ────────────────────────────────────────
sin <- cop
sin$sap$AR$Moneda <- NULL; sin$sap$AP$Moneda <- NULL
d <- reporte_correr(sin$cuentas, sin$movs, sin$sap, base = "COP")
ok("D1: documents without a currency are the base's (COP), not MXN",
   cuenta('pulse-currency-label pulse-mon1-label">COP<', d$html) == 2L && nada(d$html, 'pulse-mon1-label">MXN<'))

# ── E. The note in English ──────────────────────────────────────────────────
op <- options(hopdesk.idioma = "en")
e <- reporte_correr(cop$cuentas, cop$movs, cop$sap, base = "COP")
options(op)
ok("E1: in English, 'Additional currencies found: EUR — only COP and USD are shown.'",
   tiene(e$html, "Additional currencies found: EUR — only COP and USD are shown."))

# ── Static ──────────────────────────────────────────────────────────────────
src <- paste(readLines("R/reporte_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("S1: R/reporte_module.R has no currency code in quotes",
   !grepl('"(MXN|USD|EUR|MXP|MXV)"', src))
ok("S2: both keys exist in es and en",
   all(vapply(c("es", "en"), function(i) {
     d <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", i))
     all(c("tablas.reporte.monedas_extra", "tablas.reporte.monedas_y") %in% names(d)) }, logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
