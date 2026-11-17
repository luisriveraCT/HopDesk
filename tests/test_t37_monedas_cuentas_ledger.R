# =============================================================================
# tests/test_t37_monedas_cuentas_ledger.R  (monedas fase 3, T37)
# Bank accounts, Ledger and Vencidos without currency codes written by hand.
#
#   A. The bank-account form (R/settings/settings_cuentas.R): the whole
#      catalog (Luis), the base and the accounts' currencies on top; an old
#      ctas_cuentas row saying "Pesos"/"Dólares" opens on MXN/USD (it used to
#      fall to the first option, MXN, even for "Dólares"); a new account
#      starts on the base; the column is wider (col-md-3).
#   B. The Ledger's currency selector (R/ledger_module.R): the base first,
#      then by number of documents; the base only when the month has it (a
#      month with only USD opens on USD); no data, the base alone. For NG
#      (MXN base, USD with more documents than EUR) the same as before.
#   C. Vencidos: the base's section first, read at each payload.
#
# The module pieces are taken from the real files' AST and evaluated with
# fakes. Contraprueba on a395bd1: A, B and C fail.
# Run from the project root:  Rscript tests/test_t37_monedas_cuentas_ledger.R
# =============================================================================
cat("=== test_t37_monedas_cuentas_ledger ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("R/i18n.R", "R/monedas.R", "R/ppl_generator.R")) source(f)
source("R/settings/settings_helpers.R")
source("R/settings/settings_cuentas.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# An empty argument (as in x[, 1]) or NULL: nothing to walk into.
# (Checked inside the list: an empty argument put in a variable is "missing".)
.vacio <- function(el, k) is.null(el[[k]]) || (is.symbol(el[[k]]) && !nzchar(as.character(el[[k]])))
# The first assignment to `nombre` anywhere inside the parsed file.
asignacion <- function(file, nombre) {
  buscar <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nombre))) return(e)
      el <- as.list(e)[-1]
      for (k in seq_along(el)) { r <- if (!.vacio(el, k)) buscar(el[[k]]); if (!is.null(r)) return(r) }
    }
    NULL
  }
  for (e in parse(file, keep.source = FALSE)) { r <- buscar(e); if (!is.null(r)) return(r) }
  stop(nombre, " not found in ", file)
}

# ── A. The bank-account form ────────────────────────────────────────────────
cmap <- c(NG = "Networks Group")
fila <- function(moneda) {
  r <- .schema_ctas_cuentas()[NA_integer_, ]
  r$id <- "c1"; r$Empresa <- "NG"; r$banco_id <- "b1"; r$Moneda <- moneda; r$activa <- TRUE
  r
}
form <- function(row, ...) as.character(.cuenta_form_ui(row, NULL, NULL, "", cmap = cmap, ...))
moneda_sel <- function(html) {
  m <- regmatches(html, regexpr('(?s)<select id="cta_moneda".*?</select>', html, perl = TRUE))
  sub('(?s).*<option value="([A-Z]{3})" selected>.*', "\\1", m, perl = TRUE)
}
opciones <- function(html) {
  m <- regmatches(html, regexpr('(?s)<select id="cta_moneda".*?</select>', html, perl = TRUE))
  sub('<option value="([A-Z]{3})".*', "\\1", regmatches(m, gregexpr('<option value="[A-Z]{3}"[^>]*>', m))[[1]])
}
h <- form(NULL, base = "MXN", primero = c("MXN", "USD"))
ok("A1: the selector offers the whole catalog (Luis), MXN and USD on top",
   setequal(opciones(h), monedas_catalogo()) && identical(opciones(h)[1:2], c("MXN", "USD")))
ok("A2: each option says its name ('USD — Dólar estadounidense')",
   grepl("USD — Dólar estadounidense", h, fixed = TRUE))
ok("A3: a new account starts on the base (MXN; COP for a COP client)",
   identical(moneda_sel(h), "MXN") &&
     identical(moneda_sel(form(NULL, base = "COP", primero = "COP")), "COP"))
ok("A4: an old ctas_cuentas row in 'Dólares' opens on USD (it used to fall to MXN)",
   identical(moneda_sel(form(fila("Dólares"), base = "MXN")), "USD"))
ok("A5: an old row in 'Pesos' opens on MXN, also for a COP client",
   identical(moneda_sel(form(fila("Pesos"), base = "MXN")), "MXN") &&
     identical(moneda_sel(form(fila("Pesos"), base = "COP")), "MXN"))
ok("A6: a code stays (USD, COP, lower case too); empty or NA is the base",
   identical(moneda_sel(form(fila("USD"), base = "MXN")), "USD") &&
     identical(moneda_sel(form(fila("cop"), base = "MXN")), "COP") &&
     identical(moneda_sel(form(fila(NA_character_), base = "COP")), "COP") &&
     identical(moneda_sel(form(fila(""), base = "COP")), "COP"))
ok("A7: the currency column is col-md-3 (it was col-md-2), Banco col-md-3",
   grepl('<div class="col-md-3">\\s*<label class="form-label small fw-semibold mb-1">Moneda', h, perl = TRUE) &&
     grepl('<div class="col-md-3">\\s*<label class="form-label small fw-semibold mb-1">Banco', h, perl = TRUE))

# The observer's two helpers, evaluated with a fake shared.
env <- new.env()
env$shared <- list(moneda_base_cliente = function() list(moneda = "COP", origen = "ajuste"))
env$.leer_cuentas <- function() data.frame(Moneda = c("USD", "Dólares", "Pesos", "COP", "COP", "COP"))
eval(asignacion("R/settings/settings_cuentas.R", ".moneda_base"), env)
eval(asignacion("R/settings/settings_cuentas.R", ".monedas_cuentas"), env)
ok("A8: the form's base is the client's (COP); the default when it fails",
   identical(isolate(env$.moneda_base()), "COP") && {
     e2 <- new.env(); e2$shared <- list(moneda_base_cliente = function() stop("sin config"))
     eval(asignacion("R/settings/settings_cuentas.R", ".moneda_base"), e2)
     identical(isolate(e2$.moneda_base()), MONEDA_BASE_OMISION) })
ok("A9: the accounts' currencies, old words read as codes: COP (base), USD (2), MXN (1)",
   identical(isolate(env$.monedas_cuentas("COP")), c("COP", "USD", "MXN")))
ok("A10: the save falls back to the base, not to MXN",
   grepl("input$cta_moneda  %||% .moneda_base()",
         paste(readLines("R/settings/settings_cuentas.R", warn = FALSE), collapse = "\n"), fixed = TRUE))

# ── B. The Ledger's currency selector ───────────────────────────────────────
L <- "R/ledger_module.R"
piezas <- lapply(c("docs_curs", "data_currencies", "choices"), function(n) asignacion(L, n))
opciones_ledger <- function(month_curs, manual_curs = character(), base = "MXN") {
  e <- new.env(); e$month_curs <- month_curs; e$manual_curs <- manual_curs
  e$moneda_base <- function() base
  for (p in piezas) eval(p, e)
  e$choices
}
viejo <- function(raw) {   # a395bd1's rule, for NG
  raw <- unique(raw); pr <- c("MXN", "USD")
  d <- c(intersect(pr, raw), sort(setdiff(raw, pr))); if (length(d)) d else pr
}
ng_mes <- c(rep("MXN", 40), rep("USD", 12), rep("EUR", 2))
ok("B1: NG (MXN 40, USD 12, EUR 2) gives MXN, USD, EUR -- as before",
   identical(opciones_ledger(ng_mes), c("MXN", "USD", "EUR")) && identical(viejo(ng_mes), c("MXN", "USD", "EUR")))
ok("B2: the manual entries count too (USD only in manuals)",
   identical(opciones_ledger(rep("MXN", 3), c("USD", "USD")), c("MXN", "USD")))
ok("B3: a month with only USD opens on USD, never on an empty base",
   identical(opciones_ledger(rep("USD", 4)), "USD") && identical(viejo(rep("USD", 4)), "USD"))
ok("B4: no data: the base alone (it used to invent MXN and USD)",
   identical(opciones_ledger(character()), "MXN") && identical(opciones_ledger(character(), base = "COP"), "COP"))
ok("B5: a COP client: COP first, then by documents (EUR 3 before USD 1)",
   identical(opciones_ledger(c("USD", "EUR", "EUR", "EUR", "COP"), base = "COP"), c("COP", "EUR", "USD")))
ok("B6: NA and blanks are not currencies", identical(opciones_ledger(c("MXN", NA, "", " usd ")), c("MXN", "USD")))
src_l <- paste(readLines(L, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("B7: the two last resorts and the override's prefill are the base",
   lengths(regmatches(src_l, gregexpr("else\\n\\s+moneda_base\\(\\)", src_l, perl = TRUE))) == 2L &&
     grepl('row[["Moneda"]] %||% moneda_base()', src_l, fixed = TRUE))

# ── C. Vencidos ─────────────────────────────────────────────────────────────
llamada <- NULL
buscar_hd <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1]], as.name("hd_table_server")) && !is.null(e$secciones)) { llamada <<- e; return(invisible()) }
    el <- as.list(e)[-1]
    for (k in seq_along(el)) if (!.vacio(el, k)) buscar_hd(el[[k]])
  }
}
for (e in parse("R/vencidos_module.R", keep.source = FALSE)) buscar_hd(e)
sec_env <- new.env(); sec_env$shared <- list(moneda_base_cliente = function() list(moneda = "COP", origen = "ajuste"))
sec <- eval(llamada$secciones, sec_env)
ok("C1: Vencidos' Moneda sections put the client's base first, read at each payload",
   is.function(sec[[1]]$orden) && identical(sec[[1]]$orden(), "COP"))
sec_env$shared <- list(moneda_base_cliente = function() stop("sin config"))
ok("C2: without a base, the default one (MXN, as before)", identical(sec[[1]]$orden(), MONEDA_BASE_OMISION))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
