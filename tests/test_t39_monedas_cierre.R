# =============================================================================
# tests/test_t39_monedas_cierre.R  (monedas fase 3, T39: the close)
# The last currency codes written by hand outside SAP's loading (the
# integrator's part), on the two rules of
# requisiciones/monedas_fase3_cierre_paso0.md:
#   rule 1, to capture: the data's currencies, the base first, then the rest
#           of the catalog (monedas_para_capturar());
#   rule 2, to filter or order what is there: only the data's, the base first
#           (monedas_para_filtrar()).
#
#   A. moneda_base_sesion(): the client's base for code without `shared`
#      (session$userData$moneda_base, app.R); without it (the test apps) and
#      when it fails, MONEDA_BASE_OMISION.
#   B. The two rules.
#   C. The global short list is gone: no reference in R/, app.R or tests/.
#   D. currency_choices starts empty: the selector shows the base, and the
#      Ledger's fallbacks and the preview's .cf_all_curs() neither fail nor
#      change currency with the empty vector.
#   E. Captures: manual entry (new and edit), Requisiciones (folder and form).
#   F. D (avisos_*) and B (panorama_*) with an MXN base are identical() to
#      23ffc1f (copies of its functions below); with a COP base, COP first and
#      without a code.
# Contraprueba on 23ffc1f: A, B, E and F (COP) fail (the functions do not exist).
# Run from the project root:  Rscript tests/test_t39_monedas_cierre.R
# =============================================================================
cat("=== test_t39_monedas_cierre ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("R/i18n.R", "R/monedas.R", "R/zona.R", "R/ui_components.R", "R/manual_entry_handlers.R",
            "R/agenda_filas.R", "R/panorama_datos.R", "R/avisos_correos.R"))
  source(f)
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/panorama_module.R", c(".pan_monto_corto", ".pan_tarjeta_hoy"))
cargar("R/cashflow_export_module.R", ".cf_monedas_de_datos")
cargar("R/cashflow_preview_module.R", ".cf_all_curs")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.vacio <- function(el, k) is.null(el[[k]]) || (is.symbol(el[[k]]) && !nzchar(as.character(el[[k]])))
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
sesion <- function(userData) list(userData = userData)

# ── A. The base without `shared` ────────────────────────────────────────────
ok("A1: with session$userData$moneda_base, the client's base (COP)",
   identical(moneda_base_sesion(sesion(list(moneda_base = function() "COP"))), "COP"))
ok("A2: without it (the test apps), MONEDA_BASE_OMISION",
   identical(moneda_base_sesion(sesion(list())), MONEDA_BASE_OMISION) &&
     identical(moneda_base_sesion(NULL), MONEDA_BASE_OMISION))
ok("A3: when it fails, MONEDA_BASE_OMISION",
   identical(moneda_base_sesion(sesion(list(moneda_base = function() stop("sin config")))), MONEDA_BASE_OMISION))
ok("A4: app.R leaves it, right after `shared`, as session$userData$usuario",
   grepl("session$userData$moneda_base <- function() shared$moneda_base_cliente()$moneda",
         paste(readLines("app.R", warn = FALSE), collapse = "\n"), fixed = TRUE))

# ── B. The two rules ────────────────────────────────────────────────────────
cap <- monedas_para_capturar(c("USD", "USD", "EUR", "MXN"), "MXN")
ok("B1: capture: the data's (base first, then by documents), then the whole catalog",
   identical(cap[1:3], c("MXN", "USD", "EUR")) && setequal(cap, monedas_catalogo()) && !anyDuplicated(cap))
ok("B2: capture with no data: the base first, and USD is still there",
   identical(monedas_para_capturar(character(), "COP")[1], "COP") && "USD" %in% monedas_para_capturar(character(), "COP"))
ok("B3: filter: only the data's, the base first when present",
   identical(monedas_para_filtrar(c("USD", "MXN", "USD", "EUR", "EUR", "EUR"), "MXN"), c("MXN", "EUR", "USD")) &&
     identical(monedas_para_filtrar(c("USD", "usd "), "MXN"), "USD") &&
     identical(monedas_para_filtrar(character(), "MXN"), character()))

# ── C. The short list is gone ───────────────────────────────────────────────
nombre <- paste0("CURREN", "CIES")   # built, so this file does not count itself
archivos <- c(list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE), "app.R",
              list.files("tests", "\\.R$", recursive = TRUE, full.names = TRUE))
quedan <- archivos[vapply(archivos, function(f) any(grepl(nombre, readLines(f, warn = FALSE), fixed = TRUE)), logical(1))]
ok("C1: no reference to the old short list in R/, app.R or tests/", !length(quedan))
if (length(quedan)) cat("    ", paste(quedan, collapse = "\n     "), "\n")

# ── D. currency_choices starts empty ────────────────────────────────────────
app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
ok("D1: currency_choices starts empty (AR and AP)",
   grepl("AR = reactiveVal(character()),", app, fixed = TRUE) && grepl("AP = reactiveVal(character())", app, fixed = TRUE))
ok("D2: the top selector shows the base while it is empty (AR and AP)",
   lengths(regmatches(app, gregexpr("if (!length(choices)) choices <- moneda_base_sesion(session)", app, fixed = TRUE))) == 2L)
cur_de <- function(available, sel, base) {
  e <- new.env(); e$moneda_base <- function() base
  e$shared <- list(currency_choices = list(AP = function() available), currency = list(AP = function() sel))
  e$ledger <- "AP"
  eval(asignacion("R/ledger_module.R", "available"), e)
  eval(asignacion("R/ledger_module.R", "sel"), e)
  eval(asignacion("R/ledger_module.R", "cur"), e)
  e$cur
}
ok("D3: the Ledger with currency_choices empty: no error, the base (COP), not MXN",
   identical(cur_de(character(), NULL, "COP"), "COP") && identical(cur_de(character(), "USD", "MXN"), "MXN"))
ok("D4: the Ledger once data arrives: the selection stays if available",
   identical(cur_de(c("MXN", "USD"), "USD", "MXN"), "USD"))
sh_vacio <- list(currency_choices = list(AR = function() character(), AP = function() character()),
                 sap_data = function() list(AR = data.frame(Moneda = c("MXN", "USD")), AP = NULL),
                 manual_inv = function() NULL)
ok("D5: the preview's .cf_all_curs() with currency_choices empty: the data's, no error",
   identical(.cf_all_curs(sh_vacio), c("MXN", "USD")))

# ── E. Captures ─────────────────────────────────────────────────────────────
sel_de <- function(html, id) {
  m <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  list(sel = sub('(?s).*<option value="([A-Z]{3})" selected>.*', "\\1", m, perl = TRUE),
       ops = sub('<option value="([A-Z]{3})".*', "\\1", regmatches(m, gregexpr('<option value="[A-Z]{3}"', m))[[1]]))
}
h <- sel_de(as.character(manual_entry_tab_content("AP", c("NG"), monedas = c("USD", "USD", "MXN"), base = "MXN")), "me_moneda")
ok("E1: new manual entry: starts on the base; the ledger's currencies first, then the catalog",
   identical(h$sel, "MXN") && identical(h$ops[1:2], c("MXN", "USD")) && setequal(h$ops, monedas_catalogo()))
h <- sel_de(as.character(manual_entry_tab_content("AP", c("NG"), base = "COP")), "me_moneda")
ok("E2: a COP client: starts on COP", identical(h$sel, "COP") && identical(h$ops[1], "COP"))
req_env <- new.env()
req_env$shared <- list(moneda_base_cliente = function() list(moneda = "COP"),
                       currency_choices = list(AP = function() character()))
eval(asignacion("R/requisiciones_module.R", ".req_monedas"), req_env)
ok("E4: Requisiciones with currency_choices empty: the base (COP) first, then the catalog (USD too)",
   identical(req_env$.req_monedas()[1], "COP") && "USD" %in% req_env$.req_monedas())
req_env$shared <- list(moneda_base_cliente = function() stop("x"),
                       currency_choices = list(AP = function() c("MXN", "USD")))
ok("E5: Requisiciones, NG: MXN then USD first, as before (then the catalog)",
   identical(req_env$.req_monedas()[1:2], c("MXN", "USD")))

# ── F. D and B: identical() with an MXN base ────────────────────────────────
viejo_por_moneda <- function(moneda, importe) {            # panorama_datos.R, 23ffc1f
  vacio <- data.frame(moneda = character(), total = numeric(), n = integer(), stringsAsFactors = FALSE)
  if (!length(moneda)) return(vacio)
  moneda <- toupper(trimws(ifelse(is.na(moneda) | !nzchar(trimws(moneda)), "MXN", moneda)))
  t <- tapply(.pan_num(importe), moneda, sum)
  n <- tapply(rep(1L, length(moneda)), moneda, sum)
  out <- data.frame(moneda = names(t), total = as.numeric(t), n = as.integer(n[names(t)]),
                    stringsAsFactors = FALSE, row.names = NULL)
  out[order(out$moneda != "MXN", out$moneda), , drop = FALSE]
}
viejo_correo_monto <- function(x, moneda = "MXN", moneda_base = "MXN") {   # avisos_correos.R, 23ffc1f
  n <- formatC(round(as.numeric(x), 2), format = "f", digits = 2, big.mark = ",")
  if (identical(moneda, moneda_base)) paste0("$", n) else paste0("$", n, " ", moneda)
}
casos <- list(list(c("USD", "MXN", "mxn", NA, "EUR", ""), c(10, 100, 50, 5, 1, 2)),
              list(c("USD", "EUR"), c(3, 4)), list("MXN", 7), list(character(), numeric()))
ok("F1: .pan_por_moneda with base MXN is identical() to 23ffc1f's",
   all(vapply(casos, function(c) identical(.pan_por_moneda(c[[1]], c[[2]]), viejo_por_moneda(c[[1]], c[[2]])), logical(1))))
# F2, F4, F5 since T46 (Luis: every figure carries its code, the base too, no
# "$"): the Cinta shows the first two currencies, "y N más" from the third.
ok("F2: .pan_monto_corto: the first two currencies with their code, then \"y N más\"",
   identical(.pan_monto_corto(viejo_por_moneda(c("USD", "MXN", "mxn", NA, "EUR", ""), c(10, 100, 50, 5, 1, 2))),
             paste("157.00 MXN, 1.00 EUR", tr("panorama.cinta.mas_monedas", n = "1"))) &&
     identical(.pan_monto_corto(viejo_por_moneda(c("USD", "EUR"), c(3, 4))), "4.00 EUR, 3.00 USD") &&
     identical(.pan_monto_corto(viejo_por_moneda("MXN", 7)), "7.00 MXN"))
# F3 changed on purpose in T45 (Luis): the emails now write every amount
# with its code, the base too, through monto_moneda() (R/ui_components.R);
# correo_monto() and its "$" are gone. The old function stays above as the
# record of what it did.
ok("F3 (T45): correo_monto is gone; the emails use monto_moneda, code always, no $",
   !exists("correo_monto") && exists("viejo_correo_monto") &&
     identical(monto_moneda(1234.5, "MXN"), "1,234.50 MXN") && identical(monto_moneda(9, "USD"), "9.00 USD"))
ns <- function(x) paste0("p-", x)
t_ng <- viejo_por_moneda(c("MXN", "USD"), c(100, 5))
ok("F4: the Hoy card with base MXN: '100.00 MXN' large, '+ 5.00 USD' underneath, no '$'",
   grepl('class="pan-cifra">100.00 MXN<', as.character(.pan_tarjeta_hoy("Pagos", t_ng, "2 pagos", "PH", ns)), fixed = TRUE) &&
     grepl("+ 5.00 USD", as.character(.pan_tarjeta_hoy("Pagos", t_ng, "2 pagos", "PH", ns)), fixed = TRUE) &&
     !grepl("$", as.character(.pan_tarjeta_hoy("Pagos", t_ng, "2 pagos", "PH", ns)), fixed = TRUE))
cop <- .pan_por_moneda(c("USD", "COP", NA), c(5, 100, 1), base = "COP")
ok("F5: a COP client: COP first (a row without currency is COP), with its code",
   identical(cop$moneda, c("COP", "USD")) && identical(cop$total, c(101, 5)) &&
     identical(.pan_monto_corto(cop), "101.00 COP, 5.00 USD") &&
     grepl('class="pan-cifra">101.00 COP<', as.character(.pan_tarjeta_hoy("Pagos", cop, "x", "PH", ns, base = "COP")), fixed = TRUE) &&
     grepl('class="pan-cifra">0.00 COP<', as.character(.pan_tarjeta_hoy("Pagos", cop[0, ], "x", "PH", ns, base = "COP")), fixed = TRUE))
ok("F6: panorama_pagos_hoy passes the base on",
   identical(panorama_pagos_hoy(data.frame(ledger = "AP", status = "pending", tipo_item = NA, FechaVenc = Sys.Date(),
                                           Moneda = c("USD", "COP"), Importe = c(1, 2)), base = "COP")$moneda, c("COP", "USD")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
