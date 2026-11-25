# =============================================================================
# tests/test_t104_confirmar.R
# T104: the Agenda's Confirmar (pagos) always carries its origin account.
# On the real pagarHoyServer (testServer), with tests/test_t72f_confirmar.R's
# harness (everything before its cases: S3 in memory, the Agenda of 4 rows,
# correr()) and its catalog: one NTS account in MXN, "CT1" (••••0201).
#   1  with the account: the confirmation runs, its log carries cuenta_id and
#      the snapshot (last 4, alias, bank) -- never the whole number
#   2  the server refuses, with nothing written and its notice:
#      no account sent; an id that is not in the catalog; an account of the
#      company in another currency; another company's account
#   3  .ph_cuenta_confirmacion(): a catalog that can't be read is "lectura"
#      (try again), not "no account"
#   4  the modal: no Confirmar button when the company has no account in the
#      currency, and it says where to add one
# Run from project root:  Rscript tests/test_t104_confirmar.R
# =============================================================================
cat("=== test_t104_confirmar ===\n\n")
src <- readLines("tests/test_t72f_confirmar.R", encoding = "UTF-8", warn = FALSE)
corte <- grep("^evento_parte_real <- evento_parte_de", src)
if (length(corte) != 1L) stop("tests/test_t72f_confirmar.R changed: its harness ends at `evento_parte_real <- evento_parte_de`")
invisible(capture.output(eval(parse(text = src[seq_len(corte - 1L)], encoding = "UTF-8"), envir = globalenv())))
.pass <- 0L; .fail <- 0L
NOTA_SIN <- tr("tablas.agenda.confirmar.sin_cuenta_servidor")
confirmo <- function(r) length(final(r, "AP")) == 1L
nada     <- function(r) !length(datos(r)) && !length(final(r, "AP"))

# ── 1 · With its account ─────────────────────────────────────────────────────
CUENTA_AP <- "CT1"
r <- correr("AP")
m <- final(r, "AP")[[1]]$metadata
ok("1 · con su cuenta: confirma (control)", confirmo(r))
ok("1 · el log lleva cuenta_id y la foto: últimos 4, alias y banco",
   identical(m$cuenta_id, "CT1") && identical(m$cuenta_origen, list(ultimos4 = "0201", alias = "NTSBB", banco = "BanBajío")))
ok("1 · …y se muestra enmascarada", identical(cuenta_confirmacion_texto(m$cuenta_origen), "••••0201 · NTSBB — BanBajío"))
ok("1 · nunca el número completo en el log", !grepl("73543010201", paste(deparse(m), collapse = ""), fixed = TRUE))

# ── 2 · The server refuses ───────────────────────────────────────────────────
for (caso in list(list(NULL, "sin cuenta (el navegador no manda ninguna)"),
                  list("no-existe", "un id que no está en el catálogo"),
                  list("CT-USD", "la cuenta USD de la empresa para un pago MXN"),
                  list("CT-OTRA", "la cuenta MXN de OTRA empresa"))) {
  CUENTA_AP <- caso[[1]]
  CTAS_NTS <- rbind(transform(CTAS_T104[1, ], id = "CT1", Empresa = "NTS", alias = "NTSBB"),
                    transform(CTAS_T104[2, ], id = "CT-USD", Empresa = "NTS"),
                    transform(CTAS_T104[1, ], id = "CT-OTRA", Empresa = "OTR", cuenta = "11111111111"))
  r <- correr("AP")
  ok(sprintf("2 · %s: no confirma ni escribe nada", caso[[2]]), nada(r))
  ok(sprintf("2 · %s: …y el usuario sabe por qué", caso[[2]]), NOTA_SIN %in% r$avisos)
}
CUENTA_AP <- "CT1"; CTAS_NTS <- transform(CTAS_T104[1, ], id = "CT1", Empresa = "NTS", alias = "NTSBB")

# ── 2b · The core refuses on its own (another way in: the bank's report) ─────
ag <- sembrar("AP")
LEIDAS <- 0L; .sl <- safe_load_pagar_hoy
safe_load_pagar_hoy <- function(...) { LEIDAS <<- LEIDAS + 1L; .sl(...) }
AVISOS <- character()
sh_n <- list(effective_client_id = function() CID, current_user = function() "luis", home_client_id = function() CID,
             ctas_cuentas = function() CTAS_NTS, bancos_confirmados = reactiveVal(tibble::tibble()),
             pagar_hoy_db = reactiveVal(ag))
for (cta in list(NA_character_, "no-existe")) {
  isolate(.ph_confirmar_ap(sh_n, EMP, "MXN", ag[ag$id == "X1", ], cta, as.Date("2026-11-19"), list(),
                           .ph_fallido = function(...) NULL, .link_counter = function(...) 0L))
}
ok("2b · .ph_confirmar_ap() sin cuenta o con una que no está: ni lee la Agenda ni escribe nada",
   LEIDAS == 0L && !length(datos(list(seq = SEQ))) && sum(AVISOS %in% NOTA_SIN) == 2L)
safe_load_pagar_hoy <- .sl

# ── 3 · The core, on its own ─────────────────────────────────────────────────
sh <- function(ctas) list(effective_client_id = function() "dev", ctas_cuentas = ctas)
ok("3 · catálogo ilegible: \"lectura\" (reintentar), no \"sin cuenta\"",
   identical(.ph_cuenta_confirmacion(sh(function() stop("S3")), "CT1", "MXN")$error, "lectura"))
ok("3 · NA, \"\", otra moneda: \"sin_cuenta\"; la buena: su foto",
   identical(.ph_cuenta_confirmacion(sh(function() CTAS_NTS), NA, "MXN")$error, "sin_cuenta") &&
     identical(.ph_cuenta_confirmacion(sh(function() CTAS_NTS), "", "MXN")$error, "sin_cuenta") &&
     identical(.ph_cuenta_confirmacion(sh(function() CTAS_NTS), "CT1", "USD")$error, "sin_cuenta") &&
     identical(.ph_cuenta_confirmacion(sh(function() CTAS_NTS), "CT1", "MXN")$foto$ultimos4, "0201"))

# ── 4 · The modal ────────────────────────────────────────────────────────────
modal <- NULL
showModal <- function(ui, ...) { modal <<- paste(as.character(ui), collapse = ""); invisible(NULL) }
abrir <- function() {
  modal <<- NULL
  agenda <- sembrar("AP")
  shared <- list(company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(NULL),
                 parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda),
                 sap_data = reactiveVal(list(AP = NULL, AR = NULL)), sap_ov_db = reactiveVal(NULL),
                 manual_inv = reactiveVal(NULL), papelera_rv = reactiveVal(NULL),
                 bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
                 effective_client_id = function() CID, current_user = function() "luis",
                 usuario = reactiveVal(list(user = "luis", permisos = list())),
                 home_client_id = function() CID, ctas_cuentas = reactiveVal(CTAS_NTS))
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    do.call(session$setInputs, setNames(list("MXN"), paste0("bal_cur_", EMP)))
    do.call(session$setInputs, setNames(list(1), paste0("confirm_ap_", EMP)))
  })))
  modal
}
con <- abrir()
CTAS_NTS <- CTAS_NTS[0, ]
sin <- abrir()
ok("4 · con cuenta: el modal trae su botón Confirmar (control)", !is.null(con) && grepl("✓ Confirmar", con, fixed = TRUE))
ok("4 · sin cuenta en MXN: no hay botón Confirmar", !is.null(sin) && !grepl("✓ Confirmar", sin, fixed = TRUE))
ok("4 · …y dice qué configurar (Configuración → Cuentas de Empresa)",
   grepl(htmltools::htmlEscape(tr("tablas.agenda.confirmar.sin_cuenta", empresa = EMP, moneda = "MXN")), sin, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
