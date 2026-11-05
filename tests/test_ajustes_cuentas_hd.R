# =============================================================================
# tests/test_ajustes_cuentas_hd.R
# Ajustes › Cuentas de Empresa on the table engine (T16: bancos_cat and
# cuentas_empresa, class D):
#   1. the rows: active accounts only, the company filter, the bank's name,
#      "ppl" on the PPL source account; empty or NULL sources keep the columns;
#   2. by id, with two sessions: another session adds a bank or an account
#      ahead of the one clicked (between drawing and clicking), and ✎ still
#      opens the clicked one -- by position it opened another account, the
#      one whose number goes into the PPL file as source (B2, C2);
#   3. start-up with every source NULL or unreadable: the session goes on and
#      the engine reports no "datos failed";
#   4. "+ Cuenta" takes the bank open in the form (it took the selected row).
#
# Run from project root:  Rscript tests/test_ajustes_cuentas_hd.R
# =============================================================================
cat("=== test_ajustes_cuentas_hd ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "save_ctas_bancos", "save_ctas_cuentas", ".BANCOS_MX_CHOICES"))
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/settings/settings_cuentas.R"))
  suppressMessages(source(f))
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")
BANCOS <- tibble::tibble(id = c("b1", "b2"), nombre = c("BanBajío", "Santander"), clave = c("030", "014"))
cta <- function(id, empresa, banco, cuenta, ppl = FALSE, activa = TRUE, moneda = "MXN")
  tibble::tibble(id = id, banco_id = banco, Empresa = empresa, razon_social = paste(empresa, "SA"), rfc = "",
                 Moneda = moneda, alias = paste0(empresa, "-", moneda), cuenta = cuenta, clabe_interbancaria = "",
                 is_ppl_default = ppl, saldo_inicial = 0, activa = activa)
CTAS <- dplyr::bind_rows(cta("c1", "NTS", "b1", "0100", ppl = TRUE), cta("c2", "NCS", "b2", "0200"),
                         cta("c3", "NTS", "b2", "0300", activa = FALSE))

# ── 1. The rows ──────────────────────────────────────────────────────────────
COLS <- c("id", "ppl", "Empresa", "banco", "Moneda", "alias", "cuenta", "razon_social")
f <- tryCatch(.cuentas_empresa_filas(CTAS, BANCOS), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
ok("solo las cuentas activas, cada una con el nombre de su banco",
   identical(f$id, c("c1", "c2")) && identical(f$banco, c("BanBajío", "Santander")))
ok("la cuenta origen PPL lleva su insignia; las demás no", identical(f$ppl, c("ppl", NA_character_)))
ok("el filtro de empresa deja solo las suyas", identical(.cuentas_empresa_filas(CTAS, BANCOS, "NCS")$id, "c2"))
ok("sin catálogo de bancos, la cuenta sale sin banco (no falla)",
   identical(.cuentas_empresa_filas(CTAS, NULL)$banco, c(NA_character_, NA_character_)))
for (caso in list(list("cuentas NULL", NULL), list("cuentas vacías", .schema_ctas_cuentas()),
                  list("solo inactivas", CTAS[3, ])))
  ok(sprintf("%s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .cuentas_empresa_filas(caso[[2]], BANCOS); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })
ok("bancos: una fila por banco; NULL da 0 filas con sus columnas",
   identical(.bancos_cat_filas(BANCOS)$id, c("b1", "b2")) &&
     identical(names(.bancos_cat_filas(NULL)), c("id", "nombre", "clave")) && !nrow(.bancos_cat_filas(NULL)))

# ── 2-4. On the real observer ────────────────────────────────────────────────
sembrar <- function(bancos = BANCOS, ctas = CTAS) {
  s3_memoria_reset()
  if (!is.null(bancos)) assign(paste0(CID, "/", S3_KEYS$ctas_bancos),  bancos, envir = .mem)
  if (!is.null(ctas))   assign(paste0(CID, "/", S3_KEYS$ctas_cuentas), ctas,   envir = .mem)
}
correr <- function(ctas0, clics) {
  avisos <- character(0)
  shared <- list(usuario = reactiveVal(list(permisos = c(can_manage_bancos = TRUE))),
                 current_user = function() "dev", effective_client_id = function() CID,
                 home_client_id = function() CID, company_map = reactive(CMAP), ctas_cuentas = reactiveVal(ctas0))
  error <- tryCatch(withCallingHandlers(
    testServer(function(input, output, session) settings_cuentas_observer(input, output, session, shared),
               { session$flushReact(); clics(session, output, shared) }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
forma <- function(output) paste(as.character(output$cuentas_edit_form$html), collapse = "")
valor <- function(h, id) { m <- regmatches(h, regexpr(sprintf('id="%s"[^>]*value="[^"]*"', id), h)); sub('.*value="([^"]*)".*', "\\1", m) }

# Two sessions: the table was drawn with c1, c2; another session adds c0
# ahead of them (and b0 ahead of the banks) before this one clicks.
h_cta <- h_bco <- NULL
sembrar()
r <- correr(CTAS, function(session, output, shared) {
  otra <- dplyr::bind_rows(cta("c0", "NCS", "b1", "0999"), CTAS)
  shared$ctas_cuentas(otra)                                    # the sync bus brings the other session's save
  assign(paste0(CID, "/", S3_KEYS$ctas_bancos), dplyr::bind_rows(tibble::tibble(id = "b0", nombre = "Banorte", clave = "072"), BANCOS), envir = .mem)
  session$setInputs(`cuentas_t-accion` = list(accion = "editar", ids = "c2", nonce = 1))
  h_cta <<- forma(output)
  session$setInputs(`bancos_cat_t-accion` = list(accion = "detalle", ids = "b2", nonce = 2))
  h_bco <<- forma(output)
})
ok("dos sesiones: ✎ en la cuenta c2 abre c2 (0200), aunque otra sesión agregó una antes",
   is.null(r$error) && identical(valor(h_cta, "cta_cuenta"), "0200"))
ok("dos sesiones: doble clic en el banco b2 abre Santander, aunque otra sesión agregó uno antes",
   identical(valor(h_bco, "banco_nombre"), "Santander"))

h_nueva <- NULL
sembrar()
r <- correr(CTAS, function(session, output, shared) {
  session$setInputs(`bancos_cat_t-accion` = list(accion = "editar", ids = "b2", nonce = 1))
  session$setInputs(cta_new_cuenta = 1)
  h_nueva <<- forma(output)
})
ok("“+ Cuenta” con un banco abierto: la cuenta nueva lleva ese banco",
   is.null(r$error) && grepl('<option value="b2" selected>', h_nueva, fixed = TRUE))

sembrar(bancos = NULL, ctas = NULL)
.mem_falla <- c(paste0(CID, "/", S3_KEYS$ctas_bancos))       # the bank catalog can't be read
r <- correr(NULL, function(session, output, shared) {
  session$flushReact()
  session$setInputs(cta_empresa_filter = "NTS")
})
if (!is.null(r$error)) message("  ", r$error)
for (f in r$fallas) message("  ", f)
ok("arranque con las cuentas en NULL y el catálogo de bancos ilegible: la sesión sigue y el motor no reporta fallas",
   is.null(r$error) && !length(r$fallas))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
