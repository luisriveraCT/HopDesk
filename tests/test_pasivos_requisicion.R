# =============================================================================
# tests/test_pasivos_requisicion.R
# CM-E3b (E3b-P), 2026-11-10. Pasivos' side of the link with Requisiciones
# (requisiciones/E3b_tareas.md), on the real module with testServer and a
# fake `shared` (contract doubles: req_clip, req_abrir_evidencia,
# req_version):
#   - abrir_provision_manual(prefill, al_guardar): the modal starts with the
#     requisition's values (empresa from initials through company_map());
#     al_guardar(provision_id) is called once, only after a successful save,
#     with the saved id; cancel + a plain "+ Provisión manual" links nothing;
#     a failed save does not call it; an al_guardar that fails leaves the
#     provision saved; an empresa that cannot be matched selects none and
#     the save asks for one;
#   - the clip: orphan rows carry req_clip()'s total, the rest 0; without
#     permission (0 rows) no clip; a refresh that does not change the orphan
#     ids does not call req_clip again; the clip opens the row's provision
#     and a forged id opens nothing.
#
# Run from project root:  Rscript tests/test_pasivos_requisicion.R
# =============================================================================
cat("=== test_pasivos_requisicion ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "fmt_money", "CURRENCIES"))
cargar("R/persistence.R", "confirmaciones_vigentes")
cargar("R/pasivos_observers.R", "isTRUE_safe")
cargar("R/pasivos_capabilities.R", c("PASIVOS_CAPABILITIES", "pasivos_autoriza"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/pasivos_table_pivot.R", "R/pasivos_table_styles.R",
            "R/pasivos_table_module.R", "R/pasivos_list_module.R")) suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/pasivos_datos_prueba.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Saves and notices, in this process only (never S3).
G <- new.env(); G$guardados <- 0L; G$falla <- FALSE
PERMITIDO <- TRUE
has_capability <- function(user, cap, empresa = NULL) PERMITIDO
avisos_actividad <- function(...) invisible(TRUE)   # the manual provision's hook (C); avisos.R is not loaded here
permiso_nombre <- function(p) p
save_pasivos_provisions <- function(df, client_id, base = NULL) {
  if (isTRUE(G$falla)) stop("simulated S3 failure")
  G$df <- df; G$guardados <- G$guardados + 1L; invisible(TRUE)
}
bump_sync_version <- function(...) invisible(NULL)
pasivos_log_audit <- function(...) invisible(NULL)
find_proveedor_matches <- function(...) NULL
# What the module shows: the modal's HTML and the notices.
G$modal <- NULL; G$avisos <- character(0)
suppressMessages({
  trace("showModal", where = asNamespace("shiny"), print = FALSE,
        tracer = quote(G$modal <- as.character(ui)))
  trace("showNotification", where = asNamespace("shiny"), print = FALSE,
        tracer = quote(G$avisos <- c(G$avisos, paste0(type, ": ", paste(as.character(ui), collapse = "")))))
})

D <- pasivos_datos_prueba()
CMAP <- c(NTS = "Networks Transport", NRS = "Networks Resources", NG = "Networks Group")
nuevo_shared <- function(clip = function(ids) data.frame(provision_id = character(), total = integer())) {
  G$clip_llamadas <- 0L; G$abiertas <- character(0)
  list(
    pasivos_provisions_db = reactiveVal(D$provisions), pasivos_liabilities_db = reactiveVal(D$liabilities),
    manual_inv = reactiveVal(D$manual_inv), bancos_confirmados = reactiveVal(D$bancos_confirmados),
    visible_initials = reactiveVal(D$visibles), empresa_sel = reactiveVal(D$visibles),
    proveedores_db = reactiveVal(NULL), current_user = reactiveVal("tester"), effective_client_id = function() "prueba",
    suppress_ledger_prov_refresh = reactiveVal(FALSE), company_map = reactive(CMAP),
    req_version = reactiveVal(0L),
    req_clip = function(ids) { G$clip_llamadas <- G$clip_llamadas + 1L; G$clip_ids <- ids; clip(ids) },
    req_abrir_evidencia = function(pid) { G$abiertas <- c(G$abiertas, pid); invisible(TRUE) })
}
PREFILL <- list(requisicion_id = "r-7", folio = "REQ-7", empresa = "NTS", parte = "Sillas del Norte",
                monto = 1234.5, moneda = "USD", referencia = "REQ-7 · Sillas", notas = "Para la sala")
# The inputs the modal would send as it was drawn.
llenar <- function(session, empresa = "Networks Transport")
  session$setInputs(ppm_empresa = empresa, ppm_moneda = "USD", ppm_parte = "Sillas del Norte", ppm_codigo = "",
                    ppm_importe = 1234.5, ppm_fecha = Sys.Date() + 5L, ppm_documento = "",
                    ppm_referencia = "REQ-7 · Sillas", ppm_notas = "Para la sala")
correr <- function(expr, shared = nuevo_shared()) {
  G$modal <- NULL; G$avisos <- character(0); G$guardados <- 0L; G$falla <- FALSE; G$df <- NULL
  G$llamadas <- character(0)
  err <- tryCatch(suppressWarnings(suppressMessages(testServer(pasivos_table_module_server,
                    args = list(shared = shared), { session$flushReact(); eval(expr) }))),
                  error = function(e) conditionMessage(e))
  if (!is.null(err)) message("  testServer: ", err)
  invisible(err)
}
cb <- function(pid) { G$llamadas <- c(G$llamadas, pid); TRUE }
nuevo_id <- function() setdiff(G$df$id, D$provisions$id)

# 1. The function is there
correr(quote(G$hay <- is.function(session$getReturned()$abrir_provision_manual)))
ok("the module returns abrir_provision_manual", isTRUE(G$hay))

# 2. Prefill
correr(quote(session$getReturned()$abrir_provision_manual(PREFILL, cb)))
h <- paste(G$modal, collapse = "")
ok("prefill: proveedor, monto, referencia and notas in the modal",
   grepl('value="Sillas del Norte"', h, fixed = TRUE) && grepl('value="1234.5"', h, fixed = TRUE) &&
     grepl('value="REQ-7 · Sillas"', h, fixed = TRUE) && grepl("Para la sala", h, fixed = TRUE))
ok("prefill: the empresa from its initials, selected by its full name",
   grepl('<option value="Networks Transport" selected>', h, fixed = TRUE))
ok("prefill: the moneda selected", grepl('<option value="USD" selected>', h, fixed = TRUE))
ok("prefill: every company the session sees is offered",
   all(vapply(CMAP, function(n) grepl(sprintf('value="%s"', n), h, fixed = TRUE), TRUE)))

# 3. Save: once, with the saved id; a second click does not call it again
correr(quote({
  session$getReturned()$abrir_provision_manual(PREFILL, cb)
  llenar(session); session$setInputs(ppm_save = 1); session$flushReact()
  G$id1 <- nuevo_id()
  session$setInputs(ppm_save = 2); session$flushReact()
}))
ok("save: al_guardar called once, with the saved provision's id",
   length(G$llamadas) == 1L && length(G$id1) == 1L && identical(G$llamadas, G$id1))

# 4. Cancel, then a plain "+ Provisión manual": links nothing
correr(quote({
  session$getReturned()$abrir_provision_manual(PREFILL, cb)          # closed with Cancelar
  session$setInputs(btn_add_provision_manual = 1)
  llenar(session, empresa = "NTS"); session$setInputs(ppm_save = 1); session$flushReact()
}))
ok("cancel + a plain provision: saved, al_guardar not called", G$guardados == 1L && !length(G$llamadas))

# 5. A failed save does not call it; the retry in the same modal does, once
correr(quote({
  session$getReturned()$abrir_provision_manual(PREFILL, cb)
  llenar(session); G$falla <- TRUE; session$setInputs(ppm_save = 1); session$flushReact()
  G$tras_fallo <- length(G$llamadas)
  G$falla <- FALSE; session$setInputs(ppm_save = 2); session$flushReact()
}))
ok("failed save: al_guardar not called", identical(G$tras_fallo, 0L))
ok("... the retry that saves calls it once", length(G$llamadas) == 1L && G$guardados == 1L)

# 6. al_guardar fails: the provision stays saved, the session goes on
correr(quote({
  session$getReturned()$abrir_provision_manual(PREFILL, function(pid) stop("Requisiciones down"))
  llenar(session); session$setInputs(ppm_save = 1); session$flushReact()
}))
ok("al_guardar fails: the provision is saved anyway", G$guardados == 1L && length(nuevo_id()) == 1L)

# 7. An empresa that cannot be matched: none selected, a notice, and no save without one
correr(quote({
  p <- PREFILL; p$empresa <- "ZZZ"
  session$getReturned()$abrir_provision_manual(p, cb)
  G$h7 <- paste(G$modal, collapse = ""); G$av7 <- G$avisos
  llenar(session, empresa = ""); session$setInputs(ppm_save = 1); session$flushReact()
}))
ok("unknown empresa: the empty choice is selected, no company is",
   grepl('<option value="" selected>', G$h7, fixed = TRUE) && !grepl('Networks Transport" selected', G$h7, fixed = TRUE))
ok("unknown empresa: a notice asks to choose it", any(grepl("^warning:", G$av7)))
ok("unknown empresa: saving without one is refused, al_guardar not called",
   G$guardados == 0L && !length(G$llamadas) && any(grepl("^error:", G$avisos)))

# 8. Without permission: no modal
PERMITIDO <- FALSE
correr(quote(G$r8 <- session$getReturned()$abrir_provision_manual(PREFILL, cb)))
PERMITIDO <- TRUE
ok("without can_edit_provisiones: abrir_provision_manual opens nothing", isFALSE(G$r8) && is.null(G$modal))

# 9. The clip on orphan rows (D's orphan provision is p0)
con_clip <- function(ids) data.frame(provision_id = intersect(ids, "p0"), total = rep(3L, length(intersect(ids, "p0"))))
sh <- nuevo_shared(con_clip)
correr(quote({ f <- session$getReturned()$filas(); G$f9 <- f }), shared = sh)
ok("clip: the orphan row carries req_clip's total", identical(G$f9$.adjuntos[G$f9$id == "orphan_p0"], 3L))
ok("clip: every other row 0", all(G$f9$.adjuntos[G$f9$id != "orphan_p0"] == 0L))
huerfanas <- sort(D$provisions$id[is.na(D$provisions$liability_id) & D$provisions$estado %in% "provisional"])
ok("clip: req_clip got only the orphan provision ids, in one call",
   G$clip_llamadas == 1L && identical(sort(G$clip_ids), huerfanas))

# 10. Without permission (0 rows): no clip
correr(quote(G$f10 <- session$getReturned()$filas()))
ok("no permission (req_clip gives 0 rows): no row has a clip", all(G$f10$.adjuntos == 0L))

# 11. A refresh that does not change the orphan ids does not read again
sh <- nuevo_shared(con_clip)
correr(quote({
  G$f_antes <- session$getReturned()$filas(); n0 <- G$clip_llamadas
  # A real refresh: a scheduled (non-orphan) provision's amount changes, so
  # the table is rebuilt, but the set of orphan provisions stays the same.
  p <- D$provisions; k <- which(!is.na(p$liability_id))[1]; p$amount_pago[k] <- p$amount_pago[k] + 1
  sh$pasivos_provisions_db(p)
  session$flushReact(); f <- session$getReturned()$filas()
  G$reconstruida <- !identical(f, G$f_antes)
  G$n_refresco <- G$clip_llamadas - n0
  sh$req_version(1L); session$flushReact(); invisible(session$getReturned()$filas())
  G$n_version <- G$clip_llamadas - n0
}), shared = sh)
ok("a real refresh (the table was rebuilt) with the same orphan ids: req_clip not called again",
   isTRUE(G$reconstruida) && identical(G$n_refresco, 0L))
ok("req_version() moving: req_clip called again, once", identical(G$n_version, 1L))

# 12. The clip opens the row's provision; a forged id opens nothing
sh <- nuevo_shared(con_clip)
correr(quote({
  session$setInputs(`tabla-accion` = list(accion = "adjuntos", ids = "orphan_p0", nonce = 1)); session$flushReact()
  session$setInputs(`tabla-accion` = list(accion = "adjuntos", ids = "orphan_zzz", nonce = 2)); session$flushReact()
  session$setInputs(`tabla-accion` = list(accion = "adjuntos", ids = D$liabilities$id[1], nonce = 3)); session$flushReact()
}), shared = sh)
ok("clip click opens the row's provision; forged ids open nothing", identical(G$abiertas, "p0"))

suppressMessages({ untrace("showModal", where = asNamespace("shiny")); untrace("showNotification", where = asNamespace("shiny")) })
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
