# =============================================================================
# tests/test_t74_falla_visible.R
#
# T74 sleep proposal, common part: with the offline queue off on Connect
# Cloud (HOPDESK_HOST=connect_cloud), the only sign of a failed S3 write is
# the error .s3_write() raises. These are the user-triggered saves whose
# callers used to swallow it (inventory: tablas/reportes/
# T74_sueno_falla_visible_inventario.md). Each must now reach the user.
#
# Parse-based, not a text window: for each site, find the tryCatch() (or
# .bnc_guardar()) whose guarded expression calls the save, and check what its
# error handler does. A comment can't move anything out of view.
# Mutants: each check is run once more against a copy of the code with that
# site put back the way it was, and must fail there.
# Standalone: Rscript --vanilla tests/test_t74_falla_visible.R (repo root).
# =============================================================================
cat("=== test_t74_falla_visible ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Every call in a parsed expression tree, depth first.
.llamadas <- function(x) {
  out <- list()
  visitar <- function(e) {
    if (is.call(e)) {
      out[[length(out) + 1L]] <<- e
      args <- as.list(e)[-1]
      for (i in seq_along(args))   # an empty argument (x[, 1]) is the missing symbol: skip it
        if (!identical(args[[i]], quote(expr = ))) visitar(args[[i]])
    }
    else if (is.expression(e) || is.list(e) || is.pairlist(e)) {   # formals: f(x) has an empty default
      xs <- as.list(e)
      for (i in seq_along(xs)) if (!identical(xs[[i]], quote(expr = ))) visitar(xs[[i]])
    }
  }
  visitar(x); out
}
.nombre <- function(cl) { f <- cl[[1]]; if (is.name(f)) as.character(f) else if (is.call(f) && length(f) == 3L) as.character(f[[3]]) else "" }
.llama_a <- function(e, fn) any(vapply(.llamadas(e), function(cl) identical(.nombre(cl), fn), logical(1)))
.texto <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

# The error handlers of every tryCatch() whose guarded expression calls fn.
.manejadores <- function(codigo, fn) {
  ts <- Filter(function(cl) identical(.nombre(cl), "tryCatch"), .llamadas(codigo))
  ts <- Filter(function(cl) length(cl) >= 2L && .llama_a(cl[[2]], fn), ts)
  lapply(ts, function(cl) cl[["error"]])
}
.avisa <- function(h) !is.null(h) && grepl("showNotification", .texto(h), fixed = TRUE)
.leer <- function(f) parse(f, keep.source = FALSE)

# ── The sites ────────────────────────────────────────────────────────────────
# Each returns TRUE when the site reaches the user. `codigo` is the parsed file.
SITIOS <- list(
  list(id = "E", archivo = "R/search_module.R", que = "Lupa: cancel provision",
       revisar = function(codigo, texto) {
         hs <- .manejadores(codigo, "pasivos_provision_cancel")
         length(hs) >= 1L && all(vapply(hs, function(h) grepl("pids_fallo", .texto(h), fixed = TRUE), logical(1))) &&
           grepl("if (length(pids_fallo)) showNotification(tr(\"comun.error_guardar\")", texto, fixed = TRUE) &&
           grepl("pids_cancel <- setdiff(pids_cancel, pids_fallo)", texto, fixed = TRUE)
       }),
  list(id = "F", archivo = "R/pagar_hoy_module.R", que = "Quitar: revive provision",
       revisar = function(codigo, texto) {
         hs <- .manejadores(codigo, "pasivos_provision_revive")
         # The whole Quitar action sits in an outer tryCatch(error = function(e) e)
         # its caller checks; the revive itself must flag, never swallow.
         traga <- function(h) grepl("^function\\(e\\) (NULL|message\\(.*\\))$", .texto(h))
         any(vapply(hs, function(h) grepl("revive_fallo", .texto(h), fixed = TRUE), logical(1))) &&
           !any(vapply(hs, traga, logical(1))) &&
           grepl("if (revive_fallo) showNotification(tr(\"comun.error_guardar\")", texto, fixed = TRUE)
       }),
  list(id = "G", archivo = "R/bancos_module.R", que = "Bancos: revive/revert provision via .bnc_guardar",
       revisar = function(codigo, texto) {
         bg <- Filter(function(cl) identical(.nombre(cl), ".bnc_guardar"), .llamadas(codigo))
         en_bg <- function(fn) any(vapply(bg, function(cl) .llama_a(cl, fn), logical(1)))
         sueltos <- function(fn) length(.manejadores(codigo, fn)) > 0L
         en_bg("pasivos_provision_revive") && en_bg("pasivos_provision_item_reversed") &&
           !sueltos("pasivos_provision_revive") && !sueltos("pasivos_provision_item_reversed")
       }),
  list(id = "H", archivo = "R/pasivos_observers.R", que = "provision confirmed",
       revisar = function(codigo, texto) { hs <- .manejadores(codigo, "pasivos_provision_item_confirmed")
         length(hs) >= 1L && all(vapply(hs, .avisa, logical(1))) }),
  list(id = "B", archivo = "R/pasivos_module.R", que = "conversion: save_manual stops, save_pagar_hoy tells",
       revisar = function(codigo, texto) {
         grepl("if (!is.null(fallo_manual)) {", texto, fixed = TRUE) &&
           grepl("return(list(ok = FALSE, msg = tr(\"comun.error_guardar\")", texto, fixed = TRUE) &&
           grepl("if (!is.null(fallo_ph)) {", texto, fixed = TRUE) &&
           grepl("showNotification(tr(\"tablas.agenda.error_guardar\"", texto, fixed = TRUE) &&
           !any(vapply(c(.manejadores(codigo, "save_manual"), .manejadores(codigo, "save_pagar_hoy")),
                       function(h) grepl("warning(", .texto(h), fixed = TRUE) && !.avisa(h), logical(1)))
       }),
  list(id = "C", archivo = "R/pasivos_module.R", que = "deletes: papelera and movimientos",
       revisar = function(codigo, texto) {
         hs <- c(.manejadores(codigo, "save_papelera"), .manejadores(codigo, "save_bancos_movimientos"))
         length(hs) >= 3L && all(vapply(hs, function(h) .avisa(h) || !grepl("message(", .texto(h), fixed = TRUE), logical(1)))
       }),
  list(id = "D", archivo = "R/pasivos_wizard_module.R", que = "wizard: finished provisions to papelera",
       revisar = function(codigo, texto) { hs <- .manejadores(codigo, "save_papelera")
         length(hs) >= 1L && all(vapply(hs, function(h) .avisa(h) || !grepl("message(", .texto(h), fixed = TRUE), logical(1))) }),
  list(id = "J", archivo = "R/empresas_module.R", que = "ERP test result",
       revisar = function(codigo, texto) {
         hs <- Filter(function(h) grepl("test-result save failed", .texto(h), fixed = TRUE), .manejadores(codigo, ".erp_mutate_and_save"))
         length(hs) == 1L && .avisa(hs[[1]]) }),
  list(id = "K", archivo = "R/forecasting_module.R", que = "refresh all series",
       revisar = function(codigo, texto) { hs <- .manejadores(codigo, "fcs_fetch_and_store")
         length(hs) >= 1L && any(vapply(hs, function(h) grepl("fallidas", .texto(h), fixed = TRUE), logical(1))) &&
           grepl("if (fallidas > 0L) shiny::showNotification(tr(\"comun.error_guardar\")", texto, fixed = TRUE) }),
  list(id = "I", archivo = "R/cashflow_preview_module.R", que = "typed FX rate",
       revisar = function(codigo, texto) { hs <- .manejadores(codigo, "fx_escrito_guardar")
         length(hs) >= 1L && all(vapply(hs, .avisa, logical(1))) }),
  list(id = "M", archivo = "R/tiers_module.R", que = "user request: notification checked",
       revisar = function(codigo, texto) {
         grepl("enviada <- append_hd_notification(", texto, fixed = TRUE) &&
           grepl("if (!isTRUE(enviada)) {", texto, fixed = TRUE) }),
  list(id = "L", archivo = "R/tiers_module.R", que = "deactivate: username index",
       revisar = function(codigo, texto) { hs <- .manejadores(codigo, "unregister_username")
         length(hs) >= 1L && all(vapply(hs, .avisa, logical(1))) })
)

# Same file text, flattened by deparse so the substring checks don't depend
# on line breaks or indentation.
.plano <- function(codigo) gsub("\\s+", " ", paste(vapply(codigo, .texto, ""), collapse = " "))

cat("Each site reaches the user\n")
for (s in SITIOS) {
  codigo <- .leer(s$archivo)
  ok(sprintf("%s %s (%s)", s$id, s$que, s$archivo), s$revisar(codigo, .plano(codigo)))
}

# ── Mutants: the base (f4a9f60) version of each file must fail ──────────────
cat("\nCounter-proof: each check fails on the base f4a9f60\n")
base <- "f4a9f60"
hay_git <- nzchar(Sys.which("git")) &&
  identical(suppressWarnings(system2("git", c("cat-file", "-t", base), stdout = TRUE, stderr = FALSE)), "commit")
if (!hay_git) {
  cat(" SKIP: git or the base commit is not available here\n")
} else {
  for (s in SITIOS) {
    txt <- suppressWarnings(system2("git", c("show", paste0(base, ":", s$archivo)), stdout = TRUE, stderr = FALSE))
    codigo <- parse(text = txt, keep.source = FALSE)
    ok(sprintf("%s fails on the base", s$id), !isTRUE(tryCatch(s$revisar(codigo, .plano(codigo)), error = function(e) FALSE)))
  }
}

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
