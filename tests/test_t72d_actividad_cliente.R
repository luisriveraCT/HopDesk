# =============================================================================
# tests/test_t72d_actividad_cliente.R
# T72-D: a client never reads the write layer's technical detail in Actividad.
#   1  an entry WITH an event: its description is the event's own, with no
#      " [tabla=... filas a -> b]" suffix; the table and the counts stay in
#      metadata (tabla, filas_antes, filas_despues);
#   2  the real auditLogViewerServer(), own_client mode: a client user sees
#      no T72 keys (tabla, filas_antes, filas_despues, diferencia, origen) in
#      the metadata visible_fetched()/filtered() hand to the detail and the
#      PDF, and no old-style suffix at the end of a description; a module's
#      own entry with no T72 mark is untouched (a "tabla" key alone included);
#   3  staff sees everything as written.
# Run from project root:  Rscript tests/test_t72d_actividad_cliente.R
# Contraprueba: on 2377da3, 1 and 2 fail.
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t72d_actividad_cliente ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressWarnings(suppressMessages(expr))

# ── 1. The entry the write layer leaves ────────────────────────────────────
source("tests/helpers/s3_memoria.R")
L <- new.env(); L$x <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$x[[length(L$x) + 1L]] <- list(description = description, metadata = metadata); invisible(TRUE)
}
s3_memoria_reset()
quieto(.s3_write(tibble(id = c("a", "b")), "t72d_tabla.rds", client_id = "acme",
                 evento = list(usuario = "ana", modulo = "pruebas", accion = "guardar", descripcion = "Guardó la tabla")))
e <- if (length(L$x)) L$x[[1]] else NULL
ok("1 an entry with an event: the description is the event's own, no [tabla=...] suffix",
   !is.null(e) && identical(e$description, "Guardó la tabla"))
ok("1 ... the table and the counts stay in metadata",
   !is.null(e) && identical(e$metadata$tabla, "t72d_tabla.rds") && identical(e$metadata$filas_despues, 2L) && "filas_antes" %in% names(e$metadata))
s3_memoria_restaurar()
rm(log_action)

# ── 2/3. The viewer ────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
suppressMessages(source("R/i18n.R"))
suppressMessages(source("R/app_audit.R"))
suppressMessages(source(Sys.getenv("T72_VISOR_R", "R/audit_log_viewer_module.R")))
utils::assignInNamespace("later", function(f, delay = 0) invisible(NULL), "later")
.audit_viewer_resolve_perms <- function(shared) list(can_view_client_audit_logs = TRUE, can_view_staff_audit_log = FALSE)
hd_table_server <- function(...) list(accion = reactive(NULL))
col_fecha_hora <- col_texto <- function(...) list()
s3_memoria_reset()
ahora <- Sys.time()
fila <- function(module, action, desc, meta, user = "sistema:forecasting", hace = 10) {
  log_action(user = user, module = module, action = action, description = desc, metadata = meta,
             client_id = "networks", viewer_home_client_id = "networks")
  p <- .audit_pending[["networks"]]; p$ts[nrow(p)] <- ahora - hace * 60; .audit_pending[["networks"]] <- p
}
fila("forecasting", "sembrar_catalogo", "Catálogo de pronósticos: se agregaron métricas nuevas de la siembra [tabla=forecasting_metrics.rds filas 13 -> 14]",
     list(tabla = "forecasting_metrics.rds", filas_antes = 13L, filas_despues = 14L, origen = "forecasting_completar_catalogo",
          diferencia = list(n = c(nuevas = 1L, cambiadas = 0L, quitadas = 0L)), n_metricas = 1L))
fila("evidencias", "agregar_tabla_evidencia", "Agregó una tabla", list(tabla = "Cotización", n = 3L, duration_seconds = 42L), user = "ana", hace = 5)
fila("bancos", "eliminar_movimientos/parte", "", list(tabla = "bancos_papelera.rds", filas_antes = 0L, filas_despues = 1L), user = "luis", hace = 4)   # T72-E
invisible(.audit_flush_pending("networks"))

ver <- function(is_staff) {
  shared <- list(
    current_user_info   = function() list(user = if (is_staff) "mouse" else "ana", client_id = if (is_staff) "hopdesk" else "networks", is_staff = is_staff),
    home_client_id      = function() if (is_staff) "hopdesk" else "networks",
    effective_client_id = function() "networks")
  G <- new.env()
  quieto(testServer(auditLogViewerServer, args = list(shared = shared, mode = "own_client"), {
    session$flushReact(); G$vis <- visible_fetched(); G$fil <- filtered()
  }))
  G
}
meta_de <- function(df, accion) jsonlite::fromJSON(df$metadata[df$action == accion][1], simplifyVector = TRUE)
T72 <- c("tabla", "filas_antes", "filas_despues", "diferencia", "origen")

cl <- ver(FALSE)
m_cl <- tryCatch(meta_de(cl$fil, "sembrar_catalogo"), error = function(e) NULL)
ok("2 a client: no T72 key in the metadata the detail and the PDF read (filtered())",
   is.data.frame(cl$fil) && "sembrar_catalogo" %in% cl$fil$action && !any(T72 %in% names(m_cl %||% list(tabla = 1))))
ok("2 ... and no [tabla=...] suffix at the end of its description",
   identical(cl$fil$description[cl$fil$action == "sembrar_catalogo"], "Catálogo de pronósticos: se agregaron métricas nuevas de la siembra"))
m_ev <- tryCatch(meta_de(cl$fil, "agregar_tabla_evidencia"), error = function(e) NULL)
ok("2 ... a T72 entry's other keys stay (n_metricas)", identical(as.integer(m_cl$n_metricas), 1L))
ok("2 a module's own entry with no T72 mark is untouched (its own \"tabla\" key and duration_seconds included)",
   identical(m_ev$tabla, "Cotización") && identical(as.integer(m_ev$n), 3L) && identical(as.integer(m_ev$duration_seconds), 42L) &&
     identical(cl$fil$description[cl$fil$action == "agregar_tabla_evidencia"], "Agregó una tabla"))

st <- ver(TRUE)
m_st <- tryCatch(meta_de(st$fil, "sembrar_catalogo"), error = function(e) NULL)
ok("2 T72-E: a client does not see an action's second write (\"<accion>/parte\")", is.data.frame(cl$fil) && !any(grepl("/parte$", cl$fil$action)))
ok("3 T72-E: staff sees it", is.data.frame(st$fil) && "eliminar_movimientos/parte" %in% st$fil$action)
ok("3 staff sees the T72 keys and the description as written",
   all(c("tabla", "filas_antes", "filas_despues", "origen") %in% names(m_st %||% list())) &&
     grepl("[tabla=forecasting_metrics.rds filas 13 -> 14]", st$fil$description[st$fil$action == "sembrar_catalogo"], fixed = TRUE))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
