# =============================================================================
# tests/test_t72b_actividad_cliente.R
# T72 fase B, commit 2: a write that came with no event leaves a
# module="registro", action="sin_evento" entry in app_audit (R/persistence.R).
# It is staff's to chase, never a client's activity: the real
# auditLogViewerServer(), own_client mode, on the in-memory S3 --
#   1  a client user does NOT see sin_evento, on the server, before drawing
#      (visible_fetched(), and filtered(), what the table and the PDF use);
#      every other row stays as it was -- another "registro" action included
#      (only that action is hidden, not the module);
#   2  staff (the viewer's own is_staff question) sees it.
# Run from project root:  Rscript tests/test_t72b_actividad_cliente.R
# Contraprueba: with the viewer of 9a1711e (T72_VISOR_R=<that file>), 1 fails.
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t72b_actividad_cliente ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
suppressMessages(source("R/app_audit.R"))
suppressMessages(source(Sys.getenv("T72_VISOR_R", "R/audit_log_viewer_module.R")))
utils::assignInNamespace("later", function(f, delay = 0) invisible(NULL), "later")
.audit_viewer_resolve_perms <- function(shared) list(can_view_client_audit_logs = TRUE, can_view_staff_audit_log = FALSE)
hd_table_server <- function(...) list(accion = reactive(NULL))
col_fecha_hora <- col_texto <- function(...) list()

s3_memoria_reset()
ahora <- Sys.time()
fila <- function(module, action, hace_min, user = "ana") {
  log_action(user = user, module = module, action = action, description = "d",
             client_id = "networks", viewer_home_client_id = "networks")
  p <- .audit_pending[["networks"]]; p$ts[nrow(p)] <- ahora - hace_min * 60; .audit_pending[["networks"]] <- p
}
fila("pasivos",  "liability.created", 30)
fila("registro", "sin_evento",        25, user = "sistema")
fila("registro", "otra_accion",       20, user = "sistema")
fila("bancos",   "reconcile",         10)
invisible(.audit_flush_pending("networks"))

ver <- function(is_staff) {
  shared <- list(
    current_user_info   = function() list(user = if (is_staff) "mouse" else "ana", tier = if (is_staff) "hopdesk" else "finance",
                                          client_id = if (is_staff) "hopdesk" else "networks", is_staff = is_staff),
    home_client_id      = function() if (is_staff) "hopdesk" else "networks",
    effective_client_id = function() "networks")
  G <- new.env()
  suppressWarnings(suppressMessages(testServer(auditLogViewerServer, args = list(shared = shared, mode = "own_client"), {
    session$flushReact()
    G$todo <- fetched(); G$vis <- visible_fetched(); G$fil <- filtered()
  })))
  G
}

# ── 1. A client user ───────────────────────────────────────────────────────
cl <- ver(FALSE)
ok("1 the log has the sin_evento row (the fetch is not filtered)", is.data.frame(cl$todo) && "sin_evento" %in% cl$todo$action)
ok("1 a client user does not see sin_evento (visible_fetched)", is.data.frame(cl$vis) && !"sin_evento" %in% cl$vis$action)
ok("1 ... nor in filtered(), what the table and the PDF draw", is.data.frame(cl$fil) && !"sin_evento" %in% cl$fil$action)
ok("1 every other row stays, another \"registro\" action included",
   is.data.frame(cl$vis) && setequal(cl$vis$action, c("liability.created", "otra_accion", "reconcile")) && nrow(cl$vis) == 3L)

# ── 2. Staff ───────────────────────────────────────────────────────────────
st <- ver(TRUE)
ok("2 staff sees sin_evento, and all 4 rows", is.data.frame(st$vis) && "sin_evento" %in% st$vis$action && nrow(st$vis) == 4L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
