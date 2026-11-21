# =============================================================================
# tests/test_t72_visor_zona.R
# T72: Luis's activity WAS in prod's log (pasivos_audit and app_audit agree day
# by day), but Actividad did not show it. Two suspects, both checked here with
# the real auditLogViewerServer() on the in-memory S3, the process in UTC like
# prod (shinyapps.io: "[ENV] TZ ... ahora=... UTC"):
#   1. A hopdesk staff member jumped into networks reads NETWORKS' log -- not
#      the home client's (hopdesk), not filtered by viewer_home_client_id.
#   2. The range picker sends the browser's wall clock with no zone, and
#      shinyWidgets reads it in the server's zone: in Mexico (UTC-6) the range
#      ended 6 hours before now and hid what was just done. The viewer now
#      reads that wall clock in the browser's zone (input$client_tz).
# Run from project root:  Rscript tests/test_t72_visor_zona.R
# Contraprueba: with the viewer of 29d829e (T72_VISOR_R=<that file>), 1 passes
# and 2 fails -- suspect 1 was not it, suspect 2 is.
# =============================================================================
Sys.setenv(TZ = "UTC")   # prod's zone, before anything reads the clock
cat("=== test_t72_visor_zona ===\n\n")
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
orig_later <- later::later
utils::assignInNamespace("later", function(f, delay = 0) invisible(NULL), "later")
# The viewer's own permission lookup reads usuarios.rds; a hopdesk staff
# member has can_view_client_audit_logs (R/tier_registry.R).
.audit_viewer_resolve_perms <- function(shared) list(can_view_client_audit_logs = TRUE, can_view_staff_audit_log = FALSE)
# The table engine is not under test: what the viewer fetches is.
hd_table_server <- function(...) list(accion = reactive(NULL))
col_fecha_hora <- col_texto <- function(...) list()

MX <- "America/Mexico_City"
# What shinyWidgets' air.datetime handler hands the server: the browser's wall
# clock, parsed in the server's zone (tz = "").
del_navegador <- function(t, tz = MX) as.POSIXct(format(t, "%Y-%m-%d %H:%M:%S", tz = tz), tz = "")

s3_memoria_reset()
ahora <- Sys.time()
fila <- function(cid, accion, hace_min) {
  log_action(user = "luis", module = "pasivos", action = accion, description = "d",
             client_id = cid, viewer_home_client_id = "hopdesk")
  p <- .audit_pending[[cid]]; p$ts[nrow(p)] <- ahora - hace_min * 60; .audit_pending[[cid]] <- p
}
fila("networks", "liability.created", 30)     # Luis, half an hour ago
fila("networks", "liability.edited", 3 * 24 * 60)
fila("hopdesk",  "client_access",    60)
invisible(.audit_flush_pending("networks")); invisible(.audit_flush_pending("hopdesk"))

# 0. The helper on its own
x <- del_navegador(as.POSIXct("2026-11-20 16:00:00", tz = "UTC"))   # 10:00 in Mexico
ok("0 the picker's 10:00 (Mexico) is read as 16:00 UTC, the instant it means",
   exists(".audit_hora_navegador") &&
     abs(as.numeric(difftime(.audit_hora_navegador(x, MX), as.POSIXct("2026-11-20 16:00:00", tz = "UTC"), units = "secs"))) < 1)
ok("0 no zone known (or one that does not exist): it stays as it came",
   exists(".audit_hora_navegador") &&
     abs(as.numeric(difftime(.audit_hora_navegador(x, NULL), x, units = "secs"))) < 1 &&
     abs(as.numeric(difftime(.audit_hora_navegador(x, "Marte/Olympus"), x, units = "secs"))) < 1)

shared <- list(
  current_user_info   = function() list(user = "mouse", tier = "hopdesk", client_id = "hopdesk", is_staff = TRUE),
  home_client_id      = function() "hopdesk",
  effective_client_id = function() "networks")

quieto <- function(expr) suppressWarnings(suppressMessages(expr))
quieto(testServer(auditLogViewerServer, args = list(shared = shared, mode = "own_client"), {
  session$flushReact()
  df <- fetched()
  # 1. The client it reads
  ok("1 hopdesk staff jumped into networks: no error, it reads networks' log",
     is.data.frame(df) && nrow(df) >= 1L && all(df$client_id == "networks"))
  ok("1 ... with Luis's liability.created of half an hour ago (the default range ends at the server's now)",
     "liability.created" %in% df$action)
  ok("1 ... and none of the home client's (hopdesk) rows", !"client_access" %in% df$action)

  # 2. The browser in Mexico reports the range it shows: the last 7 days up to now
  session$rootScope()$setInputs(client_tz = MX)
  session$setInputs(audit_range = del_navegador(c(ahora - 7 * 86400, ahora)))
  session$flushReact()
  r  <- rango$rango()
  df <- fetched()
  ok("2 the range the browser reports still ends now (it used to end 6 hours earlier, in UTC)",
     abs(as.numeric(difftime(r[2], ahora, units = "mins"))) < 2)
  ok("2 ... so Luis's action of half an hour ago is shown", is.data.frame(df) && "liability.created" %in% df$action)
  ok("2 ... and the one of 3 days ago too", is.data.frame(df) && "liability.edited" %in% df$action)
}))

utils::assignInNamespace("later", orig_later, "later")
s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
