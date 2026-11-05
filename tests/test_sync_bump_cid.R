# =============================================================================
# tests/test_sync_bump_cid.R
# CM16 (2026-11-04): ~30 bump_sync_version("...") calls passed no client_id.
# In the shared deployment (CLIENT_ID = hd-admin) .s3_key() refuses to guess
# a tenant for sync_versions.rds, so the version stamp was never written and
# other sessions did not see the change until they reloaded (production log:
# "version stamp write failed: ... refusing to guess a tenant"). Every call
# must name its client.
#
# Static guard: parses app.R and every R/ file, walks the calls, and fails on
# a bump_sync_version() without a client_id argument (by name, or a second
# positional argument). The definition itself (R/sync_bus.R) is not a call.
#
# Run from project root:  Rscript tests/test_sync_bump_cid.R
# =============================================================================
cat("=== test_sync_bump_cid ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Every bump_sync_version() call in `f`: list(linea, con). Each call's text is
# taken from the parse data (the call's own expression) and parsed alone.
llamadas <- function(f) {
  datos <- utils::getParseData(parse(f, keep.source = TRUE))
  ids   <- datos$parent[datos$token == "SYMBOL_FUNCTION_CALL" & datos$text == "bump_sync_version"]
  lapply(ids, function(id) {
    expr_id <- datos$parent[datos$id == id]            # SYMBOL_FUNCTION_CALL -> expr -> the call
    texto   <- utils::getParseText(datos, expr_id)
    e       <- str2lang(texto)
    nm      <- names(as.list(e)[-1]) %||% rep("", length(e) - 1L)
    list(linea = datos$line1[datos$id == expr_id],
         con   = "client_id" %in% nm || sum(!nzchar(nm)) >= 2L)
  })
}

archivos <- c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
todas <- list(); sin <- character()
for (f in archivos) {
  ll <- llamadas(f)
  for (l in ll) {
    todas[[length(todas) + 1L]] <- l
    if (!l$con) sin <- c(sin, sprintf("%s:%s", f, l$linea %||% "?"))
  }
}
cat(sprintf("  bump_sync_version() calls: %d\n", length(todas)))
for (s in sin) message("  without client_id: ", s)
ok("found the bump_sync_version() calls (the guard sees them)", length(todas) >= 30L)
ok("every bump_sync_version() call passes client_id", !length(sin))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
