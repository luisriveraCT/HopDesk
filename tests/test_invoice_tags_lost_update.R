# =============================================================================
# tests/test_invoice_tags_lost_update.R
# Luis, smoke test of T04 (2026-10-31): "le puse etiqueta a una serie de
# facturas, luego las quité todas, luego puse solo una y parece que volvieron
# a ponerse en automático las que ya había quitado".
#
# handle_invoice_action() (Vencidos and the lupa) saved the new tags to S3
# but never put them in the session's shared$tags_db(); the session only
# caught up when the sync bus polled (every 8 s). A second tag action inside
# that window started from the stale tags and wrote them back: tags removed
# a moment before came back (a lost update, the same class as the moves bug).
# The Calendario's own tag handler (ledger_module.R) always set tags_db first.
#
# Across sessions (the coordinator, after the smoke test): both handlers now
# start from the tags as S3 has them (etiquetas_frescas()), not from the
# session's copy, so one session's tag never undoes another's from the last
# seconds. If S3 can't be read they use the session's copy -- never an empty
# table.
#
# The real handle_invoice_action() and set_invoice_tags(), with S3, the sync
# bus and the audit log stubbed (this is its own Rscript: nothing is patched
# in a live session).
#
# Run from project root:  Rscript tests/test_invoice_tags_lost_update.R
# =============================================================================
cat("=== test_invoice_tags_lost_update ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
cargar("R/persistence.R", c("set_invoice_tags", ".normalize", ".schema_tags"))
cargar("R/search_module.R", c("handle_invoice_action", "etiquetas_frescas"))

# ── Stubs: S3 is a variable; the sync bus and the log do nothing ───────────
s3_tags <- NULL
s3_cae <- FALSE   # TRUE: reading S3 fails
S3_KEYS <- list(tags = "invoice_tags.rds")
.s3_key <- function(key, client_id = NULL) paste0(client_id, "/", key)
.s3_bucket <- function() "dev"
.s3_read_raw_with_retry <- function(full_key, bucket, ...)
  if (s3_cae || is.null(s3_tags)) list(ok = FALSE) else list(ok = TRUE, value = s3_tags)
save_tags <- function(df, client_id = NULL) { s3_tags <<- df; invisible(TRUE) }
s3_save_with_retry <- function(f) { f(); list(ok = TRUE) }
bump_sync_version <- function(...) invisible(TRUE)
log_action <- function(...) invisible(NULL)
showNotification <- function(...) invisible(NULL)

fila <- function(doc) list(ledger = "AR", empresa = "NTS", moneda = "MXN", documento = doc, source = "sap",
                           inv_id = "", provision_id = "", parte = "ACME", codigo = "", importe = 100,
                           fecha = "01/10/2026", tipo = "Cobro")
etiquetados <- function(df) if (is.null(df) || !nrow(df)) character(0) else sort(unique(df$Documento))

# What the session had loaded: 1001 and 1002 tagged A (urgent).
inicial <- tibble(ledger = "AR", Empresa = "NTS", Moneda = "MXN", Documento = c("1001", "1002"),
                  tag = "urgent", tagged_by = "luis", tagged_at = Sys.time())
sesion <- function(tags) list(tags_db = reactiveVal(tags), current_user = function() "luis",
                             effective_client_id = function() "dev", home_client_id = function() "dev",
                             empresa_sel = function() "NTS")
s3_tags <- inicial
shared <- sesion(inicial)

isolate({
  # Quitar etiqueta on both, then -- before any sync poll -- Etiqueta A on one other.
  handle_invoice_action(list(action = "tag_clear", rows = list(fila("1001"), fila("1002"))), shared)
  ok("after 'Quitar etiqueta', S3 has no tags", identical(etiquetados(s3_tags), character(0)))
  ok("... and the session's tags_db already shows that (the table repaints from it)",
     identical(etiquetados(shared$tags_db()), character(0)))
  handle_invoice_action(list(action = "tag_urgent", rows = list(fila("1005"))), shared)
  ok("then 'Etiqueta A' on one invoice saves only that one: the removed tags don't come back",
     identical(etiquetados(s3_tags), "1005"))
  ok("... and the session sees the same as S3", identical(etiquetados(shared$tags_db()), "1005"))

  # Two sessions loaded the same tags; A tags 3001, then B -- whose copy
  # hasn't caught up yet -- tags 3002.
  s3_tags <<- inicial
  a <- sesion(inicial); b <- sesion(inicial)
  handle_invoice_action(list(action = "tag_urgent", rows = list(fila("3001"))), a)
  handle_invoice_action(list(action = "tag_important", rows = list(fila("3002"))), b)
  ok("two sessions: B's tag doesn't undo A's from a moment before (S3 keeps both)",
     identical(etiquetados(s3_tags), c("1001", "1002", "3001", "3002")))
  ok("... and B now sees A's tag too", "3001" %in% etiquetados(b$tags_db()))

  # S3 can't be read: start from the session's copy, never from nothing.
  s3_cae <<- TRUE
  handle_invoice_action(list(action = "tag_urgent", rows = list(fila("4001"))), b)
  s3_cae <<- FALSE
  ok("S3 unreadable: the session's copy is the base, no tag is wiped",
     identical(etiquetados(s3_tags), c("1001", "1002", "3001", "3002", "4001")))
})

# The Calendario's tag handler starts from S3 too.
led <- paste(readLines("R/ledger_module.R", warn = FALSE), collapse = "\n")
ok("the Calendario's tag buttons start from etiquetas_frescas(), then show it in the session",
   grepl("tags_db <- etiquetas_frescas(shared)", led, fixed = TRUE) &&
     grepl("shared$tags_db(tags_db)\n      tag_result <- s3_save_with_retry", led, fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
