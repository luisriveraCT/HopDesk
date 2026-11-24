# =============================================================================
# tests/test_t85_quita.R
# T85 -- money. The write net ([S3_RED], CM09) refused a save_manual() that
# empties manual_invoices or takes more than half of it (4+ rows). Confirmar
# had already archived the invoice and saved the confirmation, so the manual
# row stayed: confirming the last manual invoice (or most of them) left it
# open, able to count again. Same for deleting from the ledger or the search
# and reverting a provision.
# Now save_manual(quita = ids): the rows that leave S3's current table must
# all be named (else [S3_RED]), and then the shrink is allowed. Without quita
# the net is whole. In-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_t85_quita.R
# =============================================================================
cat("=== test_t85_quita ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
CID <- "acme"; k <- paste0(CID, "/", S3_KEYS$manual)
fila <- function(id) { x <- .schema_manual()[0, ]; x[1, "id"] <- id; x }
tabla <- function(n) do.call(rbind, lapply(seq_len(n), function(i) fila(paste0("m", i))))
en_s3 <- function() sort(as.character(get(k, envir = .mem)$id))
# Seeds n rows, removes `quitar` from the session copy, saves; the error text or "ok".
guardar <- function(n, quitar, quita = NULL, df = NULL, evento = NULL) {
  s3_memoria_reset(); base <- tabla(n); assign(k, base, envir = .mem)
  df <- df %||% base[!base$id %in% quitar, , drop = FALSE]
  args <- list(df, client_id = CID, base = base)
  if (!is.null(quita))  args$quita  <- quita
  if (!is.null(evento)) args$evento <- evento
  tryCatch({ suppressMessages(do.call(save_manual, args)); "ok" }, error = function(e) conditionMessage(e))
}
frenado <- function(r) startsWith(r, "[S3_RED]")

# 1. The cases that were refused now save, with quita
ok("1a the only manual invoice, named: saved, S3 empty",
   guardar(1, "m1", quita = "m1") == "ok" && !length(en_s3()))
ok("1b both of 2, named: saved, S3 empty",
   guardar(2, c("m1", "m2"), quita = c("m1", "m2")) == "ok" && !length(en_s3()))
ok("1c 3 of 5 (more than half), named: saved, 2 left",
   guardar(5, c("m1", "m2", "m3"), quita = c("m1", "m2", "m3")) == "ok" && identical(en_s3(), c("m4", "m5")))
ok("1d with an event (Confirmar's /parte): saved too",
   guardar(1, "m1", quita = "m1", evento = list(.parte_de = "confirmar_pago")) == "ok" && !length(en_s3()))

# 2. Without quita the net is whole (as before)
ok("2a the only one, not named: still refused, the row stays",
   frenado(guardar(1, "m1")) && identical(en_s3(), "m1"))
ok("2b 3 of 5 not named: still refused", frenado(guardar(5, c("m1", "m2", "m3"))) && length(en_s3()) == 5L)
ok("2c a small removal never needed quita (1 of 3)", guardar(3, "m1") == "ok" && identical(en_s3(), c("m2", "m3")))

# 3. quita allows only what it names
ok("3a an empty table by mistake (df empty, base full) naming one id: refused, nothing lost",
   frenado(guardar(3, character(), quita = "m1", df = tabla(3)[0, ])) && length(en_s3()) == 3L)
ok("3b ... the refusal names the rows it would have lost",
   grepl("did not name: m2, m3", guardar(3, character(), quita = "m1", df = tabla(3)[0, ]), fixed = TRUE))
ok("3c quita = an id that is not there: nothing else leaves, the save goes on",
   guardar(3, "m1", quita = c("m1", "m9")) == "ok" && identical(en_s3(), c("m2", "m3")))
ok("3d NA in quita is ignored", guardar(1, "m1", quita = c(NA, "m1")) == "ok" && !length(en_s3()))

# 4. Another session's row added meanwhile is kept (the rebase), even with quita
ok("4 a row another session added is kept while the named one leaves", {
  s3_memoria_reset(); base <- tabla(1); assign(k, rbind(base, fila("otra")), envir = .mem)
  r <- tryCatch({ suppressMessages(save_manual(base[0, ], client_id = CID, base = base, quita = "m1")); "ok" },
                error = function(e) conditionMessage(e))
  r == "ok" && identical(en_s3(), "otra")
})

# 5. Confirmar: when the archived invoices' save failed, the provisions' save
#    removes them too -- it names both (c(manual_ids, archived_mi_ids))
ok("5 the provisions' save after a failed first save: both sets named, both leave", {
  s3_memoria_reset(); base <- tabla(2); assign(k, base, envir = .mem)   # m1 archived (save failed), m2 provision
  r <- tryCatch({ suppressMessages(save_manual(base[0, ], client_id = CID, base = base, quita = c("m2", "m1"))); "ok" },
                error = function(e) conditionMessage(e))
  r == "ok" && !length(en_s3())
})

# 6. Static guardian: the 9 removal paths name what they remove
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
quitas <- function(f) sum(grepl("save_manual\\(.*quita = ", fuente(f)))
esperado <- c("R/pagar_hoy_module.R" = 5L, "R/ledger_module.R" = 1L, "R/search_module.R" = 1L, "R/pasivos_module.R" = 2L)
for (f in names(esperado))
  ok(sprintf("6 %s: its %d removal path(s) pass quita", f, esperado[[f]]), quitas(f) == esperado[[f]])
ok("6 Confirmar's provisions saves name the archived ids too (AP and AR)",
   sum(grepl("quita = c(manual_ids, archived_mi_ids)", fuente("R/pagar_hoy_module.R"), fixed = TRUE)) == 2L)
ok("6 archived_mi_ids exists before the provisions' block (AP and AR)",
   sum(grepl("archived_mi_ids <- character(0)   # T85", fuente("R/pagar_hoy_module.R"), fixed = TRUE)) == 2L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
