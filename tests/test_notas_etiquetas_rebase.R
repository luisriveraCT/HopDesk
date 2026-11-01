# =============================================================================
# tests/test_notas_etiquetas_rebase.R
# CM09 (2026-11-01):
#   - notes and tags were saved as the session's whole copy; with `base` only
#     this action's rows are applied on S3's table as it is now;
#   - the moves rebase read (.moves_read_live) answered from .app_data_cache,
#     which only this process's own saves keep current, and a failed read
#     fell back to saving the session's whole copy. It is now strict and
#     reads S3 itself.
#
# Run from project root:  Rscript tests/test_notas_etiquetas_rebase.R
# =============================================================================
cat("=== test_notas_etiquetas_rebase ===\n\n")
source("tests/helpers/s3_memoria.R")
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key"))
    eval(e, globalenv())
for (e in parse("R/ledger_module.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".moves_read_live"))
    eval(e, globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

# 1. Notes
NK <- paste0("acme/", S3_KEYS$notes)
nota <- function(ids, t = "x") .normalize(tibble::tibble(id = ids, ledger = "AR", year = 2026L, month = 11L,
                                                        title = t, visibility = "public"), .schema_notes)
s3_memoria_reset(); assign(NK, nota(c("n1", "n2", "n3")), envir = .mem)        # n3: another session
base <- nota(c("n1", "n2"))
q(save_notes(dplyr::bind_rows(base, nota("n4")), client_id = "acme", base = base))
ok("notes: a new note is added and the other session's n3 survives",
   identical(sort(get(NK, envir = .mem)$id), c("n1", "n2", "n3", "n4")))
s3_memoria_reset(); assign(NK, nota(c("n1", "n2", "n3")), envir = .mem)
q(save_notes(dplyr::filter(base, id != "n1"), client_id = "acme", base = base))
ok("notes: a delete removes only that note", identical(sort(get(NK, envir = .mem)$id), c("n2", "n3")))
s3_memoria_reset(); assign(NK, nota(c("n1", "n2")), envir = .mem); .mem_falla <- NK
r <- tryCatch(q(save_notes(nota(c("n1", "n2", "n9")), client_id = "acme", base = nota(c("n1", "n2")))),
              error = function(e) e)
ok("notes: a failed read raises and writes nothing", inherits(r, "error") && .mem_escrituras == 0L)

# 2. Tags (keyed by invoice + tag)
TK <- "acme/invoice_tags.rds"
etq <- function(docs, tag = "urgent", by = "a") .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS",
          Moneda = "MXN", Documento = docs, tag = tag, tagged_by = by), .schema_tags)
ok("setup: the tags key is invoice_tags.rds", identical(S3_KEYS$tags, "invoice_tags.rds"))
s3_memoria_reset(); assign(TK, dplyr::bind_rows(etq(c("F1", "F2")), etq("F3", by = "otra")), envir = .mem)
base <- etq(c("F1", "F2"))
nuevo <- set_invoice_tags(base, "AP", "NCS", "MXN", "F1", character(0))          # quitar etiqueta de F1
nuevo <- set_invoice_tags(nuevo, "AP", "NCS", "MXN", "F2", c("urgent", "important"), tagged_by = "yo")
q(save_tags(nuevo, client_id = "acme", base = base))
s3 <- get(TK, envir = .mem)
ok("tags: F1's tag removed", !"F1" %in% s3$Documento)
ok("tags: F2 now has both tags", setequal(s3$tag[s3$Documento == "F2"], c("urgent", "important")))
ok("tags: the other session's F3 survives", "F3" %in% s3$Documento && !".llave" %in% names(s3))

# 3. The moves rebase read
MK <- "acme/invoice_moves.rds"
mov <- function(docs) .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = docs,
                                                FechaVenc_Proyectada = as.Date("2026-11-20")), .schema_moves)
s3_memoria_reset(); assign(MK, mov(c("A1", "T1")), envir = .mem)
.cache_set("moves", mov("A1"), "acme")                                         # stale process cache
ok("moves: the live read ignores a stale .app_data_cache",
   identical(sort(.moves_read_live("acme")$Documento), c("A1", "T1")))
.mem_falla <- MK
ok("moves: a failed live read raises (the save retries, never writes its copy)",
   inherits(tryCatch(.moves_read_live("acme"), error = function(e) e), "error"))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
