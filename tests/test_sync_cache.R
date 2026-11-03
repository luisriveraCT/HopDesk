# =============================================================================
# tests/test_sync_cache.R
# CM09, finding 1 (2026-11-03): load_moves() and load_interco_v2() answer
# from .app_data_cache before S3, and only this process's own saves keep that
# cache current. A sync-bus reload cleared the preload cache but not this
# one, so a write from another process or an offline-queue replay never
# reached the sessions here. The reload now drops the table's entry first.
#
# Run from project root:  Rscript tests/test_sync_cache.R
# =============================================================================
cat("=== test_sync_cache ===\n\n")
source("tests/helpers/s3_memoria.R")
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key"))
    eval(e, globalenv())
for (e in parse("R/sync_bus.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".SYNC_CACHE_KEYS", ".sync_olvidar_cache"))
    eval(e, globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# 1. Every cache-first loader has its entry, and every entry is a synced table
codigo <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
usos <- unlist(lapply(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), function(f)
  regmatches(codigo(f), regexpr('\\.cache_get\\("[a-z_0-9]+"', codigo(f)))))
claves <- unique(sub('^\\.cache_get\\("([a-z_0-9]+)"$', "\\1", usos))
ok(sprintf("found the cache-first loaders (%s)", paste(claves, collapse = ", ")), length(claves) >= 2L)
for (k in claves)
  ok(sprintf("'%s' (read with .cache_get) is in .SYNC_CACHE_KEYS", k), k %in% .SYNC_CACHE_KEYS)
registrados <- regmatches(codigo("app.R"), regexpr('register_synced\\("[a-z_0-9]+"', codigo("app.R")))
registrados <- sub('^register_synced\\("', "", sub('"$', "", registrados))
for (n in names(.SYNC_CACHE_KEYS))
  ok(sprintf("'%s' is a table registered with register_synced()", n), n %in% registrados)

# 2. The reload drops the entry before calling the loader
sb <- paste(readLines("R/sync_bus.R", warn = FALSE), collapse = "\n")
ok("the reload loop calls .sync_olvidar_cache() before entry$loader()",
   regexpr(".sync_olvidar_cache(name", sb, fixed = TRUE) > 0 &&
   regexpr(".sync_olvidar_cache(name", sb, fixed = TRUE) < regexpr("entry$loader(client_id = active_cid)", sb, fixed = TRUE))

# 3. Behaviour: a stale cache entry no longer hides S3
s3_memoria_reset(); .app_data_cache <- list()
mov <- function(docs) .normalize(tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = docs,
                                                FechaVenc_Proyectada = as.Date("2026-11-20")), .schema_moves)
assign(paste0("acme/", S3_KEYS$moves), mov(c("A1", "T1")), envir = .mem)   # T1: written by another process
.cache_set("moves", mov("A1"), "acme")                                      # this process's stale copy
.cache_set("moves", mov("Z9"), "otro")                                      # another tenant's entry
ok("before: load_moves() answers from the stale cache", identical(load_moves("acme")$Documento, "A1"))
ok(".sync_olvidar_cache() drops the entry", isTRUE(.sync_olvidar_cache("moves_db", "acme")))
ok("after: load_moves() reads S3 and sees the other process's T1",
   identical(sort(load_moves("acme")$Documento), c("A1", "T1")))
ok("another tenant's cache entry is untouched", identical(.cache_get("moves", "otro")$Documento, "Z9"))
ok("a table without a cache entry is a no-op", identical(.sync_olvidar_cache("notes_df", "acme"), FALSE))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
