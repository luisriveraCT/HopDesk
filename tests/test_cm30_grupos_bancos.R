# =============================================================================
# tests/test_cm30_grupos_bancos.R
# CM30. Two files kept company initials outside the rename cascade and
# nothing used them: bancos.rds (never written, read only by the old PPL
# origin-account fallback, without a client_id; it exists in no client) and
# grupos.rds `companies` (never read; which group a company belongs to is
# empresas.group_id -- checked in prod: the lists matched it). They go:
#   1  no load_bancos / save_bancos / .schema_bancos / grupo_companies left,
#      no S3_KEYS$bancos and no "bancos.rds" in R/ or app.R; .schema_grupos
#      has no companies;
#   2  saving and loading grupos still works over the in-memory S3; a file
#      that still has `companies` loads without it, and its first write
#      (the client_id backfill) keeps every group, id and name.
#
# Run from project root:  Rscript tests/test_cm30_grupos_bancos.R
# =============================================================================
cat("=== test_cm30_grupos_bancos ===\n\n")
source("tests/helpers/s3_memoria.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
bump_sync_version <- function(...) invisible(TRUE)

# ── 1. Gone ──────────────────────────────────────────────────────────────────
archivos <- c(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), "app.R")
lineas <- unlist(lapply(archivos, function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8"); paste0(f, ":", seq_along(x), ": ", x)
}))
codigo <- lineas[!grepl("^[^:]+:[0-9]+: *#", lineas)]   # comments may still tell the story
quedan <- function(pat) grep(pat, codigo, value = TRUE, perl = TRUE)
ok("1 no load_bancos(), save_bancos() or .schema_bancos left in R/ or app.R",
   { q <- quedan("\\b(load_bancos|save_bancos|\\.schema_bancos)\\b(?!_)"); if (length(q)) message(paste(q, collapse = "\n")); !length(q) })
ok("1 no grupo_companies() and no `$companies` of a group read anywhere",
   { q <- quedan("grupo_companies|(grp|grupo|grupos)[A-Za-z_]*\\$companies"); if (length(q)) message(paste(q, collapse = "\n")); !length(q) })
ok("1 no S3_KEYS$bancos and no \"bancos.rds\"",
   { q <- quedan('S3_KEYS\\$bancos\\b(?!_)|S3_KEYS\\[\\["bancos"\\]\\]|"bancos\\.rds"'); if (length(q)) message(paste(q, collapse = "\n")); !length(q) })
ok("1 S3_KEYS (R/global.R) has no `bancos` entry", !"bancos" %in% names(S3_KEYS) && "bancos_cuentas" %in% names(S3_KEYS))
ok("1 .schema_grupos() has no companies column", !"companies" %in% names(.schema_grupos()))

# ── 2. grupos still saves and loads ──────────────────────────────────────────
CID <- "acme"
k <- function(key) paste0(CID, "/", key)
s3_memoria_reset()
g <- tibble::tibble(id = c("g1", "g2"), name = c("Grupo Uno", "Grupo Dos"), client_id = CID,
                    created_at = "2026-01-01", updated_at = "2026-01-01")
save_grupos(g, client_id = CID)
ok("2 save_grupos() then load_grupos(): the same groups", {
  l <- load_grupos(client_id = CID); identical(l$id, g$id) && identical(l$name, g$name) })

# A file written before CM30: companies present, one client_id empty (the
# backfill writes it -- the first write after CM30).
s3_memoria_reset()
viejo <- data.frame(id = c("g1", "g2"), name = c("Grupo Uno", "Grupo Dos"), companies = c('["A","B"]', "[]"),
                    client_id = c(CID, NA), created_at = "2026-01-01", updated_at = "2026-01-01",
                    stringsAsFactors = FALSE)
assign(k(S3_KEYS$grupos), viejo, envir = .mem)
l <- suppressMessages(load_grupos(client_id = CID))
ok("2 an older file loads without companies, every group kept", identical(l$id, c("g1", "g2")) && !"companies" %in% names(l))
s <- get(k(S3_KEYS$grupos), envir = .mem)
ok("2 …its backfill write keeps every id and name, fills client_id, and drops companies",
   identical(s$id, c("g1", "g2")) && identical(s$name, c("Grupo Uno", "Grupo Dos")) &&
     identical(s$client_id, c(CID, CID)) && !"companies" %in% names(s))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
