# =============================================================================
# tests/test_s3_parches_guardian.R
# Tests fake S3 through tests/helpers/s3_memoria.R (T30). A test that swaps
# aws.s3's functions by hand can leave one of them real -- and a test that
# loads R/ then reaches the network (it happened: T28's fixture with
# get_bucket; two Bancos tests' put_object on every run, found in T30).
#   1. no file under tests/ swaps aws.s3 by itself (assignInNamespace on
#      "aws.s3", assign into its namespace, mocked bindings of the package)
#      unless it is on the list below, each with its reason;
#   2. the list stays honest: every file on it exists and still does it.
# To fake S3 in a new test: source("tests/helpers/s3_memoria.R"); for a
# refused write, .mem_falla_escritura; for a failed read, .mem_falla.
#
# Run from project root:  Rscript tests/test_s3_parches_guardian.R
# =============================================================================
cat("=== test_s3_parches_guardian ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Files allowed to swap aws.s3 by themselves, and why.
LISTA_BLANCA <- c(
  "helpers/s3_memoria.R"                 = "the in-memory S3 itself",
  "test_s3_memoria.R"                    = "tests the helper: plays a dry-run script that swaps aws.s3",
  "helpers/tiers_fixture_app.R"          = "blocks EVERY aws.s3 function from the network, not only the six (T28)",
  "helpers/ajustes_tablas_fixture_app.R" = "writes every save into the page (#registro) for its Chrome test",
  "helpers/arranque_app.R"               = "T15: the real app against dev, its writes refused in that process",
  "helpers/bancos_fixture_app.R"         = "refused writes driven from its Chrome tests (input fallar_escritura); migrate with a Chrome turn",
  # Group A of reportes/S3_memoria_inventario.md: their own fake S3, without
  # s3_memoria. To migrate, one by one, each giving the same as before.
  "_run_saas.R"                          = "runner: its own store for all its tests (group A)",
  "_run_agenda.R"                        = "runner: its own store (group A)",
  "test_app_audit_no_sobrescribe.R"      = "own fake S3 (group A)",
  "test_auth_lectura_estricta.R"         = "own fake S3 (group A)",
  "test_hopdesk_dev_user_sync.R"         = "own fake S3 for prod and dev buckets (group A)",
  "test_minor_hardening.R"               = "own failing s3readRDS (group A)",
  "test_offline_queue_review_findings.R" = "own fake S3 (group A)",
  "test_offline_queue_stage1.R"          = "own fake S3 (group A)",
  "test_offline_queue_stage2.R"          = "own fake S3 (group A)",
  "test_offline_queue_stage2_audit.R"    = "own fake S3 (group A)",
  "test_s3_read_retry_hardening.R"       = "own failing s3readRDS (group A)",
  "test_s3_red_escritura.R"              = "own fake S3 for the write net (group A)",
  "test_ultimo_dev_vivo.R"               = "own fake S3 (group A)")

# Does this parsed file swap aws.s3 by itself?
es_aws <- function(x) is.character(x) && length(x) == 1L && identical(x, "aws.s3")
toca_aws <- function(x) {
  if (!is.call(x)) return(FALSE)
  f <- if (is.name(x[[1]])) as.character(x[[1]]) else if (is.call(x[[1]]) && identical(x[[1]][[1]], as.name("::")))
    as.character(x[[1]][[3]]) else ""
  args <- as.list(x)[-1]
  # assignInNamespace(name, value, "aws.s3") / ns = "aws.s3"
  if (f == "assignInNamespace" && any(vapply(args, es_aws, logical(1)))) return(TRUE)
  # assign(name, value, envir = asNamespace("aws.s3"))
  if (f == "assign" && any(vapply(args, function(a) is.call(a) && identical(a[[1]], as.name("asNamespace")) &&
                                     length(a) >= 2L && es_aws(a[[2]]), logical(1)))) return(TRUE)
  # testthat's local_mocked_bindings(..., .package = "aws.s3") and friends
  if (grepl("mocked_bindings$", f) && es_aws(args[[".package"]] %||% NULL)) return(TRUE)
  xs <- as.list(x)
  any(vapply(seq_along(xs), function(k) is.call(xs[[k]]) && toca_aws(xs[[k]]), logical(1)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
archivos <- c(list.files("tests", "\\.R$", full.names = TRUE), list.files("tests/helpers", "\\.R$", full.names = TRUE))
nombre <- function(a) sub("^tests/", "", a)
parchan <- character(0); sin_parsear <- character(0)
for (a in archivos) {
  ex <- tryCatch(parse(a, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) { sin_parsear <- c(sin_parsear, nombre(a)); next }
  if (any(vapply(as.list(ex), toca_aws, logical(1)))) parchan <- c(parchan, nombre(a))
}

ok("todos los archivos de tests/ se pueden leer",
   { if (length(sin_parsear)) message("  sin parsear: ", paste(sin_parsear, collapse = ", ")); !length(sin_parsear) })
nuevos <- setdiff(parchan, names(LISTA_BLANCA))
ok("ninguna prueba cambia aws.s3 por su cuenta fuera de la lista blanca (use tests/helpers/s3_memoria.R)",
   { if (length(nuevos)) message("  parchan aws.s3 a mano: ", paste(nuevos, collapse = ", ")); !length(nuevos) })
viejos <- setdiff(names(LISTA_BLANCA), parchan)
ok("la lista blanca está al día: cada archivo existe y todavía parcha aws.s3",
   { if (length(viejos)) message("  ya no parchan (sáquelos de la lista): ", paste(viejos, collapse = ", ")); !length(viejos) })
ok("cada entrada de la lista blanca dice por qué", all(nzchar(LISTA_BLANCA)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
