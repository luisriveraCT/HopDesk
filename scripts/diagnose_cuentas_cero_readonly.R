# =============================================================================
# scripts/diagnose_cuentas_cero_readonly.R
# READ-ONLY. How many supplier accounts may have lost a leading 0 to Excel
# (PENDIENTES §2 "Menores"; paso 0 of the "cuenta sin 0 inicial" task).
#
# For each client, reads the supplier catalog (proveedores.rds and
# proveedores_inactivos.rds) and classifies no_cuenta:
#   - 18 digits, valid CLABE / invalid check digit;
#   - 16-17 digits that become a VALID CLABE with zeros in front (a CLABE
#     that lost its 0: the import repairs these, so here they mean an older
#     import or a manual entry);
#   - 10-13 digits that are BanBajío accounts (banco 030 or "Cuenta BB");
#   - any other all-digit length, by length;
#   - not all digits (abroad, cards with spaces, ...);
# and finds pairs of suppliers whose accounts are the SAME once leading
# zeros are stripped (the 0 made two rows of one account: no_cuenta is the
# catalog's dedup key).
# Prints counts only; an example is the last 4 digits and the supplier id's
# first 8 characters -- never a whole account, a name or an RFC.
#
# Usage (from the repo root, never from a session):
#   Rscript --vanilla scripts/diagnose_cuentas_cero_readonly.R <path to .Renviron> [client_id ...]
# Default client: "networks".
#
# Guarantees no writes: every S3 write path -- the app's and aws.s3's own --
# is replaced by stop() before anything is read. Prints no secret (only the
# bucket name).
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
args    <- commandArgs(trailingOnly = TRUE)
renv    <- args[1]
clientes <- if (length(args) >= 2) args[-1] else "networks"
# One path, or several separated by commas, read in order as the app does
# (e.g. ".Renviron,.Renviron.local" for dev).
renvs <- strsplit(renv %||% NA_character_, ",", fixed = TRUE)[[1]]
if (anyNA(renvs) || !all(file.exists(renvs))) stop("Pass the path(s) to the .Renviron to read.")
for (r in renvs) readRenviron(r)
# DIAG_BUCKET (optional): read another bucket with the same credentials,
# e.g. DIAG_BUCKET=antiguedad-rds-dev (the .Renviron files name prod; dev
# comes from .Renviron.local's rule, which this script does not load).
if (nzchar(Sys.getenv("DIAG_BUCKET"))) Sys.setenv(S3_BUCKET = Sys.getenv("DIAG_BUCKET"))
cat("Bucket:", Sys.getenv("S3_BUCKET"), "| clients:", paste(clientes, collapse = ", "), "\n\n")

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Read-only guard ───────────────────────────────────────────────────────────
.ro <- function(...) stop("READ-ONLY diagnostic: a write was attempted.", call. = FALSE)
for (fn in c("s3saveRDS", "put_object", "delete_object", "s3write_using", "copy_object"))
  if (exists(fn, envir = asNamespace("aws.s3"), inherits = FALSE))
    utils::assignInNamespace(fn, .ro, "aws.s3")

for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
    eval(e, globalenv())
suppressMessages(source("R/persistence.R"))
.s3_write <- .ro
for (fn in ls(globalenv(), pattern = "^(save_|modificar_)")) assign(fn, .ro, envir = globalenv())
# The import's own helpers (prov_clabe_valida, prov_normalizar_encabezado):
# pure functions and constants, no S3.
suppressMessages(source("R/proveedores_excel.R"))
es_bb_tipo <- function(x) grepl("^cuenta bb$", prov_normalizar_encabezado(x))

ejemplo <- function(d, n = 3L) {
  if (!nrow(d)) return("")
  d <- utils::head(d, n)
  paste(sprintf("…%s (id %s)", substr(d$cuenta, nchar(d$cuenta) - 3L, nchar(d$cuenta)), substr(d$id, 1L, 8L)),
        collapse = ", ")
}

for (cid in clientes) {
  cat("══", cid, "══\n")
  # NULL from the strict read means "no such file" -- or, today, a 403 too
  # (object_exists() says FALSE on Forbidden): said, never counted as 0.
  leer <- function(k) {
    x <- tryCatch(.s3_leer_para_escribir(k, cid), error = function(e) { cat("  could not read", k, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(x)) { cat("  ", k, ": no file, or not readable with these credentials (a 403 looks the same)\n"); return(.schema_proveedores()) }
    .normalize(x, .schema_proveedores)
  }
  p <- dplyr::bind_rows(dplyr::mutate(leer(S3_KEYS$proveedores), .tabla = "proveedores"),
                        dplyr::mutate(leer(S3_KEYS$proveedores_inactivos), .tabla = "inactivos"))
  p$cuenta <- gsub("[[:space:]]", "", as.character(p$no_cuenta))
  p <- p[!is.na(p$cuenta) & nzchar(p$cuenta), , drop = FALSE]
  cat(sprintf("  suppliers with an account: %d (catalog %d, inactivos %d)\n",
              nrow(p), sum(p$.tabla == "proveedores"), sum(p$.tabla == "inactivos")))
  if (!nrow(p)) { cat("\n"); next }

  digitos <- grepl("^[0-9]+$", p$cuenta); n <- nchar(p$cuenta)
  banco3  <- substr(gsub("[^0-9]", "", as.character(p$banco %||% "")), 1L, 3L)
  bb      <- digitos & n >= 10L & n <= 13L & (banco3 == "030" | es_bb_tipo(p$tipo_cuenta %||% "") |
                                             grepl("BAJIO", toupper(p$banco %||% ""), fixed = TRUE))
  c18     <- digitos & n == 18L
  c18_ok  <- c18 & prov_clabe_valida(p$cuenta)
  corta   <- digitos & n %in% c(16L, 17L)
  con_0   <- ifelse(corta, paste0(strrep("0", pmax(0L, 18L - n)), p$cuenta), NA_character_)
  clabe_sin_0 <- corta & prov_clabe_valida(con_0)

  cat(sprintf("  18 digits: %d valid CLABE, %d with a wrong check digit\n", sum(c18_ok), sum(c18 & !c18_ok)))
  cat(sprintf("  16-17 digits that are a valid CLABE with zeros in front (lost their 0): %d  %s\n",
              sum(clabe_sin_0), ejemplo(p[clabe_sin_0, ])))
  cat(sprintf("  16-17 digits that are not (cards, other): %d\n", sum(corta & !clabe_sin_0)))
  cat(sprintf("  10-13 digits, BanBajío (030 / Cuenta BB): %d\n", sum(bb)))
  if (any(bb)) { t <- table(n[bb]); cat("    by length:", paste(names(t), t, sep = " digits: ", collapse = " | "),
                                        "| starting with 0:", sum(startsWith(p$cuenta[bb], "0")), "\n") }
  if (any(corta & !clabe_sin_0)) { t <- table(substr(p$cuenta[corta & !clabe_sin_0], 1L, 1L))
    cat("    16-17 non-CLABE by first digit:", paste(names(t), t, sep = "=", collapse = ", "), "\n") }
  otros <- digitos & !c18 & !corta & !bb
  if (any(otros)) {
    t <- table(n[otros]); cat("  other all-digit lengths (not BanBajío):", paste(names(t), t, sep = " digits: ", collapse = " | "), "\n")
    b <- table(ifelse(nzchar(banco3[otros]), banco3[otros], "?")); cat("    by bank key:", paste(names(b), b, sep = "=", collapse = ", "), "\n")
  } else cat("  other all-digit lengths (not BanBajío): 0\n")
  cat(sprintf("  not all digits: %d\n", sum(!digitos)))

  # Same account once leading zeros are stripped, written differently.
  sin0 <- ifelse(digitos, sub("^0+", "", p$cuenta), NA_character_)
  g <- split(seq_len(nrow(p)), sin0); g <- g[vapply(g, function(i) length(unique(p$cuenta[i])) > 1L, logical(1))]
  cat(sprintf("  accounts written twice, with and without leading zeros: %d groups (%d suppliers)\n",
              length(g), sum(lengths(g))))
  for (i in utils::head(g, 5L))
    cat("   ", paste(sprintf("%d digits in %s (id %s)", nchar(p$cuenta[i]), p$.tabla[i], substr(p$id[i], 1L, 8L)), collapse = " / "), "\n")
  cat("\n")
}
cat("Done. Nothing was written.\n")
