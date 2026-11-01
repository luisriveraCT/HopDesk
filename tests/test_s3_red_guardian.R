# =============================================================================
# tests/test_s3_red_guardian.R
# Guard for the CM09 write net (R/persistence.R). Fails when:
#   1. a key is added to .S3_SIN_RED without its reason in the comment
#      right above it;
#   2. permitir_reduccion = TRUE appears outside the documented places;
#   3. the papelera or bancos_confirmados can shrink: add_to_papelera() /
#      restore_from_papelera() must only add rows, no save_papelera() /
#      save_bancos_confirmados() may receive a filtered table, and there is no
#      "vaciar/purgar papelera" action (if one is ever added, it must pass
#      permitir_reduccion and be listed in point 2);
#   4. the number of "read with an empty fallback" sites
#      (tryCatch(load_x(...), error = function(e) <empty>)) goes UP. It may go
#      down: lower .TRINQUETE when a conversion removes some.
#
# Run from project root:  Rscript tests/test_s3_red_guardian.R
# =============================================================================
cat("=== test_s3_red_guardian ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
codigo <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
archivos <- c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))

# ── 1. Every key outside the net says why ─────────────────────────────────────
src <- readLines("R/persistence.R", warn = FALSE)
def <- grep("^\\.S3_SIN_RED <- function", src)
ok("found .S3_SIN_RED", length(def) == 1L)
if (length(def) == 1L) {
  arriba <- def - 1L
  while (arriba > 0 && grepl("^#", src[arriba])) arriba <- arriba - 1L
  comentario <- paste(src[(arriba + 1L):(def - 1L)], collapse = " ")
  fin <- def; while (!grepl("^\\s*\"sap_snapshot_AP\\.rds\"\\)|\\)\\s*$", src[fin]) && fin < def + 10) fin <- fin + 1L
  cuerpo <- paste(src[def:fin], collapse = " ")
  llaves <- c(regmatches(cuerpo, gregexpr("\"[a-z_]+\"", cuerpo))[[1]],
              regmatches(cuerpo, gregexpr("\"[A-Za-z_]+\\.rds\"", cuerpo))[[1]])
  llaves <- unique(gsub("\"|\\.rds$", "", llaves))
  tokens <- regmatches(comentario, gregexpr("[a-z_]+[A-Za-z_]*\\*?", comentario))[[1]]
  explicada <- function(k) any(vapply(tokens, function(t) {
    if (endsWith(t, "*")) startsWith(k, sub("\\*$", "", t)) else identical(t, k)
  }, logical(1)))
  for (k in llaves) ok(sprintf(".S3_SIN_RED: '%s' has its reason in the comment", k), explicada(k))
}

# ── 2. permitir_reduccion = TRUE only where documented ────────────────────────
permitidos <- c(
  "R/pasivos_module.R"      = "save_pasivos_provisions(cleaned",        # pasivos_admin_purge_closed(): intended purge
  "R/pasivos_persistence.R" = "permitir_reduccion = TRUE)",             # save_pasivos_provisions passes its flag on
  "R/persistence.R"         = "if (isTRUE(permitir_reduccion)) .s3_write(" # modificar_tabla / .s3_write_with pass it on
)
usos <- do.call(rbind, lapply(archivos, function(f) {
  x <- codigo(f); i <- grep("permitir_reduccion = TRUE", x, fixed = TRUE)
  if (length(i)) data.frame(f = f, txt = x[i], stringsAsFactors = FALSE) else NULL
}))
usos <- usos[!grepl("pass permitir_reduccion = TRUE if this is intended", usos$txt, fixed = TRUE), , drop = FALSE]
fuera <- usos[!mapply(function(f, t) f %in% names(permitidos) && grepl(permitidos[[f]], t, fixed = TRUE),
                      usos$f, usos$txt), , drop = FALSE]
for (i in seq_len(nrow(fuera))) message("  undocumented: ", fuera$f[i], ": ", trimws(fuera$txt[i]))
ok("permitir_reduccion = TRUE appears only in the documented places", nrow(fuera) == 0L)

# ── 3. The papelera and bancos_confirmados only grow ──────────────────────────
for (e in parse("R/persistence.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("add_to_papelera", "restore_from_papelera", ".schema_papelera"))
    eval(e, globalenv())
for (e in parse("R/erp_connector_registry.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
inv <- data.frame(Documento = c("F1", "F2"), Empresa = "NCS", Moneda = "MXN", stringsAsFactors = FALSE)
p1 <- tryCatch(add_to_papelera(.schema_papelera(), inv, ledger = "AP", deleted_by = "t"), error = function(e) NULL)
ok("add_to_papelera only adds rows", !is.null(p1) && nrow(p1) == 2L)
p2 <- tryCatch(restore_from_papelera(p1, p1$event_id[1], actor = "t")$papelera_df, error = function(e) NULL)
ok("restore_from_papelera only adds a row", !is.null(p2) && nrow(p2) == 3L)

filtrada <- "\\[\\s*!|filter\\(|anti_join\\(|subset\\(|\\[-"
malas <- character()
for (f in archivos) {
  x <- codigo(f)
  for (i in grep("save_papelera\\(|save_bancos_confirmados\\(", x)) {
    arg <- sub(".*save_(papelera|bancos_confirmados)\\(([^,]*),.*", "\\2", x[i])
    if (grepl(filtrada, arg)) malas <- c(malas, sprintf("%s: %s", f, trimws(x[i])))
  }
}
for (m in malas) message("  filtered save: ", m)
ok("no save_papelera()/save_bancos_confirmados() receives a filtered table", !length(malas))
acciones <- unlist(lapply(archivos, function(f) grep("(vaciar|purgar|purge)[_ ]?papelera", codigo(f), ignore.case = TRUE, value = TRUE)))
ok("there is no 'vaciar/purgar papelera' action (add one only with permitir_reduccion, listed above)",
   !length(acciones))

# ── 4. Ratchet: read-with-empty-fallback sites may only go down ───────────────
.TRINQUETE <- 77L   # 2026-11-01, feat/f-s3-lectura; lower it as conversions land
patron <- "tryCatch\\(\\s*load_[a-z_]+\\([^)]*\\)\\s*,\\s*error\\s*=\\s*function\\(e\\)\\s*(\\.schema_[a-z_]+\\(\\)|tibble::tibble\\(\\)|NULL)\\)"
n <- sum(vapply(archivos, function(f) sum(grepl(patron, codigo(f))), integer(1)))
cat(sprintf("  read-with-empty-fallback sites: %d (ratchet %d)\n", n, .TRINQUETE))
ok("read-with-empty-fallback sites did not go up", n <= .TRINQUETE)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
