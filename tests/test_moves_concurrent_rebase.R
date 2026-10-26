# =============================================================================
# tests/test_moves_concurrent_rebase.R
#
# Every write of invoice_moves.rds overwrites the WHOLE table. The three call
# sites in R/ledger_module.R build it as
#     <this session's in-memory moves_db> + <their own change>
# so anything another user saved between this session's last 8s sync poll and
# this write used to be erased -- silently, with no error, and looking exactly
# like "tesorería's moves disappeared".
#
# The fix rebases: re-read the live S3 table at write time and re-apply only
# this caller's own change on top of it. These tests exercise the real
# upsert_moves()/anti_join() rebase closures against a simulated concurrent
# writer -- no S3, no network.
# =============================================================================

cat("\n== Escrituras de movimientos rebasan sobre el estado vivo ==\n")

.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        esperado: %s\n        obtenido: %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

suppressMessages({ library(dplyr); library(tibble) })
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
src <- readLines("R/persistence.R", warn = FALSE)
i0  <- grep("^upsert_moves <- function", src)
i1  <- i0 + which(trimws(src[(i0 + 1):(i0 + 60)]) == "}")[1]
eval(parse(text = paste(src[i0:i1], collapse = "\n")), envir = globalenv())

mk <- function(doc, fecha, quien, ledger = "AP") data.frame(
  ledger = ledger, Empresa = "NCS", Moneda = "MXN", Documento = doc,
  FechaVenc_Proyectada = as.Date(fecha), notas = NA_character_,
  moved_by = quien, last_updated = Sys.time(), stringsAsFactors = FALSE)

# ── Escenario: tesorería mueve 2 facturas mientras dev tiene la app abierta ──
cargado_por_dev <- mk("A1", "2026-10-01", "dev")                       # estado al arrancar
en_s3_ahora     <- bind_rows(cargado_por_dev,
                             mk("T1", "2026-10-05", "tesoreria"),
                             mk("T2", "2026-10-06", "tesoreria"))      # tesorería guardó después

nuevo_de_dev <- mk("D9", "2026-10-09", "dev")

# Comportamiento ANTERIOR: dev escribe su copia en memoria + su cambio
viejo <- upsert_moves(cargado_por_dev, nuevo_de_dev)
.chk(nrow(viejo), 2L, "sin rebase, dev escribiria solo 2 filas")
.chk_true(!any(c("T1","T2") %in% viejo$Documento),
          "sin rebase, los 2 movimientos de tesoreria se PIERDEN")

# Comportamiento NUEVO: la misma operación reaplicada sobre el S3 vivo
rebasado <- upsert_moves(en_s3_ahora, nuevo_de_dev)
.chk(nrow(rebasado), 4L, "con rebase se escriben 4 filas")
.chk_true(all(c("T1","T2") %in% rebasado$Documento),
          "con rebase SOBREVIVEN los movimientos de tesoreria")
.chk_true("D9" %in% rebasado$Documento, "con rebase el cambio de dev tambien se aplica")

# ── El rebase de restaurar_fecha sólo debe quitar lo suyo ───────────────────
keys <- data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "A1", stringsAsFactors = FALSE)
tras_limpiar <- anti_join(en_s3_ahora, keys |> mutate(ledger = "AP"),
                          by = c("ledger","Empresa","Moneda","Documento"))
.chk(nrow(tras_limpiar), 2L, "restaurar_fecha quita exactamente 1 fila")
.chk_true(all(c("T1","T2") %in% tras_limpiar$Documento),
          "restaurar_fecha no toca los movimientos de tesoreria")

# ── Un re-move del MISMO documento sigue siendo upsert, no duplicado ────────
remove_mismo <- upsert_moves(en_s3_ahora, mk("T1", "2026-11-20", "dev"))
.chk(nrow(remove_mismo), 3L, "mover un documento ya movido no duplica la fila")
.chk(as.character(remove_mismo$FechaVenc_Proyectada[remove_mismo$Documento == "T1"]),
     "2026-11-20", "la fecha mas reciente gana en el upsert")

# ── AR y AP con el mismo numero siguen siendo distintos ─────────────────────
mixto <- upsert_moves(en_s3_ahora, mk("T1", "2026-12-01", "dev", ledger = "AR"))
.chk(nrow(mixto), 4L, "un AR con el mismo Documento que un AP no lo pisa")

# ── El codigo pasa rebase en los tres sitios de escritura ───────────────────
lg <- paste(readLines("R/ledger_module.R", warn = FALSE), collapse = "\n")
.chk(length(gregexpr("rebase = function(fresh)", lg, fixed = TRUE)[[1]]), 3L,
     "los 3 sitios de escritura pasan un rebase")
.chk_true(grepl(".moves_read_live <- function(client_id = NULL)", lg, fixed = TRUE),
          "existe la lectura viva que evita la cache de preload")
.chk_true(grepl("rm(list = intersect(full_key, ls(.s3_preload_cache))", lg, fixed = TRUE),
          "la lectura viva limpia la cache de preload antes de leer")

cat(sprintf("\n  %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_moves_concurrent_rebase.R: ", .fail, " failure(s)")
