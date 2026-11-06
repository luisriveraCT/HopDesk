# =============================================================================
# tests/test_abonos_seguros.R
# CM19 (2), 2026-11-06 -- money. Partial payments (abonos) were saved from
# the session's copy (shared$abonos_db(), loaded leniently and filtered to
# status active/voided) with save_abonos(), no base, outside the only-grows
# net:
#   - two sessions each adding an abono: the second save dropped the first;
#   - a row of any other status (or NA) vanished on the next save;
#   - a failed read gave an empty copy, and the next abono saved alone.
# Now modificar_abonos() adds or voids by id on S3's whole current table:
# strict read, no status filter, no column dropped, abonos only grow.
#
# Run from project root:  Rscript tests/test_abonos_seguros.R
# =============================================================================
cat("=== test_abonos_seguros ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }

CID <- "acme"
k <- paste0(CID, "/", S3_KEYS$abonos)
abono <- function(id, status = "active", importe = 100)
  tibble::tibble(id = id, ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = paste0("D", id),
                 Parte = "PROV", importe = importe, fecha_abono = as.Date("2026-11-01"), notas = "",
                 created_by = "t", created_at = as.POSIXct("2026-11-01 10:00:00", tz = "UTC"),
                 status = status)
# Counter-proof on the old code (no modificar_abonos there): the old writes
# were cambio(the session's copy, or load_abonos() when it had none) and
# save_abonos(). .copia_sesion plays that copy; the new code ignores it.
.copia_sesion <- NULL
if (!exists("modificar_abonos")) modificar_abonos <- function(cambio, client_id) {
  x <- cambio(.copia_sesion %||% load_abonos(client_id = client_id)); save_abonos(x, client_id = client_id)
  dplyr::filter(x, status %in% c("active", "voided"))
}
agregar <- function(nuevo) modificar_abonos(function(ab) upsert_abono(ab, nuevo), client_id = CID)
anular  <- function(id)    modificar_abonos(function(ab) void_abono(ab, id), client_id = CID)

# 1. Two sessions each add one: both stay (B used to save its copy + its own)
s3_memoria_reset(); assign(k, abono("a1"), envir = .mem)
copia_b <- load_abonos(client_id = CID)            # B loads first
agregar(abono("a2"))                               # A adds
.copia_sesion <- copia_b
agregar(abono("a3"))                               # B adds, after A, holding its old copy
.copia_sesion <- NULL
ok("two sessions: A's abono a2 and B's a3 both on S3",
   identical(sort(get(k, envir = .mem)$id), c("a1", "a2", "a3")))
ok("two sessions: B's stale copy did not decide the result", nrow(copia_b) == 1L)

# 2. A row of another status (or NA) survives adds and voids
s3_memoria_reset()
assign(k, rbind(abono("a1"), abono("raro", status = "pendiente"), abono("sin", status = NA_character_)), envir = .mem)
vista <- agregar(abono("a2"))
anular("a1")
t <- get(k, envir = .mem)
ok("status 'pendiente' and NA rows survive", all(c("raro", "sin") %in% t$id) && nrow(t) == 4L)
ok("void marks only a1", identical(t$status[t$id == "a1"], "voided") && identical(t$status[t$id == "a2"], "active"))
ok("what the screens get is still filtered to active/voided", identical(sort(vista$id), c("a1", "a2")))

# 3. A failed read writes nothing (it saved the new abono alone)
s3_memoria_reset(); assign(k, rbind(abono("a1"), abono("a2")), envir = .mem); .mem_falla <- k
ok("failed read: adding stops", falla(agregar(abono("a3"))))
ok("failed read: voiding stops", falla(anular("a1")))
ok("failed read: nothing written", .mem_escrituras == 0L)
.mem_falla <- character()
ok("failed read: a1 and a2 still on S3, untouched", identical(get(k, envir = .mem)$id, c("a1", "a2")) &&
   all(get(k, envir = .mem)$status == "active"))

# 4. No file yet: the first abono creates it
s3_memoria_reset()
agregar(abono("a1"))
ok("no file: the first abono is saved", identical(get(k, envir = .mem)$id, "a1"))

# 5. A column S3's table has and the schema does not is kept
s3_memoria_reset(); x <- abono("a1"); x$origen <- "legacy"; assign(k, x, envir = .mem)
agregar(abono("a2"))
ok("an extra column survives a save", "origen" %in% names(get(k, envir = .mem)))

# 6. Abonos only grow
ok("abonos is in .S3_SOLO_CRECEN", S3_KEYS$abonos %in% .S3_SOLO_CRECEN())
s3_memoria_reset(); assign(k, rbind(abono("a1"), abono("a2")), envir = .mem)
ok("a change that drops an abono is refused", falla(modificar_abonos(function(ab) ab[1, ], client_id = CID)))
ok("... and nothing written", .mem_escrituras == 0L)

# 7. Every write goes through modificar_abonos
codigo <- unlist(lapply(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)), fuente))
ok("no save_abonos() left in R/ or app.R", !any(grepl("save_abonos(", codigo, fixed = TRUE)))
ok("both Agenda confirmations add through modificar_abonos (pagar_hoy_module.R)",
   sum(grepl("modificar_abonos(function(ab) upsert_abono(ab, ab_new)", fuente("R/pagar_hoy_module.R"), fixed = TRUE)) == 2L)
ok("Bancos' Deshacer abono voids through modificar_abonos",
   any(grepl("modificar_abonos(function(ab) void_abono(ab, ab_id)", fuente("R/bancos_module.R"), fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
