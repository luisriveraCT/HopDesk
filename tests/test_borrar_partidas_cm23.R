# =============================================================================
# tests/test_borrar_partidas_cm23.R
# CM23, 2026-11-10. Runs scripts/borrar_partidas_prueba_cm23.R as it is, on
# the in-memory S3 (tests/helpers/s3_memoria.R): dry run writes nothing,
# APLICAR=1 sends exactly the 2 items to the papelera (restorable, hidden by
# id), leaves every other row and the provision alone, logs once; a second
# run does nothing; ids that do not match stop it without writing.
#
# Run from project root:  Rscript tests/test_borrar_partidas_cm23.R
# =============================================================================
cat("=== test_borrar_partidas_cm23 ===\n\n")
source("tests/helpers/s3_memoria.R")
.mock <- list(s3readRDS = get("s3readRDS", asNamespace("aws.s3")),
              s3saveRDS = get("s3saveRDS", asNamespace("aws.s3")),
              object_exists = get("object_exists", asNamespace("aws.s3")))
reinstalar <- function() for (nm in names(.mock)) utils::assignInNamespace(nm, .mock[[nm]], "aws.s3")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID  <- "networks"
IDS  <- c("efb376cf-acaa-4e52-b222-bab8641c426d", "716cf3db-bebd-48d5-ade2-01e5754bef15")
k_m  <- paste0(CID, "/", S3_KEYS$manual)
k_p  <- paste0(CID, "/", S3_KEYS$papelera)
renv <- tempfile(fileext = ".Renviron"); writeLines("S3_BUCKET=fake-bucket", renv)

fila <- function(id, doc, fecha = as.Date(NA), prov = NA_character_) {
  x <- .schema_manual()[NA_integer_, ]
  x$id <- id; x$ledger <- "AP"; x$Empresa <- "Networks & Logistics"; x$Moneda <- "MXN"
  x$Documento <- doc; x$Parte <- "P"; x$Importe <- 1; x$`Fecha de vencimiento` <- fecha
  x$provision_id <- prov; x$created_by <- "t"
  x
}
sembrar <- function() {
  s3_memoria_reset(); reinstalar()
  assign(k_m, dplyr::bind_rows(fila(IDS[1], "test", prov = "7c910911"), fila(IDS[2], "Internet"),
                               fila("otra-1", "F1", Sys.Date() + 3), fila("otra-2", "F2", Sys.Date() + 9)),
         envir = .mem)
  ev <- function(id, event, action, reverses = NA_character_) {
    x <- .schema_papelera()[NA_integer_, ]
    x$id <- id; x$event_id <- event; x$action <- action; x$reverses_event_id <- reverses
    x$ledger <- "AP"; x$disposition <- "confirmed"; x$deleted_by <- "dev"
    x
  }
  # As in prod: an old row, plus a confirm + undo pair for each of the 2.
  assign(k_p, dplyr::bind_rows(ev("vieja", "ev-vieja", "archived"),
                               ev(IDS[1], "a1", "archived"), ev(IDS[1], "r1", "recovered", "a1"),
                               ev(IDS[2], "a2", "archived"), ev(IDS[2], "r2", "recovered", "a2")),
         envir = .mem)
}
# The script as Luis's terminal runs it: its args, its APLICAR, its quit().
correr <- function(aplicar = FALSE) {
  Sys.setenv(APLICAR = if (aplicar) "1" else "0")
  assign("commandArgs", function(trailingOnly = FALSE) c(renv, CID), envir = globalenv())
  assign("quit", function(...) stop(structure(class = c("salida", "condition"), list(message = "quit"))),
         envir = globalenv())
  out <- utils::capture.output(res <- tryCatch(
    suppressMessages(source("scripts/borrar_partidas_prueba_cm23.R")),
    salida = function(e) "salida", error = function(e) e))
  rm(list = c("commandArgs", "quit"), envir = globalenv())
  reinstalar()
  list(res = res, out = out)
}
falló <- function(r) inherits(r$res, "error")

# 1. Dry run: shows the 2, writes nothing
sembrar()
r <- correr(FALSE)
ok("dry run ends normally", !falló(r))
ok("dry run writes nothing", .mem_escrituras == 0L)
ok("dry run names both ids", all(vapply(IDS, function(i) any(grepl(i, r$out, fixed = TRUE)), TRUE)))
ok("dry run prints no amount column", !any(grepl("Importe", r$out, fixed = TRUE)))
ok("dry run: manual_inv 4 -> 2, papelera 5 -> 7",
   any(grepl("manual_inv: 4 -> 2 rows | papelera: 5 -> 7 rows", r$out, fixed = TRUE)))
ok("dry run: an old confirm + undo pair does not count as archived",
   any(grepl("already archived in papelera: 0 of 2", r$out, fixed = TRUE)))

# 2. APLICAR=1: the 2 go to the papelera, nothing else changes
r <- correr(TRUE)
m <- get(k_m, envir = .mem); p <- get(k_p, envir = .mem)
ok("apply ends normally, every check OK", !falló(r) && !any(grepl("FAIL", r$out, fixed = TRUE)) &&
     any(grepl("Done.", r$out, fixed = TRUE)))
ok("apply: the 2 are gone from manual_inv, the other 2 stay", setequal(m$id, c("otra-1", "otra-2")))
ok("apply: the 2 are archived in the papelera by their own id (hidden by id, restorable)",
   { nuevos <- p[p$deleted_by %in% "coordinador (CM23, pedido de Luis)", ]
     nrow(nuevos) == 2L && setequal(nuevos$id, IDS) && all(nuevos$action == "archived") &&
       all(nuevos$disposition == "deleted") && all(nuevos$source == "manual") &&
       all(vapply(nuevos$original_data, function(o) o$id %in% IDS, TRUE)) })
ok("apply: the papelera keeps every earlier event", all(c("ev-vieja", "a1", "r1", "a2", "r2") %in% p$event_id) &&
     nrow(p) == 7L)
audit_keys <- grep("^networks/.*audit", ls(.mem), value = TRUE)
log <- if (length(audit_keys)) dplyr::bind_rows(lapply(audit_keys, get, envir = .mem)) else data.frame()
ok("apply: one eliminar_facturas in ledger_AP in networks' audit log",
   nrow(log) && sum(log$action == "eliminar_facturas" & log$module == "ledger_AP") == 1L)

# 3. A second run does nothing
n_antes <- .mem_escrituras
r <- correr(TRUE)
ok("second run: 'Nothing to do', no write", !falló(r) && any(grepl("Nothing to do", r$out, fixed = TRUE)) &&
     .mem_escrituras == n_antes)

# 4. Ids that do not match: stops, writes nothing
sembrar(); m0 <- get(k_m, envir = .mem); assign(k_m, m0[m0$id != IDS[2], ], envir = .mem)
r <- correr(TRUE)
ok("only one of the 2 present: stops", falló(r) && grepl("Expected exactly the 2 ids", conditionMessage(r$res)))
ok("... and writes nothing", .mem_escrituras == 0L)
sembrar(); m0 <- get(k_m, envir = .mem); assign(k_m, dplyr::bind_rows(m0, m0[1, ]), envir = .mem)
r <- correr(TRUE)
ok("an id repeated: stops, writes nothing", falló(r) && .mem_escrituras == 0L)

# 5. A failed read stops before anything is written
sembrar(); .mem_falla <- k_p
r <- correr(TRUE)
ok("papelera unreadable: stops, writes nothing", falló(r) && .mem_escrituras == 0L)
.mem_falla <- character()

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
