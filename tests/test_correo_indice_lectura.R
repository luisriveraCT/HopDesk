# =============================================================================
# tests/test_correo_indice_lectura.R
# CM18 (2026-11-05): two more hd-admin tables saved with aws.s3::s3saveRDS()
# over a lenient read, outside the .s3_write net, like the ones of CM13:
#   - username_index: a failed read came back empty, so every name looked
#     free and register/unregister saved an index holding only their row;
#   - email_queue: a failed read came back empty and the next queued (or
#     retried) mail saved a queue holding only itself; a retry also saved the
#     copy read before sending, dropping mail queued meanwhile.
# Both now read strictly and change S3's current table (modificar_hd_admin).
#
# Run from project root:  Rscript tests/test_correo_indice_lectura.R
# =============================================================================
cat("=== test_correo_indice_lectura ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/email_service.R"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }

# ── 1. username_index ─────────────────────────────────────────────────────────
k_idx <- "hd-admin/username_index.rds"
nombre <- function(u, activo = TRUE)
  data.frame(username = u, home_folder = "networks", account_code = "U0001",
             created_at = "2026-01-01", active = activo, stringsAsFactors = FALSE)
INDICE0 <- rbind(nombre("ana"), nombre("beto"))

# 1a. A failed read: not verified (it said "free"), and nothing is written
s3_memoria_reset(); assign(k_idx, INDICE0, envir = .mem); .mem_falla <- k_idx
ok("index, failed read: check_username_available says NA, not TRUE", is.na(check_username_available("ana")))
ok("index, failed read: register stops", falla(register_username("carla", "networks", "U0003")))
ok("index, failed read: unregister stops", falla(unregister_username("ana")))
ok("index, failed read: nothing written", .mem_escrituras == 0L)
.mem_falla <- character()
ok("index, failed read: ana and beto still on S3", identical(get(k_idx, envir = .mem)$username, c("ana", "beto")))

# 1b. Register and unregister change S3's current index, rows kept
s3_memoria_reset(); assign(k_idx, INDICE0, envir = .mem)
register_username("Carla", "hopdesk", "U0003")
i <- get(k_idx, envir = .mem)
ok("index: register adds next to the others", identical(i$username, c("ana", "beto", "carla")))
ok("index: carla is taken now", identical(check_username_available("CARLA"), FALSE))
ok("index: unregister says it was there", isTRUE(unregister_username("ana")))
i <- get(k_idx, envir = .mem)
ok("index: unregister only marks ana inactive (3 rows)",
   nrow(i) == 3L && identical(i$active[i$username == "ana"], FALSE) && isTRUE(i$active[i$username == "beto"]))
ok("index: ana is free again", isTRUE(check_username_available("ana")))
ok("index: unregister of an unknown name is FALSE and writes nothing",
   { n <- .mem_escrituras; identical(unregister_username("nadie"), FALSE) && .mem_escrituras == n })
register_username("ana", "networks", "U0001")
i <- get(k_idx, envir = .mem)
ok("index: re-register reactivates in place (no duplicate)", sum(i$username == "ana") == 1L && isTRUE(i$active[i$username == "ana"]))

# 1c. No index yet: everything free; the first register creates it
s3_memoria_reset()
ok("index, no file: a name is free", isTRUE(check_username_available("ana")))
register_username("ana", "networks", "U0001")
ok("index, no file: register creates it", identical(get(k_idx, envir = .mem)$username, "ana"))

# 1d. Only grows
ok("username_index is in .S3_SOLO_CRECEN", "username_index.rds" %in% .S3_SOLO_CRECEN())

# ── 2. email_queue ────────────────────────────────────────────────────────────
k_q <- "hd-admin/email_queue.rds"
correo <- function(id, status = "pending", attempts = 0L)
  data.frame(id = id, to = paste0(id, "@x.mx"), subject = "s", html = "<p>h</p>",
             queued_at = "2026-11-05T00:00:00Z", last_tried = NA_character_, attempts = attempts,
             last_error = NA_character_, status = status, stringsAsFactors = FALSE)
COLA0 <- rbind(correo("eq_1"), correo("eq_2", "failed", 1L))
ENVIADOS <- character()
# Stub of the network send: records the address; "mal@" fails. `al_enviar`
# runs during the send, to play another session writing meanwhile.
al_enviar <- function() NULL
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  ENVIADOS <<- c(ENVIADOS, to); al_enviar()
  if (startsWith(to, "mal@")) structure(FALSE, error = "rechazado") else TRUE
}

# 2a. Queuing with S3 unreadable: nothing written, no error (it saved a
#     queue holding only the new mail)
s3_memoria_reset(); assign(k_q, COLA0, envir = .mem); .mem_falla <- k_q
ok("queue, failed read: enqueue does not throw", !falla(.enqueue_email("a@x.mx", "s", "h")))
ok("queue, failed read: nothing written", .mem_escrituras == 0L)
ok("queue, failed read: read_email_queue stops", falla(read_email_queue()))
ok("queue, failed read: retry of one is FALSE-by-error, nothing sent", { ENVIADOS <- character()
   falla(retry_queued_email("eq_1")) && !length(ENVIADOS) && .mem_escrituras == 0L })
.mem_falla <- character()
ok("queue, failed read: both mails still on S3", identical(get(k_q, envir = .mem)$id, c("eq_1", "eq_2")))

# 2b. Queuing lands next to the others
s3_memoria_reset(); assign(k_q, COLA0, envir = .mem)
.enqueue_email("nuevo@x.mx", "s", "h", error = "sin clave")
q <- get(k_q, envir = .mem)
ok("queue: enqueue appends (3 rows, the new one pending)", nrow(q) == 3L && q$status[3] == "pending" && q$to[3] == "nuevo@x.mx")

# 2c. A retry while another session queues a mail: both survive (the retry
#     saved the copy read before sending)
s3_memoria_reset(); assign(k_q, COLA0, envir = .mem)
al_enviar <- function() assign(k_q, rbind(get(k_q, envir = .mem), correo("eq_3")), envir = .mem)
ok("queue: retry of eq_1 succeeds", isTRUE(retry_queued_email("eq_1")))
al_enviar <- function() NULL
q <- get(k_q, envir = .mem)
ok("queue, retry: the mail queued meanwhile (eq_3) is kept", "eq_3" %in% q$id && nrow(q) == 3L)
ok("queue, retry: eq_1 sent, one attempt, tried now",
   q$status[q$id == "eq_1"] == "sent" && q$attempts[q$id == "eq_1"] == 1L && !is.na(q$last_tried[q$id == "eq_1"]))
ok("queue, retry: eq_2 untouched", q$status[q$id == "eq_2"] == "failed" && q$attempts[q$id == "eq_2"] == 1L)

# 2d. Retry all, same race, one fails
s3_memoria_reset(); assign(k_q, rbind(correo("eq_1"), correo("mal")), envir = .mem)
al_enviar <- function() { q <- get(k_q, envir = .mem); if (!"eq_9" %in% q$id) assign(k_q, rbind(q, correo("eq_9", "sent")), envir = .mem) }
r <- retry_all_queued_emails()
al_enviar <- function() NULL
q <- get(k_q, envir = .mem)
ok("queue, retry all: counts", r$sent == 1L && r$failed == 1L)
ok("queue, retry all: the mail added meanwhile is kept", "eq_9" %in% q$id && nrow(q) == 3L)
ok("queue, retry all: statuses and errors land on S3's rows",
   q$status[q$id == "eq_1"] == "sent" && q$status[q$id == "mal"] == "failed" &&
   q$last_error[q$id == "mal"] == "rechazado" && q$attempts[q$id == "mal"] == 1L)

# 2e. A save that fails after sending: the result is still the send's
s3_memoria_reset(); assign(k_q, COLA0, envir = .mem)
orig_save <- get("s3saveRDS", asNamespace("aws.s3"))
utils::assignInNamespace("s3saveRDS", function(...) stop("simulated write failure"), "aws.s3")
res <- tryCatch(retry_queued_email("eq_1"), error = function(e) "error")
utils::assignInNamespace("s3saveRDS", orig_save, "aws.s3")
ok("queue, failed save after a good send: still TRUE (not re-sent as failed)", isTRUE(res))

ok("email_queue is in .S3_SOLO_CRECEN", "email_queue.rds" %in% .S3_SOLO_CRECEN())

# ── 3. No direct S3 saves left for these two ──────────────────────────────────
codigo <- c(fuente("R/email_service.R"), fuente("R/persistence.R"))
ok("no s3saveRDS left in email_service.R", !any(grepl("s3saveRDS", fuente("R/email_service.R"), fixed = TRUE)))
ok("no write_email_queue / .write_username_index left in R/",
   !any(grepl("write_email_queue|\\.write_username_index", unlist(lapply(list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE), fuente)))))
ok("Usuarios refuses a name it could not verify",
   any(grepl("is.na(disponible)", fuente("R/tiers_module.R"), fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
