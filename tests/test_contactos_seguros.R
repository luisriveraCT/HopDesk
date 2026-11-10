# =============================================================================
# tests/test_contactos_seguros.R
# CM20, 2026-11-09. A client's contacts (<cid>/contacts.rds, the people who
# get the limit alerts) were read leniently and saved with s3saveRDS(),
# outside the net:
#   - add_contact() with S3 unreachable saved the new contact alone and the
#     client lost every other one;
#   - deactivate_contact() tested isTRUE(df$active) on the whole column: with
#     2+ contacts it never deactivated anything;
#   - read_contacts() built the key by hand (the client id as given), not
#     with .s3_key() like the writes and T28's table.
# Now every change goes through modificar_contacts() (strict read, change on
# S3's current table, .s3_write with the net), contacts only grow, and
# deactivating says FALSE, writing nothing, when there was nothing to do.
#
# Run from project root:  Rscript tests/test_contactos_seguros.R
# =============================================================================
cat("=== test_contactos_seguros ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

CID <- "acme"
k <- paste0(CID, "/contacts.rds")
contacto <- function(id, active = TRUE)
  data.frame(id = id, name = paste("Nombre", id), email = paste0(id, "@cliente.com"), role = "",
             receives_limit_alerts = TRUE, receives_audit_summaries = FALSE,
             active = active, created_at = "2026-11-01 10:00:00", stringsAsFactors = FALSE)
en_s3 <- function() get(k, envir = .mem)

# 1. Deactivating with several contacts changes only the chosen one
s3_memoria_reset(); assign(k, rbind(contacto("c1"), contacto("c2"), contacto("c3")), envir = .mem)
r <- deactivate_contact(CID, "c2")
ok("3 contacts: deactivate_contact says TRUE", isTRUE(r))
ok("3 contacts: c2 is saved inactive", identical(en_s3()$active[en_s3()$id == "c2"], FALSE))
ok("3 contacts: c1 and c3 stay active", all(en_s3()$active[en_s3()$id %in% c("c1", "c3")]))

# 2. Nothing to deactivate: FALSE, nothing written
s3_memoria_reset(); assign(k, rbind(contacto("c1"), contacto("c2", active = FALSE)), envir = .mem)
ok("already inactive: FALSE", identical(deactivate_contact(CID, "c2"), FALSE))
ok("no such id: FALSE", identical(deactivate_contact(CID, "zz"), FALSE))
ok("... and nothing written", .mem_escrituras == 0L)

# 3. A failed read on add: error, S3 untouched (it saved the new one alone)
s3_memoria_reset(); assign(k, rbind(contacto("c1"), contacto("c2")), envir = .mem); .mem_falla <- k
ok("failed read: add_contact stops", falla(add_contact(CID, "Nuevo", "nuevo@cliente.com")))
ok("failed read: deactivate_contact stops", falla(deactivate_contact(CID, "c1")))
ok("failed read: nothing written", .mem_escrituras == 0L)
.mem_falla <- character()
ok("failed read: c1 and c2 still on S3, active", identical(en_s3()$id, c("c1", "c2")) && all(en_s3()$active))

# 4. No file yet: the first contact creates it
s3_memoria_reset()
id1 <- add_contact(CID, " Ana ", "ANA@Cliente.com", receives_limit_alerts = TRUE)
ok("no file: the first contact is saved", nrow(en_s3()) == 1L && identical(en_s3()$id, id1))
ok("no file: same cleaning as before (trim, lower-case email, active)",
   identical(en_s3()$name, "Ana") && identical(en_s3()$email, "ana@cliente.com") && isTRUE(en_s3()$active))

# 5. A column S3's table has and the schema does not is kept
s3_memoria_reset(); x <- contacto("c1"); x$telefono <- "555"; assign(k, x, envir = .mem)
try(add_contact(CID, "Nuevo", "nuevo@cliente.com"), silent = TRUE)   # the old rbind() stopped here
ok("extra column survives add_contact", "telefono" %in% names(en_s3()) && nrow(en_s3()) == 2L &&
   identical(en_s3()$telefono[en_s3()$id == "c1"], "555"))
try(deactivate_contact(CID, "c1"), silent = TRUE)
ok("extra column survives deactivate_contact", identical(en_s3()$telefono[en_s3()$id == "c1"], "555") &&
   identical(en_s3()$active[en_s3()$id == "c1"], FALSE))

# 6. Two sessions each add one: both stay
s3_memoria_reset(); assign(k, contacto("c1"), envir = .mem)
vista_b <- read_contacts(CID)                       # B has the panel open
a <- add_contact(CID, "De A", "a@cliente.com")      # A adds
b <- add_contact(CID, "De B", "b@cliente.com")      # B adds, its view is stale
ok("two sessions: c1, A's and B's contacts all on S3", setequal(en_s3()$id, c("c1", a, b)))
ok("two sessions: B's stale view did not decide it", nrow(vista_b) == 1L)

# 7. read_contacts sees a contact just added (no stale cache)
s3_memoria_reset(); assign(k, contacto("c1"), envir = .mem)
invisible(read_contacts(CID))                       # warm whatever cache it uses
nuevo <- add_contact(CID, "Recién", "recien@cliente.com")
ok("read_contacts right after add_contact has the new contact", nuevo %in% read_contacts(CID)$id)
try(deactivate_contact(CID, nuevo), silent = TRUE)
ok("read_contacts right after deactivate_contact sees it inactive",
   identical(read_contacts(CID)$active[read_contacts(CID)$id == nuevo], FALSE))

# 8. read_contacts reads the same key as the writes and T28's table
s3_memoria_reset(); assign(k, contacto("c1"), envir = .mem)
ok("read_contacts('ACME') reads acme/contacts.rds (.s3_key)", identical(read_contacts("ACME")$id, "c1"))
ok("... the same key T28's table reads strictly",
   identical(.s3_key("contacts.rds", client_id = "ACME"), k))
.mem_falla <- k
ok("read_contacts stays lenient for showing: a failed read is the empty schema",
   identical(read_contacts(CID), .schema_contacts()))
.mem_falla <- character()

# 9. Contacts only grow
ok("contacts.rds is in .S3_SOLO_CRECEN", "contacts.rds" %in% .S3_SOLO_CRECEN())
s3_memoria_reset(); assign(k, rbind(contacto("c1"), contacto("c2")), envir = .mem)
ok("a write that drops a contact is refused", falla(.s3_write(contacto("c1"), "contacts.rds", client_id = CID)))
ok("... and nothing written", .mem_escrituras == 0L && nrow(en_s3()) == 2L)

# 10. No write around the net is left
codigo <- unlist(lapply(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)),
                        function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }))
ok("no write_contacts() left in R/ or app.R", !any(grepl("write_contacts(", codigo, fixed = TRUE)))
cuerpo <- function(f) paste(deparse(get(f)), collapse = "\n")
ok("add_contact / deactivate_contact / read_contacts call no aws.s3 directly",
   !any(grepl("aws.s3::", vapply(c("add_contact", "deactivate_contact", "read_contacts"), cuerpo, ""), fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
