# =============================================================================
# tests/test_cm32_interco_req.R
# CM32, two small leftovers (PENDIENTES).
#   1  load_interco_v2()'s v1 -> v2 migration (legacy interco_settings.rds
#      with codes, interco_v2.rds absent or without codes) saves with
#      save_interco_v2(out): no client_id (the deployment's CLIENT_ID by
#      chance), no base (a whole write), and only when the caller gave no
#      client_id -- a session in a jump never migrates. The sync bus calls
#      the loader with no client_id outside a jump, so it is reached.
#      a) a jump (client_id given): the migrated registry comes back and is
#         saved under THAT client;
#      b) a v2 that exists with RFCs and its own prefixes but no codes: the
#         migration keeps them (today the whole write drops them).
#   2  rename_empresa_initials(): the requisiciones step returns quietly
#      when REQ_KEYS is not loaded (a script, a test, a load-order change),
#      so the rename counts it "unchanged" and empresas.rds goes to the new
#      initials with requisiciones still under the old ones. It must count
#      as failed (hd_renombrar_incompleto).
# In-memory S3 (tests/helpers/s3_memoria.R).
#
# Run from project root:  Rscript tests/test_cm32_interco_req.R
# =============================================================================
cat("=== test_cm32_interco_req ===\n\n")
source("tests/helpers/s3_memoria.R")
# What the other rename steps need (as test_cm29_renombre_sesiones), NOT
# R/requisiciones_persistence.R: section 2 runs without REQ_KEYS first. A
# step whose function or schema is missing fails loudly (an error) -- only
# requisiciones returns quietly.
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R"))
  suppressMessages(source(f))
suppressMessages(source("R/empresas_module.R", encoding = "UTF-8"))   # reservar_renombre_empresa() (CM40)
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
bump_sync_version <- function(...) invisible(TRUE)
fila <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
poner <- function(cid, key, x) assign(paste0(cid, "/", key), x, envir = .mem)
hay   <- function(cid, key) exists(paste0(cid, "/", key), envir = .mem, inherits = FALSE)
leer  <- function(cid, key) get(paste0(cid, "/", key), envir = .mem)
codigos <- function(reg) sort(unique(unlist(lapply(reg$companies, function(co) c(co$ar, co$ap)))))

# Client "acme": two companies, legacy v1 codes, no v2 yet.
sembrar_ic <- function(cid = "acme", v2 = NULL) {
  poner(cid, S3_KEYS$empresas, fila(.schema_empresas, id = c("e1", "e2"), initials = c("AA", "BB"),
                                    razon_social = c("Uno", "Dos"), activa = TRUE, deleted = FALSE))
  poner(cid, S3_KEYS$interco, list(ar_clients = c("C100", "C200"), ap_suppliers = "P300"))
  if (!is.null(v2)) poner(cid, S3_KEYS$interco_v2, v2)
}

# ── 1a. A jump: the migration runs and saves under that client ──────────────
s3_memoria_reset(); Sys.setenv(CLIENT_ID = "otro")   # the deployment is another client
sembrar_ic("acme")
reg <- suppressMessages(load_interco_v2(client_id = "acme"))
ok("1a control: the legacy codes are there for acme", hay("acme", S3_KEYS$interco))
ok("1a a session in a jump to acme gets acme's migrated codes (100, 200, 300)",
   identical(codigos(reg), c("100", "200", "300")))
ok("1a …saved under acme/, never under the deployment's client (otro/)",
   hay("acme", S3_KEYS$interco_v2) && !hay("otro", S3_KEYS$interco_v2) &&
     identical(codigos(leer("acme", S3_KEYS$interco_v2)), c("100", "200", "300")))

# ── 1b. A v2 with RFCs and prefixes but no codes keeps them ────────────────
s3_memoria_reset(); Sys.setenv(CLIENT_ID = "acme")
sembrar_ic("acme", v2 = list(ar_prefix = "CL", ap_prefix = "PR", rfcs = c(CL9 = "AAA010101AA1"),
                             companies = list(AA = list(ar = character(), ap = character()))))
reg <- suppressMessages(load_interco_v2())
ok("1b control: the native session migrates the codes", identical(codigos(reg), c("100", "200", "300")))
v2 <- leer("acme", S3_KEYS$interco_v2)
ok("1b …and keeps the RFCs and the prefixes the client already had",
   identical(v2$rfcs, c(CL9 = "AAA010101AA1")) && identical(v2$ar_prefix, "CL") && identical(v2$ap_prefix, "PR"))

# ── 1c. Idempotent: once migrated, a load writes nothing ────────────────────
n <- .mem_escrituras
reg2 <- suppressMessages(load_interco_v2(client_id = "acme"))
ok("1c a second load after the migration writes nothing and gives the same codes",
   identical(.mem_escrituras, n) && identical(codigos(reg2), c("100", "200", "300")))
s3_memoria_reset()
sembrar_ic("acme", v2 = list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                             companies = list(AA = list(ar = "999", ap = character()))))
n <- .mem_escrituras
reg3 <- suppressMessages(load_interco_v2(client_id = "acme"))
ok("1c a v2 that already has codes is never migrated over (no write, its own codes)",
   identical(.mem_escrituras, n) && identical(codigos(reg3), "999"))

# ── 1d. Never on the shared deployment without a client ─────────────────────
s3_memoria_reset(); Sys.setenv(CLIENT_ID = "hd-admin")
sembrar_ic("hd-admin")
n <- .mem_escrituras
r <- tryCatch(suppressMessages(.interco_v2_migrar(NULL)), error = function(e) e)
ok("1d no client_id on hd-admin: no migration, nothing written", is.null(r) && identical(.mem_escrituras, n))

# ── 1e. After save_interco_v2() the next read is the new registry ───────────
s3_memoria_reset(); Sys.setenv(CLIENT_ID = "otro")
sembrar_ic("acme", v2 = list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                             companies = list(AA = list(ar = "1", ap = character()))))
invisible(suppressMessages(load_interco_v2(client_id = "acme")))           # read once (preload filled)
nuevo <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = list(AA = list(ar = "2", ap = character())))
suppressMessages(save_interco_v2(nuevo, client_id = "acme"))
ok("1e after save_interco_v2() the next read of that client gives the new codes",
   identical(codigos(suppressMessages(load_interco_v2(client_id = "acme"))), "2"))
ok("1e …and nothing is left in the preload cache under the bare key (no client)",
   !exists(S3_KEYS$interco_v2, envir = .s3_preload_cache, inherits = FALSE))

# ── 2. The rename with REQ_KEYS not loaded ──────────────────────────────────
s3_memoria_reset()
cid <- "acme"
poner(cid, S3_KEYS$empresas, fila(.schema_empresas, id = "e1", initials = "OLD", activa = TRUE, deleted = FALSE))
ok("2 setup: REQ_KEYS is not loaded here", !exists("REQ_KEYS"))
# CM40: unreserved, the cascade is refused before any step; reserved, it runs.
r <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = cid)); NULL },
              error = function(e) e)
ok("2 CM40: without a reservation the cascade is refused, nothing written",
   inherits(r, "error") && grepl("was not reserved", conditionMessage(r), fixed = TRUE))
suppressMessages(reservar_renombre_empresa("e1", "OLD", "NEW", cid))
r <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = cid)); "completo" },
              hd_renombrar_incompleto = function(e) e)
ok("2 the rename is incomplete (hd_renombrar_incompleto), not 'complete'", inherits(r, "hd_renombrar_incompleto"))
ok("2 …with requisiciones among the failed steps", inherits(r, "hd_renombrar_incompleto") && "requisiciones" %in% r$fallidos)

# Control: with REQ_KEYS loaded the step runs as before.
source("R/requisiciones_persistence.R")
s3_memoria_reset()
poner(cid, S3_KEYS$empresas, fila(.schema_empresas, id = "e1", initials = "OLD", activa = TRUE, deleted = FALSE))
poner(cid, REQ_KEYS$requisiciones, fila(.REQ_SCHEMAS$requisiciones, id = "r1", empresa = "OLD"))
suppressMessages(reservar_renombre_empresa("e1", "OLD", "NEW", cid))
r <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = cid)) },
              hd_renombrar_incompleto = function(e) e)
ok("2 control: with REQ_KEYS loaded, requisiciones is renamed and nothing failed",
   !inherits(r, "hd_renombrar_incompleto") && "requisiciones" %in% r &&
     identical(leer(cid, REQ_KEYS$requisiciones)$empresa, "NEW"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
