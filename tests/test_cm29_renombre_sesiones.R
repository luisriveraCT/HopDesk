# =============================================================================
# tests/test_cm29_renombre_sesiones.R
# CM29 (PENDIENTES l.198). After a company's initials are renamed (CM26,
# rename_empresa_initials()), a session that was already open keeps the old
# ones and can write them back:
#   1  the rename rewrites six tables registered on the sync bus and bumps
#      none of them (only interco_v2), so the other sessions keep their copies
#      with the old initials -- and those copies are the `base` their next
#      save is rebased on;
#   2  a session that loaded before the rename edits another field of a row
#      the rename changed: rebase_por_id() takes the session's whole row, so
#      the row goes back to the OLD initials (proveedores, pasivos);
#   3  a form opened before the rename (its company choices are the session's
#      company_map(), a showModal() that is not redrawn) saves a NEW row with
#      the OLD initials: an orphan no screen shows under the company.
# Two sessions over the in-memory S3; real rename_empresa_initials(),
# save_proveedores() and save_pasivos_provisions().
#
# Run from project root:  Rscript tests/test_cm29_renombre_sesiones.R
# =============================================================================
cat("=== test_cm29_renombre_sesiones ===\n\n")
source("tests/helpers/s3_memoria.R")
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R", "R/requisiciones_persistence.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
fila  <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
# The sync bus as app.R registers it: the names and S3 keys of its
# register_synced("name", S3_KEYS$key, ...) calls, and the key -> name lookup.
source("R/sync_bus.R")
for (e in parse("R/offline_queue.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".queue_table_name_for_key"))
    eval(e, globalenv())
app_txt <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
regs <- regmatches(app_txt, gregexpr('register_synced\\(\\s*"([A-Za-z0-9_]+)",\\s*S3_KEYS\\$([A-Za-z0-9_]+)', app_txt))[[1]]
for (r in regs) {
  m <- regmatches(r, regexec('"([A-Za-z0-9_]+)",\\s*S3_KEYS\\$([A-Za-z0-9_]+)', r))[[1]]
  register_synced(m[2], S3_KEYS[[m[3]]], loader = function(...) NULL)
}
BUMPS <- character()
bump_sync_version <- function(name, client_id = NULL) { BUMPS <<- c(BUMPS, name); invisible(TRUE) }

# S3 with OLD and OTRA in every table the rename touches (as test_renombrar_iniciales).
sembrar <- function() {
  s3_memoria_reset(); BUMPS <<- character()
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = c("e1", "e2"), initials = c("OLD", "OTRA"),
                               activa = c(TRUE, TRUE), deleted = c(FALSE, FALSE)))
  poner(S3_KEYS$ctas_cuentas,   fila(.schema_ctas_cuentas,  id = c("c1", "c2"), Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$bancos_cuentas, fila(.schema_bancos_cuentas, cuenta_id = c("b1", "b2"), empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$proveedores,    fila(.schema_proveedores, id = c("p1", "p2"), nombre = c("UNO", "DOS"),
                                     Empresa = c("OLD", "OTRA"), empresas = c("OLD,OTRA", "OTRA"),
                                     activo = c(TRUE, TRUE)))
  poner(S3_KEYS$proveedores_inactivos, fila(.schema_proveedores, id = "pi1", nombre = "INACTIVO",
                                            Empresa = "OLD", empresas = "OLD", activo = FALSE))
  poner(S3_KEYS$parte_alias_map, fila(.schema_parte_alias_map, Empresa = c("OLD", "OTRA")))
  poner(S3_KEYS$interco_v2, list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                                 companies = list(OLD = list(ar = "C1"), OTRA = list(ar = "C2"))))
  poner(S3_KEYS$pasivos_provisions, fila(.schema_pasivos_provision, id = c("v1", "v2"), empresa = c("OLD", "OTRA"),
                                         parte = c("Renta", "Luz")))
  poner(S3_KEYS$pasivos_liabilities, fila(.schema_pasivos_liability, id = c("l1", "l2"), empresa = c("OLD", "OTRA")))
  poner(REQ_KEYS$requisiciones, fila(.REQ_SCHEMAS$requisiciones, id = c("r1", "r2"), empresa = c("OLD", "OTRA")))
}
# What the Empresas edit does (CM40): reserve NEW in empresas.rds, the
# cascade, then (complete) empresas.rds with NEW and the earlier initials
# (.iniciales_anteriores_tras(), CM29 part 2), the reservation cleared.
source("R/empresas_module.R", encoding = "UTF-8")
renombrar <- function() {
  suppressMessages(reservar_renombre_empresa("e1", "OLD", "NEW", CID))
  suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID))
  suppressMessages(confirmar_renombre_empresa("e1", "OLD", "NEW", CID))
}
iniciales_vivas <- function() leer(S3_KEYS$empresas)$initials
# Every initial a proveedores row names (Empresa and the comma list).
ini_prov <- function(df) unique(trimws(c(df$Empresa, unlist(strsplit(df$empresas, ",", fixed = TRUE)))))

# ── 1. The sessions are told about the tables the rename changed ─────────────
sembrar()
renombrar()
# sync-bus name of each table the rename rewrites (app.R register_synced()).
SINCRO <- c(ctas_cuentas = "ctas_cuentas", proveedores = "proveedores_db",
            proveedores_inactivos = "proveedores_inactivos_db", parte_alias_map = "parte_alias_map_db",
            interco_v2 = "interco_v2", pasivos_provisions = "pasivos_provisions_db",
            pasivos_liabilities = "pasivos_liabilities_db")
for (t in names(SINCRO))
  ok(sprintf("1 %s: renamed in S3 and bumped (%s), so open sessions reload it", t, SINCRO[[t]]),
     SINCRO[[t]] %in% BUMPS)

# ── 2. A session that loaded before the rename edits a renamed row ──────────
sembrar()
b_prov <- leer(S3_KEYS$proveedores); b_pv <- leer(S3_KEYS$pasivos_provisions)   # session B loads
renombrar()                                                                       # session A renames
ed <- b_prov; ed$nombre[ed$id == "p1"] <- "UNO SA DE CV"                          # B edits the name only
suppressMessages(save_proveedores(ed, client_id = CID, base = b_prov))
p <- leer(S3_KEYS$proveedores)
ok("2 proveedores: B's edit of the name is saved", identical(p$nombre[p$id == "p1"], "UNO SA DE CV"))
ok("2 proveedores: …and p1 keeps NEW (Empresa and the list), never OLD again",
   identical(p$Empresa[p$id == "p1"], "NEW") && identical(p$empresas[p$id == "p1"], "NEW,OTRA"))
ed <- b_pv; ed$parte[ed$id == "v1"] <- "Renta oficina"
suppressMessages(save_pasivos_provisions(ed, client_id = CID, base = b_pv))
v <- leer(S3_KEYS$pasivos_provisions)
ok("2 pasivos: B's edit of the parte is saved and v1 keeps NEW",
   identical(v$parte[v$id == "v1"], "Renta oficina") && identical(v$empresa[v$id == "v1"], "NEW"))

# ── 3. A form opened before the rename saves a new row ──────────────────────
sembrar()
b_prov <- leer(S3_KEYS$proveedores); b_pv <- leer(S3_KEYS$pasivos_provisions)
renombrar()
nuevo <- dplyr::bind_rows(b_prov, fila(.schema_proveedores, id = "p3", nombre = "TRES", Empresa = "OLD",
                                       empresas = "OLD", activo = TRUE))
suppressMessages(save_proveedores(nuevo, client_id = CID, base = b_prov))
ok("3 proveedores: the new row is saved under the company's initials now (NEW), no OLD left in the table",
   "p3" %in% leer(S3_KEYS$proveedores)$id && !"OLD" %in% ini_prov(leer(S3_KEYS$proveedores)))
nuevo <- dplyr::bind_rows(b_pv, fila(.schema_pasivos_provision, id = "v3", empresa = "OLD", parte = "Agua"))
suppressMessages(save_pasivos_provisions(nuevo, client_id = CID, base = b_pv))
v <- leer(S3_KEYS$pasivos_provisions)
ok("3 pasivos: the new provision is saved and no row names an initial empresas.rds no longer has",
   "v3" %in% v$id && all(v$empresa %in% iniciales_vivas()))

# ── CM40: the cascade alone, never reserved, is refused ─────────────────────
sembrar(); n0 <- .mem_escrituras
e <- tryCatch({ suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID)); NULL },
              error = function(e) e)
ok("CM40 without a reservation the cascade stops: nothing written, still OLD",
   !is.null(e) && grepl("was not reserved", conditionMessage(e), fixed = TRUE) &&
     .mem_escrituras == n0 && "OLD" %in% iniciales_vivas())

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
