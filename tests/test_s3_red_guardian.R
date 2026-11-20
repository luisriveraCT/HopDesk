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
  "R/persistence.R"         = "if (isTRUE(permitir_reduccion)) .s3_write(", # modificar_tabla / .s3_write_with pass it on
  "R/settings/settings_helpers.R" = "}, client_id = client_id, permitir_reduccion = TRUE)", # borrar_banco_si_libre: removes one bank no account uses; the last empties the catalog (CM12, CM17)
  # CM12e, each on S3's current table: Revertir (the chosen partners' dates),
  # saving a partner (its rows, then its stale dates) and Aplicar "skip_manual"/"all".
  "R/settings/settings_companies.R" = "client_id = cid, permitir_reduccion = TRUE)",
  # CM34c, on S3's current tables: deleting a policy removes that one from
  # policy_catalog.rds and its assignments from partner_policies.rds.
  "R/settings/settings_policies.R" = "client_id = cid, permitir_reduccion = TRUE)",
  # CM34c: Desvincular removes that supplier's link from S3's
  # parte_alias_map.rds (the last one empties it); Vincular never reduces.
  "R/pagar_hoy_module.R" = "client_id = shared$effective_client_id(), permitir_reduccion = TRUE),",
  # alertas_guardar() (C): unmarking every action of a user removes that
  # user's row from avisos_alertas.rds, on S3's matrix as it is now.
  "R/avisos_alertas_module.R" = "}, permitir_reduccion = TRUE)",
  # avisos_archivar() (D): the monthly archive removes from avisos.rds only
  # the avisos it already wrote to avisos/archivo/AAAA-MM.rds and read back.
  "R/avisos_archivo.R" = "}, permitir_reduccion = TRUE)",
  # correo_partidas_guardar() (D, T45): semana_partidas.rds is a window of
  # AVISOS_PARTIDAS_SEMANAS weeks; the older weeks leave, never this week's
  # or one inside the window (tests/test_t45_correos_semana.R checks it).
  "R/avisos_datos.R" = "}, permitir_reduccion = TRUE)",
  # purgar_invitaciones_vencidas() (T57): deletes from pending_invites.rds
  # only the expired invitations (pending past their expiry, or marked
  # "expired"); never a valid one, an unreadable date, accepted or revoked
  # (tests/test_admin_invitaciones_hd.R checks it).
  "R/persistence.R" = "}, permitir_reduccion = TRUE)   # T57: only the expired invitations leave"
)
usos <- do.call(rbind, lapply(archivos, function(f) {
  x <- codigo(f); i <- grep("permitir_reduccion = TRUE", x, fixed = TRUE)
  if (length(i)) data.frame(f = f, txt = x[i], stringsAsFactors = FALSE) else NULL
}))
usos <- usos[!grepl("pass permitir_reduccion = TRUE if this is intended", usos$txt, fixed = TRUE), , drop = FALSE]
# A file may have more than one documented use (T57): any of its entries.
fuera <- usos[!mapply(function(f, t) any(vapply(permitidos[names(permitidos) == f],
                                                function(p) grepl(p, t, fixed = TRUE), logical(1))),
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
.TRINQUETE <- 50L   # 2026-11-04 VP1: 74; 2026-11-19 CM34: 52 (the real count), CM34a: 50; lower it as conversions land
patron <- "tryCatch\\(\\s*load_[a-z_]+\\([^)]*\\)\\s*,\\s*error\\s*=\\s*function\\(e\\)\\s*(\\.schema_[a-z_]+\\(\\)|tibble::tibble\\(\\)|NULL)\\)"
n <- sum(vapply(archivos, function(f) sum(grepl(patron, codigo(f))), integer(1)))
cat(sprintf("  read-with-empty-fallback sites: %d (ratchet %d)\n", n, .TRINQUETE))
ok("read-with-empty-fallback sites did not go up", n <= .TRINQUETE)

# ── 5. Whole-copy saves pass `base` (no lost update between sessions) ─────────
# A save of a table built from a copy (the session's, or a read that may have
# failed) passes the copy it started from, so only this action's rows are
# applied on S3 as it is now. Known exceptions:
#   - the purge (permitir_reduccion) and persistence.R's own helpers.
# (The Agenda's confirmar x2 and "Vaciar agenda" in pagar_hoy_module.R were
# the last ones; CM33 converted them and the ratchet is 0.)
sin_base <- function(fn, excluir = character()) {
  unlist(lapply(setdiff(archivos, c("R/persistence.R", "R/bancos_persistence.R",
                                    "R/pasivos_persistence.R", excluir)), function(f) {
    x <- readLines(f, warn = FALSE); x[grepl("^\\s*#", x)] <- ""
    i <- grep(paste0(fn, "("), x, fixed = TRUE)
    i <- i[!grepl("<- function", x[i], fixed = TRUE)]
    malas <- i[!vapply(i, function(k) any(grepl("base\\s*=|permitir_reduccion = TRUE", x[k:min(length(x), k + 2L)])), logical(1))]
    if (length(malas)) sprintf("%s:%d: %s", f, malas, trimws(x[malas])) else NULL
  }))
}
for (fn in c("save_manual", "save_notes", "save_tags", "save_pasivos_provisions",
             "save_bancos_movimientos", "save_bancos_confirmados",
             "save_proveedores", "save_proveedores_inactivos",
             # CM38b: Empresas saves through modificar_tabla + rebase_por_id;
             # a direct save_empresas() of a copy must not come back.
             "save_empresas",
             # CM34c: policies are written with modificar_policy_catalog /
             # modificar_partner_policies; these two whole-copy saves are gone.
             "save_policy_catalog", "save_partner_policies", "save_parte_alias_map")) {
  m <- sin_base(fn, "R/pagar_hoy_module.R")
  for (s in m) message("  without base: ", s)
  ok(sprintf("%s(): every save built from a copy passes base (pagar_hoy_module.R aside)", fn), !length(m))
}
agenda <- function(excluir) c(sin_base("save_pagar_hoy", excluir), sin_base("save_pagar_hoy_with_retry", excluir))
m <- agenda("R/pagar_hoy_module.R")
for (s in m) message("  without base: ", s)
ok("save_pagar_hoy*(): every Agenda save outside pagar_hoy_module.R passes base", !length(m))
# Ratchet at 0 since CM33: confirmar (AP and AR) and "Vaciar agenda" pass the
# Agenda they read as `base`, like quitar. No Agenda save without it is left.
m <- agenda(setdiff(archivos, "R/pagar_hoy_module.R"))
for (s in m) message("  without base: ", s)
ok(sprintf("pagar_hoy_module.R: every Agenda save passes base (ratchet 0 since CM33; found %d)", length(m)),
   !length(m))
m <- sin_base("save_manual", setdiff(archivos, "R/pagar_hoy_module.R"))
for (s in m) message("  without base: ", s)
ok("pagar_hoy_module.R: every save_manual() passes base", !length(m))

# ── 6. Moves are always rebased (CM28) ────────────────────────────────────────
# invoice_moves.rds is written whole. A .save_moves_deferred() without
# `rebase` writes the session's copy, erasing dates another session moved
# (Vencidos and the lupa did, until CM28). No exceptions: every call passes
# rebase =. A direct save_moves() is only allowed inside .save_moves_deferred
# itself, or right after a strict read of S3_KEYS$moves in one of its three
# innermost enclosing blocks (it prunes S3's table, not the session's copy).
lectura_estricta <- "(leer_tabla_para_escribir|\\.s3_leer_para_escribir)\\(S3_KEYS\\$moves"
sin_rebase <- character(); moves_sueltos <- character()
for (f in archivos) {
  ex <- parse(f, keep.source = TRUE, encoding = "UTF-8")
  # The calls are met in source order (pre-order walk), so the k-th call
  # found is the k-th call token: its line.
  pd <- utils::getParseData(ex)
  lineas <- function(nombre) pd$line1[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == nombre]
  l_def <- lineas(".save_moves_deferred"); l_sm <- lineas("save_moves"); k_def <- 0L; k_sm <- 0L
  recorrer <- function(x, anc) {
    if (!is.call(x)) return(invisible())
    fn <- x[[1]]
    if (identical(fn, as.name(".save_moves_deferred"))) {
      k_def <<- k_def + 1L
      if (!"rebase" %in% names(as.list(x))) sin_rebase <<- c(sin_rebase, sprintf("%s:%d", f, l_def[k_def]))
    }
    if (identical(fn, as.name("save_moves"))) {
      k_sm <<- k_sm + 1L; linea <- l_sm[k_sm]
      dentro <- any(vapply(anc, function(a) identical(a[[1]], as.name("<-")) &&
                             identical(a[[2]], as.name(".save_moves_deferred")), logical(1)))
      bloques <- Filter(function(a) identical(a[[1]], as.name("{")), rev(anc))
      bloques <- bloques[seq_len(min(3L, length(bloques)))]
      leida <- any(vapply(bloques, function(b) grepl(lectura_estricta, paste(deparse(b), collapse = " ")), logical(1)))
      if (!dentro && !leida) moves_sueltos <<- c(moves_sueltos, sprintf("%s:%d", f, linea))
    }
    lapply(as.list(x)[-1], function(a) if (!missing(a)) recorrer(a, c(anc, list(x))))
  }
  for (e in ex) recorrer(e, list())
}
for (s in sin_rebase) message("  .save_moves_deferred without rebase: ", s)
ok(".save_moves_deferred(): every call passes rebase = (no exceptions)", !length(sin_rebase))
for (s in moves_sueltos) message("  save_moves() on a copy: ", s)
ok("save_moves(): only inside .save_moves_deferred or right after a strict read of S3's moves",
   !length(moves_sueltos))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
