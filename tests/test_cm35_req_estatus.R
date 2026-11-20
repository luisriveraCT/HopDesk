# =============================================================================
# tests/test_cm35_req_estatus.R
# CM35. Each requisition's status and trace (Luis, 2026-11-19).
#   1  status, deduced, never stored: sin_provision ("Abierta") ·
#      provisionada · por_pagar · en_agenda · pagada · papelera ("Cancelada").
#      "en_pago" splits in two: an item with no pending Agenda row is
#      por_pagar; with one, en_agenda. A paid one stays in the list (default)
#      with its confirmacion_id; incluir_pagadas = FALSE still hides them.
#   2  events for Panorama (T62), deduced from tables that only grow
#      (requisiciones_historial, pasivos_audit, bancos_confirmados) plus the
#      live Agenda: who, when, from -> to. The last event of each
#      requisition always lands on the status the list shows.
#   3  the trace survives: a deleted provision and a new one keep both
#      stretches; ids are stable between two reads; a confirmation written
#      before CM35 says por = NA, never a guess.
#   4  the approvals door is there and does nothing.
# Pure functions on in-memory tables (no S3).
#
# Run from project root:  Rscript tests/test_cm35_req_estatus.R
# =============================================================================
cat("=== test_cm35_req_estatus ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.tomar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres)
      eval(e, globalenv())
}
.tomar("R/global.R", "S3_KEYS")
.tomar("R/persistence.R", c(".normalize", "confirmaciones_vigentes", ".schema_manual", ".schema_pagar_hoy"))
.tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
.tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
source("R/pasivos_schemas.R")
suppressMessages(source("R/i18n.R"))
source("R/agenda_filas.R")   # CM35 A2: agenda_pendientes(), the Agenda's own "pending"
source("R/requisiciones_persistence.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
t <- function(x) as.POSIXct(x, tz = "UTC")

# ── Fixture ────────────────────────────────────────────────────────────────
# R1: created (ana) -> provision P1 linked (ana) -> converted to item and sent
#     to the Agenda (beto) -> paid (carla) -> payment undone (dani).
# R2: created -> P2 linked -> P2 deleted -> P3 linked -> converted, no Agenda
#     row (por_pagar).
# R3: created only (sin_provision). R4: created -> trashed (papelera).
# R5: created -> P5 linked -> converted -> paid by a confirmation written
#     before CM35 (no confirmado_por) -> stays paid.
# R6: created -> P6 linked -> converted -> Agenda row staged by eva from the
#     Calendario (no Pasivos audit for that step).
req <- function(id, folio, prov = NA_character_, papelera = as.POSIXct(NA, tz = "UTC"))
  tibble(id = id, folio = folio, titulo = paste("T", id), empresa = "NRS", proveedor = "ACME",
         monto = 100, moneda = "MXN", creado_por = "ana", creado = t("2026-11-01 10:00"),
         provision_id = prov, papelera_en = papelera)
reqs <- .normalize(bind_rows(req("R1", "REQ-2026-0001", "P1"), req("R2", "REQ-2026-0002", "P3"),
                             req("R3", "REQ-2026-0003"), req("R4", "REQ-2026-0004", papelera = t("2026-11-03 09:00")),
                             req("R5", "REQ-2026-0005", "P5"), req("R6", "REQ-2026-0006", "P6")),
                   .schema_requisiciones)
h <- function(rid, accion, por, en, detalle = NA_character_)
  tibble(id = paste(rid, accion, en), requisicion_id = rid, accion = accion, detalle = detalle, por = por, en = t(en))
historial <- .normalize(bind_rows(
  h("R1", "creada", "ana", "2026-11-01 10:00"), h("R1", "ligada", "ana", "2026-11-01 11:00", "P1"),
  h("R2", "creada", "ana", "2026-11-01 10:00"), h("R2", "ligada", "ana", "2026-11-01 11:00", "P2"),
  h("R2", "ligada", "ana", "2026-11-02 11:00", "P3"),
  h("R3", "creada", "ana", "2026-11-01 10:00"),
  h("R4", "creada", "ana", "2026-11-01 10:00"), h("R4", "papelera", "ana", "2026-11-03 09:00"),
  h("R5", "creada", "ana", "2026-11-01 10:00"), h("R5", "ligada", "ana", "2026-11-01 11:00", "P5"),
  h("R6", "creada", "ana", "2026-11-01 10:00"), h("R6", "ligada", "ana", "2026-11-01 11:00", "P6"),
  h("R1", "editada", "ana", "2026-11-01 12:00")), .schema_req_historial)
prov <- function(id, estado) tibble(id = id, estado = estado, amount_pago = 100, moneda_pago = "MXN",
                                    fecha_calculada = as.Date("2026-11-20"))
provs <- .normalize(bind_rows(prov("P1", "converted"), prov("P2", "deleted"), prov("P3", "converted"),
                              prov("P5", "item_confirmed"), prov("P6", "converted")), .schema_pasivos_provision)
a <- function(tipo, pid, user, ts, after = NULL)
  tibble(id = paste(tipo, pid, ts), ts = t(ts), user = user, action_type = tipo, target_kind = "provision",
         target_id = pid, payload_after = if (is.null(after)) NA_character_ else as.character(jsonlite::toJSON(after, auto_unbox = TRUE)))
audit <- .normalize(bind_rows(
  a("provision.converted_to_item", "P1", "beto", "2026-11-02 09:00", list(pagar_hoy_id = "PH1")),
  a("provision.deleted", "P2", "beto", "2026-11-02 10:00"),
  a("provision.converted_to_item", "P3", "beto", "2026-11-03 09:00", list(pagar_hoy_id = NA)),
  a("provision.converted_to_item", "P5", "beto", "2026-11-02 09:00", list(pagar_hoy_id = "PH5")),
  a("provision.converted_to_item", "P6", "beto", "2026-11-02 09:00", list(pagar_hoy_id = NA))), .schema_pasivos_audit)
cf <- function(id, pid, at, por = NA_character_, elim = FALSE, elim_at = NA, elim_por = NA_character_,
               action = "confirmed", reverses = NA_character_)
  tibble(confirmacion_id = id, provision_id = pid, importe = 100, moneda = "MXN", tipo = "pago",
         confirmado_at = t(at), confirmado_por = por, eliminado = elim, eliminado_at = t(elim_at),
         eliminado_por = elim_por, action = action, reverses_confirmacion_id = reverses)
conf <- .normalize(bind_rows(
  cf("C1", "P1", "2026-11-04 10:00", "carla", TRUE, "2026-11-05 10:00", "dani"),
  cf("C1r", "P1", "2026-11-04 10:00", "dani", action = "recovered", reverses = "C1"),
  cf("C5", "P5", "2026-11-04 10:00")), .schema_bancos_confirmados)
agenda <- .normalize(tibble(id = "PH6", ledger = "AP", Empresa = "NRS", Moneda = "MXN", Documento = "D6",
                            Importe = 100, status = "pending", provision_id = "P6",
                            staged_by = "eva", staged_at = t("2026-11-03 15:00")), .schema_pagar_hoy)
vacia <- function(f) f()[0, ]
vista <- function(...) requisiciones_vista_en(reqs, provs, conf, vacia(.schema_manual), vacia(.schema_req_carpetas),
                                              vacia(.schema_req_evidencia), vacia(.schema_req_comentarios),
                                              liabs = NULL, agenda = agenda, ...)
v  <- vista()
et <- setNames(v$etapa, v$id)

# ── 1. Status ─────────────────────────────────────────────────────────────
ok("1 the stages: sin_provision, provisionada, por_pagar, en_agenda, pagada, papelera (and the door)",
   all(c("sin_provision", "provisionada", "por_pagar", "en_agenda", "pagada", "papelera") %in% REQ_ETAPAS) &&
     !"en_pago" %in% REQ_ETAPAS)
ok("1 R1: its payment was undone, so it is back to an item (por_pagar)", identical(et[["R1"]], "por_pagar"))
ok("1 R2: the second provision is an item with no Agenda row (por_pagar)", identical(et[["R2"]], "por_pagar"))
ok("1 R3 sin_provision, R4 papelera", identical(et[["R3"]], "sin_provision") && identical(et[["R4"]], "papelera"))
ok("1 R5 is paid, stays in the view, with its confirmacion_id",
   identical(et[["R5"]], "pagada") && identical(v$confirmacion_id[v$id == "R5"], "C5"))
ok("1 R6 has a pending Agenda row: en_agenda", identical(et[["R6"]], "en_agenda"))
ok("1 the screen names: Abierta and Cancelada",
   identical(requisiciones_etapa_nombre("sin_provision"), tr("requisiciones.etapa.sin_provision")) &&
     identical(tr("requisiciones.etapa.sin_provision", idioma = "es"), "Abierta") &&
     identical(tr("requisiciones.etapa.papelera", idioma = "es"), "Cancelada"))

# ── 2. Events ─────────────────────────────────────────────────────────────
ev <- requisiciones_eventos_en(reqs, historial, audit, conf, agenda = agenda)
es <- ev[ev$tipo == "estatus", ]
camino <- function(rid) { x <- es[es$requisicion_id == rid, ]; x <- x[order(x$en), ]; paste(x$a, collapse = ">") }
ok("2 the columns T62 reads",
   all(c("id", "requisicion_id", "folio", "titulo", "empresa", "en", "por", "tipo", "de", "a",
         "fuente", "ref_id", "detalle") %in% names(ev)))
ok("2 R1: sin_provision > provisionada > en_agenda > pagada > por_pagar",
   identical(camino("R1"), "sin_provision>provisionada>en_agenda>pagada>por_pagar"))
p1 <- es[es$requisicion_id == "R1", ]; p1 <- p1[order(p1$en), ]
ok("2 R1: who did each step (ana, ana, beto, carla, dani) and the from -> to chain",
   identical(p1$por, c("ana", "ana", "beto", "carla", "dani")) &&
     identical(p1$de[-1], p1$a[-nrow(p1)]) && is.na(p1$de[1]))
ok("2 R2: the deleted provision sends it back to sin_provision, the new one goes on",
   identical(camino("R2"), "sin_provision>provisionada>sin_provision>provisionada>por_pagar"))
ok("2 R4: trashed shows as a status change to papelera", identical(camino("R4"), "sin_provision>papelera"))
ok("2 R6: the Agenda step taken from the Calendario comes from the live Agenda row (eva)",
   identical(camino("R6"), "sin_provision>provisionada>por_pagar>en_agenda") &&
     "eva" %in% es$por[es$requisicion_id == "R6" & es$a == "en_agenda"])
ultimo <- es[order(es$requisicion_id, es$en), ]
ultimo <- ultimo[!duplicated(ultimo$requisicion_id, fromLast = TRUE), ]
ok("2 the last event of every requisition lands on the status the list shows",
   identical(setNames(ultimo$a, ultimo$requisicion_id)[sort(names(et))], et[sort(names(et))]))
ok("2 edits are events too (tipo edicion), not status changes",
   any(ev$tipo == "edicion" & ev$requisicion_id == "R1") && !any(es$a %in% "editada"))

# ── 3. Trace ──────────────────────────────────────────────────────────────
ev2 <- requisiciones_eventos_en(reqs, historial, audit, conf, agenda = agenda)
ok("3 event ids are stable between two reads, and unique", identical(sort(ev$id), sort(ev2$id)) && !anyDuplicated(ev$id))
ok("3 a confirmation written before CM35 gives por = NA, never a guess",
   is.na(es$por[es$requisicion_id == "R5" & es$a == "pagada"]))
ok("3 R2 keeps both provisions in its trace (P2 and P3)",
   all(c("P2", "P3") %in% es$ref_id[es$requisicion_id == "R2"]))

# ── 3a. Each event's id carries ITS OWN stage, in a mixed batch (Ejecutor 3) ─
# fila() built the id suffix with ifelse() on a scalar tipo, so every status
# event of one batch took the first row's stage: creada, papelera and
# restaurada all ended "|sin_provision".
r7 <- .normalize(req("R7", "REQ-2026-0007"), .schema_requisiciones)
h7 <- .normalize(bind_rows(h("R7", "creada", "ana", "2026-11-01 10:00"),
                           h("R7", "ligada", "ana", "2026-11-01 11:00", "P7"),
                           h("R7", "papelera", "ana", "2026-11-02 10:00"),
                           h("R7", "restaurada", "beto", "2026-11-03 10:00"),
                           h("R7", "editada", "ana", "2026-11-03 11:00"),
                           h("R7", "comentario", "ana", "2026-11-03 12:00")), .schema_req_historial)
e7 <- requisiciones_eventos_en(r7, h7, audit[0, ], conf[0, ], agenda = NULL)
suf <- sub("^.*\\|", "", e7$id[order(e7$en)])
ok("3a a mixed batch: each id ends with its own stage or tipo (sin_provision, provisionada, papelera, restaurada, edicion, comentario)",
   identical(suf, c("sin_provision", "provisionada", "papelera", "restaurada", "edicion", "comentario")))
ok("3a …and the walk still goes sin_provision > provisionada > papelera > provisionada (restored to the stage before the trash)",
   identical(e7$a[e7$tipo == "estatus"][order(e7$en[e7$tipo == "estatus"])],
             c("sin_provision", "provisionada", "papelera", "provisionada")))

# ── 3b. "Pending" is the Agenda's own criterion (A2, T60) ─────────────────
# A row with status NA is not shown by the Agenda (agenda_pendientes()), so it
# must not make a requisition "en_agenda" either, nor give an event.
agenda_na <- agenda; agenda_na$status <- NA_character_
v_na  <- requisiciones_vista_en(reqs, provs, conf, vacia(.schema_manual), vacia(.schema_req_carpetas),
                                vacia(.schema_req_evidencia), vacia(.schema_req_comentarios),
                                liabs = NULL, agenda = agenda_na)
ev_na <- requisiciones_eventos_en(reqs, historial, audit, conf, agenda = agenda_na)
ok("3b an Agenda row with status NA (not shown by the Agenda) leaves R6 por_pagar, with no Agenda event",
   identical(v_na$etapa[v_na$id == "R6"], "por_pagar") &&
     !any(ev_na$fuente == "agenda") && nrow(agenda_pendientes(agenda_na)) == 0L)

# ── 4. The approvals door ──────────────────────────────────────────────────
ok("4 the approvals door exists and does nothing yet", exists(".req_aprobacion_requerida") &&
     identical(.req_aprobacion_requerida(reqs[1, ]), FALSE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
