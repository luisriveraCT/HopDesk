# =============================================================================
# tests/test_t70_t71_fin_nota.R
# T70: a regular or tarjeta liability may end (fecha_vence; NA = no end).
# T71: a liability's note reaches every provision it generates.
# Real engine and real wizard save, on in-memory S3 (tests/helpers/s3_memoria.R).
#   A  engine: nothing after the end date; no end = exactly as before; every
#      provision is born with the liability's note.
#   B  draft <-> liability round trip; validation (end before start).
#   C  the Revisión's notice before saving: how many go to the Papelera, how
#      many converted stay, how many take the new note.
#   D  editing with an earlier end date: the pending provisions after it are
#      saved as "deleted", archived in the Papelera and logged one by one;
#      converted ones stay; removing the end brings back what is missing,
#      never a deleted one.
#   E  editing the note: the pending provisions with the old note (or none)
#      take the new one; one with its own note keeps it.
# Run from project root:  Rscript tests/test_t70_t71_fin_nota.R
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/monedas.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R", "R/pasivos_edit_confirm_module.R", "R/zona.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_list_module.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")
has_capability    <- function(user, cap) TRUE
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
AUDIT <- list()
pasivos_log_audit <- function(...) { AUDIT[[length(AUDIT) + 1L]] <<- list(...); invisible(NULL) }
PAPELERA <- NULL
load_papelera  <- function(...) PAPELERA %||% data.frame()
add_to_papelera <- function(df, detail, ledger, deleted_by) rbind(df, cbind(detail, ledger = ledger, deleted_by = deleted_by))
save_papelera  <- function(df, ...) { PAPELERA <<- df; invisible(TRUE) }
pasivos_edit_confirm_open <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
CID <- "acme"
HOY <- Sys.Date()
borrador <- function(...) modifyList(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = HOY, amount_default = 100, notas = "Pagar por transferencia"), list(...))
liab0 <- pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana"); liab0$id <- "L1"

# ── A: engine ────────────────────────────────────────────────────────────────
cat("\n-- A. engine --\n")
p_sin <- pasivos_generate_provisions(liab0, window_start = HOY)
ok(nrow(p_sin) >= 20L, sprintf("A: no end date: %d monthly provisions over two years", nrow(p_sin)))
ok(all(p_sin$notas == "Pagar por transferencia"), "A: every provision is born with the liability's note")
fin <- HOY + 140
l_fin <- liab0; l_fin$fecha_vence <- fin
p_fin <- pasivos_generate_provisions(l_fin, window_start = HOY)
ok(nrow(p_fin) > 0L && all(p_fin$fecha_calculada <= fin), sprintf("A: with an end date (%s): none after it (%d left)", fin, nrow(p_fin)))
base_motor <- new.env()
for (e in parse(text = system2("git", c("show", "29d829e:R/pasivos_engine.R"), stdout = TRUE), keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) eval(e, base_motor)
l_sin_nota <- liab0; l_sin_nota$notas <- NA_character_
antes <- base_motor$pasivos_generate_provisions(l_sin_nota, window_start = HOY)
ahora <- pasivos_generate_provisions(l_sin_nota, window_start = HOY)
cols <- setdiff(names(ahora), c("id", "generated_at"))
ok(identical(as.data.frame(antes[, cols]), as.data.frame(ahora[, cols])),
   "A: no end and no note: the same provisions as the engine of 29d829e (before T70/T71)")
l_fin_fin <- l_fin; l_fin_fin$categoria <- "financiero"
ok(identical(pasivos_fecha_fin(l_fin), fin) && nrow(pasivos_provisiones_tras_fin(p_sin, l_fin_fin)) == 0L,
   "A: a financiero's fecha_vence is its term: never 'after the end' deletions")

# ── B: round trip and validation ─────────────────────────────────────────────
cat("\n-- B. draft and validation --\n")
d <- pasivos_liability_to_draft(l_fin)
ok(identical(d$fecha_fin, fin) && identical(d$notas, "Pagar por transferencia"), "B: liability -> draft keeps end date and note")
l2 <- pasivos_wizard_draft_to_liability(d, mode = "edit", edit_target_id = "L1", user = "ana", previo = l_fin)
ok(identical(as.Date(l2$fecha_vence), fin), "B: draft -> liability stores the end date in fecha_vence")
ok(is.na(pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana")$fecha_vence), "B: blank end date -> NA (no end)")
e <- pasivos_wizard_validate_recurrencia(borrador(fecha_fin = HOY - 1))$errors
ok(identical(e$fecha_fin, tr("tablas.pasivos.fin.antes_inicio")), "B: an end date before the start is an error")
ok(is.null(pasivos_wizard_validate_recurrencia(borrador(fecha_fin = NA))$errors$fecha_fin), "B: blank is fine")
ok(identical(.wiz_leer_fecha(NULL), as.Date(NA)) && identical(.wiz_leer_fecha(character()), as.Date(NA)) &&
     identical(.wiz_leer_fecha("2027-01-31"), as.Date("2027-01-31")), "B: the dateInput's value, blank -> NA")
h <- paste(as.character(pasivos_fin_ui(list(fecha_fin = NA))), collapse = "")
ok(grepl('id="pwiz_fecha_fin"', h, fixed = TRUE) && grepl('data-initial-date=""|data-initial-date ', h) &&
     grepl("pwiz_fecha_fin_ayuda", h, fixed = TRUE), "B: the step draws an empty end date and its help line")

# ── C: the Revisión's notice ─────────────────────────────────────────────────
cat("\n-- C. notice before saving --\n")
provs <- p_sin
provs$estado[2] <- "converted"                                   # a converted one, late in the year
provs$fecha_efectiva[2] <- HOY + 200
provs$notas[3] <- "Ojo: pagar en efectivo"                        # a note of its own
a <- pasivos_aviso_edicion(borrador(fecha_fin = fin, notas = "Nueva nota"), "L1", provs, liab0)
n_tras <- sum(provs$estado == "provisional" & provs$fecha_efectiva > fin)
ok(identical(a$papelera, n_tras), sprintf("C: %d pending after the end go to the Papelera", n_tras))
ok(identical(a$ya_convertidas, 1L), "C: one converted after the end stays")
ok(identical(unname(a$nota[["m"]]), 1L) && identical(unname(a$nota[["n"]]), sum(provs$estado == "provisional") - 1L),
   "C: the new note reaches the pending ones but the one with its own note")
h <- paste(as.character(pasivos_aviso_edicion_html(borrador(fecha_fin = fin, notas = "Nueva nota"), "L1", provs, liab0)), collapse = "")
ok(grepl(tr("tablas.pasivos.fin.papelera", n = n_tras), h, fixed = TRUE), "C: the notice says how many go to the Papelera")
ok(!length(pasivos_aviso_edicion(borrador(), "L1", p_sin, liab0)), "C: same note and no end: nothing to say")

# ── D/E: the real save ───────────────────────────────────────────────────────
nuevo_shared <- function(liab, provs) {
  L <- reactiveVal(liab); P <- reactiveVal(provs)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo"),
       holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function(liab, provs) {
  s3_memoria_reset()
  save_pasivos_liabilities(liab, client_id = CID)
  .s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
}
guardar <- function(sh, liab, cambios) {
  draft <- modifyList(pasivos_liability_to_draft(liab), cambios)
  wiz <- list(mode = "edit", edit_target_id = liab$id, draft = draft)
  res <- NULL
  testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, sh, session, "ana"), {})
  res
}
cat("\n-- D. earlier end date --\n")
sembrar(liab0, provs); AUDIT <- list(); PAPELERA <- NULL
sh <- nuevo_shared(liab0, provs)
res <- guardar(sh, liab0, list(fecha_fin = fin))
s3 <- load_pasivos_provisions(client_id = CID)
borradas <- s3$id[s3$estado == "deleted"]
ok(isTRUE(res$ok), "D: the save succeeds")
ok(length(borradas) == n_tras && all(s3$fecha_efectiva[s3$estado == "deleted"] > fin), "D: exactly the pending ones after the end are 'deleted' on S3")
ok(!any(s3$estado == "provisional" & s3$fecha_efectiva > fin), "D: no pending provision is left after the end")
ok(identical(s3$estado[s3$id == provs$id[2]], "converted"), "D: the converted one after the end is untouched")
ok(!is.null(PAPELERA) && setequal(PAPELERA$id, borradas), "D: each one is archived in the Papelera")
ev <- vapply(AUDIT, function(x) x$action_type, character(1))
ok(sum(ev == "provision.deleted") == length(borradas) && "liability.edited" %in% ev, "D: one provision.deleted each, plus liability.edited")
liab1 <- load_pasivos_liabilities(client_id = CID); liab1 <- liab1[liab1$id == "L1", ]
ok(identical(as.Date(liab1$fecha_vence), fin), "D: the liability keeps its end date")
res <- guardar(nuevo_shared(liab1, s3), liab1, list(fecha_fin = NA))
s3b <- load_pasivos_provisions(client_id = CID)
ok(isTRUE(res$ok) && all(s3b$estado[s3b$id %in% borradas] == "deleted"), "D: removing the end never brings a deleted one back")
cat("\n-- E. new note --\n")
sembrar(liab0, provs)
res <- guardar(nuevo_shared(liab0, provs), liab0, list(notas = "Nueva nota"))
s3 <- load_pasivos_provisions(client_id = CID)
pend <- s3[s3$estado == "provisional", ]
ok(identical(s3$notas[s3$id == provs$id[3]], "Ojo: pagar en efectivo"), "E: the provision with its own note keeps it")
ok(all(pend$notas[pend$id != provs$id[3]] == "Nueva nota"), "E: every other pending one takes the new note")
ok(identical(s3$notas[s3$id == provs$id[2]], provs$notas[2]), "E: the converted one keeps its note")

cat("\n-- F. T70b: what the Revisión says saving generates --\n")
ok(startsWith(pasivos_aviso_generacion(borrador(fecha_fin = NA)), tr("tablas.pasivos.revision.genera_24m")), "F: no end: the next 24 months")
ok(grepl(format(fin, "%d/%m/%Y"), pasivos_aviso_generacion(borrador(fecha_fin = fin)), fixed = TRUE) &&
     !grepl("24", pasivos_aviso_generacion(borrador(fecha_fin = fin)), fixed = TRUE), "F: an end date within two years: up to it, never '24 meses'")
ok(startsWith(pasivos_aviso_generacion(borrador(fecha_fin = HOY + 1000)), tr("tablas.pasivos.revision.genera_24m")), "F: an end date after two years: still the next 24 months")
ok(startsWith(pasivos_aviso_generacion(list(categoria = "financiero")), tr("tablas.pasivos.revision.genera_plazo")), "F: a financiero: up to the end of its term")
ok(!grepl("24", tr("tablas.pasivos.revision.genera_plazo"), fixed = TRUE), "F: the term text never says 24 months")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
