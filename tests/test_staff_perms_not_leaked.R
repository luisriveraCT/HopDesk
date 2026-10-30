# =============================================================================
# tests/test_staff_perms_not_leaked.R
#
# A CLIENT session must never be shown Hopdesk's own internal permissions.
# Reported 2026-09-26: the user-edit form listed "Aprobar nuevos clientes",
# "Gestionar permisos de staff HopDesk", "Acceder a contextos de clientes" and
# both internal audit-log permissions to a client admin editing their own
# team. Being merely unable to grant them is not enough -- their existence
# leaks how the development company is organised internally.
#
# Also locks in the save-path half of the fix: hidden checkboxes render no
# input, so a save that looped over ALL keys read NULL for them and rewrote
# each to FALSE, silently revoking staff permissions on every client edit.
# =============================================================================

cat("\n== Permisos internos de Hopdesk no se filtran a clientes ==\n")

.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        esperado: %s\n        obtenido: %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
if (!exists("tr")) tr <- function(clave, ...) clave
source("R/tier_registry.R", local = FALSE)
source("R/permisos.R", local = FALSE)
src <- readLines("R/tiers_module.R", warn = FALSE)

INTERNAS <- c("can_approve_clients", "can_manage_hopdesk_perms", "can_jump_clients",
              "can_view_client_audit_logs", "can_view_staff_audit_log")

cliente <- perm_visible_keys(FALSE)
staff   <- perm_visible_keys(TRUE)

for (k in INTERNAS)
  .chk_true(!(k %in% cliente), sprintf("un cliente NO ve '%s'", k))
for (k in INTERNAS)
  .chk_true(k %in% staff, sprintf("staff SI ve '%s'", k))

# Las operativas deben seguir visibles para el cliente -- ocultar de más
# rompería la gestión de su propio equipo.
for (k in c("can_manage_empresas", "can_manage_invites", "can_view_payment_audit",
            "can_edit_provisiones", "can_confirm_payments", "can_view_requisiciones"))
  .chk_true(k %in% cliente, sprintf("el cliente conserva '%s'", k))

INTERNAS <- permisos_internos()
.chk(length(staff), length(permisos_claves()), "staff ve el catalogo completo")
.chk(length(cliente), length(permisos_claves()) - length(INTERNAS),
     "el cliente ve exactamente el catalogo menos las internas")
.chk_true(setequal(setdiff(staff, cliente), INTERNAS),
          "la unica diferencia entre ambas vistas son las internas")

# Fail-closed: quien olvide pasar el argumento obtiene la vista de cliente.
.chk(perm_visible_keys(NULL), cliente, "sin argumento valido -> vista de cliente (fail-closed)")
.chk(perm_visible_keys(NA),   cliente, "NA -> vista de cliente (fail-closed)")

# El guardado debe recorrer solo las visibles y preservar las ocultas.
srcf <- paste(src, collapse = "\n")
.chk_true(grepl("vis_keys  <- perm_visible_keys(viewer_is_staff)", srcf, fixed = TRUE),
          "el guardado resuelve las llaves visibles del viewer")
.chk_true(grepl("overrides <- prev_ovr[setdiff(names(prev_ovr), vis_keys)]", srcf, fixed = TRUE),
          "el guardado preserva los overrides que el viewer no pudo ver")
.chk_true(!grepl("for (k in names(PERM_LABELS)) {", srcf, fixed = TRUE),
          "ya no queda ningun bucle de guardado sobre el catalogo completo")

cat(sprintf("\n  %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_staff_perms_not_leaked.R: ", .fail, " failure(s)")
