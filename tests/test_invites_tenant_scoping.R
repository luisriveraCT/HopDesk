# =============================================================================
# tests/test_invites_tenant_scoping.R
# Covers the 2026-10-27 fix to the Invitaciones tab, reported live by Mouse
# from a client's `dev` account:
#
#   1. The "Enviar" button did NOTHING. The UI let `dev` see the form
#      (can_invite includes is_dev) but the server handler required
#      hopdesk/principal, so req() aborted in silence -- no error, no log,
#      no clue.
#   2. That same account could pick ANY client as "Cliente destino",
#      including "Hopdesk Internal", i.e. plant an account inside another
#      tenant or inside hd-admin.
#   3. It also saw every other tenant's pending invitations (emails, names,
#      tiers) and the global hd-admin email queue.
#
# These are source-level assertions on the real file, in the same style as
# test_moves_save_failure_visibility.R: the handlers are Shiny observers that
# can't be invoked headlessly, but the guards they must contain are checkable.
# Sourced by _run_saas.R.
# =============================================================================

cat("── Invitaciones: alcance por tenant (2026-10-27) ────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

src <- paste(readLines("R/tiers_module.R", warn = FALSE), collapse = "\n")

# ── A. El allowlist de tiers existe y es compartido ──────────────────────────

.chk(exists(".INVITABLE_TIERS"), TRUE, ".INVITABLE_TIERS está definido")
.chk(sort(.INVITABLE_TIERS), sort(c("analysis", "finance", "admin")),
     "invitable tiers: exactamente analysis/finance/admin")
for (forbidden in c("dev", "hopdesk", "principal"))
  .chk(forbidden %in% .INVITABLE_TIERS, FALSE,
       sprintf("una invitación NO puede crear tier '%s'", forbidden))

# Una sola lista alimenta el desplegable y la validación del servidor: si
# alguien agrega un tier al selectInput, el check del servidor lo sigue solo.
.chk(grepl('choices = .INVITABLE_TIERS', src, fixed = TRUE), TRUE,
     "el selectInput de tier usa .INVITABLE_TIERS, no una lista literal")
.chk(grepl('tier_in %in% .INVITABLE_TIERS', src, fixed = TRUE), TRUE,
     "el servidor valida el tier contra .INVITABLE_TIERS")
.chk(grepl('choices = c("analysis", "finance", "admin")', src, fixed = TRUE), FALSE,
     "ya no queda la lista de tiers duplicada a mano")

# ── B. El gate del botón coincide con el de la UI ────────────────────────────

.inside <- function(anchor, n = 2200) {
  i <- regexpr(anchor, src, fixed = TRUE)
  if (i < 0) return("")
  substr(src, i, i + n)
}

send_h <- .inside("observeEvent(input$btn_send_invite, {")
.chk(nzchar(send_h), TRUE, "se localizó el handler de btn_send_invite")
.chk(grepl("is_dev()", send_h, fixed = TRUE), TRUE,
     "el handler admite is_dev() -- ESTE era el botón que no hacía nada")
.chk(grepl("req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))", send_h, fixed = TRUE),
     FALSE, "ya no queda el gate viejo que excluía a dev")

ui_gate <- .inside("can_invite <- ", 160)
for (who in c("is_hopdesk()", "is_principal()", "is_dev()")) {
  .chk(grepl(who, ui_gate, fixed = TRUE), TRUE,
       sprintf("la UI admite %s", who))
  .chk(grepl(who, send_h, fixed = TRUE), TRUE,
       sprintf("el servidor admite %s (los dos gates coinciden)", who))
}

# ── C. El cliente destino se decide en el servidor ───────────────────────────

.chk(grepl("viewer_is_staff", send_h, fixed = TRUE), TRUE,
     "el handler consulta viewer_is_staff() para decidir el alcance")
.chk(grepl("current_client_id()", send_h, fixed = TRUE), TRUE,
     "el handler usa current_client_id() como destino para quien no es staff")
# La regresión exacta: tomar el cliente del navegador sin condicionar.
.chk(grepl("client_in  <- input$inv_client_id", src, fixed = TRUE), FALSE,
     "ya NO se toma inv_client_id del navegador sin condicionar")

# ── D. viewer_is_staff() falla cerrado ───────────────────────────────────────

vs <- .inside("viewer_is_staff <- reactive(", 260)
.chk(nzchar(vs), TRUE, "se localizó viewer_is_staff()")
.chk(grepl("isTRUE(", vs, fixed = TRUE), TRUE,
     "viewer_is_staff(): isTRUE() -- un NULL no cuenta como staff")
.chk(grepl("error = function(e) FALSE", vs, fixed = TRUE), TRUE,
     "viewer_is_staff(): si la identidad no resuelve, NO es staff (fail-closed)")

# ── E. Revocar y reintentar están acotados ───────────────────────────────────

rev_h <- .inside("observeEvent(input$inv_revoke_token, {", 1400)
.chk(grepl("is_dev()", rev_h, fixed = TRUE), TRUE,
     "revocar: tiene gate de tier")
.chk(grepl("identical(inv_client, current_client_id())", rev_h, fixed = TRUE), TRUE,
     "revocar: un cliente no puede revocar la invitación de otro tenant")

retry_h <- .inside("observeEvent(input$email_retry_id, {", 700)
.chk(grepl("req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))", retry_h, fixed = TRUE),
     TRUE, "reintentar email: staff-only (la cola es global de hd-admin)")

retry_all <- .inside("observeEvent(input$btn_email_retry_all, {", 500)
.chk(grepl("req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))", retry_all, fixed = TRUE),
     TRUE, "reintentar todo: staff-only")

# ── F. La UI filtra lo que un cliente puede ver ──────────────────────────────

ui_blk <- .inside("staff_view <- isTRUE(viewer_is_staff())", 1500)
.chk(nzchar(ui_blk), TRUE, "la UI calcula staff_view")
.chk(grepl("invites$client_id == own_client", ui_blk, fixed = TRUE), TRUE,
     "la tabla de pendientes se filtra al propio cliente cuando no es staff")
.chk(grepl("staff_view && nrow(pending_q)", src, fixed = TRUE), TRUE,
     "el panel de cola de emails es staff-only")

# ── G. La lógica de filtrado, ejercitada de verdad ───────────────────────────
# El bug no fue sutil: la tabla listaba el archivo global completo.

inv <- data.frame(
  token        = c("t1", "t2", "t3", "t4"),
  email        = c("a@acme.com", "b@otro.com", "c@acme.com", "d@hd.com"),
  display_name = c("A", "B", "C", "D"),
  client_id    = c("acme", "otro", "acme", "hd-admin"),
  tier         = rep("analysis", 4),
  invited_by   = rep("x", 4),
  created_at   = rep("2026-10-27T00:00:00Z", 4),
  expires_at   = rep("2026-10-29T00:00:00Z", 4),
  accepted_at  = rep(NA_character_, 4),
  status       = rep("pending", 4),
  stringsAsFactors = FALSE
)

.scope <- function(invites, is_staff, own) {
  if (isTRUE(is_staff)) invites
  else invites[!is.na(invites$client_id) & invites$client_id == own, , drop = FALSE]
}

acme_view <- .scope(inv, FALSE, "acme")
.chk(nrow(acme_view), 2L, "cliente 'acme' ve sólo sus 2 invitaciones")
.chk(sort(acme_view$email), sort(c("a@acme.com", "c@acme.com")),
     "cliente 'acme' ve exactamente sus correos")
.chk("b@otro.com" %in% acme_view$email, FALSE,
     "cliente 'acme' NO ve el correo de otro tenant")
.chk("d@hd.com" %in% acme_view$email, FALSE,
     "cliente 'acme' NO ve las invitaciones de hd-admin")
.chk(nrow(.scope(inv, TRUE, "hd-admin")), 4L, "staff sigue viendo todo")

# Un client_id NA no debe colarse a nadie.
inv_na <- inv; inv_na$client_id[1] <- NA_character_
.chk(nrow(.scope(inv_na, FALSE, "acme")), 1L,
     "una fila con client_id NA no se le muestra a ningún cliente")

# ── H. El destino forzado ignora lo que diga el navegador ────────────────────

.target <- function(is_staff, from_browser, own) {
  if (isTRUE(is_staff)) trimws(from_browser %||% "") else own
}
.chk(.target(FALSE, "hd-admin", "acme"), "acme",
     "no-staff: un inv_client_id forjado a 'hd-admin' se ignora")
.chk(.target(FALSE, "otro", "acme"), "acme",
     "no-staff: un inv_client_id forjado a otro tenant se ignora")
.chk(.target(TRUE, "otro", "hd-admin"), "otro",
     "staff: sí puede elegir el cliente destino")
