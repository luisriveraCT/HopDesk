# =============================================================================
# R/erp_connector_registry.R
# Extension point for ERP connectivity (Stage 3). Mirrors R/tier_registry.R's
# role from Stage 1 — one place that defines what fields an ERP type needs,
# so the UI form and validation are generated from it rather than hardcoded
# per type.
#
# ERP_CONNECTOR_REGISTRY structure per erp_type:
#   label          — display name shown in the "add connection" type picker
#   config_fields  — list of list(name=, label=, type=("text"|"checkbox"),
#                    required=) for the NON-secret fields (rendered into
#                    the erp_connections.rds `config` column)
#   secret_fields  — list of list(name=, label=, type=("text"|"password"))
#                    for the fields that go into `secrets_encrypted`
#   test_fn        — name (character) of the function that performs a live
#                    test given a resolved (decrypted) credential + config,
#                    returns list(ok = TRUE/FALSE, message = "...")
#   homologate_fn  — name (character) of the function that turns this ERP's
#                    raw API response into HopDesk's own canonical invoice
#                    row shape (Documento, Factura, Parte, Moneda, `Saldo
#                    vencido`, etc. — see R/sap_api.R's .parse_invoices() for
#                    the shape every ledger/calendar/Bancos-matching code
#                    downstream already expects, regardless of source). This
#                    is the one function a new connector MUST implement to be
#                    usable — everything else in this app reads the
#                    canonical shape, never a specific ERP's own field names.
# =============================================================================

# Stable identifier for the SAP Business One connector -- referenced instead
# of a second hardcoded "sap_b1_service_layer" literal anywhere code needs to
# check or write "this row came from SAP specifically" (e.g. the SAP-named
# field-override feature in R/data_pipeline.R, which is deliberately
# SAP-specific by name, not yet a generic ERP-override mechanism).
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"

ERP_CONNECTOR_REGISTRY <- setNames(list(
  list(
    label = "SAP Business One — Service Layer",
    config_fields = list(
      list(name = "url",        label = "Service Layer URL",     type = "text",     required = TRUE),
      list(name = "company",    label = "Company (CompanyDB)",   type = "text",     required = TRUE),
      list(name = "ssl_verify", label = "Verificar certificado SSL", type = "checkbox", required = FALSE)
    ),
    secret_fields = list(
      list(name = "user",     label = "Usuario",    type = "text"),
      list(name = "password", label = "Contraseña", type = "password")
    ),
    test_fn       = "test_sap_b1_service_layer_connection",
    homologate_fn = ".parse_invoices"
  )
), SAP_B1_ERP_TYPE)

# ── Add a new ERP type here ─────────────────────────────────────────────
# ERP_CONNECTOR_REGISTRY[["your_new_type"]] <- list(
#   label = "...",
#   config_fields = list(...),
#   secret_fields = list(...),
#   test_fn = "your_test_function_name",
#   homologate_fn = "your_homologate_function_name"
# )
# Then implement both functions near wherever that ERP's API client lives (a
# new R/<erp>_api.R, following R/sap_api.R's shape) -- homologate_fn's job is
# solely to map that ERP's raw fields onto the canonical row shape described
# above; it must never invent new output columns other code doesn't expect.

# ── Lookup helpers ─────────────────────────────────────────────────────────────
# Mirrors tier_registry.R's tier_rank()/is_staff_tier() style: everything else
# reads through these instead of poking at ERP_CONNECTOR_REGISTRY directly.

erp_connector_types <- function() names(ERP_CONNECTOR_REGISTRY)

erp_connector_label <- function(erp_type) {
  entry <- ERP_CONNECTOR_REGISTRY[[erp_type]]
  if (is.null(entry)) NA_character_ else entry$label
}

erp_connector_config_fields <- function(erp_type) ERP_CONNECTOR_REGISTRY[[erp_type]]$config_fields

erp_connector_secret_fields <- function(erp_type) ERP_CONNECTOR_REGISTRY[[erp_type]]$secret_fields

erp_connector_test_fn_name <- function(erp_type) ERP_CONNECTOR_REGISTRY[[erp_type]]$test_fn

erp_connector_homologate_fn_name <- function(erp_type) ERP_CONNECTOR_REGISTRY[[erp_type]]$homologate_fn

#' Server-side access gate for add/edit/test/delete ERP-connection actions
#' (Stage 3 spec §5 "Access"). Reuses tier_rank()/is_staff_tier() from
#' R/tier_registry.R rather than deciding staff-ness independently — the
#' rule Stage 1 established for every other gate in the app.
#'
#' Allowed: a client's own `dev`-tier user, or a Hopdesk staff session that
#' is currently mid-jump into a client. Never a bare finance/analysis/admin
#' client user, and never a staff session sitting at home (jumped = FALSE)
#' even though hopdesk/principal outrank dev — being staff never grants a
#' standing bypass (ARCHITECTURE.md §1). This is the actual enforcement;
#' the UI only hides the option, it never itself decides access.
erp_access_allowed <- function(tier, is_staff, jumped, usuario = NULL) {
  if (isTRUE(is_staff)) return(isTRUE(jumped))
  if (!is.null(usuario)) return(puede(usuario, "can_configure_erp"))
  isTRUE(permisos_resueltos(tier)[["can_configure_erp"]])
}

#' Runs the registered test_fn for `erp_type` against `config` (a named list
#' of non-secret fields) + `secrets` (a named list of decrypted secret
#' fields). Never called directly with encrypted/raw ciphertext — the
#' caller is responsible for decrypt_secret()-ing first.
run_erp_connection_test <- function(erp_type, config, secrets) {
  fn_name <- erp_connector_test_fn_name(erp_type)
  if (is.null(fn_name) || !exists(fn_name, mode = "function")) {
    return(list(ok = FALSE, message = paste0("Unknown erp_type or missing test_fn: ", erp_type)))
  }
  fn <- get(fn_name, mode = "function")
  fn(config, secrets)
}
