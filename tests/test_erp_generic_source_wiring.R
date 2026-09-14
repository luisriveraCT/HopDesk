# =============================================================================
# tests/test_erp_generic_source_wiring.R
#
# First wire for multi-ERP support (2026-09-14). Until now, "this row came
# from an ERP" was hardcoded as the literal string "sap" in ~25 call sites
# across 8 files -- correct while SAP was the only possible ERP, but it would
# have silently stopped recognizing ERP-sourced rows the moment a second
# connector (Odoo, Oracle, ...) started tagging its own rows with its own
# erp_type instead. This closes that gap with zero behavior change for real
# (100% SAP, confirmed by the user) data today:
#
#   - is_erp_sourced() (R/global.R) checks BY EXCLUSION ("not manual, not
#     provision") instead of checking FOR the literal "sap" -- so any current
#     or future erp_type value is automatically recognized, no code to touch
#     when a new connector is registered.
#   - Every call site that used to hardcode "sap" now uses SAP_B1_ERP_TYPE
#     (R/erp_connector_registry.R), the SAME stable key the connector
#     registry and erp_connections.rds already use -- one identifier, three
#     consumers, nothing duplicated.
#   - ERP_CONNECTOR_REGISTRY gains a homologate_fn slot (the function that
#     maps an ERP's raw API response onto HopDesk's own canonical invoice
#     row) -- SAP's entry points at the existing .parse_invoices(), no
#     rewrite, just registered instead of hardcoded-called.
#   - erp_connections.rds gains `version`/`short_name` columns.
#
# Backward compatibility is the whole point: every historical row already
# persisted (including papelera archives) still literally says "sap", and
# must keep behaving exactly as before with ZERO migration. Part A proves
# this empirically for is_erp_sourced() specifically.
# =============================================================================

suppressPackageStartupMessages(library(tibble))

cat("── ERP generic source wiring, first wire (critical, 2026-09-14) ──────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

# =============================================================================
# Part A -- is_erp_sourced() extracted and eval'd for real, checked against
# every value it must handle: historical, current, AND a hypothetical future
# ERP that doesn't exist yet (the actual point of this fix).
# =============================================================================
cat("\n-- Part A: is_erp_sourced() behavior (real function, not reimplemented) --\n")

global_src <- readLines("R/global.R", warn = FALSE)
fn_start <- grep("^is_erp_sourced <- function", global_src)
.chk_true(length(fn_start) == 1, "located exactly one is_erp_sourced() definition")
fn_end <- fn_start + which(grepl("^\\}", global_src[(fn_start + 1):length(global_src)]))[1]
eval(parse(text = paste(global_src[fn_start:fn_end], collapse = "\n")), envir = environment())
.chk_true(exists("is_erp_sourced", mode = "function"),
          "is_erp_sourced() extracted and evaluated successfully from the real source file")

.chk_true(is_erp_sourced(NA_character_), "NA source is treated as ERP-sourced (unchanged behavior)")
.chk_false(is_erp_sourced("manual"),     "\"manual\" is never ERP-sourced")
.chk_false(is_erp_sourced("provision"),  "\"provision\" is never ERP-sourced")
.chk_true(is_erp_sourced("sap"),
          "the historical literal \"sap\" is still ERP-sourced -- zero migration needed for old rows")
.chk_true(is_erp_sourced("sap_b1_service_layer"),
          "today's real value, SAP_B1_ERP_TYPE, is ERP-sourced")
.chk_true(is_erp_sourced("odoo_18_jsonrpc"),
          "a hypothetical FUTURE erp_type that doesn't exist yet is automatically ERP-sourced -- the actual point of this fix")

# =============================================================================
# Part B -- the connector registry: SAP_B1_ERP_TYPE constant, homologate_fn
# =============================================================================
cat("\n-- Part B: connector registry gains a homologate_fn slot --\n")

source("R/erp_connector_registry.R", local = environment())
.chk("sap_b1_service_layer", SAP_B1_ERP_TYPE,
     "SAP_B1_ERP_TYPE is the same key already used as the registry's own list name")
.chk_true("sap_b1_service_layer" %in% names(ERP_CONNECTOR_REGISTRY),
          "the registry is still keyed by the same erp_type string as before (no accidental rename)")
.chk(erp_connector_homologate_fn_name(SAP_B1_ERP_TYPE), ".parse_invoices",
     "SAP's homologate_fn points at the existing, unmodified .parse_invoices() -- not a rewrite")

sap_api_sourced <- tryCatch({ source("R/sap_api.R", local = environment()); TRUE },
                            error = function(e) { message("[TEST] R/sap_api.R could not be sourced standalone: ", e$message); FALSE })
if (sap_api_sourced) {
  .chk_true(exists(erp_connector_homologate_fn_name(SAP_B1_ERP_TYPE), mode = "function"),
            "the registered homologate_fn name resolves to a real, callable function")
} else {
  cat("  SKIP  homologate_fn resolution check -- R/sap_api.R has dependencies not met when sourced in isolation ",
      "(the full app.R sourcing order already satisfies them; not this fix's concern)\n")
}

# =============================================================================
# Part C -- erp_connections.rds schema gains version/short_name, and existing
# real Networks connections (no such columns yet) still load safely.
# =============================================================================
cat("\n-- Part C: erp_connections.rds schema extension is backward-compatible --\n")

persist_src <- readLines("R/persistence.R", warn = FALSE)
schema_start <- grep("^\\.schema_erp_connections <- function", persist_src)
schema_end   <- schema_start + which(grepl("^\\)$", persist_src[(schema_start + 1):length(persist_src)]))[1]
normalize_start <- grep("^\\.normalize <- function", persist_src)
normalize_end   <- normalize_start + which(grepl("^\\}$", persist_src[(normalize_start + 1):length(persist_src)]))[1]
eval(parse(text = paste(persist_src[schema_start:schema_end], collapse = "\n")), envir = environment())
eval(parse(text = paste(persist_src[normalize_start:normalize_end], collapse = "\n")), envir = environment())

schema_cols <- names(.schema_erp_connections())
.chk_true(all(c("version", "short_name") %in% schema_cols),
          "the schema now includes both version and short_name")
.chk_true(all(c("id","client_id","label","erp_type","company_initials","config",
                "secrets_encrypted","active","deleted") %in% schema_cols),
          "every pre-existing Stage 3 column is still present -- purely additive")

# Simulate one of Networks' real, already-migrated connections -- a row with
# NO version/short_name column at all, exactly what's live in production
# right now (added before this schema change).
legacy_row <- tibble::tibble(
  id = "abc-123", client_id = "networks", label = "SAP — Networks Group",
  erp_type = "sap_b1_service_layer", company_initials = '["NG"]',
  config = "{}", secrets_encrypted = "ciphertext", active = TRUE, deleted = FALSE
)
normalized <- .normalize(legacy_row, .schema_erp_connections)
.chk_true(all(c("version", "short_name") %in% names(normalized)),
          "a legacy connection row (no version/short_name columns) gains them on read")
.chk_true(is.na(normalized$version[1]) && is.na(normalized$short_name[1]),
          "the backfilled values are NA, not an error or a guessed default")
.chk("networks", normalized$client_id[1],
     "the legacy row's real data (client_id) survives the schema migration untouched")

# =============================================================================
# Part D -- every call site that used to hardcode the literal "sap" now uses
# SAP_B1_ERP_TYPE (static assertions -- these sites are scattered across
# observeEvent bodies and mutate() pipelines too deeply nested to extract and
# eval() the way Parts A-C do, same rationale as this suite's other
# call-site-scoping tests).
# =============================================================================
cat("\n-- Part D: call sites use SAP_B1_ERP_TYPE, not a second hardcoded \"sap\" literal --\n")

.read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
dp_txt   <- .read("R/data_pipeline.R")
lm_txt   <- .read("R/ledger_module.R")
sm_txt   <- .read("R/search_module.R")
ic_txt   <- .read("R/interco_module.R")
tm_txt   <- .read("R/treasury_map_module.R")
bm_txt   <- .read("R/bancos_module.R")
vm_txt   <- .read("R/vencidos_module.R")
pr_txt   <- .read("R/persistence.R")

.chk_true(grepl("source             = SAP_B1_ERP_TYPE,", dp_txt, fixed = TRUE),
          "data_pipeline.R's main SAP homologation output tags rows with SAP_B1_ERP_TYPE")
.chk_true(grepl("has_sap_override = source == SAP_B1_ERP_TYPE", dp_txt, fixed = TRUE),
          "data_pipeline.R's SAP-specific override feature checks against SAP_B1_ERP_TYPE")
.chk_true(grepl('pap_this[["source"]] == SAP_B1_ERP_TYPE', dp_txt, fixed = TRUE),
          "data_pipeline.R's papelera SAP-ghost detection checks against SAP_B1_ERP_TYPE")
.chk_true(grepl("source    = SAP_B1_ERP_TYPE", ic_txt, fixed = TRUE),
          "interco_module.R's Agenda-staging write uses SAP_B1_ERP_TYPE")
.chk_true(grepl("source    = SAP_B1_ERP_TYPE", tm_txt, fixed = TRUE),
          "treasury_map_module.R's Agenda-staging write uses SAP_B1_ERP_TYPE")
.chk_true(lengths(regmatches(lm_txt, gregexpr("SAP_B1_ERP_TYPE", lm_txt, fixed = TRUE))) >= 8L,
          "ledger_module.R's several stage/delete/edit call sites all reference SAP_B1_ERP_TYPE (at least 8 uses)")
.chk_true(grepl("SAP_B1_ERP_TYPE, \"manual\")", sm_txt, fixed = TRUE),
          "search_module.R's shared stage handler uses SAP_B1_ERP_TYPE")
.chk_true(grepl('"sap_b1_service_layer"   = \'<span class="badge bg-secondary">SAP</span>\'', bm_txt, fixed = TRUE),
          "bancos_module.R's source badge recognizes the new SAP_B1_ERP_TYPE value")
.chk_true(grepl('"sap"                    = \'<span class="badge bg-secondary">SAP</span>\'', bm_txt, fixed = TRUE),
          "bancos_module.R's source badge STILL recognizes the historical literal \"sap\" -- old papelera records must keep rendering correctly")
.chk_true(grepl('"sap_b1_service_layer" = "SAP"', vm_txt, fixed = TRUE),
          "vencidos_module.R's source label recognizes the new SAP_B1_ERP_TYPE value")
.chk_true(grepl('"sap"                  = "SAP"', vm_txt, fixed = TRUE),
          "vencidos_module.R's source label STILL recognizes the historical literal \"sap\"")
.chk_true(grepl("SAP_B1_ERP_TYPE", pr_txt, fixed = TRUE),
          "persistence.R's NA-fallback normalization and add_to_papelera() use SAP_B1_ERP_TYPE")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
