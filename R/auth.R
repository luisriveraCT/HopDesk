# =============================================================================
# R/auth.R
# Authentication helpers for shinymanager.
# Credentials stored in S3 as usuarios.rds.
# No reactives, no UI. Called once at startup and once per login attempt.
#
# NOTE on password storage: shinymanager's data.frame credential path uses
# plain-text comparison (credentials$password == entered_password). Scrypt/bcrypt
# hashing is only supported by shinymanager's SQLite encrypted-db backend.
# Proper hashing will be added in Step 5 (User Management UI) when we control
# the full save/verify cycle. Until then, passwords in usuarios.rds are plain text.
# =============================================================================

# Returns a data.frame in the exact format shinymanager expects:
# columns: user, password, admin (logical), name, tier
# On first run (no S3 record), seeds a default Dev account.
.load_or_init_credentials <- function() {
  raw <- tryCatch(
    suppressMessages(.s3_read(S3_KEYS$usuarios)),
    error = function(e) NULL
  )

  if (!is.null(raw) && nrow(raw) > 0) {
    # Exclude soft-deleted accounts — they cannot log in
    if ("deleted" %in% names(raw))
      raw <- raw[is.na(raw$deleted) | raw$deleted != TRUE, , drop = FALSE]
    # Normalize to shinymanager format
    return(data.frame(
      user     = tolower(trimws(raw$username)),  # always lowercase for case-insensitive login
      password = raw$password_hash,
      admin    = raw$tier == "dev",
      name     = raw$display_name,
      tier     = raw$tier,
      stringsAsFactors = FALSE
    ))
  }

  # First run — seed Dev account with a default password
  # IMPORTANT: change this password immediately after first login
  message("[AUTH] No credentials found in S3. Seeding default Dev account.")
  message("[AUTH] Default login: user='dev'  password='Antiguedad2026!'")
  message("[AUTH] Change this password immediately via the Usuarios panel.")

  default <- data.frame(
    id            = uuid::UUIDgenerate(),
    account_code  = "U0001",
    username      = "dev",
    password_hash = "Antiguedad2026!",   # plain text — see NOTE above
    display_name  = "Developer",
    tier          = "dev",
    client_id     = tolower(Sys.getenv("CLIENT_ID")),
    permisos      = "{}",
    activo        = TRUE,
    created_at    = as.character(Sys.time()),
    last_login    = NA_character_,
    deleted       = FALSE,
    deleted_at    = NA_character_,
    stringsAsFactors = FALSE
  )

  # Save to S3 immediately
  tryCatch(
    .s3_write(default, S3_KEYS$usuarios),
    error = function(e) message("[AUTH] Warning: could not save default credentials to S3: ", e$message)
  )

  data.frame(
    user     = "dev",
    password = "Antiguedad2026!",
    admin    = TRUE,
    name     = "Developer",
    tier     = "dev",
    stringsAsFactors = FALSE
  )
}

# Cached at startup — avoids S3 read on every login attempt
.auth_credentials <- NULL

auth_get_credentials <- function() {
  if (is.null(.auth_credentials)) {
    message("[AUTH] Fetching credentials from S3 (usuarios.rds)...")
    t0 <- proc.time()
    .auth_credentials <<- .load_or_init_credentials()
    message(sprintf("[AUTH] Credentials loaded (%d user(s)) in %.1fs",
                    nrow(.auth_credentials),
                    (proc.time() - t0)[["elapsed"]]))
  } else {
    message(sprintf("[AUTH] Credentials from cache (%d user(s))", nrow(.auth_credentials)))
  }
  .auth_credentials
}

# Call after a successful save to force reload on next login attempt
auth_invalidate_credentials <- function() {
  .auth_credentials <<- NULL
}

# Load raw usuarios.rds as a data.frame (all columns, not shinymanager format)
auth_load_usuarios <- function() {
  raw <- tryCatch(
    suppressMessages(.s3_read(S3_KEYS$usuarios)),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    return(data.frame(
      id            = character(),
      account_code  = character(),
      username      = character(),
      password_hash = character(),
      display_name  = character(),
      tier          = character(),
      client_id     = character(),
      permisos      = character(),
      activo        = logical(),
      created_at    = character(),
      last_login    = character(),
      deleted       = logical(),
      deleted_at    = character(),
      stringsAsFactors = FALSE
    ))
  }
  # Ensure all columns exist (forward compatibility)
  defaults <- list(permisos = "{}", activo = TRUE,
                   last_login = NA_character_, client_id = "",
                   deleted = FALSE, deleted_at = NA_character_,
                   account_code = NA_character_)
  for (col in names(defaults))
    if (!col %in% names(raw)) raw[[col]] <- defaults[[col]]

  # Back-fill account_code for accounts created before this field existed.
  # Assigns sequential codes (U0001, U0002, ...) and persists them once.
  needs_code <- is.na(raw$account_code) | !nzchar(trimws(raw$account_code))
  if (any(needs_code)) {
    existing_nums <- suppressWarnings(as.integer(
      sub("^U0*", "", raw$account_code[grepl("^U\\d+$", raw$account_code)])))
    existing_nums <- existing_nums[!is.na(existing_nums)]
    next_n <- if (length(existing_nums)) max(existing_nums) + 1L else 1L
    for (i in which(needs_code)) {
      raw$account_code[i] <- sprintf("U%04d", next_n)
      next_n <- next_n + 1L
    }
    tryCatch(
      .s3_write(raw, S3_KEYS$usuarios),
      error = function(e) message("[AUTH] account_code backfill save failed: ", e$message)
    )
  }

  raw
}

# Save usuarios data.frame to S3 and invalidate the login cache.
# Throws on failure — callers must wrap in tryCatch and surface the error to the user.
auth_save_usuarios <- function(df) {
  .s3_write(df, S3_KEYS$usuarios)   # throws on any S3 error
  auth_invalidate_credentials()     # force reload on next login attempt
  invisible(TRUE)
}

# Resolve effective permissions for a user by combining tier defaults + JSON overrides.
# Returns a named list of booleans for all 12 permission keys.
auth_resolve_perms <- function(tier, permisos_json) {
  defaults <- list(
    dev = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = TRUE,
      can_manage_empresas = TRUE
    ),
    admin = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = FALSE,
      can_manage_empresas = TRUE
    ),
    finance = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = FALSE, can_manage_users = FALSE, can_view_tiers = FALSE,
      can_manage_empresas = FALSE
    ),
    analysis = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = FALSE, can_view_reportes = TRUE, can_edit_invoices = FALSE,
      can_move_invoices = FALSE, can_manage_tags = FALSE, can_export_pdf = TRUE,
      can_manage_providers = FALSE, can_manage_users = FALSE, can_view_tiers = FALSE,
      can_manage_empresas = FALSE
    )
  )

  base <- defaults[[tier]] %||% defaults[["finance"]]

  overrides <- tryCatch(
    jsonlite::fromJSON(permisos_json %||% "{}"),
    error = function(e) list()
  )

  for (k in names(overrides))
    if (k %in% names(base)) base[[k]] <- isTRUE(overrides[[k]])

  base
}

# Case-insensitive credential checker for shinymanager.
# Resolves credentials fresh on every login attempt so accounts created during
# the running session are immediately usable without a server restart.
auth_check_credentials <- function() {
  function(user, password) {
    creds <- auth_get_credentials()   # uses in-memory cache; re-reads S3 only after invalidation
    inner <- shinymanager::check_credentials(creds)
    inner(tolower(trimws(user)), password)
  }
}
