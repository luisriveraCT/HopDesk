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
.normalize_credentials <- function(raw, default_client_id) {
  if ("deleted" %in% names(raw))
    raw <- raw[is.na(raw$deleted) | raw$deleted != TRUE, , drop = FALSE]
  if (!nrow(raw)) return(NULL)
  data.frame(
    user            = tolower(trimws(raw$username)),
    password        = raw$password_hash,
    admin           = raw$tier == "dev",
    name            = raw$display_name,
    tier            = raw$tier,
    account_code    = raw$account_code    %||% NA_character_,
    group_ids       = raw$group_ids       %||% "[]",
    client_id       = {
      cid_raw <- raw$client_id %||% default_client_id
      ifelse(is.na(cid_raw) | !nzchar(trimws(as.character(cid_raw))),
             default_client_id, cid_raw)
    },
    allowed_clients = raw$allowed_clients %||% "[]",
    stringsAsFactors = FALSE
  )
}

.load_or_init_credentials <- function() {
  cid <- tolower(Sys.getenv("CLIENT_ID"))

  # Primary: this deployment's own users.
  # Use direct S3 read (not .s3_read) to bypass the missing-key blacklist and
  # avoid the CLIENT_ID env-var dependency inside .s3_key() — the same pattern
  # used for the hd-admin staff overlay below.
  raw <- if (nzchar(cid)) {
    tryCatch(
      suppressMessages(suppressWarnings(
        aws.s3::s3readRDS(
          object = paste0(cid, "/", S3_KEYS$usuarios),
          bucket = .s3_bucket()
        )
      )),
      error = function(e) {
        message("[AUTH] Could not read client usuarios.rds: ", e$message)
        NULL
      }
    )
  } else {
    message("[AUTH] CLIENT_ID not set — skipping client credential load")
    NULL
  }
  client_creds <- if (!is.null(raw) && nrow(raw) > 0)
    .normalize_credentials(raw, cid) else NULL

  # Staff overlay: when running as a client deployment (not hd-admin itself),
  # also load hd-admin principal + hopdesk accounts so they can log in here.
  # Stage 4 will formalize cross-client access; this is the credential layer.
  staff_creds <- NULL
  if (cid != "hd-admin") {
    hd_raw <- tryCatch(
      aws.s3::s3readRDS(object = "hd-admin/usuarios.rds", bucket = .s3_bucket()),
      error = function(e) NULL
    )
    if (!is.null(hd_raw) && is.data.frame(hd_raw) && nrow(hd_raw)) {
      hd_staff <- hd_raw[hd_raw$tier %in% c("principal", "hopdesk"), , drop = FALSE]
      if (nrow(hd_staff))
        staff_creds <- .normalize_credentials(hd_staff, "hd-admin")
    }
  }

  merged <- do.call(rbind, Filter(Negate(is.null), list(client_creds, staff_creds)))

  # Stage 4: when running as the shared hd-admin deployment, also load credentials
  # from every active client folder so client users can log in here.
  if (cid == "hd-admin") {
    registry <- tryCatch(read_client_registry(), error = function(e) NULL)
    if (!is.null(registry) && nrow(registry)) {
      active_clients <- registry[registry$status == "active", , drop = FALSE]
      client_creds_list <- lapply(active_clients$client_id, function(ccid) {
        raw_c <- tryCatch(
          aws.s3::s3readRDS(object = paste0(ccid, "/usuarios.rds"), bucket = .s3_bucket()),
          error = function(e) NULL
        )
        if (is.null(raw_c) || !nrow(raw_c)) return(NULL)
        raw_c <- raw_c[!raw_c$tier %in% c("principal", "hopdesk"), , drop = FALSE]
        if (!nrow(raw_c)) return(NULL)
        .normalize_credentials(raw_c, ccid)
      })
      client_creds_list <- Filter(Negate(is.null), client_creds_list)
      if (length(client_creds_list))
        merged <- do.call(rbind, c(list(merged), client_creds_list))
    }
  }

  if (!is.null(merged) && nrow(merged) > 0) {
    message(sprintf("[AUTH] Credentials loaded: %d client, %d staff",
                    nrow(client_creds %||% data.frame()),
                    nrow(staff_creds  %||% data.frame())))
    return(merged)
  }

  message("[AUTH] No credentials found in S3. Login will be rejected until accounts are created.")
  data.frame(
    user     = character(),
    password = character(),
    admin    = logical(),
    name     = character(),
    tier     = character(),
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

# Load raw usuarios.rds as a data.frame (all columns, not shinymanager format).
# client_id: explicit S3 folder slug (e.g. "networks", "hd-admin"). Defaults to
# Sys.getenv("CLIENT_ID") for backwards compatibility. Passing this explicitly
# is the Stage 4 mechanism for reading another client's folder without mutating
# the global env var (which would affect all concurrent sessions).
auth_load_usuarios <- function(client_id = NULL) {
  cid <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  key <- paste0(cid, "/usuarios.rds")

  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    return(data.frame(
      id                       = character(),
      account_code             = character(),
      username                 = character(),
      password_hash            = character(),
      display_name             = character(),
      tier                     = character(),
      client_id                = character(),
      permisos                 = character(),
      group_ids                = character(),
      allowed_clients          = character(),
      email                    = character(),
      requires_password_change = logical(),
      activo                   = logical(),
      created_at               = character(),
      last_login               = character(),
      deleted                  = logical(),
      deleted_at               = character(),
      stringsAsFactors = FALSE
    ))
  }
  # Ensure all columns exist (forward compatibility)
  defaults <- list(permisos = "{}", activo = TRUE,
                   last_login = NA_character_, client_id = "",
                   deleted = FALSE, deleted_at = NA_character_,
                   account_code = NA_character_, group_ids = "[]",
                   allowed_clients = "[]",
                   email = NA_character_, requires_password_change = FALSE)
  for (col in names(defaults))
    if (!col %in% names(raw)) raw[[col]] <- defaults[[col]]

  # Back-fill account_code for accounts created before this field existed.
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
      aws.s3::s3saveRDS(raw, object = key, bucket = .s3_bucket()),
      error = function(e) message("[AUTH] account_code backfill save failed: ", e$message)
    )
  }

  # Back-fill client_id for accounts that predate the multi-tenant model.
  needs_cid <- is.na(raw$client_id) | !nzchar(trimws(raw$client_id %||% ""))
  if (any(needs_cid)) {
    raw$client_id[needs_cid] <- cid
    tryCatch(
      aws.s3::s3saveRDS(raw, object = key, bucket = .s3_bucket()),
      error = function(e) message("[AUTH] client_id backfill save failed: ", e$message)
    )
    message(sprintf("[AUTH] client_id backfilled for %d user(s) → %s", sum(needs_cid), cid))
  }

  raw
}

# Save usuarios data.frame to S3 and invalidate the login cache.
# client_id: explicit folder slug — same Stage 4 override as auth_load_usuarios().
# Throws on failure — callers must wrap in tryCatch and surface the error to the user.
auth_save_usuarios <- function(df, client_id = NULL) {
  cid <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  key <- paste0(cid, "/usuarios.rds")
  aws.s3::s3saveRDS(df, object = key, bucket = .s3_bucket())
  auth_invalidate_credentials()
  invisible(TRUE)
}

# Resolve effective permissions for a user by combining tier defaults + JSON overrides.
# Returns a named list of booleans for all 12 permission keys.
# Tier hierarchy (highest to lowest): principal > hopdesk > dev > admin > finance > analysis
#
# Locked permissions (can_approve_clients, can_manage_hopdesk_perms):
#   - Always TRUE for principal tier; cannot be toggled via UI by anyone.
#   - Overrides in permisos JSON are ignored for these two keys on principal accounts.
auth_resolve_perms <- function(tier, permisos_json) {
  # Existing 13 keys + 5 new SaaS keys
  defaults <- list(
    principal = list(
      # All operational permissions — full access
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = TRUE,
      can_manage_empresas = TRUE,
      # SaaS admin permissions — locked TRUE for principal
      can_approve_clients        = TRUE,
      can_manage_hopdesk_perms   = TRUE,
      can_jump_clients           = TRUE,
      can_manage_invites         = TRUE,
      can_view_client_audit_logs = TRUE,
      can_view_staff_audit_log   = TRUE
    ),
    hopdesk = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = TRUE,
      can_manage_empresas = TRUE,
      can_approve_clients        = FALSE,
      can_manage_hopdesk_perms   = FALSE,
      can_jump_clients           = FALSE,
      can_manage_invites         = TRUE,
      can_view_client_audit_logs = TRUE,
      can_view_staff_audit_log   = FALSE
    ),
    dev = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = TRUE,
      can_manage_empresas = TRUE,
      can_approve_clients = FALSE, can_manage_hopdesk_perms = FALSE,
      # can_manage_invites = TRUE: a client's own dev/IT can invite their own
      # teammates without asking Hopdesk (Stage 2 Part C, confirmed with
      # Mouse). Note: tiers_module.R's Invitaciones tab gate is tier-equality
      # based like its siblings, not driven by this flag directly — this
      # default documents the intended permission, it isn't itself consulted.
      can_jump_clients = FALSE, can_manage_invites = TRUE,
      can_view_client_audit_logs = FALSE, can_view_staff_audit_log = FALSE
    ),
    admin = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = TRUE, can_manage_users = TRUE, can_view_tiers = FALSE,
      can_manage_empresas = TRUE,
      can_approve_clients = FALSE, can_manage_hopdesk_perms = FALSE,
      can_jump_clients = FALSE, can_manage_invites = FALSE,
      can_view_client_audit_logs = FALSE, can_view_staff_audit_log = FALSE
    ),
    finance = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = TRUE, can_view_reportes = TRUE, can_edit_invoices = TRUE,
      can_move_invoices = TRUE, can_manage_tags = TRUE, can_export_pdf = TRUE,
      can_manage_providers = FALSE, can_manage_users = FALSE, can_view_tiers = FALSE,
      can_manage_empresas = FALSE,
      can_approve_clients = FALSE, can_manage_hopdesk_perms = FALSE,
      can_jump_clients = FALSE, can_manage_invites = FALSE,
      can_view_client_audit_logs = FALSE, can_view_staff_audit_log = FALSE
    ),
    analysis = list(
      can_view_cobros = TRUE, can_view_pagos = TRUE, can_view_agenda = TRUE,
      can_view_bancos = FALSE, can_view_reportes = TRUE, can_edit_invoices = FALSE,
      can_move_invoices = FALSE, can_manage_tags = FALSE, can_export_pdf = TRUE,
      can_manage_providers = FALSE, can_manage_users = FALSE, can_view_tiers = FALSE,
      can_manage_empresas = FALSE,
      can_approve_clients = FALSE, can_manage_hopdesk_perms = FALSE,
      can_jump_clients = FALSE, can_manage_invites = FALSE,
      can_view_client_audit_logs = FALSE, can_view_staff_audit_log = FALSE
    )
  )

  base <- defaults[[tier]] %||% defaults[["finance"]]

  overrides <- tryCatch(
    jsonlite::fromJSON(permisos_json %||% "{}"),
    error = function(e) list()
  )

  # Apply overrides — but locked keys on principal accounts are immune
  locked_principal_keys <- c("can_approve_clients", "can_manage_hopdesk_perms")
  for (k in names(overrides)) {
    if (!k %in% names(base)) next
    if (identical(tier, "principal") && k %in% locked_principal_keys) next
    base[[k]] <- isTRUE(overrides[[k]])
  }

  base
}

# Case-insensitive credential checker for shinymanager.
# Resolves credentials fresh on every login attempt so accounts created during
# the running session are immediately usable without a server restart.
auth_check_credentials <- function() {
  function(user, password) {
    creds <- auth_get_credentials()
    inner <- shinymanager::check_credentials(creds)
    res   <- inner(tolower(trimws(user)), password)

    # Emergency lock check — runs only after a successful credential match.
    # The lock file is cached for 60s; a compromised active session is terminated
    # separately via the app.R session observer.
    if (isTRUE(res$result)) {
      lock <- tryCatch(read_emergency_lock(), error = function(e) NULL)
      if (is.data.frame(lock) && nrow(lock) > 0 &&
          any(tolower(lock$username) == tolower(trimws(user)))) {
        message("[AUTH] Login blocked by emergency lock: ", user)
        return(list(
          result    = FALSE,
          user_info = list(
            user    = user,
            message = "Esta cuenta ha sido bloqueada. Contacta a tu administrador."
          )
        ))
      }
    }

    res
  }
}
