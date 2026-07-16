# =============================================================================
# R/persistence.R
# All S3 read/write operations in one place.
# Every public function follows the same pattern:
#   load_*()  → returns a normalized tibble (never NULL, never errors to caller)
#   save_*()  → writes to S3, returns invisible(TRUE)
# =============================================================================

# ── Internal S3 helpers ────────────────────────────────────────────────────────
#
# Credential convention (set in .Renviron, never commit to git):
#
#   CLIENT_ID=networks           ← unique slug for this deployment (only this changes per client)
#   S3_BUCKET=my-bucket          ← shared bucket name
#   AWS_ACCESS_KEY_ID=AKIAxx     ← standard AWS env vars (picked up automatically by aws.s3)
#   AWS_SECRET_ACCESS_KEY=xxxx
#   AWS_DEFAULT_REGION=us-east-1
#
# All S3 objects are stored under a "<client_id>/" prefix inside the bucket.
# Client isolation is enforced at the prefix level via IAM policy (see Stage 7).
#
# Adding a new client = new .Renviron changing only CLIENT_ID. Zero code changes required.
#
# Future IAM migration path: when per-client IAM keys are available, .s3_bucket()
# and s3_init() accept an optional `client_id` override. The calling convention
# stays identical — only the credential lookup changes. Until then, one shared key
# covers all prefixes, scoped by IAM prefix-level policy.
# ─────────────────────────────────────────────────────────────────────────────

# Returns the CLIENT_ID slug (lowercased for S3 prefix use)
.client_id <- function() {
  id <- Sys.getenv("CLIENT_ID")
  if (!nzchar(id))
    stop("CLIENT_ID environment variable is not set. ",
         "Set it in .Renviron (e.g. CLIENT_ID=networks).", call. = FALSE)
  toupper(trimws(id))
}

# Returns the S3 bucket name.
# Future per-client IAM migration: accept optional client_id param and look up
# a client-specific S3_BUCKET_{CLIENT_ID} var here if present.
.s3_bucket <- function() {
  b <- Sys.getenv("S3_BUCKET")
  if (!nzchar(b))
    stop("S3_BUCKET environment variable is not set.", call. = FALSE)
  b
}

# Prefixes every S3 key with "<client_id>/" for logical isolation within the bucket.
# e.g. "invoice_moves.rds" → "networks/invoice_moves.rds"
# client_id: optional override — used by cross-client staff access (Stage 4).
#   When NULL, falls back to CLIENT_ID env var.
.s3_key <- function(key, client_id = NULL) {
  prefix <- if (!is.null(client_id) && nzchar(client_id))
               tolower(trimws(client_id))
             else
               tolower(.client_id())
  paste0(prefix, "/", key)
}

# .s3_read_with(): like .s3_read() but accepts an explicit client_id override.
# Checks the preload cache under the FULL prefix key (e.g. "networks/movimientos.rds")
# so that Phase-2 batch fetches benefit subsequent load_*() calls without the
# cross-session cross-client collision that the bare-suffix key scheme had.
.s3_read_with <- function(key, client_id = NULL) {
  full_key <- .s3_key(key, client_id = client_id)
  bucket   <- .s3_bucket()
  # Preload cache hit — keyed by full_key to isolate different clients.
  if (exists(full_key, envir = .s3_preload_cache, inherits = FALSE))
    return(.s3_preload_cache[[full_key]])
  if (isTRUE(.s3_missing_cache[[full_key]])) return(NULL)
  tryCatch({
    out <- NULL
    suppressMessages(capture.output(
      out <- aws.s3::s3readRDS(object = full_key, bucket = bucket),
      type = "output"
    ))
    out
  }, error = function(e) {
    .s3_missing_cache[[full_key]] <- TRUE
    NULL
  })
}

# Validates AWS credentials are present at startup. aws.s3 reads AWS_* vars
# automatically — this function just warns early if they are missing.
# Future per-client IAM migration: accept optional client_id and load a
# client-scoped credential set here before calling Sys.setenv().
s3_init <- function() {
  missing <- c(
    if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")))     "AWS_ACCESS_KEY_ID",
    if (!nzchar(Sys.getenv("AWS_SECRET_ACCESS_KEY"))) "AWS_SECRET_ACCESS_KEY",
    if (!nzchar(Sys.getenv("AWS_DEFAULT_REGION")))    "AWS_DEFAULT_REGION"
  )
  if (length(missing))
    warning("Missing AWS credentials: ", paste(missing, collapse = ", "),
            "\nS3 persistence will not work.", call. = FALSE)
  invisible(TRUE)
}

# Cache of known-missing S3 keys — avoids repeated round-trips within a session.
.s3_missing_cache <- new.env(parent = emptyenv())

# ── Emergency lock (hd-admin/emergency_lock.rds) ─────────────────────────────
#
# These functions bypass .s3_key() intentionally — the lock file lives in the
# hd-admin/ prefix regardless of the current CLIENT_ID deployment.
#
# Schema: data.frame(username, locked_at [POSIXct], locked_by, reason)
#
# Future IAM note: with per-client IAM keys, a cross-client read role would be
# needed here (same note as username_index.rds functions in Stage 2).
.emergency_lock_cache <- new.env(parent = emptyenv())

read_emergency_lock <- function() {
  now <- proc.time()[["elapsed"]]
  if (!is.null(.emergency_lock_cache$data) &&
      !is.null(.emergency_lock_cache$ts)   &&
      (now - .emergency_lock_cache$ts) < 60) {
    return(.emergency_lock_cache$data)
  }
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = "hd-admin/emergency_lock.rds", bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  .emergency_lock_cache$data <- raw
  .emergency_lock_cache$ts   <- now
  raw
}

write_emergency_lock <- function(df) {
  aws.s3::s3saveRDS(df, object = "hd-admin/emergency_lock.rds", bucket = .s3_bucket())
  .emergency_lock_cache$data <- df
  .emergency_lock_cache$ts   <- proc.time()[["elapsed"]]
  invisible(TRUE)
}

.schema_emergency_lock <- function() {
  data.frame(
    username  = character(),
    locked_at = as.POSIXct(character()),
    locked_by = character(),
    reason    = character(),
    stringsAsFactors = FALSE
  )
}

# ── Global Username Registry ────────────────────────────────────────────────────
# Lives at hd-admin/username_index.rds — bypasses .s3_key() intentionally so
# every client deployment can check global uniqueness against the same index.
#
# Future IAM hardening: this function needs read access to the hd-admin/ prefix
# regardless of the current CLIENT_ID. With per-client IAM keys, this would
# require a dedicated cross-client IAM role.

.UINDEX_KEY <- "hd-admin/username_index.rds"

.schema_username_index <- function() {
  data.frame(
    username     = character(),
    home_folder  = character(),
    account_code = character(),
    created_at   = character(),
    active       = logical(),
    stringsAsFactors = FALSE
  )
}

.read_username_index <- function() {
  raw <- tryCatch(
    aws.s3::s3readRDS(object = .UINDEX_KEY, bucket = .s3_bucket()),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_username_index())
  raw
}

.write_username_index <- function(df) {
  aws.s3::s3saveRDS(df, object = .UINDEX_KEY, bucket = .s3_bucket())
  invisible(TRUE)
}

# Returns TRUE when the username is free to claim (absent or previously deleted).
# IAM note: this reads hd-admin/username_index.rds regardless of CLIENT_ID.
# With per-client IAM keys this requires a dedicated cross-client read role —
# see scripts/iam_policy_client.json "HdAdminSharedReadOnly" for the current
# read-only workaround that grants the client key access to specific hd-admin files.
check_username_available <- function(username) {
  username <- tolower(trimws(username))
  idx <- tryCatch(.read_username_index(), error = function(e) .schema_username_index())
  if (!nrow(idx)) return(TRUE)
  active_matches <- idx[tolower(idx$username) == username & idx$active == TRUE, ]
  nrow(active_matches) == 0
}

#' Claim a username in the global index. Reactivates a soft-deleted entry if one exists.
register_username <- function(username, home_folder, account_code) {
  username <- tolower(trimws(username))
  idx <- tryCatch(.read_username_index(), error = function(e) .schema_username_index())
  existing <- which(tolower(idx$username) == username)
  if (length(existing)) {
    idx$active[existing]       <- TRUE
    idx$home_folder[existing]  <- home_folder
    idx$account_code[existing] <- account_code
  } else {
    idx <- rbind(idx, data.frame(
      username     = username,
      home_folder  = home_folder,
      account_code = account_code,
      created_at   = as.character(Sys.time()),
      active       = TRUE,
      stringsAsFactors = FALSE
    ))
  }
  .write_username_index(idx)
  invisible(TRUE)
}

#' Soft-deactivate a username. Never hard-deletes — preserves the audit trail.
unregister_username <- function(username) {
  username <- tolower(trimws(username))
  idx <- tryCatch(.read_username_index(), error = function(e) .schema_username_index())
  if (!nrow(idx)) return(invisible(FALSE))
  matches <- tolower(idx$username) == username
  if (!any(matches)) return(invisible(FALSE))
  idx$active[matches] <- FALSE
  .write_username_index(idx)
  invisible(TRUE)
}

# Pre-load cache — populated at startup (global.R) and by Phase-2 per-session.
# Two key formats coexist:
#  - Bare suffix  ("invoice_moves.rds")           ← global.R startup; .s3_read()
#  - Full prefix  ("networks/invoice_moves.rds")  ← Phase-2 batch; .s3_read_with()
# Full-prefix keys isolate clients so sessions never see each other's cached data.
.s3_preload_cache <- new.env(parent = emptyenv())

.s3_read <- function(key) {
  # Pre-load hit — no S3 round-trip needed on first read of every key.
  if (exists(key, envir = .s3_preload_cache, inherits = FALSE))
    return(.s3_preload_cache[[key]])

  full_key <- .s3_key(key)
  bucket   <- .s3_bucket()

  if (isTRUE(.s3_missing_cache[[full_key]])) return(NULL)

  tryCatch({
    out <- NULL
    suppressMessages(
      capture.output(
        out <- aws.s3::s3readRDS(object = full_key, bucket = bucket),
        type = "output"
      )
    )
    out
  }, error = function(e) {
    .s3_missing_cache[[full_key]] <- TRUE
    NULL
  })
}

.s3_write <- function(obj, key, client_id = NULL) {
  message(sprintf("[S3_WRITE] ── start key='%s'", key))

  if (!is.character(key) || !nzchar(key))
    stop("[S3_WRITE] key is NULL or empty — check that S3_KEYS contains this entry")
  full_key <- tryCatch(.s3_key(key, client_id = client_id),  error = function(e) stop("s3_key failed: ", e$message))
  message(sprintf("[S3_WRITE] full_key='%s'", full_key))

  bucket <- tryCatch(.s3_bucket(), error = function(e) stop("s3_bucket failed: ", e$message))
  message(sprintf("[S3_WRITE] bucket='%s'", bucket))

  # ── Step 1: write to local temp file ──────────────────────────────────────
  tf <- tempfile(fileext = ".rds")
  on.exit({ message("[S3_WRITE] cleanup tempfile"); unlink(tf) }, add = TRUE)
  message(sprintf("[S3_WRITE] tempfile='%s'", tf))

  tryCatch(
    saveRDS(obj, tf, compress = TRUE),
    error = function(e) stop("saveRDS failed: ", e$message)
  )
  sz <- file.info(tf)$size
  message(sprintf("[S3_WRITE] saveRDS OK — %d bytes written", sz))

  # ── Step 2: upload via s3saveRDS (high-level API, handles key normalisation) ──
  message(sprintf("[S3_WRITE] calling s3saveRDS: object='%s' bucket='%s'", full_key, bucket))
  tryCatch({
    # s3saveRDS calls get_objectkey() + put_object(file=path) internally,
    # matching exactly how aws.s3 0.3.22 expects uploads.
    aws.s3::s3saveRDS(obj, object = full_key, bucket = bucket)
    message("[S3_WRITE] s3saveRDS OK")
  }, error = function(e) {
    message(sprintf("[S3_WRITE] s3saveRDS FAILED: %s", e$message))
    # Fallback: try put_object with the file path directly
    message("[S3_WRITE] fallback → put_object(file=tf)")
    tryCatch({
      suppressMessages(
        aws.s3::put_object(file = tf, object = full_key, bucket = bucket)
      )
      message("[S3_WRITE] put_object fallback OK")
    }, error = function(e2) {
      message(sprintf("[S3_WRITE] put_object fallback FAILED: %s", e2$message))
      stop(e2$message)
    })
  })

  # ── Step 3: invalidate caches ──────────────────────────────────────────────
  rm(list = intersect(full_key, ls(.s3_missing_cache)), envir = .s3_missing_cache)
  # Invalidate bare-suffix key (legacy .s3_read() cache) and full-prefix key
  # (used by .s3_read_with()) so stale preload data never survives a write.
  if (is.character(key) && length(key) == 1L && nzchar(key) &&
      exists(key, envir = .s3_preload_cache, inherits = FALSE))
    rm(list = key, envir = .s3_preload_cache)
  if (exists(full_key, envir = .s3_preload_cache, inherits = FALSE))
    rm(list = full_key, envir = .s3_preload_cache)

  message(sprintf("[S3_WRITE] ── complete key='%s'", key))
  invisible(TRUE)
}

.s3_write_with <- function(obj, key, client_id = NULL) {
  .s3_write(obj, key, client_id = client_id)
}

# ── Schema definitions ─────────────────────────────────────────────────────────
# Each schema() returns an empty tibble with correct column types.
# Used for both validation and as fallback when the S3 key doesn't exist yet.

.schema_moves <- function() tibble(
  ledger               = character(),
  Empresa              = character(),
  Moneda               = character(),
  Documento            = character(),
  FechaVenc_Proyectada = as.Date(character()),
  notas                = character(),
  moved_by             = character(),
  last_updated         = as.POSIXct(character())
)

.schema_notes <- function() tibble(
  ledger      = character(),
  year        = integer(),
  month       = integer(),
  id          = character(),
  title       = character(),
  body        = character(),
  author      = character(),       # display_name for readability
  author_code = character(),       # account_code (U0001…) for traceability
  visibility  = character(),       # "public" | "personal"; NA treated as "public"
  created     = as.POSIXct(character()),
  updated     = as.POSIXct(character())
)

.schema_tags <- function() tibble(
  ledger    = character(),
  Empresa   = character(),
  Moneda    = character(),
  Documento = character(),
  tag       = character(),   # "important" | "urgent"
  tagged_by = character(),
  tagged_at = as.POSIXct(character())
)

.schema_manual <- function() tibble(
  id            = character(),
  ledger        = character(),   # "AR" | "AP"
  Empresa       = character(),
  Moneda        = character(),
  Documento     = character(),   # user-assigned reference
  Factura       = character(),
  Parte         = character(),   # client or vendor name
  Codigo        = character(),
  Importe       = numeric(),
  `Abono futuro`= numeric(),
  `Fecha de vencimiento` = as.Date(character()),
  Notas         = character(),
  created_by    = character(),
  created_at    = as.POSIXct(character()),
  updated_at    = as.POSIXct(character()),
  provision_id  = character(),   # FK to pasivos_provisions; NA for non-provision items
  liability_id  = character(),   # FK to pasivos_liabilities; NA for non-provision items
  referencia    = character()    # Pasivos reference field; NA for non-provision items
)

# ── Payment-policy schemas ─────────────────────────────────────────────────────

.schema_policy_catalog <- function() tibble(
  id             = character(),   # uuid
  name           = character(),   # user-defined label
  type           = character(),   # "weekdays"|"skip_holidays"|"last_day"|"month_days"|"offset_days"
  params         = list(),        # named list; structure depends on type (see spec §3.1)
  roll_direction = character(),   # "forward" | "backward"
  created_by     = character(),
  created_at     = as.POSIXct(character()),
  updated_at     = as.POSIXct(character())
)

.schema_partner_policies <- function() tibble(
  parte        = character(),   # Parte name as stored in SAP / manual entries
  policy_id    = character(),   # FK → policy_catalog$id
  policy_order = integer(),     # execution order (1 = first applied)
  ledger       = character(),   # "AR" | "AP" | "" (both)
  is_interco   = logical(),     # TRUE = suggestion-only, Aplicar will not write moves
  is_manual    = logical(),     # TRUE = stack manually edited; scope "skip_manual" will skip these
  linked_by    = character(),
  linked_at    = as.POSIXct(character())
)

.schema_policy_moves <- function() tibble(
  ledger             = character(),
  Empresa            = character(),
  Moneda             = character(),
  Documento          = character(),
  Parte              = character(),   # carried from SAP — needed for scope/merge logic
  FechaVenc_Politica = as.Date(character()),
  policy_ids         = character(),   # comma-joined IDs that produced this date
  applied_by         = character(),
  applied_at         = as.POSIXct(character())
)

# Custom loaders for policy data — list column in policy_catalog requires
# special handling that .normalize() cannot do generically.
load_policy_catalog <- function(client_id = NULL) {
  obj <- tryCatch(.s3_read_with(S3_KEYS$policy_catalog, client_id = client_id), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj) || !nrow(obj)) return(.schema_policy_catalog())
  schema <- .schema_policy_catalog()
  for (col in setdiff(names(schema), "params")) {
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  }
  if (!"params" %in% names(obj)) obj[["params"]] <- vector("list", nrow(obj))
  obj
}

load_partner_policies <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$partner_policies, client_id = client_id), .schema_partner_policies)
}

load_policy_moves <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$policy_moves, client_id = client_id), .schema_policy_moves)
}

save_policy_catalog <- function(df, client_id = NULL) {
  .s3_write(df, S3_KEYS$policy_catalog, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("policy_catalog", df)
}

save_partner_policies <- function(df, client_id = NULL) {
  .s3_write(df, S3_KEYS$partner_policies, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("partner_policies", df)
}

save_policy_moves <- function(df, client_id = NULL) {
  .s3_write(df, S3_KEYS$policy_moves, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("policy_moves", df)
}

.schema_holiday_overrides <- function() tibble(
  id          = character(),   # uuid
  country     = character(),   # "MX" | "US" | "FR"
  date        = as.Date(character()),
  action      = character(),   # "add" | "remove"
  description = character(),   # e.g. "Elección presidencial 2027"
  created_by  = character(),
  created_at  = as.POSIXct(character())
)

load_holiday_overrides <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$holiday_overrides, client_id = client_id), .schema_holiday_overrides)
}

save_holiday_overrides <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_holiday_overrides)
  .s3_write(norm, S3_KEYS$holiday_overrides, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("holiday_overrides", norm)
}

# ── ERP Connections (Stage 3) ─────────────────────────────────────────────────
# Per-client, per-company ERP credential store, one erp_connections.rds per
# client at "<client_id>/erp_connections.rds". See R/erp_connector_registry.R
# for the supported erp_type shapes and R/secrets_encryption.R for how
# secrets_encrypted is produced/consumed — this file never sees a decrypted
# secret.
#
# Isolation guarantee (docs/saas_rebuild/STAGE_3_ERP_CREDENTIALS.md §4):
# unlike most load_*/save_* pairs above, client_id here is REQUIRED, not an
# optional override that falls back to Sys.getenv("CLIENT_ID") — these
# functions must be impossible to call without stating whose folder you mean.
# Every read/write also re-asserts that every row's own client_id column
# matches the folder it was read from / is being written to, and fails loudly
# (stop(), never a silent filter) on any mismatch — a mismatch means an S3 key
# or a caller upstream is already broken, and hiding that would be worse.

.schema_erp_connections <- function() tibble(
  id                = character(),   # uuid
  client_id         = character(),   # redundant w/ S3 folder; re-checked on every read/write
  label             = character(),
  erp_type          = character(),   # key into ERP_CONNECTOR_REGISTRY
  company_initials  = character(),   # JSON array string, e.g. '["NG"]'
  config            = character(),   # JSON string — non-secret fields, shape per erp_type
  secrets_encrypted = character(),   # base64 ciphertext from encrypt_secret() — never plaintext
  created_at        = character(),
  created_by        = character(),
  updated_at        = character(),
  updated_by        = character(),
  last_tested_at    = character(),
  last_test_result  = character(),   # "ok" | "error" | NA
  last_test_message = character(),
  active            = logical(),
  deleted           = logical(),
  deleted_at        = character()
)

# Fails loudly if any row's client_id doesn't match `client_id` — see isolation
# guarantee above. `context` is only used to word the error message
# (read vs write) since the check is identical either direction.
.assert_erp_connections_isolated <- function(df, client_id, context) {
  if (nrow(df) == 0L) return(invisible(TRUE))
  bad <- is.na(df$client_id) | tolower(df$client_id) != tolower(client_id)
  if (any(bad)) {
    stop(sprintf(
      "erp_connections isolation violation on %s: %d row(s) have client_id != '%s' (found: %s). ",
      context, sum(bad), tolower(client_id),
      paste(unique(df$client_id[bad]), collapse = ", ")
    ), "This means an S3 key or a caller upstream is already broken — refusing to ",
       "silently filter or correct it.", call. = FALSE)
  }
  invisible(TRUE)
}

load_erp_connections <- function(client_id) {
  if (missing(client_id) || is.null(client_id) || !nzchar(client_id))
    stop("load_erp_connections: client_id is required and must be a non-empty string — ",
         "this function must never guess whose folder to read.", call. = FALSE)

  raw <- .normalize(.s3_read_with(S3_KEYS$erp_connections, client_id = client_id),
                     .schema_erp_connections)
  .assert_erp_connections_isolated(raw, client_id, "read")
  raw[is.na(raw$deleted) | raw$deleted != TRUE, , drop = FALSE]
}

save_erp_connections <- function(df, client_id) {
  if (missing(client_id) || is.null(client_id) || !nzchar(client_id))
    stop("save_erp_connections: client_id is required and must be a non-empty string — ",
         "this function must never guess whose folder to write.", call. = FALSE)

  norm <- .normalize(df, .schema_erp_connections)
  .assert_erp_connections_isolated(norm, client_id, "write")

  .s3_write(norm, S3_KEYS$erp_connections, client_id = client_id)
  invisible(TRUE)
}

# ── Normalizers ────────────────────────────────────────────────────────────────
# Coerce loaded data to the correct schema; adds missing columns, drops nothing.

.normalize <- function(df, schema_fn) {
  if (is.null(df) || !is.data.frame(df)) return(schema_fn())
  schema <- schema_fn()
  for (col in names(schema)) {
    if (!col %in% names(df)) {
      df[[col]] <- schema[[col]][NA_integer_]
    } else {
      # coerce to expected type quietly; use [1L] because multi-class types
      # like POSIXct ("POSIXct" "POSIXt") would make methods::as() error
      df[[col]] <- tryCatch(
        methods::as(df[[col]], class(schema[[col]])[1L]),
        error = function(e) schema[[col]][NA_integer_]
      )
    }
  }
  dplyr::select(df, dplyr::any_of(names(schema)))
}

# ── Invoice moves ──────────────────────────────────────────────────────────────

load_moves <- function(client_id = NULL) {
  cached <- if (exists(".cache_get", mode = "function")) .cache_get("moves") else NULL
  df <- if (!is.null(cached)) cached else .s3_read_with(S3_KEYS$moves, client_id = client_id)
  .normalize(df, .schema_moves) |>
    filter(ledger %in% c("AR","AP"), !is.na(Empresa),
           !is.na(Moneda), !is.na(Documento))
}

save_moves <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_moves)
  .s3_write(norm, S3_KEYS$moves, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("moves", norm)
}

# Upsert rows into moves table (matched on Empresa + Moneda + Documento + ledger)
upsert_moves <- function(db, new_rows) {
  if (!nrow(new_rows)) return(db)
  keys <- c("ledger","Empresa","Moneda","Documento")
  # Dedup new_rows before upserting — if the same invoice appears twice (e.g.,
  # two Vencidos rows with different FechaVenc_Original), keep the latest date.
  new_rows <- dplyr::arrange(new_rows, dplyr::desc(FechaVenc_Proyectada))
  new_rows <- dplyr::distinct(new_rows, ledger, Empresa, Moneda, Documento,
                               .keep_all = TRUE)
  db <- anti_join(db, new_rows, by = keys)
  bind_rows(db, new_rows)
}

# Remove moves whose invoices no longer exist in the source data
prune_moves <- function(moves_df, source_df, ledger) {
  ledger <- toupper(ledger)
  if (!nrow(source_df) || !nrow(moves_df)) return(moves_df)
  valid_keys <- source_df |>
    filter(!is.na(Documento)) |>
    distinct(Empresa, Moneda, Documento)
  moves_df |>
    filter(.data$ledger == !!ledger) |>
    semi_join(valid_keys, by = c("Empresa","Moneda","Documento")) |>
    bind_rows(filter(moves_df, .data$ledger != !!ledger))
}

# ── Calendar notes ─────────────────────────────────────────────────────────────

load_notes <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$notes, client_id = client_id), .schema_notes)
}

save_notes <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_notes)
  .s3_write(norm, S3_KEYS$notes, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("notes", norm)
}

# ── Invoice tags ───────────────────────────────────────────────────────────────

load_tags <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$tags, client_id = client_id), .schema_tags) |>
    filter(tag %in% c("important","urgent"))
}

save_tags <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_tags)
  .s3_write(norm, S3_KEYS$tags, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("tags", norm)
}

# Set tags for one invoice (replaces all existing tags for that invoice)
# new_tags: character vector, e.g. c("urgent"), c("important","urgent"), character(0) to clear
set_invoice_tags <- function(tags_df, ledger, empresa, moneda, documento,
                              new_tags, tagged_by = "user") {
  tags_df <- tags_df |>
    filter(!(ledger == !!ledger & Empresa == !!empresa &
             Moneda == !!moneda & Documento == !!documento))

  if (length(new_tags)) {
    new_rows <- tibble(
      ledger    = ledger,
      Empresa   = empresa,
      Moneda    = moneda,
      Documento = documento,
      tag       = new_tags,
      tagged_by = tagged_by,
      tagged_at = Sys.time()
    )
    tags_df <- bind_rows(tags_df, new_rows)
  }
  tags_df
}

# Summarise tags per invoice to a display label
tag_label <- function(tags) {
  hi <- "important" %in% tags
  ur <- "urgent"    %in% tags
  if (hi && ur) "🟠 Ambas"
  else if (ur)  "🔴 Urgente"
  else if (hi)  "🟡 Importante"
  else          ""
}

# Join tag labels onto an invoice data frame
add_tag_labels <- function(df, tags_df, ledger) {
  if (is.null(tags_df) || !nrow(tags_df)) {
    df[["Etiqueta"]] <- ""
    return(df)
  }
  summary <- tags_df |>
    filter(.data$ledger == !!ledger) |>
    group_by(Empresa, Moneda, Documento) |>
    summarise(Etiqueta = tag_label(tag), .groups = "drop")
  left_join(df, summary, by = c("Empresa","Moneda","Documento")) |>
    mutate(Etiqueta = replace_na(Etiqueta, ""))
}

# ── Manual invoices ────────────────────────────────────────────────────────────

load_manual <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$manual, client_id = client_id), .schema_manual)
}

save_manual <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_manual)
  .s3_write(norm, S3_KEYS$manual, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("manual", norm)
}

upsert_manual <- function(df, new_row) {
  if (is.null(df) || !is.data.frame(df)) df <- .schema_manual()
  df <- dplyr::filter(df, .data$id != new_row$id)
  dplyr::bind_rows(df, new_row)
}

delete_manual <- function(df, id) {
  filter(df, .data$id != !!id)
}

# ── Papelera — soft-delete archive ────────────────────────────────────────────
# All deleted invoices land here regardless of source (manual or SAP).
# SAP invoices are stored as a snapshot so they can be reviewed/restored.
# Manual invoices are removed from manual_inv and copied here.

.schema_papelera <- function() tibble(
  id           = character(),   # original invoice id (manual) or generated uuid (SAP)
  ledger       = character(),   # "AR" | "AP"
  source       = character(),   # "manual" | "sap"
  Empresa      = character(),
  Moneda       = character(),
  Documento    = character(),
  Parte        = character(),
  Importe      = numeric(),
  FechaEff     = as.Date(character()),
  deleted_by   = character(),
  deleted_at   = as.POSIXct(character()),
  original_data = list()        # full row as a list for potential restore
)

load_papelera <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$papelera, client_id = client_id), .schema_papelera)
}

save_papelera <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_papelera)
  .s3_write(norm, S3_KEYS$papelera, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("papelera", norm)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("papelera_rv", client_id = client_id)
}

# Add rows to papelera. detail_rows is a plain data.frame of invoice rows.
add_to_papelera <- function(papelera_df, detail_rows, ledger,
                             deleted_by = "user") {
  if (!nrow(detail_rows)) return(papelera_df)
  new_rows <- tibble(
    id           = vapply(seq_len(nrow(detail_rows)),
                          function(i) {
                            existing <- detail_rows[["id"]]
                            if (!is.null(existing) && !is.na(existing[i]) && nzchar(existing[i]))
                              existing[i]
                            else
                              uuid::UUIDgenerate()
                          }, character(1)),
    ledger       = if ("ledger" %in% names(detail_rows) &&
                       !any(is.na(detail_rows[["ledger"]])) &&
                       all(nzchar(detail_rows[["ledger"]])))
                     detail_rows[["ledger"]]
                   else
                     ledger,
    source       = if ("source" %in% names(detail_rows))
                     detail_rows[["source"]] else "sap",
    Empresa      = detail_rows[["Empresa"]],
    Moneda       = detail_rows[["Moneda"]],
    Documento    = detail_rows[["Documento"]] %||% "",
    Parte        = detail_rows[["Parte"]]     %||% "",
    Importe      = detail_rows[["Importe"]]   %||% 0,
    FechaEff     = as.Date(detail_rows[["FechaEff"]] %||% NA),
    deleted_by   = deleted_by,
    deleted_at   = Sys.time(),
    original_data = lapply(seq_len(nrow(detail_rows)),
                           function(i) as.list(detail_rows[i, , drop = FALSE]))
  )
  dplyr::bind_rows(papelera_df, new_rows)
}

# ── Intercompany settings ──────────────────────────────────────────────────────
# Persisted to S3 (same bucket/prefix as all other data) so settings survive
# redeployments. Previously stored in tempdir() which reset on every deploy.

normalize_code <- function(x) toupper(gsub("[^A-Z0-9]", "", trimws(as.character(x))))

# Legacy flat-list format — kept for reference, no longer called by the app.
load_interco <- function(client_id = NULL) {
  out <- .s3_read_with(S3_KEYS$interco, client_id = client_id)
  if (!is.list(out)) out <- list(ar_clients = character(), ap_suppliers = character())
  out$ar_clients   <- normalize_code(out$ar_clients   %||% character())
  out$ap_suppliers <- normalize_code(out$ap_suppliers %||% character())
  out
}

save_interco <- function(lst, client_id = NULL) {
  lst$ar_clients   <- normalize_code(lst$ar_clients   %||% character())
  lst$ap_suppliers <- normalize_code(lst$ap_suppliers %||% character())
  .s3_write(lst, S3_KEYS$interco, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("interco", lst)
}

# ── Intercompany v2 — per-empresa, per-ledger code registry ───────────────────
# Schema: list(ar_prefix, ap_prefix, companies = list(INIT = list(ar, ap)))
# ar/ap are character vectors of numeric code bases (no prefix stored).
# Prefix is prepended at match time, making it configurable per SAP installation.

.empty_interco_v2 <- function() {
  list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = list())
}

# One-time self-healing migration: promotes the old flat interco_settings.rds
# format to the per-empresa v2 format.  Since the old format had no per-empresa
# breakdown, all codes are assigned to every known empresa so filter behavior is
# preserved exactly.  The migrated registry is immediately saved to v2 S3 so the
# next startup skips this path entirely.
.migrate_interco_v1_to_v2 <- function(old) {
  strip_prefix <- function(codes, prefix) {
    codes <- toupper(trimws(codes[nzchar(trimws(codes))]))
    sub(paste0("^", toupper(prefix)), "", codes)
  }
  ar_bases <- strip_prefix(old$ar_clients   %||% character(), "C")
  ap_bases <- strip_prefix(old$ap_suppliers %||% character(), "P")

  cmap <- tryCatch(get("COMPANY_MAP", envir = .GlobalEnv, inherits = FALSE),
                   error = function(e) list())
  companies <- if (length(cmap)) {
    setNames(lapply(names(cmap), function(ini) list(ar = ar_bases, ap = ap_bases)), names(cmap))
  } else {
    list(`ALL` = list(ar = ar_bases, ap = ap_bases))
  }
  list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = companies)
}

load_interco_v2 <- function(client_id = NULL) {
  # When a specific client_id is given (context-jump), bypass the in-memory cache
  # (which is keyed to the native deployment) and read directly from S3.
  if (!is.null(client_id) && nzchar(client_id)) {
    raw <- tryCatch(.s3_read_with(S3_KEYS$interco_v2, client_id = client_id),
                    error = function(e) NULL)
    message(sprintf("[IC] load_interco_v2(client_id='%s') — %s from S3",
                    client_id,
                    if (is.list(raw)) paste(length(raw$companies %||% list()), "companies") else "NULL"))
    if (!is.list(raw) || is.null(raw$companies)) return(.empty_interco_v2())
    raw$ar_prefix <- raw$ar_prefix %||% "C"
    raw$ap_prefix <- raw$ap_prefix %||% "P"
    if (is.null(raw$rfcs)) raw$rfcs <- character()
    return(raw)
  }

  # 1. Cross-session in-memory cache (fastest — populated by save or Phase 2 .cache_set)
  cached <- if (exists(".cache_get", mode = "function")) .cache_get("interco_v2") else NULL
  out <- if (is.list(cached) && !is.null(cached$companies)) {
    message("[IC] load_interco_v2 — hit .app_data_cache")
    cached
  } else {
    # 2. Preload cache or live S3 read
    raw <- .s3_read(S3_KEYS$interco_v2)
    message(sprintf("[IC] load_interco_v2 — %s from S3/preload",
                    if (is.list(raw)) paste(length(raw$companies %||% list()), "companies") else "NULL"))
    raw
  }

  # 3. Fallback: auto-migrate from legacy flat interco_settings.rds when v2 is
  #    absent, has no companies, or has companies but no IC codes entered yet.
  #    Saves the migrated registry to v2 so subsequent startups skip this path.
  .v2_has_codes <- function(reg) {
    is.list(reg) && is.list(reg$companies) && length(reg$companies) > 0 &&
      any(vapply(reg$companies, function(co) {
        length(co$ar %||% character()) > 0 || length(co$ap %||% character()) > 0
      }, logical(1)))
  }
  if (!.v2_has_codes(out)) {
    old <- tryCatch(load_interco(), error = function(e) NULL)
    if (!is.null(old) && (length(old$ar_clients) > 0 || length(old$ap_suppliers) > 0)) {
      message(sprintf("[IC] v2 has no codes — auto-migrating from legacy interco_settings.rds (%d AR, %d AP codes)",
                      length(old$ar_clients), length(old$ap_suppliers)))
      out <- .migrate_interco_v1_to_v2(old)
      tryCatch(save_interco_v2(out), error = function(e)
        message("[IC] auto-migration save failed: ", e$message))
      return(out)
    }
    if (!is.list(out) || is.null(out$companies)) return(.empty_interco_v2())
  }

  out$ar_prefix <- out$ar_prefix %||% "C"
  out$ap_prefix <- out$ap_prefix %||% "P"
  # rfcs: named character vector full_code → RFC (e.g. c("C1027" = "NCS060103RF4"))
  # Absent in registries written before this field existed → default to empty.
  if (is.null(out$rfcs)) out$rfcs <- character()
  out
}

save_interco_v2 <- function(registry, client_id = NULL) {
  .s3_write(registry, S3_KEYS$interco_v2, client_id = client_id)
  # Populate both in-memory caches so subsequent reads skip S3 entirely
  if (exists(".cache_set", mode = "function")) .cache_set("interco_v2", registry)
  assign(S3_KEYS$interco_v2, registry, envir = .s3_preload_cache)
  message(sprintf("[IC] save_interco_v2 — S3 write OK (%d companies)",
                  length(registry$companies %||% list())))
  invisible(registry)
}
# ── Pagar Hoy — staged payment queue ──────────────────────────────────────────

.schema_pagar_hoy <- function() tibble(
  id           = character(),
  ledger       = character(),   # "AP" or "AR"
  Empresa      = character(),
  Moneda       = character(),
  Documento    = character(),
  Parte        = character(),
  Codigo       = character(),
  tipo_item    = character(),   # "factura" (default) | "abono"
  Importe      = numeric(),
  FechaVenc    = as.Date(character()),
  staged_by    = character(),
  staged_at    = as.POSIXct(character()),
  status       = character(),   # "pending" | "confirmed" | "cancelled"
  provision_id = character(),   # FK to pasivos_provisions; NA for non-provision items
  liability_id = character(),   # FK to pasivos_liabilities; NA for non-provision items
  source       = character(),   # "sap" | "manual" | "provision"; NA treated as "sap"
  confirmed_at = as.POSIXct(character())
)

load_pagar_hoy <- function(client_id = NULL) {
  df <- .normalize(.s3_read_with(S3_KEYS$pagar_hoy, client_id = client_id), .schema_pagar_hoy)
  df$status[is.na(df$status) | !nzchar(trimws(df$status))] <- "pending"
  df$source[is.na(df$source) | !nzchar(trimws(df$source %||% ""))] <- "sap"
  dplyr::filter(df, status %in% c("pending", "confirmed", "cancelled"))
}

save_pagar_hoy <- function(df, username = NULL, client_id = NULL) {
  norm <- .normalize(df, .schema_pagar_hoy)
  sync_on <- isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE))
  if (sync_on) {
    # Sync mode: write to shared file and update in-memory state for all sessions
    .s3_write(norm, S3_KEYS$pagar_hoy_sync, client_id = client_id)
    .GlobalEnv$.agenda_sync$data <- norm
    .GlobalEnv$.agenda_sync$version <- paste0(
      sample(c(letters, as.character(0:9)), 12, replace = TRUE), collapse = "")
    if (exists("bump_sync_version", mode = "function")) bump_sync_version("pagar_hoy_db", client_id = client_id)
  } else if (!is.null(username) && nzchar(username %||% "")) {
    # Per-user mode: write to this user's personal file
    .s3_write(norm, s3_key_pagar_hoy_user(username), client_id = client_id)
    # Cache so same-user sessions can reload via sync bus per_session_loader
    ukey <- tolower(trimws(username))
    if (!is.list(tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)))
      .GlobalEnv$.agenda_user_cache <- list()
    .GlobalEnv$.agenda_user_cache[[ukey]] <- norm
    if (exists("bump_sync_version", mode = "function"))
      bump_sync_version("pagar_hoy_db", client_id = client_id)
  } else {
    # Legacy fallback: shared file (backward compat for callers without a username)
    .s3_write(norm, S3_KEYS$pagar_hoy, client_id = client_id)
    if (exists(".cache_set", mode = "function")) .cache_set("pagar_hoy", norm)
  }
  invisible(TRUE)
}

# ── Per-user agenda helpers (sync OFF) ────────────────────────────────────────

s3_key_pagar_hoy_user <- function(username) {
  paste0("pagar_hoy_user_", tolower(trimws(username)), ".rds")
}

load_pagar_hoy_user <- function(username, client_id = NULL) {
  df <- .normalize(.s3_read_with(s3_key_pagar_hoy_user(username), client_id = client_id),
                   .schema_pagar_hoy)
  df$status[is.na(df$status) | !nzchar(trimws(df$status))] <- "pending"
  df$source[is.na(df$source) | !nzchar(trimws(df$source %||% ""))] <- "sap"
  dplyr::filter(df, status %in% c("pending", "confirmed", "cancelled"))
}

save_pagar_hoy_user <- function(df, username, client_id = NULL) {
  norm <- .normalize(df, .schema_pagar_hoy)
  .s3_write(norm, s3_key_pagar_hoy_user(username), client_id = client_id)
  invisible(TRUE)
}

# ── Shared sync agenda helpers (sync ON) ──────────────────────────────────────

load_pagar_hoy_sync <- function(client_id = NULL) {
  df <- .normalize(.s3_read_with(S3_KEYS$pagar_hoy_sync, client_id = client_id), .schema_pagar_hoy)
  df$status[is.na(df$status) | !nzchar(trimws(df$status))] <- "pending"
  df$source[is.na(df$source) | !nzchar(trimws(df$source %||% ""))] <- "sap"
  dplyr::filter(df, status %in% c("pending", "confirmed", "cancelled"))
}

save_pagar_hoy_sync <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_pagar_hoy)
  .s3_write(norm, S3_KEYS$pagar_hoy_sync, client_id = client_id)
  invisible(TRUE)
}

# ── Saldos de apertura (opening balances) persistence ─────────────────────────
# Stored per-user always (each user has their own cash-position baseline).
# S3 key: saldos_apertura_user_<username>.rds inside the client prefix.

s3_key_saldos_apertura_user <- function(username) {
  paste0("saldos_apertura_user_", tolower(trimws(username)), ".rds")
}

load_saldos_apertura_user <- function(username, client_id = NULL) {
  obj <- tryCatch(
    .s3_read_with(s3_key_saldos_apertura_user(username), client_id = client_id),
    error = function(e) NULL)
  if (!is.list(obj)) return(list())
  obj
}

save_saldos_apertura_user <- function(sa, username, client_id = NULL) {
  if (!is.list(sa)) return(invisible(FALSE))
  tryCatch({
    .s3_write(sa, s3_key_saldos_apertura_user(username), client_id = client_id)
    invisible(TRUE)
  }, error = function(e) {
    message("[AGENDA] saldos_apertura save failed for '", username, "': ", e$message)
    invisible(FALSE)
  })
}

# ── Agenda sync configuration ─────────────────────────────────────────────────

.schema_agenda_sync_config <- function() tibble::tibble(
  is_enabled = logical(),
  enabled_by = character(),
  enabled_at = as.POSIXct(character())
)

load_agenda_sync_config <- function(client_id = NULL) {
  df <- .normalize(.s3_read_with(S3_KEYS$agenda_sync_config, client_id = client_id),
                   .schema_agenda_sync_config)
  if (!nrow(df)) return(list(is_enabled = FALSE, enabled_by = NA_character_,
                             enabled_at = NA, .missing = TRUE))
  list(is_enabled = isTRUE(df$is_enabled[1]),
       enabled_by = df$enabled_by[1],
       enabled_at = df$enabled_at[1],
       .missing   = FALSE)
}

save_agenda_sync_config <- function(is_enabled, enabled_by, client_id = NULL) {
  df <- tibble::tibble(
    is_enabled = isTRUE(is_enabled),
    enabled_by = as.character(enabled_by %||% "unknown"),
    enabled_at = Sys.time()
  )
  .s3_write(df, S3_KEYS$agenda_sync_config, client_id = client_id)
  invisible(TRUE)
}

# ── Add or update rows. ────────────────────────────────────────────────────────
# keys defaults to business key (Empresa+Moneda+Documento+ledger) for SAP rows.
# Pass keys = "id" for manual entries to avoid replacing entries that share a
# Documento but are different invoices (e.g. two manual entries for the same vendor).
upsert_pagar_hoy <- function(db, new_rows,
                             keys = c("ledger", "Empresa", "Moneda", "Documento")) {
  if (!nrow(new_rows)) return(db)
  db <- dplyr::anti_join(db, new_rows, by = keys)
  dplyr::bind_rows(db, new_rows)
}

# Sync-aware load: reads from the correct source depending on sync state.
# Used as a fallback when the in-memory reactiveVal hasn't been populated yet
# (e.g., item added via Agregar before Phase 2 finishes loading).
safe_load_pagar_hoy <- function(username = NULL, client_id = NULL) {
  sync_on <- isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE))
  if (sync_on) {
    d <- tryCatch(.GlobalEnv$.agenda_sync$data, error = function(e) NULL)
    if (!is.null(d)) return(d)
    tryCatch(load_pagar_hoy_sync(client_id = client_id),
             error = function(e) .schema_pagar_hoy())
  } else if (!is.null(username) && nzchar(username %||% "")) {
    tryCatch(load_pagar_hoy_user(username, client_id = client_id),
             error = function(e) tryCatch(load_pagar_hoy(client_id = client_id),
                                          error = function(e2) .schema_pagar_hoy()))
  } else {
    tryCatch(load_pagar_hoy(client_id = client_id), error = function(e) .schema_pagar_hoy())
  }
}

# Remove specific invoices from the queue entirely
unstage_pagar_hoy <- function(db, keys_df,
                              keys = c("ledger", "Empresa", "Moneda", "Documento")) {
  if (!nrow(keys_df)) return(db)
  dplyr::anti_join(db, keys_df, by = keys)
}

# ── SAP data snapshots (outage resilience + rotation) ─────────────────────────
# After each successful SAP fetch, save a snapshot to S3 and rotate the two
# previous snapshots as emergency backups.
#
# Rotation order (newest → oldest):
#   current  → sap_snapshot_{AR|AP}.rds           (the one Phase 1 uses)
#   prev1    → sap_snapshot_{AR|AP}_prev1.rds      (one fetch back)
#   prev2    → sap_snapshot_{AR|AP}_prev2.rds      (two fetches back, oldest kept)
#
# Only the CURRENT snapshot ever feeds the calendar.  prev1/prev2 are S3-only
# safety copies in case a fetch produces bad data — they are never loaded
# automatically and never bleed into the live calendar.
#
# assign() after every .s3_write() restores the preload cache entry so the next
# session's Phase 1 gets a cache hit (~0 ms) instead of a live S3 read (~35 s).

save_sap_snapshot <- function(df, ledger, client_id = NULL) {
  ledger    <- toupper(ledger)
  cur_key   <- if (ledger == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap

  # Derive backup key names from the current key:
  #   "sap_snapshot_AR.rds" → "sap_snapshot_AR_prev1.rds" / "_prev2.rds"
  prev1_key <- sub("\\.rds$", "_prev1.rds", cur_key)
  prev2_key <- sub("\\.rds$", "_prev2.rds", cur_key)

  # ── Step 1: rotate prev1 → prev2 ────────────────────────────────────────────
  tryCatch({
    prev1_obj <- if (exists(prev1_key, envir = .s3_preload_cache, inherits = FALSE))
      .s3_preload_cache[[prev1_key]]
    else
      .s3_read(prev1_key)
    if (!is.null(prev1_obj)) {
      .s3_write(prev1_obj, prev2_key, client_id = client_id)
      assign(prev2_key, prev1_obj, envir = .s3_preload_cache)
      message(sprintf("[SNAP] %s prev1→prev2 rotated (saved_at=%s)",
                      ledger, format(prev1_obj$saved_at, "%d/%m %H:%M")))
    }
  }, error = function(e)
    message("[SNAP] ", ledger, " rotate prev1→prev2 FAILED: ", e$message))

  # ── Step 2: rotate current → prev1 ─────────────────────────────────────────
  tryCatch({
    cur_obj <- if (exists(cur_key, envir = .s3_preload_cache, inherits = FALSE))
      .s3_preload_cache[[cur_key]]
    else
      .s3_read(cur_key)
    if (!is.null(cur_obj)) {
      .s3_write(cur_obj, prev1_key, client_id = client_id)
      assign(prev1_key, cur_obj, envir = .s3_preload_cache)
      message(sprintf("[SNAP] %s cur→prev1 rotated (saved_at=%s)",
                      ledger, format(cur_obj$saved_at, "%d/%m %H:%M")))
    }
  }, error = function(e)
    message("[SNAP] ", ledger, " rotate cur→prev1 FAILED: ", e$message))

  # ── Step 3: write new snapshot as current ───────────────────────────────────
  obj <- list(data = df, saved_at = Sys.time())
  tryCatch(.s3_write(obj, cur_key, client_id = client_id), error = function(e)
    warning("Could not save SAP snapshot: ", e$message, call. = FALSE))
  # Restore preload cache after .s3_write() cleared it (see comment above).
  assign(cur_key, obj, envir = .s3_preload_cache)
  message(sprintf("[SNAP] %s snapshot saved (%d rows, saved_at=%s) — prev1/prev2 rotated",
                  ledger, nrow(df), format(obj$saved_at, "%H:%M:%S")))
  invisible(obj)
}

load_sap_snapshot <- function(ledger) {
  key     <- if (toupper(ledger) == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
  cached  <- exists(key, envir = .s3_preload_cache, inherits = FALSE)
  t0_snap <- proc.time()
  obj     <- .s3_read(key)
  elapsed <- round((proc.time() - t0_snap)[["elapsed"]], 2)
  if (is.null(obj) || !is.list(obj)) {
    message(sprintf("[SNAP] %s — NULL (%s, %.2fs)", toupper(ledger),
                    if (cached) "cache-hit/NULL" else "live-miss/NULL", elapsed))
    return(NULL)
  }
  rows <- if (!is.null(obj$data)) nrow(obj$data) else 0L
  message(sprintf("[SNAP] %s — %d rows  %.2fs  [%s]  saved_at=%s",
                  toupper(ledger), rows, elapsed,
                  if (cached) "cache-hit" else "LIVE S3 READ",
                  format(obj$saved_at, "%d/%m %H:%M")))
  list(data = obj$data, saved_at = obj$saved_at)
}

# ── Conciliación — permanent payment ledger ────────────────────────────────────

.schema_conciliacion <- function() tibble(
  id                   = character(),
  tipo                 = character(),   # "pago" | "cobro"
  Empresa              = character(),
  Parte                = character(),
  Documento            = character(),
  Moneda               = character(),
  Importe              = numeric(),
  comision             = numeric(),
  FechaPago            = as.Date(character()),
  FechaContabilizacion = as.Date(character()),
  FechaVencimiento     = as.Date(character()),
  cuenta_id            = character(),
  notas                = character(),
  created_by           = character(),
  created_at           = as.POSIXct(character())
)

load_conciliacion <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$conciliacion, client_id = client_id), .schema_conciliacion)
}

save_conciliacion <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_conciliacion), S3_KEYS$conciliacion, client_id = client_id)
}

# ── Bancos — bank account registry ────────────────────────────────────────────

.schema_bancos <- function() tibble(
  id       = character(),
  Empresa  = character(),
  banco    = character(),
  alias    = character(),
  clabe    = character(),
  cuenta   = character(),
  Moneda   = character(),
  tipo     = character(),   # "012" | "021"
  activa   = logical()
)

load_bancos <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$bancos, client_id = client_id), .schema_bancos)
}

save_bancos <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_bancos), S3_KEYS$bancos, client_id = client_id)
}

# ── Proveedores — supplier registry for Bajío file ────────────────────────────

# =============================================================================
# REPLACE the .schema_proveedores / load_proveedores / save_proveedores block
# in persistence.R with this version.
#
# Key changes vs old schema:
#   + alias      (char, max 15) — BanBajío catalog alias, the PPL key field
#   + medio_pago (char)         — "SPI" | "BCO"
#   + rfc        (char)         — RFC del beneficiario
# The old fields (codigo, tipo, banco_destino) are preserved for compatibility.
# =============================================================================

# ── Proveedores — supplier registry for BanBajío PPL file ────────────────────

.schema_proveedores <- function() tibble::tibble(
  id            = character(),
  Empresa       = character(),   # NCS / NTS / etc. (empty = all companies)
  codigo        = character(),   # SAP CardCode (future: link to AP invoices)
  nombre        = character(),   # Full legal name (max 40 chars for PPL)
  alias         = character(),   # BanBajío alias (max 15 chars) ← KEY FIELD
  clabe         = character(),   # 18-digit CLABE (SPI) or internal account (BCO)
  medio_pago    = character(),   # "SPI" | "BCO" | "SPD" | "TEF"
  rfc           = character(),   # RFC (used in SPEI concepto auto-match)
  tipo          = character(),   # legacy: "012" | "021" CECOBAN code
  banco_destino = character(),   # legacy: destination bank name
  activo        = logical(),
  no_cuenta    = character(),   # bank account number — dedup key
  banco        = character(),   # e.g. "012-BBVA BANCOMER"
  tipo_cuenta  = character(),   # e.g. "40"
  correo       = character(),   # comma-separated emails
  moneda       = character(),   # "Pesos" | "Dólares"
  tipo_relacion = character(),  # free text
  empresas     = character(),   # comma-separated: "NTS,NCS" etc.
  status       = character(),   # "completo" | "incompleto"
  fuente       = character(),   # "excel" | "manual"
  upload_batch = character(),   # UUID — import session identifier
  activo_hasta = character(),   # ISO date string or "indefinido"
  created_at   = character(),   # POSIXct as character for RDS portability
  updated_at   = character()
)

load_proveedores <- function(client_id = NULL) {
  raw <- .s3_read_with(S3_KEYS$proveedores, client_id = client_id)
  df  <- .normalize(raw, .schema_proveedores)
  # Back-compat: if alias column is all NA, copy from nombre (trimmed to 15)
  if (nrow(df) > 0 && all(is.na(df$alias) | !nzchar(trimws(df$alias)))) {
    df$alias <- substr(toupper(trimws(df$nombre)), 1, 15)
  }
  # Back-compat: if medio_pago missing, default to SPI
  if (nrow(df) > 0 && all(is.na(df$medio_pago) | !nzchar(trimws(df$medio_pago)))) {
    df$medio_pago <- "SPI"
  }
  df |> dplyr::filter(activo == TRUE)
}

save_proveedores <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_proveedores), S3_KEYS$proveedores, client_id = client_id)
}

load_proveedores_inactivos <- function(client_id = NULL) {
  raw <- .s3_read_with(S3_KEYS$proveedores_inactivos, client_id = client_id)
  .normalize(raw, .schema_proveedores)
}

save_proveedores_inactivos <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_proveedores), S3_KEYS$proveedores_inactivos, client_id = client_id)
}

# =============================================================================
# Parte → Alias override map
# Stores manual links between SAP Parte names and catalog aliases.
# Schema: Parte (chr), Empresa (chr — initials or "" for all), alias (chr),
#         linked_by (chr), linked_at (chr)
# =============================================================================
.schema_parte_alias_map <- function() tibble::tibble(
  Parte      = character(),
  Empresa    = character(),
  alias      = character(),
  linked_by  = character(),
  linked_at  = character()
)

load_parte_alias_map <- function(client_id = NULL) {
  obj <- tryCatch(.s3_read_with(S3_KEYS$parte_alias_map, client_id = client_id), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_parte_alias_map())
  schema <- .schema_parte_alias_map()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  tibble::as_tibble(obj)
}

save_parte_alias_map <- function(df, client_id = NULL) {
  .s3_write(df, S3_KEYS$parte_alias_map, client_id = client_id)
  invisible(TRUE)
}

# ── Empresas — company registry ────────────────────────────────────────────────
# Source of truth for all companies in the group.
# initials (e.g. "NCS") is the stable key used throughout the app.
# nombres_alt is a JSON array of alternative names for search/matching.

.schema_empresas <- function() tibble::tibble(
  id           = character(),
  account_code = character(),   # E0001, E0002, ...
  initials     = character(),   # NCS, NTS, NL, NRS, NG  (stable key)
  razon_social = character(),   # Full legal name
  nombre_corto = character(),   # Short display name
  nombres_alt  = character(),   # JSON: ["alias1", "alias2"]
  rfc          = character(),
  activa       = logical(),
  deleted      = logical(),
  deleted_at   = character(),
  created_at   = character(),
  updated_at   = character(),
  group_id     = character()    # FK → grupos$id; which conglomerate owns this company
)

# Load empresas from S3. Returns the empty schema on first run (no auto-seeding).
# Companies are added via the Empresas module in Settings.
load_empresas <- function(client_id = NULL) {
  raw <- tryCatch(suppressMessages(.s3_read_with(S3_KEYS$empresas, client_id = client_id)), error = function(e) NULL)

  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    message("[EMP] No empresas.rds found — returning empty schema.")
    return(.schema_empresas())
  }

  df <- .normalize(raw, .schema_empresas)

  # Back-fill account_code for records without one
  if (!"account_code" %in% names(df)) df$account_code <- NA_character_
  needs_code <- is.na(df$account_code) | !nzchar(trimws(df$account_code))
  if (any(needs_code)) {
    existing_nums <- suppressWarnings(as.integer(
      sub("^E0*", "", df$account_code[grepl("^E\\d+$", df$account_code)])))
    existing_nums <- existing_nums[!is.na(existing_nums)]
    next_n <- if (length(existing_nums)) max(existing_nums) + 1L else 1L
    for (i in which(needs_code)) {
      df$account_code[i] <- sprintf("E%04d", next_n)
      next_n <- next_n + 1L
    }
    tryCatch(
      .s3_write(df, S3_KEYS$empresas, client_id = client_id),
      error = function(e) message("[EMP] account_code backfill failed: ", e$message)
    )
  }

  # Back-fill group_id for records that pre-date the multi-tenant model.
  # Assign them to the first (and normally only) group for this deployment.
  if (!"group_id" %in% names(df) || all(is.na(df$group_id) | !nzchar(trimws(df$group_id)))) {
    grp <- tryCatch(load_grupos(client_id = client_id), error = function(e) .schema_grupos())
    default_gid <- if (nrow(grp)) grp$id[1] else NA_character_
    df$group_id <- default_gid
    tryCatch(
      .s3_write(.normalize(df, .schema_empresas), S3_KEYS$empresas, client_id = client_id),
      error = function(e) message("[EMP] group_id backfill save failed: ", e$message)
    )
  } else {
    needs_gid <- is.na(df$group_id) | !nzchar(trimws(df$group_id))
    if (any(needs_gid)) {
      grp <- tryCatch(load_grupos(client_id = client_id), error = function(e) .schema_grupos())
      default_gid <- if (nrow(grp)) grp$id[1] else NA_character_
      df$group_id[needs_gid] <- default_gid
      tryCatch(
        .s3_write(.normalize(df, .schema_empresas), S3_KEYS$empresas, client_id = client_id),
        error = function(e) message("[EMP] partial group_id backfill save failed: ", e$message)
      )
    }
  }

  df
}

# Throws on failure — callers must handle errors and notify the user.
save_empresas <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_empresas), S3_KEYS$empresas, client_id = client_id)
  invisible(TRUE)
}

# ── Grupos (conglomerate / client groups) ──────────────────────────────────────
# A "grupo" is a named conglomerate that owns a set of companies within one
# client deployment.  Each deployment (CLIENT_ID) can have multiple groups
# (though typically just one).  Users are assigned to one or more groups;
# they only see companies that belong to their groups.
#
# S3 path:  <client_id>/grupos.rds
# No client mixing can occur because each deployment has its own S3 prefix.

.schema_grupos <- function() tibble::tibble(
  id         = character(),   # UUID
  name       = character(),   # "Networks Group"
  companies  = character(),   # JSON array of empresa initials e.g. '["NG","NTS"]'
  client_id  = character(),   # deployment slug e.g. "networks", "hopdesk"
  created_at = character(),
  updated_at = character()
)

# Parse the companies JSON field into a character vector.
grupo_companies <- function(row) {
  tryCatch(jsonlite::fromJSON(row$companies %||% "[]"), error = function(e) character())
}

load_grupos <- function(client_id = NULL) {
  raw <- tryCatch(suppressMessages(.s3_read_with(S3_KEYS$grupos, client_id = client_id)), error = function(e) NULL)

  if (!is.null(raw) && is.data.frame(raw) && nrow(raw)) {
    df <- .normalize(raw, .schema_grupos)

    # Backfill client_id for grupos created before this field existed
    needs_cid <- is.na(df$client_id) | !nzchar(trimws(df$client_id))
    if (any(needs_cid)) {
      for (i in which(needs_cid)) {
        df$client_id[i] <- if (tolower(trimws(df$name[i])) == "hopdesk") {
          "hopdesk"
        } else {
          tolower(client_id %||% Sys.getenv("CLIENT_ID"))
        }
      }
      tryCatch(
        .s3_write(df, S3_KEYS$grupos, client_id = client_id),
        error = function(e) message("[GRP] client_id backfill save failed: ", e$message)
      )
      message(sprintf("[GRP] client_id backfilled for %d grupo(s)", sum(needs_cid)))
    }

    return(df)
  }

  # No grupos.rds — return empty schema. Groups are created via the UI.
  message("[GRP] No grupos.rds found — returning empty schema.")
  .schema_grupos()
}

save_grupos <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_grupos), S3_KEYS$grupos, client_id = client_id)
  if (exists("bump_sync_version", mode = "function")) bump_sync_version("grupos_db", client_id = client_id)
  invisible(TRUE)
}

# ── Group configuration (display name + logo) ──────────────────────────────────

.schema_group_config <- function() {
  list(group_name = "", logo_raw = NULL, logo_ext = "png")
}

load_group_config <- function(client_id = NULL) {
  cfg <- tryCatch(.s3_read_with(S3_KEYS$group_config, client_id = client_id), error = function(e) NULL)
  if (is.null(cfg) || !is.list(cfg)) return(.schema_group_config())
  defaults <- .schema_group_config()
  for (nm in names(defaults)) if (is.null(cfg[[nm]])) cfg[[nm]] <- defaults[[nm]]
  cfg
}

save_group_config <- function(cfg, client_id = NULL) {
  .s3_write(cfg, S3_KEYS$group_config, client_id = client_id)
  invisible(cfg)
}

# Cascade-rename a company's initials across every S3 table that stores them as
# a key.  Called by empresas_module.R when the user edits initials in the edit
# panel.  All operations are best-effort: a failure in one table is logged and
# does NOT abort the rest.  Returns (invisibly) a character vector of the S3
# table keys that were actually modified.
#
# Tables updated:
#   ctas_cuentas     — Empresa column  (uppercase initials, settings_module)
#   bancos_cuentas   — empresa column  (lowercase; bancos_persistence schema)
#   proveedores      — Empresa column  (persistence.R schema)
#   parte_alias_map  — Empresa column  (persistence.R schema)
#   interco_v2       — companies list  (named list keyed by initials)
rename_empresa_initials <- function(old_ini, new_ini, client_id = NULL) {
  touched <- character(0)

  # Replace old_ini with new_ini in one column of a data frame (exact match).
  .rni_col <- function(df, col) {
    if (!col %in% names(df)) return(df)
    hits <- !is.na(df[[col]]) & df[[col]] == old_ini
    if (any(hits)) df[[col]][hits] <- new_ini
    df
  }

  # 1. ctas_cuentas — Empresa column stores initials (e.g. "NCS")
  tryCatch({
    df <- load_ctas_cuentas(client_id = client_id)
    if (nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      save_ctas_cuentas(df, client_id = client_id)
      touched <- c(touched, "ctas_cuentas")
    }
  }, error = function(e) message("[EMP rename] ctas_cuentas: ", e$message))

  # 2. bancos_cuentas — empresa column (lowercase; bancos_persistence.R schema)
  tryCatch({
    df <- load_bancos_cuentas(client_id = client_id)
    if (nrow(df) && any(!is.na(df$empresa) & df$empresa == old_ini)) {
      df <- .rni_col(df, "empresa")
      save_bancos_cuentas(df, client_id = client_id)
      touched <- c(touched, "bancos_cuentas")
    }
  }, error = function(e) message("[EMP rename] bancos_cuentas: ", e$message))

  # 3. proveedores — Empresa column
  tryCatch({
    df <- load_proveedores(client_id = client_id)
    if (nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      save_proveedores(df, client_id = client_id)
      touched <- c(touched, "proveedores")
    }
  }, error = function(e) message("[EMP rename] proveedores: ", e$message))

  # 4. parte_alias_map — Empresa column
  tryCatch({
    raw <- tryCatch(.s3_read_with(S3_KEYS$parte_alias_map, client_id = client_id), error = function(e) NULL)
    df  <- if (is.data.frame(raw)) .normalize(raw, .schema_parte_alias_map) else NULL
    if (!is.null(df) && nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      .s3_write(.normalize(df, .schema_parte_alias_map), S3_KEYS$parte_alias_map, client_id = client_id)
      touched <- c(touched, "parte_alias_map")
    }
  }, error = function(e) message("[EMP rename] parte_alias_map: ", e$message))

  # 5. interco_v2 — companies is a named list keyed by initials
  tryCatch({
    reg <- load_interco_v2(client_id = client_id)
    if (!is.null(reg$companies) && old_ini %in% names(reg$companies)) {
      reg$companies[[new_ini]] <- reg$companies[[old_ini]]
      reg$companies[[old_ini]] <- NULL
      save_interco_v2(reg, client_id = client_id)
      touched <- c(touched, "interco_v2")
    }
  }, error = function(e) message("[EMP rename] interco_v2: ", e$message))

  message(sprintf("[EMP rename] %s \u2192 %s \u2014 touched: %s",
                  old_ini, new_ini,
                  if (length(touched)) paste(touched, collapse = ", ") else "nothing"))
  invisible(touched)
}

# ── Abonos parciales — partial payment records ─────────────────────────────────
# Each row records one partial payment applied against a specific invoice.
# status: "active" (reduces displayed balance) | "voided" (ignored by pipeline)
# Key for upsert/void is `id` (UUID) — multiple abonos per invoice allowed.

.schema_abonos <- function() tibble(
  id          = character(),
  ledger      = character(),         # "AR" or "AP"
  Empresa     = character(),
  Moneda      = character(),
  Documento   = character(),         # SAP DocNum or manual invoice reference
  Parte       = character(),
  importe     = numeric(),           # payment amount (positive)
  fecha_abono = as.Date(character()),
  notas       = character(),
  created_by  = character(),
  created_at  = as.POSIXct(character()),
  status      = character()          # "active" | "voided"
)

load_abonos <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$abonos, client_id = client_id), .schema_abonos) |>
    dplyr::filter(status %in% c("active", "voided"))
}

save_abonos <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_abonos), S3_KEYS$abonos, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("abonos", df)
}

# Add new abono records (each has a fresh UUID — never replaces existing)
upsert_abono <- function(db, new_rows) {
  if (!nrow(new_rows)) return(db)
  dplyr::bind_rows(db, new_rows)
}

# Soft-delete: mark an abono as voided by id
void_abono <- function(db, ids) {
  if (!length(ids)) return(db)
  db |> dplyr::mutate(
    status = dplyr::if_else(.data$id %in% ids, "voided", status)
  )
}

# ── SAP field overrides ────────────────────────────────────────────────────────
# Stores user-editable field values that paint on top of SAP data in the UI.
# The underlying sap_data() snapshot is NEVER modified; overrides are joined in
# build_ledger_df() at display time only.
# Key: ledger + Empresa + Moneda + Documento (mirrors the moves_db key convention).

.schema_sap_overrides <- function() tibble::tibble(
  id               = character(),
  ledger           = character(),
  Empresa          = character(),
  Moneda           = character(),
  Documento        = character(),
  Parte_override   = character(),
  Codigo_override  = character(),
  Factura_override = character(),
  Notas_override   = character(),
  Moneda_override  = character(),
  Importe_override = numeric(),
  updated_by       = character(),
  updated_at       = as.POSIXct(character())
)

load_sap_overrides <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$sap_overrides, client_id = client_id), .schema_sap_overrides)
}

save_sap_overrides <- function(df, client_id = NULL) {
  norm <- .normalize(df, .schema_sap_overrides)
  .s3_write(norm, S3_KEYS$sap_overrides, client_id = client_id)
  if (exists(".cache_set", mode = "function")) .cache_set("sap_overrides", norm)
  invisible(TRUE)
}

upsert_sap_override <- function(db, new_row) {
  if (is.null(db) || !is.data.frame(db)) db <- .schema_sap_overrides()
  db <- dplyr::filter(db,
    !(.data$ledger    == new_row$ledger    &
      .data$Empresa   == new_row$Empresa   &
      .data$Moneda    == new_row$Moneda    &
      .data$Documento == new_row$Documento))
  dplyr::bind_rows(db, new_row)
}

# Convenience: return only active abonos aggregated by invoice key
# Returns: (ledger, Empresa, Moneda, Documento, abono_total)
active_abonos_summary <- function(db) {
  if (is.null(db) || !nrow(db)) {
    return(tibble(
      ledger = character(), Empresa = character(),
      Moneda = character(), Documento = character(),
      abono_total = numeric()
    ))
  }
  db |>
    dplyr::filter(status == "active") |>
    dplyr::group_by(ledger, Empresa, Moneda, Documento) |>
    dplyr::summarise(abono_total = sum(importe, na.rm = TRUE), .groups = "drop")
}

# =============================================================================
# Client Management — Stage 3
# All files live under hd-admin/ and are read/written directly (no .s3_key()).
# =============================================================================

# ── Schemas ──────────────────────────────────────────────────────────────────

.schema_client_registry <- function() {
  data.frame(
    client_id     = character(),
    display_name  = character(),
    max_users     = integer(),
    current_users = integer(),
    contact_email = character(),
    status        = character(),   # "active" / "archived"
    created_at    = character(),
    created_by    = character(),
    stringsAsFactors = FALSE
  )
}

.schema_client_requests <- function() {
  data.frame(
    id                 = character(),
    requested_by       = character(),
    client_id_proposed = character(),
    display_name       = character(),
    contact_email      = character(),
    max_users          = integer(),
    notes              = character(),
    status             = character(),   # "pending" / "approved" / "rejected"
    requested_at       = character(),
    reviewed_by        = character(),
    reviewed_at        = character(),
    rejection_reason   = character(),
    stringsAsFactors   = FALSE
  )
}

.schema_notifications <- function() {
  data.frame(
    id         = character(),
    type       = character(),   # "user_limit_warning" / "user_limit_reached" / "user_request"
    client_id  = character(),
    message    = character(),
    created_at = character(),
    read_by    = character(),   # JSON array of usernames
    metadata   = character(),   # JSON
    stringsAsFactors = FALSE
  )
}

# Empty usuarios schema used when initializing a new client folder
.schema_usuarios_init <- function() {
  data.frame(
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
  )
}

# ── Read / Write helpers ──────────────────────────────────────────────────────

read_client_registry <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = "hd-admin/client_registry.rds", bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_client_registry())
  raw
}

write_client_registry <- function(df) {
  aws.s3::s3saveRDS(df, object = "hd-admin/client_registry.rds", bucket = .s3_bucket())
  invisible(TRUE)
}

read_client_requests <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = "hd-admin/client_requests.rds", bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_client_requests())
  raw
}

write_client_requests <- function(df) {
  aws.s3::s3saveRDS(df, object = "hd-admin/client_requests.rds", bucket = .s3_bucket())
  invisible(TRUE)
}

read_hd_notifications <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = "hd-admin/notifications.rds", bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_notifications())
  raw
}

write_hd_notifications <- function(df) {
  aws.s3::s3saveRDS(df, object = "hd-admin/notifications.rds", bucket = .s3_bucket())
  invisible(TRUE)
}

# ── initialize_client_folder() ───────────────────────────────────────────────
# Called on approval of a new client request. Writes four empty schema files
# directly to <client_id>/ prefix — does NOT go through .s3_key() to avoid
# mutating the global CLIENT_ID env var.
#
# Future IAM improvement: each client deployment should have its own IAM key
# scoped to its own prefix. When that's in place, this function would need to
# use a cross-client IAM role or the principal's admin key to write to
# arbitrary prefixes.

initialize_client_folder <- function(client_id, created_by = "system") {
  stopifnot(nzchar(client_id), !grepl("[^a-z0-9_-]", client_id))
  bucket <- .s3_bucket()

  s3_put <- function(suffix, data) {
    aws.s3::s3saveRDS(data,
                      object = paste0(client_id, "/", suffix),
                      bucket = bucket)
  }

  s3_put("usuarios.rds",  .schema_usuarios_init())
  s3_put("empresas.rds",  as.data.frame(.schema_empresas()))
  s3_put("grupos.rds",    as.data.frame(.schema_grupos()))
  s3_put("contacts.rds",  .schema_contacts())
  s3_put("app_audit.rds", tibble::tibble(
    id = character(), ts = as.POSIXct(character()),
    user = character(), module = character(), action = character(),
    description = character(), target_id = character(), metadata = character()
  ))

  message(sprintf("[CLIENT] Folder initialized for '%s' by '%s'", client_id, created_by))
  invisible(TRUE)
}

# ── update_client_user_count() ───────────────────────────────────────────────
# Recomputes current_users from the client's live usuarios.rds and updates
# client_registry.rds. Fire-and-forget — never throws.

update_client_user_count <- function(client_id) {
  tryCatch({
    bucket <- .s3_bucket()
    raw <- suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = paste0(client_id, "/usuarios.rds"), bucket = bucket)
    ))
    n_active <- if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) 0L else {
      nrow(raw[is.na(raw$deleted) | raw$deleted != TRUE, , drop = FALSE])
    }
    registry <- read_client_registry()
    if (!nrow(registry)) return(invisible(FALSE))
    idx <- which(registry$client_id == client_id)
    if (!length(idx)) return(invisible(FALSE))
    registry$current_users[idx] <- n_active
    write_client_registry(registry)
    invisible(TRUE)
  }, error = function(e) {
    message("[CLIENT] update_client_user_count failed for '", client_id, "': ", e$message)
    invisible(FALSE)
  })
}

# ── append_hd_notification() ─────────────────────────────────────────────────
# Adds one notification row to hd-admin/notifications.rds. Fire-and-forget.

append_hd_notification <- function(type, client_id, message_text, metadata = list()) {
  tryCatch({
    existing <- read_hd_notifications()
    new_row <- data.frame(
      id         = uuid::UUIDgenerate(),
      type       = type,
      client_id  = client_id,
      message    = message_text,
      created_at = as.character(Sys.time()),
      read_by    = "[]",
      metadata   = tryCatch(jsonlite::toJSON(metadata, auto_unbox = TRUE),
                             error = function(e) "{}"),
      stringsAsFactors = FALSE
    )
    write_hd_notifications(rbind(existing, new_row))
    invisible(TRUE)
  }, error = function(e) {
    message("[CLIENT] append_hd_notification failed: ", e$message)
    invisible(FALSE)
  })
}

# =============================================================================
# Stage 5 — Pending Invites
# Stored at hd-admin/pending_invites.rds
# =============================================================================
.INVITES_KEY <- "hd-admin/pending_invites.rds"

.schema_pending_invites <- function() {
  data.frame(
    token        = character(),
    email        = character(),
    display_name = character(),
    client_id    = character(),
    tier         = character(),
    invited_by   = character(),
    created_at   = character(),
    expires_at   = character(),
    accepted_at  = character(),
    status       = character(),  # "pending", "accepted", "expired", "revoked"
    stringsAsFactors = FALSE
  )
}

read_pending_invites <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = .INVITES_KEY, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw))
    return(.schema_pending_invites())
  for (col in names(.schema_pending_invites()))
    if (!col %in% names(raw)) raw[[col]] <- NA_character_
  raw
}

write_pending_invites <- function(df) {
  aws.s3::s3saveRDS(df, object = .INVITES_KEY, bucket = .s3_bucket())
  invisible(TRUE)
}

create_invite <- function(email, display_name, client_id, tier, invited_by,
                          expires_hours = 48) {
  token    <- paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
  now      <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  exp_time <- format(Sys.time() + expires_hours * 3600,
                     "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- read_pending_invites()
  new_row  <- data.frame(
    token        = token,
    email        = tolower(trimws(email)),
    display_name = display_name,
    client_id    = client_id,
    tier         = tier,
    invited_by   = invited_by,
    created_at   = now,
    expires_at   = exp_time,
    accepted_at  = NA_character_,
    status       = "pending",
    stringsAsFactors = FALSE
  )
  write_pending_invites(rbind(existing, new_row))
  token
}

resolve_invite <- function(token) {
  invites <- read_pending_invites()
  idx <- which(invites$token == token & invites$status == "pending")
  if (!length(idx)) return(NULL)
  row <- invites[idx[1], ]
  exp <- tryCatch(as.POSIXct(row$expires_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  error = function(e) NULL)
  if (!is.null(exp) && Sys.time() > exp) {
    invites[idx[1], "status"] <- "expired"
    write_pending_invites(invites)
    return(NULL)
  }
  row
}

accept_invite <- function(token) {
  invites <- read_pending_invites()
  idx <- which(invites$token == token)
  if (!length(idx)) return(invisible(FALSE))
  invites[idx[1], "status"]      <- "accepted"
  invites[idx[1], "accepted_at"] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  write_pending_invites(invites)
  invisible(TRUE)
}

# =============================================================================
# Stage 10 — Contacts Database
# Per-client external contacts stored at <client_id>/contacts.rds
# These are people who receive notifications but do NOT log in (e.g. a CFO).
# =============================================================================

.schema_contacts <- function() {
  data.frame(
    id                       = character(),
    name                     = character(),
    email                    = character(),
    role                     = character(),
    receives_limit_alerts    = logical(),
    receives_audit_summaries = logical(),
    active                   = logical(),
    created_at               = character(),
    stringsAsFactors         = FALSE
  )
}

read_contacts <- function(client_id = Sys.getenv("CLIENT_ID")) {
  key <- paste0(client_id, "/contacts.rds")
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw))
    return(.schema_contacts())
  schema <- .schema_contacts()
  for (col in names(schema)) {
    if (!col %in% names(raw)) {
      raw[[col]] <- if (is.logical(schema[[col]])) NA else NA_character_
    }
  }
  raw
}

write_contacts <- function(df, client_id = Sys.getenv("CLIENT_ID")) {
  key <- paste0(client_id, "/contacts.rds")
  aws.s3::s3saveRDS(df, object = key, bucket = .s3_bucket())
  invisible(TRUE)
}

add_contact <- function(client_id, name, email, role = "",
                        receives_limit_alerts    = FALSE,
                        receives_audit_summaries = FALSE) {
  existing <- read_contacts(client_id)
  new_row  <- data.frame(
    id                       = uuid::UUIDgenerate(),
    name                     = trimws(name),
    email                    = tolower(trimws(email)),
    role                     = trimws(role),
    receives_limit_alerts    = isTRUE(receives_limit_alerts),
    receives_audit_summaries = isTRUE(receives_audit_summaries),
    active                   = TRUE,
    created_at               = as.character(Sys.time()),
    stringsAsFactors         = FALSE
  )
  write_contacts(rbind(existing, new_row), client_id)
  invisible(new_row$id)
}

deactivate_contact <- function(client_id, contact_id) {
  df  <- read_contacts(client_id)
  idx <- which(df$id == contact_id & isTRUE(df$active))
  if (!length(idx)) return(invisible(FALSE))
  df$active[idx] <- FALSE
  write_contacts(df, client_id)
  invisible(TRUE)
}

# ── get_limit_alert_recipients() ─────────────────────────────────────────────
# Returns deduplicated email addresses for user-limit warning notifications:
#   1. client_registry.contact_email for the client
#   2. active contacts with receives_limit_alerts = TRUE
#   3. hd-admin staff with can_manage_invites = TRUE and a non-NA email

get_limit_alert_recipients <- function(client_id) {
  emails <- character(0)

  registry <- tryCatch(read_client_registry(), error = function(e) NULL)
  if (!is.null(registry)) {
    row <- registry[registry$client_id == client_id, , drop = FALSE]
    if (nrow(row) && nzchar(row$contact_email[1] %||% ""))
      emails <- c(emails, tolower(trimws(row$contact_email[1])))
  }

  contacts <- tryCatch(read_contacts(client_id), error = function(e) NULL)
  if (!is.null(contacts) && nrow(contacts)) {
    alert_contacts <- contacts[
      isTRUE(contacts$active) & isTRUE(contacts$receives_limit_alerts), , drop = FALSE
    ]
    if (nrow(alert_contacts))
      emails <- c(emails, tolower(trimws(alert_contacts$email)))
  }

  hd_staff <- tryCatch(auth_load_usuarios(client_id = "hd-admin"), error = function(e) NULL)
  if (!is.null(hd_staff) && nrow(hd_staff)) {
    active_staff <- hd_staff[
      (is.na(hd_staff$deleted) | hd_staff$deleted != TRUE) &
      isTRUE(hd_staff$can_manage_invites) &
      !is.na(hd_staff$email) & nzchar(hd_staff$email %||% ""),
      , drop = FALSE
    ]
    if (nrow(active_staff))
      emails <- c(emails, tolower(trimws(active_staff$email)))
  }

  unique(emails[nzchar(emails)])
}

# =============================================================================
# Stage 11 — Hop Permissions
# Staff access requests and time-limited grants. Both files are stored at
# hd-admin/ (admin deployment only) and are never per-client.
# =============================================================================

.HOP_REQUESTS_KEY <- "hd-admin/hop_requests.rds"
.HOP_GRANTS_KEY   <- "hd-admin/hop_grants.rds"

.schema_hop_requests <- function() {
  data.frame(
    id             = character(),
    requester      = character(),
    requester_name = character(),
    clients_json   = character(),    # JSON array: '["networks","hopdesk"]'
    decisions_json = character(),    # JSON object per-client: '{"networks":"approved","hopdesk":"pending"}'
    message        = character(),
    requested_at   = as.POSIXct(character()),
    status         = character(),    # "pending" | "resolved"
    stringsAsFactors = FALSE
  )
}

.schema_hop_grants <- function() {
  data.frame(
    id         = character(),
    request_id = character(),
    grantee    = character(),
    client_id  = character(),
    granted_by = character(),
    granted_at = as.POSIXct(character()),
    expires_at = as.POSIXct(character()),
    revoked    = logical(),
    stringsAsFactors = FALSE
  )
}

load_hop_requests <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = .HOP_REQUESTS_KEY, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw)) return(.schema_hop_requests())
  schema <- .schema_hop_requests()
  for (col in names(schema)) if (!col %in% names(raw)) raw[[col]] <- NA
  raw[, names(schema), drop = FALSE]
}

save_hop_requests <- function(df) {
  tryCatch(
    suppressMessages(
      aws.s3::s3saveRDS(df, object = .HOP_REQUESTS_KEY, bucket = .s3_bucket())
    ),
    error = function(e) warning("[HOP] save_hop_requests: ", e$message)
  )
  invisible(df)
}

load_hop_grants <- function() {
  raw <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = .HOP_GRANTS_KEY, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw)) return(.schema_hop_grants())
  schema <- .schema_hop_grants()
  for (col in names(schema)) if (!col %in% names(raw)) raw[[col]] <- NA
  raw[, names(schema), drop = FALSE]
}

save_hop_grants <- function(df) {
  tryCatch(
    suppressMessages(
      aws.s3::s3saveRDS(df, object = .HOP_GRANTS_KEY, bucket = .s3_bucket())
    ),
    error = function(e) warning("[HOP] save_hop_grants: ", e$message)
  )
  invisible(df)
}