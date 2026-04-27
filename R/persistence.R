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
#   CLIENT_ID=acme                        ← unique slug for this deployment
#
#   ACME_S3_BUCKET=acme-antiguedad-prod   ← client-specific bucket
#   ACME_AWS_ACCESS_KEY_ID=AKIAxxxxxxx    ← IAM key scoped to that bucket only
#   ACME_AWS_SECRET_ACCESS_KEY=xxxxxxx
#   ACME_AWS_DEFAULT_REGION=us-east-1
#
# All S3 objects are stored under a "<client_id>/" prefix inside the bucket,
# so a single shared bucket (with per-client IAM key prefix policies) also works.
#
# Adding a new client = new .Renviron with their CLIENT_ID + four AWS vars.
# Zero code changes required.
# ─────────────────────────────────────────────────────────────────────────────

# Returns the CLIENT_ID slug (upper-cased for env var lookup)
.client_id <- function() {
  id <- Sys.getenv("CLIENT_ID")
  if (!nzchar(id))
    stop("CLIENT_ID environment variable is not set. ",
         "Set it in .Renviron (e.g. CLIENT_ID=acme).", call. = FALSE)
  toupper(trimws(id))
}

# Returns the S3 bucket for the current client
.s3_bucket <- function() {
  cid <- .client_id()
  b   <- Sys.getenv(paste0(cid, "_S3_BUCKET"))
  if (!nzchar(b))
    stop(cid, "_S3_BUCKET environment variable is not set.", call. = FALSE)
  b
}

# Prefixes every S3 key with "<client_id>/" for logical isolation within the bucket.
# e.g. "invoice_moves.rds" → "acme/invoice_moves.rds"
.s3_key <- function(key) {
  paste0(tolower(.client_id()), "/", key)
}

# Applies client-specific AWS credentials to the session once.
# Called at app startup (from global.R) — not on every read/write.
s3_init <- function() {
  cid <- .client_id()
  key_id  <- Sys.getenv(paste0(cid, "_AWS_ACCESS_KEY_ID"))
  secret  <- Sys.getenv(paste0(cid, "_AWS_SECRET_ACCESS_KEY"))
  region  <- Sys.getenv(paste0(cid, "_AWS_DEFAULT_REGION"))

  missing <- c(
    if (!nzchar(key_id)) paste0(cid, "_AWS_ACCESS_KEY_ID"),
    if (!nzchar(secret))  paste0(cid, "_AWS_SECRET_ACCESS_KEY"),
    if (!nzchar(region))  paste0(cid, "_AWS_DEFAULT_REGION")
  )
  if (length(missing))
    warning("Missing AWS credentials: ", paste(missing, collapse = ", "),
            "\nS3 persistence will not work.", call. = FALSE)

  Sys.setenv(
    AWS_ACCESS_KEY_ID     = key_id,
    AWS_SECRET_ACCESS_KEY = secret,
    AWS_DEFAULT_REGION    = region
  )
  invisible(TRUE)
}

# Cache of known-missing S3 keys — avoids repeated round-trips within a session.
.s3_missing_cache <- new.env(parent = emptyenv())

# Pre-load cache — populated in parallel at startup (global.R).
# Keys are file suffixes (e.g. "invoice_moves.rds"); values are the raw objects.
# A binding with value NULL means the key was confirmed absent from S3.
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

.s3_write <- function(obj, key) {
  message(sprintf("[S3_WRITE] ── start key='%s'", key))

  if (!is.character(key) || !nzchar(key))
    stop("[S3_WRITE] key is NULL or empty — check that S3_KEYS contains this entry")
  full_key <- tryCatch(.s3_key(key),  error = function(e) stop("s3_key failed: ", e$message))
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
  if (is.character(key) && length(key) == 1L && nzchar(key) &&
      exists(key, envir = .s3_preload_cache, inherits = FALSE))
    rm(list = key, envir = .s3_preload_cache)

  message(sprintf("[S3_WRITE] ── complete key='%s'", key))
  invisible(TRUE)
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
  ledger  = character(),
  year    = integer(),
  month   = integer(),
  id      = character(),
  title   = character(),
  body    = character(),
  author  = character(),
  created = as.POSIXct(character()),
  updated = as.POSIXct(character())
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
  updated_at    = as.POSIXct(character())
)

# ── Normalizers ────────────────────────────────────────────────────────────────
# Coerce loaded data to the correct schema; adds missing columns, drops nothing.

.normalize <- function(df, schema_fn) {
  if (is.null(df) || !is.data.frame(df)) return(schema_fn())
  schema <- schema_fn()
  for (col in names(schema)) {
    if (!col %in% names(df)) {
      df[[col]] <- schema[[col]][NA_integer_]
    } else {
      # coerce to expected type quietly
      df[[col]] <- tryCatch(
        methods::as(df[[col]], class(schema[[col]])),
        error = function(e) schema[[col]][NA_integer_]
      )
    }
  }
  dplyr::select(df, dplyr::any_of(names(schema)))
}

# ── Invoice moves ──────────────────────────────────────────────────────────────

load_moves <- function() {
  .normalize(.s3_read(S3_KEYS$moves), .schema_moves) |>
    filter(ledger %in% c("AR","AP"), !is.na(Empresa),
           !is.na(Moneda), !is.na(Documento))
}

save_moves <- function(df) {
  norm <- .normalize(df, .schema_moves)
  .s3_write(norm, S3_KEYS$moves)
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

load_notes <- function() {
  .normalize(.s3_read(S3_KEYS$notes), .schema_notes)
}

save_notes <- function(df) {
  norm <- .normalize(df, .schema_notes)
  .s3_write(norm, S3_KEYS$notes)
  if (exists(".cache_set", mode = "function")) .cache_set("notes", norm)
}

# ── Invoice tags ───────────────────────────────────────────────────────────────

load_tags <- function() {
  .normalize(.s3_read(S3_KEYS$tags), .schema_tags) |>
    filter(tag %in% c("important","urgent"))
}

save_tags <- function(df) {
  norm <- .normalize(df, .schema_tags)
  .s3_write(norm, S3_KEYS$tags)
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

load_manual <- function() {
  .normalize(.s3_read(S3_KEYS$manual), .schema_manual)
}

save_manual <- function(df) {
  norm <- .normalize(df, .schema_manual)
  .s3_write(norm, S3_KEYS$manual)
  if (exists(".cache_set", mode = "function")) .cache_set("manual", norm)
}

upsert_manual <- function(df, new_row) {
  df <- filter(df, id != new_row$id)
  bind_rows(df, new_row)
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

load_papelera <- function() {
  .normalize(.s3_read(S3_KEYS$papelera), .schema_papelera)
}

save_papelera <- function(df) {
  norm <- .normalize(df, .schema_papelera)
  .s3_write(norm, S3_KEYS$papelera)
  if (exists(".cache_set", mode = "function")) .cache_set("papelera", norm)
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
    ledger       = ledger,
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
load_interco <- function() {
  out <- .s3_read(S3_KEYS$interco)
  if (!is.list(out)) out <- list(ar_clients = character(), ap_suppliers = character())
  out$ar_clients   <- normalize_code(out$ar_clients   %||% character())
  out$ap_suppliers <- normalize_code(out$ap_suppliers %||% character())
  out
}

save_interco <- function(lst) {
  lst$ar_clients   <- normalize_code(lst$ar_clients   %||% character())
  lst$ap_suppliers <- normalize_code(lst$ap_suppliers %||% character())
  .s3_write(lst, S3_KEYS$interco)
  if (exists(".cache_set", mode = "function")) .cache_set("interco", lst)
}

# ── Intercompany v2 — per-empresa, per-ledger code registry ───────────────────
# Schema: list(ar_prefix, ap_prefix, companies = list(INIT = list(ar, ap)))
# ar/ap are character vectors of numeric code bases (no prefix stored).
# Prefix is prepended at match time, making it configurable per SAP installation.

.empty_interco_v2 <- function() {
  list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = list())
}

load_interco_v2 <- function() {
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
  if (!is.list(out) || is.null(out$companies)) return(.empty_interco_v2())
  out$ar_prefix <- out$ar_prefix %||% "C"
  out$ap_prefix <- out$ap_prefix %||% "P"
  # rfcs: named character vector full_code → RFC (e.g. c("C1027" = "NCS060103RF4"))
  # Absent in registries written before this field existed → default to empty.
  if (is.null(out$rfcs)) out$rfcs <- character()
  out
}

save_interco_v2 <- function(registry) {
  .s3_write(registry, S3_KEYS$interco_v2)
  # Populate both in-memory caches so subsequent reads skip S3 entirely
  if (exists(".cache_set", mode = "function")) .cache_set("interco_v2", registry)
  assign(S3_KEYS$interco_v2, registry, envir = .s3_preload_cache)
  message(sprintf("[IC] save_interco_v2 — S3 write OK (%d companies)",
                  length(registry$companies %||% list())))
  invisible(registry)
}
# ── Pagar Hoy — staged payment queue ──────────────────────────────────────────

.schema_pagar_hoy <- function() tibble(
  id         = character(),
  ledger     = character(),   # always "AP"
  Empresa    = character(),
  Moneda     = character(),
  Documento  = character(),
  Parte      = character(),
  Codigo     = character(),
  tipo_item  = character(),   # "factura" (default) | "abono"
  Importe    = numeric(),
  FechaVenc  = as.Date(character()),
  staged_by  = character(),
  staged_at  = as.POSIXct(character()),
  status     = character()    # "pending" | "confirmed" | "cancelled"
)

load_pagar_hoy <- function() {
  df <- .normalize(.s3_read(S3_KEYS$pagar_hoy), .schema_pagar_hoy)
  df$status[is.na(df$status) | !nzchar(trimws(df$status))] <- "pending"
  dplyr::filter(df, status %in% c("pending", "confirmed", "cancelled"))
}

save_pagar_hoy <- function(df) {
  norm <- .normalize(df, .schema_pagar_hoy)
  .s3_write(norm, S3_KEYS$pagar_hoy)
  if (exists(".cache_set", mode = "function")) .cache_set("pagar_hoy", norm)
}

# Add or update rows.
# keys defaults to business key (Empresa+Moneda+Documento+ledger) for SAP rows.
# Pass keys = "id" for manual entries to avoid replacing entries that share a
# Documento but are different invoices (e.g. two manual entries for the same vendor).
upsert_pagar_hoy <- function(db, new_rows,
                             keys = c("ledger", "Empresa", "Moneda", "Documento")) {
  if (!nrow(new_rows)) return(db)
  db <- dplyr::anti_join(db, new_rows, by = keys)
  dplyr::bind_rows(db, new_rows)
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

save_sap_snapshot <- function(df, ledger) {
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
      .s3_write(prev1_obj, prev2_key)
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
      .s3_write(cur_obj, prev1_key)
      assign(prev1_key, cur_obj, envir = .s3_preload_cache)
      message(sprintf("[SNAP] %s cur→prev1 rotated (saved_at=%s)",
                      ledger, format(cur_obj$saved_at, "%d/%m %H:%M")))
    }
  }, error = function(e)
    message("[SNAP] ", ledger, " rotate cur→prev1 FAILED: ", e$message))

  # ── Step 3: write new snapshot as current ───────────────────────────────────
  obj <- list(data = df, saved_at = Sys.time())
  tryCatch(.s3_write(obj, cur_key), error = function(e)
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

load_conciliacion <- function() {
  .normalize(.s3_read(S3_KEYS$conciliacion), .schema_conciliacion)
}

save_conciliacion <- function(df) {
  .s3_write(.normalize(df, .schema_conciliacion), S3_KEYS$conciliacion)
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

load_bancos <- function() {
  .normalize(.s3_read(S3_KEYS$bancos), .schema_bancos)
}

save_bancos <- function(df) {
  .s3_write(.normalize(df, .schema_bancos), S3_KEYS$bancos)
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

load_proveedores <- function() {
  raw <- .s3_read(S3_KEYS$proveedores)
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

save_proveedores <- function(df) {
  .s3_write(.normalize(df, .schema_proveedores), S3_KEYS$proveedores)
}

load_proveedores_inactivos <- function() {
  raw <- .s3_read(S3_KEYS$proveedores_inactivos)
  .normalize(raw, .schema_proveedores)
}

save_proveedores_inactivos <- function(df) {
  .s3_write(.normalize(df, .schema_proveedores), S3_KEYS$proveedores_inactivos)
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

load_parte_alias_map <- function() {
  obj <- tryCatch(.s3_read(S3_KEYS$parte_alias_map), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_parte_alias_map())
  schema <- .schema_parte_alias_map()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  tibble::as_tibble(obj)
}

save_parte_alias_map <- function(df) {
  .s3_write(df, S3_KEYS$parte_alias_map)
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
  updated_at   = character()
)

# Load empresas from S3. On first run, seeds from the COMPANY_MAP constant
# so the transition is seamless — existing data is unaffected.
load_empresas <- function() {
  raw <- tryCatch(suppressMessages(.s3_read(S3_KEYS$empresas)), error = function(e) NULL)

  # First run — seed from COMPANY_MAP
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    message("[EMP] No empresas.rds found. Seeding from COMPANY_MAP.")
    seeded <- dplyr::bind_rows(lapply(seq_along(COMPANY_MAP), function(i) {
      data.frame(
        id           = uuid::UUIDgenerate(),
        account_code = sprintf("E%04d", i),
        initials     = names(COMPANY_MAP)[i],
        razon_social = unname(COMPANY_MAP[i]),
        nombre_corto = unname(COMPANY_MAP[i]),
        nombres_alt  = "[]",
        rfc          = "",
        activa       = TRUE,
        deleted      = FALSE,
        deleted_at   = NA_character_,
        created_at   = as.character(Sys.time()),
        updated_at   = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
    }))
    tryCatch(
      .s3_write(seeded, S3_KEYS$empresas),
      error = function(e) message("[EMP] seed save failed: ", e$message)
    )
    return(seeded)
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
      .s3_write(df, S3_KEYS$empresas),
      error = function(e) message("[EMP] account_code backfill failed: ", e$message)
    )
  }

  df
}

# Throws on failure — callers must handle errors and notify the user.
save_empresas <- function(df) {
  .s3_write(.normalize(df, .schema_empresas), S3_KEYS$empresas)
  invisible(TRUE)
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
rename_empresa_initials <- function(old_ini, new_ini) {
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
    df <- load_ctas_cuentas()
    if (nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      save_ctas_cuentas(df)
      touched <- c(touched, "ctas_cuentas")
    }
  }, error = function(e) message("[EMP rename] ctas_cuentas: ", e$message))

  # 2. bancos_cuentas — empresa column (lowercase; bancos_persistence.R schema)
  tryCatch({
    df <- load_bancos_cuentas()
    if (nrow(df) && any(!is.na(df$empresa) & df$empresa == old_ini)) {
      df <- .rni_col(df, "empresa")
      save_bancos_cuentas(df)
      touched <- c(touched, "bancos_cuentas")
    }
  }, error = function(e) message("[EMP rename] bancos_cuentas: ", e$message))

  # 3. proveedores — Empresa column
  tryCatch({
    df <- load_proveedores()
    if (nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      save_proveedores(df)
      touched <- c(touched, "proveedores")
    }
  }, error = function(e) message("[EMP rename] proveedores: ", e$message))

  # 4. parte_alias_map — Empresa column
  tryCatch({
    raw <- tryCatch(.s3_read(S3_KEYS$parte_alias_map), error = function(e) NULL)
    df  <- if (is.data.frame(raw)) .normalize(raw, .schema_parte_alias_map) else NULL
    if (!is.null(df) && nrow(df) && any(!is.na(df$Empresa) & df$Empresa == old_ini)) {
      df <- .rni_col(df, "Empresa")
      .s3_write(.normalize(df, .schema_parte_alias_map), S3_KEYS$parte_alias_map)
      touched <- c(touched, "parte_alias_map")
    }
  }, error = function(e) message("[EMP rename] parte_alias_map: ", e$message))

  # 5. interco_v2 — companies is a named list keyed by initials
  tryCatch({
    reg <- load_interco_v2()
    if (!is.null(reg$companies) && old_ini %in% names(reg$companies)) {
      reg$companies[[new_ini]] <- reg$companies[[old_ini]]
      reg$companies[[old_ini]] <- NULL
      save_interco_v2(reg)
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

load_abonos <- function() {
  .normalize(.s3_read(S3_KEYS$abonos), .schema_abonos) |>
    dplyr::filter(status %in% c("active", "voided"))
}

save_abonos <- function(df) {
  .s3_write(.normalize(df, .schema_abonos), S3_KEYS$abonos)
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