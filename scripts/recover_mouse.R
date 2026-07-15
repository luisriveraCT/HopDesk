# =============================================================================
# scripts/recover_mouse.R
#
# Emergency recovery: restores or resets mouse's account in hd-admin/usuarios.rds.
# Run from app root when mouse cannot log in but S3 is accessible.
#
# Usage: source("scripts/recover_mouse.R")
# =============================================================================

library(tibble)
library(uuid)

source("R/persistence.R")
S3_KEYS <- list(usuarios = "usuarios.rds")

readRenviron(".Renviron")
s3_init()

s3_read_as <- function(client, key) {
  orig <- Sys.getenv("CLIENT_ID")
  Sys.setenv(CLIENT_ID = client)
  on.exit(Sys.setenv(CLIENT_ID = orig), add = TRUE)
  suppressMessages(.s3_read(key))
}

s3_write_as <- function(client, key, obj) {
  orig <- Sys.getenv("CLIENT_ID")
  Sys.setenv(CLIENT_ID = client)
  on.exit(Sys.setenv(CLIENT_ID = orig), add = TRUE)
  .s3_write(obj, key)
}

new_pw <- readline("Enter new password for mouse: ")
if (!nzchar(new_pw)) stop("Password cannot be empty.")

message("[RECOVER] Reading hd-admin/usuarios.rds ...")
raw <- s3_read_as("hd-admin", S3_KEYS$usuarios)
if (is.null(raw)) raw <- tibble()

mouse_rows <- if (!is.null(raw$username)) which(tolower(raw$username) == "mouse") else integer(0)

if (length(mouse_rows)) {
  raw$password_hash[mouse_rows]  <- new_pw
  raw$activo[mouse_rows]         <- TRUE
  if ("deleted" %in% names(raw)) raw$deleted[mouse_rows] <- FALSE
  message("[RECOVER] Updated password for existing mouse account.")
} else {
  new_user <- tibble(
    id                       = uuid::UUIDgenerate(),
    account_code             = "U0001",
    username                 = "mouse",
    password_hash            = new_pw,
    display_name             = "Luis Rivera",
    tier                     = "principal",
    client_id                = "hd-admin",
    permisos                 = "{}",
    group_ids                = "[]",
    email                    = "suscripciones@networkslogistics.com.mx",
    requires_password_change = FALSE,
    activo                   = TRUE,
    created_at               = as.character(Sys.time()),
    last_login               = NA_character_,
    deleted                  = FALSE,
    deleted_at               = NA_character_
  )
  raw <- if (nrow(raw)) dplyr::bind_rows(raw, new_user) else new_user
  message("[RECOVER] mouse not found — added fresh record.")
}

s3_write_as("hd-admin", S3_KEYS$usuarios, raw)
message("\n[RECOVER] Done. Mouse account is active in hd-admin/usuarios.rds.")
