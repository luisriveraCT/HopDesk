# =============================================================================
# scripts/add_bunny.R
#
# Adds Ana Fernanda Nemer (Bunny) to hd-admin/usuarios.rds.
# Run from app root AFTER seed_hd_admin.R has created Mouse.
#
# Usage: source("scripts/add_bunny.R")
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

bunny_pw <- readline("Enter password for bunny (Ana Fernanda): ")
if (!nzchar(bunny_pw)) stop("Password cannot be empty.")

bunny <- tibble(
  id                       = uuid::UUIDgenerate(),
  account_code             = "U0002",
  username                 = "bunny",
  password_hash            = bunny_pw,
  display_name             = "Ana Fernanda Nemer",
  tier                     = "hopdesk",
  client_id                = "hd-admin",
  permisos                 = "{}",
  group_ids                = "[]",
  email                    = NA_character_,
  requires_password_change = FALSE,
  activo                   = TRUE,
  created_at               = as.character(Sys.time()),
  last_login               = NA_character_,
  deleted                  = FALSE,
  deleted_at               = NA_character_
)

existing <- s3_read_as("hd-admin", S3_KEYS$usuarios)
if (!is.null(existing) && nrow(existing) && any(tolower(existing$username) == "bunny")) {
  stop("bunny already exists in hd-admin/usuarios.rds — aborting.")
}

updated <- if (is.null(existing) || !nrow(existing)) bunny else dplyr::bind_rows(existing, bunny)
s3_write_as("hd-admin", S3_KEYS$usuarios, updated)

message("[OK] bunny (Ana Fernanda Nemer, hopdesk) added to hd-admin/usuarios.rds")
