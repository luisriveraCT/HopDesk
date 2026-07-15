# =============================================================================
# R/proveedores_match.R
# Fuzzy supplier matching engine.
# Used by: proveedores_module.R (upload dedup), pagar_hoy_module.R (suggestions),
#          agenda de hoy hover lookup.
# Pure functions — no reactives, no S3, no side effects.
# =============================================================================

# Score a single (query, candidate) pair. Returns 0-100.
# query: named list or 1-row df with fields: parte, rfc, no_cuenta, nombre, alias
# candidate: 1-row df from proveedores schema
score_proveedor_match <- function(query, candidate) {
  score <- 0L

  # 1. No. Cuenta exact match — strongest signal (+50)
  q_cuenta <- trimws(query$no_cuenta %||% "")
  c_cuenta <- trimws(candidate$no_cuenta %||% "")
  if (nzchar(q_cuenta) && nzchar(c_cuenta) && q_cuenta == c_cuenta)
    return(100L)  # exact account match = definitive, skip other scoring

  # 2. RFC exact match (+40)
  q_rfc <- toupper(trimws(query$rfc %||% ""))
  c_rfc <- toupper(trimws(candidate$rfc %||% ""))
  if (nzchar(q_rfc) && nzchar(c_rfc) && q_rfc == c_rfc)
    score <- score + 40L

  # 3. Name matching — compare query$parte against nombre, alias (+30 max)
  # Strip accents so "García" matches "garcia", etc.
  q_name   <- strip_accents(tolower(trimws(query$parte %||% query$nombre %||% "")))
  c_nombre <- strip_accents(tolower(trimws(candidate$nombre %||% "")))
  c_alias  <- strip_accents(tolower(trimws(candidate$alias  %||% "")))
  if (nzchar(q_name)) {
    # Exact name match
    if (q_name == c_nombre || q_name == c_alias)
      score <- score + 30L
    # Substring match either direction
    else if (grepl(q_name, c_nombre, fixed = TRUE) ||
             grepl(c_nombre, q_name, fixed = TRUE))
      score <- score + 20L
    # Token overlap: split on spaces, check shared words >=4 chars
    else {
      q_tokens <- unique(strsplit(q_name, "\\s+")[[1]])
      q_tokens <- q_tokens[nchar(q_tokens) >= 4]
      c_tokens <- unique(c(strsplit(c_nombre, "\\s+")[[1]],
                            strsplit(c_alias,  "\\s+")[[1]]))
      c_tokens <- c_tokens[nchar(c_tokens) >= 4]
      overlap  <- sum(q_tokens %in% c_tokens)
      if (overlap >= 2) score <- score + 15L
      else if (overlap == 1) score <- score + 8L
    }
  }

  # 4. Numeric token overlap — CLABE / account fragments (+10)
  q_nums <- regmatches(q_name, gregexpr("[0-9]{6,}", q_name))[[1]]
  c_nums <- regmatches(paste(c_nombre, c_alias, c_cuenta),
                       gregexpr("[0-9]{6,}", paste(c_nombre, c_alias, c_cuenta)))[[1]]
  if (length(q_nums) && length(c_nums) && any(q_nums %in% c_nums))
    score <- score + 10L

  min(score, 100L)
}

# Find top matches for a query against a proveedores data frame.
# Returns candidates with score >= threshold, sorted descending.
# Only searches active suppliers (activo == TRUE, activo_hasta not expired).
find_proveedor_matches <- function(query, proveedores_df, threshold = 30L, top_n = 8L) {
  if (is.null(proveedores_df) || !nrow(proveedores_df)) return(proveedores_df[0, ])

  # Filter to truly active: activo == TRUE AND activo_hasta not expired
  today <- as.character(Sys.Date())
  active <- proveedores_df[
    !is.na(proveedores_df$activo) & proveedores_df$activo == TRUE &
    (is.na(proveedores_df$activo_hasta) |
     proveedores_df$activo_hasta == "indefinido" |
     (!is.na(proveedores_df$activo_hasta) & proveedores_df$activo_hasta >= today)),
  ]
  if (!nrow(active)) return(active)

  scores <- vapply(seq_len(nrow(active)), function(i) {
    tryCatch(score_proveedor_match(query, active[i, ]), error = function(e) 0L)
  }, integer(1))

  active$.score <- scores
  result <- active[scores >= threshold, ]
  result <- result[order(-result$.score), ]
  head(result, top_n)
}

# Check if a supplier's active window has expired. Returns TRUE if expired.
proveedor_expirado <- function(activo_hasta) {
  if (is.na(activo_hasta) || activo_hasta == "indefinido" || !nzchar(activo_hasta))
    return(FALSE)
  tryCatch(as.Date(activo_hasta) < Sys.Date(), error = function(e) FALSE)
}

# Parse activo_hasta from a duration choice string.
# duration: "48h" | "1s" | "1m" | "1a" | "indefinido"
parse_activo_hasta <- function(duration) {
  switch(duration,
    "48h"        = as.character(Sys.Date() + 2L),
    "1s"         = as.character(Sys.Date() + 7L),
    "1m"         = as.character(Sys.Date() + 30L),
    "1a"         = as.character(Sys.Date() + 365L),
    "indefinido" = "indefinido",
    "indefinido"
  )
}
