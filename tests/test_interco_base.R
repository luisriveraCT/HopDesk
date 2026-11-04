# =============================================================================
# tests/test_interco_base.R
# CM12 (c), 2026-11-04: the IC registry (interco_v2) was saved from the
# session's copy: a stale copy undid codes another session had saved since,
# and the scanner / Business Partners set the session's registry BEFORE
# saving and only warned when the save failed. Now save_interco_v2(...,
# base) merges the change onto S3's current registry (strict read) with the
# pure interco_v2_rebase(), and the session shows only what was saved.
#
# Run from project root:  Rscript tests/test_interco_base.R
# =============================================================================
cat("=== test_interco_base ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")

source("tests/helpers/s3_memoria.R")
reg <- function(nts_ar = character(), nts_ap = character(), ncs_ar = NULL, rfcs = character(), ar_prefix = "C") {
  co <- list(NTS = list(ar = nts_ar, ap = nts_ap))
  if (!is.null(ncs_ar)) co$NCS <- list(ar = ncs_ar, ap = character())
  list(ar_prefix = ar_prefix, ap_prefix = "P", rfcs = rfcs, companies = co)
}

# ── 1. interco_v2_rebase(), pure ───────────────────────────────────────────────
base   <- reg(nts_ar = "1027")
fresco <- reg(nts_ar = c("1027", "2001"), ncs_ar = "5005")        # B added 2001 and NCS
nuevo  <- reg(nts_ar = c("1027", "3003"))                          # A added 3003
r <- interco_v2_rebase(nuevo, base, fresco)
ok("rebase: A's 3003 and B's 2001 both in NTS AR", setequal(r$companies$NTS$ar, c("1027", "2001", "3003")))
ok("rebase: B's company NCS kept", identical(r$companies$NCS$ar, "5005"))
r <- interco_v2_rebase(reg(), reg(nts_ar = "1027"), reg(nts_ar = c("1027", "2001")))
ok("rebase: A removed 1027 -> gone; B's 2001 stays", identical(r$companies$NTS$ar, "2001"))
r <- interco_v2_rebase(reg(ar_prefix = "CL"), reg(), reg(rfcs = c(C2001 = "NRS010101AA1")))
ok("rebase: A's prefix change wins, B's RFC kept",
   identical(r$ar_prefix, "CL") && identical(unname(r$rfcs["C2001"]), "NRS010101AA1"))
r <- interco_v2_rebase(reg(rfcs = c(C3003 = "X")), reg(), reg(rfcs = c(C2001 = "Y")))
ok("rebase: RFCs of both sessions kept", setequal(names(r$rfcs), c("C2001", "C3003")))

# ── 2. save_interco_v2(..., base) on S3 ───────────────────────────────────────
k <- .s3_key(S3_KEYS$interco_v2, client_id = "acme")
s3_memoria_reset()
assign(k, reg(nts_ar = c("1027", "2001")), envir = .mem)
.mem_falla <- k
ok("save with base, failed read: stops and writes nothing",
   falla(save_interco_v2(reg(nts_ar = c("1027", "3003")), client_id = "acme", base = reg(nts_ar = "1027"))) &&
   .mem_escrituras == 0L)
.mem_falla <- character()
w <- save_interco_v2(reg(nts_ar = c("1027", "3003")), client_id = "acme", base = reg(nts_ar = "1027"))
ok("save with base: S3 has both sessions' codes and the call returns it",
   setequal(get(k, envir = .mem)$companies$NTS$ar, c("1027", "2001", "3003")) &&
   setequal(w$companies$NTS$ar, c("1027", "2001", "3003")))
ok("save without base: unchanged behaviour (writes what it is given)",
   { save_interco_v2(reg(nts_ar = "9"), client_id = "acme"); identical(get(k, envir = .mem)$companies$NTS$ar, "9") })

# ── 3. The handlers pass base and show only what was saved ────────────────────
hub <- paste(readLines("R/settings/settings_hub.R", warn = FALSE), collapse = "\n")
pos <- gregexpr("save_interco_v2(", hub, fixed = TRUE)[[1]]
llamadas <- vapply(pos, function(p) substr(hub, p, p + 160L), character(1))   # each call spans <= 2 lines
ok(sprintf("settings_hub: all %d save_interco_v2() calls pass base", length(llamadas)),
   length(llamadas) == 3L && all(grepl("base = ", llamadas, fixed = TRUE)))
ok("settings_hub: no session registry set before its save (scanner, BP)",
   !grepl("shared$interco_v2(new_registry)\n    tryCatch", hub, fixed = TRUE) &&
   !grepl("shared$interco_v2(new_registry)\r\n    # Auto-persist", hub, fixed = TRUE) &&
   !grepl("shared$interco_v2(new_registry)\n    # Auto-persist", hub, fixed = TRUE))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
