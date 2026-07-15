# =============================================================================
# tests/test_policy_engine.R
# Run from project root:  source("tests/test_policy_engine.R")
# Requires no external packages — uses base stopifnot + all.equal.
# =============================================================================

library(lubridate)
library(dplyr)
source("R/policy_engine.R")

# ── Test helpers ───────────────────────────────────────────────────────────────
.pass  <- 0L
.fail  <- 0L

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected))
  if (ok) {
    cat(sprintf("  PASS  %s\n", label))
    .pass <<- .pass + 1L
  } else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

cat("── Easter ────────────────────────────────────────────────────────────────\n")
.chk(.easter_sunday(2025), as.Date("2025-04-20"), "Easter 2025")
.chk(.easter_sunday(2026), as.Date("2026-04-05"), "Easter 2026")
.chk(.easter_sunday(2027), as.Date("2027-03-28"), "Easter 2027")

cat("── .nth_weekday ──────────────────────────────────────────────────────────\n")
.chk(.nth_weekday(2025,  2L,  1L, 1L), as.Date("2025-02-03"), "1st Mon Feb 2025")
.chk(.nth_weekday(2025,  3L,  3L, 1L), as.Date("2025-03-17"), "3rd Mon Mar 2025")
.chk(.nth_weekday(2025, 11L,  3L, 1L), as.Date("2025-11-17"), "3rd Mon Nov 2025")
.chk(.nth_weekday(2025,  5L, -1L, 1L), as.Date("2025-05-26"), "Last Mon May 2025")
.chk(.nth_weekday(2025, 11L,  4L, 4L), as.Date("2025-11-27"), "4th Thu Nov 2025 (Thanksgiving)")

cat("── get_holidays ──────────────────────────────────────────────────────────\n")
mx25 <- get_holidays("MX", 2025)
.chk(as.Date("2025-01-01") %in% mx25, TRUE,  "MX 2025 Año Nuevo")
.chk(as.Date("2025-02-03") %in% mx25, TRUE,  "MX 2025 Constitución (1st Mon Feb)")
.chk(as.Date("2025-03-17") %in% mx25, TRUE,  "MX 2025 Juárez (3rd Mon Mar)")
.chk(as.Date("2025-04-17") %in% mx25, TRUE,  "MX 2025 Jueves Santo")
.chk(as.Date("2025-04-18") %in% mx25, TRUE,  "MX 2025 Viernes Santo")
.chk(as.Date("2025-05-01") %in% mx25, TRUE,  "MX 2025 Día del Trabajo")
.chk(as.Date("2025-09-16") %in% mx25, TRUE,  "MX 2025 Independencia")
.chk(as.Date("2025-11-17") %in% mx25, TRUE,  "MX 2025 Revolución (3rd Mon Nov)")
.chk(as.Date("2025-12-25") %in% mx25, TRUE,  "MX 2025 Navidad")
.chk(as.Date("2025-07-04") %in% mx25, FALSE, "MX 2025 Jul 4 not a holiday")

us25 <- get_holidays("US", 2025)
.chk(as.Date("2025-07-04") %in% us25, TRUE,  "US 2025 Independence Day (Fri)")
.chk(as.Date("2025-11-27") %in% us25, TRUE,  "US 2025 Thanksgiving")
.chk(as.Date("2025-12-25") %in% us25, TRUE,  "US 2025 Christmas (Thu)")
.chk(as.Date("2025-01-20") %in% us25, TRUE,  "US 2025 MLK Day (3rd Mon Jan)")

fr25 <- get_holidays("FR", 2025)
.chk(as.Date("2025-07-14") %in% fr25, TRUE,  "FR 2025 Fête Nationale")
.chk(as.Date("2025-04-21") %in% fr25, TRUE,  "FR 2025 Lundi de Pâques (Easter+1)")
.chk(as.Date("2025-05-29") %in% fr25, TRUE,  "FR 2025 Ascension (Easter+39)")

cat("── apply_offset_policy ───────────────────────────────────────────────────\n")
# Mar 14 = Fri; +5 = Mar 19 (Wed) → weekday, no roll
.chk(apply_offset_policy(as.Date("2025-03-14"), list(n =  5L)), as.Date("2025-03-19"), "offset +5 lands weekday")
# Mar 14 = Fri; -3 = Mar 11 (Tue) → weekday, no roll
.chk(apply_offset_policy(as.Date("2025-03-14"), list(n = -3L)), as.Date("2025-03-11"), "offset -3 lands weekday")
.chk(apply_offset_policy(as.Date("2025-03-14"), list(n =  0L)), as.Date("2025-03-14"), "offset 0")
.chk(apply_offset_policy(as.Date("2025-03-14"), list()),        as.Date("2025-03-14"), "offset missing n")
# Mar 16 = Sun; +3 = Mar 19 (Wed) → weekday forward, no roll needed
.chk(apply_offset_policy(as.Date("2025-03-16"), list(n =  3L), "forward"),  as.Date("2025-03-19"), "offset +3 Sun→Wed (already weekday)")
# Mar 19 = Wed; +3 = Mar 22 (Sat) → roll forward → Mar 24 (Mon)
.chk(apply_offset_policy(as.Date("2025-03-19"), list(n =  3L), "forward"),  as.Date("2025-03-24"), "offset +3 lands Sat → roll forward → Mon")
# Mar 19 = Wed; +3 = Mar 22 (Sat) → roll backward → Mar 21 (Fri)
.chk(apply_offset_policy(as.Date("2025-03-19"), list(n =  3L), "backward"), as.Date("2025-03-21"), "offset +3 lands Sat → roll backward → Fri")

cat("── apply_last_day_policy ─────────────────────────────────────────────────\n")
# Returns last CALENDAR day — no weekday rolling
# Feb 2025: last calendar day = Feb 28 (Fri)
.chk(apply_last_day_policy(as.Date("2025-02-15")), as.Date("2025-02-28"), "last day Feb 2025 (Fri)")
# May 2025: last calendar day = May 31 (Sat) — calendar boundary, no roll
.chk(apply_last_day_policy(as.Date("2025-05-01")), as.Date("2025-05-31"), "last day May 2025 (Sat)")
# Jun 2025: last calendar day = Jun 30 (Mon)
.chk(apply_last_day_policy(as.Date("2025-06-15")), as.Date("2025-06-30"), "last day Jun 2025 (Mon)")
# Aug 2025: last calendar day = Aug 31 (Sun) — calendar boundary, no roll
.chk(apply_last_day_policy(as.Date("2025-08-01")), as.Date("2025-08-31"), "last day Aug 2025 (Sun)")

cat("── apply_month_days_policy ───────────────────────────────────────────────\n")
# Mar 3 → target Mar 15 (Sat 2025) → roll forward → Mar 17 (Mon)
.chk(apply_month_days_policy(as.Date("2025-03-03"), list(days = c(15L, 30L))),
     as.Date("2025-03-17"), "month_days: Mar 3 → 15th (Sat) → Mon")
# Mar 20 → target Mar 30 (Sun 2025) → roll forward → Mar 31 (Mon)
.chk(apply_month_days_policy(as.Date("2025-03-20"), list(days = c(15L, 30L))),
     as.Date("2025-03-31"), "month_days: Mar 20 → 30th (Sun) → Mon")
# Mar 31 → all targets passed → Apr 15 (Tue 2025) → weekday, no roll
.chk(apply_month_days_policy(as.Date("2025-03-31"), list(days = c(15L, 30L))),
     as.Date("2025-04-15"), "month_days: past all targets → Apr 15 (Tue)")
# Mar 15 = Sat → exact match candidate, but Sat → roll forward → Mar 17 (Mon)
.chk(apply_month_days_policy(as.Date("2025-03-15"), list(days = c(15L, 30L))),
     as.Date("2025-03-17"), "month_days: exact match on Sat → roll → Mon")
# Feb 1, day=30 → clamped to Feb 28 (Fri 2025) → weekday, no roll
.chk(apply_month_days_policy(as.Date("2025-02-01"), list(days = c(30L))),
     as.Date("2025-02-28"), "month_days: day 30 clamped Feb (Fri)")

cat("── apply_weekday_policy ──────────────────────────────────────────────────\n")
# 2025-03-22 = Saturday → forward to Monday Mar 24
.chk(apply_weekday_policy(as.Date("2025-03-22"), list()),
     as.Date("2025-03-24"), "weekday: Sat forward")
# Saturday → backward to Friday Mar 21
.chk(apply_weekday_policy(as.Date("2025-03-22"), list(), "backward"),
     as.Date("2025-03-21"), "weekday: Sat backward")
# Friday already allowed
.chk(apply_weekday_policy(as.Date("2025-03-21"), list(days = c(5L))),
     as.Date("2025-03-21"), "weekday: Fri is allowed")
# Thursday not in {5} → roll forward to next Friday (Mar 28)
.chk(apply_weekday_policy(as.Date("2025-03-27"), list(days = c(5L))),
     as.Date("2025-03-28"), "weekday: Thu→Fri")

cat("── apply_skip_holidays_policy ────────────────────────────────────────────\n")
.chk(apply_skip_holidays_policy(as.Date("2025-05-01"), list(), "forward",  mx25),
     as.Date("2025-05-02"), "skip_holidays: Labor Day → May 2")
.chk(apply_skip_holidays_policy(as.Date("2025-05-01"), list(), "backward", mx25),
     as.Date("2025-04-30"), "skip_holidays: Labor Day backward → Apr 30")
.chk(apply_skip_holidays_policy(as.Date("2025-03-15"), list(), "forward",  mx25),
     as.Date("2025-03-15"), "skip_holidays: non-holiday unchanged")

cat("── compose_policies ──────────────────────────────────────────────────────\n")
hcache  <- list(MX = get_holidays("MX", 2024:2027))
p_wd    <- list(type = "weekdays",      params = list(),              roll_direction = "forward")
p_sk_mx <- list(type = "skip_holidays", params = list(country = "MX"), roll_direction = "forward")
p_off3  <- list(type = "offset_days",   params = list(n = 3L),        roll_direction = "forward")
p_last  <- list(type = "last_day",      params = list(),              roll_direction = "backward")
p_m1530 <- list(type = "month_days",    params = list(days = c(15L, 30L)), roll_direction = "forward")

# Weekday on Saturday → Monday
.chk(compose_policies(as.Date("2025-03-22"), list(p_wd), hcache),
     as.Date("2025-03-24"), "compose: Sat→Mon")

# Offset +3 from Wed Mar 19 = Sat Mar 22 → apply_offset internally rolls → Mon Mar 24
# → weekday filter: Mon is valid → Mar 24
.chk(compose_policies(as.Date("2025-03-19"), list(p_off3, p_wd), hcache),
     as.Date("2025-03-24"), "compose: offset+weekday")

# Labor Day (May 1 Thu): weekdays passes (Thu ∈ 1:5), skip_holidays skips → May 2 (Fri)
.chk(compose_policies(as.Date("2025-05-01"), list(p_wd, p_sk_mx), hcache),
     as.Date("2025-05-02"), "compose: Labor Day → May 2")

# Holy Thursday (Apr 17): weekdays passes, skip skips to Apr 18 (Good Fri),
# skip skips to Apr 19 (Sat), weekdays rolls to Apr 21 (Mon), stable
.chk(compose_policies(as.Date("2025-04-17"), list(p_wd, p_sk_mx), hcache),
     as.Date("2025-04-21"), "compose: Holy Thursday → Apr 21")

# last_day on Mar 3: last calendar day of March = Mar 31 (Mon, already a weekday)
.chk(compose_policies(as.Date("2025-03-03"), list(p_last), hcache),
     as.Date("2025-03-31"), "compose: last day Mar 2025")

# month_days on Mar 20: → Mar 30 (Sun) → weekday filter → Mar 31 (Mon)
.chk(compose_policies(as.Date("2025-03-20"), list(p_m1530, p_wd), hcache),
     as.Date("2025-03-31"), "compose: month_days 15/30 + weekday")

# Empty policies → date unchanged
.chk(compose_policies(as.Date("2025-05-01"), list(), hcache),
     as.Date("2025-05-01"), "compose: empty policies")

# NA date → NA
.chk(is.na(compose_policies(as.Date(NA), list(p_wd), hcache)), TRUE, "compose: NA date")

cat("── compute_policy_moves ──────────────────────────────────────────────────\n")

test_invoices <- tibble::tibble(
  Empresa            = c("NTS",         "NTS",         "NG"),
  Moneda             = c("MXN",         "MXN",         "USD"),
  Documento          = c("FAC001",      "FAC002",      "FAC003"),
  Parte              = c("Proveedor A", "Proveedor B", "Proveedor A"),
  FechaVenc_Original = as.Date(c("2025-05-01", "2025-05-31", "2025-03-22"))
)
test_catalog <- tibble::tibble(
  id             = c("pol-wd",    "pol-last"),
  name           = c("Hábiles",   "Fin mes"),
  type           = c("weekdays",  "last_day"),
  params         = list(list(),   list()),
  roll_direction = c("forward",   "backward"),
  created_by     = "test", created_at = Sys.time(), updated_at = Sys.time()
)
test_pp <- tibble::tibble(
  parte        = c("Proveedor A",  "Proveedor B"),
  policy_id    = c("pol-wd",       "pol-last"),
  policy_order = c(1L,             1L),
  ledger       = c("AP",           "AP"),
  is_interco   = c(FALSE,          FALSE),
  linked_by    = "test",
  linked_at    = Sys.time()
)

res <- compute_policy_moves(test_invoices, test_pp, test_catalog, ledger = "AP")

.chk(nrow(res), 3L, "compute: 3 rows returned")
fac1 <- res[res$Documento == "FAC001", ]
# May 1 (Thu) + weekdays, no holidays → Thu is weekday → May 1 unchanged
.chk(fac1$FechaVenc_Politica, as.Date("2025-05-01"), "compute: FAC001 (weekday, Thu)")
fac2 <- res[res$Documento == "FAC002", ]
# May 31 + last_day → last calendar day of May = May 31
.chk(fac2$FechaVenc_Politica, as.Date("2025-05-31"), "compute: FAC002 (last day May)")
fac3 <- res[res$Documento == "FAC003", ]
# Mar 22 (Sat) + weekdays → Mar 24 (Mon)
.chk(fac3$FechaVenc_Politica, as.Date("2025-03-24"), "compute: FAC003 (Sat→Mon)")

# Unmatched partner → no row
test_no_match <- tibble::tibble(
  Empresa = "NTS", Moneda = "MXN", Documento = "FAC999",
  Parte = "Unknown Vendor", FechaVenc_Original = as.Date("2025-06-01")
)
res_empty <- compute_policy_moves(test_no_match, test_pp, test_catalog, ledger = "AP")
.chk(nrow(res_empty), 0L, "compute: unmatched partner → 0 rows")

# ── Summary ────────────────────────────────────────────────────────────────────
cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop(sprintf("%d test(s) FAILED", .fail))
invisible(TRUE)
