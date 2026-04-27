# Antigüedad App — Performance Knowledge Base

> **Purpose:** Protect startup and render performance across every future modification.
> Any change that touches the files listed here must be reviewed against these rules before shipping.
> Current baseline (post-auth, Phase 1/2 split): auth screen ~instant · first calendar ~3-5s after login · SAP live data ~30-90s · aux data overlays after SAP.

---

## Architecture Overview

```
Process start (global.R)
  └─ Preload 2 S3 keys (snapshots only) → .s3_preload_cache
  └─ Source all module files

Session #1 — shinymanager auth gate (SKIPS ALL DATA LOADING)
  └─ secure_server() validates credentials
  └─ Login form renders immediately — event loop stays free

Session #2 — real app (after login)
  └─ Phase 1 observer (p=0, fires once, ~1s)
  │    └─ load_sap_snapshot(AR) + load_sap_snapshot(AP)  ← cache hits, ~0s each
  │    └─ Seed sap_data() → CUR_CHOICES (p=1) + calendars (p=0) render on snapshot data
  │    └─ .s3_load_complete(TRUE) + .phase1_done(TRUE)
  │
  └─ SAP dispatch observer (p=-1)
  │    └─ req(.s3_load_complete()) → fires ~1s after Phase 1
  │    └─ load_sap_data() — live SAP fetch [30-90s, blocking, per-page progress logged]
  │    └─ sap_data() updated → calendars re-render with live data
  │
  └─ Phase 2 observer (p=-2, fires after Phase 1, runs after SAP dispatch)
       └─ Batch-fetch 18 deferred S3 keys → .s3_preload_cache  [~25s on Windows]
       └─ All 14 load_*() calls are cache hits
       └─ .cache_set() population
       └─ moves_db/notes_df/etc. set → calendar overlays update

Cross-session cache (.GlobalEnv$.sap_global_cache)
  └─ Written by session #2 after first live SAP fetch
  └─ Read by session #3+ (page refresh) — skips live fetch if < 5 min old
```

---

## Stage 1 — Startup (~72s on Windows)

**What happens:** Shiny sources `global.R`, then fetches 2 S3 objects (SAP snapshots) sequentially before accepting any browser connection.

**Floor:** 2 S3 reads × ~35s each on this network = ~70s. This is a network/hardware floor, not a code problem.

### Rules

**Never add keys to `S3_KEYS_CRITICAL`.** This list must stay at exactly 2 keys:
```r
S3_KEYS_CRITICAL <- list(
  sap_snap_ar = "sap_snapshot_AR.rds",
  sap_snap_ap = "sap_snapshot_AP.rds"
)
```
Any new S3 key goes into `S3_KEYS` (deferred), never `S3_KEYS_CRITICAL`.

**Never add top-level executable code to `global.R`** outside of function definitions, `source()` calls, constant assignments, and the guarded preload block. Top-level code runs before the app accepts connections.

**Never call `source()` on a file that itself does top-level S3 reads or expensive computation.** Module files must only define functions.

**The `source()` order in `global.R` is safe.** Do not add blocking calls between the source block and the preload block.

**The preload guard** (`if (!exists(".s3_preload_done"))`) prevents double-execution if `global.R` is sourced more than once. Never remove it.

---

## Stage 2 — First Browser Connect (~25s)

**What happens:** `.data_loaded` observer fires once. It first batch-fetches all 18 deferred S3 keys, then calls `load_*()` functions which are now all cache hits. Then seeds `sap_data()` from snapshots. `sap_trigger` fires 2s later and attempts live SAP fetches (all timeout at ~800ms connect timeout for unreachable hosts).

**Floor:** 18 S3 reads × ~1.4s each = ~25s sequential. SAP timeouts add ~10s (5 companies × 800ms connect timeout × 2 ledgers, partially overlapping).

### Rules

**Never call `load_*()` functions in `.data_loaded` before the batch-fetch block.** The batch-fetch must be the first thing in the observer's `isolate({...})` block. If `load_*()` runs before the cache is populated, it triggers a live S3 read.

**Never add new `load_*()` calls to `.data_loaded` without also verifying the corresponding key exists in `S3_KEYS`.** Unlisted keys are never preloaded and will always be live reads.

**When adding a new S3 table:**
1. Add key to `S3_KEYS` in `global.R`
2. Add `load_*()` call in `.data_loaded` (after the batch-fetch block)
3. Add `.cache_set()` call for the new key
4. Add the reactiveVal and wire it into `shared` in `app.R`

**The SAP dispatch has two paths:**
- **Primary** (`observe({ req(.s3_load_complete()) ... }, priority=-1)`): fires ~1s after Phase 1 completes. This is the normal path.
- **Fallback** (`observeEvent(sap_trigger(), ..., once=TRUE, ignoreInit=TRUE)`): fires at t+10s if Phase 1 errors. `sap_trigger = reactiveTimer(10000)`.
- Both observers have `.sn <= 1L` guards — they never run in the auth gate session.
- The `identical()` check in `load_sap_data()` prevents re-rendering the calendar when SAP data hasn't changed.
- `autoInvalidate` (30 min) handles background refreshes.

**SAP connect timeout is 800ms** (`connecttimeout = 0.8` in `sap_api.R`). Do not increase this — NRS at 192.168.14.131 is unreachable on this network and a higher timeout multiplies the wait by the number of companies.

---

## Stage 3 — Calendar Render (~96s)

**What happens:** `sap_data()` updates → `df_combined` (AR + AP) → `df_calendar` → `tags_day_map_rv` → `output$calendar` → `calendar_html()`. All four calendars (AR MXN, AR USD, AP MXN, AP USD) render sequentially in single-threaded R.

**Floor:** `calendar_html()` itself takes ~0.16s per render. `df_combined` takes ~0.45s. The remainder is Shiny scheduler overhead and reactive graph resolution with 2 ledger modules × multiple outputs.

### Rules

**`df_combined` must not depend on `conciliacion_rv`.** This reactive is read with `isolate()`:
```r
conc_rv <- isolate(tryCatch(shared$conciliacion_rv(), error = function(e) NULL))
```
If this `isolate()` is ever removed, every payment confirmation invalidates `df_combined` for both ledgers simultaneously, triggering 4 full calendar re-renders (~6 minutes of blocking work).

**`df_combined` dependencies are exactly:** `sap_data()`, `manual_inv()`, `moves_db()`, `empresa_sel()`, `papelera_rv()`. Adding any new reactive dependency here multiplies the render cost by however often that reactive fires.

**`calendar_html()` takes ~0.16s** — it is not the bottleneck. Do not optimize it further unless the row count grows beyond ~500 `dcur_rows`. The HTML string-builder rewrite (replacing `htmltools` tag objects) is already in place.

**`tags_day_map_rv` pre-computes `as.Date(FechaEff)` once** before filter and group_by. Do not revert to calling `as.Date()` twice on the full dataframe.

**`to_calendar_data()` filters `confirmed == TRUE` rows** at the very top before any other processing. This must remain the first operation in that function.

**`output$calendar` renderUI has a `tryCatch` wrapper.** Keep it — it prevents one ledger's render error from blocking all four calendars.

---

## Reactive Dependency Map

Understanding this prevents accidental re-render cascades.

```
sap_data()          → df_combined (AR) → df_calendar (AR) → output$calendar (AR)
                    → df_combined (AP) → df_calendar (AP) → output$calendar (AP)
moves_db()          → df_combined (both)
manual_inv()        → df_combined (both)
empresa_sel()       → df_combined (both)
papelera_rv()       → df_combined (both)

tags_db()           → tags_day_map_rv (both) → output$calendar (both)
pagar_hoy_db()      → staged_keys_rv (both)  → output$calendar (both)
month_val           → tags_day_map_rv + output$calendar
currency_choices    → output$calendar (currency resolution only)

conciliacion_rv     → df_combined (ISOLATED — no reactive dependency)
                    → modal crossout display only (reads on click, not on change)
```

**Any reactive added to `df_combined` fires the entire left column above.** Treat additions here with extreme caution.

---

## `.annotate_ap()` in `pagar_hoy_module.R`

This function matches AP invoice rows against the proveedores catalog. It was previously O(n×m) with `purrr::map_int` — rewritten as vectorized hash lookup.

### Rules

**Never revert to `purrr::map_int` or any per-row loop** over the full catalog. With thousands of suppliers the old approach caused ~3 minute freezes.

**The lookup order is:** exact alias → exact nombre → substring scan (only for unmatched rows, aliases ≥3 chars). Do not add more expensive steps (e.g. fuzzy matching, `agrepl`) to the hot path.

**`.annotate_ap_cached()` wraps `.annotate_ap()`** with a digest-keyed cache. Always call `.annotate_ap_cached()`, never `.annotate_ap()` directly. The cache key is `paste0(empresa, "_", digest(list(Parte, Documento)))` — it invalidates correctly when AP rows change but not on currency switches or saldo edits.

---

## S3 Read Performance

**On Windows:** Sequential reads only. `makeCluster()` / PSOCK workers cost more to spawn than the reads save. `future::multisession` has the same problem. The floor is ~1.4s per read on a good connection.

**On Linux/macOS:** `parallel::mclapply` with `mc.cores=4` gives true parallelism via fork. Stage 1 drops to ~5s, Stage 2 to ~8s automatically with zero code changes.

**`suppressMessages()` on `aws.s3::s3readRDS()`** suppresses the noisy `List of 5 $ Code: chr "NoSuchKey"` output for keys that don't exist yet (e.g. `interco_settings.rds`, `bancos_cuentas.rds` on a fresh deployment). Keep this wrapper — the `tryCatch` still catches errors correctly.

**The `.s3_missing_cache` env** prevents repeated round-trips for confirmed-absent keys within a session. Never bypass it with a direct `aws.s3::s3readRDS()` call.

---

## Known Floors (Cannot Be Improved Without Infrastructure Changes)

| Bottleneck | Current | How to fix permanently |
|---|---|---|
| S3 sequential reads on Windows | ~1.4s/read | Move to Linux/WSL2 → `mclapply` free |
| SAP NRS timeout | ~800ms/company | Fix 192.168.14.131 network route |
| Shiny single-threaded rendering | 4 calendars queue | `shiny::future_promise` or async Shiny |
| S3 geographic latency | ~35s/snapshot | Move bucket to closer AWS region |

---

## Files and Their Performance Risk Level

| File | Risk | Reason |
|---|---|---|
| `global.R` | 🔴 Critical | Startup blocking — any top-level S3 read delays app boot |
| `app.R` `.data_loaded` | 🔴 Critical | Stage 2 — load order determines whether reads are cache hits |
| `R/ledger_module.R` `df_combined` | 🔴 Critical | Any new reactive dependency cascades to all 4 calendar renders |
| `R/data_pipeline.R` `to_calendar_data` | 🟡 Medium | Confirmed filter must stay first; runs 4× per render cycle |
| `R/pagar_hoy_module.R` `.annotate_ap` | 🟡 Medium | Must stay vectorized; called per empresa per render |
| `R/bancos_module.R` | 🟢 Low | Renders only when Bancos tab is active |
| `R/global.R` `calendar_html` | 🟢 Low | 0.16s per render; already optimized |
| `R/persistence.R` `.s3_read` | 🟢 Low | Cache-first; live read only on miss |
