# HopDesk — Items & Company Wiring Audit

**Scope.** Everything that touches "items" — invoices from SAP plus user-created entries — from creation through calendar display, day-modal staging, search/vencidos surfaces, Agenda de Hoy, the Bajío TXT pipeline, partial payments, and intercompany. Plus a sweep of company management to find why Paragon Logistics (PL) feels half-wired and to check that adding any new company in the future will Just Work.

**Goal.** Numbers must be impeccable for production. This document is the reasoning layer. The companion file `CLAUDE_CODE_INSTRUCTIONS.md` is the action layer — atomic tasks Claude Code can execute with no further reasoning.

---

## Quick map of the data flow

```
SAP API ──┐
          ├─→ build_ledger_df() ──→ df_combined ──→ df_calendar ──→ calendar UI
manual ───┤                            │
          │                            ├─→ day modal (detail) ──┐
abonos ───┘                            │                         ├─→ Agenda de Hoy ──→ .annotate_ap ──→ generate_ppl ──→ Bajío TXT
                                       └─→ Vencidos / Search ────┘                              ↑
                                                                                                │
                                                                            proveedores catalog (alias, CLABE, medio_pago)
```

Every staging surface (calendar day modal, vencidos, search, intercompany, treasury map, bancos return-to-calendar, manual-entry agenda toggle, browse-by-range, abono parcial) writes rows into `pagar_hoy_db`. The pagar_hoy module then runs `.annotate_ap` which has a 5-pass match pipeline; the `Codigo` → catalog `alias` exact match is **Pass 0** (the most reliable) and Pass 1+ are fuzzy fallbacks against `Parte`/`Nombre`. If `Codigo` arrives empty, Pass 0 silently no-ops and you're at the mercy of fuzzy matching.

---

## Part 1 — The `Codigo` problem (the big one)

### 1.1 Root cause: column-name disagreement

There is no unified `Codigo` column inside the data pipeline. SAP rows carry it as **`Código de proveedor`** (AP) or **`Código de cliente`** (AR). Manual entries carry it as **`Codigo`**. `build_ledger_df` (in `R/data_pipeline.R`) does `bind_rows(sap, manual)` without harmonizing — so the resulting `df_combined` has two separate columns, each populated only for its own source.

Every downstream slice that filters by name `"Codigo"` (e.g. `R/ledger_module.R:514` in `keep`, `R/ledger_module.R:849` in `.fresh_lu`'s narrowed columns, the Vencidos `disp_df` builder at `R/vencidos_module.R:359`, the search `keep` list at `R/search_module.R:71`) catches the manual rows but misses SAP rows. The pagar_hoy module is even *defending* against this at `R/pagar_hoy_module.R:606` (`if (!"Codigo" %in% names(ann)) ann[["Codigo"]] <- NA_character_`).

**Fix the source once.** Add a unified `Codigo` column at the top of `build_ledger_df` so every downstream slice picks it up automatically. The Manual schema already uses `Codigo`, so for SAP rows we coalesce: `Codigo = coalesce(Código de proveedor, Código de cliente, Codigo)`. This is one of the highest-leverage edits in the whole audit — it removes ~6 places where the bug otherwise has to be patched individually.

### 1.2 The matching contract

The user's mental model is correct and matches the code:
- A supplier in the proveedores catalog has both `codigo` (SAP CardCode) and `alias` (BanBajío key, max 15 chars).
- In Networks' workflow, **the SAP CardCode IS the BanBajío alias** (same string).
- Pass 0 in `.annotate_ap` (`R/pagar_hoy_module.R:240–251`) takes the invoice's `Codigo` and looks it up against the catalog's `alias` column. When they match, the row is fully resolved (alias, clabe, banco, medio_pago) and skipped by all later passes.

So once `Codigo` arrives intact, matching is deterministic. The whole "color-coded green/yellow/red" UX in Agenda de Hoy works as designed. Today, because `Codigo` is dropped on most paths, AP rows reach the Agenda blank and fall through to the substring-on-Parte fallback (Pass 3) — which is fragile and silently mismatches when two suppliers have similar names.

### 1.3 Every staging surface that drops `Codigo`

These are the green buttons. All of them need to thread `Codigo` end-to-end (DOM → JS payload → R `keys_df`/`new_rows` → `pagar_hoy_db`):

| # | Path | File:Line | Surface |
|---|---|---|---|
| 1 | `stage_all` | `R/ledger_module.R:547` | Calendar day modal — "Enviar todos a Agenda" |
| 2 | `stage_sel` | `R/ledger_module.R:583` | Calendar day modal — "Enviar selección" (audit & summary modes) |
| 3 | `cart_grp_click` | `R/ledger_module.R:700` | Calendar day modal — per-Parte toggle |
| 4 | `cart_inv_click` | `R/ledger_module.R:780` | Calendar day modal — per-invoice toggle |
| 5 | Vencidos stage | `R/search_module.R:833` (handler) + `www/vencidos.js:20` (payload) + `R/vencidos_module.R:469` (DOM) | Vencidos green button |
| 6 | Búsqueda stage | `R/search_module.R:833` (handler) + `R/search_module.R:461,482,366` (payload + DOM) | Búsqueda green button |
| 7 | Bancos return-to-calendar | `R/bancos_module.R:2783` | Bancos → Papelera → restaurar |
| 8 | Intercompany | `R/interco_module.R:347` | IC table → "Agregar a Agenda de Hoy" |
| 9 | Treasury map | `R/treasury_map_module.R:824` | Map → "Send to agenda" |
| 10 | Abono parcial | `R/staging_browse_module.R:553` | (currently does not stage at all — see Part 3) |

These already carry it correctly: Manual Entry insert/edit (`app.R:1408,1432`) and Browse-by-range (`R/staging_browse_module.R:317`).

### 1.4 The schema is already correct

`pagar_hoy.rds` schema (`R/persistence.R:498`) declares `Codigo` as a first-class column. `.normalize` (`R/persistence.R:222–237`) adds it as `NA` for rows that omit it and silently drops anything not in the schema on save. So the persisted file has been "correct" all along — every row that came in without `Codigo` simply has `NA` there. Today the `NA`s are why Pass 0 misses.

---

## Part 2 — Empresa: the mandatory variable

User confirmed: **no item can ever live without an Empresa, and Empresa must be one of the listed companies in the group.** The current code respects this in spirit but has gaps that allow violations.

### 2.1 The "manual entry doesn't appear in calendar" bug — explanation

**Root cause: `bancos_confirmados` match key is missing `Moneda` (and `Parte`).**

`R/ledger_module.R:119–121`:
```r
match_key <- paste(df[["Empresa"]], df[["Documento"]])
conf_key  <- paste(bc_keys[["empresa"]], bc_keys[["documento"]])
df[["confirmed"]] <- df[["confirmed"]] | (match_key %in% conf_key)
```

The `Moneda` column exists in `bancos_confirmados` (per its schema at `R/bancos_persistence.R:55–69`) but is dropped from this match. Source 1 (the older `conciliacion_rv` path, lines 101–105) correctly uses `Empresa + Moneda + Documento`. Source 2 is asymmetric.

Then immediately at line 130: any manual row marked `confirmed = TRUE` is **deleted from `df_combined` entirely**. So the trap is:

> User creates a manual entry for `Empresa=NTS, Moneda=USD, Documento=ABC-123`. If anywhere in `bancos_confirmados` there is a row with `empresa=NTS, documento=ABC-123` (regardless of currency, regardless of party, regardless of original ledger), the new manual entry is marked confirmed and silently dropped from the calendar.

Documento numbers are user-typed for manual entries — collisions with old SAP/bancos history are entirely possible. This is the user's reported bug.

### 2.2 Other Empresa risks for impeccable numbers

**`empresa_sel_rv` resets on every `empresas_db()` change.** `app.R:643–649` — when the empresas registry updates (e.g. the user adds PL via the Empresas panel), `empresa_sel_rv` is overwritten with the full list of all active companies. The user's filter selection (e.g. "only NTS") is lost. Not a numbers-correctness bug but a bad UX surprise mid-session that could lead the user to publish wrong-scope reports.

**`apply_ic_filter` falls back to `df` when `ic_codes` and `ic_rfcs` are both empty** (`R/data_pipeline.R:262–264`). Good defensive behavior. But for a brand-new empresa (PL) with no IC codes registered yet, the IC filter becomes a no-op for that empresa specifically — and `code_col` is per-ledger global (`Código de proveedor` for AP). When the user has IC mode set to "exclude," PL invoices that *should* be IC-flagged (because they're going to/from a sister company) are not excluded. **For Networks at go-live this matters: the calendar will show IC payments from/to PL as if they're real third-party flows until PL is fully registered in IC settings.** No code bug — but a deployment checklist item.

**`df_combined`'s "fallback when empresa filter empties everything" is dangerous** (`R/ledger_module.R:81–87`). When the empresa filter removes all rows because of a name mismatch, the code logs a message and **shows everything unfiltered**. For the treasurer this means: under certain empresas-rds inconsistency states, the calendar quietly displays the wrong company's data instead of nothing. Better to show "no data" with a clear "filter mismatch" notice than silently show all.

### 2.3 The `Empresa` value travels OK, but the *initials* often don't

Several modules need to recover initials from a full name (or vice-versa) to look up bank accounts, IC config, etc. The lookup `ini = names(COMPANY_MAP)[unname(COMPANY_MAP) == empresa]` appears 8+ times across `pagar_hoy_module.R`, `treasury_map_module.R`, `interco_module.R`, `data_pipeline.R`, `vencidos_module.R`. Every one reads `COMPANY_MAP` statically. None of them use `shared$company_map()` reactively. **This is the reason adding PL "feels half-wired"** — see Part 4.

---

## Part 3 — Abono parcial (partial payments)

### 3.1 The feature is unreachable from the UI

`show_abono_modal()` exists at `R/staging_browse_module.R:439`. Its docstring says "call from `pick_abono` observer in app.R." There is no `pick_abono` observer in app.R. The "Agregar" picker (`app.R:1326–1334`) only has Cobro and Pago buttons. So the entire abono parcial UI flow has no entry point.

`setup_abono_browse()` is wired (`app.R:718`), so the handler would run if a row payload arrived — but no UI ever sends one.

### 3.2 Even if reachable, it doesn't stage to Agenda

The handler at `R/staging_browse_module.R:547–585` writes only to `abonos_db` (which feeds `active_abonos_summary` in `R/data_pipeline.R:227` to reduce the calendar's displayed `Saldo vencido`). It never calls `upsert_pagar_hoy`. The JS payload at line 378 captures `codigo` but the R handler at lines 553–568 doesn't read it.

### 3.3 The intent (per user)

> Reduce the calendar balance for the affected invoice to the new (post-abono) amount, AND stage a row to Agenda de Hoy so the partial payment can ride out via the Bajío TXT today. There should be a hover-over popup on the calendar showing original amount and all partial payments registered against it.

The reduction-of-balance leg already works in `data_pipeline.R`. What's missing:
1. UI entry point — a third "Abono parcial" button in the `btn_add_entry` picker.
2. The handler needs to **also** insert a row into `pagar_hoy_db` with `Importe = abono amount` (not the original invoice's full amount), tagged so the user can recognize it.
3. A hover-over popup on calendar invoices that have any non-deleted abono rows, showing original `Saldo vencido`, list of abonos, current displayed balance.

### 3.4 Consistency requirements (the "hard part" the user flagged)

For this to be impeccable in production:

- **Each abono is a child of exactly one invoice.** The natural key is `(ledger, Empresa, Moneda, Documento)`. This is already what `active_abonos_summary` joins on in `data_pipeline.R:233`. So the agenda-staging row built by the handler must use the same key for the *parent*, but a fresh `id` for itself.
- **The Agenda row for an abono is a different "thing" than the parent invoice.** Two scenarios to disambiguate:
  - **A.** User stages the parent invoice for full payment, then registers an abono. The two get summed by treasurer's eye. Bad.
  - **B.** User registers the abono, parent invoice still appears on calendar with reduced balance. Treasurer can decide to also stage the (reduced) parent for full or partial payment another day. Good.
  - **The fix:** abono Agenda rows should have a distinct identity — recommend a new column `tipo_item` on `pagar_hoy` (values: `"factura" | "abono"`) so the Agenda UI can render them visibly differently and the Bajío TXT path knows what it's processing. The `Importe` is whatever the user typed for the abono.
- **What if the same invoice gets multiple abonos in one day?** Possible. They should be separate rows in pagar_hoy with separate `id`s. Both go in the Bajío TXT (the bank doesn't care). The calendar's reduced balance is `Saldo_vencido - sum(active abonos)`.
- **Deleting an abono.** Must reduce sum on calendar AND remove the corresponding pagar_hoy row if it's still pending. Currently `abonos_db` has a soft-delete `status` field (`active` | `deleted`). When status flips to `deleted`, the calendar already updates because `active_abonos_summary` filters. The pagar_hoy row needs an analog action — preferably `unstage_pagar_hoy` keyed by `id` (not by Empresa+Moneda+Documento — see issue 6.3 below).
- **What if user stages the parent first, then registers abono?** Calendar shows reduced balance; the staged parent in Agenda is still showing the *full* original `Importe`. Mismatch. Two options:
  - (i) Treat the abono as a strict mutation of the staged parent: edit pagar_hoy row's `Importe` down by the abono amount instead of inserting a new row.
  - (ii) Reject abonos against parents already staged for today; force the user to unstage first.
  - **Recommend (i).** Simpler mental model. The user sees one row in Agenda for the day with the correct partial amount. The hover popup shows the original and the abono. Implementation: when registering abono, check if a pagar_hoy row exists for `(ledger, Empresa, Moneda, Documento)` with `status=pending`; if yes, decrement its `Importe`; otherwise insert a new pending row with `Importe = abono_amount`.

### 3.5 Hover popup

Once the data model is consistent, the hover popup is straightforward — the calendar already gets `abono_total`, `Saldo_original`, and `has_abono` (`R/data_pipeline.R:236–248`). The day modal's per-invoice rendering needs to read the same fields and show: original `Saldo_original`, list of `(fecha_abono, importe, notas)` from `abonos_db` for that invoice, and the current displayed `Importe`.

---

## Part 4 — Company management & PL wiring

### 4.1 What's actually working

- `R/global.R:184–198` rebuilds `COMPANY_MAP` from `empresas.rds` at app boot, after the critical S3 preload. So if PL exists in `empresas.rds` when the app boots, every module that reads `COMPANY_MAP` at startup sees PL.
- `app.R:643–648` re-assigns `COMPANY_MAP` (and `empresa_sel_rv`) whenever `empresas_db()` updates mid-session. So new empresas added via the Empresas panel propagate to *some* surfaces.
- `app.R:1216–1269` (`output$empresa_ui`) is fully reactive — adding PL via Empresas → button appears immediately in the topbar.
- `R/empresas_module.R:421–434` cascades initials renames (e.g. `NL` → `NLOG`) across `ctas_cuentas`, `interco_v2`, `proveedores`, `parte_alias_map`. No data is lost.

### 4.2 What's broken — UI surfaces that capture the company list at first render

The pattern: `all_companies <- sort(unname(COMPANY_MAP))` at module-init time, then build tabs/panels from it. Captured once. Doesn't react to subsequent changes.

| File | Lines | Symptom |
|---|---|---|
| `R/pagar_hoy_module.R:20` | `pagarHoyUI` builds per-empresa tab panels at UI render time | Adding PL mid-session: PL tab missing until app restart |
| `R/pagar_hoy_module.R:124` | `pagarHoyServer` captures `all_companies` at server-init | All per-empresa observers (`bal_*`, `tbl_ap_*`, `tbl_ar_*`) only registered for empresas present at session start |
| `R/treasury_map_module.R:14, 32–33` | `inv_map`, opening balance store init | New empresa not reachable from map, no opening-balance slot |
| `R/treasury_map_module.R:342` | `selectInput` `target_co` choices | New empresa can't be selected as target until refresh |
| `R/interco_module.R:45` | `inv_map` (full-name → initials) captured | Any IC computation involving the new empresa silently misses |
| `R/proveedores_module.R:87, 353` | Empresa filter dropdowns | New empresa doesn't appear; supplier creation/edit can't tag PL |

### 4.3 What's broken — server-side static reads in `data_pipeline.R` and `sap_api.R`

| File | Lines | Symptom |
|---|---|---|
| `R/data_pipeline.R:332` | `inv_map` in `aggregate_ic_codes` | Full-name → initials lookup misses new empresas |
| `R/sap_api.R:322–325, 395` | `fetch_all_companies`, `sap_companies_available` iterate `names(COMPANY_MAP)` | OK on first session after `empresas.rds` is loaded (because R/global.R rebuilds). But: if PL is *added* via the Empresas panel mid-session and SAP is then refreshed, the fetcher uses the rebuilt `COMPANY_MAP` (since app.R:648 re-assigns into `.GlobalEnv`). This **does** work. But it depends on app.R:648 running before SAP refresh — fragile coupling. |

### 4.4 Why "the other companies feel hard-wired"

Because they were the only ones present when `COMPANY_MAP` was first captured at app boot, and the captured copy is what these modules use. PL was added later (it's already in the seed at `R/global.R:60` now, but historically it wasn't). Any module that doesn't read `shared$company_map()` reactively keeps the old list.

### 4.5 What "wire PL correctly" actually means as a checklist

Given an empresa initial PL, full name "Paragon Logistics", to be in production-ready state, the following must all be true:

1. **Empresa registry** — row exists in `empresas.rds` with `initials="PL"`, `nombre_corto="Paragon Logistics"`, `activa=TRUE`, `deleted=FALSE`. ✅ Done by user via Empresas panel.
2. **SAP credentials** — env vars `SAP_PL_URL`, `SAP_PL_COMPANY`, `SAP_PL_USER`, `SAP_PL_PASSWORD` all set on the deploy host. Skipped silently with a log message if missing (`R/sap_api.R:329–333`).
3. **Bank accounts** — at least one row in `ctas_cuentas` with `Empresa="PL"`, `activa=TRUE`, valid `cuenta` (and `is_ppl_default=TRUE` for the currency the user wants the Bajío TXT to use). Without this, the Bajío TXT generator gets `cuenta_origen=""` and the file is malformed.
4. **Intercompany registry** — at minimum, an entry in `interco_v2$companies$PL` with whatever AR and AP CardCodes from sister companies. Without this, IC filtering treats PL as having no IC partners.
5. **Bajío "tipo de empresa" registration** (per user note) — n/a in current code; the Bajío contract is tied to the cuenta, not the empresa.
6. **All UI surfaces re-render with PL visible.** Per 4.2 above, this requires app restart today. The fix is to convert the static `unname(COMPANY_MAP)` reads to reactive `shared$company_map()` reads, or to add a session-start refresh trigger so the per-empresa observers and panels re-build.

The user-facing failure mode: if (1)–(4) are done but (6) isn't, the user thinks "PL is added" but Agenda de Hoy has no PL tab, the calendar's empresa toggle has a PL button (because that one IS reactive) but nothing in Agenda or treasury map respects it. **Inconsistent across surfaces — the worst kind of bug for trust.**

### 4.6 Other false-wirings found across the app

These are the asymmetric/inconsistent patterns I noticed during the sweep that aren't directly Codigo or Empresa, but matter for production correctness:

- **`unstage_pagar_hoy` keys ignore `id`** (`R/persistence.R:528–532`). Removes by `(ledger, Empresa, Moneda, Documento)`. But the comment on `upsert_pagar_hoy` (line 518) explicitly says: "Pass `keys = "id"` for manual entries to avoid replacing entries that share a Documento but are different invoices." `unstage` is not symmetric. So: stage two manual entries for `(NTS, MXN, ABC-123)` (different Parte, deliberate dup), then unstage one of them — both go.
- **Two staging paths bypass `upsert_pagar_hoy` entirely** (`R/interco_module.R:362`, `R/treasury_map_module.R:840`). They use `dplyr::bind_rows + distinct(id)`. Since `id` is regenerated every call, the same invoice can be staged twice from these surfaces and the id-based dedup doesn't catch it. Result: duplicates in pagar_hoy → double-counted in calendar sums.
- **`ph_unlink_supplier` is asymmetric to `ph_link_supplier`** (`R/pagar_hoy_module.R:1473` vs `1510`). Link scopes by (Parte, Empresa). Unlink scopes by Parte alone — clobbers the link for ALL empresas. Mid-session this can quietly break the matching for empresas the user didn't even click on.
- **`bancos_confirmados` match key drops `Moneda`** (`R/ledger_module.R:119–121`) — the manual-entry-disappears bug, see 2.1.
- **`to_calendar_data` drops `Empresa` from the group key** (`R/data_pipeline.R:418`). In single-empresa mode this is harmless (upstream filter has reduced df to one empresa); in multi-empresa mode the bubble totals collapse identical-name parties across empresas. Risk for Networks: if treasurer toggles "all empresas" while reviewing a day, sums look fine but the audit-mode detail under the same bubble shows multiple empresas — visually confusing. Recommend adding Empresa to the group key.
- **Cache and S3 diverge on save** — `save_pagar_hoy` (`R/persistence.R:511–514`), `save_manual` (`R/persistence.R:354–357`) cache the *un-normalized* df but write the normalized one to S3. So same-session reads from cache may carry extra columns or wrong types that S3-loaded reads wouldn't. Not currently observed to cause bugs but is a latent class of inconsistency.
- **`load_pagar_hoy` filters `status %in% c("pending","confirmed","cancelled")`** (`R/persistence.R:508`). NA `status` rows are silently dropped on every load (because `NA %in% x` is `FALSE`). So if a row ever got persisted with NA status (concurrent write race, schema migration, etc.) it vanishes invisibly on next reload. Better: explicitly check and recover (treat NA as `pending`).
- **`tipo` column written by every staging path is dead weight** (e.g. `R/ledger_module.R:567,613,740`, `R/bancos_module.R:2786`). It's not in `pagar_hoy` schema (`R/persistence.R:491–504`), so `.normalize` drops it on save. Misleading to readers. (Note: this is *not* the same as `medio_pago` SPI/BCO/SPD which is per-supplier in the proveedores catalog and works correctly.)
- **`generate_ppl` doesn't auto-detect SPD for USD payments** (`R/ppl_generator.R:64–80`). Branches on BCO (CLABE prefix "030" or banco_dest contains "BAJIO") vs everything-else (which it labels "SPI"). USD payments today get labeled SPI in the file, which is the wrong protocol code for cross-currency. Per user: known, low priority right now, "works great" — but worth flagging for when USD volumes increase.
- **`apply_ic_filter` falls back to no-op when both `ic_codes` and `ic_rfcs` are empty for a given empresa** (`R/data_pipeline.R:262–264`). Means: a freshly-added empresa with no IC config registered behaves as "all transactions are external," which is wrong if it transacts with sister companies. See 2.2.

### 4.7 Files that look unwired or stale

- **`/global.R`** (root) — superseded by `R/global.R`. ~221-line diff. Both auto-source by Shiny, R/global.R wins because it's sourced second alphabetically and reassigns the drifted symbols. Confusing and brittle. Delete.
- **`app_timed.R`** — diagnostic timer-instrumented copy of `app.R`. Never sourced from production. Delete.
- **`R/manual_entry_handlers.R::manual_entry_handlers()`** — the standalone modal opener. Replaced by `show_combined_entry_modal` in staging_browse. Only `manual_edit_handlers` (the editor) is still referenced. The shared helper `manual_entry_tab_content` is also still referenced. Remove the unused `manual_entry_handlers` function only.
- **`R/bootstrap_proveedores.R`** — one-shot migration helper for seeding NTS suppliers. Hardcoded values for a specific historical migration. Never called from anywhere. Delete the entire file.
- **`R/.RData`, `R/.Rhistory`** — dev clutter. Already harmless but should not be in S3 nor committed. Add to .gitignore if not already.
- **`wipe_scripts/*`** — operational scripts, not app code. Keep but not relevant to the app.

---

## Part 5 — Sums correctness sanity check

User asked for "impeccable numbers." Here's where sums are computed and whether they can drift:

| Where | What | Trustworthy? |
|---|---|---|
| Calendar bubbles (`R/data_pipeline.R:408–425`, via `to_calendar_data`) | `sum(Importe)` by `(Fecha, Moneda, Parte)` | ⚠️ Empresa not in group key — see 4.6. Single-empresa mode is fine; multi-empresa mode collapses across empresas. |
| Day modal totals (`R/ledger_module.R:730, 1201, 1679`) | `sum(detail[mask, "Importe"])` | ✅ Computed from `detail` which still has Empresa, so correct. |
| Agenda Pagar/Cobrar pills (`R/pagar_hoy_module.R:387–410`) | `sum(Importe)` by `(Moneda, ledger)` across all empresas | ✅ Intentionally cross-empresa (group net position view). |
| Per-empresa balance calc (`R/pagar_hoy_module.R:427–457`) | `tp = sum(s_ap$Importe)`, `tc = sum(s_ar$Importe)` filtered by Empresa+Moneda | ✅ Correct. |
| Bajío TXT trailer (`R/ppl_generator.R:106–110`) | `sum(payments$importe) * 100` (centavos) | ✅ Correct — uses the rows that survived `validate_ppl` & filtered by `ppl_status == "ok"`. |
| Calendar after abonos (`R/data_pipeline.R:230–248`) | `Saldo_vencido_displayed = pmax(0, Saldo_vencido - abono_total)` | ✅ Correct logic. The `pmax(0, ...)` guard handles overpayment edge case. But note: if a user records an abono > saldo (typo, or saldo updated downward by SAP after abono was registered), the displayed value is 0, hiding the discrepancy. Recommend a warning when `abono_total > Saldo_original`. |

The most likely sum-correctness production risk is the Empresa-collapse in `to_calendar_data` if the treasurer ever toggles "all empresas" (which is the default). With Networks' five companies all currently active, two of them sharing common vendors (e.g. "FedEx" appears under multiple empresas), the calendar bubble for FedEx-on-2026-05-15 would sum across empresas. Not strictly incorrect (the treasurer wants to see total cash impact across the group), but the audit detail underneath shows it broken out per empresa, so the same number can look like it disagrees.

---

## Part 6 — Recommendations summary

Numbered priority order. P0 = blocks production for Networks. P1 = significant correctness/UX. P2 = quality of life.

**P0 — must fix before Networks goes live**
1. Harmonize `Codigo` in `build_ledger_df`. (Part 1.1)
2. Thread `Codigo` through all 10 staging paths. (Part 1.3)
3. Fix `bancos_confirmados` match key — add `Moneda`. (Part 2.1)
4. Convert `pagar_hoy_module`, `treasury_map_module`, `interco_module`, `proveedores_module` to read `shared$company_map()` reactively at panel-build time so PL is visible everywhere. (Part 4.2, 4.4)
5. Wire abono parcial UI entry point + agenda staging + calendar hover. (Part 3)
6. Fix `unstage_pagar_hoy` to support id-based unstage; replace `bind_rows + distinct(id)` in interco/treasury_map with proper `upsert_pagar_hoy`. (Part 4.6)
7. Fix `ph_unlink_supplier` to scope by (Parte, Empresa) to match link. (Part 4.6)
8. Fix `empresa_sel_rv` reset behavior so the user's filter survives an empresas update. (Part 2.2)

**P1 — important for impeccable numbers**
9. Add Empresa to `to_calendar_data`'s group key. (Part 4.6, Part 5)
10. Make `df_combined`'s "show all on filter mismatch" fallback show "no data" instead. (Part 2.2)
11. Make `load_pagar_hoy` recover NA status as `pending` instead of dropping. (Part 4.6)
12. Make `save_*` functions cache the normalized df, not the raw one. (Part 4.6)

**P2 — cleanup**
13. Delete `app_timed.R`, root `global.R`, `R/bootstrap_proveedores.R`, and the `manual_entry_handlers()` function. (Part 4.7)
14. Remove the `tipo` (cobro/pago) column from staging-row builders. (Part 4.6)
15. Add a deployment checklist for adding a new empresa. (Part 4.5)

**Deferred (per user)**
- SPD auto-detection in `generate_ppl`. (Part 4.6)
- Reactive re-render of IC settings panel when company_map changes mid-session. (Section 4.5 last paragraph)

---

## What this audit deliberately did not look at

- The reporte module (`R/reporte_module.R`) — 1946 lines, but it's a read-only PDF generator, doesn't write to anything. Out of scope for "ensure items work flawlessly."
- The bancos module's parsing logic (`R/bancos_parser.R`, 482 lines) — handles bank-statement uploads. Doesn't touch items directly, only `bancos_movimientos`. Out of scope.
- The auth/tier/usuarios system. Functional and orthogonal to the items pipeline.
- Performance. The codebase has clear performance instrumentation (`app_timed.R`, `[TIMER]` log lines, `.s3_preload_cache`, `.app_data_cache`). Not in this audit's scope.

If after the P0/P1 fixes any items numbers still look off, the next layer to audit would be the conciliacion ↔ bancos_confirmados two-source path in `df_combined` (lines 89–123) — there's a comment hinting at a "wire-cut" migration that may have left other asymmetries beyond just the Moneda one.
