# Audit — Unifying "Is This Invoice Confirmed?" Across the App

**Status: audit + recommendation only. No code changes were made as part of
this document** (beyond the two narrow, already-merged fixes referenced in
§0 for context — this document does not ask anyone to redo those). This is
written for a **separate future Claude Code session** to pick up and turn
into an implementation plan, in the same spirit as `docs/saas_rebuild/`'s
`STAGE_N_*.md` documents — read that folder's `ARCHITECTURE.md` for the
house style if you want more examples of the format this document follows.

## 0. How this document came to exist

While wiring the Intercompany module to exclude already-confirmed invoices
(so Treasury has a clean "what's still open" view even when Finance is slow
to close things out in SAP — see the two commits below), it became clear
that "is this invoice confirmed" is **computed independently in at least six
places** across the codebase, with real drift between them. Two commits
already landed to fix the most urgent, concrete instances of this:

1. `R/interco_module.R` — added confirmed-invoice + abono-netted exclusion to
   the Intercompany table/graph (commit `7d1478d`).
2. `R/vencidos_module.R` — the "Vencidos" badge counter had its own
   independent, buggy reimplementation of the confirmed-check (missing
   `Moneda` from its key, no case/whitespace normalization); fixed to just
   read the `confirmed` column the calendar's own logic already computes
   (commit `98677b6`).

Fixing #2 surfaced the bigger question this document is about: **there is no
single source of truth for "confirmed," and the module count keeps growing.**
The user (Mouse) asked for a full audit of the fractured state before anyone
attempts a real unification — that's what follows.

## 1. The canonical logic today, and where it lives

The most complete implementation is `df_combined()` in `R/ledger_module.R`
(~lines 279-417 as of this audit — **re-verify with a fresh read before
editing**, this file changes often). It's the reactive that feeds the AR/AP
calendar, and it ORs together **four independent sources** to decide whether
an invoice is settled:

| # | Source | Match key | Normalization |
|---|---|---|---|
| 1 | `conciliacion_rv` (legacy table) | `(Empresa, Moneda, Documento)` + `tipo` | `toupper(trimws(...))` on all three |
| 2 | `bancos_confirmados`, active (`!eliminado`), `tipo` matches ledger | `(Empresa, Documento, Moneda)` | `toupper(trimws(...))` on all three |
| 3 | Papelera SAP-ghost rows (calendar's own trash, `source=="sap"`) | `(Empresa, Moneda, Documento)` | **raw, not normalized** — inconsistent with 1/2/4 |
| 4 | `pagar_hoy_db`, `status=="confirmed"`, `ledger` matches | `(Empresa, Documento, Moneda)` | `toupper(trimws(...))` on all three |

It produces **three** boolean columns, not two — this matters for anyone
building a canonical function, since UI code depends on distinguishing them:

| Column | Meaning | Set by |
|---|---|---|
| `confirmed` | Master exclusion flag — union of all 4 sources. Everything downstream (calendar heat-map, Vencidos, Search) reads only this one. | All 4 |
| `is_paid_ghost` | "Paid, shown as a struck-through ghost." Excludes manual-source Source 4 rows on purpose — those get physically removed from the dataframe instead of ghosted. | Sources 1, 2, and the SAP-only subset of 4 |
| `is_ghost` | "Soft-deleted via the calendar's own trash," a different concept (deleted, not necessarily paid) that happens to render the same way. | Source 3 only |

Confirmed manual-source rows are dropped from the dataframe entirely
(`R/ledger_module.R:402-404`); confirmed SAP-source rows stay with
`confirmed=TRUE` so they render as ghosts and get excluded from sums.
Provisions have all three flags forcibly cleared regardless of any match
(`R/ledger_module.R:410-417`), as a belt-and-suspenders guard.

`is_paid_ghost`/`is_ghost` are always a proper subset of `confirmed` by
construction — that invariant holds consistently. One micro-drift already
found: `to_calendar_data()` (`R/data_pipeline.R:538-543`) only checks
`confirmed`/`is_ghost` for exclusion, never `is_paid_ghost` directly — currently
harmless only because every `is_paid_ghost=TRUE` row also has
`confirmed=TRUE`. If a future edit ever set one without the other, this would
silently break. Worth guarding against explicitly if a canonical function is
built.

## 2. Every consumer, and its verdict

| File | Verdict | What it actually does |
|---|---|---|
| `ledger_module.R` (`df_combined()`) | **Canonical source** | See §1. |
| `data_pipeline.R` (`to_calendar_data()`) | Correctly reads | Only reads `confirmed`/`is_ghost`, doesn't recompute. See the drift risk noted above. |
| `ledger_module.R` (day-detail modal, invoice-group rendering, ~lines 2228-2499, 2746, 2877-2881) | Correctly reads | All reference the pre-computed columns; no recomputation. |
| `search_module.R` (`build_ledger_table()`, ~lines 40-83) | Correctly reads | Pulls from `shared$df_combined_AR()/AP()` directly and carries `confirmed` through unchanged. |
| `vencidos_module.R` | **Fixed this session** | Badge now reads `df$confirmed` from `shared$df_combined_AR()/AP()`, same as its own table-render logic already did. No more independent recomputation. |
| `interco_module.R` (`.filter_ic()`) | **5th independent implementation** — diverges | Uses Sources 2 + 4 only (`bancos_confirmados` + `pagar_hoy_db`) — **missing Source 1 (`conciliacion_rv`) and Source 3 (papelera ghosts) entirely**. Adds an exact-amount-match safety guard the canonical logic does **not** have. Net effect: an invoice confirmed only via the legacy `conciliacion_rv` path, or ghosted via papelera, disappears from the calendar/Vencidos but would still show up in Intercompany today. |
| `cashflow_preview_module.R` / `cashflow_export_module.R` (`build_export_combined_df()`, shared by both) | **Total omission** | Calls the same `build_ledger_df()` as the calendar, applies its own papelera anti-join and IC filter, but **never applies any of the 4 confirmation sources at all**. Confirmed-but-not-yet-SAP-closed invoices are fully counted in the live Cash Flow Preview panel and the exported Word/Excel report. |
| `reporte_module.R` (`compute_pulse()`, the Cash Flow Pulse pages) | **Total omission** | Builds its own SAP-normalization pipeline directly from `shared$sap_data()`, bypassing `df_combined()`/`build_ledger_df()` completely. Same class of gap as Cash Flow Preview/Export. |
| `bancos_module.R` (Historial tab) | Not a duplicate | Reads `bancos_confirmados`/`conciliacion_rv` directly for **display** (browsing the source tables themselves), not re-deriving a ledger-style confirmed flag. Fine as-is. |
| `pagar_hoy_module.R` | Correct, different role | `staged()` filters `status=="pending"` before a row can be (re-)selected for confirmation — a write-side guard, not a re-derivation of confirmed status computed elsewhere. This is also the actual *writer* of Sources 2 and 4 (see §3). |
| `pasivos_module.R` (~750-774, 838-862) | Different concept, not a duplicate | Checks `status != "confirmed"` before deleting `pagar_hoy_db` rows during a provision revert — a deletion-safety guard on the same field Source 4 uses, not a display/exclusion recomputation. |
| `pasivos_table_module.R` / `pasivos_engine.R` (provision lifecycle, `estado` field) | **Good counter-example** | Provisions track confirmation via an explicit **FK** (`bancos_confirmados.provision_id`/`confirmacion_id` → `pasivos_provision_item_confirmed()` in `pasivos_engine.R:897-917`, driven by a watcher in `pasivos_observers.R:37-93`), not a heuristic `(Empresa, Documento, Moneda)` key match. Worth citing as the model to move *toward*, not away from, if a redesign ever revisits the key itself. |

## 3. Everything that can write or reverse a confirmation — a full inventory

Beyond the two documented handler pairs (`pagar_hoy_module.R`'s
`do_confirm_ap_*`/`do_confirm_ar_*`, and `bancos_module.R`'s
`undo_conf`/`delete_conf`), there are more write paths than previously known:

1. **`pagar_hoy_module.R:1436-1456` (AP) and `:1662-1682` (AR)** — every
   confirmation, right now, **still writes a fresh row to `conciliacion_rv`**
   in addition to `bancos_confirmados`, despite the surrounding code treating
   `conciliacion_rv` as a "legacy, pre-migration" concern. This is not
   historical — it is actively written today. See the concrete bug this
   causes in §4.1.
2. **`bancos_module.R:1921-1969` (`.do_vinculation()`, "Vincular" dedup
   modal)** — can set `bancos_confirmados$eliminado <- TRUE` as a side effect
   of merging duplicate bank-matching candidates, if the discarded
   candidate's source is `"confirmado"`.
3. **`bancos_module.R:2434-2499` ("Sugerencias" auto-suggestion dedup,**
   `sug_keep_a`/`sug_keep_b`**)** — same kind of side-effect reversal, a third
   and fourth independent path (beyond `undo_conf`/`delete_conf`) that can
   un-confirm an invoice.
4. **`settings/settings_sincro.R:228-310`** (agenda sync activate/deactivate)
   — wholesale-replaces `pagar_hoy_db` with another user's full snapshot,
   unfiltered by status. Not a confirmed-logic bug per se, but a possible
   source of lost `status="confirmed"` rows if a confirmation happens
   concurrently with a sync toggle. Worth a look when this area gets touched.
5. **`bancos_parser.R:423-482` (`auto_match_agenda()`)** — a sixth, related
   but distinct matching heuristic: matches newly-imported bank statement
   rows against `bancos_confirmados` using **amount + date only** (±1%, ±2
   days), no Empresa/Documento/Moneda key at all. This sets a *reconciliation*
   flag on the bank movement row, not the invoice's `confirmed` status — a
   different concept that happens to touch the same table. Mentioned here
   only so the eventual unification doesn't accidentally conflate the two.

**No direct table manipulation was found in `wipe_scripts/` or `scripts/`.**

## 4. Concrete, demonstrable bugs (not just theoretical inconsistency)

### 4.1 🔴 "Deshacer" does not actually undo a confirmation for the calendar

Because every confirmation writes to `conciliacion_rv` (§3.1) — a table
whose schema (`R/persistence.R:1173-1189`) **has no `eliminado` column and no
UI to reverse a row** — and because `undo_conf`/`delete_conf` only ever touch
`bancos_confirmados.eliminado` and `pagar_hoy_db.status`, the following
sequence is very likely reproducible today:

1. Confirm an AR or AP invoice through Agenda de Hoy.
2. Click "Deshacer confirmación" in Bancos → Historial.
3. `bancos_confirmados.eliminado` flips `TRUE` (Source 2 stops matching).
   `pagar_hoy_db.status` resets to `"pending"` (Source 4 stops matching).
   **The `conciliacion_rv` row from step 1 is never touched — Source 1 still
   matches, forever.**
4. Net result: the invoice is simultaneously "pending" in Agenda de Hoy
   (so staff can re-stage/re-confirm it) **and** permanently rendered as
   `confirmed=TRUE`/struck-through/excluded from calendar sums, because
   Source 1 alone is enough to set `confirmed=TRUE`.

This needs an explicit decision from Mouse before it's fixed — see §6.

### 4.2 🔴 Cash Flow Preview, the exported Word/Excel report, and the Reporte module's Cash Flow Pulse all overstate projections

None of `build_export_combined_df()` (feeds both the live preview panel and
the export) or `reporte_module.R`'s `compute_pulse()` apply *any* of the four
confirmation sources. Every invoice Treasury has already confirmed as
paid/collected via Agenda de Hoy — even ones the calendar itself correctly
excludes — still counts in full in these three views. This is the same class
of problem the Intercompany fix (commit `7d1478d`) just solved for a
different module; these are the next-most-valuable places to point at a
canonical function.

## 5. Recommended direction (for the next session + Mouse to finalize — not prescriptive)

Extract the logic currently embedded in `df_combined()` (§1) into one
reusable function — something like `compute_confirmed_flags(df, ledger,
shared)` — living in `R/data_pipeline.R` next to `build_ledger_df()`, since
that's already the file responsible for assembling ledger data before
per-module concerns take over. It should:

- Take the merged ledger dataframe + the `shared` reactive-accessor list, and
  return the same dataframe with `confirmed`/`is_paid_ghost`/`is_ghost`
  columns set, using exactly the four-source logic in §1 (normalizing Source
  3 the same way as 1/2/4 while doing this — that inconsistency should be
  fixed as part of extraction, not left behind).
- Be called by `df_combined()` itself (replacing its inline block — the
  calendar's behavior must not change as a result of the extraction).
- Be called by `build_export_combined_df()` (fixes §4.2 for Cash Flow
  Preview/Export) and by `reporte_module.R`'s `compute_pulse()` (fixes §4.2's
  Reporte gap).
- Be evaluated for `interco_module.R`: once this function exists, Intercompany
  should call it too (picking up the two sources it's currently missing —
  `conciliacion_rv` and papelera ghosts) rather than keeping its own 5th
  implementation. Its exact-amount-match safety guard (added in commit
  `7d1478d` specifically because confirmation records never expire and the
  bare key has no protection against DocNum reuse) is a genuinely good idea
  that arguably belongs in the canonical function too, as an opt-in parameter
  — worth a real discussion with Mouse rather than silently dropping it during
  unification, since it's the one piece of defense-in-depth logic that exists
  today.
- `vencidos_module.R` and `search_module.R` need no changes — they already
  just read the resulting columns.

## 6. Needs an explicit decision from Mouse before implementation — do not decide unilaterally

1. **§4.1's `conciliacion_rv` bug**: should new confirmations stop writing to
   `conciliacion_rv` going forward (treating it as a frozen historical
   table), or should it be given an `eliminado` column and wired into
   `undo_conf`/`delete_conf` so reversal actually works end-to-end? Either is
   viable; this is a real product decision with real behavior consequences
   for Treasury's workflow, not a mechanical bug fix.
2. **The multiple reversal paths in §3.2-3.4**: are the Vincular/Sugerencias
   dedup flows' ability to silently un-confirm an invoice *intentional*
   (a reasonable side effect of correcting a duplicate-match mistake) or
   *accidental*? Worth walking through with Mouse using concrete scenarios
   before touching them.
3. **Whether to carry the amount-match safety guard into the canonical
   function** (§5) — and if so, whether it should also gain a date-window
   check, given the same reasoning that motivated adding it to Intercompany
   applies equally to the calendar itself.

## 7. Test plan hints for the implementing session

No existing automated test suite covers this area (`tests/_run_saas.R` and
friends are about the multi-tenant/SaaS rebuild, unrelated). A real
implementation should add coverage for at minimum:

- Each of the 4 sources independently sets `confirmed=TRUE` on a matching
  synthetic invoice row, and does *not* set it on a non-matching row (case,
  whitespace, and — for Source 3 — the normalization fix from §5).
- `is_paid_ghost`/`is_ghost` subset invariant holds after extraction.
- A manual-source confirmed row is removed from the dataframe entirely; a
  SAP-source one is retained with `confirmed=TRUE`.
- `build_export_combined_df()` and `compute_pulse()` correctly exclude a
  confirmed invoice after the fix (regression test for §4.2).
- Whatever `conciliacion_rv` decision comes out of §6.1, write a test that
  locks in the *new* undo behavior end-to-end (confirm → undo → invoice is
  fully open again, in every module, not just Agenda de Hoy).

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\CONFIRMED_INVOICE_LOGIC_AUDIT.md in
full before writing any code. This document is an audit and a recommended
direction, not a locked spec — §6 lists three product decisions that
genuinely need Mouse's input before you implement anything. Ask him about
those three explicitly, in your own words, before writing code. Do not
assume an answer and proceed.

Before touching R/ledger_module.R's df_combined(), re-verify every line
number cited in this document with a fresh read — it changes often and the
numbers here are a point-in-time snapshot from the audit date.

Once you have answers to §6, extract the four-source confirmed-matching
logic out of df_combined() into a reusable function in R/data_pipeline.R
per §5's recommended shape, wire it into df_combined() itself first (with
zero behavior change to the calendar — verify this before moving on), then
into build_export_combined_df() (R/cashflow_export_module.R /
R/cashflow_preview_module.R) and reporte_module.R's compute_pulse(). Decide
with Mouse, using §5's discussion, whether interco_module.R's existing
confirmed-exclusion (R/interco_module.R, commit 7d1478d) should be migrated
onto the same canonical function in this pass or left as a follow-up —
either is reasonable, but say which you're doing and why.

Work in small, reviewable increments, verifying the calendar's own behavior
is unchanged after each step before moving to the next module. Write the
tests described in this document's §7. Report back file-by-file. Do not
merge to master yourself.
```
