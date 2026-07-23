# Master Plan — Ledger Integrity (Confirmed-Status Unification + Agenda/Calendario Wiring)

**Status: consolidated staged implementation plan. Supersedes the separate
stage lists in `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` and
`docs/AGENDA_CALENDARIO_WIRING_AUDIT.md` — read this document for stage
order and prompts; read those two for full technical evidence (exact
current-code citations, incident details, the reasoning behind each
decision) whenever a stage's own instructions say to re-verify something.**

## Why one plan, not two

These started as separate efforts — unifying six independent "is this
invoice confirmed" implementations, and fixing how Agenda de Hoy relates to
Calendario — but investigating the second one surfaced that they share code
(`df_combined()`'s confirmed-matching logic, `R/pagar_hoy_module.R`'s confirm
handlers) and one direct sequencing dependency: the Agenda/Calendario fix
**removes a source** (`pagar_hoy_db.status=="confirmed"` matching) from the
confirmed-matching model before the other effort extracts that logic into a
shared function. Doing the extraction first would mean redoing it. Mouse
confirmed (2026-07-23): consolidate into one plan, ensure nothing from
either audit is lost, add heavy test coverage, work in small focused stages,
report and get a manual-verification checklist confirmed after each one
before starting the next, never merge to master without review.

## The unifying principle, stated once

**Calendario is the single root of truth.** Every other module — Agenda de
Hoy, Vencidos, Intercompany, Cash Flow Preview/Export, Reporte's Cash Flow
Pulse — is a pure mirror: it reads Calendario's own data, never holds
independent state, and any edit/delete/confirm action writes only to
Calendario's own source tables (the SAP/ERP snapshot, `manual_inv`). Mirrors
update automatically by re-reading the root, not via separately-wired
signals — if a mirror needs a code change to "notice" a root-level change,
that mirror has a bug.

Three item types, three rules:
- **ERP-sourced**: full lifecycle like manual (staged, edited, viewed in
  Vencidos), but no in-app action may ever remove it from Calendario — only
  the ERP's own snapshot update can. Confirming turns it into a **ghost**:
  crossed out, and excluded from every calculation everywhere (day totals,
  day-view sums, selections, Vencidos, Intercompany, Cash Flow
  Preview/Export, Reporte Pulse) — visible only as struck-through in
  Calendario's own box/day-view display, nowhere else. Agenda-level removal
  (confirm or Quitar) is always safe/benign for ERP, since Agenda never
  holds the real data.
- **Manual entries**: full lifecycle, but confirming or deleting removes the
  item from Calendario entirely (not ghosted) — and from Agenda, and from
  everywhere else *by consequence* of no longer existing at the root, not
  because each module was separately wired to react. The item survives only
  in a confirmation-history or deletion-history record, fully restorable.
- **Provisions**: never themselves enter Agenda, under any circumstance. A
  provision is converted (explicit user action) into a real, separate
  manual invoice (new id, linked back via `provision_id`); that derived
  invoice follows the manual-entry rules above. Recovering (undoing) a
  provision-derived confirmation must fully restore it, including its
  connection to the liability that generated it if applicable.

## Stage sequence

Each stage: branch off `master` (or the prior stage's branch if it hasn't
merged yet and touches the same files — check with Mouse), work in small
reviewable increments, run the existing suite plus new tests for the stage,
report pass/fail, ask about anything uncertain rather than guess, end with a
concrete manual-verification checklist, do not merge yourself.

### Stage 1 — Retire `conciliacion_rv` ✅ DONE, awaiting validation

Branch `confirmed-logic-stage-a`, commit `d8cae04`, not yet merged. Source:
`docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md`. **Do not start Stage 2 until Mouse
has confirmed this one works** (validation was in progress when the Agenda
incident in Stage 2's own audit was discovered — circle back to it).

### Stage 2 — `is_erp_sourced()` helper

Source: `docs/AGENDA_CALENDARIO_WIRING_AUDIT.md` §2.2/§3.3. Define one
shared helper, replace all ~10 hardcoded `is.na(source) | source == "sap"`
sites (`R/pagar_hoy_module.R`, `R/ledger_module.R`, `R/data_pipeline.R`) with
calls to it. Mechanical, low-risk — good first stage after Stage 1 lands.
Test: a second synthetic `source` value behaves identically to `"sap"`
through the helper at every site.

### Stage 3 — Unified archive mechanism, as an append-only event log

Source: §3.2, **extended 2026-07-23** per Mouse's explicit request while
testing Stage 1: a single mutable row with a soft-delete flag isn't enough —
he wants a real audit trail. Concretely, on top of the original design
(generalize `papelera`'s full-row-preservation shape with a `disposition`
column, one shared restore function):

- **Every confirm/delete/recover is its own permanent, immutable event
  record** — recovering an item must write a NEW line documenting the
  recovery (who, when), not just flip a flag on the original record in
  place. Never delete or overwrite an event row once written.
- **Each recovery event carries an explicit FK back to the specific
  confirm/delete event it reverses** (not just "this invoice, generically" —
  the exact event), so an item confirmed → recovered → confirmed again →
  recovered again produces a fully traceable chain, not an ambiguous pile of
  same-invoice rows. This is what makes "how many times was this recovered"
  and "was it eventually confirmed or deleted again" answerable later.
- Both Historial de confirmaciones and Papelera need this — same mechanism,
  applied to both dispositions, per Mouse's original "unify, standardize"
  instruction. This doesn't change *what* Stage 4/5 wire into it, just the
  shape of the mechanism itself — worth designing this properly now since
  every later stage builds on it.

Build and test this mechanism in isolation first — nothing calls it yet.
Exact schema fields: re-verify `papelera`'s current schema fresh before
extending it (don't trust either audit doc's field list without a fresh
read).

### Stage 4 — Confirm/undo rewrite: ERP + plain manual entries

Source: §3.1, §3.2 (non-provision part). Two independent-but-related fixes,
landed together since both touch the same confirm/undo handlers:

- ERP-sourced confirm now always fully unstages the `pagar_hoy_db` row
  (never leaves `status=="confirmed"` behind). `undo_conf`'s ERP branch
  stops touching `pagar_hoy_db` at all — only clears the `bancos_confirmados`
  flag, relying on that alone to make the item reappear.
- Plain manual-entry confirm archives the `manual_inv` row (Stage 3's
  mechanism, `disposition="confirmed"`) instead of deleting it; undo
  restores it verbatim. The existing explicit-delete ("eliminar") paths
  migrate onto the same mechanism (`disposition="deleted"`) — regression
  test against papelera's current behavior to confirm no change in what
  those paths already do correctly.

Verify: `df_combined()`'s `pagar_hoy_db.status=="confirmed"` matching
(Source 3, post-Stage-1 numbering) never matches anything anymore — confirm
this with a test, but leave the dead code removal for Stage 9 (it's about
to be extracted/rewritten there anyway; removing it twice is wasted work).

### Stage 5 — Confirm/undo rewrite: provision-derived rows

Source: §2.9, §3.5, provision part of §3.2. Mouse's explicit rule:
confirmation-history covers *everything* that gets confirmed, no
special-casing by origin — extend Stage 4's archive mechanism to
provision-derived `manual_inv`/`pagar_hoy` rows too. Separately (same
stage, since both are provision-specific): fix the confirmed, real bug
where `undo_conf` and `pasivos_observers.R`'s reversal watcher both react to
the same event with no coordination, leaving a stray orphaned `pagar_hoy_db`
row behind — `undo_conf` must skip its own Agenda-restore logic entirely
when `provision_id` is non-NA. Verify end-to-end: confirm a provision-derived
invoice → undo → provision is `"provisional"` again, zero `pagar_hoy_db`
trace, `liability_id` connection intact (already proven correct in the
audit, but lock it in with a regression test since this stage touches the
surrounding code).

### Stage 6 — Fix "send straight to Agenda", keep the feature

Source: §2.10, §3.4. Both the Pasivos conversion modal's `stage_to_agenda`
option and the direct-manual-entry "send to agenda" checkbox currently
fabricate a `pagar_hoy` row from raw form input instead of staging the row
that was actually just written to `manual_inv`. Mouse wants this one-click
convenience kept — rework both to: write the root row first, then stage it
via the same primitive Calendar/Search staging already uses. Test:
construct a case where the fabricated row would have differed from the real
one, to make the "staged from the real row" assertion meaningful.

### Stage 7 — Ghosts must not affect any calculation, inside Calendario's own UI

Source: §2.11, §3.6, §3.7. Three related fixes: make ghost rows unselectable
in the day-modal `DT` table (matching Vencidos' existing
`pointer-events:none` precedent — Mouse's call: pick whichever is more
standard UI practice, unselectable is the more common pattern for
already-settled line items and is now the direction); this structurally
also fixes the "Selección" subtotal counting ghosts; add the hourglass
consistency guard (never show the staged-count badge over a day with zero
visible line items).

**Added 2026-07-23, unrelated bug found while reproducing the above**: in
Agenda de Hoy, removing ("Quitar") the last pending item in an
empresa+currency bucket sometimes leaves the `DT` table showing the
now-removed row while the header/count and action buttons correctly go to
zero (reproduced once, self-corrected on a second stage+remove cycle in the
same session). Investigated the full server-side reactive chain — header,
buttons, and table all read the identical `staged()`/`shared$pagar_hoy_db()`
value with no caching, `isolate()`, `dataTableProxy`, or debounce difference
between them, which rules out every R-side explanation. This looks like a
client-side DT/htmlwidgets redraw quirk at the 1-row-to-0-row transition,
not a logic bug — try forcing a full table re-initialization on that
specific transition (rather than trusting DT's incremental redraw) as the
fix, and confirm live in a browser since this class of bug won't show up in
an automated R test.

### Stage 8 — Vincular dedup warning dialog

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §6.2 (Stage B in that
document's original numbering). Add an explicit confirm-or-cancel dialog
before Vincular's duplicate-merge modal is allowed to silently un-confirm an
invoice, naming the affected invoice. No dependency on Stages 2-7; could run
in parallel if useful, sequenced here for narrative continuity.

### Stage 9 — Extract `compute_confirmed_flags()` — now a 2-source model

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage C), **revised**
per this plan's own §"Why one plan" and `AGENDA_CALENDARIO_WIRING_AUDIT.md`
§3.1/§3.8: by this point Source 3 (`pagar_hoy_db.status=="confirmed"`) is
permanently dead (Stage 4 removed every path that could produce it) — the
canonical function only needs **two** sources, `bancos_confirmados` matching
and papelera SAP-ghosts. Extract into `R/data_pipeline.R`, wire into
`df_combined()` with zero further behavior change to the calendar (verify
this explicitly), following the pure-function signature convention already
established (plain data frames in, no `shared` reactive access inside
`data_pipeline.R`).

### Stage 10 — Amount-match guard

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage D). Standard
everywhere (bancos_confirmados matching only), no date-window check —
Mouse's explicit reasoning: a date guard would treat normal month-end SAP
delays as staleness and reopen valid confirmations. Test the two named edge
cases: a USD invoice re-snapshotted after confirmation, and an abono applied
around confirmation time (match against the amount captured *at
confirmation time*, not a live-recomputed balance — resolve carefully per
the original audit's design note, don't copy Intercompany's netted-balance
approach verbatim).

### Stage 11 — Wire canonical function into Cash Flow Preview/Export

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage E).

### Stage 12 — Wire canonical function into Reporte's Cash Flow Pulse

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage F).

### Stage 13 — Migrate Intercompany onto the canonical function

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage G). Retires
Intercompany's standalone 5th independent confirmed-check implementation —
by this point it inherits the amount-match guard automatically (Stage 10)
and picks up the two sources it was previously missing (papelera ghosts,
and correctly does not reintroduce the retired `conciliacion_rv`).

### Stage 14 — Concurrency (open scope call — a real, if minor, incident happened 2026-07-23)

Source: `AGENDA_CALENDARIO_WIRING_AUDIT.md` §2.7/§4.4. The
`pagar_hoy_db`/`manual_inv` read-modify-write race (no version check, any
user can Quitar, `manual_inv` isn't even in the cross-session sync
registry) is real but systemic — broader than either audit's original
scope.

**Update, same day**: a `bancos_confirmados` row Mouse confirmed during
testing (a $0.02 "prueba" entry) disappeared entirely between two other
confirmations of the same test invoice. No dedup/upsert bug was found
anywhere in the write path (`do_confirm_ap_<emp>`, `save_bancos_confirmados`,
`.normalize()` — all pure-append, verified read-only) — the most plausible
explanation is exactly this class of race (a second session/tab's stale
read-modify-write silently overwriting the first's newer data), not yet
confirmed (need to know whether more than one browser tab/session was open
during the test). If confirmed, this is no longer a hypothetical systemic
risk — it already destroyed a real row today, and this stage's "deferred by
default" framing needs to be revisited with Mouse rather than assumed.

## Open items needing Mouse's input before or during the relevant stage

- Invoice `1025618` (`AGENDA_CALENDARIO_WIRING_AUDIT.md` §1) — re-enter
  manually with surviving fields, or is there another record for the real
  due date? (Not a code stage — a data action Mouse takes himself.)
- 8-second cross-session poll interval for the hourglass/Agenda badge —
  acceptable, or tighten? (Relevant to Stage 7.)
- Should `bancos_confirmados` gain a `source` column for at-a-glance
  ERP-vs-local audit visibility? Low-priority, could fold into Stage 2 or
  skip.
- Stage 14's scope call.
- **Not yet examined against the 3-type framework**: abono rows (partial-
  payment records that also live in `pagar_hoy_db`, removed on confirm
  alongside manual/provision rows per `AGENDA_CALENDARIO_WIRING_AUDIT.md`
  §2.3). Their real data lives in `abonos_db`, a separate table from
  `manual_inv`, so they're likely already safe by the same reasoning as ERP
  rows (Agenda losing the reference doesn't destroy the source data) — but
  Mouse's ERP/manual/provision framework didn't mention abonos explicitly,
  and this was not independently verified. Check during Stage 4 rather than
  assuming; flag to Mouse if it turns out abonos need their own fix.

## Completeness check (self-review before presenting)

Cross-checked every numbered finding/decision in both source audits against
the stage list above:
`CONFIRMED_INVOICE_LOGIC_AUDIT.md` §§1-7 → Stages 1, 8-13, all covered.
`AGENDA_CALENDARIO_WIRING_AUDIT.md` §§1-5 → Stages 2-7 cover every numbered
item in its §3 except §3.9 (concurrency, deliberately deferred as Stage 14)
and the four §4 open items (listed above, not code stages). §2.8
(Vencidos already clean) and §2.12 (ERP delete-already-only-ghosts) require
no stage — confirmed-correct, included above only for completeness, not
action.
