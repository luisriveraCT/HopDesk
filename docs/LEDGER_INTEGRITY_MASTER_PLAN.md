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

**Testing bar, confirmed with Mouse 2026-07-23**: "around 100 quick tests
per stage, true thorough." Table-driven/parameterized coverage (many field
checks, many source/status/edge-case combinations) rather than a handful of
hand-picked examples — Stage 3 landed 79 genuinely distinct assertions this
way, not padding. Apply this bar from here on; Stages 1-2's lighter suites
(8 and 14 tests) predate this instruction and weren't backfilled unless
Mouse asks.

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
- **Both histories are permanent in S3 forever** — confirmed explicitly by
  Mouse, 2026-07-23, for auditory-compliance reasons: "at least a simple log
  with the details of what happened, who did it, timestamp, whatever's
  needed for auditory compliance." No event row, once written, is ever
  physically deleted or edited by any code path — this matches papelera's
  own existing documented behavior ("se conservan permanentemente para
  auditoría") almost exactly; the fix is extending that same guarantee to
  cover confirmations too, not inventing a new policy.

Build and test this mechanism in isolation first — nothing calls it yet.
Exact schema fields: re-verify `papelera`'s current schema fresh before
extending it (don't trust either audit doc's field list without a fresh
read).

**✅ DONE 2026-07-23** — branch `confirmed-logic-stage-2` (stacked; touches
`R/persistence.R`/`R/bancos_persistence.R`, unrelated to Stage 2's own
files but the branch wasn't merged yet). Purely additive: `.schema_papelera()`
gained `event_id`/`disposition`/`action`/`reverses_event_id`; `add_to_papelera()`
gained a `disposition` param (defaults to `"deleted"`, so every existing
call site is byte-for-byte unchanged); new `restore_from_papelera()`.
`.schema_bancos_confirmados()` gained `action`/`reverses_confirmacion_id`/
`recovered_at`; new `recover_confirmacion()`. 79 new tests (zero-field-loss
round trips across every real manual-invoice field, multi-cycle chains,
error conditions, backward compatibility) — full existing suite still
green. Nothing in production code calls either new function yet.

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

**Correction, confirmed with Mouse 2026-07-23**: the "can't Quitar/re-stage
a confirmed SAP row, solo SAP puede cerrarlos" guards (`pagar_hoy_module.R`'s
two Quitar handlers, `ledger_module.R`'s two stage_all/stage_selected
collision guards — the exact sites Stage 2 migrated onto `is_erp_sourced()`)
are based on the WRONG premise. Mouse's rule: users may remove *anything*
from Agenda regardless of source — the only real guardrail is that no
in-app action may remove an ERP row from **Calendario** (already correct,
confirmed separately, §2.12). **Delete these four guard blocks entirely as
part of this stage** — don't just leave them calling `is_erp_sourced()`,
remove the restriction itself. (Stage 2's helper migration itself was still
correct, low-risk, mechanical work — only the business rule these four
specific call sites implement turned out to be wrong, not the refactor.)

Verify: `df_combined()`'s `pagar_hoy_db.status=="confirmed"` matching
(Source 3, post-Stage-1 numbering) never matches anything anymore — confirm
this with a test, but leave the dead code removal for Stage 9 (it's about
to be extracted/rewritten there anyway; removing it twice is wasted work).

**✅ DONE 2026-07-23** — branch `confirmed-logic-stage-2` (stacked, same
files as Stages 2-3). All four guards deleted. Confirm now always removes
the `pagar_hoy_db` row (every source); plain manual entries archive via
Stage 3's mechanism with a new `bancos_confirmados.archive_event_id` link
so `undo_conf` can find and restore the exact right archived row; `undo_conf`
now branches on provision/archived-manual/neither and calls
`recover_confirmacion()`/`restore_from_papelera()` accordingly. Found and
fixed along the way: entries staged via Calendar/Search's "Stage
all"/"selected" mint a fresh `pagar_hoy` id decoupled from `manual_inv`'s
own id (only the direct-creation "send to agenda" path shares it) —
id-match alone silently missed archiving these; added a business-key
fallback. Also fixed a pre-existing quirk where a mixed provision+manual
confirm batch only processed one type. The explicit-delete ("eliminar")
paths were deliberately left untouched — they already archive correctly
with `disposition="deleted"` and `restore_from_papelera()` already supports
restoring them, but no UI to trigger that restore was built in this stage
(wasn't in scope; only confirm/undo was). 59 new tests (static scan + full
confirm-then-undo integration simulations covering both id-matching cases,
ERP's no-touch path, and multi-cycle chains) — 160 total in this suite,
full existing suite still green.

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

**✅ DONE 2026-07-23** — branch `confirmed-logic-stage-2` (stacked, same
files as Stages 2-4). Provision-derived rows now archive at confirm time,
same as plain manual (2 more `add_to_papelera(..., disposition="confirmed")`
sites, 4 total). `undo_conf`'s `has_provision` branch now does nothing
beyond the unconditional `recover_confirmacion()` call — no `pagar_hoy_db`,
no `manual_inv` — deferring entirely to `pasivos_observers.R`'s existing
revival watcher, which needed no code change at all (its own `manual_inv`
cleanup step naturally degrades into a safe no-op once the row is already
archived rather than hard-deleted). The archived copy is never
auto-restored on undo — that would duplicate the provision's own revived
placeholder — it just stays as a permanent record, which is correct since
the provision's own `estado`/FK columns (never touched, confirmed already)
are the real undo mechanism for this case. 24 new tests, including a
comment-vs-code-aware static scan proving the branch's actual code (not
its explanatory comment) never references `pagar_hoy_db`/`manual_inv`.
184 total in this suite, full existing suite still green.

**Correction, same day (2026-07-23)**: the design above was wrong.
Mouse's actual rule: recovering a CONFIRMED item always recovers the
ITEM, regardless of provision lineage — a provision itself is never
confirmed (it can never reach Agenda), so `provision_id` on a confirmation
is pure traceability metadata, not a branch condition. Deleting an
unconverted PROVISION is a different operation entirely (Pasivos-level,
unrelated to Agenda/bancos_confirmados) that returns the provision — not
something undo_conf needs to handle. Fixed: `undo_conf()` no longer
branches on `provision_id` at all; every archived confirmation restores
through the single `has_archive` path. This surfaced a real, necessary
second fix: `pasivos_observers.R`'s reversal watcher was independently
reacting to the same event, reviving the provision and re-deleting the
item undo_conf() had just restored — removed that reversal-detection
block entirely (confirmation-detection is untouched, unaffected). **Open
question, not yet resolved**: the provision's own `estado` now stays at
"item_confirmed" after its item's confirmation is undone, since nothing
reverts it — whether that should transition to something else (not
"provisional" — the item still exists) is a real product decision.
Tests updated to match (181 total, still green).

Also added, per Mouse's explicit request: `R/global.R`'s `is_erp_sourced()`
now carries a full philosophy reference for how ERP/manual/provision-
derived rows differ and where they're alike — read that comment block
first when touching any of this again.

### Stage 5B — Search module: convert to a live reactive mirror (open question, not started)

Source: Mouse's extension request, 2026-07-23, prompted by confirming
Vencidos is already a pure mirror — verify the same holds for Search.

**Audited, mixed result.** The *mutation* side is already fully compliant:
every edit/tag/move/restore/delete/stage action in `R/search_module.R`
routes through the exact same shared `handle_invoice_action()` function
Vencidos calls, writing only to Calendario's root tables (`tags_db`,
`moves_db`, `papelera_rv`, `manual_inv`, `pagar_hoy_db`,
`pasivos_provisions_db`) — no Search-only reactiveVal holding data exists.
`stage_all`/`stage_selected` are the identical code path already audited
for Calendar/Vencidos, not a Search-specific duplicate.

The *display* side is genuinely different in kind, not just less
thorough: Search's table is a **one-shot static HTML string** built once
per `show_search_modal()` call and shipped via `showModal()` — not a live
Shiny render. While the modal is open, edits are reflected via a
client-side JS "optimistic" DOM patch (`searchApplyUpdate`,
`search_module.R:810-858`) that guesses the new state from what was *sent*
to the server, not from the server's *actual* response — and the modal
does not auto-refresh at all if root data changes elsewhere while it's
open (closing and reopening via `btn_search` is the only way to re-pull
from `df_combined_*()`). Vencidos has no client-side shadow-state layer at
all — it's a genuine `moduleServer()`/`reactive()`/`renderUI()` mirror
that self-heals live.

Fixing this properly means converting Search's ~1000-line modal into a
real reactive component (module server + `reactive()` + `renderUI()`/`DT`)
and removing the now-redundant `searchApplyUpdate` JS layer — a
meaningfully larger, riskier change than anything else in this plan so
far, to a file with deep JS/server interplay. **Not started — needs an
explicit go-ahead given the size, and ideally its own dedicated,
heavily-tested stage rather than being folded into Stage 6/7.**

### Stage 6 — Fix "send straight to Agenda", keep the feature

Source: §2.10, §3.4. Both the Pasivos conversion modal's `stage_to_agenda`
option and the direct-manual-entry "send to agenda" checkbox currently
fabricate a `pagar_hoy` row from raw form input instead of staging the row
that was actually just written to `manual_inv`. Mouse wants this one-click
convenience kept — rework both to: write the root row first, then stage it
via the same primitive Calendar/Search staging already uses. Test:
construct a case where the fabricated row would have differed from the real
one, to make the "staged from the real row" assertion meaningful.

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked, same
branch as Stages 2-5). New `stage_manual_row_to_agenda()`
(`R/data_pipeline.R`) derives every `pagar_hoy` field from a `manual_inv`
row passed in directly — no `shared`/reactive access, matching the file's
existing pure-function convention. Both call sites now re-read the row by
id from the data frame that was just bound/saved (`manual_df`/`df`) and
pass it through, instead of touching `input$...` a second time:
Pasivos keeps minting its own fresh `pagar_hoy` id (decoupled from
`manual_inv`'s id, its existing convention — overridden back after the
helper call, which defaults to sharing the id per §2.1) and still passes
`liability_id`/`source="provision"` correctly since those come through
automatically once `provision_id` is set on the row. Direct manual-entry
keeps sharing the id (the helper's default). 42 new tests: unit coverage
on the new helper (field mapping, plain-manual vs provision-derived source
inference, blank-string handling), a divergence test constructing a row
whose stored Empresa deliberately differs from what naive raw-input
reconstruction would have produced (proving real re-derivation, not a
tautology), and a static scan confirming both call sites call the helper
and no longer reference `input$pcm_*`/`input$me_*` inside the staging
block. 223 total in this suite, full existing suite still green (a
pre-existing, unrelated failure in `test_pasivos_stage2.R`'s Group 3 —
`save_pasivos_liabilities()` called with an unsupported `client_id` arg —
was confirmed via a clean-checkout re-run to predate this stage).

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

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked).
`vin_keep_a` (the only risky path — `vin_keep_b` already only ever *keeps* a
confirmado candidate, never discards one) now checks the discarded
candidate's source before doing anything: if `"confirmado"`, shows a warning
dialog naming the invoice (Parte/Documento/Importe/Moneda/Fecha) with
Cancelar + a separate confirm button; the actual mutation (unchanged,
extracted into `.do_vin_keep_a()`) only runs after that confirm. Also
captured `moneda` on the confirmado candidate struct
(`.build_candidates_bank_side()`), missing before. 20 new tests (branch-
decision logic, per the stage's own note that the modal itself isn't
meaningfully unit-testable).

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

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked).
Extracted `compute_confirmed_flags(df, ledger, bancos_confirmados_df,
papelera_df)` into `R/data_pipeline.R`; `df_combined()` now calls it instead
of the ~110-line inline 3-source block. Dead Source 3 dropped entirely, not
left as unused code. 22 new tests, including a frozen snapshot of the exact
pre-extraction 3-source logic run against identical synthetic input and
asserted byte-for-byte identical to the new function's output — the "zero
further behavior change" contract made concrete.

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

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked). Source 1
now also requires the amount to match, using the same 2-decimal-rounded-
string key shape already proven in `interco_module.R`'s `.ckey()`. Matched
against `Saldo_original` (the balance *before* the current render's
abono-netting), not `Importe` — SAP rows have no such column at all — and
not the live `Saldo vencido`, resolving the design note's open question
correctly. Source 2 (papelera ghosts) deliberately left unguarded — a
discrete, immediate, single-invoice action, not the long-lived DocNum-reuse
risk this guard targets. 12 new tests covering both named edge cases plus
the DocNum-reuse case the guard exists to catch.

### Stage 11 — Wire canonical function into Cash Flow Preview/Export

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage E).

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked).
`build_export_combined_df()` now calls `compute_confirmed_flags()` on each
ledger and drops confirmed rows entirely (including SAP ghosts — an
export/preview has no visual-ghosting concept, unlike the calendar's
day-modal). `cashflow_preview_module.R` calls this exact same function, so
one fix covers both the live preview panel and the Word/Excel export
(confirmed by grep). 7 new tests, including an integration test with a mock
`shared` and a real confirmed/open invoice pair.

### Stage 12 — Wire canonical function into Reporte's Cash Flow Pulse

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage F).

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked). Design
choice reasoned through per this stage's own instruction: `compute_pulse()`'s
`normalize_sap()` builds its own deliberately SAP-only frame (no `source`
column at all), bypassing `build_ledger_df()`/`df_combined()` entirely —
routing it through the full pipeline just to reach the canonical function
would be a much bigger, riskier change than calling
`compute_confirmed_flags()` directly against `ar_all`/`ap_all` as they
already are. That exposed a real, latent bug in `compute_confirmed_flags()`
itself: when `source` is entirely absent (not just NA-valued, as every
prior caller always had it), `is_manual`/`is_provision` silently collapsed
to `logical(0)` instead of all-FALSE, breaking every downstream mask
recycling. Fixed generally in the shared function, not worked around per
caller. 8 new Stage 12 tests plus 2 regression tests added to Stage 9's
suite for the missing-source-column case directly.

### Stage 13 — Migrate Intercompany onto the canonical function

Source: `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` §5/§7 (Stage G). Retires
Intercompany's standalone 5th independent confirmed-check implementation —
by this point it inherits the amount-match guard automatically (Stage 10)
and picks up the two sources it was previously missing (papelera ghosts,
and correctly does not reintroduce the retired `conciliacion_rv`).

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked, final
stage of the master plan's confirmed-logic unification). `.filter_ic()`'s
own `.ckey()` + dead-Source-3 implementation retired entirely in favor of
`compute_confirmed_flags()`, called against the raw SAP snapshot data it
already works with. The pre-netting amount is now captured into
`Saldo_original` *before* the abono-netting mutation runs, fixing a latent
bug in the old `.ckey()` approach (which matched the netted balance) as a
side effect of the migration, not just a refactor. 12 new tests, including
the concrete regression this stage exists to fix: an invoice confirmed ONLY
via a papelera ghost (never in `bancos_confirmados`) now correctly
disappears from Intercompany too.

**Master plan status**: Stages 1, 8-13 (the confirmed-invoice-logic
unification, `CONFIRMED_INVOICE_LOGIC_AUDIT.md`'s original scope) are
complete — every consumer (calendar, Vencidos, Agenda de Hoy, Cash Flow
Preview/Export, Reporte's Pulse, Intercompany) now reads confirmed/ghost
status from the one canonical `compute_confirmed_flags()`. Stages 2-7
(`AGENDA_CALENDARIO_WIRING_AUDIT.md`'s scope — ERP/manual/provision
architecture, the archive mechanism, ghost isolation) are also complete.
Stage 14 (concurrency) is partially addressed — see its own section above
for exactly what's closed and what remains deliberately deferred.

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

**✅ Confirmed and partially fixed, 2026-07-24** — branch
`confirmed-logic-stage-2` (stacked). The suspicion above was confirmed: a
separate incident that same day (a live bug in `stage_all`/`stage_sel`/
`handle_invoice_action` losing the `source` field on staged Agenda rows,
fixed separately) required repairing 12 `manual_inv` rows directly against
S3. An already-open tab kept showing the pre-repair rows and re-confirming
them had no visible effect — because `manual_inv` was the *only* shared
table completely absent from the cross-session sync registry
(`R/sync_bus.R`), so a stale tab's in-memory copy never self-heals like
every other table already does every 8 seconds. A second, independent gap
made even registering it insufficient: every loader checks a
process-global preload cache before touching S3, cleared only by that
process's own writes — so a poll-triggered reload would still serve stale
data even after a correct version bump, silently affecting all
already-registered keys too, not just `manual_inv`.

Fixed: `manual_inv` registered in the sync bus; `save_manual()` bumps its
version on every write; the sync bus's reload loop now clears the
preload-cache entry for whichever key it's about to reload, before
calling its loader (closes the gap for every registered key). The six
highest-stakes `manual_inv` archive/confirm/undo/delete read sites
(`do_confirm_ap_<emp>`/`do_confirm_ar_<emp>`, the Quitar-cascade,
`undo_conf`, `confirm_delete`, `handle_invoice_action`'s delete branch,
the Pasivos conversion write) now read fresh from S3 instead of trusting
the in-memory reactiveVal, mirroring a pattern two other `pasivos_module.R`
handlers already used safely. 26 new tests, 276 total in the
confirmed-logic suite, full existing suite still green.

**Still deferred, unchanged from the original scope call**: a general
optimistic-locking/version-check system across all shared tables (`.s3_write()`
remains an unconditional full-object overwrite everywhere, no ETag/version
check anywhere), and the residual ~8-second window where two tabs write to
the same key inside one poll interval. `pagar_hoy_db` itself was already
registered before this fix and still has the same class of naive
read-modify-write at its own ~19 write sites — only `manual_inv`'s highest-
stakes sites were hardened this pass, not every site for every table.

### Stage 14b — `cart_inv_click` and two more staging sites still lost `source` (found 2026-07-24)

Source: a fresh live recurrence of Stage 6's original bug, reported the
same day, *after* Stage 6's fix and the app restart — ruling out a stale
process. A manual invoice ("test"/Networks Trucking Services, $1,000)
was confirmed via Agenda de Hoy (`bancos_confirmados` written correctly,
unstaged correctly) but never archived out of `manual_inv` — identical
symptom, different cause.

**✅ DONE 2026-07-24** — branch `confirmed-logic-stage-2` (stacked).
Root-caused (independently corroborated by two parallel investigations —
one tracing every `pagar_hoy_db`-row-construction site, one auditing
`do_confirm_ap_<emp>`'s archive-matching logic end to end, explicitly
ruling out the `mover_fecha` date-move, the Parte-edit path, and Stage
14's `load_manual()` concurrency fix as causes) to `R/ledger_module.R`'s
`cart_inv_click` observer — the per-individual-invoice "+" toggle shown
when a Parte group is expanded in the calendar day-modal's cart, distinct
from the already-fixed group-level `cart_<i>` button and `stage_all`/
`stage_sel`. It had the row's real `source` available but hardcoded
`source = "sap"` unconditionally, never consulting it — missed by Stage
6's fix because it's a fourth, separate staging entry point.

Fixed with the same `ifelse(is.na(source) | source != "manual", "sap",
"manual")` normalization used everywhere else. Also hardened two more
sites that never set `source` at all — `.ic_send_rows()`
(`R/interco_module.R`) and `send_to_agenda` (`R/treasury_map_module.R`)
— not implicated in this incident (both are fed only by SAP/intercompany
snapshot data today) but closing the same gap defensively so a manual
row can never silently misclassify if either path is ever extended to
handle one. 8 new tests appended to `tests/test_stage_source_propagation.R`
(same file — same bug class/incident family), full suite still green
(393 total in the confirmed-logic suite, plus the broader `_run_saas`/
`_run_agenda`/`_run_s1`/`_run_s2`/`_run_s3`/`_run_cart`/`_run_tag` suites).

The specific stuck production row (`manual_inv` id
`984022c5-3a8d-416f-a21f-eed8472d975f`) was repaired directly against S3,
mirroring `do_confirm_ap_<emp>`'s own archive logic exactly (via the real
`add_to_papelera()` function, not a reimplementation): archived to
`papelera` with `disposition="confirmed"`, and `archive_event_id`
backfilled on both of its `bancos_confirmados` rows (the user had
re-staged and re-confirmed it a second time after the first confirm
appeared to have no effect on the calendar — expected, given the bug).

**This raises a real question the earlier "12 stuck invoices" repair
didn't have to answer**: `cart_inv_click` was *not* covered by Stage 6's
audit or its static tests at the time, meaning there is no verification
step yet that exhaustively enumerates *every* `pagar_hoy_db`-row-
construction site as a set and asserts each one sets `source` correctly
— today's fix (and Stage 6's) each found the culprit by targeted
investigation of one incident, not by that kind of exhaustive check.
Worth flagging to Mouse as an open item (see below) rather than assuming
this is now provably the last one.

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
- Whether it's worth writing one exhaustive test that enumerates every
  `pagar_hoy_db`-row-construction call site (grep-based, so it can't miss
  a future one either) and asserts each sets `source` correctly, rather
  than relying on incident-by-incident discovery as happened for both
  Stage 6 and Stage 14b.
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
