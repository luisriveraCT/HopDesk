# Audit — Agenda de Hoy ↔ Calendario Wiring (Staging, Confirm, Undo, Hourglass)

**Status: audit + confirmed incident report + recommended direction. No code
changes were made as part of this document.** Written for a separate
implementing session to pick up, in the same spirit as
`docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md` and `docs/saas_rebuild/`'s
`STAGE_N_*.md` documents.

## 0. How this document came to exist

While validating Stage A of the confirmed-invoice-logic unification (see
`docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md`), Mouse tested the fixed "Deshacer
confirmación" flow on a real invoice (Hapag Lloyd, doc `1025618`, Networks
Crossdocking Services, AP, USD 532). The undo worked in the narrow sense
Stage A targeted (no longer stuck confirmed forever), but the invoice
reappeared in **Agenda de Hoy**, not purely in **Calendario**. Mouse's stated
architecture, confirmed here as the target design:

> Calendario is the single source of truth for open items. Agenda de Hoy is a
> disposable, temporary mirror — it stages a subset of Calendario's items for
> today's payment run, and nothing else. It must never independently
> "produce" or store information; if an item exists in Agenda, it must also
> exist in Calendario. Confirming an item removes it from Agenda (sends it to
> confirmation history) and, in the background, either crosses it out in
> Calendario (ERP-sourced item — any ERP, not just SAP) or removes it from
> Calendario entirely (manual entry) — Vencidos then reflects whichever of
> those two things happened automatically, since it reads Calendario's own
> `confirmed` column.

Investigating why undo landed the invoice in Agenda surfaced a second,
materially worse bug: **for non-ERP-sourced invoices, confirming permanently
deletes the invoice's real data with no recovery path** — undo can only fake
a lossy stand-in in Agenda, and removing that stand-in (as Mouse did, testing
the "Quitar" button) erases the last trace entirely. This is confirmed to
have already happened to a real invoice — see §1.

## 1. Confirmed incident — invoice `1025618` is unrecoverable

Read-only diagnostic run against the live `networks` client data
(2026-07-23), cross-referencing `pagar_hoy_db`, `manual_inv`, and
`bancos_confirmados`:

| Table | Current state for doc `1025618` |
|---|---|
| `pagar_hoy_db` | **0 rows** (all statuses) |
| `manual_inv` | **0 rows** |
| `bancos_confirmados` | 1 row: `confirmacion_id=140b6c0c…`, `agenda_item_id=bee5766e…`, `empresa=Networks Crossdocking Services`, `parte=HAPAG LLOYD MEXICO SA DE CV`, `codigo=P1581`, `importe=532`, `moneda=USD`, `tipo=pago`, `fecha=2026-07-22`, `confirmado_at=2026-07-23 09:29:26`, `eliminado=TRUE`, `eliminado_at=2026-07-23 09:29:43`, `mov_id=NA`, `provision_id=NA` |

Reconstructed timeline, matching every mechanism in §2 below exactly:

1. Invoice was staged into Agenda with `source == "manual"` (not SAP — see
   §3 for why this matters; the confirmed record has no `mov_id`, consistent
   with a manual entry never linked to a real bank movement).
2. Confirmed via Agenda de Hoy at 09:29:26 → `bancos_confirmados` row
   written → because `source == "manual"`, both the `pagar_hoy_db` row
   (id `bee5766e…`) **and** the `manual_inv` row were deleted
   (`R/pagar_hoy_module.R`, §2.3) — no papelera archive, no soft-delete.
3. "Deshacer confirmación" clicked 17 seconds later (09:29:43) →
   `bancos_confirmados.eliminado <- TRUE` → `undo_conf` looked for
   `pagar_hoy_db$id == "bee5766e…"`, found nothing (deleted in step 2), so it
   **synthesized a brand-new `pagar_hoy_db` row** from `bancos_confirmados`
   fields — `FechaVenc` set to the *payment date* (2026-07-22, matching
   exactly what Mouse's screenshot showed as "Vencimiento: 22/07/2026" for
   all 4 Hapag Lloyd rows), not the invoice's real original due date, which
   is now unknowable.
4. Mouse clicked "Quitar" on this synthesized row during his own testing →
   `unstage_pagar_hoy()` deleted it → **zero trace of the invoice remains
   anywhere in the system**, except the summary fields captured in the
   `bancos_confirmados` row above (which is the only reason this incident is
   reconstructable at all).

**Only `bancos_confirmados.importe`, `moneda`, `documento`, `parte`,
`codigo`, `empresa`, and the payment date survive.** The invoice's real due
date, any notes, and its `Factura`/`Abono futuro` fields (if set) are gone.
Recommend Mouse re-enter this invoice manually using the fields above; there
is no code-level recovery possible.

**Blast radius check (read-only diagnostic, both registered clients,
`networks` and `hopdesk`):** no other `pagar_hoy_db` row today is a
`status=="pending"`, `source=="manual"` row with no matching `manual_inv`
row — i.e. no other *currently-visible* ghost stand-ins exist. This does
**not** rule out other past invoices having suffered the same fate as
`1025618` and then also being "Quitar"'d or otherwise cleared, leaving zero
trace — the diagnostic can only find ghosts that are still sitting in Agenda
today, not ones already fully erased. There is no way to retroactively
detect those from data alone.

## 2. Current mechanics, in full

### 2.1 Staging (Calendario/Search → Agenda)

Three entry points, all functionally identical — copy a reference into
`pagar_hoy_db` with `status = "pending"`, never touching the source data:

- Calendar day-modal "Stage all"/"Stage selection" — `R/ledger_module.R:957-993, 998-1048`.
- Search-tab "Stage all"/"Stage selected" — `R/search_module.R:1032-1074`.
- Direct manual-entry-with-send-to-agenda — `app.R:2449-2497`. **This path
  deliberately sets the new `pagar_hoy_db` row's `id` equal to the new
  `manual_inv` row's `id`** (`app.R`, `ph_row <- tibble::tibble(id = new_row$id, ...)`)
  — the same primary key is shared across two tables by design. This is the
  specific mechanism that lets the confirm handler's "direct id match"
  branch (§2.3) delete the exact right `manual_inv` row — it is not a bug in
  isolation, but it is the load-bearing fragility that makes the
  confirm-time delete possible at all for this path.

`upsert_pagar_hoy()` (`R/persistence.R:1053-1058`) only ever mutates
`pagar_hoy_db`. `df_combined()` never filters on `status == "pending"` (only
`"confirmed"` rows affect `confirmed`/`is_paid_ghost`, `R/ledger_module.R:357-382`
post-Stage-A) — **a staged item is always still fully visible in Calendario**,
today, already, matching half of Mouse's target design already. The other
half — Agenda ever "producing" an item not backed by a live Calendario row —
only happens via the confirm/undo mechanics below.

### 2.2 The `source` field and the "sap"-hardcoding risk

`.schema_pagar_hoy` (`R/persistence.R:919`): `source` is documented as
`"sap" | "manual" | "provision"`, with `NA`/blank normalized to `"sap"` on
every load (`R/persistence.R:926,968,983`). The idiom
`is.na(source) | source == "sap"` (treat "not explicitly manual/provision" as
ERP-safe) recurs, independently spelled out, in at least 10 places:
`R/pagar_hoy_module.R:1394,1597,1162,1223`, `R/ledger_module.R:340,1164,1267,1591`,
`R/data_pipeline.R:355-369`. There is **no scaffolding anywhere** for a
non-SAP ERP source value — `R/erp_connector_registry.R` is exclusively about
connection *credentials*, unrelated to this `source` column. Per Mouse's
explicit direction (§0), this needs to generalize: any future second ERP
integration would silently fall through every one of these ~10 sites'
`== "sap"` checks and be treated as "manual" — i.e. **destructively deleted
on confirm**, exactly like `1025618` — unless every site is found and
updated by hand. This is the same shape of bug as the SaaS rebuild's
`CLIENT_ID`-as-stand-in-for-identity anti-pattern
([[project_saas_architecture_audit]] — see the `log_action()` finding): one
concept (an ERP-safe vs. locally-owned row) spelled out independently in
many places, guaranteed to drift.

### 2.3 Confirm handlers — the destructive branch

`do_confirm_ap_*`/`do_confirm_ar_*` (`R/pagar_hoy_module.R:1329-1473, 1533-1676`):

- **ERP-sourced rows** (`source` is `NA` or `"sap"`): `pagar_hoy_db` row kept,
  `status <- "confirmed"` (never deleted). Nothing about the row is ever
  destroyed — consistent with treating a live ERP snapshot as data HopDesk
  doesn't own and shouldn't destroy.
- **Manual/provision rows and all abono rows**: `pagar_hoy_db` row is
  **physically deleted** (`unstage_pagar_hoy`, anti-join by `id`) — no
  papelera archive. The corresponding `manual_inv` row is **also physically
  deleted** (`R/pagar_hoy_module.R:1435-1454, 1638-1657`) — also no papelera
  archive.

This is a real, structural inconsistency with how deletion works everywhere
*else* in this app. Both other manual-delete paths —
the calendar day-modal's own "eliminar" (`R/ledger_module.R:1754-1774`) and
the Search tab's bulk "eliminar" (`R/search_module.R:1006-1027`) — **always**
call `add_to_papelera()` (preserving the complete original row via
`original_data`) *before* calling `delete_manual()`. The confirm handler is
the **only** place in the codebase that deletes a `manual_inv` row without
first archiving it. `delete_manual()` itself (`R/persistence.R:709-711`) is
a bare unguarded filter — it is only safe elsewhere because every other
caller archives first.

`bancos_confirmados`'s schema (`R/bancos_persistence.R:57-74`) captures a
*confirmation-record* summary (empresa/parte/documento/codigo/importe/
moneda/fecha/tipo), **not** a full copy of the source row. Compared field-by-
field against `.schema_manual` (`R/persistence.R:367-386`), it permanently
loses `Factura`, `Abono futuro`, `Notas`, `created_by`, `created_at`,
`updated_at`, `liability_id`, `referencia`, and the real
`Fecha de vencimiento` (silently substituted with the payment date) — the
`AR` confirm handler's version doesn't even capture `provision_id` (only AP's
does). None of this is recoverable once the source row is deleted.

### 2.4 "Quitar" (unstage without confirming)

`R/pagar_hoy_module.R:1147-1205` (AP), `:1208-1236` (AR). Removes the
selected `pagar_hoy_db` row(s) via `unstage_pagar_hoy()`. **AP's version
additionally reverts and deletes a linked `manual_inv` row** if the removed
row carries a `provision_id` (lines 1192-1199) — same unguarded hard-delete
pattern as §2.3, no papelera. **Any user, not just the one who staged the
item, can trigger this** — there is no ownership check.

### 2.5 `undo_conf`

`R/bancos_module.R:3286-3392`. Always soft-deletes the `bancos_confirmados`
row (`eliminado <- TRUE`) and, if `agenda_item_id` still exists in
`pagar_hoy_db`, resets that row's `status` back to `"pending"` in place
(ERP-sourced case — the row was never deleted, so this cleanly works and
preserves the real `FechaVenc`). If the row is gone (manual/provision case),
it synthesizes a brand-new `pagar_hoy_db` row from `bancos_confirmados`
fields (lines 3339-3361) — the lossy stand-in described in §1. **Either
way, the result always lands as a `pagar_hoy_db` row** — there is no code
path where undo returns an item to Calendario without also creating or
resetting an Agenda entry, which is the direct cause of Mouse's original
complaint (independent of the deeper data-loss bug it also surfaces for the
manual/provision case).

### 2.6 The hourglass badge is structurally decoupled from calendar content

`staged_keys_rv` (`R/ledger_module.R:575-583`) buckets `pagar_hoy_db` rows
with `status=="pending"` by **`FechaVenc`** alone. `calendar_html()`
(`R/global.R:489-509, 551-555, 588-590`) computes `has_staged` (hourglass) and
`has_data`/`body_html` (the actual line items, from `df_combined()`) as two
**entirely independent conditionals** on the same day cell — nothing
requires the two to agree. Any pending Agenda row whose `FechaVenc` doesn't
match a real, currently-visible Calendario row for that day (exactly what a
synthesized undo stand-in produces, since its `FechaVenc` is the payment
date rather than the real due date) renders a populated hourglass badge over
an empty tile body — this is the exact "hourglass with nothing under it"
Mouse observed on day 22, and it is fully explained without needing a second
hypothesis.

### 2.7 Reactivity and concurrency

**Same-session reactivity is already correct.** `staged_keys_rv` is a plain
`reactive()` over the `reactiveVal` `shared$pagar_hoy_db()` — any local
`shared$pagar_hoy_db(x)` call (confirm, Quitar, undo, staging) invalidates it
immediately, same reactive flush. Mouse's suspicion that the hourglass
"doesn't refresh on confirm/remove" is not reproducible for the acting
user's own session.

**Cross-session propagation is poll-based, not push, at an 8-second
interval** (`setup_sync_bus`, `R/sync_bus.R:57-132`, `poll_ms = 8000`,
registered `app.R:1256`). A different logged-in user will see up to ~8s of
staleness before another user's stage/confirm/Quitar shows up as a badge
change. This may be what Mouse actually observed, or may be acceptable —
worth confirming (§4, Q6) rather than assuming it needs to become instant
push.

**`pagar_hoy_db` and `manual_inv` both use full-table read-modify-write with
no version check or lock** (`save_pagar_hoy`/`save_manual` do a plain
`.s3_write()` of the entire local dataframe). `manual_inv` is **not** in the
cross-session sync registry at all (`app.R:1196-1254` lists every synced
key; `manual_inv` is absent) — it's loaded once per session and never
refreshed by the poll. Combined with "any user can Quitar," there is a real
lost-update window: two sessions racing a stage/confirm/Quitar within the
same ~8s poll gap (or, for `manual_inv`, at any time, since it never
re-syncs) can silently clobber each other's change when the loser's
full-table write lands second. This is a systemic risk broader than this
specific bug — flagged for a scope decision in §4 (Q7), not assumed to be
in-scope for the redesign below.

## 2.8 Vencidos has zero direct mutation — already correct, no fix needed

Verified by grepping the entire file for any `shared$X(value)` write-call or
`save_*`/`delete_*`/`add_to_papelera` call: `R/vencidos_module.R` (737 lines)
contains none. Every edit action (tag/move/delete/stage) routes through
`handle_invoice_action()` (`R/search_module.R:871-1088`), shared verbatim
with the Search tab and wired at `app.R:2196-2198`
(`observeEvent(input$vencidos_action, { handle_vencidos_action(input, shared) })`).
Vencidos is, today, already a pure read-only mirror of `df_combined_AR/AP()`
— exactly Mouse's rule for it. No change needed here.

## 2.9 `undo_conf` and the provision-revival watcher collide, with no coordination

Both `undo_conf` (`R/bancos_module.R:3286-3392`) and
`pasivos_observers.R`'s reversal watcher (lines 95-136) react to the exact
same event — a `bancos_confirmados` row's `eliminado` flipping to `TRUE`.
`undo_conf` has **no check of `row$provision_id`** anywhere in its body; it
unconditionally runs its generic "restore-in-place or synthesize a new
`pagar_hoy_db` row" logic regardless of whether the confirmation belongs to
a provision-derived item. Meanwhile `pasivos_observers.R` independently
calls `pasivos_provision_revive()` (resets the provision to
`estado="provisional"`, clears `manual_inv_id`/`pagar_hoy_id`/`bancos_conf_id`)
and deletes the derived `manual_inv` row — but never touches whatever
`undo_conf` just wrote to `pagar_hoy_db`.

Net effect, confirmed by tracing both code paths against the actual
confirm-time deletion (`R/pagar_hoy_module.R:1396-1409`, which physically
removes provision/manual `pagar_hoy` rows, not merely flags them): for a
provision-derived confirmation, `undo_conf`'s `orig_idx` lookup always comes
up empty (the row was deleted at confirm time), so it always takes the
"synthesize a new row" branch — leaving a **stray orphaned `pending` row in
Agenda**, carrying `provision_id`, for a provision whose `estado` is now back
to `"provisional"` and which, per Mouse's explicit rule, should have zero
Agenda footprint. This is a real, confirmed bug, independent of the
manual-entry data-loss bug in §1.

**Checked and confirmed NOT a bug:** `liability_id` reconnection. The
provision's own row (`pasivos_provisions_db`, schema at
`R/pasivos_schemas.R:68-111`) is never deleted through any part of this
cycle, and `pasivos_provision_revive()` (`R/pasivos_engine.R:959-984`) never
reads or writes `liability_id` — only `estado`, `manual_inv_id`,
`pagar_hoy_id`, `bancos_conf_id`, `reverted_count`. So "reconnecting a
recovered provision to its originating liability" is already automatically
true today; nothing to fix there.

## 2.10 "Send straight to Agenda" fabricates a row at TWO sites — a feature worth keeping, wired unsafely

Mouse wants this one-click "create/convert and immediately stage to Agenda"
convenience kept. Today it exists at two independent sites, and **both**
violate the root/mirror principle (§0) by handing Agenda a freshly
manufactured row instead of staging an existing Calendario-sourced one:

- **Pasivos conversion modal**, `stage_to_agenda` branch of
  `.pasivos_perform_conversion` (`R/pasivos_module.R:163-196`): after
  correctly writing the new invoice to `manual_inv` (the root write), it
  builds a brand-new `pagar_hoy` row **directly from the modal's raw form
  inputs** (`input$pcm_empresa`, `input$pcm_documento`, etc. — even
  synthesizing a placeholder `Documento` via `paste0("CONV_", prov_id)` if
  left blank) and calls `upsert_pagar_hoy()` with it. The low-level write
  primitive is the same function Calendar/Search staging uses — but the row
  handed to it is fabricated, not derived from re-reading the row that was
  just written to the root table.
- **Direct manual-entry creation with "send to agenda"**, `app.R:2449-2497`:
  identical shape — writes `manual_inv` correctly, then separately
  constructs and upserts a `pagar_hoy` row by hand (deliberately sharing the
  new `manual_inv` row's own `id`, per §2.1).

Both should instead: write the root row first, then stage it via the exact
same "stage this existing Calendario row" path Calendar's day-modal and
Search already use (i.e. re-derive the `pagar_hoy` row from the row that now
actually exists in `manual_inv`, the same way `ledger_module.R:975-1002`
does for a normal Stage-selected action) — same UX, same one-click
convenience, but Agenda is handed a reference to something real rather than
a hand-built guess.

## 2.11 Ghosted rows remain selectable and are counted in the day-modal's "Selección" subtotal

`ledger_module.R`'s day-modal table (`output$modal_tbl`) does not restrict
`DT::datatable`'s `selection` on confirmed/ghost rows — they're only styled
(struck-through, greyed, `line-through`/`opacity:0.55`,
`ledger_module.R:2744-2819`), never made unselectable. The plain total
(`all_total`, shown with nothing selected) already excludes confirmed rows
via `unconfirmed_mask` (`ledger_module.R:2877-2881`), but `sel_total` (the
running total for whatever the user has clicked, `ledger_module.R:2888`)
indexes into `sorted_amts`, built with **no confirmed/ghost filtering at
all** (`ledger_module.R:2868-2874`). So a user can select a struck-through
"Confirmado" row and its amount silently counts toward "Selección: …" — a
direct hit on the "ghost must never affect any calculation" rule, and it's
specifically the multi-select hypothetical-cash-flow feature this would
undermine most. Vencidos already blocks this correctly for its own display
(`.ven-confirmed-ghost { pointer-events: none; }`,
`R/vencidos_module.R` CSS) — the day-modal doesn't have the equivalent
guard.

## 2.12 Confirmed already-correct, no fix needed: ERP "deletion" already only ghosts, never wipes

Cross-checked against Mouse's rule ("no manual input may wipe an ERP row —
only the ERP's own update can"): today, "deleting" an ERP-sourced row via
the calendar's papelera mechanism does **not** remove it from
`df_combined()`'s data at all — Source 2 (papelera SAP-ghosts, post-Stage-A
numbering) keeps the row in the dataframe and only sets
`confirmed`/`is_ghost` (`R/ledger_module.R`, papelera block). The underlying
SAP snapshot itself is never touched by any in-app delete action. So the
"delete" button, when pointed at an ERP row, already only ghosts it — it can
never actually wipe it. This matches the rule exactly and needs no change.

## 3. Recommended direction — confirmed with Mouse, 2026-07-23

1. **Confirming always fully removes the item from `pagar_hoy_db`, for every
   source** — ERP and manual/provision alike. Today only manual/provision
   rows are removed; ERP rows are kept with `status <- "confirmed"`. Change
   ERP-sourced confirm to unstage (delete) the Agenda row too — this is
   always safe for ERP per Mouse's explicit rule (Agenda removal never
   touches the real Calendario data). Calendario's crossout for ERP rows
   continues to come entirely from `bancos_confirmados` matching
   (post-Stage-A Source 1) — **`pagar_hoy_db.status=="confirmed"`
   (post-Stage-A Source 3) becomes structurally dead** once this lands (no
   source ever leaves a `status=="confirmed"` row behind anymore) and should
   be *removed*, not just left inert, from `df_combined()`'s matching logic.
   This also means `undo_conf`'s "restore the original pagar_hoy row
   in-place" branch becomes dead code for ERP too — undo for an ERP
   confirmation should simply clear the `bancos_confirmados` flag and touch
   `pagar_hoy_db` not at all, relying purely on the flag-clear to make the
   item reappear (crossed-out → open) in Calendario.

2. **One unified archive mechanism for BOTH "deleted" and "confirmed"
   dispositions of a `manual_inv` row — generalize `papelera`, don't build a
   second parallel table.** Mouse's explicit direction: confirmation-history
   covers *everything that gets confirmed* (plain manual entries **and**
   provision-derived ones — no special-casing by origin), and "the
   mechanisms for deleted and confirmed are almost the same... unify,
   standardize, simplify." Concretely: extend `papelera`'s existing
   generic-archive shape (already stores the complete original row via
   `original_data`, plus who/when) with a `disposition` column
   (`"deleted"` | `"confirmed"`), and route **every** exit of a live
   `manual_inv` row — the existing calendar/search "eliminar" paths AND the
   confirm handlers AND provision-derived confirm/Quitar — through this one
   mechanism instead of two. Undo/restore becomes one shared function
   (move the archived row back to `manual_inv` unmodified, regardless of
   which disposition it was archived under) rather than two separate,
   drift-prone implementations. Exact schema fields to be verified fresh
   against `papelera`'s current schema when this stage is implemented, not
   locked here.

3. **Generalize the `is.na(source) | source == "sap"` idiom into one shared
   helper** (e.g. `is_erp_sourced(source)`), defined once, called at all ~10
   current sites. Any future second ERP integration then only needs to give
   its rows a distinct `source` value — it does not need to be found and
   special-cased at every site by hand.

4. **Fix the "send straight to Agenda" convenience feature at both sites
   (§2.10) without removing it** — Mouse explicitly wants this UX kept.
   Rework both the Pasivos conversion modal's `stage_to_agenda` branch and
   the direct-manual-entry-creation "send to agenda" path so the root write
   (to `manual_inv`) happens first, then the resulting row is staged via the
   same "stage an existing Calendario row" primitive Calendar/Search
   already use — same one-click result for the user, no fabricated Agenda
   row.

5. **Fix the `undo_conf` / `pasivos_observers.R` collision (§2.9)**:
   `undo_conf` should skip its own Agenda-restore/synthesize logic entirely
   whenever the confirmation being undone carries a non-NA `provision_id`,
   deferring completely to the existing provision-revival watcher (which
   already correctly reverts the provision and needs no Agenda footprint at
   all, per Mouse's "provisions must never touch Agenda" rule).

6. **Ghost rows become unselectable in the day-modal's `DT` table** (§2.11),
   matching Vencidos' own existing `pointer-events: none` precedent —
   consistency with an existing in-app pattern over inventing a new one.
   This also structurally guarantees the "Selección" subtotal can never
   include a ghost, without needing a second, separate filter fix.

7. **Hourglass consistency guard**: never render the staged-count badge over
   a day with zero visible Calendario line items. Mostly becomes a
   regression guard once points 1-2 land (the stand-in-with-wrong-date
   scenario that caused this should no longer be producible), but keep the
   guard anyway as defense-in-depth.

8. **A structural simplification this unlocks, worth sequencing around**:
   once every confirmed manual/provision row is archived (never flagged) and
   every ERP confirm always fully unstages Agenda, `df_combined()`'s
   confirmed-matching model drops from 3 sources (post-Stage-A) to
   effectively **2** — `bancos_confirmados` matching and papelera SAP-ghosts.
   `pagar_hoy_db.status=="confirmed"` matching (Source 3) is removed
   entirely, not merely deprecated. This has a direct sequencing
   implication for `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md`'s Stage C
   (extracting `compute_confirmed_flags()`) — see the consolidated plan.

9. Concurrency (§2.7, `pagar_hoy_db`/`manual_inv` read-modify-write race) —
   still an open scope call, not assumed in-scope for this effort. Default
   recommendation: treat as a separate, consciously-deferred follow-up given
   it's a systemic risk broader than this specific feature — flag if that's
   wrong.

## 4. Still open — everything else confirmed 2026-07-23

1. Invoice `1025618` (§1) — re-enter manually with the surviving fields
   (Empresa=Networks Crossdocking Services, Parte=Hapag Lloyd,
   Documento=1025618, Importe=532 USD, Codigo=P1581; real due date unknown —
   22/07/2026 was the payment date, not the due date), or is there another
   system of record to pull the real due date from?
2. Is the 8-second cross-session poll interval for the hourglass badge count
   acceptable, or does it need to tighten? (Same-session reactivity is
   already instant.)
3. Should `bancos_confirmados` gain a `source` column (mirroring
   `pagar_hoy_db`'s), so audit/reporting can tell ERP vs. locally-owned
   confirmations apart at a glance? Low-priority, nice-to-have.
4. Concurrency (§3.9) — confirm the default (deferred) or pull it into scope.

## 5. Test plan hints for the implementing session

- Confirm (ERP-sourced) → assert `pagar_hoy_db` row is gone, `bancos_confirmados`
  row exists, Calendario shows the crossed-out row, Vencidos reflects it, and
  the row is excluded from every calculation per §2.11/§2.12's rule (day
  total, day-view sum, selection subtotal, Intercompany, Cash Flow
  Preview/Export, Reporte Pulse — cross-check against
  `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md`'s own test plan for the latter
  three, since this is the same underlying rule).
- Confirm (manual-sourced, both plain and provision-derived) → assert the
  `manual_inv` row is archived (moved, `disposition="confirmed"`) not
  deleted, unmodified field-for-field; `pagar_hoy_db` row gone; Calendario no
  longer shows the row at all (not ghosted — fully absent); Vencidos no
  longer shows it (inherits automatically, no direct Vencidos change needed
  per §2.8).
- Undo (any source) → row fully restored with **zero field loss** (the
  regression test that would have caught `1025618`'s `FechaVenc`
  substitution) and **no** `pagar_hoy_db` row is created or left behind, for
  ERP, plain manual, AND provision-derived cases (§2.9's collision fix needs
  its own explicit test: undo a provision-derived confirmation, assert
  exactly one outcome — provision back to `"provisional"`, zero `pagar_hoy_db`
  trace — not two independent mechanisms leaving inconsistent state).
- Deletion (explicit "eliminar") → same archive mechanism, `disposition="deleted"`,
  same restore guarantee — this is the regression path for papelera's
  existing behavior once it's generalized; must not regress.
- "Send to Agenda" convenience (§2.10, both entry points) → the staged
  `pagar_hoy` row's fields exactly match what's now in `manual_inv` (proving
  it was staged from the real row, not fabricated) — construct a case where
  the modal's raw input would have differed from a hand-built row to make
  this a meaningful assertion, not a tautology.
- Selection subtotal (§2.11) → a ghost row cannot be selected at all in the
  day-modal table; if selection is attempted programmatically in a test,
  confirm it's excluded from the sum regardless (defense in depth).
- Hourglass: staged-but-unconfirmed item → badge count matches; confirm or
  Quitar it → badge count decrements immediately in the same session. A day
  with zero visible Calendario line items never shows a populated badge,
  under any flow above.
- Multi-source regression: add a second synthetic `source` value in tests
  (not a real ERP) and confirm every one of the ~10 sites now correctly
  treats it as ERP-owned via the new shared helper, with no site missed.
- Concurrency (only if pulled into scope per §4.4): two sessions
  staging/confirming/Quitar-ing concurrently within the poll window must not
  silently drop either change.

## This document has been superseded as the sole planning reference

Everything in §3 is confirmed direction as of 2026-07-23. The actual staged
implementation sequence — combined with `docs/CONFIRMED_INVOICE_LOGIC_AUDIT.md`,
since the two efforts now share code paths and one specific sequencing
dependency (§3.8) — lives in `docs/LEDGER_INTEGRITY_MASTER_PLAN.md`. Read
that document for the stage order and prompts; this document remains the
evidentiary record (incident details, exact current-code citations, the
reasoning behind each decision) to consult when a stage's own instructions
say to re-verify something against the audit.
