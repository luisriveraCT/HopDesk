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

## 3. Recommended direction

Translating Mouse's target architecture (§0) into concrete mechanism:

1. **Confirming always fully removes the item from `pagar_hoy_db`, for every
   source.** Today only manual/provision rows are removed; ERP rows are kept
   with `status <- "confirmed"`. Change ERP-sourced confirm to unstage
   (delete) the row too — Calendario's crossout already comes entirely from
   `bancos_confirmados` matching (post-Stage-A Source 1), so
   `pagar_hoy_db.status=="confirmed"` (post-Stage-A Source 3) becomes
   unnecessary as a confirmed-signal once this lands; it can likely be
   retired from `df_combined()`'s matching logic entirely (needs
   verification once implemented, not assumed here).

2. **Stop deleting `manual_inv` rows on confirm. Move them instead**, into a
   new mirror table — schema-identical to `manual_inv`, plus confirmation
   metadata — call it `manual_inv_confirmados`. Confirm = move the row
   (unmodified) from `manual_inv` to `manual_inv_confirmados`. Undo = move it
   back, unmodified. This is exactly Mouse's §0/answer-3 proposal ("a
   different table identical to the regular item table... send it back to
   its original table with zero damage") and eliminates every lossy-summary
   problem in §2.3/§2.5 at the root, rather than patching around it.
   `bancos_confirmados` keeps its current, different job — the bank-side
   confirmation/reconciliation record (linked account, `mov_id`, etc.) — it
   is not a replacement for keeping the original invoice data intact; the
   two tables serve genuinely different purposes and both should exist.

3. **Generalize the `is.na(source) | source == "sap"` idiom into one shared
   helper** (e.g. `is_erp_sourced(source)` / `is_locally_owned_source(source)`),
   defined once, called at all ~10 current sites. Any future second ERP
   integration then only needs to give its rows a distinct `source` value —
   it does not need to be found and special-cased at every site by hand.

4. **Leave calendar rendering untouched at staging time** (Mouse confirmed no
   change needed there) but make the hourglass badge internally consistent:
   at minimum, stop it from ever appearing over a day with zero visible line
   items (since after point 1-2 land, a stand-in-with-wrong-date should no
   longer be possible in the first place — this becomes primarily a
   regression guard, not the main fix). Confirm with Mouse whether the 8s
   cross-session poll interval for the badge count is acceptable or needs
   tightening (§4 Q6).

5. Decide the concurrency question (§2.7) as its own explicit scope call
   (§4 Q7) — likely a separate follow-up given it's broader than this
   feature, but flagging it low-risk-to-defer only if Mouse agrees the
   current staff headcount/usage pattern makes simultaneous conflicting
   edits rare in practice.

## 4. Needs an explicit decision from Mouse before implementation

1. Confirm points 1-4 in §3 as the direction (or redirect).
2. `manual_inv_confirmados`'s exact schema — full copy of `.schema_manual`
   plus which metadata columns (proposed: `confirmed_at`, `confirmed_by`,
   `eliminado`, `eliminado_at` for the undo-reversal itself, mirroring
   `bancos_confirmados`'s own soft-delete shape)?
3. Should `bancos_confirmados` gain a `source` column (mirroring
   `pagar_hoy_db`'s), so future reporting/audit can tell at a glance whether
   a given confirmation was ERP or locally-owned, independent of whether a
   `manual_inv_confirmados` row also exists?
4. Provision-derived invoices currently interact with `pasivos_provisions_db`
   (estado/manual_inv_id/pagar_hoy_id FKs) on both confirm and Quitar (§2.4).
   Moving to a mirror-table design for manual/provision rows needs this FK
   wiring re-verified end-to-end, not just the manual case — should this be
   its own stage, given `pasivos_*` is a large, separate subsystem?
5. Invoice `1025618` itself (§1) — confirm you'll re-enter it manually with
   the surviving fields (Empresa=Networks Crossdocking Services,
   Parte=Hapag Lloyd, Documento=1025618, Importe=532 USD, Codigo=P1581,
   original due date unknown — payment date shown was 22/07/2026 but that is
   NOT the real due date), or is there another system of record (e.g. the
   original SAP/freight-forwarder export) to pull the real due date from?
6. Is the 8-second cross-session poll interval for the hourglard badge count
   (and Agenda's pending list in general) acceptable, or does it need to
   become push/faster? (Same-session reactivity is already instant — this
   only affects a *different* logged-in user seeing your change.)
7. Scope call on the `pagar_hoy_db`/`manual_inv` concurrent-write race
   (§2.7): fix now as part of this effort, or treat as a separate,
   consciously-deferred follow-up?

## 5. Test plan hints for the implementing session

- Confirm (ERP-sourced) → assert `pagar_hoy_db` row is gone, `bancos_confirmados`
  row exists, Calendario shows the crossed-out row, Vencidos reflects it.
- Confirm (manual-sourced) → assert `manual_inv` row moved (not deleted) to
  `manual_inv_confirmados` unmodified, `pagar_hoy_db` row gone, Calendario no
  longer shows the row, Vencidos no longer shows it.
- Undo (either source) → assert the row is fully restored to its origin
  table with **zero field loss** (this is the regression test that would
  have caught `1025618`'s `FechaVenc` substitution) and **no** `pagar_hoy_db`
  row is created.
- Hourglass: staged-but-unconfirmed item → badge count matches; confirm or
  Quitar it → badge count decrements immediately in the same session.
- A day with zero visible Calendario line items never shows a populated
  hourglass badge, under any of the above flows.
- Multi-source regression: add a second synthetic `source` value in tests
  (not a real ERP) and confirm every one of the ~10 sites now correctly
  treats it as ERP-owned via the new shared helper, with no site missed.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\AGENDA_CALENDARIO_WIRING_AUDIT.md in
full before writing any code. This is an audit and recommended direction,
not a locked spec — §4 lists decisions that need Mouse's explicit input
before implementation. Ask him about each one in your own words; do not
assume an answer.

This is a HIGH-SEVERITY fix: §1 documents a real, already-occurred,
unrecoverable data loss incident, and §2.3/2.4 show the exact mechanism
(confirm/Quitar hard-deleting manual_inv with no papelera archive) is still
live in production right now. Work in small, focused, independently-tested
stages (Mouse's explicit instruction) — re-verify every line number cited
here with a fresh read before editing, this codebase changes often. After
EACH stage: run the existing test suite plus new tests for that stage,
report results, stop and ask about anything not fully certain, and give
Mouse a concrete manual-verification checklist (specific clicks, specific
things to check in Calendario/Vencidos/Agenda/console) before starting the
next stage. Do not merge to master yourself — Mouse reviews and merges each
stage.

Suggested stage breakdown (adjust if a better one emerges once you're in the
code):
1. `is_erp_sourced()`/equivalent helper + migrate all ~10 hardcoded
   `source == "sap"` sites onto it (mechanical, low-risk, a good first
   regression-test target).
2. `manual_inv_confirmados` mirror table + move-not-delete on confirm for
   manual-sourced rows + move-back-not-synthesize on undo. Verify zero field
   loss end-to-end.
3. ERP-sourced confirm also fully unstages `pagar_hoy_db` (matching the
   manual case now); verify Source 3 in df_combined() (pagar_hoy_db
   status=="confirmed") is no longer needed and remove or justify keeping it.
4. Provision-derived invoices: re-verify the pasivos_provisions_db FK wiring
   against the new mirror-table design end-to-end.
5. Hourglass consistency guard (never show over zero visible line items).
6. Concurrency (pagar_hoy_db/manual_inv race) — only if Mouse confirms it's
   in scope for this effort per §4 Q7.
```
