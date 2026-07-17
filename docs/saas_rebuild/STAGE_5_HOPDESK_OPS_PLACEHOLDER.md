# Stage 5 — Hopdesk Staff Future Tooling (Placeholder Only — No Functional Code)

Read `ARCHITECTURE.md` §7 first. Stages 1–4 are merged into
`saas-checkpoint-2026-07-15` and confirmed passing (362/362).

## What this stage is, and — importantly — is not

Mouse's own words from the original scoping conversation: *"Hopdesk staff
will absolutely eventually need tickets and dashboards. Please design it
all, create the comments in the code without writing code or variables but
just establish the architecture and idea so that we can do that on a later
project and we don't forget it."*

This stage produces **comments, not code**. No new schema, no new
reactiveVals, no new S3 keys, no new UI elements a user can click. If you
find yourself writing a function body, a `data.frame()`/`tibble()` schema
definition, or a real column name, stop — that's a future stage's job, not
this one. The one thing this stage *is* allowed to add: a small number of
well-placed comment blocks (and, if it genuinely helps orient a future
implementer, one new near-empty file that is pure prose comments, not
sourced into anything) describing the shape of two future features:

1. **A support ticketing system** for Hopdesk staff.
2. **Cross-client operational dashboards** for Hopdesk staff.

## Why this belongs in its own stage, and why it's low-risk

Nothing here can regress anything — there's no code to run, so the test
plan below is "confirm the app behaves identically to before this stage,"
not a normal automated suite. Low stakes, but still worth doing carefully:
Mouse was explicit that forgetting this scope entirely is the failure mode
to avoid, not writing it prematurely.

## Design — what to actually write, and where

### 1. Where these features would live: `hd-admin`'s own folder, never a client's

Per `ARCHITECTURE.md` §7: Hopdesk's own S3 folder (`hd-admin/`) stays
deliberately empty of client-shaped data forever. Ticket and dashboard data
is fundamentally different in kind from anything a client's own modules
handle (Calendario, Vencidos, Bancos, etc.) — it's Hopdesk's own internal
operational data about *how Hopdesk services clients*, not a client's own
financial/operational data. It would get its own schema and its own
module(s), never a repurposing of any existing client-facing module or
table.

### 2. Ticketing system — describe the concept, not the schema

Add a comment block (see placement in §4) describing, in prose:
- A ticket represents a piece of support work Hopdesk staff does for one
  client — conceptually similar to how a "jump" already represents staff
  entering a client's context, but a ticket is a persistent record of *why*
  and *what happened*, not just a temporary access grant.
- A ticket would likely reference: which client it's for, which staff
  member is handling it, a status (open/in progress/resolved — exact
  values not decided), and a free-text description — and probably a link
  to the relevant `app_audit` entries from whatever jump/actions were taken
  to resolve it, since the audit infrastructure (Stage 4) already captures
  "staff did X in client Y's folder at time Z."
- Whether a ticket should *require* a jump grant to exist, or can exist
  independently (e.g. logging a phone call that didn't involve touching
  the client's data at all) is an open product question for whoever scopes
  this for real — note it as open, don't resolve it here.

### 3. Cross-client dashboards — describe the concept, not the schema

Add a comment block describing, in prose:
- An aggregate view across every active client in the registry (not any
  one client's own data) — e.g. how many clients are active, how many
  seats each is using against their limit (the registry already tracks
  `max_users`/`current_users` — Stage 6 of the original CLAUDIO spec work),
  how many open tickets exist and their age, which clients have had recent
  jump activity.
- This is a natural extension of what `principal`'s global audit view
  (Stage 4) already partially does (see everything across every client) —
  a dashboard would summarize/visualize that same underlying data rather
  than requiring a wholly separate data source.
- Exact metrics, chart types, and layout are explicitly not this stage's
  job to invent.

### 4. Placement of the comment blocks

- **`app.R`**, near the `nav_menu(title = ..., value = "GRUPO", ...)`
  definition: a comment noting that a future "Soporte"/dashboard nav
  destination for staff would likely live alongside or within this menu,
  visible only when `is_staff` (reuse Stage 1's `is_staff`/tier-registry
  concept — do not invent a new visibility mechanism for this placeholder).
- **`app.R`**, near the `redirected_to_staff_home()` observer (Stage 1):
  a comment noting that today staff land on the Grupo/Usuarios tab at
  home, and a future dashboard might become the actual landing destination
  instead — not a decision to make now, just a marker of where that
  decision would be wired in.
- **One new file**, `R/hopdesk_ops_placeholder.R` — not sourced by
  `global.R` or `app.R` (it does nothing, so it shouldn't run) — containing
  only a substantial header comment consolidating everything from §2 and
  §3 above into one place a future implementer would read first. Explicitly
  mark it, in the file itself, as "not implemented, not sourced, written
  2026 during the SaaS architecture rebuild to preserve scope — see
  `docs/saas_rebuild/STAGE_5_HOPDESK_OPS_PLACEHOLDER.md` for the original
  discussion."

## Explicitly out of scope

- Any new S3 key, schema, reactiveVal, module, or UI element that a user
  can actually interact with.
- Deciding ticket status values, dashboard metrics, or any other concrete
  detail not already settled above — leave these as open questions in the
  comments, don't silently resolve them.
- Any change to existing functional code at all — this stage's diff should
  be comments and one new non-functional file only.

## "Test plan" (verification, not a normal test suite)

- Run the full existing test suite once — it should be unaffected (this
  stage adds no functional code, so nothing should change: still 362/362).
- Confirm `R/hopdesk_ops_placeholder.R` is not referenced by any
  `source()` call anywhere — it should have zero effect on the running app.
- Manually open the app as both a client user and as Bunny (hopdesk) and
  confirm nothing about the UI changed at all — this stage should be
  invisible to every actual user.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md and
docs\saas_rebuild\STAGE_5_HOPDESK_OPS_PLACEHOLDER.md in full before doing
anything. This stage is comments only - if you find yourself writing a
function body, a schema, or a real variable/column name, stop and
reconsider; that belongs to a future stage, not this one.

Branch off saas-checkpoint-2026-07-15 (Stages 1-4 already merged), name
the branch stage-5-hopdesk-ops-placeholder.

Add the two comment blocks to app.R at the locations described in this
document's Design §4, and create R/hopdesk_ops_placeholder.R as a
non-functional, non-sourced file containing the consolidated prose
description from §2 and §3. Do not add it to global.R's source() list.

Run the full existing test suite and confirm it is unaffected (still
362/362 - if the count changed, you added functional code by mistake).
Manually confirm the app's UI is pixel-for-pixel unchanged for both a
client session and a staff session.

When done, report back exactly what was added, file by file, and confirm
the test count is unchanged. Do not merge to master or the checkpoint
branch yourself.
```
