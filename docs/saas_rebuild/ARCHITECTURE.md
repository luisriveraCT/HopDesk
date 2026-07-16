# HopDesk — Multi-Tenant SaaS Architecture

**Status:** Design reference. This is the shared source of truth for every stage
document in this folder. If a stage document and this file ever disagree, this
file wins — flag the conflict back to Mouse rather than resolving it silently.

**Origin:** Written after a full audit of the existing codebase found the
current wiring architecturally unsound (see "Known issues fixed by this plan"
below). Do not treat any existing mechanism as correct just because it exists —
several of the mechanisms described as "current state" here are exactly what
each stage replaces.

---

## 1. Who uses this app, and what they may touch

**Hopdesk** is the company operating this SaaS. Its own staff use the app to
service clients. Its own operational data (if any — see §7) lives in a folder
of its own.

**A Client** is one conglomerate/group (e.g. "Networks Group"). A Client
registers itself once, then registers the individual companies it owns. All of
a Client's data — companies, users, invoices, ERP credentials, logs — lives in
exactly one S3 folder, named by the client's slug (`networks/`, `hopdesk/` as a
*test* client distinct from Hopdesk's own staff folder `hd-admin/`, etc.).

**A Client's users** never see or touch any data outside their own client's
folder. Not other clients' data, not Hopdesk's internal tools, not other
clients' users. This is called **the vacuum**: once you are inside a client
context — whether because you are a native user of that client, or because you
are Hopdesk staff who just jumped in — everything you can see and do is scoped
to that one folder, with no standing exceptions.

**Hopdesk staff** are the only accounts that can cross client boundaries, and
only through one explicit mechanism: **the jump** (§5). A staff member without
an active jump is in their own home context (`hd-admin/`) and sees none of any
client's data. A staff member who has jumped into a client is, for as long as
the jump lasts, *inside the vacuum* — their own user-management and data
actions are scoped to that client only, exactly like a native client user's
would be. Jumping grants entry; it does not grant a standing bypass.

---

## 2. Tenant isolation — what actually enforces the boundary

The enforcement mechanism is, and remains, **the S3 folder-per-client
prefix** (`.s3_key()` in `R/persistence.R`). Every piece of a client's data —
users, companies, invoices, ERP credentials, audit logs — is namespaced under
`<client_id>/`. This is unchanged by this rebuild and is the correct
foundation; the rebuild's job is to make sure every other layer of the app
(session routing, tier logic, credential loading) *actually respects* that
boundary consistently, which today it does not.

---

## 3. Deployment model

**Decision: one shared deployment**, not one deployment per client.

Rationale: the real isolation boundary is the S3 data layer (§2), not the
process. Splitting into per-client deployments only pays off when a specific
client contractually requires dedicated infrastructure, or is heavy enough to
risk starving other clients' performance. Neither applies at current scale.
The long-term scaling lever for a Shiny app is **horizontal scaling of the
app tier** (multiple container replicas behind a load balancer — a
`Dockerfile` already exists in the repo — or a paid shinyapps.io tier with
multiple instances), while keeping one shared codebase and the per-client S3
data split. Revisit only if a specific client someday requires dedicated
infrastructure by contract.

Every user — Hopdesk staff and every client's users — logs in at the same URL,
the same login screen. The env var `CLIENT_ID` is set to `hd-admin` for this
one deployment and is used only as: (a) the home S3 folder for Hopdesk's own
data, (b) the seed for the login-credential merge (every active client's
`usuarios.rds` is merged into one login table — see `R/auth.R`). It must never
again be used as a stand-in for "is this session staff" (see §4 — that was the
root cause of the tesoreria bug).

Housekeeping: retire the three stale shinyapps.io/Posit Connect Cloud
deployment records that predate the current one; confirm in the shinyapps.io
dashboard which single app is receiving live traffic before deleting anything.

---

## 4. Tiers, staff vs. client, and `is_staff`

### 4.1 The tier registry (single source of truth)

Today, "is this tier a staff tier" is answered independently in at least four
places in the code (`tier %in% c("hopdesk","principal")` duplicated with
slight variations), and one of those places lost a timing race — that's
exactly how a `finance`-tier client user ended up on the staff landing tab.

**Fix:** one tier registry, one place, everything else reads from it. Stage 1
introduces this. Every tier carries:

| field | meaning |
|---|---|
| `label` | display name |
| `rank` | numeric, higher = more powerful, used for hierarchy comparisons |
| `internal_only` | `TRUE` = a Hopdesk staff tier, never shown/assignable/visible to any client session, under any circumstance, now or for any tier added later |
| default permission flags | unchanged from today's `auth_resolve_perms()` defaults, just carried in the same structure instead of a parallel list |

Current tiers and their `internal_only` value:

| tier | internal_only | who |
|---|---|---|
| `principal` | `TRUE` | Mouse only. The tier above `hopdesk` — the only tier that may audit Hopdesk staff's own activity (§6). Formalized, not new. |
| `hopdesk` | `TRUE` | Hopdesk support staff. Can jump into clients (with a grant, §5), can see client logs, cannot see `principal`-only data or tiers. |
| `dev` | `FALSE` | A **client's own IT department**. Sits at the top of that client's own tier ladder — not necessarily given to the highest-ranked business person, but to whoever handles the technical/discretionary work: connecting the ERP, managing that client's own users, viewing that client's own logs. Deliberately has visibility into that client's financial data too — a client's IT team fully services their own company, by design. |
| `admin` | `FALSE` | A client's senior business user — full operational access, cannot manage that client's own users/tiers or ERP connections (that's `dev`'s job). |
| `finance` | `FALSE` | A client's operational user — day-to-day work, permission-gated actions. |
| `analysis` | `FALSE` | A client's read-only/limited user. |

Any tier added in the future — for either side — must be added to this
registry with an explicit `internal_only` value. There is no other place in
the code that should ever independently decide whether a tier is a staff tier.

### 4.2 `is_staff` — computed once, used everywhere

`is_staff` is not a separately-edited field a human sets on a user record —
it is **derived from the user's tier via the registry** (`internal_only`),
computed exactly once per session at the point the user's tier is first known
(right where `current_user_info()` resolves today), and exposed as part of
that same object everywhere downstream already reads it. No other observer,
UI-visibility rule, or watchdog may re-derive it independently — they all read
`current_user_info()$is_staff`.

This single change removes the entire class of bug the tesoreria incident
belongs to: routing, tab visibility, the redirect-away-from-staff-landing
logic, and the hop-grant watchdog's staff/client exemption all become
consumers of one fact instead of four independent guesses.

### 4.3 UI-level enforcement of the vacuum for user management

A client session's "add user" tier dropdown must only ever be populated from
`internal_only == FALSE` tiers. A staff session's dropdown may show both. This
is cosmetic only — the real enforcement is server-side: any handler that
creates or edits a user must reject (not just hide the option for) an attempt
to set `internal_only == TRUE` on a session that is not itself `is_staff`.
Never trust the client-side dropdown alone.

---

## 5. The jump (staff temporary access) vs. living at home

These are two structurally distinct mechanisms sharing no state:

- **Home context**: every user — staff or client — has exactly one home
  folder (`hd-admin` for staff, `<client_id>` for a client user). This is set
  once at login from the user's own record and never changes for the
  lifetime of the session unless a jump happens.
- **A jump**: a `hopdesk`-tier session may temporarily enter one client's
  vacuum, gated by an explicit grant (`hop_grants` — this mechanism already
  exists and is kept) with an expiry and a revocation path. While jumped, the
  staff member is inside that client's vacuum (§1) — every module reloads
  from that client's data, every user-management action is scoped to that
  client, and every write is logged to *both* that client's log and the
  staff's own home log (already implemented in `R/app_audit.R`, kept as-is).
- **Never mix the two.** Today's code stores "whose data am I looking at"
  in one shared variable (`active_client_id`) for both cases, and a watchdog
  meant only for jumps (`tiers_module.R`'s 30-second poller) doesn't
  distinguish a client user innocently sitting in their own home folder from
  a staff member on an expiring grant — because to that code, both look
  identical: "active_client_id isn't hd-admin, and I'm not principal/dev."
  Stage 2 gives home-context sessions and jump sessions genuinely separate
  state so this class of false-positive becomes structurally impossible, not
  just guarded against.

---

## 6. Audit logging — three visibility tiers

The dual-write mechanism (`R/app_audit.R`) already does the hard part
correctly: every action is logged to the client folder it affects, and when
staff act inside a jumped client, the entry is written to *both* that
client's log and Hopdesk's own home log, tagged as an outsider write. Stage 4
adds the missing visibility rules and UI:

| viewer | sees |
|---|---|
| A client's own `dev`/`admin` user | Their own client's log only — every action in their folder, including Hopdesk staff actions taken during a jump. |
| Hopdesk staff (`hopdesk` tier) | Any client's log (with filters), never Hopdesk's own staff activity log. |
| `principal` (Mouse) | Everything — every client's log, and Hopdesk staff's own activity log, filterable by actor type, specific client, specific user. |

One filter UI, reused by every viewer above (just scoped by what they're
allowed to see) — date range, time range, and whatever else Stage 4 finds
useful, kept small and compact (screen real estate is scarce in this app).

---

## 7. Hopdesk staff's own future tooling (design placeholder — not built now)

Hopdesk staff will eventually need their own operational tools — a support
ticketing system, cross-client dashboards — that have nothing to do with any
one client's calendar/invoices/bancos modules. `hd-admin/`'s folder stays
deliberately empty of client-shaped data forever; if/when this is built, it
gets its *own* schema and its *own* modules, not a repurposing of any
client-facing module. Stage 5 leaves comment-only placeholders marking where
this would attach (main nav, a home-context landing area) without inventing
names, schemas, or code for a feature that isn't scoped yet.

---

## 8. Per-client ERP connectivity (Stage 3)

Today, SAP connectivity is 100% hardcoded env vars in the one shared
`.Renviron`, scoped only to Networks Group. There is no way for any other
client to connect their own ERP. Stage 3 replaces this with a self-serve,
per-client, per-company credential system, designed so that:

- A client's `dev`-tier user can add/edit/test their own ERP connections,
  one credential set per company (Networks needs 5; another client might need
  a different shape entirely — the schema must not assume SAP's fields).
- Hopdesk staff can also set this up *for* a client, using the exact same
  mechanism, via a jump — never a separate staff-only backdoor path.
- It is provably impossible for a client's connection to read another
  client's credentials, even by an application bug — see Stage 3's design
  for exactly how this is enforced, not just intended.
- Secrets are encrypted before they ever reach S3 (simple, one master key
  for now — see Stage 3), with comments marking the upgrade path to
  per-client keys/KMS if that's ever needed.
- Networks' existing SAP credentials are migrated in, pre-filled, during
  Stage 3 — nobody has to retype them from memory.

---

## 9. Working conventions for every stage

- One git branch per stage, branched off `saas-checkpoint-2026-07-15` (or the
  latest merged stage branch), pushed to `origin` as work proceeds, merged to
  `master` only once Mouse has reviewed and the stage's test plan passes.
- Never patch a symptom in isolation — if a fix requires touching the tier
  registry, the jump/home split, or the audit log, go to the source of truth
  (this document + the relevant stage doc), not the nearest `if` statement.
- Every stage document contains its own test plan and its own literal prompt
  to hand to the implementing Claude Code session. Work stage by stage;
  do not start Stage *N+1* until Stage *N*'s tests pass and Mouse has signed
  off.

## Known issues this plan fixes (for traceability)

1. **Tesoreria routing bug** — a `finance`-tier client user was landed on the
   staff-only Usuarios/Grupo tab and never reliably redirected away, because
   staff/client routing was decided independently in ≥4 places with a timing
   race in one of them. Fixed structurally by §4.
2. **Hop-grant watchdog false positive** — the 30-second staff-grant poller
   couldn't distinguish "client user at home" from "staff on an expiring
   grant," because both used the same `active_client_id` state. Fixed by §5.
3. **Non-uniform module reloading** — only the calendar/SAP-snapshot path was
   ever wired to reload on a context change; every other module was not,
   which is why switching tabs showed empty modules regardless of the routing
   bug above. Fixed by §5 (Stage 2 makes context-reload a single mechanism
   every module subscribes to, not a per-module ad hoc pattern).
4. **No per-client ERP self-service** — fixed by §8 (Stage 3).
5. **No audit-log visibility rules or UI** — fixed by §6 (Stage 4), building
   on the dual-write mechanism that already exists.
