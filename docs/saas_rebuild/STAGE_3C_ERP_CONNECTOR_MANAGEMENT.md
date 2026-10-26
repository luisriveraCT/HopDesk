# Stage 3c — Multi-ERP Connector Management (design only, not yet built)

Read `STAGE_3_ERP_CREDENTIALS.md` and `STAGE_3B_ERP_LIVE_CUTOVER.md` first,
then the commit message for `2364f4c` ("First wire for multi-ERP support")
— that commit is this stage's prerequisite and already merged. This
document is a **design**, deliberately not an implementation task list yet.
Per the user's own instruction: imagine and design the shape of this tool
now; build it slowly, one deliberate piece at a time, later.

## Why this stage

All real invoice data today is 100% SAP (confirmed directly by the user).
Stage 3 already built the right foundation for what comes next — a
connector registry (`R/erp_connector_registry.R`) that generates its own UI
form and validation from a declared field list, an encrypted per-client
credential store (`erp_connections.rds`), and a self-serve settings screen
for a client's own `dev`-tier user. Stage 3c's own commit generalized the
one remaining hardcoded assumption (`source == "sap"` everywhere) into a
registry-driven identifier, and added a `homologate_fn` slot — the function
that turns any ERP's raw response into HopDesk's own canonical row shape.

The user has stated HopDesk will wire up "many" ERPs over time (Odoo and
Oracle named specifically), done by HopDesk's own staff, as a **recurring,
constant, repetitive task** — not a one-time migration. That reframes the
problem: Stage 3's UI was designed for a client configuring *their own*
one or two connections occasionally. What's missing is tooling built for
the *operator* doing this *often*, *across every client*, as their job.

## What "done" looks like for this stage's design (not its build)

A staff member who is not the original developer of HopDesk can: see, at a
glance, every ERP connection across every client and whether it's healthy;
configure a new connection of an *already-supported* ERP type in minutes,
guided by a form the registry generates automatically; and know exactly
what has to happen, and in what order, when HopDesk decides to support a
genuinely new ERP product for the first time.

## Prior art this design deliberately borrows from

Multi-connector management is a solved problem in industry, not a novel
one — no reason to invent a shape from scratch when a proven one exists:

- **Adapter/connector-registry pattern** (already in place, Stage 3): one
  interface, many providers, each provider a self-contained plug. The same
  shape as Plaid's institution connectors, Stripe's payment-method
  abstraction, or a database driver interface.
- **Schema-driven admin UI** (Stage 3's own doc comment already calls this
  out: "the UI form and validation are generated from it rather than
  hardcoded per type"). This is the same idea behind Django admin, Retool,
  or a headless-CMS content type — define the shape once, get CRUD for
  free, for every current and future type, with zero new UI code per type.
- **Connector catalog + guided setup wizard**: Zapier's "Connect a new
  app," Fivetran's connector setup flow, Segment's destination catalog.
  The common shape, independent of product: (1) pick a type from a
  catalog, (2) fill a dynamically-rendered config form, (3) test the
  connection live before it's trusted, (4) manage and monitor what's
  already connected in one table, not scattered per-client screens.
- **One mechanism, not two that are supposed to agree** — this project's
  own hard-won lesson from Stage 1 (tier checks drifting across call
  sites) and Stage 3 (`erp_access_allowed()` explicitly reusing
  `tier_rank()` rather than inventing a parallel staff path). This
  stage's design reuses that exact function again, not a new gate.

## Design

### 1. A cross-client "Conexiones ERP" admin screen (Hopdesk staff only)

Stage 3's UI lives inside each client's own **Empresas** tab — correct for
self-serve, wrong for an operator managing many clients at once. This
stage adds a second, staff-only surface: one table, one screen, every
`erp_connections.rds` row from every client, aggregated. Columns: client,
company, `short_name` (badge), `erp_type` → `label` (via
`erp_connector_label()`), `version`, `active`, health (`last_tested_at` /
`last_test_result` — already in the schema, never surfaced staff-wide
today), and an action to jump into that client's own connection (reusing
Stage 3's existing edit form via the jump mechanism — never a second,
independently-built edit path, per the "one mechanism" principle above).

Visibility: gated to `hopdesk`/`principal` tier only (Stage 1's
`TIERS_STAFF_ONLY_TABS` mechanism, the same gate every other
Hopdesk-internal screen already uses) — this is explicitly an operator
tool, not client-facing functionality like Stage 3's own screen.

### 2. The connector catalog view: what's built vs. what's planned

A read-only view of `ERP_CONNECTOR_REGISTRY` itself (`erp_connector_types()`
+ `erp_connector_label()`) — "SAP Business One — Service Layer: 5 active
connections" today — plus a plain, hand-maintained roadmap list (Odoo,
Oracle, whatever's next) with no connections yet. Not editable through the
UI — adding a real entry to the registry is unavoidably a code change (see
§4) — but staff should not have to open the codebase to know what exists
and what's coming.

### 3. Guided setup wizard for a new connection (of an existing type)

Formalizes what Stage 3's per-client form already does, generalized to any
staff workflow: pick a client → pick an `erp_type` from
`erp_connector_types()` → the form renders itself from that type's
`config_fields`/`secret_fields` (already built, Stage 3) → **Test
connection** must pass (`run_erp_connection_test()`, already built) before
the row can be saved active. No new persistence, no new validation logic —
this stage's UI work is arranging Stage 3's existing pieces for an
operator's workflow, not rebuilding them.

### 4. What actually happens when a genuinely new ERP type is added

Being direct about this rather than implying a no-code fantasy: no
serious integration platform (Fivetran, Zapier, Plaid) lets a non-engineer
define a brand-new connector purely through UI, because someone still has
to teach the system that provider's actual API shape. That work is real
and stays a developer task. What this stage can still make faster and
consistent is the *scaffold*: a documented checklist/template (a new
`R/<erp>_api.R` following `R/sap_api.R`'s exact shape: session/auth,
paginated fetch, a `homologate_fn` producing the identical canonical row
columns, a `test_fn`) plus one `ERP_CONNECTOR_REGISTRY` entry — the
registry's own comment block already sketches this shape; this stage
would turn it into an actual step-by-step doc + starter file, so "add
Odoo" is a bounded, well-defined unit of work instead of a fresh
architecture decision every time.

### 5. Health visibility, since this becomes routine

Given how often this will now happen, connections silently going stale
(expired credentials, an ERP-side API change) needs to be *visible*, not
discovered when a client's calendar goes empty. Reuse
`last_tested_at`/`last_test_result`/`last_test_message` (already in the
schema, Stage 3) and surface a simple staleness signal on the cross-client
screen (§1) — e.g., flag anything not successfully tested in N days. No
new scheduled job in this design; a manual "test all" action is enough to
start.

## Explicitly out of scope for this stage's design

- Any actual second `erp_type` (Odoo, Oracle, or otherwise) — this stage
  designs the *tooling* for adding one, not the first one itself.
- A genuinely no-code connector builder — see §4's reasoning; not a real
  pattern anywhere in the industry, not proposed here either.
- Automated/scheduled health checks — a manual "test now" action is this
  stage's whole scope; a background job is a separate, later decision.
- Any change to Stage 3's own self-serve client UI — this stage adds a
  second, staff-facing surface alongside it, not a replacement.

## Suggested build order, when it's time

1. The cross-client read (aggregate `erp_connections.rds` across every
   client folder) + the read-only table UI (§1) — the highest-value,
   lowest-risk piece; pure reads, nothing to break.
2. The connector catalog view (§2) — also read-only, trivial once §1's
   screen exists.
3. The guided setup wizard (§3) — wires existing Stage 3 functions into a
   staff-facing flow; no new persistence logic.
4. The new-connector scaffold doc + starter file (§4) — pure documentation
   and a template, whenever the first real second ERP is actually planned.
5. Health staleness surfacing (§5) — a small addition once §1 exists.

Each of these is independently shippable and independently testable — no
reason to build this as one large change when it naturally splits into
five small, low-risk ones.
