# HopDesk — Cash Flow Platform Design Notes

**Status:** Design reference, consolidated from two published design notes ("Notes to the Flow Statement" and "Wiring the Ledger"). Research and analysis only — nothing here has been implemented yet unless explicitly stated.

**Scope:** HopDesk is the product; Networks Group is the pilot client used to do this design work, not the target — everything here is meant to generalize to future, unrelated clients. Pilot client: Networks Group (logistics/crossdocking). Prepared: 28 Aug 2026 – 3 Sep 2026.

**Hard constraint carried through all of this work:** never reference "CFA" or "CFA Institute" by name anywhere in HopDesk-facing material — there is no affiliation or collaboration with CFA Institute, and naming it risks implying one. Where this document draws on CFA-curriculum-style financial analysis technique, it is described generically ("corporate financial-reporting practice," "the analytical layer").

---

# Part I — Notes to the Flow Statement

*A standards-grounded read of what a cash flow statement is required to do — corporate financial-reporting practice, IAS 7, ASC 230 — set against how Calendario actually computes cash flow today, and a design for generalizing it past a single client.*

## Note 1 — What a cash flow statement is required to do

Synthesized from corporate financial-reporting practice, IAS 7 *Statement of Cash Flows*, and ASC 230. Examples are framed against a logistics/crossdocking operator, since that's the pilot client, but nothing here is Networks-specific.

A cash flow statement exists to reconcile the change in cash and cash equivalents over a period, and to do it in a way accrual earnings can't: it shows whether a business generates cash from what it actually does, separate from how it's investing that cash and how it's funding itself. Financial-reporting practice treats the three-way split — operating, investing, financing — as the entire analytical backbone: liquidity, solvency, and financial flexibility all get read off which of the three buckets is producing or consuming cash.

### The three activities, in a logistics operator's terms

- **Operating (CFO)** — the principal revenue-producing activity and everything that isn't investing or financing. Freight/handling collections from shippers, fuel, tolls, driver payroll, warehouse rent (where not capitalized), carrier and vendor payments, taxes paid.
- **Investing (CFI)** — acquisition or disposal of long-term productive assets and other investments: trucks, trailers, forklifts, warehouse real estate, and — from a lender's own standalone books — loans made to another group entity.
- **Financing (CFF)** — transactions with the providers of capital: bank loan draws and principal repayments, the principal portion of a capitalized fleet lease, equity contributions or distributions, and — from a borrower's own standalone books — an intercompany loan received.

### IAS 7 vs ASC 230 — how much judgment each standard leaves you

IAS 7 classifies "in the manner most appropriate to the entity's business" (IAS 7.11) — the same transaction can be operating for one company and investing for another. ASC 230 leaves far less room: categories are closer to fixed. The sharpest difference shows up in interest and dividends:

| Item | IFRS (IAS 7) | US GAAP (ASC 230) |
|---|---|---|
| Interest paid | CFO or CFF — entity's choice, applied consistently | CFO only |
| Interest received | CFO or CFI | CFO only |
| Dividends paid | CFO or CFF | CFF only |
| Dividends received | CFO or CFI | CFO only |
| Income taxes paid | CFO, unless specifically traceable to financing/investing | CFO, no exceptions |

> **Forward note — IAS 7 amendment tied to IFRS 18.** IAS 7.34A, as amended alongside IFRS 18 *Presentation and Disclosure in Financial Statements*, moves most entities toward classifying interest paid as financing and interest/dividends received as investing by default — narrowing the historical either/or. Mandatory for annual periods beginning 1 Jan 2027, early adoption permitted. Any classification engine built now should treat the IFRS interest/dividend rule as a per-client, per-period setting, not a hardcoded constant — the "correct" default is about to shift under IFRS reporters specifically.
>
> **Note (added 3 Sep 2026):** this table of divergences was later judged too narrow — see Part II, Note 8's closing caveat. There are more IFRS/US-GAAP disagreement points than interest/dividends alone (income tax classification, non-cash disclosure differences, and more that the 2027 IFRS 18 alignment will introduce). Any policy-config layer built on this table must treat "where the standards disagree" as an open, extensible set, not a fixed short list.

### Direct vs indirect method — presentation only, CFO only

The split only affects how CFO is shown; CFI and CFF are identical either way, and net CFO comes out the same. **Direct method** lists gross cash receipts and payments by category — cash from customers, cash to suppliers, cash to employees, interest paid, taxes paid. **Indirect method** starts at net income and walks to CFO through non-cash add-backs (depreciation, stock comp, gains/losses on disposals) and working-capital deltas (ΔAR, ΔInventory, ΔAP, accruals). ASC 230 encourages direct but requires a supplemental indirect reconciliation even when direct is used — which is precisely why almost no one bothers with direct method in practice: it doesn't save the indirect work, it adds to it. Indirect is predominant practice among large public filers under both frameworks.

### The requirement HopDesk is closest to missing entirely: IAS 7 §44A–44E

> **IAS 7, paragraph 44A.** An entity shall provide disclosures that enable users of financial statements to evaluate changes in liabilities arising from financing activities, including both changes arising from cash flows and non-cash changes.

Paragraph 44B breaks that into: (a) changes from financing cash flows, (b) changes from obtaining or losing control of subsidiaries, (c) foreign-exchange effects, (d) fair-value changes, (e) other. In practice this is satisfied with an opening-to-closing **roll-forward** per financing liability: opening balance, plus new draws, minus principal repaid in cash, plus or minus FX and fair-value movement, equals closing balance — with only the cash rows (draws, principal repaid, interest paid) actually landing in the cash flow statement, and the non-cash rows disclosed alongside for completeness. This is a roll-forward a treasury platform is unusually well positioned to produce automatically, because it already has the amortization schedule — see Note 2.

### Consolidation and intercompany — elimination, not filtering

Under both IFRS 10 and ASC 810, 100% of intercompany balances, transactions, and cash flows are eliminated in full on consolidation, regardless of ownership percentage — not netted, not excluded row-by-row, but reversed with a mirror elimination entry, because the consolidated group cannot recognize a cash movement from trading with itself. Non-controlling-interest cash flows — dividends paid to NCI holders, NCI share buybacks — are classified as financing activities of the group. Critically, this consolidated-group elimination is a *different report* from each legal entity's own standalone statutory statement, where intercompany flows are real and must be classified normally: an intercompany loan given is CFI for the lender, an intercompany loan received is CFF for the borrower, and an intercompany cost allocation payment is CFO for whoever pays it.

### Why this matters past compliance — the analytical layer

The point of correct classification isn't just clean disclosure — it's what the numbers, once correctly classified, let an analyst read: Free Cash Flow to the Firm and to Equity; the CFO/net-income "quality of earnings" ratio, where a persistent shortfall of CFO below NI is a red flag; a common-size cash flow statement (each line as a share of revenue or of total inflows) for trend and peer comparison; coverage ratios like CFO/current liabilities and cash-flow-to-capex; and reading the sign pattern across CFO/CFI/CFF as a life-cycle signature — a growth entity typically runs −CFO/−CFI/+CFF (borrowing to fund both operations and growth), a mature one +CFO/−CFI/−CFF (self-funding, returning capital), a declining one +CFO/+CFI/−CFF (divesting, paying down debt). None of this is available until the underlying flows are actually classified — which is the gap Note 2 and Note 3 walk through.

## Note 2 — HopDesk today, as built for Networks

Traced directly against `R/ledger_module.R`, `R/data_pipeline.R`, `R/pasivos_calendar_glue.R`, `R/cashflow_preview_module.R`, `R/cashflow_export_module.R`, `R/interco_module.R`, `R/treasury_map_module.R`, and `R/bancos_module.R`.

Calendario is a well-built **direct-method receivables/payables scheduler**. Every figure it shows is a projected or scheduled AR/AP amount, grouped by counterparty and due date — structurally close to how a direct-method CFO section reads. What it doesn't yet have is the classification and consolidation layer that turns those flows into a statement.

**Three data sources, all projected, none realized** — `data_pipeline.R:159–393`
SAP open AR/AP balances, manual entries, and pasivos provisions all merge in `build_ledger_df()`. Real bank transactions from Bancos are never a flow-amount source — they only clear a projected invoice once matched, via `compute_confirmed_flags()` (`data_pipeline.R:626`). The date driving every row, `FechaEff`, is a projected/policy due date, not a settlement date.

**Classification is AR-vs-AP only — no CFO/CFI/CFF anywhere** — `data_pipeline.R:173, 189, 209` · `ledger_module.R:970, 2557`
The only axis is direction of money — "Cobros" vs "Pagos." A real classification field exists one layer down: pasivos carry `categoria` (`regular` / `financiero` / `tarjeta`) and, for financed liabilities, a `flavor` (`credito_simple` / `arrendamiento` / `linea_revolvente` / `otro`). It's computed and attached to ledger rows in `pasivos_provisions_as_ledger_rows()` — then never read again downstream. It's the closest thing to a CFI/CFF tag already in the codebase, and it's currently orphaned.

**Loan principal and interest are computed, then collapsed** — `pasivos_engine.R:245–540, 19` · `pasivos_calendar_glue.R:92–93`
`pasivos_engine.R` internally computes `componente_capital` / `componente_interes` / `componente_fees` for every amortization instance — but by the time a provision reaches the calendar, it's flattened to one lump `amount_pago`. A loan repayment and a vendor payment render identically. This is the specific gap that IAS 7 §44A's financing-liability roll-forward (Note 1) is designed to catch.

**Intercompany is a filter, not an elimination** — `data_pipeline.R:398–449` · `cashflow_export_module.R:383–394`
`apply_ic_filter()` supports `exclude / include / only` — a row-level keep-or-drop, applied identically across Calendario, Preview, and Export. There's no netting logic. With the default `include` and more than one company selected, an intercompany invoice is counted on both legs — once as AR under the seller, once as AP under the buyer — a real double-count against the IFRS 10 / ASC 810 elimination requirement in Note 1.

**No consolidation model — company selection is just a row filter** — `ledger_module.R:267–277` · `cashflow_export_module.R:430–434`
Each invoice belongs to exactly one `Empresa`. Toggling the company "bubbles" changes which rows get summed; there's no separate consolidated-group total, eliminated or otherwise. `ic_mode="exclude"` is the closest thing to a group view today, and it's a blunt row-drop rather than a true elimination entry.

**The main grid never nets AR against AP** — `global.R:492–863, 700–722` · `cashflow_export_module.R:262–289`
The calendar heat-map shows one ledger at a time (Cobros *or* Pagos) with a per-day total — no running balance anywhere in `ledger_module.R` or `global.R`. The only place AR and AP combine into a net position is the Preview/Export pipeline, via `FLUJO NETO` and a cumulative `POS. ACUMULADA` row (`.build_summary()`, `cashflow_export_module.R:262`) — and Preview and Export share that exact same pipeline; Export just adds document formatting on top.

## Note 3 — Gap matrix

Each standards requirement from Note 1, checked against the Note 2 trace.

| Requirement | Status | Where it stands today |
|---|---|---|
| CFO / CFI / CFF classification | ❌ missing | Only AR/AP direction exists |
| Direct-method line items (by category, not just AR/AP) | ⚠️ partial | Direct-method *shape* is there; no category taxonomy |
| Financing-liability roll-forward (IAS 7 §44A) | ⚠️ partial | Principal/interest split exists in `pasivos_engine.R`, unexposed |
| Intercompany elimination (IFRS 10 / ASC 810) | ❌ missing | Row filter (`ic_mode`) can double-count instead |
| Consolidated vs. standalone entity views | ❌ missing | Company toggle only; no eliminated group total |
| NCI cash flow handling | ❌ missing | No ownership-percentage concept in the entity model |
| Multi-framework interest/dividend rules | ❌ missing | No interest/dividend concept at all yet — all pasivos flows are generic |
| Indirect-method reconciliation | ❌ missing | Would need full P&L ingestion, not just AR/AP |
| Running net position (FLUJO NETO) | ✅ present | Preview/Export only, not the main calendar grid |
| Common-size / ratio analytics | ❌ missing | No revenue or peer-comparison layer |
| Actuals-vs-forecast variance | ❌ missing | Confirmed invoices are dropped, not retained for comparison |

## Note 4 — The generalized design

Framed for the platform, not the pilot client — every piece below is meant to onboard a non-logistics client through configuration, not new code.

**4.1 — A configurable taxonomy engine.** Tag flow type at the lowest granularity that already exists — chart-of-accounts entry, vendor category, or the pasivos `categoria`/`flavor` pair — and map that tag to CFO/CFI/CFF through a rule set, not a hardcoded switch. Ship a sensible default mapping (fleet purchase → CFI, loan principal → CFF, loan interest → CFO, vendor/carrier payment → CFO), let each client override it, and keep an audit trail of reclassifications — IAS 7 requires a consistent method year over year, so the platform needs to remember what a client chose and flag drift.

**4.2 — A real entity & consolidation model.** Replace the flat `Empresa` filter with a legal-entity hierarchy carrying ownership percentages. Build an elimination engine that matches an intercompany AR leg against its AP counterpart and produces an actual elimination entry — not a row drop — so the platform can render **both** a consolidated (eliminated) group view and a standalone entity view as toggles, per the "both, as separate toggles" decision already made for Networks. NCI cash flows (distributions, buybacks) get their own CFF tag once ownership percentage exists in the model.

**4.3 — Financing-liability continuity.** Surface the principal/interest/fees split `pasivos_engine.R` already computes internally. Add a per-liability roll-forward view — opening balance, cash draws, principal repaid, FX movement, fair-value movement, closing balance — which is exactly what IAS 7 §44A asks for, and route the cash rows to CFF (principal) and CFO (interest) automatically instead of one undifferentiated "pago."

**4.4 — Direct method native, indirect method on request.** Keep direct method as the native presentation — it already matches the transactional grain of the ledger. Offer indirect-method reconciliation as an add-on report for clients whose reporting standard expects it, but treat it as a real scope boundary: indirect method needs a full income statement (revenue, COGS, D&A, non-cash items), which HopDesk doesn't currently ingest — this isn't a formatting toggle, it's a new data source.

**4.5 — Multi-currency — decide what "fused" means.** The current spot/estimated-rate fusion is a good treasury-liquidity view (how much cash, in one currency, is available right now). It is not IAS 21-grade statutory translation, which distinguishes functional currency, translates flow items at an average rate and balance items at a closing rate, and tracks a cumulative translation adjustment reserve. These produce genuinely different numbers. See Note 6 — this is a decision, not a default.

**4.6 — An analytics layer.** Once flows are classified, the analytical layer from Note 1 becomes cheap to add and is where the platform stops being a scheduler and starts being an analysis product: common-size cash flow (each line over revenue), the CFO/net-income quality ratio, cash-flow coverage ratios, and a life-cycle signature read across CFO/CFI/CFF sign patterns per entity.

**4.7 — What onboarding a second, unrelated client actually needs.** A client-level configuration object: entity tree with ownership percentages, reporting-framework preset (IFRS-flexible / US-GAAP-fixed / a named local GAAP), chart-of-accounts-to-taxonomy mapping, base currency, and an intercompany registry. A manufacturing or SaaS client should be onboardable by filling in that configuration — zero new R code, the same engine Networks uses.

## Note 5 — Migration bridge

What already exists in the Networks build, and what it becomes under the generalized design.

| Exists today | Becomes |
|---|---|
| `pasivos_categoria` / `flavor` | Seed values for the taxonomy engine's default CFO/CFI/CFF mapping (4.1) |
| `ic_mode` (exclude/include/only) | Becomes a view toggle on top of a true elimination engine, not the elimination itself (4.2) |
| `amount_pago` (lump sum) | Stays as the cash figure, but is joined by the already-computed principal/interest/fees split for the roll-forward view (4.3) |
| `build_ledger_df()` | Ingestion stays the same shape; a taxonomy tag gets attached at merge time instead of being computed and dropped |
| `Empresa` row filter | One leaf of the entity hierarchy in 4.2, not the whole consolidation model |
| `.build_summary()` FLUJO NETO / POS. ACUMULADA | Extends naturally into the CFO/CFI/CFF subtotal breakdown once 4.1 exists — same cumulative-sum mechanics |

## Note 6 — Open decisions

Genuine forks — the "right" answer depends on what HopDesk is claiming to produce, not on the code.

**What does "fused currency" promise the user?** Today's spot-rate fusion is honest as a liquidity view but would misstate a statutory cash flow statement if presented as one.
- *Path A — treasury view:* Keep spot/estimated-rate fusion, label it explicitly as a liquidity/cash-positioning figure, and never claim statement-grade accuracy.
- *Path B — statutory grade:* Build IAS 21 functional-currency translation (average rate for flows, closing rate for balances, CTA reserve) as a separate, more expensive mode for clients that need real financial statements out of HopDesk.

**How far does intercompany elimination reach?** Matched invoice pairs are the safe, provable case — but not the only place intercompany cash actually moves.
- *Path A — invoice-matched only:* Eliminate only IC flows that trace to a matched AR/AP invoice pair. Safe and auditable, but misses shared-cost allocations or transfers booked outside SAP.
- *Path B — registry-driven:* Elimination follows the broader IC registry (already used for filtering today), catching more real intercompany cash movement — at the cost of needing a way to flag and reconcile non-invoice IC activity.

**What's the default reporting framework for a new client?** The default shapes every future client's first impression, and IFRS and US GAAP genuinely disagree on where interest and dividends land (Note 1).
- *Path A — IFRS-first:* Closer to Mexican NIF B-2 (serves Networks well) and the more common basis for future non-US clients, given the "eventually every company in the world" ambition.
- *Path B — US GAAP-first:* Fixed, less judgment-heavy categories are simpler to default correctly and explain, and may match more near-term prospective clients.

### Sources (Part I)

1. [IAS 7 — Statement of Cash Flows, IAS Plus (Deloitte)](https://www.iasplus.com/en/standards/ias/ias7)
2. [Statement of Cash Flows Under ASC 230, BDO](https://arch.bdo.com/Statement-of-Cash-Flows-Under-ASC-230)
3. [IAS 7 §44A–44E agenda decision, IFRS Foundation](https://www.ifrs.org/content/dam/ifrs/supporting-implementation/agenda-decisions/2026/disclosure-of-changes-in-liabilities-arising-from-financing-activities-jan-26.pdf)
4. [Statement of cash flows: IFRS Accounting Standards vs. US GAAP, KPMG](https://kpmg.com/us/en/articles/2023/ifrs-accounting-standards-us-gaap1.html)
5. [Understanding ASC 810: Intercompany Elimination Rules, Houseblend](https://www.houseblend.io/articles/asc-810-intercompany-elimination-rules)

---

# Part II — Wiring the Ledger

*Turning ERP data — SAP today, anything tomorrow — into classified cash flows and analytics. Verified live against Networks' own SAP instance, not assumed from documentation. Companion to Part I.*

## Note 1 — What's blocking richer data today

Not a literature review — this was run live against Networks' production SAP Business One instance (company NG, VPN-connected, read-only, logged out cleanly after each probe).

Today, `R/sap_api.R` pulls exactly two entities — `Invoices` and `PurchaseInvoices` — and from each, eleven header fields (`DocNum`, `CardCode`, `DocTotal`, `DocTotalFc`, `PaidToDate`, etc.). Everything Part I flagged as missing — GL classification, realized payments, financing-liability detail — is missing because nothing else has ever been requested from SAP, not because SAP doesn't have it. It does. Most of it is just not authorized for the account HopDesk currently connects with.

### The permission wall

Querying each entity with the live `SAP_NG_*` credentials from `erp_connections.rds`/`.Renviron`:

```
GET ChartOfAccounts   →  403  -3000  "The logged-on user does not have permission to use this object."
GET JournalEntries    →  403  -3000  "The logged-on user does not have permission to use this object."
GET IncomingPayments  →  403  -3000  "The logged-on user does not have permission to use this object."
GET Budgets           →  403  -3000  "The logged-on user does not have permission to use this object."
GET ChecksforPayment  →  403  -3000  "The logged-on user does not have permission to use this object."
GET BankStatements    →  200  OK — accessible, 0 rows (native SAP bank-statement import is unused; Bancos parses TXT exports instead)
GET Invoices          →  200  OK — already in production use
GET PurchaseInvoices  →  200  OK — already in production use
```

Five different object types, one identical SAP error code (`-3000`) across all of them — this is a single authorization gap, not five separate ones. The `LARivera` integration user's General Authorizations profile covers Sales (AR) and Purchasing (AP) documents only; it has no grant anywhere in Financials or Banking. This matches — and now extends — what was already known about this account (blocked from `BusinessPartners` too).

> **Resource needed.** A Networks SAP admin (Basis/IT) needs to extend the integration user's **General Authorizations** — Administration → System Initialization → Authorizations — to grant *read-only* access to: **Financials** (Chart of Accounts, Journal Entries, Cash Flow setup) and **Banking** (Incoming Payments, Outgoing Payments, Checks for Payment). No create/update/delete authorization is needed anywhere for this work — every extraction proposed here is read-only. Best practice would be a dedicated least-privilege integration user rather than widening `LARivera`'s own access, but either satisfies the technical requirement.

### The bigger finding: SAP already has a cash-flow taxonomy engine built in

The permission wall blocks *data*, but not *schema* — the Service Layer's `$metadata` document is readable regardless, and it describes something worth knowing about before proposing to build a classification engine from scratch:

| Object | Confirmed fields (live schema) | What it's for |
|---|---|---|
| `ChartOfAccounts` | `CashAccount`, `CashFlowRelevant` (both `SAPB1.BoYesNoEnum`, of 89 total properties) | Marks which accounts *are* cash/bank (excluded from the flow itself) vs. which accounts' movements should *appear* in a cash flow report |
| `CashFlowLineItem` | `LineItemName`, `ActiveLineItem`, `ParentArticle`, `Level`, `Drawer` | A hierarchical taxonomy of named line items (parent/child, nesting level) — this *is* a CFO/CFI/CFF-style category tree, native to SAP |
| `CashFlowAssignment` | `CashFlowLineItemID`, `Credit`, `PaymentMeans`, `CheckNumber`, `AmountLC`, `AmountFC`, `JDTLineId` | Links a specific GL posting line (`JDTLineId`) to a line-item category, with the payment instrument and both local/foreign amounts |

This is SAP Business One's native **indirect cash flow report** engine (Administration → Setup → Financials → Cash Flow Line Items, then a "Cash Flow Relevant" flag + category per account, populated as incoming/outgoing payments are entered). Whether Networks' finance team has actually configured it — whether `CashFlowLineItem` has real rows, whether accounts carry real `CashFlowRelevant` flags — is unknown from here; that needs the same permission grant above to check. **This is the single highest-leverage thing to verify next**: if it's populated, HopDesk doesn't need to invent a classification scheme from nothing — it can ingest one an accountant has already curated, the same way the SAP-native cash flow report already would.

Testing the corresponding service endpoint directly (`CashFlowLineItemsService_GetCashFlowLineItemList`) also returned a permission error — SAP error `-6006`, "Modifying this object is not permitted for current user" — confirming this whole area sits behind the same authorization gap, not a separate technical limitation.

### The rest of the object landscape

Correcting a few entity names along the way (Service Layer's real names don't always match intuition — `ChecksforPayment`, not `Checks`; Fixed Assets are `AssetDocument`/`AssetGroup`/`AssetClass`/`AssetDepreciationGroup`, not one `FixedAssets` collection):

```
Entity existence check (349 entity types in schema)

ChecksforPayment  →  exists, same -3000 permission block
AssetDocument, AssetGroup, AssetClass, AssetDepreciationGroup  →  exist in schema
    (collection path not yet resolved — worth a follow-up pass once Financials
    access is granted; likely the same authorization group)
Deposit, InternalReconciliation  →  exist as types, not confirmed as directly
    queryable top-level collections
```

## Note 2 — The canonical financial data model

The ERP-agnostic core everything else builds on. Grounded in what actually got verified against SAP, plus a sourced survey of NetSuite, Business Central, QuickBooks Online, Xero, and Odoo — not because any of these is a near-term target, but because a model built to fit only SAP's shape isn't ERP-agnostic, it's SAP-agnostic-in-name-only.

Every ERP surveyed — regardless of vendor, market tier, or API style — exposes some account-level classification field, because every one of them ultimately renders the same universal accounting split: **Asset / Liability / Equity / Revenue / Expense**. That five-way split is the actual GAAP/IFRS bedrock (it's how ASC 210/220 and IAS 1 require a balance sheet and income statement to be organized, and how the XBRL `us-gaap` taxonomy roots its element hierarchy) — it's not a SAP convention or a NetSuite convention, it's the thing every one of these vendor-specific fields is a dialect of. That's what makes one default mapping table generalize across ERPs.

| ERP | Classification field | Cash/bank marker | Native CF taxonomy? |
|---|---|---|---|
| SAP Business One | n/a (no generic type field) | `CashAccount` | Yes — CashFlowRelevant + CashFlowLineItem tree (Note 1) |
| NetSuite | `Account.accttype` | `accttype` = "Bank" | No — reports derive CFO/CFI/CFF from accttype via a fixed rule, not a stored flag |
| MS Dynamics 365 BC | `Account.category` (6-value enum) | — (bank ledger entries not in the stable API) | No |
| QuickBooks Online | `Account.AccountType` + `AccountSubType` | AccountType = "Bank" | No — richest granularity of the group, still no CF flag |
| Xero | `Account.Type` + `Account.Class` | Type = "BANK" | No |
| Odoo | `account.account.account_type` | account_type = "asset_cash" | No |
| ISO 20022 (bank side) | `BkTxCd` → `Domn`/`Fmly`/`SubFmlyCd` | — (account itself is implicit — this codes the transaction) | Partial — classifies transaction *nature*, needs an inference layer on top to reach CFO/CFI/CFF |

SAP is the outlier in a specific way: it's the only one with a purpose-built cash-flow category tree, but it has no generic "account type" field the way the others do — `CashFlowRelevant` is boolean, not typed. The canonical model needs both concepts, because different ERPs will supply one, the other, or (best case, if Networks has it configured) both.

### Canonical entities

Six entities, English field names, ERP-neutral — this is what every adapter (SAP today, whatever's next) targets. Each row also notes what SAP currently gives us today, blocked or not:

| Canonical entity | Core fields | SAP source (Note 1) |
|---|---|---|
| `Account` | code, name, class(Asset/Liab/Equity/Rev/Exp), is_cash, cf_relevant, cf_category_id | ChartOfAccounts — ❌ blocked |
| `CFCategory` | id, name, parent_id, level | CashFlowLineItem — ❌ blocked |
| `JournalLine` | entry_id, line_id, account_code, debit, credit, posting_date, cf_category_id | JournalEntries — ❌ blocked |
| `ARInvoice` / `APInvoice` | doc_num, counterparty_code, counterparty_name, tax_id, doc_date, due_date, currency, amount_lc, amount_fc, paid_to_date | Invoices / PurchaseInvoices — ✅ live today |
| `Payment` | doc_num, direction(in/out), counterparty_code, bank_account, means(cash/check/transfer/card), doc_date, amount, linked_invoice_ids, cf_category_id | IncomingPayments / OutgoingPayments / ChecksforPayment — ❌ blocked |
| `FinancingLiability` | id, counterparty, opening_balance, principal_component, interest_component, fx_delta, closing_balance | Not in SAP schema as such — this is `pasivos_engine.R`'s own amortization model |

> **On pulling full financial statements.** Once JournalEntries access exists, a trial balance is just JournalLine grouped by account_code and summed — and a trial balance is the seed for everything past it: an actual P&L, an actual balance sheet, and — critically — the indirect-method reconciliation Part I flagged as needing "a new data source, not a formatting toggle." This one permission grant is what turns that from a scope boundary into a straightforward extension.

## Note 3 — The adapter contract

Today, adding an ERP means writing a new `R/<erp>_api.R` that "follows `sap_api.R`'s shape" — an informal instruction, not a contract. `.parse_invoices()` transmutes SAP's raw fields directly into HopDesk's internal, Spanish-labeled ledger columns (`Documento`, `Parte`, `Saldo vencido`...) in one step. That collapses two genuinely different concerns into one function: "what can this ERP give me" and "how does HopDesk want it shaped." Every future adapter has to re-derive both from scratch, and any change to HopDesk's internal shape means touching every adapter that's ever been written.

### The proposed split

1. **Adapter → canonical.** Each `R/<erp>_api.R` emits *only* the Note 2 canonical schema, in English, for whichever entities that ERP and that connection's permissions actually support. It knows SAP (or NetSuite, or whatever) intimately. It knows nothing about HopDesk's Calendario, Spanish labels, or how cash flow gets classified.
2. **Canonical → HopDesk.** One shared function turns canonical rows into today's internal ledger shape, the classification/consolidation engine's input, and eventually the analytics layer's input. It's written once and knows nothing about SAP, NetSuite, or any specific ERP.

### Capability flags — because no two connections will offer the same data

Right now, SAP genuinely only offers two of the six canonical entities to this connection (`ARInvoice`, `APInvoice` — the rest is schema-present but permission-blocked, per Note 1). A future Odoo or NetSuite connection might offer a completely different subset for entirely different reasons — a smaller ERP, a different admin's authorization choices, a module that isn't licensed. The registry pattern this codebase already uses for connection credentials (`ERP_CONNECTOR_REGISTRY`) extends naturally to this:

| Connection | Capabilities |
|---|---|
| SAP NG (today, as authorized) | `ar_invoices`, `ap_invoices` |
| SAP NG (after the Note 1 grant) | `ar_invoices`, `ap_invoices`, `chart_of_accounts`, `gl_journal`, `payments`, `native_cf_taxonomy` |

The classification and analytics layers read a connection's capability flags and degrade gracefully — a client with only `ar_invoices`/`ap_invoices` still gets the AR/AP scheduler HopDesk already does well; a client with `gl_journal` additionally gets real CFO/CFI/CFF classification and, eventually, indirect-method reports. Nothing breaks for a thinner connection; richer connections simply unlock more.

## Note 4 — The pipeline

How the pieces connect, end to end (deliberately not a UI diagram — this is the data spine everything else, including any future UI, would sit on top of):

```
ERP (Any ERP)
  SAP B1 — R/sap_api.R  ─┐
  Next ERP — R/<erp>_api.R ─┤→ canonical schema, capability-flagged →
                             ↓
       Canonical Financial Data Model
       (Account · CFCategory · JournalLine · AR/APInvoice · Payment · FinancingLiability)
                             ↓
       Classification Engine
       1. SAP-native CashFlowLineItem, if configured
       2. Account-class default mapping (Note 2)
       3. Client override table
                             ↓
       Consolidation / Elimination Engine
       (entity hierarchy + IC elimination, Part I Note 4.2)
                             ↓
       Analytics & Reporting
       (CFO/CFI/CFF statement · ratios · roll-forwards)
                             ↓
       Existing surfaces: Calendario · Preview · Export
```

## Note 5 — What to pull, in order

Sequenced by dependency, not by ambition — each phase is usable on its own.

**Phase 1 — Formalize what's already flowing.** Refactor `.parse_invoices()`'s output into the Note 2 canonical `ARInvoice`/`APInvoice` shape, with the canonical→HopDesk mapping (Note 3) as a separate step. Zero new SAP data, zero new permissions — pure structural groundwork that every later phase depends on.

**Phase 2 — Unlock GL truth and realized cash.** Contingent on the Note 1 authorization grant. Add `ChartOfAccounts` + `CashFlowLineItem` + `CashFlowAssignment` (read whatever taxonomy Networks' own finance team may have already built), `JournalEntries` (GL truth — trial balance becomes possible), and `IncomingPayments`/`OutgoingPayments`/`ChecksforPayment` (realized cash, replacing or cross-validating the manually-parsed Bancos TXT pipeline — see the open fork in Note 11). This is where CFO/CFI/CFF classification, financing-liability roll-forwards, and actuals-vs-forecast variance all become buildable.

**Phase 3 — Full statements and the analytics layer.** Fixed-asset events (once the correct Service Layer paths for `AssetDocument`/`AssetGroup` are resolved) for real CFI classification; a derived P&L and balance sheet from JournalLine for indirect-method reporting; and the ratio/common-size/FCFF analytics from Part I's Note 1.6, now backed by real classified data instead of AR/AP aggregates.

## Note 7 — One client, several ERPs

Not a hypothetical — Networks runs SAP as the system of record and CONTPAQi for fast CFDI invoice generation, at the same time. A canonical model scoped to "one client → one ERP" is already wrong for the pilot client.

This is actually two different problems that look alike from a distance:

**Shape A — different companies, different systems.** Some legal entities in the group use SAP, others use something else entirely. No overlap, no double-counting risk — just two adapters active for one client, each owning a disjoint set of companies. This is already close to supported: `erp_connections.rds` scopes a connection to a `company_initials` array today specifically so one client can hold several connections. Nothing new needed here beyond a second working adapter.

**Shape B — the same company, two systems, overlapping data.** One invoice gets created in CONTPAQi (for quick CFDI stamping) and the same transaction also needs to land in SAP's books. Counted from both sources, it's double-counted. This is the real design problem, and it's the one worth solving well rather than working around.

### The CFDI answer — ingest the government's document, not the software's database

CONTPAQi's desktop products (the tier almost certainly in use here) expose no vendor REST API — only a Windows-only COM SDK requiring a local install, or reading the underlying SQL Server tables directly (both real, documented integration patterns, both meaningfully heavier than the alternative below). But Mexican law forces every invoicing system — CONTPAQi, SAP's own CFDI add-on, Aspel, anything — to emit the **same government-defined XML schema** (CFDI 4.0, SAT "Anexo 20") when it stamps an invoice. That shared, mandated format is the actual ERP-agnostic integration point for any Mexican client, not a per-vendor adapter.

> **CFDI structure that matters for cash flow.** Comprobante carries DocDate-equivalent (Fecha), Total, Moneda, Emisor/Receptor RFC — the invoice itself. But for deferred-payment invoices (PPD), the invoice XML alone does NOT tell you when cash moved. A separate "Pago" complement (REP), issued at actual payment time, carries FechaPago, FormaDePagoP, Monto, and a DoctoRelacionado/IdDocumento pointing back to the original invoice's UUID plus NumParcialidad for installment tracking. For a cash-flow platform, the REP complement — not the invoice — is the realized-payment data source.

The natural dedup key across CONTPAQi, SAP, and anything else is the CFDI's **`TimbreFiscalDigital/@UUID`** — assigned once, externally, by the government-authorized stamping provider at issuance time, never reissued even if the document is later cancelled and replaced. That's meaningfully more solid than the two-tier fuzzy key `dedup_movimientos()` already uses for Bancos (built because bank-statement descriptions drift across overlapping exports) — a UUID doesn't drift, because nothing about how it's assigned depends on which software is asking. Practical grain: dedup on `(UUID)` for invoices, `(REP UUID, DoctoRelacionado/IdDocumento, NumParcialidad)` for payments, since one payment can apply against several invoices and one invoice can collect several partial payments over time.

**Fork — what's the dedup key for a future client with no CFDI-equivalent?** CFDI's UUID is a Mexico-specific gift — a government-issued, software-agnostic natural key. Nothing guarantees the same exists elsewhere.
- *Path A — per-jurisdiction natural key:* Where one exists (a bank-assigned ISO 20022 EndToEndId, a national e-invoicing UUID like CFDI's), use it. Make the dedup key pluggable per connection, not hardcoded to CFDI.
- *Path B — fall back to the fuzzy key:* Where no natural key exists, reuse the same two-tier fuzzy-match pattern `dedup_movimientos()` already validated for Bancos — proven, not reinvented.

SAT's own "Descarga Masiva" bulk CFDI download exists and is genuinely ERP-agnostic, but requires custody of the client's e.firma private key — a heavier trust ask than parsing files the client's own systems already produce. Better suited as a periodic reconciliation/gap-check than the primary ingestion path.

## Note 8 — Borrowing a category, not inventing one

The category problem, solved by adopting an existing, externally-governed vocabulary — because a scheme HopDesk has to invent is also a scheme HopDesk has to maintain forever, across every ERP, alone.

Telling signal from the research: none of the accounting-data aggregation platforms that already solve "normalize many ERPs into one schema" for a living (Codat, Merge, Rutter — each processing dozens of accounting systems) have independently built a cash-flow operating/investing/financing classification layer. All of them stop at balance-sheet/income-statement account classification, converging independently on the same five root classes: **Asset, Liability, Equity, Revenue, Expense**. That convergence — three unrelated companies solving the same integration problem and landing in the same place — is itself evidence this part is settled. The one place that absence stops is exactly HopDesk's problem: nobody in that space has built the operating/investing/financing layer, because there's already an internationally governed answer for it.

### Account classification — reuse a proven enum, don't design one

Adopt the converged shape directly rather than inventing HopDesk's own — concretely, the same root classes plus a sub-type enum matching what's already battle-tested across 60+ accounting platforms (Merge.dev's publicly documented `Account` model):

| Root class | Sub-type examples (borrowed, not invented) |
|---|---|
| Asset | `BANK`, `ACCOUNTS_RECEIVABLE`, `FIXED_ASSET`, `OTHER_CURRENT_ASSET`, `OTHER_ASSET` |
| Liability | `ACCOUNTS_PAYABLE`, `CREDIT_CARD`, `OTHER_CURRENT_LIABILITY`, `LONG_TERM_LIABILITY` |
| Equity | — (rarely subdivided further) |
| Revenue | `OTHER_INCOME` |
| Expense | `COST_OF_GOODS_SOLD`, `OTHER_EXPENSE` |

### Cash-flow classification — the IFRS and US GAAP taxonomies, not a bespoke scheme

Both are real, annually-maintained, internationally governed vocabularies with actual published element names for exactly this layer — adopt them directly as HopDesk's canonical `cf_category` values rather than naming categories ourselves:

| Concept | IFRS Taxonomy element | US GAAP Taxonomy element |
|---|---|---|
| Operating total | `CashFlowsFromUsedInOperatingActivities` | `NetCashProvidedByUsedInOperatingActivities` |
| Investing total | `CashFlowsFromUsedInInvestingActivities` | `NetCashProvidedByUsedInInvestingActivities` |
| Financing total | `CashFlowsFromUsedInFinancingActivities` | `NetCashProvidedByUsedInFinancingActivities` |
| Customer receipts | `ReceiptsFromSalesOfGoodsAndRenderingOfServices` | — (no dedicated element; folded into the CFO total) |
| PPE purchase | `PurchaseOfPropertyPlantAndEquipmentClassifiedAsInvestingActivities` | `PaymentsToAcquirePropertyPlantAndEquipment` |
| Borrowing proceeds | `ProceedsFromBorrowingsClassifiedAsFinancingActivities` | `ProceedsFromIssuanceOfLongTermDebt` |
| Borrowing repayment | (implied opposite leg) | `RepaymentsOfLongTermDebt` |
| Dividends paid | `DividendsPaidClassifiedAsFinancingActivities` | `PaymentsOfDividendsCommonStock` |

> **Where the two vocabularies genuinely disagree.** No ready-made element-by-element crosswalk exists between the IFRS and US GAAP taxonomies — that has to be maintained in-house. The good news: it's small(ish). Every disagreement traces back to a handful of items — interest paid, interest received, dividends paid, dividends received, income tax classification, and more likely to surface once local GAAPs beyond Mexico enter the picture — where IFRS leaves the classification to entity policy and US GAAP fixes it. Model this as an explicit, **open-ended policy config** per client/framework (interest_paid → CFO|CFF, interest_received → CFO|CFI, dividends_paid → CFO|CFF, dividends_received → CFO|CFI, plus room to add more axes as they're identified), not a sprawling ontology to keep in sync, and **not a fixed short list either** — this table was originally scoped to exactly four fields and that turned out to be too narrow; treat "where the standards disagree" as a set that grows, not a closed one.

XBRL GL (the Global Ledger Taxonomy Framework) was also evaluated as a candidate for the underlying GL/journal-entry structure — it's real, maintained by XBRL International, and its `gl-cor` module is a reasonable conceptual reference for how to model accounts/entries/agents generically. But it hasn't had a substantive update since 2015, has thin tooling support next to the reporting taxonomies, and isn't a wire format worth depending on. Read it, don't build on it.

Net effect for the "test of time and scale" concern: HopDesk maintains almost nothing here. Two vocabularies are pinned to external, annually-refreshed governance bodies (the IFRS Foundation and FASB); the only in-house artifact is a small, mostly-static crosswalk over a handful of known divergence points.

## Note 9 — The Calendario toggle

Design only — not built yet. Reuses the existing cash-flow table rather than replacing it.

The Preview/Export table already computes everything a Structured view needs — `.build_summary()` already groups rows into Cobros/Pagos and produces FLUJO NETO and a cumulative POS. ACUMULADA. The toggle doesn't need a second pipeline: it needs the same rows re-grouped by a `cf_category` tag (Note 8's vocabulary) instead of by counterparty.

| | Basic (today) | Structured (new) |
|---|---|---|
| Grouping | Cobros / Pagos, by counterparty | Operaciones (CFO) / Inversión (CFI) / Financiamiento (CFF) |
| Bottom line | FLUJO NETO | FCF — same number, different breakdown above it |
| Running total | POS. ACUMULADA | Identical — a cumulative sum doesn't care how the rows above it were grouped |

> **The invariant worth stating explicitly.** Today's FLUJO NETO and tomorrow's FCF are the same total, always. The toggle changes how the number is explained, never what the number is — which is exactly why this is safe to build as a view-layer switch on `build_cashflow_export_data()`'s existing output, not a second data pipeline to keep in sync with the first.

Because Preview and Export already consume the identical `export_data` structure, a `view_mode` flag living once at that shared layer means every export format — Word, HTML, Excel — inherits both views automatically the moment the shared builder respects it. No per-format work. The same data-builder is also the natural thing to eventually expose as a standing report in the Reporte module, rather than only as a Calendario-adjacent toggle — not scoped now, just noted so the builder isn't designed as if Calendario were its only caller.

## Note 10 — How far can we get today

With zero new SAP permissions — what Phase 1 alone actually buys, honestly stated.

| Statement | Status today | What's missing |
|---|---|---|
| Statement of cash flows | ⚠️ partial | CFO and most of CFF achievable now (see below) — CFI is essentially empty without a manual tag or Phase 2 |
| Estado de Resultados (income statement) | ❌ proxy only | No revenue/expense recognition data exists — only an invoice-header proxy (see below), never to be labeled a real income statement |
| Balance sheet | ❌ not attainable | Needs ChartOfAccounts + JournalEntries for every account, not just AR/AP — squarely Phase 2 |
| Notes / additional disclosure (financing-liability roll-forward) | ✅ buildable now | Nothing — `pasivos_engine.R` already computes the principal/interest/fees split internally, it just needs exposing downstream |

### The cash flow statement, specifically

Everything not tagged otherwise defaults to CFO — AR collections, ordinary vendor payments. The already-computed (but currently orphaned) `pasivos_categoria`/`flavor` field routes "financiero" liabilities to CFF, and the already-computed `componente_capital`/`componente_interes` split lets principal land in CFF and interest in CFO, automatically, using data that already exists today. What's genuinely missing: loan *draws* (proceeds received) have no data source at all yet — only repayments are tracked — and CFI has nothing to populate it, since no field anywhere marks an invoice as capex-related.

**A cheap way to make CFI non-empty before Phase 2:** let a manual AP entry or pasivos row carry an optional "es inversión" tag — pure data entry, zero new ERP integration. Doesn't need SAP FixedAssets access to start being useful; it just needs someone to check a box on the handful of invoices that are genuinely capex.

### The income statement proxy — labeled honestly, or not at all

AR invoice `DocDate` × `DocTotal`, summed by period, approximates revenue billed; the same for AP approximates purchases booked. This is *not* a real income statement — no COGS matching, no non-invoiced expenses (payroll, fuel, depreciation), no accrual adjustments — and should be labeled something like "Proxy de facturación," never "Estado de Resultados," anywhere it surfaces. It does, however, unblock the denominator for the CFO/revenue common-size ratios Part I flagged as blocked entirely — a proxy denominator is better than none, as long as it's never presented as more than that.

## Note 11 — Open asks

What this note needs from someone other than a model reading code — the point of a director asking for resources instead of working around not having them.

1. **SAP authorization grant (Note 1).** Read-only General Authorizations for Financials + Banking, for the integration user. This is the one blocker every later phase depends on — everything in Phase 2 and 3 is designed and ready to build the moment it's granted.
2. **Where do Networks' CFDI files actually live?** Note 7's design assumes CFDI XML (and REP payment complements) are reachable somewhere — a shared folder, an accounting-firm handoff, CONTPAQi's own export. That's not yet confirmed. Worth checking before committing to this as the CONTPAQi ingestion path.
3. **The interest/dividend classification policy (Note 8).** A business decision per client/framework, not something to infer silently — needs an explicit owner once the classification engine is actually built.

**Fork — should Bancos keep parsing bank TXT exports, or move to SAP's own Payment/Checks objects?** Once Phase 2 is unblocked, HopDesk would have two candidate sources for realized cash movement.
- *Path A — keep Bancos as-is:* Manual TXT parsing already works, is bank-format-agnostic, and doesn't depend on SAP data-entry discipline (someone has to actually record incoming/outgoing payments promptly in SAP for that source to be current).
- *Path B — source from SAP's Payment objects:* Removes a manual step and ties realized cash directly to the same system of record as AR/AP, at the cost of depending on SAP being kept current — worth testing side-by-side once access exists, not deciding blind.

**Fork — is Networks' native CashFlowLineItem tree actually configured?** Unanswerable until the Note 1 grant lands. If it's populated, Phase 2's classification engine should treat it as the primary signal (an accountant already made these calls); if it's empty or unused, the Note 2 account-class default mapping becomes the primary path instead, with the SAP-native tree kept as a future opt-in per client.

### Sources (Part II)

1. Live Service Layer probes against Networks' production SAP B1 instance (company NG), run 28 Aug 2026 — read-only, session closed after each call.
2. [SAP Business One Service Layer API Reference — SAP](https://help.sap.com/doc/056f69366b5345a386bb8149f1700c19/10.0/en-US/Service%20Layer%20API%20Reference.html)
3. [NetSuite Account Types — Oracle NetSuite documentation](https://docs.oracle.com/en/cloud/saas/netsuite/ns-online-help/section_3947502870.html)
4. [Business Central account resource type — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/business-central/dev-itpro/api-reference/v2.0/resources/dynamics_account)
5. [QuickBooks Online Account API — Intuit Developer](https://developer.intuit.com/app/developer/qbo/docs/api/accounting/most-commonly-used/account)
6. [Xero Accounting API — Types and Codes](https://developer.xero.com/documentation/api/accounting/types)
7. [Odoo account.account reference — Odoo documentation](https://www.odoo.com/documentation/16.0/developer/reference/standard_modules/account/account_account.html)
8. [ISO 20022 camt.053 specification summary](https://github.com/darko-mijic/iso-20022-camt-053-parser/blob/main/docs/iso20022-camt-053-spec.md)
9. [ISO 20022 Adoption Redefined — Kyriba](https://www.kyriba.com/blog/iso-20022-adoption-is-just-the-beginning/)
10. [Connectivity-as-a-Service — GTreasury](https://www.gtreasury.com/solutions/connectivity/connectivity)
11. [CFDI Anexo 20 guía de llenado — SAT (Mexico)](http://omawww.sat.gob.mx/tramitesyservicios/Paginas/documentos/GuiaAnexo311221.pdf)
12. [SAT Descarga Masiva de CFDI — phpcfdi (reference client)](https://github.com/phpcfdi/sat-ws-descarga-masiva)
13. [IFRS Taxonomy illustrative example — Statement of cash flows, IFRS Foundation](https://www.ifrs.org/content/dam/ifrs/standards/taxonomy/2021/illustrative-examples/ixbrl-example3-2021-03-24.xhtml)
14. [US GAAP Taxonomy — Statement of Cash Flows elements, XBRL US](https://xbrl.us/data-rule/guid-cashflows/)
15. [XBRL GL — Global Ledger Taxonomy Framework, XBRL International](https://www.xbrl.org/the-standard/what/global-ledger/)
16. [General Index of Financial Information (GIFI) — Canada Revenue Agency](https://www.canada.ca/en/revenue-agency/services/forms-publications/publications/rc4088/general-index-financial-information-gifi.html)
17. [Unified Accounting API — Account model, Merge.dev](https://docs.merge.dev/accounting/accounts/)
18. [How we built a standard chart of accounts — Codat](https://codat.io/blog/how-we-built-categories/)

---

## Related work not yet folded into this document

A separate handoff prompt was written (3 Sep 2026) for a design/architecture-focused session to design the **classification-management UI and data model** — the tool that lets a user accept HopDesk's default CFO/CFI/CFF suggestions or build custom classification rules, across two separate domains (AR/AP and Bank cash flow), with an explicit unsolved problem flagged for that design pass: batched/aggregated settlements (credit card payments, and similarly-shaped cases) can't be classified as one atomic line item, since they net together purchases that individually belong to different categories. That design work is folded in below as Part III, prepared 9–14 Sep 2026.

---

# Part III — The Classification-Management UI

*The tool a client actually touches: accept HopDesk's default CFO/CFI/CFF mapping or build their own. Grounded in a 16-tool competitive survey (consumer finance, SMB accounting, FP&A SaaS, enterprise EPM) run 9 Sep 2026 and published separately as the artifact "Mapping the Ledger," plus a fresh code-verification pass against the live repo on 14 Sep 2026 — `sap_api.R:436/448`, `data_pipeline.R:159/398/626`, and `pasivos_engine.R`'s `componente_capital`/`componente_interes`/`componente_fees` all still match Part II's citations exactly, so nothing below is fighting stale assumptions about the code.*

## Note 1 — What the survey confirms, and what it can't reach

Two of Part II's Note 8 decisions turn out to be independently validated by the market, not just internally reasoned:

- **Borrowing a taxonomy instead of inventing one.** No tool in the survey — not YNAB, not QuickBooks, not Anaplan — designs its own operating/investing/financing vocabulary from scratch either; they either delegate entirely to the source ERP's account type (Xero, Wave) or, where they do maintain their own tree (Fathom, PlanGuru), they seed it from a standard chart-of-accounts backbone and let the client extend it. Nobody in 16 tools reinvents the CFO/CFI/CFF split itself.
- **Keeping `CFCategory` a separate entity, never a hijacked hierarchy.** This is the single most load-bearing finding from the survey: Prophix's own documentation states its Account dimension is explicitly *excluded* from carrying more than one hierarchy — the exact same shape as trying to make `pasivos_categoria` or the counterparty list double as both an accounting key and a cash-flow-statement key. Part II's Note 2 canonical model already keeps `CFCategory` as its own entity joined via `cf_category_id`, which this independently confirms rather than changes.

Where the survey has nothing to offer: **true consolidation.** Fathom does automatic intercompany eliminations, Agicap and Trovata do FX-aware entity roll-ups, Datarails shows entity/currency natively — but none of them perform IFRS 10 / ASC 810-grade elimination accounting (a mirror elimination entry, not a netted or dropped row). Part I's Note 4.2 elimination engine remains a genuine, unprecedented build for this survey to inform — it's a bigger and separate workstream from the categorization UI, not a sub-feature of it (see Note 6 below).

## Note 2 — The three-tier engine, reconciled with a rule-engine shape

Part II's Note 4 pipeline already specifies a priority order: (1) SAP-native `CashFlowLineItem`, if Networks' finance team has configured one, (2) the Note 2 canonical account-class default mapping, (3) a client override table. The survey supplies a concrete, market-tested shape for tier 3 and a validated pattern for tier 2:

- **Tier 3 — client overrides.** Monarch Money is the only tool in the survey with a fully documented answer to "what happens when two rules match the same row" (an ordered, sequential list, each rule applied in turn, drag-to-reorder) and to "should this apply retroactively" (an explicit checkbox showing the affected count before committing). Adopt that shape directly: `cf_category_rules` rows are ordered, multi-condition, and re-runnable against history with a visible before-you-commit count — never silent backfill.
- **Tier 2 — default mapping.** PlanGuru auto-assigns every imported account using the source system's own coarse account-type field, then opens a review screen for what that signal gets wrong — its own docs cite a real failure (QuickBooks' single "Equity" type collapsing every equity account into one PlanGuru line). HopDesk's tier 2 is the Part I §4.1 default table (fleet purchase→CFI, loan principal→CFF, loan interest→CFO, vendor/carrier payment→CFO) seeded from `pasivos_categoria`/`flavor` — coarse by the same nature, so it inherits the same failure mode, and needs the same fix: anything it assigns stays visibly overridable, never silently final.
- **Tier 1 — SAP-native.** Untouched by the survey (no tool studied reads a source ERP's own pre-built CF taxonomy) — Part II's own open ask (Note 11, item 3 of Part II) to check whether `CashFlowLineItem` is actually configured on Networks' instance stands as written.

The category tree itself takes its shape from Fathom — the deepest hierarchy any *self-serve* tool in the survey supports (Classification → Heading → Subheading → Account, non-destructive on delete) — collapsed to three levels for v1: **Grupo** (CFO/CFI/CFF, seeded from Part II Note 8's real IFRS/US-GAAP element names — `CashFlowsFromUsedInOperatingActivities` and siblings — locked) → **Categoría** → **Subcategoría**, both client-editable, deleting one only orphans its rules back to "sin categorizar" rather than destroying anything.

## Note 3 — Solving the flagged problem: batched settlements

Stated precisely against the actual schema (`pasivos_schemas.R:31`): a `tarjeta` liability's cash outflow is one lump payment to the card issuer, tagged `tarjeta_provision_source` ∈ `registered_expenses` / `manual` / `average_12m`. The spend behind that lump sum can legitimately span several categories (fuel, tolls, office supplies), and a rule keyed on counterparty ("BANCO X — TARJETA CORPORATIVA") can only assign the *entire* payment to one category — exactly the multi-category-merchant failure Mint's research writeup flags for Amazon/Walmart, except structural here rather than incidental.

The three provision sources don't all fail the same way, so they don't all need the same fix:

- **`registered_expenses`** — if the underlying expense rows already carry (or can carry) their own category, classify at the expense-line grain and let the liability's total be the sum of already-classified pieces. *Open question, not yet verified against code:* whether `registered_expenses` rows currently carry any classifiable field at all — worth a direct check before assuming this path is free.
- **`manual` and `average_12m`** — there is no per-transaction data to itemize; itemization is not a data-completeness gap, it's structurally unavailable. The survey's answer is Monarch's **rule-driven auto-split**, the one place a competitor had already solved precisely this shape of problem: a standing rule that splits one matched transaction into fixed dollar amounts or percentages automatically, used in Monarch for exactly the "one shared bill, several real categories" case. Copilot Money's percentage-based split presets are the same mechanic from the consumer side. Applied here: the client defines a **reparto** (allocation template) once per `tarjeta` liability or per liability category — e.g. 55% Nómina-adjacent, 25% Combustible, 20% Gastos generales — and the classification engine explodes the single cash amount into N sub-rows by percentage *before* CFO/CFI/CFF aggregation, not after.
- The reparto is a **stored, editable object**, not a one-time computation — IAS 7's requirement to apply a classification method consistently period over period (Part I, Note 1) means the same split has to keep applying until the client deliberately changes it, and a changed split is a reclassification event subject to the same audit trail as any other rule edit (Part I §4.1).

## Note 4 — Two domains, one panel

Part II's closing paragraph named the two domains this UI actually has to cover: AR/AP and Bank cash flow. They don't share a rule vocabulary, because they don't share a data shape:

- **AR/AP domain** — rows are projected, not realized (Part II Note 2: "all projected, none realized"). Rule fields are `Parte`, `Empresa`, ledger (AR/AP), `pasivos_categoria`/`flavor`, monto range — the same fields the existing Calendario pipeline already computes.
- **Bank domain** — rows come from the Bancos module's parsed TXT statements, closer in shape to what Xero's cash-coding and Monarch's merchant-string rules operate on: bank-statement description text, counterparty-as-written-by-the-bank, amount, account. This is also where a realized-cash reconciliation against the AR/AP projection eventually lives (Part II Note 11's Bancos-vs-SAP-Payments fork), independent of this feature.

Both domains write into the same `cf_categories` tree and the same permanent "Sin categorizar" queue (the convergent Jirav/Runway/Datarails/Monarch pattern — never a silent default), but the Settings panel presents them as two tabs under one "Categorías de Flujo" entry, not two separate features a client has to learn twice.

## Note 5 — Revised data model

Supersedes the sketch published in "Mapping the Ledger" — same two registries, plus a third to carry Note 3's split mechanic, and a domain flag on the rules table to carry Note 4's split:

```
cf_categories.rds
  id, parent_id, nivel        # "grupo" | "categoria" | "subcategoria"
  nombre, codigo_xbrl         # Grupo rows only — the real Part II Note 8 element names
  orden, es_sistema           # TRUE for the 3 seeded Grupos — CFO / CFI / CFF

cf_category_rules.rds
  id, categoria_id, dominio   # "ar_ap" | "bancos"
  prioridad, activo
  campo      # ar_ap: parte | empresa | pasivos_categoria | ledger | monto
             # bancos: descripcion | contraparte_banco | monto | cuenta
  operador   # "es" | "contiene" | "rango"
  valor

cf_category_splits.rds        # resolves Note 3
  id, liability_id_or_categoria_origen
  categoria_destino_id, porcentaje
  activo, actualizado_por, actualizado_en   # audit trail — IAS 7 consistency requirement
```

## Note 6 — What this feature is deliberately not

Two pieces of Part I stay out of scope for the classification-UI build, because they *read* `cf_category` once it exists rather than needing to exist before it does:

- **The elimination/consolidation engine** (Part I §4.2) — a genuine, unprecedented build (Note 1 above); belongs to the entity-hierarchy/ownership-percentage work, not the category tree.
- **The financing-liability roll-forward view** (Part I §4.3 / Part II Note 10 — already "buildable now" from data that exists) — a reporting view on top of `componente_capital`/`componente_interes`/`componente_fees`, independent of whether a client has customized their CFO/CFI/CFF tree at all.

## Note 7 — Revised phase order

Supersedes the 5-phase list in "Mapping the Ledger" — same shape, with the engine priority (Note 2) and the split mechanic (Note 3) now placed correctly:

1. **Registries + seeded template** — `cf_categories`/`cf_category_rules`/`cf_category_splits`, Grupos seeded with real XBRL element names, Categorías seeded from the Part I §4.1 default table (tier 2).
2. **Tree editor** — add/rename/reorder/soft-delete, non-destructive, in a new Settings-hub panel.
3. **Rule mapper + reparto builder + "sin categorizar" queue** — tier 3, both domains (Note 4), the percentage-split UI for `tarjeta` liabilities (Note 3), bulk-assign reusing the existing Intercompany-scanner interaction.
4. **Wire the Note 9 (Part II) toggle** — Básico ⟷ Estructurado into `.build_summary()`; "Sin categorizar" always its own line; Word/Excel export inherit for free.
5. **Tier 1 (stretch, blocked)** — once the pending SAP General Authorizations grant lands, check whether `CashFlowLineItem`/`CashFlowAssignment` is actually configured on Networks' instance (Part II Note 11); if so, it outranks tier 2 for every account it covers.

### Sources (Part III)

Full per-tool citations live in the published artifact "Mapping the Ledger" (https://claude.ai/code/artifact/f7df5329-2079-48fc-ae88-4de930c997c2) rather than duplicated here — Monarch (rule ordering + retroactive apply), Copilot Money (percentage split presets), Fathom (4-level self-serve hierarchy, non-destructive delete), Prophix (Account dimension excluded from alternate hierarchies), PlanGuru (type-based auto-map + exception review), Jirav/Runway/Datarails (convergent "unmapped" surfacing).

---

# Part IV — Four More Languages

*SAP B1 was verified live. NetSuite, Microsoft Dynamics 365 (Business Central + Finance), QuickBooks Online, and Xero are verified against each vendor's own primary documentation — Xero's via its published OpenAPI spec directly, not just prose docs, which is the closest thing to a second "live probe" this pass could get without real credentials. Prepared 14 Sep 2026. Purpose: extend Part II's adapter contract from one ERP to five, and pressure-test Part I/II/III's design decisions against what four more systems actually do.*

## Note 1 — Five languages, side by side

What Part II's adapter contract (Note 3) means by "a new `R/<erp>_api.R`" concretely has to speak, per ERP:

| ERP | Protocol | Query mechanism | Scripting needed? | Auth |
|---|---|---|---|---|
| SAP Business One | Service Layer (OData-flavored REST) | `$filter`/`$expand` | No — pure REST for read access | Session cookie, user/pass |
| Oracle NetSuite | REST Web Services + SuiteQL | `SELECT ... FROM transaction WHERE ...` — real SQL, arbitrary JOINs, 1,000 rows/page, 100,000-row cap | No for reads — SuiteScript only needed for RESTlets/customization | OAuth 2.0 (TBA sunsetting — no new TBA integrations after release 2027.1) |
| MS Dynamics 365 Business Central | API v2.0 (OData-flavored REST) + bound actions for state changes (`post`, `send`) | `$filter` | AL, but only for extending/customizing — not for read access | Entra ID (Azure AD) OAuth2, client-credentials, service-to-service — closest analog to SAP B1's service-account model of the four |
| MS Dynamics 365 Finance | OData v4 over Data Entities, + async Data Management Framework for bulk | `$filter`/`$expand` (first-level only)/`$select`, no `has`/`in`, 10,000 max page | X++, for the same reason as AL above | Entra ID OAuth2, messier/older docs than BC |
| QuickBooks Online | REST + a SQL-like query endpoint | `SELECT * FROM Invoice WHERE Balance > '0'` — **no JOINs, no OR, no GROUP BY, no field projection** — every relation is a separate call | No | OAuth 2.0, one scope for all accounting data (no granular read-only scope) |
| Xero | REST, OData-adjacent `where` filter | `Status=="ACTIVE" AND Type=="BANK"` — LINQ-like, **no `IN` operator** (open request) | No | OAuth 2.0, granular read scopes — but legacy broad scopes sunset for new apps after 2 Mar 2026 |

**The practical ranking for "closest to what HopDesk already knows how to build":** Dynamics 365 Business Central's service-to-service auth pattern is the nearest cousin to SAP B1's session-based service account — same "register once, run headless forever" shape. NetSuite's SuiteQL is the most *capable* query language of the five (the only one with real JOINs), which matters because QuickBooks' and Xero's thin filters mean HopDesk's adapter — not the ERP — has to do more of the relational stitching (e.g., resolving an invoice's customer name is a second call, every time, for both).

**The practical warning for cost/timeline planning:** QuickBooks Online is the only one of the five with a mandatory pre-production security/technical review (1–3 weeks minimum, longer for Marketplace listing) gating a first customer connection — plan around this explicitly if a QBO client is ever the pilot for adapter #2, it isn't a same-day "flip a connector on" story the way SAP B1 has been.

## Note 2 — Chart of accounts, updated

Part II's Note 2 table listed classification fields at a glance; here's the same five ERPs (SAP B1 + the four) at the depth this pass actually reached:

| ERP | Account type enum | Cash/bank signal | Nesting |
|---|---|---|---|
| SAP B1 | n/a (no generic type field) | `CashAccount` (boolean) | Confirmed present in schema, depth not probed this pass |
| NetSuite | 17 `acctType` values | **No boolean equivalent** — cash-ness is entirely `acctType = "Bank"` (even petty cash gets typed Bank) | Parent/sub-account exists, no documented hard depth limit |
| Dynamics 365 BC | 6 fixed `G/L Account Category` values (Assets/Liabilities/Equity/Income/COGS/Expense) + a **separate, decoupled** structural `Account Type` (Posting/Heading/Total/Begin-Total/End-Total) | `"Reconciliation Account"` boolean, plus a wholly separate Bank Account master table | User-defined hierarchical subcategories in their own table |
| QuickBooks Online | 16 `AccountType` values, 125 `AccountSubType` values (both confirmed via Intuit's own SDK docs) | No boolean — `AccountType = Bank`, subtypes `Checking`/`Savings`/`MoneyMarket`/`CashOnHand` | Sub-accounts exist in the product (up to ~5 levels, per the earlier consumer-tool survey) — not re-confirmed at the API-field level this pass |
| Xero | 18 `Account.Type` values, rolling up to a **read-only, derived** `Account.Class` (Asset/Equity/Expense/Liability/Revenue) | No boolean — `Type = BANK` | **Confirmed flat at the API level** — no parent-account field exists anywhere in the schema; the only grouping concept is a separate `ReportingCode` tag, not a tree |

**What this changes about Part II Note 2's account-class default mapping (§4.1):** the five-way Asset/Liability/Equity/Revenue/Expense convergence Note 2 already found holds up exactly at this depth too — QuickBooks' `Classification` field and Xero's `Account.Class` are both explicitly *derived* from the more granular type, the same relationship SAP B1's canonical model already assumes. Nothing here changes the canonical `Account.class` design; it just confirms it generalizes to four more real schemas without modification.

## Note 3 — Cash-flow-statement maturity, ranked

Ordered from "closest to a purpose-built cash-flow taxonomy engine" to "doesn't have one at all":

1. **SAP B1** — `CashFlowLineItem`/`CashFlowAssignment` is a genuine, GL-independent hierarchical taxonomy tree with its own assignment records (Part II Note 1). Still unknown whether Networks' instance has it configured — the single highest-leverage open question from Part II, unchanged by this pass.
2. **Dynamics 365 Business Central** — no separate taxonomy entity, but the `G/L Account Category` table carries an **`"Additional Report Definition"` field explicitly documented as existing for cash-flow-statement and specialized financial-reporting purposes** — the closest thing any of the four new ERPs has to a purpose-built cash-flow tag on the account itself, short of SAP B1's dedicated object. BC's separate **Cash Flow Forecast** module is also the most transparent of anything found in this entire research arc (across both this pass and the earlier 16-tool survey): every forecasted line is explicitly source-typed — General Ledger, Purchasing, Sales, Service, Fixed Assets, or Manual — never an opaque blend.
3. **NetSuite** — a real, native Statement of Cash Flows (indirect method), but CFO/CFI/CFF section membership is a **Financial Report Builder** construct (a filter-driven, reparentable tree of report sections keyed on account name/number/type/class/department/location/subsidiary) — a reporting-layer overlay, not a first-class taxonomy object the way SAP B1's is.
4. **QuickBooks Online** — a `CashFlow` report exists via the Reports API, but classification is purely `AccountType`/`AccountSubType`-derived and **read-only at every layer, including the API** — no override exists anywhere, confirming and extending the earlier UI-level finding (Part III's "Four traps" — QBO's locked SCF) down to the data model itself.
5. **Xero** — **no Statement of Cash Flows in the public Accounting API at all.** A real one exists only in a separate, partner-gated "Finance API" built for lenders, with an unresolved question of whether it even covers non-U.S. entities (Mexico's status is unconfirmed from public docs). For practical purposes, a Xero adapter gets nothing here — CFO/CFI/CFF would be 100% HopDesk's own classification work, same as if Xero had no report at all.

**What this changes about Part I:** nothing in the standards story changes, but it sharpens the priority argument for HopDesk's own classification engine (Part III) — for 4 of 5 ERPs studied, HopDesk's own tagging is either the *only* source of CFO/CFI/CFF (Xero, and QBO in practice since it's locked) or a meaningfully better one than what ships natively (NetSuite's reporting-layer overlay, BC's still-manual-per-account tagging). SAP B1 is the outlier where the ERP might already have curated this — which is exactly why Part II Note 11's open ask (check if `CashFlowLineItem` is actually configured on Networks' instance) stays the single highest-leverage unresolved question across the whole document.

## Note 4 — XBRL and the standards layer: confirmed, not contradicted

Checked all four for native XBRL/IFRS support, specifically to pressure-test Part II Note 8's "borrow, don't invent" decision:

- **NetSuite** — no confirmed native XBRL/SEC-filing export; the documented pattern is NetSuite as system-of-record feeding a *separate* disclosure-management product (Workiva, ActiveDisclosure) that does the actual tagging.
- **Dynamics 365 Business Central** — Microsoft is **actively removing** XBRL reporting features from the product, in its own words. Not a gap to plan around — a feature being deleted.
- **QuickBooks Online** — no native XBRL, confirmed by an unanswered Intuit developer-community question asking whether it exists at all.
- **Xero** — no native XBRL; an open, unshipped product-idea request (driven by South African iXBRL mandates), not a roadmap commitment.

**Zero for four.** Combined with Part II Note 8's original finding (Codat/Merge/Rutter never built a CFO/CFI/CFF layer either), this is now a much stronger result than "no aggregator does this" — **no ERP does this either.** XBRL and the IFRS/US-GAAP taxonomy are external, governance-body-maintained vocabularies that every system in this survey treats as somebody else's problem. HopDesk adopting them directly (Part II Note 8) isn't catching up to an industry practice — it's doing something none of the five systems studied actually do themselves.

**IAS 7 §44A financing-liability roll-forward — now confirmed absent by design, not by oversight.** The Dynamics 365 research surfaced the actual IASB project history: the Board *considered* mandating a reconciliation format for the financing-liability roll-forward and explicitly **decided not to require one**. That's not a product gap Microsoft or anyone else failed to fill — it's a deliberate standards-setting choice to leave the presentation open. Part I §4.3's roll-forward view (buildable now from `pasivos_engine.R`'s existing principal/interest/fees split) isn't reinventing something the market already ships; it's building something the standard-setter deliberately left as whitespace.

## Note 5 — Consolidation maturity, ranked

Directly informs Part I §4.2's elimination-engine design — same ranking logic as Note 3, now for consolidation:

1. **NetSuite OneWorld** — the most mature: real elimination-entry postings to dedicated Elimination Subsidiaries, auto-reversing entries the following period, a dedicated Cumulative Translation Adjustment–Elimination account for FX residue, idempotent (delete-and-regenerate) re-runs from the Period Close Checklist.
2. **Dynamics 365 Finance** — a genuinely separate elimination-rules engine, run during consolidation or via an "elimination proposal" step — confirmed distinct from its automatic Due-to/Due-from reciprocal posting (which by itself does *not* eliminate anything).
3. **Dynamics 365 Business Central** — no automated elimination engine; consolidation is per-account mapping/filtering fields (`Consol. Translation Method`, `Exclude From Consolidation`) plus a **bilateral intercompany mapping model** — an explicit `IC Partner Code` and an Outbox/Inbox message-passing journal between named partners. Less automatic than NetSuite or F&O, but the shape (explicit, named, two-way mapping) is the closest fit to HopDesk's own already-known constraint: SAP B1's `BusinessPartners` 403 and CardCode-only IC detection (per the interco-v2 project history) means HopDesk already can't rely on automatic matching either — BC validates building an explicit mapping-table UI as the right-sized answer, not a compromise.
4. **QuickBooks Online** — no native consolidation via API even on Intuit Enterprise Suite (their new multi-entity product); every connection is still one realm at a time, and IES's consolidation features aren't API-exposed yet. A QBO adapter gets zero help here, same as today's SAP B1 situation.
5. **Xero** — no native multi-entity anything; confirmed third-party-only (Fathom, Syft), each of which connects to every organisation separately and consolidates externally — architecturally identical to what HopDesk would have to do itself.

**What this changes about Part I §4.2:** three of five ERPs offer nothing to lean on for elimination — the elimination engine has to be HopDesk's own, ERP-agnostic layer regardless of which adapter is behind it, exactly as §4.2 already assumed. Where an ERP *does* have one (NetSuite, F&O), the useful thing to import isn't the engine itself but the **signal**: NetSuite's `eliminate` flag on the Account record and F&O's elimination rules are both queryable, explicit markers of "this account represents an intercompany position" — a much cleaner detection primitive than what SAP B1's Service Layer currently exposes for the same concept. If NetSuite or F&O ever become a HopDesk adapter, IC detection there should be markedly easier than it's been for SAP B1.

## Note 6 — Sync primitives worth adopting regardless of which ERP

Two patterns, found independently, that generalize past any single adapter and are worth folding into Part II Note 3's adapter contract as a shared capability, not a per-ERP quirk:

- **An append-only, cursor-walked, all-transactions feed** — Xero's `/Journals` endpoint (no `where` filter, deliberately; walked by a monotonic `JournalNumber` offset) is the cleanest version of this found anywhere in this research arc. Every transaction of every type posts through it exactly once, immutably. This is a categorically better incremental-sync primitive than polling each entity type separately with an `UpdatedDateUTC` filter, because it can't miss a transaction type the adapter forgot to poll.
- **Thin webhook + periodic reconciliation sweep, as a pair, never webhook alone** — QuickBooks Online's Change Data Capture (`changedSince`, 30-day lookback cap) exists specifically to backstop missed/dropped webhook deliveries; Xero's webhooks are confirmed to cover only 6 of its event categories (no Payment, Account, ManualJournal, or BankTransaction events at all), making a periodic `/Journals`-cursor sweep mandatory, not optional, for completeness.

**Why this matters beyond adapter #2:** this is the same shape of problem Bancos already hit and fixed — two independent detection paths that have to agree, or drift silently ([[feedback_dedup_bug]]). A shared "rate-budget + cursor-or-poll + reconciliation sweep" abstraction in the adapter contract would generalize that lesson forward instead of re-discovering it per ERP.

## Note 7 — What elevates HopDesk's own standard

Concrete, adoptable-now changes to Part II's canonical model and Part III's classification engine, surfaced by seeing four more systems' choices:

- **Add an explicit Format layer to the adapter contract.** Dynamics 365 Finance's Electronic Reporting framework splits cleanly into Data model → Model mapping → Format, with **format-level validation rules that can block generation outright**. Part II Note 3's adapter contract already has the first two (the canonical schema, and each `<erp>_api.R` module); it has no equivalent of the third. Concretely: before generating a Structured cash-flow export, check the active connection's capability flags and refuse — with a clear, named reason — rather than silently rendering an incomplete or misleading statement when (say) `financing_liability_detail` isn't available for that client's tier.
- **Give every classified row a visible provenance tag.** BC's Cash Flow Forecast tags every line with where it came from (General Ledger / Purchasing / Sales / Service / Fixed Assets / Manual) rather than presenting one opaque number. Part III's 3-tier engine (SAP-native → default mapping → client override) already has the tiers — it doesn't yet expose which tier produced a given row's category to the person looking at the report. Worth adding as a rendered attribute (a small badge or footnote marker on each line, not just an internal field), the same way BC makes its forecast composition auditable at a glance.
- **Name the cash/bank signal explicitly, don't infer it.** Confirm Part II Note 2's `is_cash` canonical field by the same name and boolean shape BC uses (`Reconciliation Account`) and SAP B1 already has (`CashAccount`) — both real ERPs converge on "a plain boolean on the account," which is one more point of evidence the canonical model already has this right and shouldn't overthink it into something more dimensional.
- **Steal NetSuite's declarative section-tree shape for HopDesk's own statement layout**, not just for the category tree (Part III already did this) but for the *export/report layer* itself — a small tree of `{label, parent, filter_expr}` nodes is a reasonable long-term shape for letting a client customize their Word/Excel statement's section order and grouping without touching R code, once Part III's Phase 4 toggle exists and clients start asking for layout variations on top of it.

## Note 8 — Updated capability-flags table

Extends Part II Note 3's registry pattern from one ERP to five — what each connection could realistically offer, and the integration cost of getting there:

| Connection | Canonical entities realistically reachable | Integration cost/gate |
|---|---|---|
| SAP B1 (as authorized today) | `ar_invoices`, `ap_invoices` | None — live |
| SAP B1 (after Note 1/Part II grant) | + `chart_of_accounts`, `gl_journal`, `payments`, possibly `native_cf_taxonomy` | One permissions grant, already requested |
| NetSuite | All six canonical entities, plus a real `ic_elimination_flag` signal | OAuth2 setup + SuiteQL; no schema access gate found, but full field schemas sit behind an authenticated Schema Browser |
| Dynamics 365 Business Central | All six, plus `is_cash_equivalent` and bilateral IC mapping | Entra ID app registration + permission set — closest to SAP B1's service-account effort |
| Dynamics 365 Finance | All six, plus a real elimination-rules engine | Same Entra ID model, messier docs, X++ only for customization beyond standard entities |
| QuickBooks Online | `ar_invoices`, `ap_invoices`, `chart_of_accounts`, `gl_journal` (JournalEntry), `payments` — **no consolidation, no CF-category override** | OAuth2 + **mandatory production security/technical review (1–3 weeks minimum)** + a metering layer that penalizes read-heavy usage specifically |
| Xero | `ar_invoices`, `ap_invoices`, `chart_of_accounts`, `gl_journal` (via `/Journals`) — **no native CF statement, no consolidation** | OAuth2, granular scopes required for any app built after 2 Mar 2026; best sync primitive of the five but thinnest financial-statement surface |

**If HopDesk builds adapter #2 next**, this pass's evidence points two directions depending on which client profile drives the decision: **NetSuite or Dynamics 365 Finance** for a future multi-subsidiary corporate-group client structurally like Networks (both have a real elimination engine, directly serving Part I §4.2); **Dynamics 365 Business Central** for the closest philosophical and technical fit to what HopDesk is already building for itself (per-account cash-flow tagging field, transparent forecast provenance, SAP-B1-like service-account auth) at SMB scale. QuickBooks Online and Xero are both worth building eventually for market reach, but neither contributes anything toward the two hardest unsolved problems this whole document is organized around — cash-flow classification and consolidation — so building either first would mean solving both from scratch in HopDesk with no ERP-side help at all, same as SAP B1 today, just for a different market.

### Sources (Part IV)

1. [NetSuite REST Web Services / SuiteQL — Oracle](https://docs.oracle.com/en/cloud/saas/netsuite/ns-online-help/section_157909186990.html)
2. [NetSuite Account Types — Oracle](https://docs.oracle.com/en/cloud/saas/netsuite/ns-online-help/section_3947502870.html)
3. [NetSuite Financial Report Builder — Oracle](https://docs.oracle.com/en/cloud/saas/netsuite/ns-online-help/chapter_N2105415.html)
4. [NetSuite Elimination Subsidiaries — Oracle](https://docs.oracle.com/en/cloud/saas/netsuite/ns-online-help/section_N268759.html)
5. [Dynamics 365 Business Central API v2.0 — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/business-central/dev-itpro/api-reference/v2.0/)
6. [G/L Account Category table — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/business-central/application/base-application/table/microsoft.finance.generalledger.account.g-l-account-category)
7. [Cash Flow Forecast overview — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/business-central/finance-cash-flow-overview)
8. [Electronic Reporting overview — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/fin-ops-core/dev-itpro/analytics/general-electronic-reporting)
9. [D365 Finance elimination rules — Microsoft Learn](https://learn.microsoft.com/en-us/dynamics365/finance/general-ledger/elimination-rules)
10. [IASB — Disclosure of Changes in Liabilities from Financing Activities project history](https://www.ifrs.org/projects/completed-projects/2019/disclosure-of-changes-in-liabilities-arising-from-financing-activities-ias-7/tad-disclosure-of-changes-in-liabilities-arising-from-financing-activities/)
11. [AccountTypeEnum / AccountSubTypeEnum — Intuit SDK Javadoc](https://static.developer.intuit.com/sdkdocs/qbv3doc/ipp-v3-java-devkit-javadoc/com/intuit/ipp/data/AccountTypeEnum.html)
12. [QuickBooks Online Reports API — CashFlow](https://g-accon.com/quickbooks-online-reports-api-is-changing/)
13. [Xero OpenAPI spec (accounting) — GitHub](https://github.com/XeroAPI/Xero-OpenAPI)
14. [Xero API rate limits — developer.xero.com](https://developer.xero.com/faq/limits)
15. [Xero Finance API — Statement of Cash Flows](https://developer.xero.com/documentation/api/finance/financialstatements)
16. [Xero accounting API scope migration — devblog.xero.com](https://devblog.xero.com/upcoming-changes-to-xero-accounting-api-scopes-705c5a9621a0)
