# =============================================================================
# R/hopdesk_ops_placeholder.R
#
# NOT IMPLEMENTED. NOT SOURCED. Written 2026 during the SaaS architecture
# rebuild to preserve scope — see
# docs/saas_rebuild/STAGE_5_HOPDESK_OPS_PLACEHOLDER.md for the original
# discussion, and ARCHITECTURE.md §7 for how this fits the broader rebuild.
#
# This file contains no code, no schema, and no reactiveVals — only prose,
# consolidating Mouse's original scoping request so a future implementer
# reads this first instead of re-discovering the shape from scratch. It is
# not referenced by global.R, app.R, or any other file's source() calls, and
# has zero effect on the running app.
#
# Mouse's own words from the original scoping conversation: "Hopdesk staff
# will absolutely eventually need tickets and dashboards. Please design it
# all, create the comments in the code without writing code or variables
# but just establish the architecture and idea so that we can do that on a
# later project and we don't forget it."
#
# -----------------------------------------------------------------------
# Where this would live
# -----------------------------------------------------------------------
# Per ARCHITECTURE.md §7, Hopdesk's own S3 folder (hd-admin/) stays
# deliberately empty of client-shaped data forever. Ticket and dashboard
# data is fundamentally different in kind from anything a client's own
# modules handle (Calendario, Vencidos, Bancos, etc.) — it is Hopdesk's own
# internal operational data about how Hopdesk services clients, not a
# client's own financial/operational data. Whenever this gets built for
# real, it would get its own schema and its own module(s), never a
# repurposing of any existing client-facing module or table.
#
# -----------------------------------------------------------------------
# 1. A support ticketing system for Hopdesk staff
# -----------------------------------------------------------------------
# A ticket represents a piece of support work Hopdesk staff does for one
# client — conceptually similar to how a "jump" already represents staff
# entering a client's context, but a ticket would be a persistent record of
# *why* and *what happened*, not just a temporary access grant.
#
# A ticket would likely reference: which client it's for, which staff
# member is handling it, a status (open/in progress/resolved — exact
# values not decided here), and a free-text description — and probably a
# link to the relevant app_audit entries from whatever jump/actions were
# taken to resolve it, since the audit infrastructure (Stage 4) already
# captures "staff did X in client Y's folder at time Z."
#
# Open product question, not resolved here: should a ticket *require* a
# jump grant to exist, or should it be able to exist independently (e.g.
# logging a phone call that didn't involve touching the client's data at
# all)? Left for whoever scopes this for real.
#
# -----------------------------------------------------------------------
# 2. Cross-client operational dashboards for Hopdesk staff
# -----------------------------------------------------------------------
# An aggregate view across every active client in the registry (not any
# one client's own data) — e.g. how many clients are active, how many
# seats each is using against their limit (the registry already tracks a
# user-count/limit pair — see Stage 6 of the original CLAUDIO spec work),
# how many open tickets exist and their age, which clients have had recent
# jump activity.
#
# This would be a natural extension of what principal's global audit view
# (Stage 4) already partially does — sees everything across every client —
# a dashboard would summarize/visualize that same underlying data rather
# than requiring a wholly separate data source.
#
# Exact metrics, chart types, and layout are explicitly not decided here —
# that belongs to whichever future stage actually scopes and builds this.
# =============================================================================
