---
name: compare
description: Compare another schema library, a release of one, or a project using Sury (Zod, Valibot, ArkType, Effect Schema, TypeBox, fast-json-stringify, rescript-rest, ...) against Sury and turn it into ranked, concrete ideas. Use for "what's new in Zod X for Sury", "compare with <lib>", "what can we learn from <project>".
---

# Compare with another library

Research only unless asked to build. The output is a decision aid, not a tour.

## 1. Read the source, not the marketing

Changelog, release notes, docs and, where it matters, the implementation. If the
network blocks a fetch, say so and use what the repo already has
(`packages/e2e`, `CONTRIBUTING.md`'s comparison section, `IDEAS.md`).

## 2. Map every feature to Sury

For each feature, write the Sury spelling that does the same today, checked
against the actual API (`packages/sury/src/entry.ts`, `index.d.ts`, `docs/`),
not from memory. Classify:

| feature | theirs | Sury today | status |
|---|---|---|---|

status is one of: **have**, **have, different spelling**, **missing**, **not
wanted** (say which CLAUDE.md goal it conflicts with). Where a claim is about
speed or size, measure it (`pnpm benchmark:comparison`, `pnpm spec check`)
instead of repeating their number.

## 3. Rank what is missing

By DX value to a Sury user first, then by cost to the core. For the top items
sketch the API as the example a user would write and what it reads back, and
note which module it would live in per the layering.

## 4. Deliver

The table, the ranked list, and anything worth stealing from their
implementation (a trick in generated code, an error message shape). Check
`IDEAS.md` for an existing entry before proposing one; when asked, add new ideas
there in its existing style.
