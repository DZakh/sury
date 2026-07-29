# Union compiler architecture

The union compiler treats member semantics as data before it emits JavaScript.
A phase-zero preflight first handles only trusted no-op cases: the value was
already validated against this exact schema, or a primitive union source is
provably at least as narrow and contains no transformation. These gates never
choose between competing members. Factory flattening and codec-rule resolution
also happen before the planner proper.

The planner then has four phases:

1. **Normalize** consumes the factory's canonical, transparently flattened
   member list after codec-rule resolution, records the declared-source mask,
   and precomputes exact-literal and broad object/number facts. Refs, recursive
   schemas, parsers, functions, and transformed nested unions remain boundaries.
2. **Analyze** creates one compact member record. It stores the source index,
   accepted-input (`m`) and possible-output (`o`) tag masks, effect category,
   throw capabilities, semantic discriminator, class identity, route, and stable
   specificity tier. `m === 0` explicitly means “accepts no runtime input” and
   `o === 0` means “cannot produce output”; they are independent facts. Effects
   are identity, validation/refinement, coercion/transformation, terminal
   rejection, or opaque/deoptimized. Thus `T -> never` remains executable,
   while `never -> T` creates no route.
3. **Plan** streams those records into sparse tag-keyed route buckets, open
   effect-compatible groups, and explicit deoptimized routes. Semantic overlap
   uses discriminator key/value data and SameValueZero comparison, never
   generated condition strings. A sparse reverse summary records, per runtime
   tag family, either a common discriminator key and its values or a broad
   overlap barrier. Non-bucketed members contribute to a separate broad mask.
   An overlap or effect boundary closes unsafe grouping. Opaque members are
   represented by their effect plus a closed route rather than a special emitted
   group kind. A late semantic discriminator cannot jump an already-open broad
   member; hard instance and `NaN` tiers remain explicit and stable.
4. **Emit** consumes the completed plan. It builds shared raw-tag narrows where
   the plan permits, emits member decoders in planned order, and implements
   fallback without re-reading schemas for overlap or effect decisions.

Only a Sury decoding error means that a member did not match. Every fallback
catch calls `getOrRethrow`, so foreign exceptions escape immediately. Coercing
members retain their own effect segment even when they share a raw tag route
with validators. The IR's throw bits describe semantic capability; the builder's
throw mark describes whether the completed emitted body can still throw after
its checks were hoisted. The latter only removes dead `try` blocks and never
changes overlap or fallback decisions.

An overlapping async member is awaited inside an async dispatch IIFE so a
rejected Promise can be classified before fallback. Semantically disjoint async
groups keep the allocation-free dispatch and return through `Promise.resolve`.

No plan cache is installed. Operations already cache their compiled function;
caching this short-lived mutable-schema analysis would add startup allocation
and retention to save work only on recompilation.

## Deoptimization boundaries

The planner deliberately does not expand:

- opaque refs and recursive schemas;
- non-transparent nested unions;
- functions;
- custom parsers;
- unknown or representation-changing transformations.

A boundary may still expose a bounded root tag mask for reachability, but its
inner semantics are left to its decoder. This keeps recursive planning finite
and prevents guessed overlap from changing first-match behavior.

## Compatibility constraints

The plan preserves source order inside a priority tier. Class/subclass and
`NaN`/number hard priorities are explicit; discriminator priorities are applied
only when they do not cross a semantic overlap. Literal values use SameValueZero:
`NaN` matches `NaN`, `-0` matches `0`, and symbols match only by identity.

`T -> never` is an executable rejecting member but contributes no output.
`never -> T` contributes neither an input route nor target coverage. This makes
reverse conversion reject a previously tolerated uncovered target in some
codecs, and JSON Schema output omits non-producing `never` branches.

An exact literal source no longer erases an earlier same-representation member:
its validation or explicit transformation runs before the literal fallback.
The existing rule that suppresses unrelated implicit cross-tag coercion for an
exact source is unchanged.

Foreign exceptions raised by a custom decoder inside a union now escape with
their original identity. Code that previously relied on such an exception being
wrapped as an invalid conversion must catch it itself. Async members can now
fall through after a rejected Promise containing a Sury error; all-reject errors
remain flat and source ordered. Reverse codecs may now reject uncovered targets
that were previously bridged by `never -> T`.

## Local validation and measurements

All measurements below were produced locally on macOS arm64 with Node 24.4.1
and pnpm 9.0.5. The repository requests Node 24.16.0, which was not available
in the test environment. Performance runs used 8 × 2 rounds, five screening
jobs, and confirmed the reported results. GitHub CI was not used.

The bundle measurement distinguishes the complete public entry, the isolated
`union` export, a direct union-only entry, and the contribution esbuild
attributes to `union.ts`:

| Revision | Full min / gzip | `union` min / gzip | Direct min / gzip | `union.ts` |
| --- | ---: | ---: | ---: | ---: |
| `11eb8f9a` | 59,016 / 22,608 | 25,313 / 10,896 | 25,163 / 10,824 | 8,812 |
| `origin/main` (`2aa708b`) | 54,008 / 20,702 | 21,504 / 9,431 | 21,358 / 9,321 | tree-shaken into the old composite compiler |
| this change | 60,334 / 23,385 | 26,525 / 11,596 | 26,394 / 11,504 | 10,042 |

Against the correctness baseline, that is +1,318 bytes (+2.2%) minified and
+777 bytes (+3.4%) gzipped for the full entry, and +1,212 bytes (+4.8%)
minified and +700 bytes (+6.4%) gzipped for the isolated `union` export. The
rewrite therefore does not meet the aspirational reduction target. The retained
cost is the explicit semantic IR, effect analysis, and fallback planner; removing
those late in the change would trade reviewability or correctness for size.

Generated decoder source was measured separately from library code. On the same
ten-decoder panel available at `11eb8f9a`, emitted source grew from 2,547 to
2,913 bytes (+14.4%): parse 2,178 → 2,547, encode 303 → 283, and decode
66 → 83. On the same four-decoder panel available on `origin/main`, it grew
from 1,187 to 1,412 bytes (+19.0%). Inspection of the emitted source confirms
that:

- same-tag validating members share a raw tag narrow;
- a coercing member remains a separate ordered attempt;
- a shared discriminator still tries the earlier refining member before the
  exact-literal fallback;
- instance checks stay before the generic object route while preserving order
  among the instance tier;
- only `getOrRethrow`-classified Sury errors enter the accumulated fallback
  list;
- the large-union decoder uses one literal disjunction, bounded class and
  structural routes, an explicit opaque attempt, then broad tag fallbacks.

The full `--perf=only` suite was run against both baselines. Representative
runtime paths are:

| Path | vs `11eb8f9a` | vs `origin/main` | Interpretation |
| --- | ---: | ---: | --- |
| large union, accepted first / late literal | -100.0% / -100.0% | -34.6% / -41.2% | stable literal route |
| large union, structural discriminator | -99.8% | unchanged within noise | direct semantic route |
| large union, class route | -99.7% | +68.9% | mixed class/opaque-route tradeoff |
| overlapping subclass | unchanged | -33.6% | preserves the target's existing improvement |
| large union, deoptimized fallback | -67.1% | unchanged within noise | opaque attempt remains ordered |
| large union, rejected by all | -87.1% | +4.7% | sparse route traversal |
| overlap barrier, rejected by all | -23.9% | +84.1% | main skipped required overlap fallback |
| JSON union, accepted fallback | -52.3% to -54.4% | behavior changed | avoids double decoding |
| transformed string fallback | +4.8% parse / +6.9% decode | behavior changed | explicit effect boundary |
| optional nullable fallback | +24.0% | +23.7% | ordered Sury-failure fallback now remains explicit |
| literal nullable bridge | +108.2% | +108.8% | very small bridge path now pays the general overlap machinery |

The large-union broad-number result is 67.0% faster than `11eb8f9a` but much
slower than main. Main jumps past an earlier opaque member that accepts numbers;
the current decoder must execute that rejecting member before the later broad
number member to preserve first-match behavior, so the main number is not a
correctness-equivalent baseline.

Creation plus compilation is generally slower than `11eb8f9a`: representative
union cases range from +14.0% to +65.9%; the large planner stayed within the
13.2% compile noise floor and schema creation was 3.3% faster. This is the
expected cost of explicit normalization and analysis. Schema construction alone
is often faster than main because planning is deferred to compilation. The full
reports contain 396 unchanged cases against `11eb8f9a` and 350 against main;
behavior-changing cases are reported separately rather than timed as if
equivalent.

The completed local checks were:

- `pnpm spec check --perf=skip`
- `pnpm spec check --perf=only --against 11eb8f9a`
- `pnpm spec check --perf=only --against origin/main`
- `pnpm --filter=sury test`
- `pnpm --filter=sury typecheck`
- `pnpm compliance`
- bundle/export and generated-source measurements
- `git diff --check`

## Critical self-review

- Hard tiers deliberately reorder subclass/instance, exact `NaN`, and semantic
  discriminator checks ahead of broad checks. A future tier must prove that it
  cannot reorder two precedence-sensitive overlapping members.
- The discriminator summary uses one literal field. Distinct values on that
  shared field prove disjointness; a different key, a broad member, or an opaque
  route becomes a barrier. Additional literal fields are ignored, which can miss
  an optimization but must never invent one.
- Declared-source metadata at an encoded ref boundary is trusted only for a
  bounded root mask. A mask that is too broad only deoptimizes; a future
  under-narrow mask could incorrectly hide a reachable member.
- Effect traits are conservative for current schema kinds. A future parser or
  transformation kind must default to opaque until its Sury/foreign throw and
  representation-change behavior is classified.
- Async awaiting is gated by semantic overlap bits. A missed overlap would let a
  rejected Promise bypass fallback; a false overlap is correct but allocates an
  avoidable async IIFE.
- Grouped synthetic errors are recognized by expected-schema identity. A custom
  member that deliberately throws a Sury error with that exact expected schema
  could be flattened as though it came from the group.
- Same-tier source order is preserved, but semantically disjoint tag groups may
  be laid out in priority order. This is runtime-equivalent only while their
  accepted input sets remain disjoint.
- Duplicate member identities are intentionally retained because refiners and
  transforms may have effects. Repeating a pure schema therefore repeats work.
