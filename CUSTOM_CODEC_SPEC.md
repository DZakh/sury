# Custom codec spec

**Proposal — not implemented.** Spec for the breaking change that removes
`S.transform` from the ReScript surface (and `$res_transform` from the entry) in
favor of `S.to` with an explicit per-direction conversion object, and makes
async conversions — decode *and* encode — first-class on both surfaces.
`CODEC_SPEC.md` stays the normative statement of *built-in* conversions; this
file governs the *custom* ones layered on the same `S.to`.

Two decisions are still marked **[open]** below: the exact Reject semantics and
the custom-coder seam. Everything else is settled.

## Current state, and what's wrong with it

Three APIs exist for the same job:

1. **JS `S.to(s1, s2, decode?, encode?)`** — either coder may be omitted
   independently. The typing is seam-inconsistent: `decode: (TOutput) =>
   TTargetInput` lands on the target's input, but `encode: (TTargetOutput) =>
   TOutput` claims to start from the target's *output* while the runtime wiring
   (serializer on the target's head node) actually hands it the target's
   *input*-space value. For non-transforming targets the two spaces coincide, so
   nobody has noticed; for transforming targets the declared type is wrong.
2. **ReScript `S.transform(schema, () => {parser?, asyncParser?, serializer?})`**
   — the output side of the schema is an opaque `unknown` copy, so
   `S.reverse`, `outputExpression` and `toJSONSchema` see nothing. Missing
   parser/serializer is only reported when an operation in that direction is
   compiled. There is no async serializer at all.
3. **No JS async conversion at all** — `asyncParser` exists only behind
   `$res_transform`; the JS surface stops at `S.asyncDecoderAssert`, which can
   check but not convert.

Prior art (as of 2026): **Zod 4** `z.codec(A, B, {decode, encode})` and **Effect**
`Schema.transform(from, to, {strict, decode, encode})` both force the pair as
required object keys, and both put the custom functions at the
`A.Output ↔ B.Input` seam (the target still runs after decode). Zod discovers
asyncness at call time (`decodeAsync`, runtime error from sync `decode`);
Effect erases the question by returning `Effect` from both coders. Effect's
`strict` is compile-time only. Zod's one-way `.transform()` poisons `encode`
with a runtime throw; Effect opts out per direction by failing with a
`Forbidden` issue. Valibot and ArkType have no encode direction. Sury compiles
operations ahead of time, so it cannot copy Zod's call-time discovery — it
needs to know sync/async *before* the coder runs, hence the tagged shapes
below.

## The API

One optional argument to `S.to`; each direction is one value of a closed
five-case set — auto (built-in), reject, sync function, async function,
reject-with-message:

```ts
s1.with(S.to, s2);
s1.with(S.to, s2, {
  decode: fn | "auto" | "reject" | { async: fn } | { reject: string },
  encode: fn | "auto" | "reject" | { async: fn } | { reject: string },
});
```

```ts
// S.d.ts
type Conversion<A, B> =
  | ((value: A) => B)
  | "auto"
  | "reject"
  | { async: (value: A) => Promise<B>; reject?: never }
  | { reject: string; async?: never };

type Custom<TOutput, TTargetOutput> = {
  decode: Conversion<TOutput, TTargetOutput>;
  encode: Conversion<TTargetOutput, TOutput>;
};
```

```rescript
type conversion<'i, 'o> =
  | @as("auto") Auto
  | @as("reject") Reject
  | Sync('i => 'o)
  | Async('i => promise<'o>)
  | RejectWithMessage(string)

type custom<'from, 'to> = {
  decode: conversion<'from, 'to>,
  encode: conversion<'to, 'from>,
}

// Bound with a small S.res adapter, not a direct external
let to: (t<'from>, t<'to>, ~custom: custom<'from, 'to>=?) => t<'to>
```

```rescript
s1->S.to(s2)
s1->S.to(S.any, ~custom={
  decode: Async(userId => loadUser(~userId)),
  encode: Sync(user => user.id),
})
```

- Both fields are **required** whenever the object is given — the record/object
  type enforces it. Omitting the object entirely is `Auto`/`Auto`, i.e. plain
  `S.to`.
- The ReScript variant cannot be `@unboxed` — `Async` and `RejectWithMessage`
  would both erase to plain objects, which untagged variants can't tell apart.
  `Auto`/`Reject` erase to `"auto"`/`"reject"` via `@as` for free; the three
  payload cases go through a per-slot adapter in `S.res` (`Sync(f)` → `f`,
  `Async(f)` → `{async: f}`, `RejectWithMessage(m)` → `{reject: m}`) — the one
  place this proposal spends `S.res.mjs` bytes, sanctioned by the
  adapt-argument-shape convention.
- The TS `Conversion` union's `instantiations` cost lands on every `S.to` call
  site and must be priced by the spec run, together with an *inference* fixture
  (does `decode`'s parameter type resolve on a generic target?).

## Rule 1: `Auto` is the explicit built-in

`Auto` in a slot means "use the built-in conversion for this direction", under
exactly the `CODEC_SPEC.md` rules; if no built-in codec exists for the pair,
the operation needing that direction fails at operation creation (the timing
built-in checks already have). Mixed objects — custom one way, built-in the
other — are legal for the first time.

**Guard: `Auto` toward an `unknown`-tagged side is a creation error.** The
built-in conversion to `unknown` is pass-through, so `{decode: Sync(f),
encode: Auto}` with an `S.any` target would silently return transformed values
unchanged — the exact place every migrated parser-only `S.transform` would
otherwise land. The error names the two honest spellings: `Sync(x => x)` or
`Reject`.

## Rule 2 [open, recommended]: `Reject` is an unreachable path, `S.never`-style

A rejected direction is not a poisoned operation — it is an unreachable path,
with the same semantics `S.never` variants already have in `CODEC_SPEC.md`:

- **Standalone**: compiling an operation that needs the rejected direction
  fails at operation creation; `RejectWithMessage`'s string is the reason.
- **Inside a union**: the variant doesn't participate in that direction —
  skipped in dispatch, not counted toward rule 4 coverage, never triggering the
  partial-match rejections.
- **All variants rejected** for a direction → operation-creation error
  aggregating the messages.

Consequences:

- **`Option.getOr` becomes a plain union** and `Option_getWithDefault`'s
  special machinery (and its FIXMEs) is deleted:

  ```rescript
  S.union([
    S.string,
    S.unit->S.to(S.string, ~custom={decode: Sync(_ => "anonymous"), encode: Reject}),
  ])
  ```

  Forward: `undefined` decodes to the default. Reverse: strings dispatch to the
  same-type variant; the rejected-encode variant is unreachable. `getOrWith` is
  `Sync(_ => cb())`; async defaults (`Async(_ => fetchDefault())`) come free.
  `S.Option.getOr` stays public as sugar because it also stamps `default`
  metadata for JSON Schema, which can't be introspected out of a closure.
- **The `S.never.with(S.to, X)` idiom is subsumed** — the rule 2/3 error
  messages in `CODEC_SPEC.md` can suggest `{decode: "reject", encode: "auto"}`
  rewrites instead of `S.never` wrapping.
- **Naming caution**: under these semantics "reject" means *unreachable*, not
  *fails when reached* — in a union it yields to a sibling rather than
  erroring. Per-value failure is spelled with a throwing function. If that
  reading is too surprising, the honest name is `"never"`.

The alternative — `Reject` as a hard, whole-operation rejection even inside
unions (the no-salvage stance) — errors earlier but blocks the `getOr`
unification and keeps `S.never` wrapping as a second concept.

## Rule 3 [open, recommended]: custom coders land on s2's **output**

```
Sync/Async decode : s1.Output -> s2.Output
Sync/Async encode : s2.Output -> s1.Output
```

The custom pair bypasses `s2`'s own forward pipeline; `s2` contributes its
output-side refinements (which run on decode's result), validation of `'to`
values in a reversed parse, and the output type for `reverse`,
`outputExpression`, `toJSONSchema`, error messages and union dispatch.

For (each also argued in reverse for the junction seam, `s2.Input`, which
Zod/Effect use):

- It is the only ReScript-typeable seam (`t<'value>` carries the output type
  alone) — the junction seam would make `Sync('from => 'to)` a lie on any
  transforming target.
- No double work: the junction seam re-validates and possibly re-transforms
  decode's result, against the stated avoid-double-validation goal.
- It matches the schema-as-type model: `S.to(target)` is "convert to this
  type"; custom is "convert to this type, my way".
- The junction is still one chain link away, folded into the same compiled
  function: `s1->S.to(S.json, ~custom={…})->S.to(userSchema)`. The reverse
  direction of this trade is not available — under the junction seam there is
  no spelling that *skips* the target's pipeline.

Junction's genuine advantages, conceded: `Auto` and custom functions would
share one seam on every target (under the output seam a mixed object on a
transforming target straddles two seams and stops being an inverse pair);
decode results get validated by `s2`; and it is what Zod/Effect users expect.

**Guard that fences off the divergence:** attaching a custom `Sync`/`Async`
coder when the target carries its own `.to` chain is a creation error — "target
has its own conversion; chain `.to` explicitly". On plain targets (primitives,
instances, `S.any` — the overwhelming majority) the two seams are
indistinguishable, so the open decision only prices the guarded case.

## Rule 4: sync/async is static, per direction

- `Async` decode compiles through the existing `asyncParser` path
  (`B_embedTransformation(_, _, true)`): sync operations fail with the existing
  "use parseAsyncOrThrow" invalid operation, `S.isAsync` reports true.
- `Async` encode is supported: under `S.reverse` the encode slot becomes the
  reversed chain's *parser*, and async parsing already exists.
  `S.isAsync(S.reverse(schema))` reports true, `S.asyncEncoder` runs it, sync
  `S.encoder` fails at compile. The forward direction stays sync-parseable when
  only the encode side is async.
- **Async inside a union** inherits whatever `asyncParser`-in-union does today —
  but this API makes it reachable from JS for the first time, and union
  fall-through is built on synchronous dispatch (a rejected promise can't hand
  the value to the next variant). Needs its own spec row; a creation-time
  rejection of `Async` in multi-variant positions is the fallback.

## Rule 5: failure semantics are unchanged

A coder fails by throwing. A thrown `S.Error` is adopted as-is; anything else
surfaces as `invalid_conversion` with the original as `cause` and the reached
path prepended — exactly the current `S.transform`/custom-`to` behavior, and
the union fall-through / foreign-exception rules of `CODEC_SPEC.md` apply
untouched.

## Rule 6: metadata machinery degrades gracefully

`S.meta({examples})` maps examples through `getDecoder(reverse(schema))` at
creation; `Option.getOr` runs decoders at creation for the default's input
form; `toJSONSchema` tries encode-reversal for schemas with a user `.to`. A
`Reject` or `Async` encode must not detonate these: metadata derivation skips
what it can't compute (no example input-forms, best-effort JSON Schema) —
only real value operations raise. Double reversal (`reverse(reverse(s))`)
restores every slot exactly, including these degradations.

## Reversal

`S.reverse` of `s1->S.to(s2, ~custom)` is a schema from `'to` to `s1.Input`:
validate against `s2`'s output side, run the encode slot, continue through `s1`
reversed. The decode and encode slots trade places the way
`parser`/`serializer` do today — `Auto` and `Reject` slots included.

## Surface changes (breaking)

| Surface  | Removed                                                    | Changed                                                                 | Added                                          |
| -------- | ---------------------------------------------------------- | ------------------------------------------------------------------------ | ---------------------------------------------- |
| ReScript | `S.transform`, `S.transformDefinition`                     | `S.to` gains `~custom=?` (adapter in `S.res`)                            | `S.conversion`, `S.custom` types, `S.any`      |
| JS       | positional `decode`/`encode` params of `S.to`              | third arg becomes the `Custom` object; coders move to the settled seam   | `Conversion`/`Custom` types, async + reject slots |
| entry.ts | `transform as $res_transform` (~4.5k in `bundleSize.yaml`) | —                                                                        | —                                              |

The internal `transform` machinery in `modifiers.ts` shrinks to whatever
`js_asyncDecoderAssert` still needs, or is re-expressed through the codec
wiring and deleted. If Rule 2 lands as recommended, `Option_getWithDefault`
goes with it.

## Migration

| Before                                            | After                                                                      |
| ------------------------------------------------- | --------------------------------------------------------------------------- |
| `s->S.transform(() => {parser, serializer})`      | `s->S.to(target, ~custom={decode: Sync(parser), encode: Sync(serializer)})`  |
| `s->S.transform(() => {parser})`                  | `…, encode: Reject})` — **not `Auto`** (see Rule 1's guard)                  |
| `s->S.transform(() => {asyncParser, serializer})` | `s->S.to(target, ~custom={decode: Async(asyncParser), encode: Sync(serializer)})` |
| `s->S.transform(() => {asyncParser})`             | `…{decode: Async(asyncParser), encode: Reject})`                             |
| no natural target schema                          | `target = S.any` (byte-for-byte the old transform behavior)                  |
| JS `S.to(s1, s2, decode, encode)`                 | `S.to(s1, s2, {decode, encode})`                                             |
| JS `S.to(s1, s2, decode)` (one-sided)             | `S.to(s1, s2, {decode, encode: "reject"})` or `"auto"`                       |
| `S.never.with(S.to, X)` union arms                | `{decode: "reject", encode: "auto"}` (Rule 2)                                |

## Spec plan

Every row lands as `packages/sury/specs/*.yaml` through the spec skill — codegen
snapshots plus `examples` for the behaviors above. At minimum:

- `codec-custom-pair` — sync pair on a typed target: generated parse/encode
  code, target refinement running after decode, reverse round-trip.
- `codec-custom-mixed-auto` — custom decode + `Auto` encode; plus the
  `Auto`-toward-`unknown` creation error.
- `codec-custom-async-decode` / `codec-custom-async-encode` — codegen,
  `isAsync` on each side, sync-op rejection; async-in-union row.
- `codec-custom-reject` — standalone operation-creation error with the
  authored message; union-variant skipping; the `getOr`-as-union golden
  diffed against today's `Option.getOr` output.
- `codec-custom-transforming-target` — the Rule 3 guard error.
- `codec-custom-chained-input` — the chain-through-input idiom.
- `codec-custom-any` — `S.any` target reproducing today's `transform` golden.
- `codec-custom-union-any` — an `S.any`-targeted codec inside a union: the
  degraded reverse dispatch that real targets fix.
- `bundleSize.yaml` — `$res_transform` row deleted; new row for the `S.res`
  adapter; `instantiations` prices the `Conversion` union at call sites.

Union-adjacent behavior changes (Rule 2 in unions, `getOr` reimplementation)
require the `pnpm --filter=sury fuzz:union --ref=HEAD` diff before and after,
per `CLAUDE.md`. Test migration follows the table above;
`docs/rescript-usage.md`'s Transforms chapter and the `S.shape` note pointing
at `S.transform` are rewritten around `S.to`.

## Open questions

- Rule 2: `Reject` as unreachable (recommended) vs hard rejection; and the
  `"reject"` vs `"never"` name.
- Rule 3: output seam (recommended) vs junction seam.
- Whether decode's result should skip the target's refinements too (this spec
  says they run; generated-code cost could overturn it).
