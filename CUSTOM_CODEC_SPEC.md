# Custom codec spec

**Settled, not implemented.** Replaces `S.transform` (and the `$res_transform`
entry export) with an explicit per-direction codec argument on `S.to`, and makes
async conversions — decode *and* encode — first-class on both surfaces.
`CODEC_SPEC.md` governs the *built-in* conversions; this file governs the
*custom* ones layered on the same `S.to`.

Why: today JS `S.to(s1, s2, decode?, encode?)` allows one-sided coders with a
seam-inconsistent d.ts; ReScript `S.transform` hides the output side behind an
opaque `unknown` (blinding `reverse`, `outputExpression`, `toJSONSchema`); and
JS has no async conversion at all. Zod 4 (`z.codec`) and Effect
(`Schema.transform`) both force `{decode, encode}` as required keys but discover
asyncness at call time — Sury compiles operations ahead of time, so sync/async
must be visible in the definition.

## The API

```ts
s1.with(S.to, s2);           // built-in conversion (unchanged)
s1.with(S.to, s2, decodeFn); // shorthand — encode errors at operation creation (rule 3)
s1.with(S.to, s2, {
  decode: fn | "auto" | "never" | { async: fn },
  encode: fn | "auto" | "never" | { async: fn },
});
```

```ts
// S.d.ts
type Conversion<A, B> =
  | ((value: A) => B)
  | "auto"
  | "never"
  | { async: (value: A) => Promise<B> };

type Codecs<TOutput, TTargetOutput> = {
  decode: Conversion<TOutput, TTargetOutput>;
  encode: Conversion<TTargetOutput, TOutput>;
};
```

```rescript
type conversion<'i, 'o> =
  | @as("auto") Auto
  | @as("never") Never
  | Sync('i => 'o)
  | Async('i => promise<'o>)

type codecs<'from, 'to> = {
  decode: conversion<'from, 'to>,
  encode: conversion<'to, 'from>,
}

// Adapter in S.res, not a direct external
let to: (t<'from>, t<'to>, ~custom: codecs<'from, 'to>=?) => t<'to>
```

```rescript
s1->S.to(s2)
s1->S.to(S.any, ~custom={
  decode: Async(userId => loadUser(~userId)),
  encode: Sync(user => user.id),
})
```

- Both `codecs` fields are required. Omitting the argument is `Auto`/`Auto`.
- `@as` erases `Auto`/`Never` to the exact JS strings; `Sync`/`Async` keep the
  default variant representation, which a per-slot adapter in `S.res` unwraps
  to `f` / `{async: f}` before calling the public JS `to`. That switch is the
  proposal's only `S.res.mjs` cost. ReScript has no shorthand form — the
  record is always full.
- The TS `Conversion` union's `instantiations` cost lands on every `S.to` call
  site — priced by the spec run, plus an inference fixture (does `decode`'s
  parameter type resolve on a generic target?).

## Rule 1: `Auto` is the built-in conversion

`Auto` means "use the built-in conversion for this direction", under exactly
the `CODEC_SPEC.md` rules — including toward/from `unknown` sides, where the
built-in is a pass-through upcast (to `unknown`) or validating downcast (from
`unknown`). No built-in codec for the pair → the operation needing that
direction fails at operation creation, as built-in checks already do. Mixed
objects — custom one way, built-in the other — are legal for the first time:

```rescript
// One-way normalization: validating pass-through on encode
S.string->S.to(S.string, ~custom={decode: Sync(String.trim), encode: Auto})
```

## Rule 2: `Never` is an unreachable path

The `S.never` rules from `CODEC_SPEC.md`, spelled inline. Not a per-value
failure (that's a throwing function) and no message form:

- **Standalone**: compiling an operation that needs the direction fails at
  operation creation, naming both schemas and the direction.
- **In a union**: the variant is skipped for that direction — not counted
  toward rule 4 coverage, no partial-match rejections. It yields to siblings;
  all variants `Never` → operation-creation error.
- Subsumes the `S.never.with(S.to, X)` idiom; `CODEC_SPEC.md`'s suggested
  rewrites switch to `{decode: "never", encode: "auto"}` spellings.
- **`Option.getOr` becomes a plain union**, deleting `Option_getWithDefault`
  and its FIXMEs — `getOrWith` is `Sync(_ => cb())`, async defaults come free,
  and `S.Option.getOr` stays public as sugar that also stamps `default`
  metadata (not introspectable out of a closure):

  ```rescript
  S.union([
    S.string,
    S.unit->S.to(S.string, ~custom={decode: Sync(_ => "anonymous"), encode: Never}),
  ])
  ```

## Rule 3: the decode shorthand has an ambiguous encode

`s1.with(S.to, s2, decodeFn)` wires the decode slot only. Compiling any
operation that needs the encode direction fails **at operation creation**:
`Encoding is ambiguous when only a decode function is provided — use
S.to(target, {decode, encode})`. Unlike `Never` this is a hard error even
inside a union — silently skipping would commit to a semantics the user never
chose. (Breaking: today's 3-arg call falls back to the built-in encode.)

## Rule 4: custom coders land on s2's **output**

```
decode : s1.Output -> s2.Output
encode : s2.Output -> s1.Output
```

The custom pair bypasses `s2`'s forward pipeline; `s2` contributes its
output-side refinements (they run on decode's result), validation of `'to`
values in a reversed parse, and the output type for `reverse`,
`outputExpression`, `toJSONSchema`, error messages and union dispatch. Chosen
over the Zod/Effect junction seam (`-> s2.Input`) because it is the only
ReScript-typeable seam, avoids double validation of decode's result, and
matches the schema-as-type model. The junction stays one chain link away,
folded into the same compiled function:

```rescript
s1->S.to(S.json, ~custom={decode: Sync(parse), encode: Sync(print)})->S.to(userSchema)
```

**Guard:** a custom `Sync`/`Async` coder on a target that carries its own `.to`
chain is a creation error — "target has its own conversion; chain `.to`
explicitly". That fences off the only case where the seams diverge; on plain
targets (primitives, instances, `S.any`) they are indistinguishable.

## Rule 5: sync/async is static, per direction

- `Async` decode compiles through the existing `asyncParser` path: sync
  operations fail with the existing "use parseAsyncOrThrow" invalid operation;
  `S.isAsync` reports true.
- `Async` encode rides reversal: under `S.reverse` the encode slot becomes the
  reversed chain's parser, so `S.asyncEncoder` runs it,
  `S.isAsync(S.reverse(schema))` reports true, sync `S.encoder` fails at
  compile, and the forward direction stays sync-parseable.
- Async inside a union inherits today's `asyncParser`-in-union behavior — newly
  reachable from JS, and union fall-through is synchronous dispatch, so it
  needs its own spec row; creation-time rejection in multi-variant positions is
  the fallback.

## Rule 6: failure and metadata

A coder fails by throwing: a thrown `S.Error` is adopted as-is, anything else
surfaces as `invalid_conversion` with the original as `cause` and the reached
path prepended — unchanged, union rules of `CODEC_SPEC.md` untouched.

Metadata machinery degrades gracefully: `S.meta({examples})`, `Option.getOr`
defaults and `toJSONSchema` run reverse decoders at creation — a `Never` or
`Async` encode makes them skip what they can't compute (no example input-forms,
best-effort JSON Schema) rather than throw. Only real value operations raise.

## Reversal

`S.reverse` of `s1->S.to(s2, ~custom)` is a schema from `'to` to `s1.Input`:
validate against `s2`'s output side, run the encode slot, continue through `s1`
reversed. Decode and encode slots trade places the way `parser`/`serializer`
do — `Auto`, `Never` and the shorthand's ambiguous encode included — and
double reversal restores every slot exactly.

## Surface changes (breaking)

| Surface  | Removed                                                    | Changed                                                        | Added                                            |
| -------- | ---------------------------------------------------------- | --------------------------------------------------------------- | ------------------------------------------------ |
| ReScript | `S.transform`, `S.transformDefinition`                     | `S.to` gains `~custom=?` (adapter in `S.res`)                   | `S.conversion`, `S.codecs` types, `S.any`        |
| JS       | 4-arg positional `S.to`; 3-arg changes meaning (rule 3)    | third arg becomes `fn \| Codecs`; coders move to the output seam | `Conversion`/`Codecs` types, async + never slots |
| entry.ts | `transform as $res_transform` (~4.5k in `bundleSize.yaml`) | —                                                               | —                                                |

## Migration

| Before                                            | After                                                                             |
| ------------------------------------------------- | ---------------------------------------------------------------------------------- |
| `s->S.transform(() => {parser, serializer})`      | `s->S.to(target, ~custom={decode: Sync(parser), encode: Sync(serializer)})`         |
| `s->S.transform(() => {parser})`                  | `…, encode: Never})` keeps the old fail-on-encode; `Auto` is legal but validates    |
| `s->S.transform(() => {asyncParser, serializer})` | `s->S.to(target, ~custom={decode: Async(asyncParser), encode: Sync(serializer)})`   |
| `s->S.transform(() => {asyncParser})`             | `…{decode: Async(asyncParser), encode: Never})`                                     |
| no natural target schema                          | `target = S.any` (byte-for-byte the old transform behavior)                         |
| JS `S.to(s1, s2, decode, encode)`                 | `S.to(s1, s2, {decode, encode})`                                                    |
| JS `S.to(s1, s2, decode)`                         | unchanged spelling, new meaning — add `encode: "auto"` to keep the old behavior     |
| `S.never.with(S.to, X)` union arms                | `X` with `{decode: "never", encode: "auto"}` on its conversion link                 |

## Implementation plan

Each phase goes through the spec skill; the printed metric summary is the
deliverable per phase.

1. **Runtime core** (`jsapi.ts`, touching `union.ts`, `parse.ts`): rework
   `js_to(schema, target, custom?)` — creation-time shape validation (both
   keys, known slot values, rule 4's guard), slot wiring: `Sync` builders on
   the output seam, `Async` through `B_embedTransformation(_, _, true)` on
   each side, `Never` marking the direction as `never_` so `unionFactory`'s
   unreachable-variant rules apply, the shorthand's ambiguous-encode builder,
   `Auto` falling through to the built-in path. Union dispatch is touched ⇒
   `pnpm --filter=sury fuzz:union --ref=HEAD` before and after.
2. **Types** (`S.d.ts`): `Conversion`/`Codecs`, the three-form `S.to`
   signature and `with` overload; `instantiations` + inference fixtures.
3. **ReScript surface** (`S.res`, `entry.ts`): delete
   `transform`/`transformDefinition`, add `conversion`/`codecs` and the
   `let to` adapter, bind `S.any`; drop `$res_transform` from the entry and
   `bundleSize.yaml`.
4. **`Option.getOr`** reimplemented on rule 2's union spelling, keeping the
   `default`-metadata stamping; delete `Option_getWithDefault`. Separable —
   land after 1–3 stabilize.
5. **Internal cleanup**: re-express `js_asyncDecoderAssert` through the codec
   wiring; delete the internal `transform` in `modifiers.ts`.
6. **Specs, tests, docs**: rows below; migrate `S_transform_*` tests to
   `S_to_*`; rewrite `docs/rescript-usage.md`'s Transforms chapter and the
   `S.shape` note; update `CODEC_SPEC.md`'s suggested rewrites.

## Spec coverage

- `codec-custom-pair` — sync pair on a typed target: codegen, target
  refinements after decode, reverse round-trip.
- `codec-custom-shorthand` — decode-only form: forward codegen, encode's
  operation-creation error, hard error inside a union.
- `codec-custom-mixed-auto` — custom decode + `Auto` encode, including the
  normalization pattern on a same-type target and the validating downcast from
  an `S.any` target.
- `codec-custom-async-decode` / `codec-custom-async-encode` — codegen,
  `isAsync` per side, sync-op rejection; async-in-union row.
- `codec-custom-never` — standalone operation-creation error; union skipping;
  the `getOr`-as-union golden diffed against today's `Option.getOr` output.
- `codec-custom-transforming-target` — rule 4's guard error.
- `codec-custom-chained-input` — the chain-through-input idiom.
- `codec-custom-any` — `S.any` target reproducing today's `transform` golden;
  plus an `S.any`-targeted codec inside a union (the degraded reverse dispatch
  real targets fix).
- `bundleSize.yaml` — `$res_transform` row deleted; a row for the `S.res`
  adapter.

## Deliberately out of scope

- Per-slot failure messages (the generated error already names both schemas
  and the direction).
- A seam-choice option (rule 4 is final; the chain idiom covers the junction).
- Async-encode ergonomics beyond what reversal gives.
