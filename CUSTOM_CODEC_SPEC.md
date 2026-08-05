# Custom codec spec

**Settled, not implemented.** Spec for the breaking change that removes
`S.transform` from the ReScript surface (and `$res_transform` from the entry) in
favor of `S.to` with an explicit per-direction conversion object, and makes
async conversions — decode *and* encode — first-class on both surfaces.
`CODEC_SPEC.md` stays the normative statement of *built-in* conversions; this
file governs the *custom* ones layered on the same `S.to`.

## Why (condensed)

Today three APIs cover one job, badly: JS `S.to(s1, s2, decode?, encode?)`
allows one-sided coders and its d.ts is seam-inconsistent (decode typed into
the target's input, encode typed from the target's output, runtime handing it
the target's input-space value); ReScript `S.transform` hides the output side
behind an opaque `unknown` (blinding `reverse`, `outputExpression`,
`toJSONSchema`) and reports missing directions only at operation compile; and
the JS surface has no async conversion at all. Zod 4 (`z.codec`) and Effect
(`Schema.transform`) both force `{decode, encode}` as required keys — but both
discover asyncness at call time, which Sury cannot: operations are compiled
ahead of time, so sync/async must be visible in the definition. Valibot and
ArkType have no encode direction.

## The API

```ts
s1.with(S.to, s2);
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

type Custom<TOutput, TTargetOutput> = {
  decode: Conversion<TOutput, TTargetOutput>;
  encode: Conversion<TTargetOutput, TOutput>;
};
```

```rescript
type conversion<'i, 'o> =
  | Auto
  | Never
  | Sync('i => 'o)
  | Async('i => promise<'o>)

type custom<'from, 'to> = {
  decode: conversion<'from, 'to>,
  encode: conversion<'to, 'from>,
}

// Adapter in S.res, not a direct external (see below)
let to: (t<'from>, t<'to>, ~custom: custom<'from, 'to>=?) => t<'to>
```

```rescript
s1->S.to(s2)
s1->S.to(S.any, ~custom={
  decode: Async(userId => loadUser(~userId)),
  encode: Sync(user => user.id),
})
```

- Both fields are **required** whenever the object is given. Omitting the
  object is `Auto`/`Auto`, i.e. plain `S.to`.
- **ReScript adapter**: `Auto`/`Never` compile to `"auto"`/`"never"` via `@as`
  and pass through; `Sync(f)` → `f`, `Async(f)` → `{async: f}` are mapped by a
  small per-slot switch in `S.res` before calling the public JS `to`. This is
  the one place the proposal spends `S.res.mjs` bytes, sanctioned by the
  adapt-argument-shape convention. (With no message payload the variant could
  be `@unboxed` — `Async({async: f})` spelling, zero adapter — rejected for the
  worse call-site DX; recorded here in case the adapter's bytes ever matter.)
- The TS `Conversion` union's `instantiations` cost lands on every `S.to` call
  site and must be priced by the spec run, together with an inference fixture
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
`Never`.

## Rule 2: `Never` is an unreachable path — the `S.never` rules, spelled inline

A `Never` direction is not a poisoned operation and not a per-value failure —
it is an unreachable path, with the same semantics `S.never` variants already
have in `CODEC_SPEC.md`:

- **Standalone**: compiling an operation that needs the direction fails at
  operation creation — `Decoding string to number is never performed`-style,
  naming both schemas and the direction. There is no per-slot message form;
  per-value failure is spelled with a throwing `Sync`/`Async` function.
- **Inside a union**: the variant doesn't participate in that direction —
  skipped in dispatch, not counted toward rule 4 coverage, never triggering
  the partial-match rejections. It *yields* to siblings; it never errors there.
- **All variants `Never`** for a direction → operation-creation error.

Consequences:

- **`Option.getOr` becomes a plain union** and `Option_getWithDefault`'s
  special machinery (and its FIXMEs) is deleted:

  ```rescript
  S.union([
    S.string,
    S.unit->S.to(S.string, ~custom={decode: Sync(_ => "anonymous"), encode: Never}),
  ])
  ```

  Forward: `undefined` decodes to the default. Reverse: strings dispatch to the
  same-type variant; the `Never`-encode variant is unreachable. `getOrWith` is
  `Sync(_ => cb())`; async defaults (`Async(_ => fetchDefault())`) come free.
  `S.Option.getOr` stays public as sugar because it also stamps `default`
  metadata for JSON Schema, which can't be introspected out of a closure.
- **The `S.never.with(S.to, X)` idiom is subsumed** — the rule 2/3 error
  messages in `CODEC_SPEC.md` suggest `{decode: "never", encode: "auto"}`
  rewrites instead of `S.never` wrapping.

## Rule 3: custom coders land on s2's **output**

```
Sync/Async decode : s1.Output -> s2.Output
Sync/Async encode : s2.Output -> s1.Output
```

The custom pair bypasses `s2`'s own forward pipeline; `s2` contributes its
output-side refinements (which run on decode's result), validation of `'to`
values in a reversed parse, and the output type for `reverse`,
`outputExpression`, `toJSONSchema`, error messages and union dispatch. Chosen
over the Zod/Effect junction seam (`s1.Output -> s2.Input`) because it is the
only ReScript-typeable seam, it avoids double validation/transformation of
decode's result, and it matches the schema-as-type model — while the junction
stays one chain link away, folded into the same compiled function:

```rescript
s1->S.to(S.json, ~custom={decode: Sync(parse), encode: Sync(print)})->S.to(userSchema)
```

**Guard: a custom `Sync`/`Async` coder on a target that carries its own `.to`
chain is a creation error** — "target has its own conversion; chain `.to`
explicitly". This fences off the only case where the two seams diverge (and
where a mixed `Auto` would straddle seams). On plain targets — primitives,
instances, `S.any`, the overwhelming majority — the seams are
indistinguishable.

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
`Never` or `Async` encode must not detonate these: metadata derivation skips
what it can't compute (no example input-forms, best-effort JSON Schema) — only
real value operations raise. Double reversal (`reverse(reverse(s))`) restores
every slot exactly, including these degradations.

## Reversal

`S.reverse` of `s1->S.to(s2, ~custom)` is a schema from `'to` to `s1.Input`:
validate against `s2`'s output side, run the encode slot, continue through `s1`
reversed. The decode and encode slots trade places the way
`parser`/`serializer` do today — `Auto` and `Never` slots included.

## Surface changes (breaking)

| Surface  | Removed                                                    | Changed                                                               | Added                                             |
| -------- | ---------------------------------------------------------- | ---------------------------------------------------------------------- | ------------------------------------------------- |
| ReScript | `S.transform`, `S.transformDefinition`                     | `S.to` gains `~custom=?` (adapter in `S.res`)                          | `S.conversion`, `S.custom` types, `S.any`         |
| JS       | positional `decode`/`encode` params of `S.to`              | third arg becomes the `Custom` object; coders move to the output seam  | `Conversion`/`Custom` types, async + never slots  |
| entry.ts | `transform as $res_transform` (~4.5k in `bundleSize.yaml`) | —                                                                      | —                                                 |

## Migration

| Before                                            | After                                                                            |
| ------------------------------------------------- | --------------------------------------------------------------------------------- |
| `s->S.transform(() => {parser, serializer})`      | `s->S.to(target, ~custom={decode: Sync(parser), encode: Sync(serializer)})`        |
| `s->S.transform(() => {parser})`                  | `…, encode: Never})` — **not `Auto`** (Rule 1's guard)                             |
| `s->S.transform(() => {asyncParser, serializer})` | `s->S.to(target, ~custom={decode: Async(asyncParser), encode: Sync(serializer)})`  |
| `s->S.transform(() => {asyncParser})`             | `…{decode: Async(asyncParser), encode: Never})`                                    |
| no natural target schema                          | `target = S.any` (byte-for-byte the old transform behavior)                        |
| JS `S.to(s1, s2, decode, encode)`                 | `S.to(s1, s2, {decode, encode})`                                                   |
| JS `S.to(s1, s2, decode)` (one-sided)             | `S.to(s1, s2, {decode, encode: "never"})` or `"auto"`                              |
| `S.never.with(S.to, X)` union arms                | `X` with `{decode: "never", encode: "auto"}` on its conversion link (Rule 2)       |

## Implementation plan

Each phase goes through the spec skill; the printed metric summary is the
deliverable per phase.

1. **Runtime core** (`jsapi.ts`, touching `union.ts`, `parse.ts`):
   rework `js_to(schema, target, custom?)` — creation-time shape validation
   (both keys, known slot values, Rule 1 and Rule 3 guards), slot wiring:
   `Sync` parser/serializer builders targeting the output seam, `Async` through
   `B_embedTransformation(_, _, true)` on each side, `Never` marking the
   direction's pipeline as `never_` so `unionFactory` applies its existing
   unreachable-variant rules, `Auto` falling through to the built-in path.
   Union dispatch is touched ⇒ `pnpm --filter=sury fuzz:union --ref=HEAD`
   before and after.
2. **Types** (`S.d.ts`): `Conversion`/`Custom`, the new `S.to` signature and
   `with` overload, delete the positional-coders overload; `instantiations` +
   inference fixtures in the specs.
3. **ReScript surface** (`S.res`, `entry.ts`): delete `transform`/
   `transformDefinition`, add `conversion`/`custom` and the `let to` adapter,
   bind `S.any`; drop `$res_transform` from the entry and `bundleSize.yaml`.
4. **`Option.getOr` reimplementation** on Rule 2's union spelling, keeping the
   `default`-metadata stamping; delete `Option_getWithDefault`. Separable —
   land after 1–3 stabilize.
5. **Internal cleanup**: re-express `js_asyncDecoderAssert` through the codec
   wiring; delete the internal `transform` in `modifiers.ts`.
6. **Specs, tests, docs**: rows below; migrate `S_transform_*` tests to
   `S_to_*`; rewrite `docs/rescript-usage.md`'s Transforms chapter and the
   `S.shape` note; update `CODEC_SPEC.md`'s suggested rewrites to the
   `"never"` spellings.

## Spec coverage

- `codec-custom-pair` — sync pair on a typed target: generated parse/encode
  code, target refinement running after decode, reverse round-trip.
- `codec-custom-mixed-auto` — custom decode + `Auto` encode; the
  `Auto`-toward-`unknown` creation error.
- `codec-custom-async-decode` / `codec-custom-async-encode` — codegen,
  `isAsync` on each side, sync-op rejection; async-in-union row.
- `codec-custom-never` — standalone operation-creation error; union-variant
  skipping; the `getOr`-as-union golden diffed against today's `Option.getOr`
  output.
- `codec-custom-transforming-target` — the Rule 3 guard error.
- `codec-custom-chained-input` — the chain-through-input idiom.
- `codec-custom-any` — `S.any` target reproducing today's `transform` golden.
- `codec-custom-union-any` — an `S.any`-targeted codec inside a union: the
  degraded reverse dispatch that real targets fix.
- `bundleSize.yaml` — `$res_transform` row deleted; new row for the `S.res`
  adapter; `instantiations` prices the `Conversion` union at call sites.

## Deliberately out of scope

- Per-slot failure messages (dropped with the `Reject` naming — the generated
  error already names both schemas and the direction).
- A seam-choice option (Rule 3 is final; the chain idiom covers the junction).
- Async encode messages/`asyncEncoder` ergonomics beyond what reversal gives.
