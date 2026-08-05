# Custom codec spec

**Proposal — not implemented.** Spec for the breaking change that removes
`S.transform` from the ReScript surface (and `$res_transform` from the entry) in
favor of `S.to` with an explicit codec object, and makes async conversions —
decode *and* encode — first-class on both surfaces. `CODEC_SPEC.md` stays the
normative statement of *built-in* conversions; this file governs the *custom*
ones layered on the same `S.to`.

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
required object keys. Both put the custom functions at the `A.Output ↔ B.Input`
seam (the target still runs after decode). Zod discovers asyncness at call time
(`decodeAsync`, runtime error from sync `decode`); Effect erases the question by
returning `Effect` from both coders. Effect's `strict` is compile-time only —
`true` (default) requires the coders' return types to exactly match the opposite
schema's boundary type, `false` relaxes the check for branded/narrowed types; it
has no runtime meaning, and Sury doesn't need an equivalent (a cast at the
return expression covers the same rare cases). Zod's one-way `.transform()`
poisons `encode` with a runtime throw; Effect opts out per direction by failing
with a `Forbidden` issue. Valibot and ArkType have no encode direction. Sury
compiles operations ahead of time, so it cannot copy Zod's call-time discovery —
it needs to know sync/async *before* the coder runs.

## The seam: output ↔ output

The custom pair sits between **`s1`'s output and `s2`'s output**:

```
decode : s1.Output -> s2.Output
encode : s2.Output -> s1.Output
```

not at the `s1.Output ↔ s2.Input` junction Zod/Effect (and today's `js_to`
wiring) use. Three reasons:

- **It's the only ReScript-typeable seam.** `S.t<'value>` carries the output
  type alone, so `decode: 'from => 'to, encode: 'to => 'from` is exactly
  expressible; a function into `s2.Input` could only be typed `'from => unknown`.
- **The junction seam is still one chain link away.** Zod/Effect need the target
  to keep running after decode because their transform is the only conversion
  primitive. In Sury a chain is free — every `S.to` folds into the same compiled
  function — so "decode to the target's input" is spelled explicitly:

  ```rescript
  // decode lands on userSchema's *input*; userSchema still validates/maps
  S.string
  ->S.to(S.json, ~codec={decode: parseCustomFormat, encode: printCustomFormat})
  ->S.to(userSchema)
  ```

- **It makes the two directions actually symmetric**, fixing the d.ts today.

There is deliberately **no mode option** for choosing the seam. One semantics,
composition for the rest.

### What the target contributes

With a codec, `s2`'s own forward pipeline is bypassed — decode's word is final.
`s2` still earns its argument slot:

- Its **output-side refinements** run on decode's result (same as today's
  `customBuilder` path applying target refiners), and validate the value when
  the reversed schema is used as a parser. `S.encoder` keeps skipping
  validation, per the existing typed-decode rule in `union.ts`.
- It is the **type of the schema's output side**: `S.reverse`,
  `outputExpression`, `toJSONSchema` of the reverse, error messages and union
  dispatch all see a real type instead of `S.transform`'s opaque `unknown`.
- **`S.any`** (already a JS export; gets a ReScript binding `t<'a>`) is the
  explicit opt-out for genuinely opaque outputs — it reproduces `S.transform`'s
  old behavior exactly, since transform's hidden target *was* an `unknown` copy.

## The codec object

One optional third argument to `S.to`, Zod-codec-shaped. Each direction is one
of: a function (sync), a function under the `async*` key (async), or a string
(unsupported, the string is the error reason):

```ts
S.string.with(S.to, S.int32, {
  decode: (s) => parseIntOrThrow(s),
  encode: (i) => `${i}`,
});

S.uuid.with(S.to, userSchema, {
  asyncDecode: (id) => loadUser(id),
  encode: (user) => user.id,
});

S.string.with(S.to, S.string, {
  decode: hash,
  encode: "Password hashing is one-way",
});
```

```ts
// S.d.ts — exactly one flavor per direction, both directions required
type Codec<TOutput, TTargetOutput> = (
  | { decode: ((value: TOutput) => TTargetOutput) | string; asyncDecode?: never }
  | { asyncDecode: (value: TOutput) => Promise<TTargetOutput>; decode?: never }
) & (
  | { encode: ((value: TTargetOutput) => TOutput) | string; asyncEncode?: never }
  | { asyncEncode: (value: TTargetOutput) => Promise<TOutput>; encode?: never }
);
```

The union-of-shapes type is the statically-enforced version; its
instantiation cost lands on every `S.to` call site, so the spec run must price
it (`instantiations` in the ts block). If it measures badly, the fallback is
four plain optional keys with the same rules enforced at creation only.

ReScript mirrors the runtime shape with an optional-field record — same keys,
no adapter, the record *is* the JS object:

```rescript
type codec<'from, 'to> = {
  decode?: 'from => 'to,
  asyncDecode?: 'from => promise<'to>,
  encode?: 'to => 'from,
  asyncEncode?: 'to => promise<'from>,
}

@module("sury")
external to: (t<'from>, t<'to>, ~codec: codec<'from, 'to>=?) => t<'to> = "to"

// The unsupported-direction string, typed. `%identity` — compiles to the bare
// string, so the runtime contract is exactly the JS one.
external unsupported: string => ('a => 'b) = "%identity"
```

```rescript
let intFromString = S.string->S.to(S.int, ~codec={
  decode: s => s->Int.fromString->Option.getExn,
  encode: Int.toString,
})

let userSchema = S.uuid->S.to(S.any, ~codec={
  asyncDecode: userId => loadUser(~userId),
  encode: user => user.id,
})

let hashed = S.string->S.to(S.string, ~codec={
  decode: hash,
  encode: S.unsupported("Password hashing is one-way"),
})
```

## Rule 1: both directions, exactly one flavor each

A codec object must carry **exactly one of `decode`/`asyncDecode` and exactly
one of `encode`/`asyncEncode`**. Anything else — one direction missing, or both
flavors of one direction — is rejected **when the schema is created**. Sury
can't tell whether a missing direction should fail or fall back to built-in
coercion, so it refuses to guess — the same no-silent-salvage stance as
`CODEC_SPEC.md`'s "No built-in decoder for a variant".

- **TS**: enforced statically by the `Codec` union above (and the removal of
  the positional `decode`/`encode` parameters).
- **ReScript / untyped JS**: optional record fields can't express
  exactly-one, so it's a creation-time panic; the message teaches the string
  opt-out and the chain idiom.

Wanting a *built-in* conversion for one direction is spelled with a chain, not a
half codec — put the codec between schemas where it is total, and let plain
`S.to` links do the built-in legs.

## Rule 2: a string means "this direction is unsupported"

A string in the `decode` or `encode` slot declares the direction unsupported,
with the string as the reason. Compiling any operation that needs that
direction — `S.encoder`, a reversed parse — fails **at operation creation**
with an `invalid_operation` carrying the string; never per value. This
preserves `S.transform`'s best property (a missing serializer failed at
operation compile, not per value) while making the intent explicit and the
message user-authored. It is also the "explicitly opted out" that the
reversibility rule in `CLAUDE.md` requires. Zod's equivalent is a runtime
`ZodEncodeError` on first encode; Effect's is a per-value `Forbidden` issue —
operation-creation is earlier than both.

The `async*` keys take functions only — an unsupported direction has no
asyncness, so the string always sits on the plain key.

## Rule 3: sync and async split by key, per direction

Codegen must know a coder's asyncness before calling it (`await` insertion,
`isAsync`, sync-operation rejection), and detecting it from the function value
is unreliable — so it's static, in the key name.

- `asyncDecode` compiles through the existing `asyncParser` path
  (`B_embedTransformation(_, _, true)`) and inherits its rules: sync operations
  fail with the existing "use parseAsyncOrThrow" invalid operation, `S.isAsync`
  reports true.
- **`asyncEncode` is supported.** Under `S.reverse` the encode slot becomes the
  reversed chain's *parser*, and async parsing already exists — so an async
  encode is just an `asyncParser` of the reversed schema. `S.isAsync(S.reverse(schema))`
  reports true, `S.asyncEncoder` (already exported) runs it, and a sync
  `S.encoder` compile fails with the same invalid operation. What's genuinely
  new is only the bookkeeping: the forward schema stays sync-parseable when
  only `asyncEncode` is async.

## Rule 4: failure semantics are unchanged

A coder fails by throwing. A thrown `S.Error` is adopted as-is; anything else
surfaces as `invalid_conversion` with the original as `cause` and the reached
path prepended — exactly the current `S.transform`/custom-`to` behavior, and the
union fall-through / foreign-exception rules of `CODEC_SPEC.md` apply untouched.

## Reversal

`S.reverse` of `s1->S.to(s2, ~codec)` is a schema from `'to` to `s1.Input`:
validate against `s2`'s output side, run the encode slot, continue through `s1`
reversed. The decode and encode slots trade places the way `parser`/`serializer`
do today — string slots included, so an unsupported direction stays
unsupported under double reversal. Reversing twice restores the original.

## Surface changes (breaking)

| Surface  | Removed                                                    | Changed                                                                       | Added                                  |
| -------- | ---------------------------------------------------------- | ------------------------------------------------------------------------------ | -------------------------------------- |
| ReScript | `S.transform`, `S.transformDefinition`                     | `S.to` gains `~codec=?` (rules 1–3 at creation)                                | `S.codec` type, `S.any`, `S.unsupported` (`%identity`, zero bytes) |
| JS       | positional `decode`/`encode` params of `S.to`              | third arg becomes the `Codec` object; coder types move to the output↔output seam | `Codec` type, async keys, string opt-out |
| entry.ts | `transform as $res_transform` (~4.5k in `bundleSize.yaml`) | —                                                                              | —                                      |

No new entry export is needed at all: the codec object rides the existing `to`
export, `any` is already exported, and the ReScript `unsupported` is an
`%identity` cast. The internal `transform` machinery in `modifiers.ts` shrinks
to whatever `js_asyncDecoderAssert` still needs, or is re-expressed through the
codec wiring and deleted.

## Migration

| Before                                            | After                                                                 |
| ------------------------------------------------- | --------------------------------------------------------------------- |
| `s->S.transform(() => {parser, serializer})`      | `s->S.to(target, ~codec={decode: parser, encode: serializer})`         |
| `s->S.transform(() => {parser})`                  | `…, encode: S.unsupported("reason")})` — fail-on-encode is now explicit |
| `s->S.transform(() => {asyncParser, serializer})` | `s->S.to(target, ~codec={asyncDecode: asyncParser, encode: serializer})` |
| `s->S.transform(() => {asyncParser})`             | `…{asyncDecode, encode: S.unsupported("reason")})`                     |
| no natural target schema                          | `target = S.any` (byte-for-byte the old transform behavior)            |
| JS `S.to(s1, s2, decode, encode)`                 | `S.to(s1, s2, {decode, encode})`                                       |
| JS `S.to(s1, s2, decode)` (one-sided)             | add `encode` or a reason string                                        |

The custom-schema docs example gains a real reverse:

```rescript
let mySet = itemSchema =>
  S.instance(%raw(`Set`))
  ->S.to(S.any, ~codec={
    decode: decodeSetItems(itemSchema),
    encode: encodeSetItems(itemSchema),
  })
  ->S.meta({name: `Set.t<${S.inputExpression(itemSchema)}>`})
```

## Spec plan

Every row lands as `packages/sury/specs/*.yaml` through the spec skill — codegen
snapshots plus `examples` for the behaviors above. At minimum:

- `codec-custom-pair` — sync codec on a typed target: generated parse/encode
  code, target refinement running after decode, reverse round-trip.
- `codec-custom-async-decode` — `asyncDecode` codegen, `isAsync`, sync-op
  rejection.
- `codec-custom-async-encode` — `asyncEncode`: forward stays sync,
  `S.asyncEncoder` works, sync `S.encoder` rejected at compile.
- `codec-custom-invalid-shape` — creation-time rejections: missing direction,
  both flavors of one direction.
- `codec-custom-unsupported` — string slot failing at encoder creation with the
  authored reason, not per value; double-reverse keeps it.
- `codec-custom-chained-input` — the chain-through-input idiom compiling to the
  same code the junction seam would have produced.
- `codec-custom-any` — `S.any` target reproducing today's `transform` golden
  (diff against the existing transform-based specs before deleting them).
- `bundleSize.yaml` — `$res_transform` row deleted with no replacement row
  (`to` already ships); the `instantiations` metric prices the `Codec` union
  type at call sites — the gate for keeping static exactly-one enforcement.

Test migration follows the table above (`S_transform_*` tests become `S_to_*`);
`docs/rescript-usage.md`'s Transforms chapter and the `S.shape` note pointing at
`S.transform` are rewritten around `S.to`.

## Open questions

- **`Codec` type cost** — if the union-of-shapes type measures badly on
  `instantiations`, fall back to four optional keys + creation-time checks and
  accept losing static exactly-one in TS.
- **Should decode's result skip the target's refinements too?** This spec says
  they run (cheap, symmetric with reversed-parse validation), matching current
  behavior; measuring the generated-code cost on real schemas could overturn it.
- **Async + union membership** — `asyncDecode`/`asyncEncode` inside union
  variants inherit today's asyncParser-in-union behavior; worth a dedicated
  spec row once the wiring lands.
