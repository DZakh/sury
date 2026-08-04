# Custom codec spec

**Proposal — not implemented.** Spec for the breaking change that removes
`S.transform` from the ReScript surface (and `$res_transform` from the entry) in
favor of `S.to` with an explicit decode/encode pair, and adds async conversions
to the JS surface. `CODEC_SPEC.md` stays the normative statement of *built-in*
conversions; this file governs the *custom* ones layered on the same `S.to`.

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
   compiled.
3. **No JS async conversion at all** — `asyncParser` exists only behind
   `$res_transform`; the JS surface stops at `S.asyncDecoderAssert`, which can
   check but not convert.

Prior art (as of 2026): **Zod 4** `z.codec(A, B, {decode, encode})` and **Effect**
`Schema.transform(from, to, {strict, decode, encode})` both force the pair as
required object keys. Both put the custom functions at the `A.Output ↔ B.Input`
seam (the target still runs after decode). Zod discovers asyncness at call time
(`decodeAsync`, runtime error from sync `decode`); Effect erases the question by
returning `Effect` from both coders. Zod's one-way `.transform()` poisons
`encode` with a runtime throw; Effect opts out per direction by failing with a
`Forbidden` issue. Valibot and ArkType have no encode direction. Sury compiles
operations ahead of time, so it cannot copy Zod's call-time discovery — it needs
to know sync/async *before* the coder runs.

## The seam: output ↔ output

The custom pair sits between **`s1`'s output and `s2`'s output**:

```
decode : s1.Output -> s2.Output
encode : s2.Output -> s1.Output
```

not at the `s1.Output ↔ s2.Input` junction Zod/Effect (and today's `js_to`
wiring) use. Three reasons:

- **It's the only ReScript-typeable seam.** `S.t<'value>` carries the output
  type alone, so `~decode: 'from => 'to, ~encode: 'to => 'from` is exactly
  expressible; a function into `s2.Input` could only be typed `'from => unknown`.
- **The junction seam is still one chain link away.** Zod/Effect need the target
  to keep running after decode because their transform is the only conversion
  primitive. In Sury a chain is free — every `S.to` folds into the same compiled
  function — so "decode to the target's input" is spelled explicitly:

  ```rescript
  // decode lands on userSchema's *input*; userSchema still validates/maps
  S.string
  ->S.to(S.json, ~decode=parseCustomFormat, ~encode=printCustomFormat)
  ->S.to(userSchema)
  ```

- **It makes the two directions actually symmetric**, fixing the d.ts today:

  ```ts
  decode?: (value: TOutput) => TTargetOutput;
  encode?: (value: TTargetOutput) => TOutput;
  ```

There is deliberately **no mode option** for choosing the seam. One semantics,
composition for the rest.

### What the target contributes

With custom coders, `s2`'s own forward pipeline is bypassed — decode's word is
final. `s2` still earns its argument slot:

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

## Rule 1: the pair is all-or-nothing

A custom decode without a custom encode (or vice versa) is rejected **when the
schema is created**. Sury can't tell whether the missing direction should fail
or fall back to built-in coercion, so it refuses to guess — the same
no-silent-salvage stance as `CODEC_SPEC.md`'s "No built-in decoder for a
variant".

- **TS**: enforced statically — the `S.to` d.ts overloads become
  `(schema, target)` and `(schema, target, decode, encode)`; the 3-arg form is
  removed (breaking).
- **ReScript**: `~decode` / `~encode` are optional labeled args on the one
  `S.to` external, so the pair rule is a creation-time panic rather than a type
  error. The panic message teaches both escape hatches below.

Wanting a *built-in* conversion for one direction is spelled with a chain, not a
half pair — put the custom pair between schemas where it is total, and let plain
`S.to` links do the built-in legs.

## Rule 2: `S.unsupported` is the explicit one-way opt-out

```rescript
let hashed = S.string->S.to(S.string, ~decode=hash, ~encode=S.unsupported)
```

`S.unsupported` is a recognized function reference (`'a => 'b`; also exported to
JS). Passing it installs an `invalid_operation` builder for that direction, so
compiling any operation that needs it — `S.encoder`, reversed parse — fails **at
operation creation**, once, where the operation is written; never per value.
This preserves `S.transform`'s best property (a missing serializer failed at
compile, not per value) while making the intent explicit, and it is the
"explicitly opted out" that the reversibility rule in `CLAUDE.md` requires.
Zod's equivalent is a runtime `ZodEncodeError` on first encode; Effect's is a
per-value `Forbidden` issue — creation-time is earlier than both.

## Rule 3: sync and async split by API name

Codegen must know a coder's asyncness before calling it (`await` insertion,
`isAsync`, `parser` vs `asyncParser` selection), and detecting it from the
function value is unreliable. So it's static, in the API name — the same
convention that already splits `S.parser`/`S.asyncParser`:

```rescript
@module("sury")
external to: (
  t<'from>, t<'to>,
  ~decode: 'from => 'to=?, ~encode: ('to => 'from)=?,
) => t<'to> = "to"

@module("sury")
external toAsync: (
  t<'from>, t<'to>,
  ~decode: 'from => promise<'to>, ~encode: 'to => 'from,
) => t<'to> = "toAsync"
```

```ts
S.string.with(S.to, S.int32, (s) => parseIntOrThrow(s), (i) => `${i}`);
S.uuid.with(S.toAsync, userSchema, (id) => loadUser(id), (user) => user.id);
```

- `S.toAsync` is **new on the JS surface too** — it closes the gap that async
  conversion currently exists only behind `$res_transform`.
- On `toAsync` both args are non-optional in both languages: there is no
  built-in async fallback, so the plain two-arg spelling has no async flavor.
- `toAsync`'s decode compiles through the existing `asyncParser` path
  (`B_embedTransformation(_, _, true)`) and inherits its rules — sync operations
  on the schema fail with the existing "use parseAsyncOrThrow" invalid
  operation, `S.isAsync` reports true.
- **encode stays sync.** The serializer pipeline has no async support today;
  an async encode is a future extension (an `asyncEncoder` operation plus a
  `toAsync` variant), not smuggled in here.

Naming note: library precedent is prefix-style (`asyncParser`, `asyncDecoder`),
but `S.uuid->S.asyncTo(...)` doesn't read; `toAsync` follows Zod's
`decodeAsync` suffix. Either way, one name = one asyncness.

## Rule 4: failure semantics are unchanged

A coder fails by throwing. A thrown `S.Error` is adopted as-is; anything else
surfaces as `invalid_conversion` with the original as `cause` and the reached
path prepended — exactly the current `S.transform`/custom-`to` behavior, and the
union fall-through / foreign-exception rules of `CODEC_SPEC.md` apply untouched.

## Reversal

`S.reverse` of `s1->S.to(s2, ~decode, ~encode)` is a schema from `'to` to
`s1.Input`: validate against `s2`'s output side, run `encode`, continue through
`s1` reversed. Reversing twice restores the original. `decode`/`encode` swap
roles under reversal the way `parser`/`serializer` do today.

## Surface changes (breaking)

| Surface  | Removed                                                     | Changed                                                                                     | Added                                     |
| -------- | ----------------------------------------------------------- | ------------------------------------------------------------------------------------------- | ----------------------------------------- |
| ReScript | `S.transform`, `S.transformDefinition`                      | `S.to` gains `~decode`/`~encode` (pair enforced at creation)                                 | `S.toAsync`, `S.any`, `S.unsupported`     |
| JS       | 3-arg `S.to` overload (one-sided coders)                    | `S.to` coder types move to the output↔output seam; one-sided call panics at creation         | `S.toAsync`, `S.unsupported` (`S.any` exists) |
| entry.ts | `transform as $res_transform` (~4.5k in `bundleSize.yaml`)  | —                                                                                            | `toAsync`, `unsupported` (pure, annotated) |

The internal `transform` machinery in `modifiers.ts` can shrink to whatever
`js_asyncDecoderAssert` still needs, or be re-expressed through the `toAsync`
wiring and deleted.

## Migration

| Before                                                          | After                                                            |
| --------------------------------------------------------------- | ---------------------------------------------------------------- |
| `s->S.transform(() => {parser, serializer})`                     | `s->S.to(target, ~decode=parser, ~encode=serializer)`             |
| `s->S.transform(() => {parser})`                                 | `…, ~encode=S.unsupported)` — the fail-on-encode is now explicit  |
| `s->S.transform(() => {asyncParser, serializer})`                | `s->S.toAsync(target, ~decode=asyncParser, ~encode=serializer)`   |
| `s->S.transform(() => {asyncParser})`                            | `…toAsync…, ~encode=S.unsupported)`                               |
| no natural target schema                                         | `target = S.any` (byte-for-byte the old transform behavior)       |
| JS `S.to(s1, s2, decode)` (one-sided)                            | add `encode` or `S.unsupported`                                   |

The docs' async example becomes:

```rescript
let userSchema = S.uuid->S.toAsync(S.any, ~decode=userId => loadUser(~userId), ~encode=user => user.id)
```

and the custom-schema example gains a real reverse:

```rescript
let mySet = itemSchema =>
  S.instance(%raw(`Set`))
  ->S.to(S.any, ~decode=decodeSetItems(itemSchema), ~encode=encodeSetItems(itemSchema))
  ->S.meta({name: `Set.t<${S.inputExpression(itemSchema)}>`})
```

## Spec plan

Every row lands as `packages/sury/specs/*.yaml` through the spec skill — codegen
snapshots plus `examples` for the behaviors above. At minimum:

- `codec-custom-pair` — sync pair on a typed target: generated parse/encode
  code, target refinement running after decode, reverse round-trip.
- `codec-custom-async` — `toAsync` codegen, `isAsync`, sync-op rejection.
- `codec-custom-one-sided` — creation-time rejection message (JS 3-arg and
  ReScript single-label spellings).
- `codec-custom-unsupported` — `S.unsupported` failing at encoder creation, not
  per value.
- `codec-custom-chained-input` — the chain-through-input idiom compiling to the
  same code the junction seam would have produced.
- `codec-custom-any` — `S.any` target reproducing today's `transform` golden
  (diff against the existing transform-based specs before deleting them).
- `bundleSize.yaml` — `$res_transform` row replaced by `to`/`toAsync`; the
  ReScript entry should get smaller (no lazy definition record, no
  `transformDefinition` type).

Test migration follows the table above (`S_transform_*` tests become `S_to_*` /
`S_toAsync_*`); `docs/rescript-usage.md`'s Transforms chapter and the
`S.shape` note pointing at `S.transform` are rewritten around `S.to`.

## Open questions

- **Async encode** — blocked on serializer-pipeline async support; the API above
  leaves room (`asyncEncoder` operation + a future variant) without reserving
  syntax now.
- **Should decode's result skip the target's refinements too?** This spec says
  they run (cheap, symmetric with reversed-parse validation), matching current
  behavior; measuring the generated-code cost on real schemas could overturn it.
- **`toAsync` vs `asyncTo`** — see Rule 3's naming note; pick once, before the
  docs are written.
