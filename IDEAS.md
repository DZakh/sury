# Ideas draft

## Alpha.5

Alpha.5 is the biggest release on the road to v11. It introduces a redesigned operation API, a brand-new union match engine, a richer set of built-in schemas, sharper and customizable error messages, and a wave of API polish across both the TS/JS and ReScript surfaces.

### 🧠 The mental model shift: schemas all the way down

The biggest idea in Alpha.5 isn't a single new function — it's a **change in how you think about operations**.

In every other validation library — and in earlier Sury — moving between data shapes meant reaching for a different function: `parseOrThrow`, `parseJsonOrThrow`, `parseJsonStringOrThrow`, `convertToJsonOrThrow`, `convertToJsonStringOrThrow`, `reverseConvertToJsonOrThrow`… one operation per supported target.

**Sury Alpha.5 collapses all of that into two ideas:**

1. **`S.json`, `S.jsonString`, `S.unknown`, `S.date`, `S.uint8Array`, … are just schemas.** There is nothing special about JSON. If you can describe it as a schema, it can be a stage in a pipeline.
2. **`S.parser`, `S.decoder` and `S.encoder` accept any sequence of schemas** and compile them into a single, ultra-optimized function via `new Function`. No per-target operations, no special casing — you build the exact pipeline you want.

```ts
// Old mental model: pick the right function for the job.
S.parseOrThrow(data, schema);
S.parseJsonStringOrThrow(rawString, schema);
S.reverseConvertToJsonOrThrow(value, schema);

// New mental model: name the stages, Sury compiles the path.
S.parser(schema)(data);
S.decoder(S.jsonString, schema)(rawString);   // string → JSON.parse → validate → output
S.encoder(schema, S.json)(value);              // output → input → JSON-shaped value
S.encoder(schema, S.jsonString)(value);        // …or all the way to a JSON string
S.decoder(S.jsonString, schema, S.json)(raw);  // parse, then re-emit as plain JSON
```

The same idea scales beyond JSON:

```ts
// Decode a Uint8Array of UTF-8 bytes, parse the JSON payload, validate, done.
S.decoder(S.uint8Array, S.jsonString, schema);

// Take a domain object, encode it to a JSON string, then to a Uint8Array of UTF-8 bytes.
S.encoder(schema, S.jsonString, S.uint8Array);
```

Each pipeline is fused into a single generated function with no intermediate allocations, so chaining stages doesn't cost you performance — in many cases it's **faster** than the equivalent hand-written code, because Sury can inline the whole path. You go from a fixed menu of operations to an open set: any schema can be an input, any schema can be an output, and the library handles the plumbing.

**The same pipeline idea works inside schemas too — via `S.to`.** A field, an array element, a tuple slot, or any nested schema can be its own multi-stage chain:

```ts
const apiUser = S.schema({
  // Field arrives as a string, parsed as JSON, validated as the addresses array.
  addresses: S.string.with(S.to, S.jsonString).with(S.to, S.array(addressSchema)),

  // Field arrives as bytes, decoded as UTF-8, validated as an ISO datetime, mapped to Date.
  createdAt: S.uint8Array.with(S.to, S.string).with(S.to, S.isoDateTime).with(S.to, S.date),

  // Element-level transforms work the same way.
  ids: S.array(S.string.with(S.to, S.number)),
});
```

`S.to` is the same compiler as `S.decoder` / `S.encoder`, just used at a single point in a larger schema. The whole tree — top-level operation plus every nested `S.to` — still folds into one generated function, so deep pipelines stay free of runtime overhead.

`S.parser` and `S.assert` aren't even separate primitives — they're just **specializations of `S.decoder`** with `S.unknown` on the input side:

- `S.parser(schema)` ≡ `S.decoder(S.unknown, schema)` — accept anything, validate against the schema, produce its output type.
- `S.assert(schema, data)` ≡ a `S.decoder` from `S.unknown` *through* the schema *to* `S.literal(true).with(S.noValidation, true)` — the target is a no-op constant with validation disabled, so the compiler runs the schema's validation but emits no output-construction code at all. That's why `assert` is 2–3× faster than `parser`.

See the [migration cheat sheet](#typescript--javascript) below for the full mapping of the old per-target operations to the new pipeline form.

### 🆕 New built-in schemas

A bigger toolbox for everyday data:

- **`S.date`** — standalone `Date` instance schema. Validates `instanceof Date` and rejects Invalid Date. Unlike `S.isoDateTime` (which validates ISO 8601 UTC strings) and `S.string->S.to(S.date)` (which decodes ISO strings into Date objects), `S.date` validates existing Date instances directly.
- **`S.isoDateTime`** — standalone string schema for ISO 8601 UTC datetime strings: no timezone offsets, arbitrary sub-second precision. Replaces the removed `S.datetime` for the "validate an ISO string" use case. For string ↔ Date conversion use `S.string->S.to(S.date)`.
- **`S.uint8Array`** — first-class `Uint8Array` schema. Combine with `S.to(S.string)` for binary ↔ text encoding/decoding.
- **`S.compactColumns`** — transforms columnar data (`[[a1, a2], [b1, b2]]`) to and from row objects (`[{foo: a1, bar: b1}, {foo: a2, bar: b2}]`). Typed as `Schema<Output[][], Input[][]>`, and `S.toExpression` renders it as the concrete element type (e.g. `"string[][]"`) without needing an explicit `S.to`.
- **Standalone format schemas** — `S.port`, `S.email`, `S.uuid`, `S.cuid`, `S.url` are now standalone schemas instead of refinement helpers. `Email`, `Uuid`, `Cuid`, `Url` are exposed on the `StringFormat` type for type-level reuse.

### 🪄 Smarter unions: three-tier matching

When compiling `source -> targetUnion` (via `S.to` or reversal), Sury now uses a three-tier match based on the source's **derived tag** — the tag at compile time, which may be narrower than the original (upstream transformations can refine it).

> 🚧 **Work in progress.** When the source is itself a union, the algorithm runs independently for each source variant. This per-variant fan-out is still being polished and may change before the stable release.

If the source is `unknown` (no derived tag), the tag-based tiers are skipped and target variants are attempted directly in target-union order at runtime.

1. **Same-tag group.** Collect target variants sharing the source's tag. If non-empty, match only within this group: variants with a matching `const`/`format` (string literals, `Int32`, etc.) are tried first in target-union order, then remaining catch-all same-tag variants. Variants with a different tag are never tried — if every branch in the group fails, the match errors.
2. **Nullish bridge.** Only when tier 1 is empty. If the source tag is `null` or `undefined`, use the opposite nullish target variant (if present), exclusively.
3. **Fallback.** Only when tiers 1 and 2 are both empty. Build a decoder for every target variant in target-union order. Cross-type coercions live here: `number`/`bigint` → `string` via `"" + i`, `string` → `number` via `+i`, `string` → `bigint` via `BigInt(i)`, stringified-const matches like `"null" → null`, and more.

**Worked example** — `S.union([S.bigint, S.float, S.nullLiteral])->S.to(S.union([S.string, S.unit]))`:

Forward:

- `123n` → `"123"` (tier 3: bigint → string)
- `123.12` → `"123.12"` (tier 3: float → string)
- `null` → `undefined` (tier 2: nullish bridge)

Reverse:

- `"null"` → `null` (tier 3: stringified-const literal match)
- `undefined` → `null` (tier 2: nullish bridge)
- `"123"` → `123n` (tier 3: bigint attempted first by target order; parse succeeds)
- `"123.12"` → `123.12` (tier 3: bigint parse throws, falls through to float)
- `"abc"` → error (tier 3: no variant's decoder succeeds)

**Identity beats coercion.** For `S.union([S.string, S.bigint])->S.to(S.union([S.float, S.string]))`:

- `"123"` → `"123"` (tier 1: `string` matches `string`, never coerced to `float` even though a `float` target exists)
- `123n` → `"123"` (tier 3: no `bigint` target, falls through to `string` via `"" + i`)

To opt into `string → float` when a `string` target also exists, write the transform into a variant explicitly: `S.union([S.string->S.to(S.float), S.string])` as the target. The transform variant is const/format-refined relative to the catch-all `string` and matches first within tier 1.

Beyond matching, union conversion now always performs **exhaustive validation** — every variant is checked instead of bailing on the first match — so transformed unions stay consistent across encode and decode.

### ⚡ Cleaner refinements

`S.refine` is now **boolean-returning** instead of callback-based — return `true` for valid, `false` for invalid:

```ts
// TS/JS
const positive = S.number.with(S.refine, (n) => n > 0);

const passwordsMatch = S.schema({
  password: S.string,
  confirm: S.string,
}).with(S.refine, (data) => data.password === data.confirm, {
  error: "Passwords don't match",
  path: ["confirm"],
});
```

```rescript
// ReScript
let positive = S.float->S.refine(n => n > 0.)

let passwordsMatch =
  S.schema(s =>
    {
      "password": s.field("password", S.string),
      "confirm": s.field("confirm", S.string),
    }
  )->S.refine(
    data => data["password"] == data["confirm"],
    ~error="Passwords don't match",
    ~path=["confirm"],
  )
```

- Renamed `S.asyncParserRefine` → **`S.asyncDecoderAssert`** (TS/JS). The `EffectCtx` (`s`) parameter is gone — throw directly to signal failure instead of calling `s.fail()`.
- ReScript: `schema` is no longer passed to the `S.transform` and `S.refine` context — they receive only the value.

### 🧹 Sharper, friendlier errors

- `InvalidType` errors now include the received schema, so you can pattern-match on the concrete type that came in.
- Dropped the noisy `Failed parsing/converting/asserting` prefix when the error is at the root.
- Renamed `Failed parsing/converting/asserting at path` → **`Failed at path`** — short, neutral, and applies uniformly to every operation.
- Every error thrown from `transform`/`refine` is wrapped in `SuryError`, so you never lose stack context.
- Renamed error code `unsupported_conversion` → **`unsupported_decode`** (variant `UnsupportedConversion` → `UnsupportedDecode`). The new message reads: `Can't decode X to Y. Use S.to to define a custom decoder` — a direct hint at the fix.
- TS: `S.Error` is now a discriminated variant instead of a `code` string property — full type-safe pattern matching.
- ReScript: `S.ErrorClass.constructor` → **`S.Error.make`** (accepts full error details, no more `flag` parameter); `S.ErrorClass.t` and `S.ErrorClass.value` → **`S.Error.class`**; new **`S.Error.classify`** turns an error into a variant of every possible error code.

### 🎯 Customizable error messages

Override validation messages per-use without forking a schema:

```ts
S.email.with(S.meta, { errorMessage: { format: "Must be a valid email" } });

// `_` is a catch-all for any constraint
S.email.with(S.meta, { errorMessage: { _: "Invalid input" } });

// Empty {} resets all overrides
schema.with(S.meta, { errorMessage: {} });
```

- Added `errorMessage` (singular) on schema types — renamed from the plural `errorMessages`.
- New typed **`SchemaErrorMessage`** with fields for every constraint key: `format`, `type`, `minimum`, `maximum`, `minLength`, `maxLength`, `minItems`, `maxItems`, `pattern`, `_`.
- The `S.String.Refinement` module and `S.String.refinements` accessor are removed — read `pattern`/`format`/`errorMessage` from the schema directly.

### 📦 JSON pipeline improvements

- **Strips `undefined` on encode.** Encoding to JSON now drops `undefined` fields by default — no more accidentally serializing keys with `undefined` values.
- **Automatic string encoders.** The JSON decoder no longer special-cases each non-JSON type. Any schema with a string encoder (`S.date` → `toISOString()`, `S.bigint`, etc.) now plays nicely with `S.json` and `S.jsonString` out of the box.

### 🛠️ Internal refactors (worth knowing about)

- Object schemas no longer carry an `items` field, and tuple schemas store `items` as an array of schemas instead of an array of items. The dedicated `item` type is removed.
- ReScript: `S.null` is renamed to **`S.nullAsOption`** to reflect what it actually does (decode `null` into `option`).

### 🚚 Migration cheat sheet

#### TypeScript / JavaScript

A consistent family of functions replaces the OrThrow zoo:

| Before | After |
|---|---|
| `S.parseOrThrow(data, schema)` | `S.parser(schema)(data)` |
| `S.parseAsyncOrThrow(data, schema)` | `S.asyncParser(schema)(data)` |
| `S.parseJsonOrThrow(data, schema)` | `S.decoder(S.json, schema)(data)` |
| `S.parseJsonStringOrThrow(data, schema)` | `S.decoder(S.jsonString, schema)(data)` |
| `S.convertOrThrow(data, schema)` | `S.decoder(schema)(data)` |
| `S.convertToJsonOrThrow(data, schema)` | `S.decoder(schema, S.json)(data)` |
| `S.convertToJsonStringOrThrow(data, schema)` | `S.decoder(schema, S.jsonString)(data)` |
| `S.reverseConvertOrThrow(data, schema)` | `S.encoder(schema)(data)` |
| `S.reverseConvertToJsonOrThrow(data, schema)` | `S.encoder(schema, S.json)(data)` |
| `S.reverseConvertToJsonStringOrThrow(data, schema)` | `S.encoder(schema, S.jsonString)(data)` |
| `S.assertOrThrow(data, schema)` | `S.assert(schema, data)` |
| `S.compile(...)` | `S.parser` / `S.decoder` / `S.encoder` |
| `S.transform` | `S.to` |

#### ReScript

The labeled-argument operation API is now the single entry point:

| Before | After |
|---|---|
| `S.parseOrThrow(data, schema)` | `S.parseOrThrow(data, ~to=schema)` |
| `S.parseAsyncOrThrow(data, schema)` | `S.parseAsyncOrThrow(data, ~to=schema)` |
| `S.parseJsonOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=S.json, ~to=schema)` |
| `S.parseJsonStringOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=S.jsonString, ~to=schema)` |
| `S.assertOrThrow(data, schema)` | `S.assertOrThrow(data, ~to=schema)` |
| — | `S.assertAsyncOrThrow(data, ~to=schema)` *(new)* |
| `S.reverseConvertOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.unknown)` |
| `S.reverseConvertToJsonOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.json)` |
| `S.reverseConvertToJsonStringOrThrow(data, schema)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.jsonString)` |
| `S.reverseConvertToJsonStringOrThrow(data, schema, ~space=2)` | `S.decodeOrThrow(data, ~from=schema, ~to=S.jsonStringWithSpace(2))` |
| `S.convertOrThrow(data, schema)` | `S.decoder1(schema)(data)` |
| `S.compile(schema, ~input=Any, ~output=Value, ~mode=Sync, ~typeValidation=true)` | `S.parser(~to=schema)` |
| `S.compile(schema, ~input=Any, ~output=Value, ~mode=Async, ~typeValidation=true)` | `S.asyncParser(~to=schema)` |
| `S.compile(schema, ~input=Value, ~output=Unknown, ~mode=Sync, ~typeValidation=false)` | `S.decoder(~from=schema, ~to=S.unknown)` |
| `S.compile(schema, ~input=Value, ~output=Unknown, ~mode=Async, ~typeValidation=false)` | `S.asyncDecoder(~from=schema, ~to=S.unknown)` |

## v11

### ideas

- Add `promise` type and `S.promise` (instead of async flag internally)

TODO:

Test null<> in ppx

```
// Test that refinement works correctly with reverse

S.reverse(S.schema({
  foo: S.string->S.to(S.number)
})->S.refine(value => value.foo > 0))
```

### TS operation functions

- rename `serializer` to reverse parser ?
- Make `foo->S.to(S.unknown)` stricter ??

- Add `S.to(from, target, parser, serializer)` instead of `S.transform`?
- Make built-in refinements not work with `unknown`. Use `S.to` (manually & automatically) to deside the type first
- Better inline empty recursive schema operations (union convert)
- Don't iterate over JSON value when it's `S.json` convert without parsing
- Add `S.date.with(S.migrationFrom, S.string, <optionalParser>)`.
- Allow to pass {} instead of S.schema({}) to S.array and other schemas

### Final release fixes

- Add `S.env` to support coercion for union items separately. Like `rescript-envsafe` used to do with `preprocess`
- Make `S.record` accept two args
- Update docs

### Known bugs left over from the validation refactor (`val.validation: array<validationCheck>`)

- **Union discriminant hoists refinement checks with `&&` instead of `;`.**
  Now that refinements are structured checks, the union item merge loop
  hoists all checks on a val via `andJoinChecks`, fusing type checks and
  refinement checks into one `&&`-joined condition with a single error throw.
  This causes two problems: (1) `typeof==="string"&&length===N` shares one
  error instead of separate type/refinement errors, and (2) same-type items
  with different refinements (e.g. `S.union([S.string->S.email, S.string->S.url])`)
  lose per-item error messages. Fix: split hoisted checks by `fail` reference —
  first group (type checks) → discriminant condition, remaining groups
  (refinement checks) → body code as `cond||fail;`. For same-type items with
  different refinements, use if/else if dispatch on the refinement cond instead
  of try/catch. Failing regression tests in `S_union_test.res`.
- **`noValidation` on a literal inside a union silently breaks dispatch.**
  `literalDecoder` short-circuits when `expectedSchema.noValidation` is set
  and emits no check at all, so there's nothing for the union discriminant
  hoister to lift — that case becomes a catch-all. Fix: either emit the
  equality check regardless of `noValidation` when the val ends up inside a
  union, or reject `S.noValidation` on a literal-in-union at schema
  construction time. Failing regression test: `S_noValidation_test.res ›
  Union dispatch still works when a case has noValidation`.
- **`err.received` is wrong for refine-chain vals on type failures.** Because
  `B.refine` sets `~schema=prev.expected`, `val.schema` on a refined val
  equals the target schema, and `failInvalidType` reads `val.schema` for
  `received`. So `err.received === err.expected` on a primitive type failure.
  User-visible reason text is unaffected (it uses `input->stringify`) but
  programmatic consumers reading `err.received` get the target schema instead
  of the source type. Fix: either have the fail function reach through
  `val.prev.schema` (with a comment on the invariant that validation-owning
  vals always have a prev) or stop mutating `val.schema` to the target in
  `refine` and walk the chain differently for "Expected X" messages.
  FIXME is tagged at `Sury.res:failInvalidType`.

## v11 initial

- Add `s.parseChild` to EffectContext ???
- Support arrays for `S.to`
- Remove fieldOr in favor of optionOr?
- Allow to pass custom error message via `.with`
- Make S.to extensible
- ~~Add S.Date (S.instanceof) and remove S.datetime~~ (S.date added; S.datetime kept for backward compat)
- Add refinement info to the tagged type

## v???

- `S.promise: S.t<'value> => S.t<promise<'value>>` and `S.await: S.t<promise<'value>> => S.t<'value>`
- Remove `S.deepStrict` and `S.deepStrip` in favor of `S.deep` (if it works)
- Make S.serializeToJsonString super fast
- Somehow determine whether transformed or not (including shape)
- Add JSDoc
- s.optional for object
- S.transform(s => {
  s.reverse(input => input) // Or s.asyncReverse(input => Promise.resolve(input))
  input => input
  }) // or asyncTransform // Maybe format ?
- Clean up Caml_option.some, Js_dict.get
- Github Action: Add linter checking that the generated files are up to date (?)
- Support optional fields (can have problems with serializing) (???)
- S.mutateWith/S.produceWith (aka immer) (???)
- Add S.function (?) (An alternative for external ???)

```

let trimContract: S.contract<string => string> = S.contract(s => {
s.fn(s.arg(0, S.string))
}, ~return=S.string)

```

- Use internal transform for trim
- Add schema input to the error ??? What about build errors?
- async serializing support
- Add S.promise
- S.create / S.validate
- Add S.codegen
- Rename S.inline to S.toRescriptCode + Codegen type + Codegen schema using type
- Make `error.reason` tree-shakeable
- S.toJSON/S.castToJson ???
- S.produce
- S.mutator
- Check only number of fields for strict object schema when fields are not optional (bad idea since it's not possible to create a good error message, so we still need to have the loop)

```

```
