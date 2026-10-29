# Codec spec

Spec for the codec implementation - how a conversion (`S.to`, or the implicit one
created by reversing a schema) picks what to decode into what.

**Implemented.** `src/union.ts` is the union factory, decoder and encoder
these rules describe; `docs/js-usage.md` and `docs/rescript-usage.md` carry the
user-facing version under "Converting to / from a union". The
`packages/sury/specs/codec-*.yaml` specs snapshot the behavior, and their
`FIXME: Codec next expects:` notes they used to carry are gone - every row
now matches.

## Shared definitions

> Two schemas have the **same type** when their type tags match - including the
> class for instances and the format for formatted primitives, where relevant.
> `S.int32` and `S.number` are different types, and so are `S.json` and
> `S.string` - even though every JSON string would validate against `S.string`.
> A schema with no tag of its own to compare - a recursive schema, or a union
> treated as a normal schema (below) - has the same type as another only when
> the two are strictly equal, i.e. the same schema reference.

- **Checks run at operation creation time**, against the **derived** types - the
  actual schema at that point in the pipeline, which may be narrower than the
  originally defined one (an upstream transformation can refine it). Reversing a
  schema doesn't re-run the checks: it reverses the already-resolved per-variant
  pipelines.
- **Nested unions are flattened** before the rules apply:
  `S.union([S.string, S.union([S.number, S.boolean])])` acts as a three-variant
  union. The exception is a union carrying its own format, transformation, or
  refinement - it's treated as a normal (non-union) schema on its side of the
  conversion, so it type-matches by strict equality: the same union reference on
  the other side matches, an identically written one does not.
- **`S.never` marks an unreachable path.** `S.never` variants - including
  transformed ones like `S.never.with(S.to, S.number)`, which match by their
  `never` input - are ignored by type matching: they never trigger the
  exceptions below and don't count toward rule 4's coverage.

## Rule 1: non-union → non-union

Built-in decoding (coercion) always applies:

```ts
S.string.with(S.to, S.schema(undefined)); // "undefined" <-> undefined
S.schema(null).with(S.to, S.schema(undefined)); // null <-> undefined
```

Which pairs have a built-in decoder is out of scope here - literal-to-literal
conversion in particular keeps whatever it does today
(`codec-literal-string-literal-number`, `codec-literal-array-literal-string`).
These rules only decide which schemas that decoder is asked about.

## Rule 2: non-union → union

The built-in decoder is applied separately for every target variant, attempted
in definition order:

```ts
const schema = S.json.with(S.to, S.union([S.bigint, S.string]));

S.parseOrThrow(schema)("123"); // 123n - the bigint variant comes first
S.parseOrThrow(schema)("abc"); // "abc" - bigint decoding fails, string accepts
S.parseOrThrow(schema)(true); // throws - no implicit double decoding (true -> "true" -> ...)
```

`S.json` is not the exact `string` type, so string inputs still go through
variant decoding instead of passing through.

**Definition order is not tag order.** The first variant that accepts wins, and
a differently-tagged variant sitting between two same-tag ones keeps its turn.

```ts
const schema = S.json.with(S.to, S.union(["123", S.bigint, S.string]));

S.parseOrThrow(schema)("123"); // "123" - the literal matches first and stays a string
S.parseOrThrow(schema)("124"); // 124n - the literal fails, the bigint variant decodes
S.parseOrThrow(schema)("abc"); // "abc" - bigint decoding fails, the catch-all string accepts
```

**Built-in decoding fills gaps, it doesn't re-type what the source already
has.** A variant whose tag the source can produce takes those values as they
are; the built-in decoder only steps in for a variant whose tag the source has
no way to produce. `bigint` is not a JSON tag, so a JSON string is offered to
`BigInt`; `string` and `number` both are, so a JSON boolean never becomes
`"true"` and a JSON string never becomes a number:

```ts
const schema = S.json.with(S.to, S.union([S.literal("a"), S.number, S.literal("b")]));

S.parseOrThrow(schema)("b"); // "b" - "a" fails, "b" matches
S.parseOrThrow(schema)("5"); // throws - S.number takes JSON numbers as they are, it doesn't decode "5"
S.parseOrThrow(schema)("c"); // throws - no variant accepts
```

**Grouping is codegen, not semantics.** Emitting same-tag variants under one
shared type check - `typeof i==="string"&&(i==="a"||i==="b")` - is an
optimization, allowed exactly while it can't change which variant wins. Hoisting
`"a"` and `"b"` past `S.number` above is legal: neither literal can take a value
`S.number` would have taken. Hoisting `"123"` and the catch-all `S.string` past
`S.bigint` is not: the catch-all takes every string, `"124"` among them. Where
the shortcut isn't available, each check stays in its own definition slot and
the repeated `typeof` is reused from a var.

`S.unknown` is a normal type here - it only matches another `unknown`. But an
`unknown` value may already *be* any of the variant types, so the
no-re-typing rule leaves no gap to fill: built-in decoding never steps in,
and the conversion is pure validation - every variant checked by its type,
in definition order, values narrowed but never re-typed. Nothing is coerced
either way, so an `unknown` source is never ambiguous and never triggers the
partial-match rejection below.

**A const source does not enable unrelated implicit coercion.** When a variant
has the identical `const`, other literal values and cross-tag gap-filling are
unreachable. Earlier members that natively accept the literal's representation
still run in definition order, however: a broad refinement or an explicit
same-input transformation may reject or transform before the literal fallback.
There is no partial-match ambiguity to reject.

```ts
S.schema(undefined).with(S.to, S.union([S.schema(null), S.schema(undefined)]));
// undefined -> undefined; the null variant is unreachable, so no nullish bridge

S.schema("x").with(S.to, S.union([S.string.with(S.minLength, 3), S.schema("x")]));
// the refinement rejects, then the literal fallback accepts "x"
```

A const facing two or more consts of another kind, none of them its own, is
rejected when the operation is created: it could become any of them. One
facing only consts of its own kind, none of them itself, is rejected too: it
could never succeed. Met as one member of a source union, at any depth, it
fails as a value instead, since the other members still convert. `null` and
`undefined` keep their bridge:

```ts
S.schema(0).with(S.to, S.union(["a", "b"])); // ❌ 0 -> "a" or 0 -> "b"?
S.schema(0).with(S.to, S.union([S.literal(0).with(S.to, S.literal("a")), S.never.with(S.to, S.literal("b"))])); // ✅ 0 <-> "a"
S.schema(0).with(S.to, S.schema("a")); // ✅ 0 -> "a", rule 1
S.schema(undefined).with(S.to, S.union(["a", "b"])); // ✅ undefined -> "a"
S.schema(0).with(S.to, S.union([1, 2])); // ❌ 0 is neither
S.union([S.schema({ t: "a" }), S.schema({ t: "c" })]).with(S.to, S.schema({ t: S.union(["a", "b"]) })); // ✅ { t: "a" }, fails { t: "c" }
S.schema(0).with(S.to, S.union([1, 2, S.number])); // ✅ 0 -> 0, the number takes it
```

The same holds for an object or tuple by the const at each field or item. A
list's items are checked against a variant's, never replaced, so an empty list
passes as it is:

```ts
S.schema({ t: "a" }).with(S.to, S.union([S.schema({ t: "b" }), S.schema({ t: "c" })])); // ❌ { t: "a" } is neither
S.array(S.literal(1)).with(S.to, S.union([S.array(S.literal(2)), S.array(S.literal(3))])); // ✅ [] -> [], fails [1]
S.schema({ x: S.array(S.literal(1)) }).with(S.to, S.union([S.schema({ x: S.array(S.literal(2)) }), S.schema({ x: S.array(S.literal(3)) })])); // ✅ { x: [] } -> { x: [] }, fails { x: [1] }
S.array(S.literal(1)).with(S.to, S.array(S.literal(2))); // ✅ [1] -> [2], rule 1
```

**Exception - partial type match.** If the source has the same type as *some but
not all* target variants, the operation is rejected when it's created. Sury
can't tell whether you want a pass-through for the matching variant, decoding
attempts in definition order, or simply widened the type with no decoding
intent:

```ts
S.string.with(S.to, S.union([S.number, S.string]));
// Invalid operation: for "123" - keep "123" or decode to 123?
```

Say what you mean with an explicit variant:

```ts
// Try decoding to number first, keep the string otherwise:
S.string.with(S.to, S.union([S.string.with(S.to, S.number), S.string]));

// Pass strings through, never producing a number:
S.string.with(S.to, S.union([S.never.with(S.to, S.number), S.string]));
```

The `Invalid operation` error suggests these rewrites.

**Exception - nullish arm.** A `null` or `undefined` target variant opposite a
single source that can't hold the value is a widened type, not a decoding: the
arm is dropped, so it neither makes the operation ambiguous nor is ever
produced, and the reverse rejects the value. A string in particular never reads
the text `"undefined"` as it. Only a carrier keeps the arm: `unknown`, `S.json`
and a `S.jsonString` document (as `null`), `S.env` (an unset var), a
recursive schema.

```ts
S.parseOrThrow(S.string.with(S.to, S.optional(S.number)))("undefined");
// throws - Expected number, received "undefined"
S.parseOrThrow(S.string.with(S.to, S.optional(S.string)))("abc"); // "abc" - the arm is unreachable
S.parseOrThrow(S.json.with(S.to, S.optional(S.string)))(null); // undefined - JSON holds it
S.parseOrThrow(S.env.with(S.to, S.optional(S.string)))(""); // undefined - `""` is an unset var
```

An object field spells a missing value by leaving the key out, so a present
`null` there is read as `S.parseOrThrow` reads it:

```ts
const read = S.parseOrThrow(S.json, S.schema({ b: S.optional(S.string) }));
read({}); // { b: undefined }
read({ b: null }); // throws - Failed at b: Expected string | undefined, received null
S.parseOrThrow(S.json, S.array(S.optional(S.string)))([null]); // [undefined]
```

The arm meets the link it is written after, not the head of the chain:

```ts
S.parseOrThrow(S.env.with(S.to, S.boolean).with(S.to, S.optional(S.boolean)))(undefined);
// throws - the arm meets S.boolean, which has no place for an unset var.
// S.env.with(S.to, S.optional(S.boolean)) reads it as undefined
```

## Rule 3: union → non-union

The mirror of rule 2: every source variant gets its own built-in decoder to the
target, dispatched in definition order:

```ts
const schema = S.union([S.bigint, S.string]).with(S.to, S.json);

S.parseOrThrow(schema)(123n); // "123"
S.parseOrThrow(schema)("123"); // "123"
S.parseOrThrow(schema)("abc"); // "abc"
```

**Exception - partial type match.** If the target has the same type as some but
not all source variants, the operation is rejected. Sury can't tell whether the
non-matching variants should decode to the target or be rejected as failed
cases:

```ts
S.union([S.number, S.string]).with(S.to, S.string);
// Invalid operation: for 123 - decode to "123" or fail as a non-matching case?
```

A const target has the same type only as the identical const, and an object,
tuple or array holding a const only as one holding it too, so a union holding
one beside others of its kind is rejected:

```ts
S.union([1, 2]).with(S.to, S.literal(1)); // ❌ 2 -> 1 or fail?
S.union([1, S.literal(2).with(S.to, S.literal(1))]).with(S.to, S.literal(1)); // ✅ 2 -> 1
S.union([S.schema({ o: S.literal(1) }), S.schema({ o: S.literal(2) })]).with(S.to, S.schema({ o: S.literal(1) })); // ❌ { o: 2 } -> { o: 1 } or fail?
S.union([S.array(S.literal(1)), S.array(S.literal(2))]).with(S.to, S.array(S.literal(1))); // ❌ the same for [2]
S.schema({ o: S.union([1, 2]) }).with(S.to, S.union([S.schema({ o: S.literal(1) }), S.schema({ o: S.literal(2) })])); // ❌ the same at o
S.union([S.schema({ o: S.literal(1) }), S.schema({ o: S.literal(2) })])
  .with(S.to, S.union([S.schema({ o: S.literal(1) }), S.schema({ o: S.literal(2) })])); // ✅ { o: 2 } -> { o: 2 }, rule 4
```

Say what you mean with an explicit target union:

```ts
// Decode numbers to strings:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.string), S.string]));

// Reject numbers:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.never), S.string]));
```

The `Invalid operation` error suggests these rewrites.

**Exception - nullish arm.** The mirror of rule 2's: a `null` or `undefined`
source variant opposite a single target that can't hold the value is dropped
rather than converted, and the value is rejected naming what is left.
`S.optional(X).with(S.to, T)` and `S.nullable(X).with(S.to, T)` read as
`X -> T` over a wider input type:

```ts
const schema = S.optional(S.string).with(S.to, S.number);

S.parseOrThrow(schema)("12"); // 12
S.parseOrThrow(schema)(undefined); // throws - Expected string, received undefined
S.optional(S.number).with(S.to, S.string); // ✅ 12 -> "12", undefined rejected, never "undefined"
S.optional(S.string).with(S.to, S.string); // ✅ string -> string, undefined rejected
S.optional(S.string).with(S.to, S.json); // ✅ undefined -> null, JSON holds it
S.optional(S.string).with(S.to, S.env); // ✅ undefined -> undefined, an unset var
S.env.with(S.to, S.string.with(S.minLength, 0)); // ✅ string -> string, undefined rejected
```

Only the arm is dropped, so a union another member leaves ambiguous is still
rejected (`codec-optional-union2-string-partial`).

## Rule 4: union → union

No coercion - values pass through to the same-type target variant. The two
unions must cover each other: every source variant needs at least one same-type
target variant, and every target variant needs at least one same-type source
variant. Otherwise the operation is rejected:

```ts
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string])); // ✅
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string, S.boolean])); // ❌ boolean has no source variant
S.union([S.string, S.number, S.boolean]).with(S.to, S.union([S.number, S.string])); // ❌ boolean has no target variant
S.union([S.string, S.number, S.bigint]).with(S.to, S.union([S.json, S.bigint])); // ❌ json is not the exact string/number type
```

A const passes through to the identical const only. One the target doesn't
spell out is checked against the target's consts of its kind, which narrows an
enum and widens one the other way:

```ts
S.union(["a", "b", "c"]).with(S.to, S.union(["a", "b"])); // ✅ "a" -> "a", fails "c"
S.union([1, 2, "x"]).with(S.to, S.union([1, "x"])); // ✅ fails 2, never 2 -> 1
S.union([S.array(S.literal(1)), S.string]).with(S.to, S.union([S.array(S.literal(2)), S.string])); // ✅ [] -> [], fails [1], never [1] -> [2]
S.union(["a", "b", S.literal("c").with(S.to, S.never)]).with(S.to, S.union(["a", "b"])); // ✅ "c" is never there
```

A target union described with `S.meta`, numbered with `S.protobufField` or
named with `S.protobufTypeName` is still a union here:

```ts
S.optional(S.string).with(S.to, S.meta(S.nullable(S.string), { name: "NS" })); // ✅ undefined <-> null
S.union(["a", "b"]).with(S.to, S.meta(S.union([0, 1]), { description: "x" })); // ❌ "a" has no same-type target variant
```

A transformed source variant matches by its output type, and a transformed
target variant by its input type - so per-variant conversion is always available
explicitly:

```ts
S.optional(S.string).with(S.to, S.nullable(S.boolean)); // ❌ string doesn't match boolean
S.optional(S.string).with(S.to, S.nullable(S.string.with(S.to, S.boolean))); // ✅
```

**Exception - nullish bridge.** A `null` or `undefined` variant left unmatched
by type may match the opposite nullish variant on the other side - even one that
already has a same-type match. At runtime the same-type target wins; the bridge
only kicks in when there is none:

```ts
S.optional(S.string).with(S.to, S.nullable(S.string)); // ✅ undefined <-> null
S.optional(S.literal("x")).with(S.to, S.nullable(S.literal("x"))); // ✅ "x" matches by type, undefined <-> null
S.optional(S.string).with(S.to, S.union([S.string, null, undefined])); // ✅ undefined -> undefined (same type wins); reverse maps null -> undefined
```

**Worked example** - `S.union([S.bigint, S.number, null]).with(S.to, S.union([S.bigint, S.number, undefined]))`:

Forward:

- `123n` → `123n` (bigint passes through)
- `123.12` → `123.12` (number passes through)
- `null` → `undefined` (nullish bridge)

Reverse (via `S.encodeOrThrow`):

- `undefined` → `null` (nullish bridge)
- `123n` → `123n`, `123.12` → `123.12` (pass through)

Union conversion always performs exhaustive validation - every variant is
checked, so transformed unions stay consistent across decode and encode.

## Failure hands the value to the next variant

Any *validation* failure of a variant - a discriminant miss, a refinement
failure, or a `S.Error` raised anywhere inside its body - passes the value to the
next variant. Only when none is left does the union throw, aggregating the
per-variant reasons under one error. This is the uniform rule for plain
validation unions and for every conversion rule above.

A variant's own code throwing - a coder, or a refinement predicate that hit a
`TypeError` on a value it was never written for - is that variant failing, and
passes the value on like any other failure. What propagates instead is an
exception nothing in the schema owns: a getter on the input, say. That one is a
bug in the value's environment, not a statement about whether the value
matches, so it surfaces rather than being tried past:

```ts
S.union([S.string.with(S.refine, (v) => { throw new TypeError("x") }), S.string]);
// the predicate's throw fails the first variant; the catch-all takes the value
```

Two consequences:

- A variant's selection condition can absorb its cond-expressible checks, since
  failing them means "try the next variant" anyway. Same-type variants with
  different refinements therefore keep their dispatch:
  `S.union([S.string.with(S.minLength, 3), S.number, S.string])` accepts `"ab"` through
  the catch-all.
- Where no later variant could accept a value that entered this one (disjoint
  types, disjoint literal discriminants - the discriminated-union shape), the
  fall-through is dead code. The variant throws its own precise error instead
  (`Failed at ["a"]: Expected string, received 42`), which keeps happy paths free
  of `try`/`catch` and sharpens the message.

## No built-in decoder for a variant

A pair with no built-in decoder is rejected when the operation is created -
`Can't decode boolean -> number. Define custom codec with S.to`. Being one
variant of a union changes nothing about that: if any variant's decoder can't be
built, the whole operation is rejected, under every rule above.

```ts
S.boolean.with(S.to, S.number); // ❌ the reference case, no union involved
S.boolean.with(S.to, S.union([S.string, S.symbol])); // ❌ boolean -> symbol has no decoder
S.union([S.boolean, S.symbol]).with(S.to, S.string); // ❌ symbol -> string has no decoder
S.boolean.with(S.to, S.union([S.number, S.symbol])); // ❌ neither variant is decodable
```

Only the first of those four is rejected today
(`codec-bool-number-unsupported`); the three union shapes each compile into
something else.

None of the three salvage attempts are available: a variant is never dropped
from the generated code, never left as a dispatch branch that throws per value,
and a conversion with no decodable variant at all never compiles into an
operation that throws for every input. The error belongs to the operation, so it
is raised once, where the operation is written.

`S.never` remains the way to say a path is deliberately unreachable - it is
ignored by variant matching, so it never triggers this rejection:

```ts
S.boolean.with(S.to, S.union([S.string, S.never.with(S.to, S.symbol)])); // ✅ the symbol path is unreachable
```

## Protobuf oneof

A union in a message is a oneof, and what decode makes of bytes with no member
set depends on how the union spells it:

```ts
S.union([email, phone, S.schema({ case: undefined })]); // no member set: { case: undefined }
S.optional(S.union([email, phone]));                    // no member set: undefined
S.union([email, phone]);                                // no member set: fails at the oneof
```

- **An arm for no member set is what none set reads as.** It holds only
  literals, or is a bare literal (`codec-protobuf-oneof-case`,
  `codec-protobuf-oneof-literal-unset`).
- **`S.optional` reads none set as `undefined`** (`codec-protobuf-oneof-case-optional`).
- **With neither, the oneof is required.** None set is `undefined` for the
  union to check, as any absent field is its default for the field's schema to
  check, so the decode fails at the oneof with the union's own error
  (`codec-protobuf-oneof-required`, `codec-protobuf-oneof-required-nested`).
  Encode writes the member the union dispatches to, whichever spelling.

## Protobuf enum

An enum is open on the wire: a number its schema doesn't list reads as that
number and is written back as it was read, a one-value enum
(`google.protobuf.NullValue`) included, and a parse still refuses it. A literal
the field converts its number through answers for it instead. With
`M = S.schema({ k: field }).with(S.to, S.protobuf)`, read with
`S.encodeOrThrow(M)` and written with `S.decodeOrThrow(M)`:

```ts
const en = { number: 1, type: "enum" } as const;
S.protobufField(S.union([0]), en);                             // reads [8, 3] as 3, writes 3 as [8, 3]
S.protobufField(S.union([0]), en).with(S.to, S.number);        // reads [8, 5] as 5, writes 5 as [8, 5]
S.protobufField(S.number.with(S.to, S.literal(0)), en);        // refuses 5 both ways
S.protobufField(S.literal(null).with(S.to, S.literal(0)), en); // reads [8, 5] as null, writes null as nothing
S.protobufField(S.union([S.literal("a").with(S.to, S.literal(0)), S.literal("b").with(S.to, S.literal(1))]), en);
// reads [8, 1] as "b", writes "b" as [8, 1], refuses [8, 5]
S.protobufField(S.union([0, 1]), en).with(S.to, S.union([S.literal(0).with(S.to, S.literal("a")), S.literal(1).with(S.to, S.literal("b"))]));
// refused: "a" faces the wire, and has no number to go as
```

One spec per line, in order: `codec-protobuf-enum-single`,
`codec-protobuf-enum-single-to-number`,
`codec-protobuf-enum-single-number-to-literal`,
`codec-protobuf-enum-null-to-zero-wire`,
`codec-protobuf-enum-per-member-chain`,
`codec-protobuf-enum-to-string-members`.

## Protobuf Any

`S.protobufAny` (`{ typeUrl, value }`) converts to a message `S.protobufTypeName`
names, to an object holding one beside literals, or to a union of those:

```ts
const Payload = S.protobufAny.with(S.to, S.union([
  S.schema({ type: "user", value: User }), // acme.v1.User
  S.schema({ type: "other", value: S.protobufAny }),
]));
```

- **Decode picks the member by name, not by rule 2.** Every member is an
  object, the same type as the source, so rule 2 would only validate; the Any
  reads the name after the type URL's last `/` instead and decodes `value` into
  the member holding that message. A member holding `S.protobufAny` takes every
  name no other member holds, the empty Any included; without one, an unlisted
  name fails (`codec-protobuf-any`, `codec-protobuf-any-closed`).
- **Encode is rule 3.** The union dispatches the value to its member, and that
  member packs: `type.googleapis.com/` and the name, and the message encoded
  (`codec-protobuf-any-standalone`).
- **Both directions refuse what wouldn't come back as it left.** Several
  members need a literal, distinct per member, to tell them apart, and a message
  is held by one member only; either is rejected when the operation is created
  (`codec-protobuf-any-untagged`, `codec-protobuf-any-same-tag`,
  `codec-protobuf-any-same-type`). A catch-all holding a type another member
  names fails the encode, since it would decode as that member.
- **Copies of one Any merge as the message it is.** A later copy's type URL
  or value replaces the one before, so a copy naming a new type over the first's
  value reads that value as the new type, the catch-all's included, as
  protobuf-es does (`codec-protobuf-any` `second-copy-*`, `third-copy-*`).
- **Anything else is the Any's own object.** `S.parseOrThrow(S.protobufAny)`
  validates `{ typeUrl, value }`, and a target holding no named message converts
  field by field (`codec-protobuf-any-json-string`).

## Custom conversions

`S.to`'s third argument replaces the built-in decoder for a pair, one direction
at a time. Every rule above still decides which schemas the conversion is asked
about; only the conversion itself changes.

Each direction takes a function, `"auto"` (keep the built-in decoder),
`"never"`, or `{async: fn}`. Async is declared, not discovered - an async slot
compiles only into an async operation, and the sync one is rejected when it's
created (`codec-custom-async-decode`, `codec-custom-async-encode`).

```ts
S.string.with(S.to, S.number, { decode: (v) => v.length, encode: (v) => "x".repeat(v) });
S.string.with(S.to, S.string, { decode: (v) => v.trim(), encode: "auto" });
```

**A coder's result is an input, not a conclusion.** It lands at the target's
*input*, so the target's whole pipeline runs over it: a coder that returns the
wrong thing fails right there, and a target that decodes further keeps decoding
(`codec-custom-pair`, `codec-custom-chained-input`). `S.any` is the target for a
value no schema describes (`codec-custom-any`).

ReScript's `~custom` is the one exception, and it is a narrower claim, not a
wider one: the ReScript compiler has already checked the coder's signature, so
the result lands at the target's *output* and only the output-side refinements
run. Two things are carved back out, because the type they rest on isn't a
proof. A literal, since a type says `string` and never the string `"a"`, so a
`const` is checked whatever the value claims to be. And `S.any` on either side
- its `t<'any>` is a variable that unifies with whatever the coder returns, so
a pair written against it goes through the junction like the JS one. Because
the trusting seam skips the target's own conversion, it rejects a target that
converts (`The target already converts`); chain `S.to` instead.

A target whose fields convert on their own is linked through what its output
claims, so those conversions don't run a second time over the coder's result,
while its refinements still check it (`codec-custom-output-seam-field-coder`,
`codec-custom-output-seam-union-field-coder`). A direction left to `"auto"`
converts through the target itself, while the coder's side still skips the
field's conversion (`codec-custom-output-seam-mixed-auto`). What the
target's output claims is all the seam checks: a conversion into `unknown`
takes whatever it is handed, whatever its encode
(`codec-custom-output-seam-unknown-never`,
`codec-custom-output-seam-unknown-decode-only`,
`codec-custom-output-seam-unknown-async-inner`,
`codec-custom-output-seam-unknown-jsonstring`,
`codec-custom-output-seam-unknown-lossy`). `S.list` claims the shape of a
ReScript list, `0` or a `{hd, tl}` cell, so an Output read back through the
seam tells it from another member's object
(`codec-custom-output-seam-list-arm-reversed`).

**A single function is a decode-only shorthand**, and the encode direction is
then rejected when the operation is created - not skipped, and not skipped
inside a union either, since yielding the variant would commit to a semantics
the caller never wrote (`codec-custom-shorthand`, `codec-custom-shorthand-union`).

**`"never"` marks one direction unreachable**, where `S.never` marks the whole
path. In that direction the variant is ignored by the rules above exactly like
`S.never`: inside a union it yields to its siblings, standalone it rejects the
operation, and a union whose every variant yields is rejected as well
(`codec-custom-never`, `codec-custom-never-union`). That single-direction reach
is what `S.optional(schema, default)` is built from - the default arm decodes
`undefined` and takes the never slot on encode.

## Spec coverage

Every `packages/sury/specs/codec-*.yaml` spec matches these rules. The rows that
changed when the implementation landed:

| Spec                                 | Rule | Behavior                                                             |
| ------------------------------------ | ---- | -------------------------------------------------------------------- |
| `codec-union-nested-refined-union`    | -    | a union with its own refinement isn't flattened, and it still refines |
| `codec-bool-union2-unsupported`       | -    | rejected - no `boolean -> symbol` decoder                            |
| `codec-bool-union2-all-unsupported`   | -    | rejected - no decoder for either member                              |
| `codec-union2-string-unsupported`     | -    | rejected - no `symbol -> string` decoder                             |
| `codec-json-union2`                   | 2    | non-bigint string falls back to the `S.string` member                |
| `codec-json-union3-ungrouped`         | 2    | `"123"` matches the literal, `"124"` reaches the `S.bigint` member    |
| `codec-number-union2-int32`           | 2    | compiles: int32 first, string next                                   |
| `codec-string-optional`              | 2    | nullish arm - `undefined` is never produced, and rejected on encode  |
| `codec-string-optional-number`       | 2    | nullish arm - `"undefined"` text is rejected, `"12"` decodes to `12` |
| `codec-optional-string-string`       | 3    | nullish arm - `undefined` rejected as `Expected string`              |
| `codec-optional-number-string`       | 3    | nullish arm - `undefined` is not written as text                     |
| `codec-optional-string-number`       | 3    | nullish arm - `X -> T` decodes, `undefined` rejected                 |
| `codec-optional-string-uint8array`   | 3    | nullish arm - the text packs, `undefined` rejected                   |
| `codec-file-optional-email`          | 2    | nullish arm - a file reads into `email`, not into a choice           |
| `codec-nullable-string-string`, `codec-nullish-string-string` | 3 | nullish arm - `null` the same way                        |
| `codec-object-optional-string-string`| 3    | nullish arm - an optional field required on output, `Failed at a`   |
| `codec-optional-union2-string-partial` | 3  | nullish arm dropped, still rejected - `number` is the partial match  |
| `codec-string-union2-partial`         | 2    | rejected - partial type match                                        |
| `codec-union2-string-partial`         | 3    | rejected - partial type match                                        |
| `codec-optional-nullable-partial`     | 4    | rejected - `string` has no same-type target member                   |
| `codec-union2-union3-extra-target`    | 4    | rejected - `boolean` has no source member                            |
| `codec-union3-union2-extra-source`    | 4    | rejected - `boolean` has no target member                            |
| `codec-union3-union2-json`            | 4    | rejected - `json` is not the exact `string`/`number` type             |
| `codec-union-refined-fallback`         | -    | new: a refined member falls through to a same-type catch-all          |
| `union2-refine-throws`                 | -    | new: a throwing refine fails its variant, so the catch-all matches    |
| `optional-object`                      | -    | new: an array is not an object, in the `X \| undefined` dispatch too   |
| `union2-object-number`                 | -    | new: the same, for an object member sharing a union with another tag  |
| `object1`, `object5-optional`          | -    | an array is not an object at the top level either, in every mode      |

The already-conformant rows kept their behavior; several lost dead code the old
implementation emitted (a `try/catch` around a body that can't throw, an `else if`
re-testing its own `else`) and now report the precise per-member error where the
fall-through is provably dead.
