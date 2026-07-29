# Codec spec

Spec for the codec implementation — how a conversion (`S.to`, or the implicit one
created by reversing a schema) picks what to decode into what.

**Implemented.** `src/union.ts` is the union factory, decoder and encoder
these rules describe; `docs/js-usage.md` and `docs/rescript-usage.md` carry the
user-facing version under "Decoding into / out of a union". The
`packages/sury/specs/codec-*.yaml` specs snapshot the behavior, and their
`FIXME: Codec next expects:` notes they used to carry are gone — every row
now matches.

## Shared definitions

> Two schemas have the **same type** when their type tags match — including the
> class for instances and the format for formatted primitives, where relevant.
> `S.int32` and `S.number` are different types, and so are `S.json` and
> `S.string` — even though every JSON string would validate against `S.string`.
> A schema with no tag of its own to compare — a recursive schema, or a union
> treated as a normal schema (below) — has the same type as another only when
> the two are strictly equal, i.e. the same schema reference.

- **Checks run at operation creation time**, against the **derived** types — the
  actual schema at that point in the pipeline, which may be narrower than the
  originally defined one (an upstream transformation can refine it). Reversing a
  schema doesn't re-run the checks: it reverses the already-resolved per-variant
  pipelines.
- **Nested unions are flattened** before the rules apply:
  `S.union([S.string, S.union([S.number, S.boolean])])` acts as a three-variant
  union. The exception is a union carrying its own format, transformation, or
  refinement — it's treated as a normal (non-union) schema on its side of the
  conversion, so it type-matches by strict equality: the same union reference on
  the other side matches, an identically written one does not.
- **`S.never` marks an unreachable path.** `S.never` variants — including
  transformed ones like `S.never.with(S.to, S.number)`, which match by their
  `never` input — are ignored by type matching: they never trigger the
  exceptions below and don't count toward rule 4's coverage.

## Rule 1: non-union → non-union

Built-in decoding (coercion) always applies:

```ts
S.string.with(S.to, S.schema(undefined)); // "undefined" <-> undefined
S.schema(null).with(S.to, S.schema(undefined)); // null <-> undefined
```

Which pairs have a built-in decoder is out of scope here — literal-to-literal
conversion in particular keeps whatever it does today
(`codec-literal-string-literal-number`, `codec-literal-array-literal-string`).
These rules only decide which schemas that decoder is asked about.

## Rule 2: non-union → union

The built-in decoder is applied separately for every target variant, attempted
in definition order:

```ts
const schema = S.json.with(S.to, S.union([S.bigint, S.string]));

S.parser(schema)("123"); // 123n — the bigint variant comes first
S.parser(schema)("abc"); // "abc" — bigint decoding fails, string accepts
S.parser(schema)(true); // throws — no implicit double decoding (true -> "true" -> ...)
```

`S.json` is not the exact `string` type, so string inputs still go through
variant decoding instead of passing through.

**Definition order is not tag order.** The first variant that accepts wins, and
a differently-tagged variant sitting between two same-tag ones keeps its turn.

```ts
const schema = S.json.with(S.to, S.union(["123", S.bigint, S.string]));

S.parser(schema)("123"); // "123" — the literal matches first and stays a string
S.parser(schema)("124"); // 124n — the literal fails, the bigint variant decodes
S.parser(schema)("abc"); // "abc" — bigint decoding fails, the catch-all string accepts
```

**Built-in decoding fills gaps, it doesn't re-type what the source already
has.** A variant whose tag the source can produce takes those values as they
are; the built-in decoder only steps in for a variant whose tag the source has
no way to produce. `bigint` is not a JSON tag, so a JSON string is offered to
`BigInt`; `string` and `number` both are, so a JSON boolean never becomes
`"true"` and a JSON string never becomes a number:

```ts
const schema = S.json.with(S.to, S.union([S.literal("a"), S.number, S.literal("b")]));

S.parser(schema)("b"); // "b" — "a" fails, "b" matches
S.parser(schema)("5"); // throws — S.number takes JSON numbers as they are, it doesn't decode "5"
S.parser(schema)("c"); // throws — no variant accepts
```

**Grouping is codegen, not semantics.** Emitting same-tag variants under one
shared type check — `typeof i==="string"&&(i==="a"||i==="b")` — is an
optimization, allowed exactly while it can't change which variant wins. Hoisting
`"a"` and `"b"` past `S.number` above is legal: neither literal can take a value
`S.number` would have taken. Hoisting `"123"` and the catch-all `S.string` past
`S.bigint` is not: the catch-all takes every string, `"124"` among them. Where
the shortcut isn't available, each check stays in its own definition slot and
the repeated `typeof` is reused from a var.

`S.unknown` is a normal type here — it only matches another `unknown`. But an
`unknown` value may already *be* any of the variant types, so the
no-re-typing rule leaves no gap to fill: built-in decoding never steps in,
and the conversion is pure validation — every variant checked by its type,
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

S.schema("x").with(S.to, S.union([S.string.with(S.min, 3), S.schema("x")]));
// the refinement rejects, then the literal fallback accepts "x"
```

**Exception — partial type match.** If the source has the same type as *some but
not all* target variants, the operation is rejected when it's created. Sury
can't tell whether you want a pass-through for the matching variant, decoding
attempts in definition order, or simply widened the type with no decoding
intent:

```ts
S.string.with(S.to, S.union([S.number, S.string]));
// Invalid operation: for "123" — keep "123" or decode to 123?
```

Say what you mean with an explicit variant:

```ts
// Try decoding to number first, keep the string otherwise:
S.string.with(S.to, S.union([S.string.with(S.to, S.number), S.string]));

// Pass strings through, never producing a number:
S.string.with(S.to, S.union([S.never.with(S.to, S.number), S.string]));
```

The same applies to widening into optional or nullable targets:

```ts
S.string.with(S.to, S.optional(S.string));
// Invalid operation: for "undefined" — keep the string or decode to undefined?

// Widen without decoding — the undefined variant is unreachable:
S.string.with(S.to, S.union([S.string, S.never.with(S.to, S.schema(undefined))]));
```

The `Invalid operation` error suggests these rewrites.

## Rule 3: union → non-union

The mirror of rule 2: every source variant gets its own built-in decoder to the
target, dispatched in definition order:

```ts
const schema = S.union([S.bigint, S.string]).with(S.to, S.json);

S.parser(schema)(123n); // "123"
S.parser(schema)("123"); // "123"
S.parser(schema)("abc"); // "abc"
```

**Exception — partial type match.** If the target has the same type as some but
not all source variants, the operation is rejected. Sury can't tell whether the
non-matching variants should decode to the target or be rejected as failed
cases:

```ts
S.union([S.number, S.string]).with(S.to, S.string);
// Invalid operation: for 123 — decode to "123" or fail as a non-matching case?
```

Say what you mean with an explicit target union:

```ts
// Decode numbers to strings:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.string), S.string]));

// Reject numbers:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.never), S.string]));
```

The `Invalid operation` error suggests these rewrites.

## Rule 4: union → union

No coercion — values pass through to the same-type target variant. The two
unions must cover each other: every source variant needs at least one same-type
target variant, and every target variant needs at least one same-type source
variant. Otherwise the operation is rejected:

```ts
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string])); // ✅
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string, S.boolean])); // ❌ boolean has no source variant
S.union([S.string, S.number, S.boolean]).with(S.to, S.union([S.number, S.string])); // ❌ boolean has no target variant
S.union([S.string, S.number, S.bigint]).with(S.to, S.union([S.json, S.bigint])); // ❌ json is not the exact string/number type
```

A transformed source variant matches by its output type, and a transformed
target variant by its input type — so per-variant conversion is always available
explicitly:

```ts
S.optional(S.string).with(S.to, S.nullable(S.boolean)); // ❌ string doesn't match boolean
S.optional(S.string).with(S.to, S.nullable(S.string.with(S.to, S.boolean))); // ✅
```

**Exception — nullish bridge.** A `null` or `undefined` variant left unmatched
by type may match the opposite nullish variant on the other side — even one that
already has a same-type match. At runtime the same-type target wins; the bridge
only kicks in when there is none:

```ts
S.optional(S.string).with(S.to, S.nullable(S.string)); // ✅ undefined <-> null
S.optional(S.literal("x")).with(S.to, S.nullable(S.literal("x"))); // ✅ "x" matches by type, undefined <-> null
S.optional(S.string).with(S.to, S.union([S.string, null, undefined])); // ✅ undefined -> undefined (same type wins); reverse maps null -> undefined
```

**Worked example** — `S.union([S.bigint, S.number, null]).with(S.to, S.union([S.bigint, S.number, undefined]))`:

Forward:

- `123n` → `123n` (bigint passes through)
- `123.12` → `123.12` (number passes through)
- `null` → `undefined` (nullish bridge)

Reverse (via `S.encoder`):

- `undefined` → `null` (nullish bridge)
- `123n` → `123n`, `123.12` → `123.12` (pass through)

Union conversion always performs exhaustive validation — every variant is
checked, so transformed unions stay consistent across decode and encode.

## Failure hands the value to the next variant

Any *validation* failure of a variant — a discriminant miss, a refinement
failure, or a `S.Error` raised anywhere inside its body — passes the value to the
next variant. Only when none is left does the union throw, aggregating the
per-variant reasons under one error. This is the uniform rule for plain
validation unions and for every conversion rule above.

A variant that throws something that isn't a Sury error propagates it instead.
That exception is a bug in the code that raised it — a `TypeError` from a
predicate that assumed the wrong type, say — not a statement about whether the
value matches, and treating it as "try the next variant" would let a later
catch-all turn the bug into a silently successful parse:

```ts
S.union([S.string.with(S.refine, (v) => v.trim().length > 0), S.string]);
// a non-string never reaches the predicate — but if one did, the TypeError
// surfaces rather than falling through to the catch-all
```

Two consequences:

- A variant's selection condition can absorb its cond-expressible checks, since
  failing them means "try the next variant" anyway. Same-type variants with
  different refinements therefore keep their dispatch:
  `S.union([S.string.with(S.min, 3), S.number, S.string])` accepts `"ab"` through
  the catch-all.
- Where no later variant could accept a value that entered this one (disjoint
  types, disjoint literal discriminants — the discriminated-union shape), the
  fall-through is dead code. The variant throws its own precise error instead
  (`Failed at ["a"]: Expected string, received 42`), which keeps happy paths free
  of `try`/`catch` and sharpens the message.

## No built-in decoder for a variant

A pair with no built-in decoder is rejected when the operation is created —
`Can't decode boolean to number. Use S.to to define a custom decoder`. Being one
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

`S.never` remains the way to say a path is deliberately unreachable — it is
ignored by variant matching, so it never triggers this rejection:

```ts
S.boolean.with(S.to, S.union([S.string, S.never.with(S.to, S.symbol)])); // ✅ the symbol path is unreachable
```

## Spec coverage

Every `packages/sury/specs/codec-*.yaml` spec matches these rules. The rows that
changed when the implementation landed:

| Spec                                 | Rule | Behavior                                                             |
| ------------------------------------ | ---- | -------------------------------------------------------------------- |
| `codec-union-nested-refined-union`    | —    | a union with its own refinement isn't flattened, and it still refines |
| `codec-bool-union2-unsupported`       | —    | rejected — no `boolean -> symbol` decoder                            |
| `codec-bool-union2-all-unsupported`   | —    | rejected — no decoder for either member                              |
| `codec-union2-string-unsupported`     | —    | rejected — no `symbol -> string` decoder                             |
| `codec-json-union2`                   | 2    | non-bigint string falls back to the `S.string` member                |
| `codec-json-union3-ungrouped`         | 2    | `"123"` matches the literal, `"124"` reaches the `S.bigint` member    |
| `codec-number-union2-int32`           | 2    | compiles: int32 first, string next                                   |
| `codec-string-optional-partial`       | 2    | rejected — partial type match                                        |
| `codec-string-union2-partial`         | 2    | rejected — partial type match                                        |
| `codec-union2-string-partial`         | 3    | rejected — partial type match                                        |
| `codec-optional-nullable-partial`     | 4    | rejected — `string` has no same-type target member                   |
| `codec-union2-union3-extra-target`    | 4    | rejected — `boolean` has no source member                            |
| `codec-union3-union2-extra-source`    | 4    | rejected — `boolean` has no target member                            |
| `codec-union3-union2-json`            | 4    | rejected — `json` is not the exact `string`/`number` type             |
| `codec-union-refined-fallback`         | —    | new: a refined member falls through to a same-type catch-all          |
| `union2-refine-throws`                 | —    | new: a foreign exception propagates instead of matching the catch-all |
| `optional-object`                      | —    | new: an array is not an object, in the `X \| undefined` dispatch too   |
| `union2-object-number`                 | —    | new: the same, for an object member sharing a union with another tag  |
| `object1`, `object5-optional`          | —    | an array is not an object at the top level either, in every mode      |

The already-conformant rows kept their behavior; several lost dead code the old
implementation emitted (a `try/catch` around a body that can't throw, an `else if`
re-testing its own `else`) and now report the precise per-member error where the
fall-through is provably dead.
