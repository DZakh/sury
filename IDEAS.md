# Ideas draft

## Union schema as decoder input — investigation & plan

### Desired logic (already documented)

docs/js-usage.md:858 / docs/rescript-usage.md:925: "If the source is itself a
union, the algorithm runs independently for each source variant." Today this is
only true when the source union is the top-level driving schema.

### How it works today

- `getDecoder(s1, s2)` chains `s1.to = s2` (Sury.res:2581-2591), so top-level
  union sources are handled by `unionDecoder`'s `toPerCase` path
  (Sury.res:3626-3629, 3962-3967): each variant gets `.to(target)` appended and
  parses independently — validation doubles as routing because the trusted
  union input is first widened with `input.schema = unknown`
  (Sury.res:3641-3646).
- At val level (object fields, array items, tuples) the union arrives as
  `input.schema` at the *target's* decoder. The only union-input handling is
  the `isWiderUnionSchema` pass-through (same-index prefix, scalar tags only,
  Sury.res:3597-3619); everything else is widened to unknown, and non-union
  target decoders have no union branch at all (`unsupportedDecode`).

### Bug inventory (all reproduced on main)

1. **Correctness, silent rejection: nested union→union.**
   `S.decoder(S.schema({f: S.union([S.bigint, null])}), S.schema({f: S.union([S.string, undefined])}))`
   compiles to target-validation only (`if(!(typeof v0==="string"||v0===void 0))fail`)
   — `{f: 123n}` is rejected at runtime although the documented algorithm
   converts it. Same for array items.
2. **Build error: nested union→scalar.** `{f: (str|num)} -> {f: string}` →
   "Can't decode string | number to string" (no union branch in
   stringDecoderFn, Sury.res:2030).
3. **Build panic: object source into union-of-objects target.**
   `S.decoder(objA, S.union([objB, objA]))`, `S.decoder(u, u)` for a
   union-of-objects `u`, and `(objA|objB) -> objA` all crash with
   "[Sury] The schema doesn't have additional items": the case parse reads a
   property missing on the typed source val (B.Val.get, Sury.res:1777); the
   panic is not a SuryError, so `getArrItemsCode`'s catch rethrows
   (Sury.res:3770) instead of treating it as a failed case.
4. **Perf: trusted unions revalidate.** Self-decode of a union-of-objects
   field revalidates and rebuilds the object (twice when nested and
   converted). Scalar unions escape via isWiderUnionSchema, but
   object/array/instance/union/ref items are excluded from it, and reorders
   (`(str|num) -> (num|str|bool)`) revalidate instead of noop.
5. **ref/recursive union inputs** are unsupported ("Can't decode R to
   string"); only jsonDecoder ad-hoc handles ref inputs (FIXME Sury.res:4670).
6. **Adjacent pre-existing codegen bug (blocks per-variant work):**
   `S.union([S.string->S.to(S.number), S.bool])` parsed from unknown emits
   `if(typeof i==="string"){if(!Number.isNaN(v0)){let v0=+i;i=v0}}` — the
   check is hoisted above the var it reads → runtime ReferenceError, and NaN
   strings would pass silently. Same family as the "union discriminant hoists
   refinement checks" known bug below.

### Design choice

The only central interception points for a union-typed `input.schema` are the
parse loop and the `encoder` slot — scalar decoders can't be reached around,
and patching every decoder duplicates code (goal 3). Use the **encoder slot**:
the parse loop already calls `input.schema.encoder` before the decoder when
`schema !== expected && expected.tag !== unknown` (Sury.res:2320-2328);
precedent: `date` (Sury.res:5050) and union-tagged `json` (Sury.res:4705). The
widen-branch even guards on `input.schema.encoder === None` already.

In `Union.factory`:

```rescript
mut.encoder = Some(Builder.encoder((~input, ~target) =>
  input->B.refine(
    ~schema=unknown, // phase 1; phase 3 keeps the typed source
    ~expected=input.schema->updateOutput(mut => mut.to = Some(target))->castToInternal,
  )
))
```

The loop re-enters, runs the chained union's own `unionDecoder`, and
`toPerCase` produces the per-variant dispatch — the exact machinery and
generated-code shape the top level has today. No duplicated codegen; one
mechanism covers union→scalar, union→union, union→object, union→ref targets.
Unions with a custom `parser` (option machinery) keep their semantics
automatically: `updateOutput` preserves `parser`, so `toPerCase` stays `None`
for them. The standard `unknown` parse hot path can't regress — unknown has no
encoder, so the encoder never fires there.

### Phases

0. Fix bug 6 (hoisting of checks on transformed vals) and add the missing
   nested/transformed-union tests (S_to_test.res FIXMEs) — every
   per-variant parse emits through that path. **(pending — transformed-union
   variants are declined by the union encoder until this is fixed)**
1. **(done)** Union encoder (fixes bugs 1 and 2): `Union.encoder` re-drives
   the source union with `.to(target)` appended. Declines for ref-reaching
   targets (S.json/recursive keep dedicated handling) and for source variants
   with `.to`/`parser`/ref. Also implements tier-1 const priority (matching
   const variants tried first — fixes the pre-existing `enum(a,b) ->
   enum(b,a,c)` 'a'→"b" remap), and drops guaranteed-to-fail cases from
   generated code (their embedded errors join the exhaustive failure args
   instead of runtime `try{throw}` chains). Regression tests in S_to_test.res.
2. **(done, 2b pending)** Typed-object case parsing (fixes bug 3): B.Val.get
   on a missing property of a typed object without additionalItems throws
   UnsupportedDecode (catchable) instead of panicking → becomes a per-case
   failure. Combined with case dropping, `objA -> (objB|objA)` compiles to a
   noop. Optional 2b: compile-time tier-1 discriminant matching for object
   groups to prune impossible variants without relying on codegen failures.
3. Trusted-input perf (fixes bug 4):
   - Replace isWiderUnionSchema with order-independent subset matching;
     reference-equal items always match (covers object/array/instance/
     recursive); primitives also match on tag+const+format with no
     to/refiner. → noop for `{f:u}->{f:u}`, reorders, enum subsets.
   - Optional 3b: keep the typed source in the encoder (skip widening):
     per-case body parses from the trusted case schema, routing conds
     synthesized from the hoisted type/discriminant checks, last case as plain
     `else`. Avoids re-running union refiners per case on already-validated
     input. Also covers the single-surviving-case try/catch FIXMEs
     (S_to_test.res:1077, 1104). Gate on benchmarks + generated-code size.
4. Unlocked cleanups: drop `originalItems` (Option.getWithDefault FIXME);
   recheck the CustomJSON encode error FIXME (S_to_test.res:428) — the full
   fix needs unified ref-input handling (jsonDecoderFn ref FIXME), separate
   follow-up; docs already promise per-source-variant behavior, add a nested
   example; cross off the related known bugs below.

### Risks

- unionDecoder codegen is the most intricate in the lib (deopt paths,
  mergeWithCatch interplay Sury.res:4163-4191); the encoder adds a new caller
  — cover async + hasTransform combinations explicitly.
- The isWiderUnionSchema replacement touches option machinery (fromDefault
  skip Sury.res:3978, nestedLoc priority issue #150) — S_option/S_null/
  S_default suites have compiled-code assertions that will legitimately
  change; review individually.
- Phase 1 makes some currently-erroring schemas compile — audit tests that
  assert build errors for unions.

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
