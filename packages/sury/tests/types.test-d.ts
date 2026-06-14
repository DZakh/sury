// Type-level assertion tests for the inferred Output/Input of `S.schema`.
// Checked by `pnpm tsc` — a wrong assertion makes `Expect<...>` violate its
// `extends true` constraint and fails the build. No runtime component (the file
// is never executed; the emitted .js is gitignored).
//
// Each case builds a real schema and asserts its inferred `S.Output` / `S.Input`
// against the exact type you would write by hand.

import * as S from "../src/S.js";

// Structural type-equality (key-order independent, distinguishes any/unknown).
type Equals<A, B> = (<T>() => T extends A ? 1 : 2) extends <T>() => T extends B
  ? 1
  : 2
  ? true
  : false;
type Expect<T extends true> = T;

// ─────────────────────────────────────────────────────────────────────────────
// All required.
// ─────────────────────────────────────────────────────────────────────────────
const allRequired = S.schema({ a: S.string, b: S.number, c: S.boolean });
type _allRequired_out = Expect<
  Equals<S.Output<typeof allRequired>, { a: string; b: number; c: boolean }>
>;
type _allRequired_in = Expect<
  Equals<S.Input<typeof allRequired>, { a: string; b: number; c: boolean }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Mixed required / optional.
// ─────────────────────────────────────────────────────────────────────────────
const mixed = S.schema({ a: S.string, b: S.optional(S.number), c: S.boolean });
type _mixed_out = Expect<
  Equals<
    S.Output<typeof mixed>,
    { a: string; b?: number | undefined; c: boolean }
  >
>;
type _mixed_in = Expect<
  Equals<
    S.Input<typeof mixed>,
    { a: string; b?: number | undefined; c: boolean }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// All optional.
// ─────────────────────────────────────────────────────────────────────────────
const allOptional = S.schema({
  a: S.optional(S.string),
  b: S.optional(S.number),
});
type _allOptional_out = Expect<
  Equals<
    S.Output<typeof allOptional>,
    { a?: string | undefined; b?: number | undefined }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Nested object.
// ─────────────────────────────────────────────────────────────────────────────
const nested = S.schema({
  a: S.string,
  inner: S.schema({ b: S.number, c: S.boolean }),
});
type _nested_out = Expect<
  Equals<
    S.Output<typeof nested>,
    { a: string; inner: { b: number; c: boolean } }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Nested object with an optional inner field.
// ─────────────────────────────────────────────────────────────────────────────
const nestedOptional = S.schema({
  a: S.string,
  inner: S.schema({ b: S.number, c: S.optional(S.boolean) }),
});
type _nestedOptional_out = Expect<
  Equals<
    S.Output<typeof nestedOptional>,
    { a: string; inner: { b: number; c?: boolean | undefined } }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Tuple.
// ─────────────────────────────────────────────────────────────────────────────
const tuple = S.schema([S.string, S.number] as const);
type _tuple_out = Expect<Equals<S.Output<typeof tuple>, [string, number]>>;
type _tuple_in = Expect<Equals<S.Input<typeof tuple>, [string, number]>>;

// ─────────────────────────────────────────────────────────────────────────────
// Literal values (string / number / boolean).
// ─────────────────────────────────────────────────────────────────────────────
const literals = S.schema({
  kind: "a" as const,
  n: 1 as const,
  b: true as const,
  s: S.string,
});
type _literals_out = Expect<
  Equals<
    S.Output<typeof literals>,
    { kind: "a"; n: 1; b: true; s: string }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Union of objects.
// ─────────────────────────────────────────────────────────────────────────────
const unionObjects = S.union([
  S.schema({ kind: "a" as const, a: S.string }),
  S.schema({ kind: "b" as const, b: S.number }),
]);
type _union_out = Expect<
  Equals<
    S.Output<typeof unionObjects>,
    { kind: "a"; a: string } | { kind: "b"; b: number }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// `S.unknown` field — optional, because `undefined` is assignable to `unknown`.
// ─────────────────────────────────────────────────────────────────────────────
const unknownField = S.schema({ u: S.unknown, a: S.string });
type _unknown_out = Expect<
  Equals<S.Output<typeof unknownField>, { u?: unknown; a: string }>
>;
type _unknown_in = Expect<
  Equals<S.Input<typeof unknownField>, { u?: unknown; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// `S.any` field — also optional, value stays `any`.
// ─────────────────────────────────────────────────────────────────────────────
const anyField = S.schema({ an: S.any, a: S.string });
type _any_out = Expect<
  Equals<S.Output<typeof anyField>, { an?: any; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Empty object.
// ─────────────────────────────────────────────────────────────────────────────
const empty = S.schema({});
type _empty_out = Expect<Equals<S.Output<typeof empty>, {}>>;

// ─────────────────────────────────────────────────────────────────────────────
// Transform field (`S.to`) — Output and Input differ.
// ─────────────────────────────────────────────────────────────────────────────
const transform = S.schema({
  x: S.string.with(S.to, S.number),
  a: S.string,
});
type _transform_out = Expect<
  Equals<S.Output<typeof transform>, { x: number; a: string }>
>;
type _transform_in = Expect<
  Equals<S.Input<typeof transform>, { x: string; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// `S.never` field — the field's parsed value is `never`, so the faithful type
// keeps it as a REQUIRED `never` property. The object is therefore uninhabited,
// which correctly reflects that this schema can never parse (see S_never_test.res
// "Fails to parse a object with Never field"). This mirrors a hand-written type.
// ─────────────────────────────────────────────────────────────────────────────
const neverField = S.schema({ oldKey: S.never, key: S.string });
type _never_out = Expect<
  Equals<S.Output<typeof neverField>, { oldKey: never; key: string }>
>;
type _never_in = Expect<
  Equals<S.Input<typeof neverField>, { oldKey: never; key: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// `S.optional(S.never)` field — the realistic deprecated-field pattern. It
// resolves to `undefined` (the `never | undefined` collapses), so the field is
// optional and the object stays inhabited, parsing successfully when the field
// is absent (see S_never_test.res "...when it's optional and not present").
// ─────────────────────────────────────────────────────────────────────────────
const optionalNeverField = S.schema({
  oldKey: S.optional(S.never),
  key: S.string,
});
type _optionalNever_out = Expect<
  Equals<
    S.Output<typeof optionalNeverField>,
    { oldKey?: undefined; key: string }
  >
>;
