// Type-level regression tests for the UnknownToOutput / UnknownToInput
// optimization (S.d.ts). These are checked by `pnpm tsc` — any failing
// assertion makes `Expect<...>` violate its `extends true` constraint and
// fails the build. There is no runtime component.
//
// Two things are asserted for every case:
//   1. The optimized resolvers produce the *same* type as the pre-optimization
//      baseline (`Old*` copies below).
//   2. The concrete inferred type matches the hand-written expectation.

import * as S from "../src/S.js";

// ─────────────────────────────────────────────────────────────────────────────
// Equality helpers (structural, key-order independent).
// ─────────────────────────────────────────────────────────────────────────────
type Equals<A, B> = (<T>() => T extends A ? 1 : 2) extends <T>() => T extends B
  ? 1
  : 2
  ? true
  : false;
type Expect<T extends true> = T;
type ExpectFalse<T extends false> = T;

// ─────────────────────────────────────────────────────────────────────────────
// Baseline (pre-optimization) copies of the resolvers, reproduced verbatim so
// the optimized versions can be diffed against them at the type level.
// ─────────────────────────────────────────────────────────────────────────────
type OldHasUndefined<T> = [T] extends [undefined]
  ? true
  : undefined extends T
  ? true
  : false;
type OldFlatten<T> = T extends object
  ? { [K in keyof T as T[K] extends never ? never : K]: T[K] }
  : T;
type OldUnknownToOutput<T> = T extends S.Schema<infer Output, unknown>
  ? Output
  : T extends (...args: any[]) => any
  ? T
  : T extends unknown[]
  ? { [K in keyof T]: OldUnknownToOutput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? OldFlatten<
      {
        [k in keyof T as OldHasUndefined<OldUnknownToOutput<T[k]>> extends true
          ? k
          : never]?: OldUnknownToOutput<T[k]>;
      } & {
        [k in keyof T as OldHasUndefined<OldUnknownToOutput<T[k]>> extends true
          ? never
          : k]: OldUnknownToOutput<T[k]>;
      }
    >
  : T;
type OldUnknownToInput<T> = T extends S.Schema<unknown, infer Input>
  ? Input
  : T extends (...args: any[]) => any
  ? T
  : T extends unknown[]
  ? { [K in keyof T]: OldUnknownToInput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? OldFlatten<
      {
        [k in keyof T as OldHasUndefined<OldUnknownToInput<T[k]>> extends true
          ? k
          : never]?: OldUnknownToInput<T[k]>;
      } & {
        [k in keyof T as OldHasUndefined<OldUnknownToInput<T[k]>> extends true
          ? never
          : k]: OldUnknownToInput<T[k]>;
      }
    >
  : T;

// For one shape, whether the optimized Output/Input resolvers match the
// baseline resolvers exactly. Wrapped in `Expect<...>` at each (concrete) use
// site — applying `Expect` here, while `T` is still abstract, would collapse to
// `boolean` and fail at the definition.
type OutMatchesBaseline<T> = Equals<
  S.UnknownToOutput<T>,
  OldUnknownToOutput<T>
>;
type InMatchesBaseline<T> = Equals<S.UnknownToInput<T>, OldUnknownToInput<T>>;

// Convenience aliases for building shape inputs (the argument type of S.schema).
type Str = S.Schema<string, string>;
type Num = S.Schema<number, number>;
type Bool = S.Schema<boolean, boolean>;
type Sym = S.Schema<symbol, symbol>;
type OptStr = S.Schema<string | undefined, string | undefined>;
type OptNum = S.Schema<number | undefined, number | undefined>;
type OptBool = S.Schema<boolean | undefined, boolean | undefined>;
type Unk = S.Schema<unknown, unknown>;
type Anyish = S.Schema<any, any>;
// Output ≠ Input, to exercise both resolver branches independently.
type NumFromStr = S.Schema<number, string>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 1 — all required.
// ─────────────────────────────────────────────────────────────────────────────
type C_AllRequired = { a: Str; b: Num; c: Bool };
type _allRequired_outBaseline = Expect<OutMatchesBaseline<C_AllRequired>>;
type _allRequired_inBaseline = Expect<InMatchesBaseline<C_AllRequired>>;
type _allRequired_out = Expect<
  Equals<S.UnknownToOutput<C_AllRequired>, { a: string; b: number; c: boolean }>
>;
type _allRequired_in = Expect<
  Equals<S.UnknownToInput<C_AllRequired>, { a: string; b: number; c: boolean }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 2 — mixed required / optional (the hot spot from issue #166).
// ─────────────────────────────────────────────────────────────────────────────
type C_MixedOptional = { a: Str; b: OptNum; c: Bool; d: OptNum; e: Sym };
type _mixed_outBaseline = Expect<OutMatchesBaseline<C_MixedOptional>>;
type _mixed_inBaseline = Expect<InMatchesBaseline<C_MixedOptional>>;
type _mixed_out = Expect<
  Equals<
    S.UnknownToOutput<C_MixedOptional>,
    {
      a: string;
      b?: number | undefined;
      c: boolean;
      d?: number | undefined;
      e: symbol;
    }
  >
>;
type _mixed_in = Expect<
  Equals<
    S.UnknownToInput<C_MixedOptional>,
    {
      a: string;
      b?: number | undefined;
      c: boolean;
      d?: number | undefined;
      e: symbol;
    }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 3 — all optional.
// ─────────────────────────────────────────────────────────────────────────────
type C_AllOptional = { a: OptStr; b: OptNum };
type _allOptional_outBaseline = Expect<OutMatchesBaseline<C_AllOptional>>;
type _allOptional_inBaseline = Expect<InMatchesBaseline<C_AllOptional>>;
type _allOptional_out = Expect<
  Equals<
    S.UnknownToOutput<C_AllOptional>,
    { a?: string | undefined; b?: number | undefined }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 4 — nested object.
// ─────────────────────────────────────────────────────────────────────────────
type C_Nested = { a: Str; inner: { b: Num; c: Bool } };
type _nested_outBaseline = Expect<OutMatchesBaseline<C_Nested>>;
type _nested_inBaseline = Expect<InMatchesBaseline<C_Nested>>;
type _nested_out = Expect<
  Equals<
    S.UnknownToOutput<C_Nested>,
    { a: string; inner: { b: number; c: boolean } }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 5 — nested object with an optional inner field.
// ─────────────────────────────────────────────────────────────────────────────
type C_NestedOptional = { a: Str; inner: { b: Num; c: OptBool } };
type _nestedOpt_outBaseline = Expect<OutMatchesBaseline<C_NestedOptional>>;
type _nestedOpt_inBaseline = Expect<InMatchesBaseline<C_NestedOptional>>;
type _nestedOpt_out = Expect<
  Equals<
    S.UnknownToOutput<C_NestedOptional>,
    { a: string; inner: { b: number; c?: boolean | undefined } }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 6 — tuple (array branch; resolver path is shared but not the focus).
// ─────────────────────────────────────────────────────────────────────────────
type C_Tuple = [Str, Num];
type _tuple_outBaseline = Expect<OutMatchesBaseline<C_Tuple>>;
type _tuple_inBaseline = Expect<InMatchesBaseline<C_Tuple>>;
type _tuple_out = Expect<Equals<S.UnknownToOutput<C_Tuple>, [string, number]>>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 7 — literal values (string / number / boolean).
// ─────────────────────────────────────────────────────────────────────────────
type C_Literals = { kind: "a"; n: 1; b: true; s: Str };
type _literals_outBaseline = Expect<OutMatchesBaseline<C_Literals>>;
type _literals_inBaseline = Expect<InMatchesBaseline<C_Literals>>;
type _literals_out = Expect<
  Equals<S.UnknownToOutput<C_Literals>, { kind: "a"; n: 1; b: true; s: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 8 — function-typed field (passes through unchanged, stays required).
// ─────────────────────────────────────────────────────────────────────────────
type C_Function = { fn: () => void; a: Str };
type _function_outBaseline = Expect<OutMatchesBaseline<C_Function>>;
type _function_inBaseline = Expect<InMatchesBaseline<C_Function>>;
type _function_out = Expect<
  Equals<S.UnknownToOutput<C_Function>, { fn: () => void; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 9 — `S.unknown` field MUST become optional (undefined ⊆ unknown).
// ─────────────────────────────────────────────────────────────────────────────
type C_Unknown = { u: Unk; a: Str };
type _unknown_outBaseline = Expect<OutMatchesBaseline<C_Unknown>>;
type _unknown_inBaseline = Expect<InMatchesBaseline<C_Unknown>>;
type _unknown_out = Expect<
  Equals<S.UnknownToOutput<C_Unknown>, { u?: unknown; a: string }>
>;
type _unknown_in = Expect<
  Equals<S.UnknownToInput<C_Unknown>, { u?: unknown; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 10 — `S.any` field (also becomes optional, value stays `any`).
// ─────────────────────────────────────────────────────────────────────────────
type C_Any = { an: Anyish; a: Str };
type _any_outBaseline = Expect<OutMatchesBaseline<C_Any>>;
type _any_inBaseline = Expect<InMatchesBaseline<C_Any>>;
type _any_out = Expect<
  Equals<S.UnknownToOutput<C_Any>, { an?: any; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 11 — empty object.
// ─────────────────────────────────────────────────────────────────────────────
type C_Empty = {};
type _empty_outBaseline = Expect<OutMatchesBaseline<C_Empty>>;
type _empty_inBaseline = Expect<InMatchesBaseline<C_Empty>>;
type _empty_out = Expect<Equals<S.UnknownToOutput<C_Empty>, {}>>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 12 — union of objects (resolver distributes over each member).
// ─────────────────────────────────────────────────────────────────────────────
type C_Union = { kind: "a"; a: Str } | { kind: "b"; b: Num };
type _union_outBaseline = Expect<OutMatchesBaseline<C_Union>>;
type _union_inBaseline = Expect<InMatchesBaseline<C_Union>>;
type _union_out = Expect<
  Equals<
    S.UnknownToOutput<C_Union>,
    { kind: "a"; a: string } | { kind: "b"; b: number }
  >
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 13 — Output ≠ Input field, verifying the two resolvers stay distinct.
// ─────────────────────────────────────────────────────────────────────────────
type C_Transform = { x: NumFromStr; a: Str };
type _transform_outBaseline = Expect<OutMatchesBaseline<C_Transform>>;
type _transform_inBaseline = Expect<InMatchesBaseline<C_Transform>>;
type _transform_out = Expect<
  Equals<S.UnknownToOutput<C_Transform>, { x: number; a: string }>
>;
type _transform_in = Expect<
  Equals<S.UnknownToInput<C_Transform>, { x: string; a: string }>
>;

// ─────────────────────────────────────────────────────────────────────────────
// Case 14 — `Schema<never>` field: the ONE intentional divergence from the
// baseline, so it is asserted directly rather than via OutMatchesBaseline.
//
// A field whose resolved type is exactly `never` is the only input on which the
// optionality predicates disagree:
//   • Baseline `HasUndefined<X>` starts with `[X] extends [undefined]`, which is
//     true whenever X is assignable to `undefined` — and `never` is assignable
//     to everything, so a `never` field was treated as optional. The optional
//     `a?: never` then survives `Flatten` as `a?: undefined` (optional widens
//     `never` to `undefined`).
//   • The optimized `undefined extends R[K]` is `false` for `never`, so the
//     field is required (`a: never`) and `Flatten` — which drops keys whose
//     value `extends never` — removes it entirely.
//
// For every non-`never` field the two predicates agree exactly, so this is the
// sole behavioral difference. No Sury combinator yields a `Schema<never>` field
// in normal use; you must hand-write `S.Schema<never, never>`. The new result
// (drop the impossible field) is arguably the cleaner of the two.
// ─────────────────────────────────────────────────────────────────────────────
type C_NeverField = { a: S.Schema<never, never>; b: Num };
// New: the impossible field is dropped on both Output and Input.
type _never_out = Expect<
  Equals<S.UnknownToOutput<C_NeverField>, { b: number }>
>;
type _never_in = Expect<Equals<S.UnknownToInput<C_NeverField>, { b: number }>>;
// Baseline kept it as an optional-`undefined` slot.
type _never_oldOut = Expect<
  Equals<OldUnknownToOutput<C_NeverField>, { a?: undefined; b: number }>
>;
// And therefore new ≠ old for this (only this) shape.
type _never_diverges = ExpectFalse<
  Equals<S.UnknownToOutput<C_NeverField>, OldUnknownToOutput<C_NeverField>>
>;
